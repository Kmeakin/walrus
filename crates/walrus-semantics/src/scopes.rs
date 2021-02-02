use crate::{
    diagnostic::Diagnostic,
    hir::{ExprId, FnDefId, Module, Pat, PatId, TypeId, Var},
};
use la_arena::{Arena, ArenaMap, Idx};
use std::collections::HashMap;

pub type ScopeId = Idx<Scope>;
pub type Bindings = HashMap<Var, Binding>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scopes {
    pub scopes: Arena<Scope>,
    pub scope: ScopeId,
    pub scope_of_expr: ArenaMap<ExprId, ScopeId>,
    pub scope_of_type: ArenaMap<TypeId, ScopeId>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub bindings: Bindings,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent: None,
            bindings: [
                (Var::new("Bool"), Binding::BuiltinType),
                (Var::new("Int"), Binding::BuiltinType),
                (Var::new("Float"), Binding::BuiltinType),
                (Var::new("Char"), Binding::BuiltinType),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Binding {
    Local(PatId),
    Fn(FnDefId),
    BuiltinType,
}

impl Scopes {
    pub fn scope_chain(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope), move |&scope| self.scopes[scope].parent)
    }

    pub fn lookup_in_scope(&self, scope: ScopeId, var: &Var) -> Option<&Binding> {
        self.scope_chain(scope)
            .find_map(|scope| self.scopes[scope].bindings.get(var))
    }

    pub fn scope_of_expr(&self, id: ExprId) -> ScopeId { self.scope_of_expr[id] }
    pub fn scope_of_type(&self, id: TypeId) -> ScopeId { self.scope_of_type[id] }
}

impl Scopes {
    fn new() -> Self {
        let mut scopes = Arena::default();
        let scope = scopes.alloc(Scope::default());
        Self {
            scopes,
            scope,
            scope_of_expr: ArenaMap::default(),
            scope_of_type: ArenaMap::default(),
            diagnostics: Vec::new(),
        }
    }

    fn set_scope_of_expr(&mut self, id: ExprId, scope: ScopeId) {
        self.scope_of_expr.insert(id, scope);
    }

    fn set_scope_of_type(&mut self, id: TypeId, scope: ScopeId) {
        self.scope_of_type.insert(id, scope);
    }

    fn child_scope(&mut self, scope: ScopeId) -> ScopeId {
        self.scopes.alloc(Scope {
            parent: Some(scope),
            bindings: HashMap::new(),
        })
    }

    fn in_child_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        let old_scope = self.scope;
        self.scope = self.child_scope(old_scope);
        f(self);
        self.scope = old_scope;
    }

    fn insert_pat(&mut self, module: &Module, bindings: &mut Bindings, id: PatId) {
        let pat = &module.data[id];
        match pat {
            Pat::Var(var) => self.insert_binding(bindings, var, Binding::Local(id)),
            pat => pat.walk_child_pats(|id| self.insert_pat(module, bindings, id)),
        }
    }

    fn insert_binding(&mut self, bindings: &mut Bindings, name: &Var, binding: Binding) {
        match bindings.get(&name) {
            Some(first) => self.diagnostics.push(Diagnostic::DuplicateBinding {
                first: *first,
                second: binding,
            }),
            None => {
                self.scopes[self.scope]
                    .bindings
                    .insert(name.clone(), binding);
                bindings.insert(name.clone(), binding);
            }
        }
    }
}
