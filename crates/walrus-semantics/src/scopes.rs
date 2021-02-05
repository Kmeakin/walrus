use crate::{diagnostic::Diagnostic, hir::*};
use arena::{Arena, ArenaMap, Idx};
use std::collections::HashMap;

pub fn scopes(module: &Module) -> Scopes {
    let mut scopes = Scopes::new();
    scopes.module_scope(module);
    scopes
}

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
                (Var::new("Bool"), Binding::BuiltinType(BuiltinType::Bool)),
                (Var::new("Int"), Binding::BuiltinType(BuiltinType::Int)),
                (Var::new("Float"), Binding::BuiltinType(BuiltinType::Float)),
                (Var::new("Char"), Binding::BuiltinType(BuiltinType::Char)),
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
    BuiltinType(BuiltinType),
    Struct(StructDefId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BuiltinType {
    Bool,
    Int,
    Float,
    Char,
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

    fn insert_binding(&mut self, bindings: &mut Bindings, name: &Var, binding: Binding) {
        match bindings.get(name) {
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

    fn module_scope(&mut self, module: &Module) {
        let mut bindings = Bindings::new();
        for decl in &module.decls {
            match decl {
                Decl::Fn(id) => {
                    let fn_def = &module.data[*id];
                    self.insert_binding(&mut bindings, &fn_def.name, Binding::Fn(*id))
                }
                Decl::Struct(id) => {
                    let struct_def = &module.data[*id];
                    self.insert_binding(&mut bindings, &struct_def.name, Binding::Struct(*id))
                }
            }
        }

        for decl in &module.decls {
            match decl {
                Decl::Fn(id) => self.fn_def_scope(module, *id),
                Decl::Struct(id) => self.struct_def_scope(module, *id),
            }
        }
    }

    fn param_list_scope(&mut self, module: &Module, params: &[Param]) {
        let mut bindings = Bindings::new();
        for param in params {
            let Param { pat, ty } = param;
            self.pat_scope(module, &mut bindings, *pat);
            if let Some(ty) = ty {
                self.type_scope(module, *ty)
            }
        }
    }

    fn fn_def_scope(&mut self, module: &Module, id: FnDefId) {
        let fn_def = &module.data[id].clone();
        self.in_child_scope(|this| {
            this.param_list_scope(module, &fn_def.params);
            if let Some(ret_type) = fn_def.ret_type {
                this.type_scope(module, ret_type)
            }
            this.expr_scope(module, fn_def.expr)
        })
    }

    fn struct_def_scope(&mut self, module: &Module, id: StructDefId) {}

    fn expr_scope(&mut self, module: &Module, id: ExprId) {
        self.set_scope_of_expr(id, self.scope);
        let expr = &module.data[id];
        match expr {
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Expr(expr) => self.expr_scope(module, *expr),
                        Stmt::Let { pat, ty, expr } => {
                            self.pat_scope(module, &mut Bindings::new(), *pat);
                            if let Some(ty) = ty {
                                self.type_scope(module, *ty);
                            }
                            self.expr_scope(module, *expr);
                        }
                    }
                }
                if let Some(expr) = expr {
                    self.expr_scope(module, *expr)
                }
            }
            Expr::Lambda { params, expr } => {
                self.param_list_scope(module, params);
                self.expr_scope(module, *expr)
            }
            expr => expr.walk_child_exprs(|id| self.expr_scope(module, id)),
        }
    }

    fn pat_scope(&mut self, module: &Module, bindings: &mut Bindings, id: PatId) {
        let pat = &module.data[id];
        match pat {
            Pat::Var(var) => self.insert_binding(bindings, var, Binding::Local(id)),
            pat => pat.walk_child_pats(|id| self.pat_scope(module, bindings, id)),
        }
    }

    fn type_scope(&mut self, module: &Module, id: TypeId) {
        self.set_scope_of_type(id, self.scope);
        let ty = &module.data[id];
        ty.walk_child_types(|id| self.type_scope(module, id))
    }
}
