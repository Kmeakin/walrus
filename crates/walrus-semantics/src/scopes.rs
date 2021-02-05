use crate::{
    builtins::{self, Builtin},
    diagnostic::Diagnostic,
    hir::*,
};
use arena::{Arena, ArenaMap, Idx};
use std::collections::HashMap;

pub fn scopes(module: &Module) -> Scopes {
    let mut scopes = Scopes::new();
    scopes.module_scope(module);
    scopes
}

pub type ScopeId = Idx<Scope>;
pub type Denotations = HashMap<Var, Denotation>;
type Vars = HashMap<Var, VarId>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Denotation {
    Local(PatId),
    Fn(FnDefId),
    Struct(StructDefId),
    Builtin(Builtin),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
    parent: Option<ScopeId>,
    denotations: Denotations,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scopes {
    scopes: Arena<Scope>,
    scope: ScopeId,
    scope_of_expr: ArenaMap<ExprId, ScopeId>,
    scope_of_type: ArenaMap<TypeId, ScopeId>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Public interface
impl Scopes {
    pub fn scope_chain(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope), move |&scope| self.scopes[scope].parent)
    }

    pub fn lookup_in_scope(&self, scope: ScopeId, var: &Var) -> Option<Denotation> {
        self.scope_chain(scope)
            .find_map(|scope| self.scopes[scope].denotations.get(var))
            .copied()
            .or_else(|| builtins::lookup(var).map(|builtin| Denotation::Builtin(builtin)))
    }

    pub fn scope_of_expr(&self, id: ExprId) -> ScopeId { self.scope_of_expr[id] }
    pub fn scope_of_type(&self, id: TypeId) -> ScopeId { self.scope_of_type[id] }
}

/// Private helpers
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
            denotations: Denotations::new(),
        })
    }

    fn in_child_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        let old_scope = self.scope;
        self.scope = self.child_scope(old_scope);
        f(self);
        self.scope = old_scope;
    }

    fn insert_denotation(
        &mut self,
        module: &Module,
        vars: &mut Vars,
        id: VarId,
        denotation: Denotation,
    ) {
        let var = module.data[id].clone();
        if self.insert_var(module, vars, id) {
            self.scopes[self.scope].denotations.insert(var, denotation);
        }
    }

    fn insert_var(&mut self, module: &Module, vars: &mut Vars, id: VarId) -> bool {
        let var = module.data[id].clone();
        match vars.get(&var) {
            None => {
                vars.insert(var, id);
                true
            }
            Some(first) => {
                self.diagnostics.push(Diagnostic::DuplicateVar {
                    first: *first,
                    second: id,
                });
                false
            }
        }
    }
}

/// The actual implementation
impl Scopes {
    fn module_scope(&mut self, module: &Module) {
        let mut toplevel_defs = Vars::new();

        for decl in &module.decls {
            match decl {
                Decl::Struct(id) => self.struct_def_scope(module, &mut toplevel_defs, *id),
                Decl::Fn(id) => self.fn_def_scope(module, &mut toplevel_defs, *id),
            }
        }
    }

    fn struct_def_scope(&mut self, module: &Module, toplevel_defs: &mut Vars, id: StructDefId) {
        let struct_def = &module.data[id];
        self.insert_denotation(
            module,
            toplevel_defs,
            struct_def.name,
            Denotation::Struct(id),
        );

        let mut fields = Vars::new();
        for field in &struct_def.fields {
            self.insert_var(module, &mut fields, field.name);
            self.type_scope(module, field.ty);
        }
    }

    fn fn_def_scope(&mut self, module: &Module, toplevel_defs: &mut Vars, id: FnDefId) {
        let fn_def = &module.data[id];
        self.insert_denotation(module, toplevel_defs, fn_def.name, Denotation::Fn(id));

        self.in_child_scope(|this| {
            let mut params = Vars::new();
            for param in &fn_def.params {
                this.param_scope(module, &mut params, param)
            }
            if let Some(ty) = fn_def.ret_type {
                this.type_scope(module, ty)
            }
            this.expr_scope(module, fn_def.expr)
        })
    }

    fn param_scope(&mut self, module: &Module, vars: &mut Vars, param: &Param) {
        let Param { pat, ty } = param;
        self.pat_scope(module, vars, *pat);
        if let Some(ty) = ty {
            self.type_scope(module, *ty)
        }
    }

    fn pat_scope(&mut self, module: &Module, vars: &mut Vars, id: PatId) {
        let pat = &module.data[id];
        match pat {
            Pat::Var(var) => self.insert_denotation(module, vars, *var, Denotation::Local(id)),
            pat => pat.walk_child_pats(|id| self.pat_scope(module, vars, id)),
        }
    }

    fn type_scope(&mut self, module: &Module, id: TypeId) {
        self.set_scope_of_type(id, self.scope);
        let ty = &module.data[id];
        ty.walk_child_types(|id| self.type_scope(module, id))
    }

    fn expr_scope(&mut self, module: &Module, id: ExprId) {
        self.set_scope_of_expr(id, self.scope);
        let expr = &module.data[id];
        match expr {
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Expr(expr) => self.expr_scope(module, *expr),
                        Stmt::Let { pat, ty, expr } => {
                            self.pat_scope(module, &mut Vars::new(), *pat);
                            if let Some(ty) = ty {
                                self.type_scope(module, *ty)
                            }
                            self.expr_scope(module, *expr)
                        }
                    }
                }
                if let Some(expr) = expr {
                    self.expr_scope(module, *expr)
                }
            }
            Expr::Lambda { params, expr } => {
                let mut vars = Vars::new();
                for param in params {
                    self.param_scope(module, &mut vars, &param)
                }
                self.expr_scope(module, *expr)
            }
            expr => expr.walk_child_exprs(|id| self.expr_scope(module, id)),
        }
    }
}
