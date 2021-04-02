use crate::{builtins::Builtin, diagnostic::Diagnostic, hir::*};
use arena::{Arena, ArenaMap, Idx};
use std::collections::HashMap;

pub fn scopes(module: &Module) -> Scopes {
    let mut scopes = Scopes::new();
    scopes.module_scope(&module.hir);
    scopes
}

pub type ScopeId = Idx<Scope>;
pub type Denotations = HashMap<Var, Denotation>;
type Vars = HashMap<Var, VarId>;
type LambdaDepth = u32;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Denotation {
    Local(VarId),
    Fn(FnDefId),
    Struct(StructDefId),
    Enum(EnumDefId),
    Builtin(Builtin),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub denotations: Denotations,
    pub lambda_depth: LambdaDepth,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scopes {
    scopes: Arena<Scope>,
    scope: ScopeId,
    scope_of_expr: ArenaMap<ExprId, ScopeId>,
    scope_of_type: ArenaMap<TypeId, ScopeId>,
    scope_of_pat: ArenaMap<PatId, ScopeId>,
    scope_of_var: ArenaMap<VarId, ScopeId>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Public interface
impl Scopes {
    pub fn scope_chain(&self, scope: ScopeId) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(Some(scope), move |&scope| self.scopes[scope].parent)
    }

    fn lookup_in_scope(&self, scope: ScopeId, var: &Var) -> Option<Denotation> {
        self.scope_chain(scope)
            .find_map(|scope| self.scopes[scope].denotations.get(var))
            .copied()
            .or_else(|| Builtin::lookup(var).map(Denotation::Builtin))
    }

    pub fn lookup_var(&self, id: VarId, var: &Var) -> Option<Denotation> {
        let scope = self.scope_of_var[id];
        self.lookup_in_scope(scope, var)
    }

    pub fn scope_of_var(&self, id: VarId) -> &Scope { &self.scopes[self.scope_of_var[id]] }
    pub fn scope_of_expr(&self, id: ExprId) -> &Scope { &self.scopes[self.scope_of_expr[id]] }
    pub fn scope_of_type(&self, id: TypeId) -> &Scope { &self.scopes[self.scope_of_type[id]] }
    pub fn scope_of_pat(&self, id: PatId) -> &Scope { &self.scopes[self.scope_of_pat[id]] }
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
            scope_of_pat: ArenaMap::default(),
            scope_of_var: ArenaMap::default(),
            diagnostics: Vec::new(),
        }
    }

    fn set_scope_of_expr(&mut self, id: ExprId, scope: ScopeId) {
        self.scope_of_expr.insert(id, scope);
    }

    fn set_scope_of_type(&mut self, id: TypeId, scope: ScopeId) {
        self.scope_of_type.insert(id, scope);
    }

    fn set_scope_of_pat(&mut self, id: PatId, scope: ScopeId) {
        self.scope_of_pat.insert(id, scope);
    }

    fn set_scope_of_var(&mut self, id: VarId, scope: ScopeId) {
        self.scope_of_var.insert(id, scope);
    }

    fn child_scope(&mut self, scope: ScopeId, lambda_depth: LambdaDepth) -> ScopeId {
        self.scopes.alloc(Scope {
            parent: Some(scope),
            denotations: Denotations::new(),
            lambda_depth,
        })
    }

    fn in_child_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        let old_scope = self.scope;
        self.scope = self.child_scope(old_scope, self.scopes[old_scope].lambda_depth);
        f(self);
        self.scope = old_scope;
    }

    fn in_child_lambda_scope(&mut self, mut f: impl FnMut(&mut Self)) {
        let old_scope = self.scope;
        self.scope = self.child_scope(old_scope, self.scopes[old_scope].lambda_depth + 1);
        f(self);
        self.scope = old_scope;
    }

    fn insert_denotation(
        &mut self,
        hir: &HirData,
        vars: &mut Vars,
        id: VarId,
        denotation: Denotation,
    ) {
        let var = hir[id].clone();
        if self.insert_var(hir, vars, id) {
            self.scopes[self.scope].denotations.insert(var, denotation);
        }
    }

    fn insert_var(&mut self, hir: &HirData, vars: &mut Vars, id: VarId) -> bool {
        self.set_scope_of_var(id, self.scope);
        let var = hir[id].clone();
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
    fn module_scope(&mut self, hir: &HirData) {
        let mut toplevel_defs = Vars::new();

        for decl in hir.decls() {
            match decl {
                Decl::Struct(id) => self.struct_def_scope(hir, &mut toplevel_defs, id),
                Decl::Enum(id) => self.enum_def_scope(hir, &mut toplevel_defs, id),
                Decl::Fn(id) => self.fn_def_scope(hir, &mut toplevel_defs, id),
            }
        }
    }

    fn struct_def_scope(&mut self, hir: &HirData, toplevel_defs: &mut Vars, id: StructDefId) {
        let struct_def = &hir[id];
        self.insert_denotation(hir, toplevel_defs, struct_def.name, Denotation::Struct(id));

        let mut fields = Vars::new();
        for field in &struct_def.fields {
            self.insert_var(hir, &mut fields, field.name);
            self.type_scope(hir, field.ty);
        }
    }

    fn enum_def_scope(&mut self, hir: &HirData, toplevel_defs: &mut Vars, id: EnumDefId) {
        let enum_def = &hir[id];
        self.insert_denotation(hir, toplevel_defs, enum_def.name, Denotation::Enum(id));

        let mut variants = Vars::new();
        for variant in &enum_def.variants {
            self.insert_var(hir, &mut variants, variant.name);
            let mut fields = Vars::new();
            for field in &variant.fields {
                self.insert_var(hir, &mut fields, field.name);
                self.type_scope(hir, field.ty);
            }
        }
    }

    fn fn_def_scope(&mut self, hir: &HirData, toplevel_defs: &mut Vars, id: FnDefId) {
        let fn_def = &hir[id];
        self.insert_denotation(hir, toplevel_defs, fn_def.name, Denotation::Fn(id));

        self.in_child_scope(|this| {
            let mut params = Vars::new();
            for param in &fn_def.params {
                this.param_scope(hir, &mut params, param)
            }
            if let Some(ty) = fn_def.ret_type {
                this.type_scope(hir, ty)
            }
            this.expr_scope(hir, fn_def.expr)
        })
    }

    fn param_scope(&mut self, hir: &HirData, vars: &mut Vars, param: &Param) {
        let Param { pat, ty } = param;
        self.pat_scope(hir, vars, *pat);
        if let Some(ty) = ty {
            self.type_scope(hir, *ty)
        }
    }

    fn pat_scope(&mut self, hir: &HirData, vars: &mut Vars, id: PatId) {
        self.set_scope_of_pat(id, self.scope);
        let pat = &hir[id];
        match pat {
            Pat::Var { var, .. } => {
                self.insert_denotation(hir, vars, *var, Denotation::Local(*var))
            }
            Pat::Struct { name, fields, .. } | Pat::Enum { name, fields, .. } => {
                self.set_scope_of_var(*name, self.scope);
                let mut seen_fields = Vars::new();
                for field in fields {
                    self.set_scope_of_var(field.name, self.scope);
                    self.insert_var(hir, &mut seen_fields, field.name);

                    match field {
                        FieldPat { pat: Some(pat), .. } => self.pat_scope(hir, vars, *pat),
                        FieldPat { name, pat: None } => {
                            self.insert_denotation(hir, vars, *name, Denotation::Local(*name))
                        }
                    }
                }
            }
            pat => pat.walk_child_pats(|id| self.pat_scope(hir, vars, id)),
        }
    }

    fn type_scope(&mut self, hir: &HirData, id: TypeId) {
        self.set_scope_of_type(id, self.scope);
        let ty = &hir[id];
        match ty {
            Type::Var(var) => self.set_scope_of_var(*var, self.scope),
            ty => ty.walk_child_types(|id| self.type_scope(hir, id)),
        }
    }

    fn expr_scope(&mut self, hir: &HirData, id: ExprId) {
        self.set_scope_of_expr(id, self.scope);
        let expr = &hir[id];
        match expr {
            Expr::Block { stmts, expr } => {
                self.in_child_scope(|this| this.block_scope(hir, stmts, *expr))
            }
            Expr::Lambda { params, expr } => self.in_child_lambda_scope(|this| {
                let mut vars = Vars::new();
                for param in params {
                    this.param_scope(hir, &mut vars, param)
                }
                this.expr_scope(hir, *expr)
            }),
            Expr::Match { test, cases } => {
                self.expr_scope(hir, *test);
                for case in cases {
                    self.in_child_scope(|this| {
                        let mut vars = Vars::new();
                        this.pat_scope(hir, &mut vars, case.pat);
                        this.expr_scope(hir, case.expr)
                    })
                }
            }
            Expr::Var(var) => self.set_scope_of_var(*var, self.scope),
            Expr::Struct { name, fields } | Expr::Enum { name, fields, .. } => {
                self.set_scope_of_var(*name, self.scope);
                let mut seen_fields = Vars::new();
                for field in fields {
                    self.insert_var(hir, &mut seen_fields, field.name);
                    self.set_scope_of_var(field.name, self.scope);
                    self.expr_scope(hir, field.val)
                }
            }
            expr => expr.walk_child_exprs(|id| self.expr_scope(hir, id)),
        }
    }

    fn block_scope(&mut self, hir: &HirData, stmts: &[Stmt], last: Option<ExprId>) {
        match stmts {
            [Stmt::Expr(expr), rest @ ..] => {
                self.expr_scope(hir, *expr);
                self.block_scope(hir, rest, last);
            }
            [Stmt::Let { pat, ty, expr }, rest @ ..] => self.in_child_scope(|this| {
                this.pat_scope(hir, &mut Vars::new(), *pat);
                if let Some(ty) = ty {
                    this.type_scope(hir, *ty)
                }
                this.expr_scope(hir, *expr);

                this.block_scope(hir, rest, last)
            }),
            [] => {
                if let Some(expr) = last {
                    self.expr_scope(hir, expr)
                }
            }
        }
    }
}
