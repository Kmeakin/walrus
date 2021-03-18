use crate::codegen::Compiler;
use arena::ArenaMap;
use walrus_semantics::{
    hir::{Expr, ExprId, PatId, VarId},
    scopes::Denotation,
};

pub type FreeVars = ArenaMap<VarId, ()>;

impl<'ctx> Compiler<'ctx> {
    pub fn free_vars(&self, expr: ExprId) -> FreeVars {
        let mut free_vars = FreeVars::new();
        self.free_vars_helper(&mut free_vars, expr);
        free_vars
    }

    fn free_vars_helper(&self, free_vars: &mut FreeVars, expr_id: ExprId) {
        let expr = &self.hir[expr_id];
        match expr {
            Expr::Var(var_id) => {
                let var = &self.hir[*var_id];
                let usage_scope = self.scopes.scope_of_expr(expr_id);
                let denotation = self.scopes.lookup_var(*var_id, var);
                if let Some(Denotation::Local(pat_id)) = denotation {
                    let defining_scope = self.scopes.scope_of_var(*var_id);
                    if usage_scope.lambda_depth != defining_scope.lambda_depth {
                        free_vars.insert(pat_id, ());
                    }
                }
            }
            expr => expr.walk_child_exprs(|expr| self.free_vars_helper(free_vars, expr)),
        }
    }
}
