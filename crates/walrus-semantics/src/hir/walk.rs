use super::*;

impl Expr {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Expr::Lit(_) | Expr::Var(_) | Expr::Continue => {}
            Expr::Tuple(exprs) => exprs.iter().for_each(|expr| f(*expr)),
            Expr::Field { expr, .. }
            | Expr::Unop { expr, .. }
            | Expr::Lambda { expr, .. }
            | Expr::Loop(expr) => f(*expr),
            Expr::Binop { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Expr::Call { func, args } => {
                f(*func);
                args.iter().for_each(|expr| f(*expr))
            }
            Expr::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Let { expr, .. } | Stmt::Expr(expr) => f(*expr),
                    }
                }
                if let Some(expr) = expr {
                    f(*expr)
                }
            }
            Expr::If {
                test,
                then_branch,
                else_branch,
            } => {
                f(*test);
                f(*then_branch);
                if let Some(else_branch) = else_branch {
                    f(*else_branch)
                }
            }
            Expr::Break(expr) | Expr::Return(expr) => {
                if let Some(expr) = expr {
                    f(*expr)
                }
            }
        }
    }
}

impl Type {
    pub fn walk_child_types(&self, mut f: impl FnMut(TypeId)) {
        match self {
            Self::Var(_) | Self::Infer => {}
            Self::Tuple(pats) => pats.iter().copied().for_each(f),
            Self::Fn { params, ret } => {
                params.iter().copied().for_each(|ty| f(ty));
                f(*ret);
            }
        }
    }
}

impl Pat {
    pub fn walk_child_pats(&self, mut f: impl FnMut(PatId)) {
        match self {
            Self::Var(_) | Self::Ignore => {}
            Self::Tuple(pats) => pats.iter().copied().for_each(f),
        }
    }
}
