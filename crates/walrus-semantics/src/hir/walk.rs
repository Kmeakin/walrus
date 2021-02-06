use super::*;

impl Expr {
    pub fn walk_child_exprs(&self, mut f: impl FnMut(ExprId)) {
        match self {
            Self::Lit(_) | Self::Var(_) | Self::Continue => {}
            Self::Tuple(exprs) => exprs.iter().for_each(|expr| f(*expr)),
            Self::Field { expr, .. }
            | Self::Unop { expr, .. }
            | Self::Lambda { expr, .. }
            | Self::Loop(expr) => f(*expr),
            Self::Binop { lhs, rhs, .. } => {
                f(*lhs);
                f(*rhs);
            }
            Self::Call { func, args } => {
                f(*func);
                args.iter().for_each(|expr| f(*expr))
            }
            Self::Block { stmts, expr } => {
                for stmt in stmts {
                    match stmt {
                        Stmt::Let { expr, .. } | Stmt::Expr(expr) => f(*expr),
                    }
                }
                if let Some(expr) = expr {
                    f(*expr)
                }
            }
            Self::If {
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
            Self::Break(expr) | Self::Return(expr) => {
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
    pub fn walk_child_pats(&self, f: impl FnMut(PatId)) {
        match self {
            Self::Var(_) | Self::Ignore => {}
            Self::Tuple(pats) => pats.iter().copied().for_each(f),
        }
    }
}
