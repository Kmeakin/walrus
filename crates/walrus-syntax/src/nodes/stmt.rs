use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Expr {
        expr: Expr,
        semicolon: Option<Semicolon>,
    },
    Let {
        kw_let: KwLet,
        pat: Pat,
        ascription: Option<Ascription>,
        eq: Eq,
        expr: Expr,
        semicolon: Semicolon,
    },
    Semicolon(Semicolon),
}
