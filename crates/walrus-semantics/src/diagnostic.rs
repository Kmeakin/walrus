use crate::{
    hir::{ExprId, Var},
    scopes::Binding,
    ty::Type,
};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitError {
    Int(ParseIntError),
    Float(ParseFloatError),
    EscapeChar(char),
    UnicodeChar(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Diagnostic {
    BadLit(LitError),
    DuplicateBinding {
        first: Binding,
        second: Binding,
    },
    NotValue {
        var: Var,
        expr: ExprId,
        binding: Binding,
    },
    UnboundVar {
        var: Var,
        expr: ExprId,
    },
    IfBranchMismatch {
        then_branch: ExprId,
        else_branch: ExprId,
        then_ty: Type,
        else_ty: Type,
    },
}
