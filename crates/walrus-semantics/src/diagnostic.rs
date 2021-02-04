use crate::{
    hir::{BinOp, ExprId, Field, PatId, TypeId, Var},
    scopes::Binding,
    ty::Type,
};
use either::Either;
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
        id: Either<ExprId, TypeId>,
        binding: Option<Binding>,
    },
    IfBranchMismatch {
        then_branch: ExprId,
        else_branch: ExprId,
        then_ty: Type,
        else_ty: Type,
    },
    ReturnNotInFn(ExprId),
    BreakNotInLoop(ExprId),
    TypeMismatch {
        id: Either<ExprId, PatId>,
        expected: Type,
        got: Type,
    },
    CalledNonFn {
        expr: ExprId,
        ty: Type,
    },
    ArgCountMismatch {
        expr: ExprId,
        ty: Type,
        expected: usize,
        got: usize,
    },
    InferenceFail(Either<ExprId, PatId>),
    CannotApplyBinop {
        lhs_type: Type,
        op: BinOp,
        rhs_type: Type,
    },
    NoSuchField {
        expr: ExprId,
        ty: Type,
        field: Field,
    },
    NoFields {
        expr: ExprId,
        ty: Type,
    },
}
