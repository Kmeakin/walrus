use crate::{
    hir::{Binop, ExprId, Field, PatId, StructField, VarId},
    scopes::Denotation,
    ty::{FnType, InferenceId, Type, VarMode},
};
use either::Either;
use std::{
    fmt,
    num::{ParseFloatError, ParseIntError},
};
use walrus_syntax::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitError {
    Int(ParseIntError),
    Float(ParseFloatError),
    EscapeChar(char),
    UnicodeChar(u32),
}

impl fmt::Display for LitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitError::Int(e) => write!(f, "{e}"),
            LitError::Float(e) => write!(f, "{e}"),
            LitError::EscapeChar(c) => write!(f, "'{c}' is not an escape character"),
            LitError::UnicodeChar(i) => write!(f, "{i} is not a valid unicode scalar"),
        }
    }
}

impl fmt::Display for VarMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarMode::Value => write!(f, "value"),
            VarMode::Type => write!(f, "type"),
            VarMode::Struct => write!(f, "struct"),
            VarMode::Enum => write!(f, "enum"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Diagnostic {
    UnnecessarySemicolon(crate::syntax::Semicolon),
    BadLit {
        err: LitError,
        span: Span,
    },
    DuplicateVar {
        first: VarId,
        second: VarId,
    },
    UnboundVar {
        var: VarId,
        mode: VarMode,
        denotation: Option<Denotation>,
    },
    TypeMismatch {
        id: Either<ExprId, PatId>,
        expected: Type,
        got: Type,
    },
    InferenceFail(InferenceId),
    ReturnNotInFn(ExprId),
    BreakNotInLoop(ExprId),
    ContinueNotInLoop(ExprId),
    CalledNonFn {
        expr: ExprId,
        ty: Type,
    },
    ArgCountMismatch {
        expr: ExprId,
        ty: FnType,
        expected: usize,
        got: usize,
    },
    CannotApplyBinop {
        expr: ExprId,
        lhs_type: Type,
        op: Binop,
        rhs_type: Type,
    },
    NotLValue {
        lhs: ExprId,
    },
    NoSuchField {
        parent: Either<ExprId, PatId>,
        field: Field,
        ty: Type,
        possible_fields: Option<Either<Vec<StructField>, usize>>,
    },
    MissingField {
        id: Either<ExprId, PatId>,
        field: VarId,
        ty: Type,
        possible_fields: Vec<StructField>,
    },
    FalliablePattern {
        id: PatId,
    },
}

impl Diagnostic {
    pub fn is_error(&self) -> bool { !matches!(self, Self::UnnecessarySemicolon(_)) }
}
