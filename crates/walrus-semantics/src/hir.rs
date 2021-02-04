use crate::{diagnostic::Diagnostic, syntax};
use arena::{Arena, ArenaMap, Idx};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use std::{fmt, ops::Index};

mod lower;
mod walk;

pub use self::lower::lower;

pub type FnDefId = Idx<FnDef>;
pub type ExprId = Idx<Expr>;
pub type TypeId = Idx<Type>;
pub type PatId = Idx<Pat>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Var(SmolStr);

impl Var {
    pub fn new(s: impl Into<SmolStr>) -> Self { Self(s.into()) }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:?}", self.0) }
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl From<syntax::Var> for Var {
    fn from(var: syntax::Var) -> Self { Self(var.0.text) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Decl>,
    pub data: ModuleData,
    pub source: ModuleSource,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleData {
    pub fn_defs: Arena<FnDef>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub pats: Arena<Pat>,
}

impl Index<FnDefId> for ModuleData {
    type Output = FnDef;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fn_defs[id] }
}
impl Index<ExprId> for ModuleData {
    type Output = Expr;
    fn index(&self, id: ExprId) -> &Self::Output { &self.exprs[id] }
}
impl Index<TypeId> for ModuleData {
    type Output = Type;
    fn index(&self, id: TypeId) -> &Self::Output { &self.types[id] }
}
impl Index<PatId> for ModuleData {
    type Output = Pat;
    fn index(&self, id: PatId) -> &Self::Output { &self.pats[id] }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleSource {
    pub fn_defs: ArenaMap<FnDefId, syntax::FnDef>,
    pub exprs: ArenaMap<ExprId, syntax::Expr>,
    pub types: ArenaMap<TypeId, syntax::Type>,
    pub pats: ArenaMap<PatId, syntax::Pat>,
}

impl Index<FnDefId> for ModuleSource {
    type Output = syntax::FnDef;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fn_defs[id] }
}
impl Index<ExprId> for ModuleSource {
    type Output = syntax::Expr;
    fn index(&self, id: ExprId) -> &Self::Output { &self.exprs[id] }
}
impl Index<TypeId> for ModuleSource {
    type Output = syntax::Type;
    fn index(&self, id: TypeId) -> &Self::Output { &self.types[id] }
}
impl Index<PatId> for ModuleSource {
    type Output = syntax::Pat;
    fn index(&self, id: PatId) -> &Self::Output { &self.pats[id] }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDefId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub name: Var,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeId>,
    pub expr: ExprId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: PatId,
    pub ty: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Lit),
    Var(Var),
    Tuple(Vec<ExprId>),
    Field {
        expr: ExprId,
        field: Field,
    },
    Unop {
        op: UnOp,
        expr: ExprId,
    },
    Binop {
        lhs: ExprId,
        op: BinOp,
        rhs: ExprId,
    },
    Call {
        func: ExprId,
        args: Vec<ExprId>,
    },
    Block {
        stmts: Vec<Stmt>,
        expr: Option<ExprId>,
    },
    Loop(ExprId),
    If {
        test: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
    },
    Break(Option<ExprId>),
    Return(Option<ExprId>),
    Continue,
    Lambda {
        params: Vec<Param>,
        expr: ExprId,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    Tuple(u32),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    Add,
    Sub,
    Not,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
}

impl From<syntax::UnaryOp> for UnOp {
    fn from(op: syntax::UnaryOp) -> Self {
        match op {
            syntax::UnaryOp::Add(_) => Self::Add,
            syntax::UnaryOp::Sub(_) => Self::Sub,
        }
    }
}
impl From<syntax::BinaryOp> for BinOp {
    fn from(op: syntax::BinaryOp) -> Self {
        match op {
            syntax::BinaryOp::Add(_) => Self::Add,
            syntax::BinaryOp::Sub(_) => Self::Sub,
            syntax::BinaryOp::Mul(_) => Self::Mul,
            syntax::BinaryOp::Div(_) => Self::Div,
            syntax::BinaryOp::Assign(_) => Self::Assign,
            syntax::BinaryOp::Eq(_) => Self::Eq,
            syntax::BinaryOp::NotEq(_) => Self::NotEq,
            syntax::BinaryOp::Less(_) => Self::Less,
            syntax::BinaryOp::LessEq(_) => Self::LessEq,
            syntax::BinaryOp::Greater(_) => Self::Greater,
            syntax::BinaryOp::GreaterEq(_) => Self::GreaterEq,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Let {
        pat: PatId,
        ty: Option<TypeId>,
        expr: ExprId,
    },
    Expr(ExprId),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    Bool(bool),
    Int(u32),
    Float(OrderedFloat<f32>),
    Char(char),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Var(Var),
    Ignore,
    Tuple(Vec<PatId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(Var),
    Infer,
    Tuple(Vec<TypeId>),
    Fn { params: Vec<TypeId>, ret: TypeId },
}
