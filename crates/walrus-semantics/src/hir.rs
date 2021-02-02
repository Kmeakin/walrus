use crate::syntax;
use la_arena::{Arena, ArenaMap, Idx};
use std::{fmt, iter::FromIterator, ops::Index};

pub type FnDefId = Idx<FnDef>;
pub type ImportId = Idx<Import>;
pub type ExprId = Idx<Expr>;
pub type TypeId = Idx<Type>;
pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub data: ModuleData,
    pub source: ModuleSource,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleData {
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub pat: Arena<Pat>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Item {
    FnDef(FnDefId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Import {
    pub path: Path,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub name: Name,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeId>,
    pub body: Block,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: PatId,
    pub ty: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Missing,
    Lit(Lit),
    Path(Path),
    Tuple(Vec<ExprId>),
    Field {
        base: ExprId,
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
    Block(Block),
    Loop(Block),
    If {
        test: ExprId,
        then: Block,
        else_: Option<ExprId>,
    },
    Break(Option<ExprId>),
    Return(Option<ExprId>),
    Continue,
    Lambda {
        params: Vec<Param>,
        body: ExprId,
    },
}
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    Missing,
    Tuple(u32),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<ExprId>,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Bind(Name),
    Wildcard,
    Tuple(Vec<PatId>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Path(Path),
    Infer,
    Tuple(Vec<TypeId>),
    Fn { params: Vec<TypeId>, ret: TypeId },
}
