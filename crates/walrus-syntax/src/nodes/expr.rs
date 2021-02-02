use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Lit),
    Path(Path),
    Paren(Paren<Expr>),
    Tuple(Tuple<Expr>),
    Lambda(LambdaExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    Field(FieldExpr),
    If(IfExpr),
    Return(ReturnExpr),
    Break(BreakExpr),
    Continue(ContinueExpr),
    Loop(LoopExpr),
    Block(Block),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: ArgList,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList(pub Paren<Punctuated0<Expr, Comma>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add(Plus),
    Sub(Minus),
    Mul(Star),
    Div(Slash),
    Assign(Eq),
    Eq(EqEq),
    NotEq(BangEq),
    Less(Less),
    LessEq(LessEq),
    Greater(Greater),
    GreaterEq(GreaterEq),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Add(Plus),
    Sub(Minus),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldExpr {
    pub expr: Box<Expr>,
    pub dot: Dot,
    pub var: Var,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub kw_if: KwIf,
    pub test_expr: Box<Expr>,
    pub then_block: Block,
    pub else_expr: Option<ElseExpr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElseExpr {
    ElseBlock {
        kw_else: KwElse,
        block: Block,
    },
    ElseIf {
        kw_else: KwElse,
        if_expr: Box<IfExpr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub lcurly: LCurly,
    pub stmts: Vec<Stmt>,
    pub expr: Box<Option<Expr>>,
    pub rcurly: RCurly,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnExpr {
    pub kw_return: KwReturn,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BreakExpr {
    pub kw_break: KwBreak,
    pub expr: Box<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ContinueExpr {
    pub kw_continue: KwContinue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaExpr {
    pub params: ParamList,
    pub fat_arrow: FatArrow,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoopExpr {
    pub kw_loop: KwLoop,
    pub block: Block,
}
