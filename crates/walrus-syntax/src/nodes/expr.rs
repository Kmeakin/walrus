use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Lit(Lit),
    Var(Var),
    Paren(Paren<Self>),
    Tuple(Tuple<Self>),
    Lambda(LambdaExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Call(CallExpr),
    Field(FieldExpr),
    Struct(StructExpr),
    Enum(EnumExpr),
    If(IfExpr),
    Match(MatchExpr),
    Return(ReturnExpr),
    Break(BreakExpr),
    Continue(ContinueExpr),
    Loop(LoopExpr),
    Block(Block),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(lit) => lit.span(),
            Expr::Var(var) => var.span(),
            Expr::Paren(expr) => expr.span(),
            Expr::Tuple(expr) => expr.span(),
            Expr::Lambda(expr) => expr.span(),
            Expr::Unary(expr) => expr.span(),
            Expr::Binary(expr) => expr.span(),
            Expr::Call(expr) => expr.span(),
            Expr::Field(expr) => expr.span(),
            Expr::Struct(expr) => expr.span(),
            Expr::Enum(expr) => expr.span(),
            Expr::If(expr) => expr.span(),
            Expr::Match(expr) => expr.span(),
            Expr::Return(expr) => expr.span(),
            Expr::Break(expr) => expr.span(),
            Expr::Continue(expr) => expr.span(),
            Expr::Loop(expr) => expr.span(),
            Expr::Block(expr) => expr.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub func: Box<Expr>,
    pub args: ArgList,
}

impl CallExpr {
    pub fn span(&self) -> Span { self.func.span().cover(self.args.0.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgList(pub Paren<Punctuated0<Expr, Comma>>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub lhs: Box<Expr>,
    pub op: Binop,
    pub rhs: Box<Expr>,
}

impl BinaryExpr {
    pub fn span(&self) -> Span { self.lhs.span().cover(self.rhs.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryExpr {
    pub op: Unop,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn span(&self) -> Span { self.op.span().cover(self.expr.span()) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Binop {
    Or(OrOr),
    And(AndAnd),
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
pub enum Unop {
    Not(Bang),
    Add(Plus),
    Sub(Minus),
}

impl Unop {
    pub fn span(&self) -> Span {
        match self {
            Unop::Not(t) => t.span,
            Unop::Add(t) => t.span,
            Unop::Sub(t) => t.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldExpr {
    pub base: Box<Expr>,
    pub dot: Dot,
    pub field: Field,
}

impl FieldExpr {
    pub fn span(&self) -> Span { self.base.span().cover(self.field.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    Tuple(DecInt),
    Named(Var),
}

impl Field {
    pub fn span(&self) -> Span {
        match self {
            Field::Tuple(x) => x.span,
            Field::Named(x) => x.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructExpr {
    pub name: Var,
    pub fields: Curly<Punctuated0<FieldInit, Comma>>,
}

impl StructExpr {
    pub fn span(&self) -> Span { self.name.span().cover(self.fields.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumExpr {
    pub name: Var,
    pub colon_colon: ColonColon,
    pub variant: Var,
    pub fields: Curly<Punctuated0<FieldInit, Comma>>,
}

impl EnumExpr {
    pub fn span(&self) -> Span { self.name.span().cover(self.fields.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldInit {
    pub name: Var,
    pub colon: Colon,
    pub val: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub kw_if: KwIf,
    pub test_expr: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Option<ElseExpr>,
}

impl IfExpr {
    pub fn span(&self) -> Span {
        match &self.else_branch {
            Some(else_branch) => self.kw_if.span.cover(else_branch.span()),
            None => self.kw_if.span.cover(self.then_branch.span()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElseExpr {
    ElseBlock { kw_else: KwElse, block: Box<Expr> },
    ElseIf { kw_else: KwElse, if_expr: Box<Expr> },
}

impl ElseExpr {
    pub fn span(&self) -> Span {
        match self {
            ElseExpr::ElseBlock { kw_else, block } => kw_else.span.cover(block.span()),
            ElseExpr::ElseIf { kw_else, if_expr } => kw_else.span.cover(if_expr.span()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchExpr {
    pub kw_match: KwMatch,
    pub test_expr: Box<Expr>,
    pub cases: Curly<Punctuated0<MatchCase, Comma>>,
}

impl MatchExpr {
    pub fn span(&self) -> Span { self.kw_match.span.cover(self.cases.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchCase {
    pub pat: Pat,
    pub fat_arrow: FatArrow,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub lcurly: LCurly,
    pub stmts: Vec<Stmt>,
    pub expr: Box<Option<Expr>>,
    pub rcurly: RCurly,
}

impl Block {
    pub fn span(&self) -> Span { self.lcurly.span.cover(self.rcurly.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnExpr {
    pub kw_return: KwReturn,
    pub expr: Option<Box<Expr>>,
}

impl ReturnExpr {
    pub fn span(&self) -> Span {
        match &self.expr {
            Some(expr) => self.kw_return.span.cover(expr.span()),
            None => self.kw_return.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BreakExpr {
    pub kw_break: KwBreak,
    pub expr: Option<Box<Expr>>,
}

impl BreakExpr {
    pub fn span(&self) -> Span {
        match &self.expr {
            Some(expr) => self.kw_break.span.cover(expr.span()),
            None => self.kw_break.span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ContinueExpr {
    pub kw_continue: KwContinue,
}

impl ContinueExpr {
    pub fn span(&self) -> Span { self.kw_continue.span }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaExpr {
    pub params: ParamList,
    pub fat_arrow: FatArrow,
    pub expr: Box<Expr>,
}

impl LambdaExpr {
    pub fn span(&self) -> Span { self.params.span().cover(self.expr.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoopExpr {
    pub kw_loop: KwLoop,
    pub expr: Box<Expr>,
}

impl LoopExpr {
    pub fn span(&self) -> Span { self.kw_loop.span.cover(self.expr.span()) }
}
