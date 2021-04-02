use crate::{diagnostic::Diagnostic, syntax};
use arena::{Arena, ArenaMap, Idx};
use derive_more::Display;
use smol_str::SmolStr;
use std::{fmt, ops::Index};

mod lower;
mod walk;

pub use self::lower::lower;

pub type VarId = Idx<Var>;
pub type FnDefId = Idx<FnDef>;
pub type StructDefId = Idx<StructDef>;
pub type EnumDefId = Idx<EnumDef>;
pub type ExprId = Idx<Expr>;
pub type TypeId = Idx<Type>;
pub type PatId = Idx<Pat>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub is_mut: bool,
    var: SmolStr,
}

impl Var {
    pub fn new(s: impl Into<SmolStr>) -> Self {
        Self {
            is_mut: false,
            var: s.into(),
        }
    }
    pub fn new_with_mutability(s: impl Into<SmolStr>, is_mut: bool) -> Self {
        Self {
            is_mut,
            var: s.into(),
        }
    }

    pub fn as_str(&self) -> &str { &self.var }
    pub const fn as_string(&self) -> &SmolStr { &self.var }

    #[allow(clippy::missing_const_for_fn)]
    pub fn into_string(self) -> SmolStr { self.var }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:?}", self.var) }
}
impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.var) }
}

impl From<syntax::Var> for Var {
    fn from(var: syntax::Var) -> Self { Self::new(var.0.text) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub decls: Vec<Decl>,
    pub hir: HirData,
    pub source: ModuleSource,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct HirData {
    pub vars: Arena<Var>,
    pub fn_defs: Arena<FnDef>,
    pub struct_defs: Arena<StructDef>,
    pub enum_defs: Arena<EnumDef>,
    pub exprs: Arena<Expr>,
    pub types: Arena<Type>,
    pub pats: Arena<Pat>,
}

impl HirData {
    pub fn decls(&self) -> impl Iterator<Item = Decl> {
        self.fn_defs
            .iter_ids()
            .map(Decl::Fn)
            .chain(self.struct_defs.iter_ids().map(Decl::Struct))
            .chain(self.enum_defs.iter_ids().map(Decl::Enum))
    }
}

impl Index<VarId> for HirData {
    type Output = Var;
    fn index(&self, id: VarId) -> &Self::Output { &self.vars[id] }
}
impl Index<FnDefId> for HirData {
    type Output = FnDef;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fn_defs[id] }
}
impl Index<StructDefId> for HirData {
    type Output = StructDef;
    fn index(&self, id: StructDefId) -> &Self::Output { &self.struct_defs[id] }
}
impl Index<EnumDefId> for HirData {
    type Output = EnumDef;
    fn index(&self, id: EnumDefId) -> &Self::Output { &self.enum_defs[id] }
}
impl Index<ExprId> for HirData {
    type Output = Expr;
    fn index(&self, id: ExprId) -> &Self::Output { &self.exprs[id] }
}
impl Index<TypeId> for HirData {
    type Output = Type;
    fn index(&self, id: TypeId) -> &Self::Output { &self.types[id] }
}
impl Index<PatId> for HirData {
    type Output = Pat;
    fn index(&self, id: PatId) -> &Self::Output { &self.pats[id] }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleSource {
    pub vars: ArenaMap<VarId, syntax::Var>,
    pub fn_defs: ArenaMap<FnDefId, syntax::FnDef>,
    pub struct_defs: ArenaMap<StructDefId, syntax::StructDef>,
    pub enum_defs: ArenaMap<EnumDefId, syntax::EnumDef>,
    pub exprs: ArenaMap<ExprId, syntax::Expr>,
    pub types: ArenaMap<TypeId, syntax::Type>,
    pub pats: ArenaMap<PatId, syntax::Pat>,
}
impl Index<VarId> for ModuleSource {
    type Output = syntax::Var;
    fn index(&self, id: VarId) -> &Self::Output { &self.vars[id] }
}
impl Index<FnDefId> for ModuleSource {
    type Output = syntax::FnDef;
    fn index(&self, id: FnDefId) -> &Self::Output { &self.fn_defs[id] }
}
impl Index<StructDefId> for ModuleSource {
    type Output = syntax::StructDef;
    fn index(&self, id: StructDefId) -> &Self::Output { &self.struct_defs[id] }
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
    Struct(StructDefId),
    Enum(EnumDefId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub name: VarId,
    pub params: Vec<Param>,
    pub ret_type: Option<TypeId>,
    pub expr: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub name: VarId,
    pub fields: Vec<StructField>,
}

impl StructDef {
    pub fn lookup_field(&self, hir: &HirData, name: VarId) -> Option<(usize, &StructField)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, field)| hir[field.name] == hir[name])
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: VarId,
    pub ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub name: VarId,
    pub variants: Vec<EnumVariant>,
}

impl EnumDef {
    pub fn get_variant(&self, hir: &HirData, variant: VarId) -> Option<(usize, &EnumVariant)> {
        self.variants
            .iter()
            .enumerate()
            .find(|(_, v)| hir[variant] == hir[v.name])
    }

    pub fn len(&self) -> usize { self.variants.len() }
    pub fn is_empty(&self) -> bool { self.len() == 0 }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: VarId,
    pub fields: Vec<StructField>,
}

impl EnumVariant {
    pub fn lookup_field(&self, hir: &HirData, name: VarId) -> Option<(usize, &StructField)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, field)| hir[field.name] == hir[name])
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: PatId,
    pub ty: Option<TypeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Lit),
    Var(VarId),
    Tuple(Vec<ExprId>),
    Field {
        expr: ExprId,
        field: Field,
    },
    Struct {
        name: VarId,
        fields: Vec<FieldInit>,
    },
    Enum {
        name: VarId,
        variant: VarId,
        fields: Vec<FieldInit>,
    },
    Unop {
        op: Unop,
        expr: ExprId,
    },
    Binop {
        lhs: ExprId,
        op: Binop,
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
    Match {
        test: ExprId,
        cases: Vec<MatchCase>,
    },
    Break(Option<ExprId>),
    Return(Option<ExprId>),
    Continue,
    Lambda {
        params: Vec<Param>,
        expr: ExprId,
    },
}

impl Expr {
    pub fn is_lvalue(&self, hir: &HirData) -> bool {
        match self {
            Expr::Var(_) => true,
            Expr::Field { expr, .. } => hir[*expr].is_lvalue(hir),
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MatchCase {
    pub pat: PatId,
    pub expr: ExprId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FieldInit {
    pub name: VarId,
    pub val: ExprId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Field {
    Tuple(u32),
    Named(VarId),
}

#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Unop {
    #[display(fmt = "!")]
    Not,
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
}

#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Binop {
    Lazy(LazyBinop),
    Arithmetic(ArithmeticBinop),
    Cmp(CmpBinop),
    Assign,
}

#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LazyBinop {
    #[display(fmt = "||")]
    Or,
    #[display(fmt = "&&")]
    And,
}

#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ArithmeticBinop {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "-")]
    Sub,
    #[display(fmt = "*")]
    Mul,
    #[display(fmt = "/")]
    Div,
}

#[derive(Debug, Display, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CmpBinop {
    #[display(fmt = "==")]
    Eq,
    #[display(fmt = "!=")]
    NotEq,
    #[display(fmt = "<")]
    Less,
    #[display(fmt = "<=")]
    LessEq,
    #[display(fmt = ">")]
    Greater,
    #[display(fmt = ">=")]
    GreaterEq,
}

impl From<syntax::Unop> for Unop {
    fn from(op: syntax::Unop) -> Self {
        match op {
            syntax::Unop::Not(_) => Self::Not,
            syntax::Unop::Add(_) => Self::Add,
            syntax::Unop::Sub(_) => Self::Sub,
        }
    }
}
impl From<syntax::Binop> for Binop {
    fn from(op: syntax::Binop) -> Self {
        match op {
            syntax::Binop::Or(_) => Self::Lazy(LazyBinop::Or),
            syntax::Binop::And(_) => Self::Lazy(LazyBinop::And),
            syntax::Binop::Add(_) => Self::Arithmetic(ArithmeticBinop::Add),
            syntax::Binop::Sub(_) => Self::Arithmetic(ArithmeticBinop::Sub),
            syntax::Binop::Mul(_) => Self::Arithmetic(ArithmeticBinop::Mul),
            syntax::Binop::Div(_) => Self::Arithmetic(ArithmeticBinop::Div),
            syntax::Binop::Assign(_) => Self::Assign,
            syntax::Binop::Eq(_) => Self::Cmp(CmpBinop::Eq),
            syntax::Binop::NotEq(_) => Self::Cmp(CmpBinop::NotEq),
            syntax::Binop::Less(_) => Self::Cmp(CmpBinop::Less),
            syntax::Binop::LessEq(_) => Self::Cmp(CmpBinop::LessEq),
            syntax::Binop::Greater(_) => Self::Cmp(CmpBinop::Greater),
            syntax::Binop::GreaterEq(_) => Self::Cmp(CmpBinop::GreaterEq),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Bool(bool),
    Int(u32),
    Float(f32),
    Char(char),
    String(SmolStr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Lit(Lit),
    Var {
        is_mut: bool,
        var: VarId,
    },
    Ignore,
    Tuple(Vec<PatId>),
    Struct {
        name: VarId,
        fields: Vec<FieldPat>,
    },
    Enum {
        name: VarId,
        variant: VarId,
        fields: Vec<FieldPat>,
    },
}

impl Pat {
    pub fn is_infalliable(&self, hir: &HirData) -> bool {
        match self {
            Self::Lit(_) | Self::Var { .. } | Self::Ignore => true,
            Self::Tuple(pats) => pats.iter().all(|pat| hir[*pat].is_infalliable(hir)),
            Self::Struct { fields, .. } => fields
                .iter()
                .all(|field| field.pat.map_or(true, |pat| hir[pat].is_infalliable(hir))),
            Self::Enum { .. } => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FieldPat {
    pub name: VarId,
    pub pat: Option<PatId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(VarId),
    Infer,
    Tuple(Vec<TypeId>),
    Fn { params: Vec<TypeId>, ret: TypeId },
}
