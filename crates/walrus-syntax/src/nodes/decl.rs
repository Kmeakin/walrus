use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDef),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDef {
    pub kw_fn: KwFn,
    pub name: Var,
    pub params: ParamList,
    pub ret: Option<RetType>,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamList(pub Paren<Punctuated0<Param, Comma>>);
