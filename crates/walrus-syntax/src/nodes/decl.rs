use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn {
        kw_fn: KwFn,
        params: ParamList,
        ret: Option<RetType>,
        block: Block,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamList(pub Paren<Punctuated0<Param, Comma>>);
