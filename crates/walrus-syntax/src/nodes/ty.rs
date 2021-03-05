use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Var(Var),
    Infer(Underscore),
    Paren(Paren<Self>),
    Tuple(Tuple<Self>),
    Fn {
        args: Paren<Punctuated0<Self, Comma>>,
        ret: RetType,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RetType {
    pub thin_arrow: ThinArrow,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ascription {
    pub colon: Colon,
    pub ty: Type,
}
