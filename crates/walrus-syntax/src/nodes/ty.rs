use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Path(Path),
    Infer(Underscore),
    Paren(Paren<Type>),
    Tuple(Tuple<Type>),
    Fn {
        args: Paren<Punctuated0<Type, Comma>>,
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
