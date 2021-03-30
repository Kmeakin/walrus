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

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Var(var) => var.span(),
            Type::Infer(ty) => ty.span,
            Type::Paren(ty) => ty.span(),
            Type::Tuple(ty) => ty.span(),
            Type::Fn { args, ret } => args.span().cover(ret.span()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RetType {
    pub thin_arrow: ThinArrow,
    pub ty: Box<Type>,
}

impl RetType {
    pub fn span(&self) -> Span { self.thin_arrow.span.cover(self.ty.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ascription {
    pub colon: Colon,
    pub ty: Type,
}
