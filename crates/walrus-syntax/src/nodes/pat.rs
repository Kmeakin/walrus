use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Lit(Lit),
    Var { kw_mut: Option<KwMut>, var: Var },
    Ignore(Underscore),
    Paren(Paren<Self>),
    Tuple(Tuple<Self>),
    Struct(StructPat),
    Enum(EnumPat),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Lit(lit) => lit.span(),
            Pat::Var { kw_mut, var } => match kw_mut {
                Some(kw_mut) => kw_mut.span.cover(var.span()),
                None => var.span(),
            },
            Pat::Ignore(pat) => pat.span,
            Pat::Paren(pat) => pat.span(),
            Pat::Tuple(pat) => pat.span(),
            Pat::Struct(pat) => pat.span(),
            Pat::Enum(pat) => pat.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPat {
    pub name: Var,
    pub fields: Curly<Punctuated0<FieldPat, Comma>>,
}

impl StructPat {
    pub fn span(&self) -> Span { self.name.span().cover(self.fields.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldPat {
    pub name: Var,
    pub pat: Option<(Colon, Pat)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumPat {
    pub name: Var,
    pub colon_colon: ColonColon,
    pub variant: Var,
    pub fields: Curly<Punctuated0<FieldPat, Comma>>,
}

impl EnumPat {
    pub fn span(&self) -> Span { self.name.span().cover(self.fields.span()) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}
