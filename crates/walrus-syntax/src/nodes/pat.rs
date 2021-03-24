use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Lit(Lit),
    Var(Var),
    Ignore(Underscore),
    Paren(Paren<Self>),
    Tuple(Tuple<Self>),
    Struct(StructPat),
    Enum(EnumPat),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPat {
    pub name: Var,
    pub fields: Curly<Punctuated0<FieldPat, Comma>>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}
