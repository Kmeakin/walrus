use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub decls: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    Fn(FnDef),
    Struct(StructDef),
    Enum(EnumDef),
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
pub struct StructDef {
    pub kw_struct: KwStruct,
    pub name: Var,
    pub fields: Curly<Punctuated0<StructField, Comma>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub kw_enum: KwEnum,
    pub name: Var,
    pub variants: Curly<Punctuated0<EnumVariant, Comma>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: Var,
    pub colon: Colon,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Var,
    pub fields: Curly<Punctuated0<StructField, Comma>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParamList(pub Paren<Punctuated0<Param, Comma>>);

impl ParamList {
    pub fn span(&self) -> Span { self.0.span() }
}
