use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Var(Var),
    Ignore(Underscore),
    Paren(Paren<Pat>),
    Tuple(Tuple<Pat>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ascription: Option<Ascription>,
}
