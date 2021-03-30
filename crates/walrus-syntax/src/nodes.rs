use crate::tokens::*;
use walrus_lexer::Span;

mod decl;
mod expr;
mod lit;
mod pat;
mod stmt;
mod ty;

pub use self::{decl::*, expr::*, lit::*, pat::*, stmt::*, ty::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated0<Inner, Sep> {
    pub first: Option<Inner>,
    pub tail: Vec<(Sep, Inner)>,
    pub trail: Option<Sep>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated0NoTrail<Inner, Sep> {
    pub first: Option<Inner>,
    pub tail: Vec<(Sep, Inner)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated1<Inner, Sep> {
    pub first: Inner,
    pub tail: Vec<(Sep, Inner)>,
    pub trail: Option<Sep>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated1NoTrail<Inner, Sep> {
    pub first: Inner,
    pub tail: Vec<(Sep, Inner)>,
}

impl<Inner, Sep> Default for Punctuated0<Inner, Sep> {
    fn default() -> Self {
        Self {
            first: None,
            tail: Vec::new(),
            trail: None,
        }
    }
}

impl<Inner, Sep> Default for Punctuated0NoTrail<Inner, Sep> {
    fn default() -> Self {
        Self {
            first: None,
            tail: Vec::new(),
        }
    }
}

impl<Inner, Sep> Punctuated0<Inner, Sep> {
    pub fn iter(&self) -> impl Iterator<Item = &Inner> {
        let tail = self.tail.iter().map(|(_, item)| item);
        self.first.iter().chain(tail)
    }
}

impl<Inner, Sep> Punctuated0NoTrail<Inner, Sep> {
    pub fn iter(&self) -> impl Iterator<Item = &Inner> {
        let tail = self.tail.iter().map(|(_, item)| item);
        self.first.iter().chain(tail)
    }
}

impl<Inner, Sep> Punctuated1<Inner, Sep> {
    pub fn iter(&self) -> impl Iterator<Item = &Inner> {
        let tail = self.tail.iter().map(|(_, item)| item);
        std::iter::once(&self.first).chain(tail)
    }
}

impl<Inner, Sep> Punctuated1NoTrail<Inner, Sep> {
    pub fn iter(&self) -> impl Iterator<Item = &Inner> {
        let tail = self.tail.iter().map(|(_, item)| item);
        std::iter::once(&self.first).chain(tail)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Delimited<T, Open, Close> {
    pub open: Open,
    pub inner: Box<T>,
    pub close: Close,
}

pub type Paren<T> = Delimited<T, LParen, RParen>;
pub type Curly<T> = Delimited<T, LCurly, RCurly>;
pub type Tuple<T> = Paren<Punctuated0<T, Comma>>;

impl<T> Paren<T> {
    pub fn span(&self) -> Span { self.open.span.cover(self.close.span) }
}

impl<T> Curly<T> {
    pub fn span(&self) -> Span { self.open.span.cover(self.close.span) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub Ident);

impl Var {
    pub const fn span(&self) -> Span { self.0.span }
}
