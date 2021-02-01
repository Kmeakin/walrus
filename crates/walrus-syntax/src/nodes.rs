use crate::tokens::*;
use std::iter;

mod lit;

pub use self::lit::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated0<Inner, Sep> {
    pub first: Option<Inner>,
    pub tail: Vec<(Sep, Inner)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punctuated1<Inner, Sep> {
    pub first: Inner,
    pub tail: Vec<(Sep, Inner)>,
}

impl<Inner, Sep> Default for Punctuated0<Inner, Sep> {
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

impl<Inner, Sep> Punctuated1<Inner, Sep> {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var(pub Ident);
