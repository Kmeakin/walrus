use walrus_lexer::Span;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    String(String),
    Bool(BoolLit),
    Int(IntLit),
    Float(FloatLit),
    Char(CharLit),
}

impl Lit {
    pub const fn span(&self) -> Span {
        match self {
            Lit::String(string) => string.span,
            Lit::Bool(bool) => bool.span(),
            Lit::Int(int) => int.span(),
            Lit::Float(float) => float.span(),
            Lit::Char(char) => char.span(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BoolLit {
    True(KwTrue),
    False(KwFalse),
}

impl BoolLit {
    pub const fn span(&self) -> Span {
        match self {
            BoolLit::True(t) => t.span,
            BoolLit::False(f) => f.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntLit {
    Dec(DecInt),
    Bin(BinInt),
    Hex(HexInt),
}

impl IntLit {
    pub const fn span(&self) -> Span {
        match self {
            IntLit::Dec(i) => i.span,
            IntLit::Bin(i) => i.span,
            IntLit::Hex(i) => i.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatLit(pub Float);

impl FloatLit {
    pub const fn span(&self) -> Span {
        match self {
            Self(f) => f.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CharLit {
    Simple(SimpleChar),
    Escaped(EscapedChar),
    Unicode(UnicodeChar),
}

impl CharLit {
    pub const fn span(&self) -> Span {
        match self {
            CharLit::Simple(c) => c.span,
            CharLit::Escaped(c) => c.span,
            CharLit::Unicode(c) => c.span,
        }
    }
}
