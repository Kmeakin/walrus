use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
    Bool(BoolLit),
    Int(IntLit),
    Float(FloatLit),
    Char(CharLit),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BoolLit {
    True(KwTrue),
    False(KwFalse),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntLit {
    Dec(DecInt),
    Bin(BinInt),
    Hex(HexInt),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatLit(pub Float);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CharLit {
    Simple(SimpleChar),
    Escaped(EscapedChar),
    Unicode(UnicodeChar),
}
