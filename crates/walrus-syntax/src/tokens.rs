use smol_str::SmolStr;
use text_size::TextRange as Span;
use walrus_lexer::{Token, TokenKind};

macro_rules! token {
    ($name:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub span: Span,
        }

        impl<'a> From<Token<'a>> for $name {
            fn from(token: Token) -> Self {
                debug_assert!(token.kind == TokenKind::$name);
                Self { span: token.range }
            }
        }
    };
}

macro_rules! token_with_text {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            pub span: Span,
            pub text: SmolStr,
        }

        impl<'a> From<Token<'a>> for $name {
            fn from(token: Token) -> Self {
                debug_assert!(token.kind == TokenKind::$name);
                Self {
                    span: token.range,
                    text: SmolStr::from(token.text),
                }
            }
        }
    };
}

token!(Error);

token!(Whitespace);
token!(LineComment);
token!(BlockComment);

token!(KwBreak);
token!(KwContinue);
token!(KwElse);
token!(KwFalse);
token!(KwFn);
token!(KwIf);
token!(KwImport);
token!(KwLet);
token!(KwLoop);
token!(KwReturn);
token!(KwTrue);

token_with_text!(Ident);
token_with_text!(DecInt);
token_with_text!(BinInt);
token_with_text!(HexInt);
token_with_text!(Float);
token_with_text!(SimpleChar);
token_with_text!(EscapedChar);
token_with_text!(UnicodeChar);

token!(LParen);
token!(RParen);
token!(LCurly);
token!(RCurly);
token!(Colon);
token!(Comma);
token!(Dot);
token!(Eq);
token!(FatArrow);
token!(Semicolon);
token!(ThinArrow);
token!(Underscore);
