#![allow(clippy::use_self)] // Gives a false positive on the Logos proc-macro
#![allow(clippy::non_ascii_literal)]

use logos::Logos;

pub type Lexer<'a> = logos::Lexer<'a, TokenKind>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[rustfmt::skip]
#[logos(subpattern DecDigit  = r"[0-9]")]
#[logos(subpattern DecDigit_ = r"[0-9_]")]
#[logos(subpattern BinDigit  = r"[0-1]")]
#[logos(subpattern BinDigit_ = r"[0-1_]")]
#[logos(subpattern HexDigit  = r"[0-9a-fA-F]")]
#[logos(subpattern HexDigit_ = r"[0-9a-fA-F_]")]
pub enum TokenKind {
    #[error] Error,
    #[regex(r"\s+")] Whitespace,
    #[regex(r"//[^\n]*")] LineComment,
    #[token(r"/*", block_comment)] BlockComment,

    #[token("break")] KwBreak,
    #[token("continue")] KwContinue,
    #[token("else")] KwElse,
    #[token("enum")] KwEnum,
    #[token("false")] KwFalse,
    #[token("fn")] KwFn,
    #[token("if")] KwIf,
    #[token("let")] KwLet,
    #[token("loop")] KwLoop,
    #[token("match")] KwMatch,
    #[token("mut")] KwMut,
    #[token("return")] KwReturn,
    #[token("struct")] KwStruct,
    #[token("true")] KwTrue,

    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*")] Ident,
    #[regex(r"(?&DecDigit)(?&DecDigit_)*")]         DecInt,
    #[regex(r"(0b|0B)(?&BinDigit)(?&BinDigit_)*")]  BinInt,
    #[regex(r"(0x|0X)(?&HexDigit)(?&HexDigit_)*")]  HexInt,
    #[regex(r"(?&DecDigit)(?&DecDigit_)*\.(?&DecDigit)(?&DecDigit_)*")] Float,
    #[regex(r"'[^']'")]                              SimpleChar,
    #[regex(r"'\\.'")]                               EscapedChar,
    #[regex(r"'\\(u|U)\{(?&HexDigit)(?&HexDigit_)*\}'")] UnicodeChar,

    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,

    #[token(".")] Dot,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(":")] Colon,
    #[token("::")] ColonColon,
    #[token("->")] ThinArrow,
    #[token("=>")] FatArrow,
    #[token("_")] Underscore,

    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,

    #[token("!")] Bang,
    #[token("&&")] AndAnd,
    #[token("||")] OrOr,

    #[token("=")] Eq,
    #[token("==")] EqEq,
    #[token("!=")] BangEq,
    #[token("<")] Less,
    #[token("<=")] LessEq,
    #[token(">")] Greater,
    #[token(">=")] GreaterEq,
}

fn block_comment(lexer: &mut Lexer) {
    const OPEN: &str = "/*";
    const CLOSE: &str = "*/";

    let mut level = 1;
    while level > 0 && !lexer.remainder().is_empty() {
        let src = lexer.remainder();

        if src.starts_with(OPEN) {
            level += 1;
            lexer.bump(OPEN.len());
        } else if src.starts_with(CLOSE) {
            level -= 1;
            lexer.bump(CLOSE.len());
        } else {
            lexer.bump(src.chars().next().unwrap().len_utf8())
        }
    }
}

impl TokenKind {
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            Self::Whitespace | Self::LineComment | Self::BlockComment
        )
    }
}

#[cfg(test)]
mod tests {
    use insta::*;

    fn test_lex(src: &str) {
        let got = crate::lex(src).collect::<Vec<_>>();

        let mut settings = insta::Settings::new();
        settings.set_snapshot_path("../snapshots");
        settings.set_prepend_module_to_snapshot(false);
        settings.bind(|| assert_debug_snapshot!(got));
    }

    macro_rules! test_lex {
        ($name:ident, $src:expr) => {
            #[test]
            fn $name() { test_lex($src); }
        };
    }

    test_lex!(empty_file, "");
    test_lex!(
        trivia,
        "// line comment
/* block comment */
"
    );
    test_lex!(
        keywords,
        r"break continue else enum false fn if let loop match mut return struct true"
    );
    test_lex!(idents, "abc_DEF_123");
    test_lex!(unicode_idents, "セイウチ");
    test_lex!(dec_int, "123_456_7890");
    test_lex!(bin_int, "0b101");
    test_lex!(hex_int, "0x1234_56789_abc_def");
    test_lex!(float, "123.456");
    test_lex!(simple_char, "'a'");
    test_lex!(escaped_char, r"'\n'");
    test_lex!(unicode_char, r"'\u{0a}'");
    test_lex!(symbols, "() {} . , ; : :: -> => _");
    test_lex!(operators, "+ - * / ! = == != < <= > >= || &&");
}
