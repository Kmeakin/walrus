use logos::Logos;

pub type Lexer<'a> = logos::Lexer<'a, TokenKind>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Logos)]
#[rustfmt::skip]
pub enum TokenKind {
    #[error] Error,
    #[regex(r"\s+")] Whitespace,
    #[regex(r"//[^\n]*")] LineComment,
    #[token(r"/*", block_comment)] BlockComment,

    #[token("break")] KwBreak,
    #[token("continue")] KwContinue,
    #[token("else")] KwElse,
    #[token("false")] KwFalse,
    #[token("fn")] KwFn,
    #[token("if")] KwIf,
    #[token("import")] KwImport,
    #[token("let")] KwLet,
    #[token("loop")] KwLoop,
    #[token("return")] KwReturn,
    #[token("true")] KwTrue,

    #[regex(r"[0-9_]+", priority = 0)] Int,
    #[regex(r"(\p{XID_Start}|_)\p{XID_Continue}*", priority = 1)] Ident,
    #[token("_", priority = 2)] Underscore,

    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LCurly,
    #[token("}")] RCurly,

    #[token(".")] Dot,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(":")] Colon,
    #[token("->")] ThinArrow,
    #[token("=>")] FatArrow,

    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,

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
        r"break continue else false fn if import let loop return true"
    );
    test_lex!(idents, "abc_DEF_123");
    test_lex!(int, "123_456_7890");
    test_lex!(symbols, "() {} . , ; -> =>");
    test_lex!(operators, "+ - * / = == != < <= > >=");
}
