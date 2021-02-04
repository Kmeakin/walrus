#![warn(
    clippy::all,
    clippy::nursery,
    clippy::pedantic,
    missing_copy_implementations,
    missing_debug_implementations,
    rust_2018_idioms,
    unused_qualifications
)]
#![allow(
    clippy::doc_markdown,
    clippy::enum_glob_use,
    clippy::module_name_repetitions,
    clippy::must_use_candidate,
    clippy::similar_names,
    clippy::wildcard_imports,
    elided_lifetimes_in_paths,
    dead_code
)]

mod token_kind;

use std::{convert::TryFrom, fmt, iter, ops::Range};
use text_size::{TextRange, TextSize};

use crate::token_kind::Lexer;
pub use crate::token_kind::TokenKind;

#[derive(Copy, Clone, PartialEq)]
pub struct Token<'a> {
    pub range: TextRange,
    pub kind: TokenKind,
    pub text: &'a str,
}

impl fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?} {:?}", self.range, self.kind, self.text)
    }
}

pub fn lex(src: &str) -> impl Iterator<Item = Token> {
    const ERROR: &str = "Programmer error: should have ensured that source was less than 4 GiB";

    let mut lexer = Lexer::new(src);
    iter::from_fn(move || {
        let kind = lexer.next()?;
        let text = lexer.slice();

        let range = {
            let Range { start, end } = lexer.span();
            let start = TextSize::try_from(start).expect(ERROR);
            let end = TextSize::try_from(end).expect(ERROR);
            TextRange::new(start, end)
        };
        Some(Token { kind, text, range })
    })
}
