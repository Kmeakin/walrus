use nom::Parser;
use nom_supreme::parser_ext::ParserExt;
use walrus_lexer::Token;
use walrus_syntax::{nodes::*, tokens::*};

mod support;
mod tokens;

mod decl;
mod expr;
mod lit;
mod pat;
mod ty;

pub use self::{decl::*, expr::*, lit::*, pat::*, support::*, tokens::*, ty::*};

#[cfg(test)]
use crate::test_parse;

pub type Err = ();
pub type IResult<'i, T> = nom::IResult<Input<'i>, T, Err>;
pub type Input<'i> = &'i [Token<'i>];

#[cfg(test)]
#[macro_export]
macro_rules! test_parse {
    ($name:ident, $parser:expr, $src:expr) => {
        #[test]
        fn $name() { test_parse($parser, $src) }
    };
}

#[cfg(test)]
fn test_parse<T>(mut parser: impl Fn(Input) -> IResult<T>, src: &'static str)
where
    T: std::fmt::Debug,
{
    use insta::*;

    let tokens = walrus_lexer::lex(src)
        .filter(|token| !token.kind.is_trivia())
        .collect::<Vec<_>>()
        .clone();
    let got = parser.parse(&tokens);

    let mut settings = insta::Settings::new();
    settings.set_snapshot_path("../snapshots");
    settings.set_prepend_module_to_snapshot(false);
    settings.bind(|| assert_debug_snapshot!(got));
}
