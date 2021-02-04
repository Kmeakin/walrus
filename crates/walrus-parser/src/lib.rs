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
    clippy::single_match_else,
    clippy::wildcard_imports,
    dead_code,
    elided_lifetimes_in_paths
)]
#![feature(box_syntax)]

mod grammar;

use nom::Parser;
use walrus_syntax::SourceFile;

pub fn parse(src: &str) -> SourceFile {
    let tokens = walrus_lexer::lex(src)
        .filter(|token| !token.kind.is_trivia())
        .collect::<Vec<_>>()
        .clone();

    let (_input, source_file) = crate::grammar::source_file
        .parse(&tokens)
        .expect("Parser should not fail");
    source_file
}
