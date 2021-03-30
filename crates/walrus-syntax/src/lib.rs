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

pub mod nodes;
pub mod tokens;

pub use crate::{nodes::*, tokens::*};
pub use walrus_lexer::Span;
