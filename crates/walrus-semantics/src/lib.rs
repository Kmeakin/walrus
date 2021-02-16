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
#![feature(format_args_capture, or_patterns)]

mod builtins;
mod diagnostic;
pub mod hir;
pub mod scopes;
pub mod ty;

pub(crate) use walrus_syntax as syntax;
