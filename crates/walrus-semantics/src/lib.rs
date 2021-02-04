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
#![feature(format_args_capture)]

mod diagnostic;
mod hir;
mod scopes;
mod ty;

pub(crate) use walrus_syntax as syntax;
