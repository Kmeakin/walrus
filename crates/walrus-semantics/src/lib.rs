#![warn(
    missing_copy_implementations,
    missing_debug_implementations,
    rust_2018_idioms,
    unused_qualifications,
    clippy::all,
    clippy::pedantic,
    clippy::nursery
)]
#![allow(
    clippy::must_use_candidate,
    clippy::wildcard_imports,
    clippy::enum_glob_use,
    clippy::module_name_repetitions,
    elided_lifetimes_in_paths
)]

mod diagnostic;
mod hir;
mod scopes;

pub(crate) use walrus_syntax as syntax;
