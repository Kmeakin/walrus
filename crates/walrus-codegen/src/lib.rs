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
#![feature(format_args_capture)]

mod codegen;
mod free_vars;

#[cfg(test)]
mod tests;

use walrus_semantics::{hir::HirData, scopes::Scopes, ty::InferenceResult};

use crate::codegen::*;

pub fn codegen(name: &str, hir: HirData, scopes: Scopes, types: InferenceResult) -> String {
    let llvm = Context::create();
    let builder = llvm.create_builder();
    let module = llvm.create_module(name);

    {
        let compiler = Compiler {
            llvm: &llvm,
            module,
            builder,

            hir,
            scopes,
            types,
        };
        compiler.codegen_module()
    }
}
