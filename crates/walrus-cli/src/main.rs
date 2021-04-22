#![feature(format_args_capture)]

use clap::{App, Arg};
use codespan_reporting::{
    files::SimpleFile,
    term::{emit, termcolor::ColorChoice, Config},
};
use std::{error::Error, process::Command};

mod diagnostics;

fn main() {
    match do_it() {
        Ok(()) => (),
        Err(err) => {
            eprintln!("{}", err);
            std::process::exit(1);
        }
    }
}

fn do_it() -> Result<(), Box<dyn Error>> {
    let root = crate_root::root().unwrap();
    let root = root.to_str().unwrap();
    let builtins_path = format!("{root}/walrus_builtins.c");

    let matches = App::new("walrus")
        .version("1.0")
        .author("Karl Meakin")
        .about("Compile Walrus programs")
        .arg(Arg::with_name("FILE").required(true))
        .get_matches();

    let file = &matches.args["FILE"].vals[0].to_str().unwrap();
    if !file.ends_with(".walrus") {
        return Err("Input filename should have `.walrus` extension".into());
    }

    let src = std::fs::read_to_string(file)?;

    let file_db = SimpleFile::new(file, &src);

    let syntax = walrus_parser::parse(&src);
    let mut hir = walrus_semantics::hir::lower(&syntax);
    let mut scopes = walrus_semantics::scopes::scopes(&hir);
    let mut types = walrus_semantics::ty::infer(hir.clone(), scopes.clone());

    let mut diagnostics = Vec::new();
    diagnostics.extend(std::mem::take(&mut hir.diagnostics));
    diagnostics.extend(std::mem::take(&mut scopes.diagnostics));
    diagnostics.extend(std::mem::take(&mut types.diagnostics));

    let mut writer = termcolor::StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    for diag in &diagnostics {
        let diag = diagnostics::render_diagnostic(&hir, diag);
        emit(&mut writer, &config, &file_db, &diag).expect("Could not emit diagnostic");
    }

    let n_errors = diagnostics.iter().filter(|diag| diag.is_error()).count();
    if n_errors > 0 {
        return Err(format!("Aborting compilation due to {n_errors} fatal errors").into());
    }

    let name = file.strip_suffix(".walrus").unwrap();
    let llvm_ir_path = format!("{name}.ll");
    let llvm_ir = walrus_codegen::codegen(name, hir.hir, scopes, types);
    std::fs::write(&llvm_ir_path, llvm_ir)?;

    let mut command = Command::new("clang");
    command.args(&[
        "-fcolor-diagnostics",
        "-Wno-override-module",
        "-o",
        name,
        &builtins_path,
        &llvm_ir_path,
    ]);
    let output = command.output()?;

    if !output.status.success() {
        eprintln!("Failed running command: {:?}", command);
        eprintln!("{}", String::from_utf8(output.stderr).unwrap());
    }

    Ok(())
}
