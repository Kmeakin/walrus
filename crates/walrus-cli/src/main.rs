#![feature(format_args_capture)]

use clap::{App, AppSettings, Arg};
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Stage {
    Check,
    Build,
    Run,
}

fn do_it() -> Result<(), Box<dyn Error>> {
    let root = crate_root::root().unwrap();
    let root = root.to_str().unwrap();
    let builtins_path = format!("{root}/walrus_builtins.c");

    let args = &[
        Arg::new("FILE").required(true),
        Arg::new("color")
            .long("color")
            .value_name("COLOR")
            .takes_value(true)
            .possible_values(&["always", "never"])
            .about("Emit colored diagnostics"),
        Arg::new("verbose")
            .short('v')
            .long("verbose")
            .about("Use verbose output"),
        Arg::new("debug")
            .short('d')
            .long("debug")
            .about("Dump debug information after each pass"),
    ];

    let matches = App::new("walrus")
        .setting(AppSettings::DeriveDisplayOrder)
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version("1.0")
        .author("Karl Meakin")
        .about("Compile Walrus programs")
        .subcommand(
            App::new("check")
                .about("Check the program for errors, but do not compile it")
                .args(args),
        )
        .subcommand(
            App::new("build")
                .about("Compile the program into an executable, but do not run it")
                .args(args),
        )
        .subcommand(
            App::new("run")
                .about("Compile and run the program")
                .args(args),
        )
        .get_matches();

    let (stage, matches) = match matches.subcommand() {
        Some(("check", matches)) => (Stage::Check, matches),
        Some(("build", matches)) => (Stage::Build, matches),
        Some(("run", matches)) => (Stage::Run, matches),
        _ => unreachable!(),
    };

    let file = &matches.value_of("FILE").unwrap();
    if !file.ends_with(".walrus") {
        return Err("Input filename should have `.walrus` extension".into());
    }
    let file = std::fs::canonicalize(file)?;
    let file = file.to_str().unwrap();

    let is_verbose = matches.is_present("verbose");
    let is_debug = matches.is_present("debug");
    let is_color = !matches!(matches.value_of("color"), Some("never"));

    let src = std::fs::read_to_string(file)?;
    let n_lines = src.lines().count();
    let start_time = std::time::Instant::now();
    let file_db = SimpleFile::new(file, &src);

    let syntax = walrus_parser::parse(&src);
    if is_debug {
        dbg!(&syntax);
    }

    let mut hir = walrus_semantics::hir::lower(&syntax);
    if is_debug {
        dbg!(&hir);
    }

    let mut scopes = walrus_semantics::scopes::scopes(&hir);
    if is_debug {
        dbg!(&scopes);
    }

    let mut types = walrus_semantics::ty::infer(hir.clone(), scopes.clone());
    if is_debug {
        dbg!(&types);
    }

    let mut diagnostics = Vec::new();
    diagnostics.extend(std::mem::take(&mut hir.diagnostics));
    diagnostics.extend(std::mem::take(&mut scopes.diagnostics));
    diagnostics.extend(std::mem::take(&mut types.diagnostics));

    let mut writer = termcolor::StandardStream::stderr(if is_color {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    });
    let config = Config::default();
    for diag in &diagnostics {
        let diag = diagnostics::render_diagnostic(&hir, diag);
        emit(&mut writer, &config, &file_db, &diag).expect("Could not emit diagnostic");
    }

    let n_errors = diagnostics.iter().filter(|diag| diag.is_error()).count();
    if n_errors > 0 {
        return Err(format!("Aborting compilation due to {n_errors} fatal errors").into());
    }

    if is_verbose {
        let end_time = std::time::Instant::now();
        let time = end_time.duration_since(start_time);
        println!("Checked {n_lines} lines in {time:?}");
    }
    if stage < Stage::Build {
        return Ok(());
    }

    let program_name = file.strip_suffix(".walrus").unwrap();
    let llvm_ir_path = format!("{program_name}.ll");
    let llvm_ir = walrus_codegen::codegen(program_name, hir.hir, scopes, types);
    std::fs::write(&llvm_ir_path, llvm_ir)?;

    let mut command = Command::new("clang");
    if is_color {
        command.arg("-fcolor-diagnostics");
    }
    command.args(&[&builtins_path, &llvm_ir_path, "-o", program_name]);
    let output = command.output()?;

    if is_verbose {
        eprintln!("Running command: {:?}", command);
    }

    if !output.status.success() {
        eprintln!("Failed running command: {:?}", command);
        eprintln!("{}", String::from_utf8(output.stderr).unwrap());
    }

    if is_verbose {
        let end_time = std::time::Instant::now();
        let time = end_time.duration_since(start_time);
        println!("Compiled {n_lines} lines in {time:?}");
    }

    if stage < Stage::Run {
        return Ok(());
    }

    let mut program = Command::new(&program_name);
    if is_verbose {
        eprintln!("Running program: {:?}", program)
    }
    program.status()?;

    Ok(())
}
