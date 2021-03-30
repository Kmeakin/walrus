#![feature(format_args_capture)]

use clap::{App, Arg};
use codespan_reporting::{
    diagnostic::{Diagnostic as CodespanDiagnostic, Label},
    files::SimpleFile,
    term::{emit, termcolor::ColorChoice, Config},
};
use either::Either::*;
use std::error::Error;
use walrus_semantics::{
    builtins::BuiltinKind,
    diagnostic::Diagnostic,
    hir::{Field, Module},
    scopes::Denotation,
    ty::InferenceId,
};

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
    let matches = App::new("walrus")
        .version("1.0")
        .author("Karl Meakin")
        .about("Compile Walrus programs")
        .arg(Arg::with_name("FILE").required(true))
        .get_matches();

    let file = &matches.args["FILE"].vals[0].to_str().unwrap();
    let src = std::fs::read_to_string(file)?;

    let file_db = SimpleFile::new(file, &src);

    let syntax = walrus_parser::parse(&src);
    let mut hir = walrus_semantics::hir::lower(&syntax);
    let scopes = walrus_semantics::scopes::scopes(&hir);
    let types = walrus_semantics::ty::infer(hir.clone(), scopes.clone());

    let mut diagnostics = Vec::new();
    diagnostics.extend(std::mem::take(&mut hir.diagnostics));
    diagnostics.extend(scopes.diagnostics);
    diagnostics.extend(types.diagnostics);

    let mut writer = termcolor::StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    for diag in &diagnostics {
        let diag = render_diagnostic(&hir, diag);
        emit(&mut writer, &config, &file_db, &diag).expect("Could not emit diagnostic");
    }

    let n_errors = diagnostics.iter().filter(|diag| diag.is_error()).count();
    if n_errors > 0 {
        return Err(format!("Aborting compilation due to {n_errors} fatal errors").into());
    }

    Ok(())
}

fn render_diagnostic(module: &Module, diag: &Diagnostic) -> CodespanDiagnostic<()> {
    match diag {
        Diagnostic::UnnecessarySemicolon(semicolon) => CodespanDiagnostic::note()
            .with_message("Unnecessary semicolon")
            .with_labels(vec![
                Label::primary((), semicolon.span).with_message("This semicolon is not needed")
            ]),
        Diagnostic::BadLit { err, span } => CodespanDiagnostic::error()
            .with_message("Bad literal")
            .with_labels(vec![Label::primary((), *span).with_message(err.to_string())]),
        Diagnostic::DuplicateVar { first, second } => {
            let name = &module.hir[*first];
            let first = &module.source[*first];
            let second = &module.source[*second];
            CodespanDiagnostic::error()
                .with_message(format!("Name `{name}` is already defined"))
                .with_labels(vec![
                    Label::primary((), second.span()).with_message("Second definition"),
                    Label::secondary((), first.span()).with_message("First definition"),
                ])
        }
        Diagnostic::UnboundVar {
            var,
            mode,
            denotation,
        } => {
            let syntax = &module.source[*var];
            let name = &module.hir[*var];
            match denotation {
                None => CodespanDiagnostic::error()
                    .with_message(format!("Unbound variable: `{name}`"))
                    .with_labels(vec![Label::primary((), syntax.span())
                        .with_message(format!("Name `{name} is not defined"))]),
                Some(denotation) => {
                    let det = match denotation {
                        Denotation::Local(_) | Denotation::Fn(_) => "value",
                        Denotation::Struct(_) => "struct",
                        Denotation::Enum(_) => "enum",
                        Denotation::Builtin(b) => match b.kind() {
                            BuiltinKind::Type => "type",
                            BuiltinKind::Value => "function",
                        },
                    };
                    CodespanDiagnostic::error()
                        .with_message(format!("No {mode} named {name} in scope"))
                        .with_labels(vec![Label::primary((), syntax.span()).with_message(
                            format!(
                                "The name `{name}` is defined, but it is a {det}, not a {mode}"
                            ),
                        )])
                }
            }
        }
        Diagnostic::TypeMismatch { id, expected, got } => {
            let span = match id {
                Left(id) => module.source[*id].span(),
                Right(id) => module.source[*id].span(),
            };
            let expected = expected.to_string(&module.hir);
            let got = got.to_string(&module.hir);
            CodespanDiagnostic::error()
                .with_message("Type mismatch")
                .with_labels(vec![Label::primary((), span)
                    .with_message(format!("Expected {expected}, got {got}"))])
        }
        Diagnostic::InferenceFail(id) => {
            let span = match id {
                InferenceId::Var(id) => module.source[*id].span(),
                InferenceId::Expr(id) => module.source[*id].span(),
                InferenceId::Type(id) => module.source[*id].span(),
                InferenceId::Pat(id) => module.source[*id].span(),
            };
            let msg = match id {
                InferenceId::Var(_) => "variable",
                InferenceId::Expr(_) => "expr",
                InferenceId::Type(_) => "type",
                InferenceId::Pat(_) => "pattern",
            };
            CodespanDiagnostic::error()
                .with_message("Could not infer type")
                .with_labels(vec![
                    Label::primary((), span).with_message(format!("Could not infer type of {msg}"))
                ])
        }
        Diagnostic::ReturnNotInFn(expr) => {
            let span = module.source[*expr].span();
            CodespanDiagnostic::error()
                .with_message("Return outside of function")
                .with_labels(vec![Label::primary((), span)
                    .with_message("`return` is only allowed inside a function body")])
        }
        Diagnostic::BreakNotInLoop(expr) => {
            let span = module.source[*expr].span();
            CodespanDiagnostic::error()
                .with_message("Break outside of loop")
                .with_labels(vec![Label::primary((), span)
                    .with_message("`break` is only allowed inside a loop body")])
        }
        Diagnostic::ContinueNotInLoop(expr) => {
            let span = module.source[*expr].span();
            CodespanDiagnostic::error()
                .with_message("Continue outside of loop")
                .with_labels(vec![Label::primary((), span)
                    .with_message("`continue` is only allowed inside a loop body")])
        }
        Diagnostic::CalledNonFn { expr, ty } => {
            let span = module.source[*expr].span();
            let ty = ty.to_string(&module.hir);
            CodespanDiagnostic::error()
                .with_message("Called non function")
                .with_labels(vec![
                    Label::primary((), span).with_message(format!("{ty} is not a function type"))
                ])
        }
        Diagnostic::ArgCountMismatch {
            expr,
            ty,
            expected,
            got,
        } => {
            let span = module.source[*expr].span();
            let ty = ty.to_string(&module.hir);
            CodespanDiagnostic::error()
                .with_message("Called function with wrong number of arguments")
                .with_labels(vec![Label::primary((), span).with_message(format!(
                    "The function type {ty} expects {expected} arguments, but {got} were passed \
                     to it"
                ))])
        }
        Diagnostic::CannotApplyBinop {
            expr,
            lhs_type,
            op,
            rhs_type,
        } => {
            let span = module.source[*expr].span();
            let lhs_type = lhs_type.to_string(&module.hir);
            let rhs_type = rhs_type.to_string(&module.hir);
            CodespanDiagnostic::error()
                .with_message("Applied operator to incompatible types")
                .with_labels(vec![Label::primary((), span).with_message(format!(
                    "Cannot perform the operation {lhs_type} {op} {rhs_type}"
                ))])
        }
        Diagnostic::NotLValue { lhs } => {
            let span = module.source[*lhs].span();
            CodespanDiagnostic::error()
                .with_message("Not lvalue")
                .with_labels(vec![
                    Label::primary((), span).with_message("Can only assign to lvalue expressions")
                ])
        }
        Diagnostic::NoSuchField {
            parent,
            field,
            ty,
            possible_fields,
        } => {
            let span = match parent {
                Left(id) => module.source[*id].span(),
                Right(id) => module.source[*id].span(),
            };
            let ty = ty.to_string(&module.hir);
            let msg = match possible_fields {
                None => format!("The type {ty} has no fields"),
                Some(Left(fields)) if fields.is_empty() => format!("The type {ty} has no fields"),
                Some(Right(fields)) if *fields == 0 => format!("The type {ty} has no fields"),

                Some(Left(fields)) => format!(
                    "The type {ty} has the following possible fields: {}",
                    fields
                        .iter()
                        .map(|field| &module.hir[field.name])
                        .map(|field| format!(".{field}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Some(Right(fields)) => format!(
                    "The type {ty} has the following possible fields: {}",
                    (0_usize..*fields)
                        .map(|field| format!(".{field}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            };
            let field = match field {
                Field::Tuple(x) => x.to_string(),
                Field::Named(var) => module.hir[*var].as_str().to_string(),
            };
            CodespanDiagnostic::error()
                .with_message(format!("No `.{field}` field"))
                .with_labels(vec![Label::primary((), span).with_message(msg)])
        }
        Diagnostic::MissingField {
            id,
            field,
            ty,
            possible_fields,
        } => {
            let span = match id {
                Left(id) => module.source[*id].span(),
                Right(id) => module.source[*id].span(),
            };
            let ty = ty.to_string(&module.hir);
            let field = &module.hir[*field];
            let msg = format!(
                "The type {ty} has the following fields: {}",
                possible_fields
                    .iter()
                    .map(|field| &module.hir[field.name])
                    .map(|field| format!(".{field}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            CodespanDiagnostic::error()
                .with_message(format!("Missing field `{field}`"))
                .with_labels(vec![Label::primary((), span).with_message(msg)])
        }
        Diagnostic::FalliablePattern { id } => {
            let span = module.source[*id].span();
            CodespanDiagnostic::error()
                .with_message("Falliable pattern")
                .with_labels(vec![
                    Label::primary((), span).with_message("Patterns in this context must not fail")
                ])
        }
    }
}