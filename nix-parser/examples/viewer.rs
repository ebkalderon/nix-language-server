use std::io::Read;
use std::process;
use std::{env, io};

use codespan::Files;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{emit, Config};
use nix_parser::ast::SourceFile;
use nix_parser::error::Errors;
use nix_parser::parser::parse_source_file_partial;

fn main() {
    let mut expr = String::new();
    io::stdin()
        .read_to_string(&mut expr)
        .expect("Failed to read expression from stdin");

    if env::args().find(|arg| arg == "--partial").is_some() {
        partial(&expr);
    } else {
        full(&expr);
    }
}

fn full(expr: &str) {
    let expr: SourceFile = expr.parse().unwrap_or_else(|e| {
        print_diagnostics(expr, e);
        process::exit(1);
    });

    println!("# Full AST:\n\n{:?}\n", expr);
    println!("# Display:\n\n{}", expr);
}

fn partial(expr: &str) {
    let partial = parse_source_file_partial(&expr).unwrap_or_else(|e| {
        print_diagnostics(expr, e);
        process::exit(1);
    });

    if let Some(ref expr) = partial.value() {
        if partial.has_errors() {
            println!("# Partial AST:\n\n{:?}\n", partial);
        } else {
            println!("# Full AST:\n\n{:?}\n", expr);
        }

        println!("# Display:\n\n{}", expr);
    } else {
        eprintln!("No expression value produced");
        if partial.has_errors() {
            println!("{:?}", partial.errors());
        }

        process::exit(1);
    }
}

fn print_diagnostics(expr: &str, errors: Errors) {
    let mut files = Files::new();
    let id = files.add("stdin", expr);
    let diagnostics = errors.to_diagnostics(id);

    let mut lock = StandardStream::stdout(ColorChoice::Auto);
    let config = Config::default();

    for diag in diagnostics {
        emit(&mut lock, &config, &files, &diag).unwrap();
    }
}
