use std::io::{Read, Write};
use std::time::Instant;
use std::{env, io, process};

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

    let verbose = env::args()
        .find(|arg| arg == "-v" || arg == "--verbose")
        .is_some();

    if env::args().find(|arg| arg == "--partial").is_some() {
        partial(&expr, verbose);
    } else {
        full(&expr, verbose);
    }
}

fn full(expr: &str, verbose: bool) {
    let start = Instant::now();
    let expr: SourceFile = expr.parse().unwrap_or_else(|e| {
        print_diagnostics(expr, e);
        process::exit(1);
    });
    let end = start.elapsed();

    if verbose {
        let stdout = io::stdout();
        let mut lock = stdout.lock();
        writeln!(lock, "# Full AST:\n\n{:?}\n", expr).unwrap();
        writeln!(lock, "# Display:\n\n{}\n", expr).unwrap();
        writeln!(lock, "# Time: {:?}", end).unwrap();
    } else {
        println!("{}", expr);
    }
}

fn partial(expr: &str, verbose: bool) {
    let start = Instant::now();
    let partial = parse_source_file_partial(&expr).unwrap_or_else(|e| {
        print_diagnostics(expr, e);
        process::exit(1);
    });
    let end = start.elapsed();

    let stdout = io::stdout();
    let mut lock = stdout.lock();

    if let Some(ref expr) = partial.value() {
        if partial.has_errors() {
            writeln!(lock, "# Partial AST:\n\n{:?}\n", partial).unwrap();
        } else {
            writeln!(lock, "# Full AST:\n\n{:?}\n", expr).unwrap();
        }

        writeln!(lock, "# Display:\n\n{}", expr).unwrap();

        if verbose {
            writeln!(lock, "# Time: {:?}", end).unwrap();
        }
    } else {
        eprintln!("No expression value produced");
        if partial.has_errors() {
            eprintln!("# Errors:\n\n{:?}", partial.errors());
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
