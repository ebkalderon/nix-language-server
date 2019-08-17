use std::io::Read;
use std::process;
use std::{env, io};

use nix_parser::parser::{parse_expr, parse_expr_partial};

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
    let expr = parse_expr(&expr).unwrap_or_else(|e| {
        eprintln!("parse error: {:?}", e);
        process::exit(1);
    });

    println!("# Full AST:\n\n{:?}\n", expr);
    println!("# Display:\n\n{}", expr);
}

fn partial(expr: &str) {
    let partial = parse_expr_partial(&expr).unwrap_or_else(|e| {
        eprintln!("parse error: {:?}", e);
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
        process::exit(1);
    }
}
