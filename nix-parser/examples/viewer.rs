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
    let binds = parse_expr(&expr).unwrap_or_else(|e| {
        eprintln!("parse error: {:?}", e);
        process::exit(1);
    });

    println!("# Full AST:\n\n{:?}\n", binds);
    let display: Vec<_> = binds.iter().map(ToString::to_string).collect();
    println!("# Display:\n\n{}", display.join("\n"));
}

fn partial(expr: &str) {
    let partial = parse_expr_partial(&expr).unwrap_or_else(|e| {
        eprintln!("parse error: {:?}", e);
        process::exit(1);
    });

    if let Some(ref binds) = partial.value() {
        if partial.has_errors() {
            println!("# Partial AST:\n\n{:?}\n", partial);
        } else {
            println!("# Full AST:\n\n{:?}\n", binds);
        }

        let display: Vec<_> = binds.iter().map(ToString::to_string).collect();
        println!("# Display:\n\n{}", display.join("\n"));
    } else {
        eprintln!("No expression value produced");
        process::exit(1);
    }
}
