use std::io::{self, Read};
use std::process;

use nix_parser::parser::parse_expr;

fn main() {
    let mut expr = String::new();
    io::stdin()
        .read_to_string(&mut expr)
        .expect("Failed to read expression from stdin");

    let binds = parse_expr(&expr).unwrap_or_else(|e| {
        eprintln!("parse error: {:?}", e);
        process::exit(1);
    });

    let display: String = binds.iter().map(|b| format!("{}\n", b)).collect();
    println!("Display:\n\n{}\n", display);
    println!("AST:\n{:?}", binds);
}
