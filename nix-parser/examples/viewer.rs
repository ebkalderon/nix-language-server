use std::io::Read;
use std::time::Instant;
use std::{fs, io, process};

use codespan::Files;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{emit, Config};
use nix_parser::ast::SourceFile;
use nix_parser::error::Errors;
use nix_parser::parser::parse_source_file_partial;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(about = "Parses a Nix expression and displays the result")]
struct Cli {
    /// Display the abstract syntax tree (AST)
    #[structopt(long)]
    debug: bool,
    /// Continue even in the presence of parse errors
    #[structopt(short, long)]
    partial: bool,
    /// Produce pretty printed output
    #[structopt(long)]
    pretty: bool,
    /// Display the amount of time spent parsing
    #[structopt(long)]
    time: bool,
    /// Source file to parse (reads from stdin if "-")
    #[structopt(default_value = "default.nix")]
    file: String,
}

fn main() {
    let args = Cli::from_args();

    let expr = if args.file == "-" {
        let mut expr = String::new();
        io::stdin()
            .read_to_string(&mut expr)
            .map(|_| expr)
            .expect("Failed to read expression from stdin")
    } else {
        fs::read_to_string(&args.file).expect("Unable to read file")
    };

    if args.partial {
        parse_partial(&expr, args);
    } else {
        parse_full(&expr, args);
    }
}

fn parse_full(expr: &str, args: Cli) {
    let start = Instant::now();
    let ast: SourceFile = expr.parse().unwrap_or_else(|errors| {
        print_diagnostics(expr, &errors);
        process::exit(1);
    });
    let end = start.elapsed();

    print_source_file(&ast, &args);

    if args.time {
        println!("\nParse time: {:?}", end);
    }
}

fn parse_partial(expr: &str, args: Cli) {
    let start = Instant::now();
    let partial = parse_source_file_partial(expr).unwrap_or_else(|errors| {
        print_diagnostics(expr, &errors);
        process::exit(1);
    });
    let end = start.elapsed();

    if let Some(ref ast) = partial.value() {
        print_source_file(ast, &args)
    } else {
        println!("No expression value produced");
    }

    if !partial.errors().is_empty() {
        print!("\n");
        print_diagnostics(expr, partial.errors());
    }

    if args.time {
        println!("\nParse time: {:?}", end);
    }
}

fn print_source_file(ast: &SourceFile, args: &Cli) {
    if args.debug {
        if args.pretty {
            println!("{:#?}", ast);
        } else {
            println!("{:?}", ast);
        }
    } else {
        if args.pretty {
            println!("{:#}", ast);
        } else {
            println!("{}", ast);
        }
    }
}

fn print_diagnostics(expr: &str, errors: &Errors) {
    let mut files = Files::new();
    let id = files.add("stdin", expr);
    let diagnostics = errors.to_diagnostics(id);

    let mut lock = StandardStream::stdout(ColorChoice::Auto);
    let config = Config::default();

    for diag in diagnostics {
        emit(&mut lock, &config, &files, &diag).unwrap();
    }
}
