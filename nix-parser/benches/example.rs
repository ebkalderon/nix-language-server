use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nix_parser::ast::SourceFile;

const EXAMPLE_FILE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/example.nix"));

fn parse_example(b: &mut Criterion) {
    let module = black_box(EXAMPLE_FILE);
    b.bench_function("parse example.nix", move |b| {
        b.iter(|| SourceFile::from_str(module).expect("Failed to parse example.nix"));
    });
}

criterion_group!(benches, parse_example);
criterion_main!(benches);
