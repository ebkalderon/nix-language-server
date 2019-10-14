use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use nix_parser::ast::SourceFile;

const EXAMPLE_FILE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/example.nix"));

fn parse_example(b: &mut Criterion) {
    let mut group = b.benchmark_group("parse_example");

    let module = black_box(EXAMPLE_FILE);
    group.throughput(Throughput::Bytes(EXAMPLE_FILE.len() as u64));
    group.bench_function("parse example.nix", move |b| {
        b.iter_with_large_drop(|| {
            SourceFile::from_str(module).expect("Failed to parse example.nix")
        });
    });

    group.finish()
}

criterion_group!(benches, parse_example);
criterion_main!(benches);
