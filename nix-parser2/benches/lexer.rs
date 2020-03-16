use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use nix_parser::lexer::Lexer;
use nix_parser2::lexer;

const EXAMPLE_FILE: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../nix-parser/example.nix"
));

fn lexer(b: &mut Criterion) {
    let mut group = b.benchmark_group("lexer");

    let module = black_box(EXAMPLE_FILE);
    group.throughput(Throughput::Bytes(EXAMPLE_FILE.len() as u64));
    group.bench_function("old", move |b| {
        b.iter(|| {
            let lexer = Lexer::new_with_whitespace(module).unwrap();
            black_box(lexer);
        });
    });
    group.bench_function("new", move |b| {
        b.iter(|| {
            let lexer: Vec<_> = lexer::tokenize(module).collect();
            black_box(lexer);
        });
    });

    group.finish()
}

criterion_group!(benches, lexer);
criterion_main!(benches);
