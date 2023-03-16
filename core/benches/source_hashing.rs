use criterion::{black_box, criterion_group, criterion_main, Criterion};
use warp_core::code::SourceHasher;

pub fn source_hasher(c: &mut Criterion) {
    c.bench_function("SourceHasher hash small file", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| SourceHasher::hash("./fixtures/smol.erl"))
    });
    c.bench_function("SourceHasher hash large file", |b| {
        b.to_async(tokio::runtime::Runtime::new().unwrap())
            .iter(|| SourceHasher::hash("./fixtures/smol.erl"))
    });
}

criterion_group!(benches, source_hasher);
criterion_main!(benches);
