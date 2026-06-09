use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn allocation_workload(iterations: usize) -> usize {
    let mut total = 0;
    for i in 0..iterations {
        let mut v = Vec::with_capacity(32);
        v.push(i as u8);
        total += v.len();
    }
    total
}

fn observe_overhead(c: &mut Criterion) {
    let mut group = c.benchmark_group("observe_overhead");

    group.bench_function("hot_tier_off_alloc_workload", |b| {
        hew_runtime::observe::set_hot_tier_enabled(false);
        b.iter(|| black_box(allocation_workload(black_box(1_000))));
    });

    group.bench_function("hot_tier_on_alloc_workload", |b| {
        hew_runtime::observe::set_hot_tier_enabled(true);
        b.iter(|| black_box(allocation_workload(black_box(1_000))));
    });

    group.finish();
    hew_runtime::observe::set_hot_tier_enabled(false);
}

criterion_group!(benches, observe_overhead);
criterion_main!(benches);
