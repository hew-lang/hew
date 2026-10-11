use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use std::hint::black_box;
use std::time::Instant;

use hew_runtime::execution_context::{set_current_context, HewExecutionContext};
use hew_runtime::tracing::{hew_trace_enable, hew_trace_lifecycle, hew_trace_reset, SPAN_SPAWN};

#[allow(
    clippy::cast_possible_truncation,
    reason = "benchmark workload; truncation is intentional"
)]
fn allocation_workload(iterations: usize) -> usize {
    let mut total = 0;
    for i in 0..iterations {
        let mut v = Vec::with_capacity(32);
        #[expect(
            clippy::cast_possible_truncation,
            reason = "bench uses wrapping byte pattern; truncation is intentional"
        )]
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

    let mut trace_group = c.benchmark_group("trace_hot_path");
    trace_group.throughput(Throughput::Elements(1));

    hew_trace_reset();
    trace_group.bench_function("disabled_lifecycle_event", |b| {
        b.iter(|| hew_trace_lifecycle(black_box(42), black_box(SPAN_SPAWN)));
    });

    // Lifecycle recording deliberately fails closed outside a dispatch. Install
    // the same execution-context carrier used by the scheduler so this measures
    // real timestamp, sequence, lock, and bounded-ring work rather than only the
    // enabled guard.
    let mut context = HewExecutionContext::default();
    context.trace.trace_id_lo = 1;
    context.trace.span_id = 1;
    let previous_context = set_current_context(&raw mut context);

    hew_trace_enable(1);
    trace_group.bench_function("enabled_single_thread_lifecycle_event", |b| {
        b.iter(|| hew_trace_lifecycle(black_box(42), black_box(SPAN_SPAWN)));
    });

    // Four emitters contend on the shared producer ring. Thread startup and
    // teardown happen once per Criterion sample and are therefore amortized
    // across `iters * 4` recorded events. Throughput is declared as four events
    // per reported iteration so Criterion's elements/second remains honest.
    trace_group.throughput(Throughput::Elements(4));
    trace_group.bench_function("enabled_four_thread_contended_lifecycle_events", |b| {
        b.iter_custom(|iters| {
            let start = Instant::now();
            std::thread::scope(|scope| {
                for worker in 0_u64..4 {
                    scope.spawn(move || {
                        let mut context = HewExecutionContext::default();
                        context.trace.trace_id_lo = worker + 1;
                        context.trace.span_id = 1;
                        let previous_context = set_current_context(&raw mut context);
                        for actor_id in 0..iters {
                            hew_trace_lifecycle(black_box(actor_id), black_box(SPAN_SPAWN));
                        }
                        let restored = set_current_context(previous_context);
                        debug_assert_eq!(restored, &raw mut context);
                    });
                }
            });
            start.elapsed()
        });
    });

    trace_group.finish();
    hew_trace_reset();
    let restored = set_current_context(previous_context);
    debug_assert_eq!(restored, &raw mut context);
}

criterion_group!(benches, observe_overhead);
criterion_main!(benches);
