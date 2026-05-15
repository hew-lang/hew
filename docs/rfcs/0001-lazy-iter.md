# RFC 0001 — Lazy iterators in `std::iter`

| | |
|---|---|
| **Status** | Proposed |
| **Target** | `std::iter` |
| **Tracking issues** | 4 known compiler bugs blocking the ergonomic shape (see "Compiler followups" below) |
| **Author** | Stephen Olesen (slepp) — drafted by Gerty |

## Summary

Add a defunctionalized lazy iterator chain to `std::iter` for `Vec<int>` /
`int`-source workloads. The new API fuses `filter`, `map`, and the terminal
(sum / fold / collect / take) into a single drain loop with **zero
intermediate `Vec` allocation**, closing roughly half of the historical
performance gap with Rust on the closure-bench workload.

No existing API is changed. The new functions live next to the existing
eager `iter.filter_int / map_int / sum` family under the `lazy_*` prefix.

## Motivation

A prior profiling exercise (the "hew-closure-flame" writeup) put Hew at
**~12× slower than Rust** on a simple `sum(map(add5, filter(even, xs)))`
benchmark over `1..=100_000`. The breakdown attributed the gap to three
distinct sources:

1. **Eager iterators allocate intermediate `Vec<int>`** at every stage
   (~50% of CPU).
2. **Closures cross `iter.*` as runtime `fn` pointers**, not monomorphized
   types — no inlining, no SIMD (~5–10% of CPU but blocks further wins).
3. **The input Vec build loop** is ~30% of CPU on its own.

(1) and (3) are stdlib problems. (2) needs compiler work.

A proof-of-concept in `hew-lazy-iter-poc` validated that **(1) alone is
worth a 1.99× win** with no compiler changes — just by removing the
intermediate Vecs through a fused drain. With a lazy `Range` source that
also kills (3), Hew actually **beats Rust by 1.23×** on the same workload.

This RFC ships that win as part of the stdlib.

## API

All new functions are added to `std/iter.hew` alongside the existing eager
specializations. They share the `lazy_` prefix so the stdlib's
type-specialized naming convention is preserved (`_int` where the
underlying value type is concrete).

### Sources

```hew
pub fn lazy_from_range(lo: int) -> fn(int) -> int
pub fn lazy_from_vec_int(xs: Vec<int>) -> fn(int) -> int
```

A source is `fn(int) -> int` producing element `i` from a 0-based index.
The drain bounds iteration externally.

### Combinators (pass-through)

```hew
pub fn lazy_filter_pred_int(pred: fn(int) -> bool) -> fn(int) -> bool
pub fn lazy_map_fn_int(f: fn(int) -> int) -> fn(int) -> int
```

These are identity at the value level — the drain folds in the predicate
and map directly. They exist for API symmetry so the call site reads as
a pipeline. See "Why defunctionalized?" below.

### Terminals (fused drains)

```hew
pub fn lazy_sum(src, pred, mapf, n) -> int
pub fn lazy_sum_filter(src, pred, n) -> int
pub fn lazy_sum_map(src, mapf, n) -> int
pub fn lazy_fold(src, pred, mapf, init, op, n) -> int
pub fn lazy_collect_int(src, pred, mapf, n) -> Vec<int>
pub fn lazy_take_int(src, pred, n, max_steps) -> Vec<int>
```

The terminal drives the chain. After LLVM inlining the result is shaped
like Rust's `iter().filter(...).map(...).sum()`.

### Usage

```hew
import std::iter;

fn main() {
    let src  = iter.lazy_from_range(1);
    let even = iter.lazy_filter_pred_int((x: int) => x % 2 == 0);
    let add5 = iter.lazy_map_fn_int((x: int) => x + 5);
    let r    = iter.lazy_sum(src, even, add5, 100000);
    println(r); // 2_500_300_000
}
```

## Performance

### PoC numbers (hew-lazy-iter-poc, 11 runs, median µs)

| Variant | Source | Chain | Median (µs) | vs eager |
|---|---|---:|---:|---:|
| rust | `Vec` via `collect` | `iter.filter().map().sum()` | 75 | 12.3× faster |
| hew-eager (closure-bench) | `Vec` via push loop | `filter_int / map_int / sum` (3 Vecs) | 922 | 1.00× |
| **hew-lazy** *(this PR)* | `Vec` via push loop | userland lazy drain (0 Vecs) | **464** | **1.99×** |
| **hew-lazy-range** | pure index closure (no Vec) | userland lazy drain (0 Vecs) | **61** | **15.1×** |

The `hew-lazy-range` result is what's possible once both the intermediate
Vecs (this RFC) **and** the input Vec build (just a different source
constructor) are eliminated.

### In-stdlib bench (`examples/benchmarks/lazy_iter/`)

| Variant | Median (µs) | Stdev (µs) | Runs |
|---------|------------:|-----------:|-----:|
| eager   |         749 |       116  |   11 |
| lazy    |         429 |        52  |   11 |

Lazy speedup: **~1.75×** over eager on the Vec-source shape, on the
machine that built this PR. Matches the PoC directionally; absolute
numbers differ because the PoC measured a slightly different harness
setup.

Reproduce: `./examples/benchmarks/lazy_iter/run.sh`.

## Why defunctionalized?

The canonical Rust-shaped API would be:

```hew
type Lazy<T> { next: fn() -> Option<T>; }

trait LazyMethods<T> {
    fn filter(self: Lazy<T>, pred: fn(T) -> bool) -> Lazy<T>;
    fn map<U>(self: Lazy<T>, f: fn(T) -> U) -> Lazy<U>;
    fn take(self: Lazy<T>, n: int) -> Lazy<T>;
    fn collect(self: Lazy<T>) -> Vec<T>;
    fn fold<A>(self: Lazy<T>, init: A, op: fn(A, T) -> A) -> A;
}
```

Four independent compiler bugs in v0.4.0 block that shape:

### Compiler followups

1. **`Option<T>`-returning closures fail to lower.** A closure typed
   `fn() -> Option<int>` typechecks but the Hew-dialect lowering pass
   errors with "failed to lower Hew dialect ops". Blocks the canonical
   `next: () -> Option<T>` shape entirely.
   - **Repro:** [`0001-lazy-iter-spikes/spike8.hew`](./0001-lazy-iter-spikes/spike8.hew)
   - **TODO:** file upstream issue `compiler: Option<T>-returning closures fail Hew-dialect lowering`.

2. **Conditional value-yield inside a captured-state closure breaks MLIR
   verification.** A closure that captures a `Vec<int>` *and* yields
   different `int`s via `if/else` (or early `return -1`) triggers `module
   verification failed`. Same captures returning an unconditional value
   compile fine.
   - **Repro:** [`0001-lazy-iter-spikes/spike10.hew`](./0001-lazy-iter-spikes/spike10.hew),
     [`spike13.hew`](./0001-lazy-iter-spikes/spike13.hew)
   - **TODO:** file upstream issue `compiler: conditional yield in captured-state closure fails MLIR verifier`.

3. **Nested closures-of-closures SIGSEGV at runtime.** A combinator like
   `(i) => f(src(i))` where both `f` and `src` are captured closures
   compiles and links cleanly, then segfaults on first call. Blocks
   stacking combinators (`filter(...).map(...)`).
   - **Repro:** [`0001-lazy-iter-spikes/spike16.hew`](./0001-lazy-iter-spikes/spike16.hew)
   - **TODO:** file upstream issue `runtime: closure capturing another closure SIGSEGV on call`.

4. **Closure-typed struct fields don't typecheck on access.** A
   `type Plan { src: fn(int) -> int; ... }` parses, but `p.src(i)` is
   typed as `<error>` at the call site. Blocks `Lazy<T>` from being a
   real record.
   - **Repro:** [`0001-lazy-iter-spikes/spike19.hew`](./0001-lazy-iter-spikes/spike19.hew)
   - **TODO:** file upstream issue `types: closure-typed struct field call site types as <error>`.

Until those are fixed, the API in this RFC is the most ergonomic shape
the language can express. The shape is **deliberately a v0**: every entry
point is a free function taking concrete `int`-typed closures so the
compiler stays well inside the verified region.

## Why ship anyway?

Three reasons:

1. **The perf win is real and large.** Halving the closure-bench runtime
   without touching the compiler is the kind of stdlib change that pays
   for itself in days, not quarters.
2. **The API is forward-compatible.** Once the four bugs are fixed, the
   `lazy_*` free functions can stay as a thin compatibility layer that
   forwards to a proper `Lazy<T>` record. Existing call sites keep working.
3. **It anchors the followup work.** Filing four narrow compiler issues
   with reproducers gives the compiler team a concrete punch list that
   the stdlib has a real use for. Without this RFC, "fix lazy iter ergo"
   is too abstract to prioritise.

## Alternatives considered

- **Wait for compiler fixes, then ship `Lazy<T>` directly.** Rejected:
  blocks the perf win on four independent bugfixes, and we already know
  the workaround works.

- **Ship a generic `Lazy<T>` with `_int / _str / _f64` monomorphizations.**
  Rejected for v0: `_str` and `_f64` weren't part of the PoC, and the
  closure-bench gap is purely an `int` story. Adding `_str` / `_f64`
  later is a strict extension.

- **Add an `Iterator` trait now.** Rejected: the existing stdlib already
  defers this ("a tracking issue will be filed to drive the migration
  once the trait design is settled" — `std/iter.hew`). This RFC stays
  inside that constraint.

## Migration / breaking changes

**None.** All new names; existing `iter.*` API is untouched. Tests
covering the existing eager path continue to pass alongside the lazy
additions (82 `iter_test.hew` cases pass after this change — 69 existing
+ 13 new).

## Followups (not in this PR)

- File the four compiler issues above.
- Add `lazy_*_str` and `lazy_*_f64` once the closure-bench analysis is
  reproduced on those types.
- Define and adopt an `Iterator` trait, then refactor the `lazy_*`
  free functions into a `Lazy<T>` record with chained methods.
- Add a Rust-side comparison binary to `examples/benchmarks/lazy_iter/`
  so the cross-language number is reproducible from inside this repo
  (currently lives only in the standalone PoC repo).
