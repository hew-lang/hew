## std::iter — lazy vs eager filter-map-sum bench

Workload: `xs = 1..=100_000`, compute `sum(map(add5, filter(even, xs)))`.
Expected result: `2_500_300_000`.

Two variants share the same workload and the same input shape (`Vec<int>`):

- `bench_eager.hew` — the long-standing stdlib path: `iter.filter_int` →
  `iter.map_int` → `iter.sum`. Each combinator returns a fresh `Vec<int>`.
- `bench_lazy.hew` — the new lazy path: `iter.lazy_from_vec_int` →
  `iter.lazy_sum(src, pred, mapf, n)`. Zero intermediate Vecs.

Each binary times one run and exits. The shell harness runs each
binary 12 times, discards the first invocation as warm-up, and reports
the median. We split it this way because v0.4.0 still has the
`for + closures + now_nanos` MLIR bug that breaks multi-run timing
inside a single process.

### Run

    ./run.sh

Output goes to `results.md`.

### Latest numbers

See `results.md` (regenerate with `./run.sh`).
