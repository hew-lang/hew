# Behaviour calibration

This suite explores the costs and shapes of ordinary Hew programs alongside
Rust, C, Go and Erlang/OTP. It helps identify surprising overhead, understand
language and runtime tradeoffs, and choose useful follow-up investigations.
It is not a language ranking or a performance promise.

The [18 October lifecycle follow-up](results/2026-10-18-lifecycle.md)
attributes task and actor allocations and readiness notifications, with a
before/after comparison of the resulting runtime changes.

The [16 October 2026 Linux report](results/2026-10-16-linux-x86_64.md)
includes the first measured shapes, distributions and follow-up questions.

## Run

The runner currently requires a POSIX host; this suite has been executed on
Linux. Install the normal Hew development prerequisites, Rust, Clang, Go and Erlang/OTP.
Tokio is pinned in `tokio/Cargo.lock`. GNU time supplies process peak RSS on
Linux; optional Valgrind profiling measures native allocator growth.

From the repository root:

```sh
make hew
make bench-calibration
# Include native allocator measurements:
make bench-calibration BENCH_CALIBRATION_ARGS='--allocations'
# Quick execution and output checks:
make bench-calibration-smoke
```

Or use a previously built compiler without rebuilding it:

```sh
make bench-calibration HEW_BIN=/path/to/hew
```

Results, individual samples and build logs go under this directory's ignored
`out/`. To retain another run, select a different output directory:

```sh
make -C examples/benchmarks/calibration run HEW=/path/to/hew \
    OUT=/absolute/path/to/new-output ARGS='--repeats 7 --allocations'
```

To regenerate the plots from saved receipts, install Matplotlib and run
`python3 plot.py RECEIPT.json [CONTROL.json] --output shapes.svg` in this directory.

The default runner inherits at most two CPUs on Linux, builds serially, uses
one and two runtime workers, and limits individual commands by time. For a
shared machine, also apply a suitable process memory limit before running.
Missing comparator tools are recorded as skipped; build, execution and output
failures stop the run and preserve partial results. `make check` in this
directory exercises failure controls for those boundaries.

## Questions and workloads

| Workload          | What it explores                                                 | Checked result                     |
| ----------------- | ---------------------------------------------------------------- | ---------------------------------- |
| `scalar`          | Scalar arithmetic and the optimized code-generation floor        | Modular recurrence                 |
| `vector_scan`     | Checked indexing, loop optimization and contiguous reads         | Sum of all visited values          |
| `vector_build`    | Collection growth, element storage, traversal and release        | Sum of constructed elements        |
| `bytes_shared`    | Snapshot sharing, retain/release and reads                       | Sum of selected original bytes     |
| `bytes_cow`       | Preserve a snapshot while mutating its copy                      | Sum of changed and preserved bytes |
| `task_ready`      | Creation and joining of already-ready work, across batch sizes   | Sum of task results                |
| `actor_roundtrip` | Root-to-actor request/reply, including completion                | Sum of counter replies             |
| `actor_fanout`    | Worker clients talking concurrently to independent actors        | Sum of every completed reply       |
| `timer`           | Timer resolution and wakeup overshoot for sequential 1 ms sleeps | Number of completed waits          |

Primitives run at several iteration counts or buffer sizes. Concurrency uses
batch/actor counts of 1, 16 and 64 with one and two workers. Hew runs at O0 and
O2, Rust and C at O2, and Go with its ordinary optimized build. A second Rust
primitive build enables overflow checks at O2: this separates some arithmetic
safety cost from other code-generation differences. The O0 Hew
results describe development execution overhead rather than a competitor's
optimized baseline.

Each process parses its inputs and prepares the initial data before timing.
Actor creation precedes actor timings; task creation and joining are included
in task/fanout timings. A warmup task initializes concurrency runtimes. The
runner also discards one full process run per case and rotates implementation
order for subsequent runs. Outputs are checked before timings are accepted.

Each fixture emits one JSON object with `workload`, `iterations`, `size`,
`elapsed_ns` and `checksum`. Request/reply and timer workloads include individual
`latencies_ns` samples, capped at 10,000 per run. Timer samples are overshoot
beyond the requested millisecond, rather than full wait duration. Raw samples
are retained so different summaries do not require another measurement.

Batch medians and interquartile ranges describe variation between process runs.
Latency percentiles pool the operations recorded across those runs. A short
tail sample is a diagnostic hint, not an estimated service-level objective.
Very short batches can be dominated by measurement overhead and optimization;
look across sizes and repeat a suspicious result before drawing conclusions.

## Contracts that affect interpretation

- Hew bytes have value semantics with implicit shared storage and copy-on-write.
  Rust uses `Arc<[u8]>` with `Arc::make_mut`. Go shares a mutable slice and makes
  an explicit copy before mutation. C uses atomic reference bookkeeping for
  sharing and explicit allocation/copy/free for mutation. Sharing a Go slice
  does not provide Hew's snapshot guarantee.
- Hew and Rust/C release values as scopes end. Go reclaims through its normal
  garbage collector. The timed batch includes reclamation only when the runtime
  actually performs it; process RSS helps expose retained storage separately.
- Ready tasks are Hew tasks, Tokio tasks, Go goroutines or monitored BEAM
  processes. BEAM processes bring isolated heaps and monitoring semantics.
  These are idiomatic analogous operations with different guarantees.
- Hew request completion is intrinsic. Tokio uses a oneshot reply channel per
  request; Go reuses each client's reply channel; BEAM sends tagged messages.
  Root-to-actor latency is distinct from arbitrary task-to-task scheduling
  latency. Fanout measures worker-client throughput, without per-request tails.
- GNU time's peak RSS covers the entire process, including startup and JSON
  output after the timed interval. BEAM virtual-machine startup and Go/Tokio
  runtime storage belong to that ecosystem footprint. RSS is not live payload
  size or the number of allocations.
- Valgrind counts native allocations for Hew, Rust/Tokio and C. Differences
  between two operation counts give an approximate incremental cost. Go and
  BEAM internal allocation strategies cannot be compared through that oracle.
  Valgrind timing is excluded from ordinary runtime results. The
  `actor_roundtrip_alloc` variant omits per-request timing and sample formatting
  so measurement/reporting allocations do not masquerade as actor costs.

## Compilation is a separate workload

Every implementation's ordinary build command is timed, including linking.
The first invocation in each run and unchanged-source rebuilds are reported
separately. Choose a new output directory to measure a first local artifact
build. Existing registry, compiler, filesystem and Go caches
remain in use. These are local application builds, not cold-machine toolchain
installation or Hew compiler-bootstrap measurements.

Go and Cargo can reuse compiled artifacts on a rebuild. Direct Hew, Rust and C
compiler invocations compile again. That difference is useful ecosystem
information, so the report keeps both states visible.

Generated programs with 32, 128 and 512 scalar functions explore compilation
scaling. Their executable results are checked too. This is a simple source-size
probe; generic expansion, large dependency graphs, actor protocol generation
and compiler memory scaling need their own representative applications.

## Using results

Start with broad relationships: does work scale with bytes, operations, tasks
or workers? Does an allocation or scheduling boundary explain a step? Does an
optimization-level change affect a particular operation? Investigate large
unexplained gaps with generated LLVM, allocation profiles or a reduced source
program before changing language semantics or prescribing a workaround.

The suite does not yet cover overload fairness, busy-worker wakeup tails,
cancellation and supervision costs, networking throughput or distributed actor
latency. A good result here cannot establish those behaviours. Platform and
load details accompany each saved report; shared-host runs are calibration
evidence, not universal numbers.
