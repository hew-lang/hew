# Compiler and runtime efficiency, 23 October 2026

The release compiler handles the measured source-size increase without a
scaling cliff. The largest developer-facing cost is a fixed frontend floor,
with excessive repeated filesystem canonicalization. Compiled scalar and
collection loops benefit from O2; small tasks and requests spend substantial
work on lifecycle bookkeeping. Idle work parks rather than spinning. One
concrete avoidable allocation occurs during shutdown even when tracing was
never enabled.

This investigation changes no compiler or runtime behaviour. The
[receipt](2026-10-23-efficiency.json) retains individual samples, commands,
input hashes, generated probe sources and profile summaries.

## Provenance and limits

- Official **0.7.1 Linux x86_64 SDK**, source
  `7b489ef7392ed3baced2a4d119e1e4741995ece2`.
- SDK archive SHA-256:
  `2dcd57f05dc4146ab8543680d2adf7d67020e70b5cfef350fdcaa5e3712790a9`;
  compiler: `82c83d7c050bb044af07aca71c904209504b64317286683d39cb0191b5df5dc6`;
  matching `libhew.a`:
  `00732a7c43edc0357b63b47d7b91bcbbd74f3e93f7a90379dae46f762ec03b05`.
- Standard library came from that SDK, selected explicitly with `HEW_STD`.
  Its tree manifest hash is in the receipt. No local compiler bootstrap or
  debug compiler supplied representative measurements.
- Fixture checkout: `7284807ff20a02a6684d02a13e5e02977986c87a`.
  The mixed package combines the existing shapes directory module and imported
  machine definitions with a generic identity function, vectors and 32 actor
  requests. Its checked output is `Circle(r=5) Completed 528`.
- Linux x86_64, Ryzen 9 7950X, affinity CPUs 30 and 31. One or two Hew workers.
  Shared-host load was high: initial 1/5/15-minute averages 28/15/11, later
  18/20/16. Wall-time comparisons, especially task batching and two-worker
  throughput, are noisy. No other-language ranking is implied.
- Three retained native samples per case; runtime cases discard a warmup and
  alternate O0/O2 order. Runtime checksums use the existing calibration oracle.
  CPU time includes startup and output; batch time is measured inside Hew.
  GNU time supplies peak process RSS. CPU values for runtime/startup/cleanup
  use child resource accounting with finer precision than GNU time's display.
- Valgrind 3.26 supplies allocation and instruction attribution. These runs
  are excluded from native timing. Runtime profiles use the same SDK with
  **O2 plus debug information**; release compiler internal Rust names remain
  stripped. Hardware sampling was unavailable under the host's perf policy.
  No sandbox or other-platform execution was tested.

## Compilation and `hew run`

Median process wall time in milliseconds; RSS is the maximum of three runs.
`--emit-obj` includes frontend, lowering, LLVM optimization and object emission.
Subtracting independently sampled stages is an estimate, not an exact phase
timer. Checks do not depend on an optimization level.

| Program              | Check | O0 object | O2 object | O0 build | O2 build | Check / build RSS MiB |
| -------------------- | ----: | --------: | --------: | -------: | -------: | --------------------: |
| Print 42             |   200 |       201 |       202 |      292 |      292 |          35.6 / 129.8 |
| 32 scalar functions  |   202 |       207 |       228 |      295 |      318 |          36.3 / 129.6 |
| 128 scalar functions |   220 |       242 |       299 |      335 |      416 |          38.5 / 129.9 |
| 512 scalar functions |   266 |       386 |       624 |      488 |      736 |          51.2 / 130.4 |
| Mixed package        |   240 |       269 |       343 |      361 |      445 |          39.1 / 133.7 |

The compiler's own timing hooks put the tiny program's frontend at 182 ms
and semantic lowering at 9 ms in the first sample. Across the scalar sizes,
frontend time increases modestly while LLVM O2/object work grows more visibly.
Linking adds roughly 90–110 ms and accounts for the much higher process peak
RSS of a full build. CPU totals for these builds track wall time closely;
the receipt retains user and system CPU separately.

The mixed package's frontend check allocates **178.84 MB in 774,704 blocks**,
with **12.15 MB peak live heap** and 0.328 MB live at exit under DHAT. Its
Callgrind check executes 924 million instructions; the O2 build executes
1.875 billion before counting linker subprocess work. Allocation traffic is
not live memory, RSS or a demonstrated leak.

The tiny check makes **175,418 `readlink` calls**, 175,417 returning an error,
plus 289 `statx` and only 38 `openat` calls. `EINVAL` is the ordinary response
when canonicalization probes a component that is not a symlink; the problem
is repetition. The mixed-package Callgrind profile attributes 30.1 million
self instructions to glibc `realpath`.

A separate **diagnostic-only** check with the existing symbolized development
compiler, `0.7.2-dev.12+73dd8abfc3`, confirms the pattern: 198,724 `readlink`
calls for the same tiny source. Its debug timings and instruction proportions
are not compared with the release SDK. Its Callgrind callers locate the
dominant chain:

`Checker::scope_site` (10,959 calls), `collect_type_param_bounds` (3161) and
`source_parameter_heads` (2799) repeatedly call `DefTable::module_for_source`.
That lookup calls `intern_source_key`, which canonicalizes the path before
querying the map. The profile records 19,114 module lookups, 19,127 calls to
`intern_source_key` and 19,967 total filesystem canonicalizations. The source
owner is `hew-types/src/def_table.rs`; builtin authority helpers are much
smaller callers in this probe. Normalize/intern source identity at an owned
frontend boundary and reuse it during lookup. Preserve symlink, synthetic
source fallback and authority semantics rather than weakening identity checks.

For the same checked `42` output, an already-built executable takes a median
**1.55 ms wall / 1.15 ms CPU**, versus **302 ms wall / 299 ms CPU** for
`hew run`. Peak RSS is 2.5 MiB versus 129.7 MiB. Each unchanged `hew run`
invocation recompiles; the command currently has no application artifact cache.
Its latency is explained by compilation and linking, not slow execution of
the printed expression.

## Executable work

One-worker medians below. Batch time excludes setup/output; CPU and RSS cover
the whole process. Each row has a validated checksum at O0 and O2.

| Workload                 | Operations / size               | O0 batch ms | O2 batch ms | O2 process CPU ms | O2 peak RSS MiB |
| ------------------------ | ------------------------------- | ----------: | ----------: | ----------------: | --------------: |
| Scalar recurrence        | 20 million steps                |       72.91 |       51.46 |             52.87 |            3.00 |
| Vector scan              | 4096 scans of 4096 elements     |      107.09 |       10.13 |             12.16 |            3.02 |
| Vector build/read/drop   | 1024 vectors of 4096 elements   |       69.78 |       28.38 |             29.99 |            2.91 |
| Shared bytes             | 2 million snapshots, 4096 bytes |       29.03 |        6.24 |              8.09 |            2.96 |
| COW bytes                | 16,384 copies, 4096 bytes       |        1.25 |        0.77 |              2.11 |            2.99 |
| Ready tasks, batch 1     | 4096 tasks                      |       35.13 |       39.09 |             40.44 |            5.54 |
| Ready tasks, batch 64    | 4096 tasks                      |       13.69 |       15.57 |             19.26 |            5.35 |
| Root actor request/reply | 1000 requests                   |       13.57 |       13.21 |             13.63 |            4.43 |
| Actor fanout             | 16 clients, 256 requests each   |       14.17 |       13.79 |             16.47 |            4.34 |
| Sequential timers        | 100 sleeps of 1 ms              |      121.33 |      109.79 |              6.43 |            4.27 |

O0/O2 differences in the short concurrent cases are not established
regressions or improvements. O2 ready-task samples range 30.45–44.67 ms at
batch 1 and 11.00–17.60 ms at batch 64. Sequential handoffs generate thousands
of voluntary context switches; larger batches amortize them. Two workers do
not consistently help these small workloads on this host. The receipt retains
both worker counts; it does not justify changing the default worker policy.

For O2 one-worker requests, pooled p50/p95/p99 latency is 8.31/16.62/21.37 µs.
Timer overshoot is 82.8/118.4/1458.9 µs. These are bounded diagnostic samples
under load, not latency guarantees.

## Allocation and lifecycle attribution

The incremental estimates use differences between 32 and 256 operations.
Startup and formatting add noise, and scheduler races can change observation
allocations. They identify cost magnitude, not exact ABI allocation counts.

| Operation                                                 | Approximate allocations / operation | Allocated bytes / operation |
| --------------------------------------------------------- | ----------------------------------: | --------------------------: |
| Construct/read/drop 4096-element vector                   |                               12.04 |                      65,706 |
| Share a 4096-byte snapshot                                |                                0.04 |                           2 |
| Mutate a preserved 4096-byte snapshot                     |                                1.04 |                        4122 |
| Ready task, batch 16                                      |                               13.71 |                        1107 |
| Successful root actor request, without latency formatting |                               11.06 |                        1170 |

Shared snapshots do not allocate proportionally to operations; COW mutation
allocates approximately one buffer, as expected. Vector growth allocates
roughly twice the final 32 KiB payload across its lifetime. This is ordinary
capacity growth, not evidence of copying the entire vector on every push.

The symbolized O2 task profile allocates for task/result/executor ownership,
continuation frames, invocation contexts, cancellation tokens and observations,
plus collection/result aggregation. Actor requests allocate the envelope,
reply channel, admission/request/call state and nested continuation frames.
All these categories are reclaimed in the measured runs. They provide
independent readiness, completion, cancellation and cleanup lifetimes;
coalescing storage must preserve those boundaries.

Two specific costs deserve separate work:

1. **Unused tracing storage is initialized at shutdown.** DHAT attributes one
   **4,456,448-byte** retained allocation to
   `finalize_scheduler_shutdown → session_reset → hew_trace_reset → TraceStore`
   in both task and actor probes, with tracing disabled throughout. The lazy
   store reserves 16,384 events when reset first touches it. This is reachable
   process-global capacity, not a leak or proof of 4.46 MB resident pages.
2. **Dispatch watermark scans run on ordinary actor turns.**
   `observe_dispatch_close` uses 534,272 inclusive instructions, about 13% of
   the complete 256-request profile's 4,132,173 instructions. The implementation
   scans all 257 active-ticket slots to publish a completion watermark, even
   with the hot observe tier disabled. Barrier completeness remains required;
   disabling hot event counters alone cannot safely remove this work.

The always-on allocator wrapper is also measurable: allocation/deallocation
paths account for 325,579/249,561 inclusive instructions in that actor profile
and 468,200/357,857 in the 3,942,040-instruction task profile. These figures
include the underlying allocator and overlap other inclusive totals. Separate
counter atomics from allocator work before proposing an observability change.

## Idle and cleanup

A probe leaves an actor idle while a task sleeps for 500 ms, then stops the
actor and joins it. O2 median process CPU is **2.52 ms**, wall time 549 ms,
peak RSS 4.18 MiB and 21 voluntary context switches. Its syscall profile has
31 futex calls and two epoll waits, rather than a periodic busy loop.

The existing cancelled-defer/resource fixture was repeated 256 times in one
process. All three O0 and O2 runs emit exactly one defer and close per losing
task in the required order. O2 takes 15.33 ms process CPU. Memcheck finds no
invalid accesses, definite leaks or indirect leaks. Runtime exit retention
is unchanged between the allocation probe sizes: 4,457,886 reachable bytes,
dominated by the dormant trace store. It also reports 656–1152 possibly-lost
bytes involving thread/TLS state; those diagnostics remain unresolved and
are not reclassified as a clean leak result. These bounded cases do not
establish long-running service retention or overload/cancellation-tail bounds.

## Prioritized follow-ups

1. Eliminate repeated canonicalization in `DefTable::module_for_source` within
   one frontend invocation. The minimal check's syscall count and current
   caller attribution make this the clearest developer-loop target. Keep
   canonical source identity authoritative and cache only facts whose scope
   and invalidation are established.
2. Reset tracing without constructing an absent trace store. This is a small,
   directly attributed unnecessary allocation; retain generation/reset races
   and enabled-tracing behaviour.
3. Reduce completion-watermark scanning cost while preserving Observe barriers.
   Bound scans to actual participants or defer work only with a proof that
   concurrent dispatch publication and barrier cuts remain correct.
4. Investigate task/request storage coallocation against the measured lifetime
   map. Do not replace ownership or cancellation guarantees with a faster
   unchecked path. Validate allocation deltas and genuine pending wakes,
   then measure native CPU under bounded representative work.
5. Reassess unchanged-source `hew run` caching after the frontend floor is
   reduced. Compiler, runtime, stdlib, dependency and option identities must
   participate; a cache is more complex than the first two fixes.

## Reproduction

Use the official matching SDK, set `HEW_STD` to its `std/`, and use a dedicated
`TMPDIR`. The receipt substitutes `$SDK`, `$REPO` and `$TASK` for local paths.
Build the existing primitives/concurrency O0/O2 targets with
`make -C examples/benchmarks/calibration ... HEW="$SDK/bin/hew" OUT=...`.
Execute the receipt's workloads under a two-CPU affinity with
`HEW_WORKERS=1` or `2`, validating JSON with `run.py::validate` before accepting
timings. Use `/usr/bin/time` for RSS and child resource accounting for CPU.

For allocation differences, run Memcheck at 32 and 256 operations. Report
possible losses separately; the invalid-access/definite/indirect error gate
uses `--errors-for-leak-kinds=definite,indirect` with all leak kinds displayed.
For attribution, build `concurrency-profile` and use Callgrind/DHAT at 256
operations. `strace -f -c -e trace=%file` on the minimal check reproduces the
canonicalization count. Instrumented timings are not representative timings.
