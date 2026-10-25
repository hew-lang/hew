# Task communication and parallel service work — 25 October 2026

This pass reduces the cost of ordinary in-memory pipe sends. Against main
`0a5a44563f2d4326137c8ea17ee3948ffbe162fd`, candidate
`a75e7be36bbf0eca1c3f62bd10471d1b09597e6f` improves same-process task roundtrip
and pipeline timings, with about 32 fewer allocation calls per roundtrip.
Considerable overhead remains: the eight-pair pipeline still takes roughly
67 times Tokio's timed batch on the same machine. A separate authenticated
cross-VM compute service scales 6.75× from one to eight server workers; that
scaling is present in both revisions.

The runtime change lets an unencoded pipe send use its channel operation
directly. Previously it passed through the shared encoder/transport machinery,
which decoded an envelope into an allocated slot and created a second envelope
and operation. Encoded and transport-backed sends still use their shared
admission gate. A shared sender now owns its channel reference, so an operation
can safely observe closure after explicit endpoint finish. The compiler change
limits scalar loop snapshots to values crossing the helper boundary; private
helper temporaries stay local. Emitted scan, growth and reuse snapshot records
shrink from 296/288/384 to 184/176/272 bytes respectively. This storage reduction
does not establish the cause of each timing difference.

Measurements ran on an Intel Emerald Rapids C4 VM, with one hardware thread
per physical core, within one NUMA domain and excluding a housekeeping core.
The runtime cases use identical checked workloads, release compiler/runtime
pairs, observation off and 21 AB/BA pairs per row. Task setup and readiness
handshakes precede the timed communication. CPU/RSS measurements cover the
whole process; compilation, allocation instrumentation and PMU captures are
separate. Kernel activity and the VM's physical host were not isolated.

Runtime ratios below are **geometric means of paired baseline/candidate timed
intervals**; values above one favour the candidate. Brackets are marginal 95%
paired-bootstrap intervals, not simultaneous guarantees across the table.
Potential outliers remain in the estimates.

| Workload                             | Workers | Median timed batch, baseline → candidate | Paired ratio [95% interval] |
| ------------------------------------ | ------: | ---------------------------------------: | --------------------------: |
| Task pipe roundtrip, 20,000 requests |       1 |                      140.512 → 99.241 ms |        1.418 [1.410, 1.427] |
| Task pipe roundtrip, 20,000 requests |       2 |                     191.478 → 141.799 ms |        1.349 [1.340, 1.358] |
| Task pipe roundtrip, 20,000 requests |       8 |                     183.312 → 171.624 ms |        1.068 [1.058, 1.079] |
| Eight pipe pairs, 5,000 items each   |       1 |                     151.250 → 110.458 ms |        1.371 [1.364, 1.378] |
| Eight pipe pairs, 5,000 items each   |       2 |                     181.233 → 120.793 ms |        1.491 [1.444, 1.538] |
| Eight pipe pairs, 5,000 items each   |       8 |                     167.267 → 104.861 ms |        1.572 [1.529, 1.617] |

The two-worker pipeline has an order-effect warning. The same-binary,
eight-worker pipeline control gives 1.014 [0.983, 1.044]. Ready-task creation/join
and actor fanout controls show no resolved revision benefit across one, two and
eight workers; their batches are short, around 12–30 ms. These results support
a pipe-path improvement rather than a general scheduler speedup. A single
ping-pong pair also remains slower with more workers than with one.

Both ping-pong participants are forked tasks communicating through two
capacity-one `std.stream.pipe<i64>` channels. The pipeline has eight concurrent
producer/consumer pairs. Every value and final checksum is checked. Clocked
runs, separate from the table above, sample every 64th operation. Pooled
roundtrip distributions contain 6,573 samples per revision and worker count:

| Workers |   Baseline p50 / p95 / p99 |  Candidate p50 / p95 / p99 |
| ------: | -------------------------: | -------------------------: |
|       1 |   7.010 / 7.232 / 9.487 µs |   4.999 / 5.126 / 5.780 µs |
|       2 | 9.610 / 10.242 / 16.534 µs |  6.870 / 9.432 / 13.829 µs |
|       8 | 9.022 / 12.275 / 18.086 µs | 8.826 / 10.211 / 15.300 µs |

Pipeline receive-wait p50/p99 fall from 37.165/56.098 to 22.898/37.500 µs at
eight workers. This measures time inside the sampled receive, including
waiting for its producer; it is not a timestamped one-way delivery latency.
Its batch comparison also has an order-effect warning. Pooled percentiles are
descriptive, with dependent observations within each process.

Local Valgrind probes use the same source revisions at 32 and 256 iterations,
with latency clocks disabled. Subtracting the smaller run estimates incremental
allocation traffic while removing much of the fixed process setup:

| Work unit         | Allocation calls, baseline → candidate | Allocated bytes, baseline → candidate |
| ----------------- | -------------------------------------: | ------------------------------------: |
| One roundtrip     |                          85.04 → 53.04 |                         7,464 → 4,648 |
| One pipeline item |                          43.98 → 28.00 |                         3,828 → 2,422 |

These are two-point workload slopes, not universal per-operation constants or
live-memory measurements. The probes report zero Valgrind errors and no
definitely or indirectly lost blocks; possibly lost and reachable allocations
remain at exit.

Three separate C4 user-space PMU captures per case give the following median
counts, normalized by completed roundtrips or pipeline items. These captures
include process setup; counters ran at 100% without multiplexing.

| Work unit / workers | Cycles, baseline → candidate | Instructions, baseline → candidate |
| ------------------- | ---------------------------: | ---------------------------------: |
| Roundtrip / 1       |              22,423 → 15,484 |                    38,098 → 26,214 |
| Roundtrip / 8       |              54,501 → 35,960 |                    40,549 → 28,890 |
| Pipeline item / 1   |               12,149 → 8,691 |                    20,667 → 14,719 |
| Pipeline item / 8   |             103,101 → 67,288 |                    20,804 → 14,949 |

Eight-worker pipeline cycle samples still place 79.77% of Self time in the
profiling allocator's allocation/deallocation functions, down from 86.64%.
Both profiles report zero lost samples. This identifies the allocator path as
important remaining work; it does not isolate system allocation from shared
live-byte accounting or establish which atomic operation causes a stall.
The sampled binaries were relinked from optimized release IR with symbols;
PMU totals and paired timings use the recorded workload binaries.

The Tokio reference uses the same capacity-one pipeline shape, values,
readiness barrier and checksum. Its eight-worker timed batch is 1.638 ms versus
108.961 ms for the candidate Hew run in that comparison: paired ratio
66.711 [64.798, 68.712]. The Tokio samples are short and the series has a drift
warning, so this is a large remaining gap rather than a precise cross-language
ranking. The fixture matches work and communication shape, not every ownership,
cleanup and failure guarantee. Removing unnecessary pipe work is worthwhile,
but this pass closes only part of that gap.

The compute-service trial uses TCP+Noise between two same-zone C4 VMs. Each
64-byte request runs two million checked scalar terms, split into eight task
shards, and returns the exact payload. Two client lanes each make four warmup
and 32 timed requests. Authenticated node identities, completed request counts
and accumulated work are checked after timing. An independent oracle uses
complete 251-term periods plus at most 250 tail terms; each trial reports
17,999,785,728 accumulated work units including warmup.

With the candidate revision, median client intervals for the 64 timed replies
are 167.284, 88.545 and 24.790 ms at one, two and eight server workers: roughly
383, 723 and 2,582 requests/s. The one-to-eight ratio is 6.75×. The corresponding
baseline intervals are 166.966, 87.465 and 24.983 ms. Revision ratios remain
near parity, so this demonstrates parallel compute behind the remote actor
service, with no material revision gain visible in these medians. It includes
network and actor-service overhead; it is not an echo-only transport test.
Ordinary same-zone placement leaves physical host/rack distance uncontrolled.

Vec effects are mixed. The following separate one-core series reports the
**median paired** baseline/candidate process-wall ratio, with 21 pairs each:

| Vec case            | Ratio [95% interval] |
| ------------------- | -------------------: |
| Scan 65,536         | 1.063 [1.045, 1.080] |
| Fresh growth 4,096  | 0.971 [0.961, 0.981] |
| Fresh growth 65,536 | 1.031 [1.029, 1.041] |
| Reuse 65,536        | 1.031 [1.025, 1.046] |

Small growth still regresses, with some samples below 50 ms. A separate late
inlining experiment against a matched relink control gives 1.075× scan but
0.882× small growth, 0.907× large growth and 0.873× reuse. It was not adopted.
Relinking alone changed scan to 0.926× versus the original binary, showing why
that experiment needs its own control and cannot be presented as an isolated
source-level inlining effect.

The next runtime pass should attribute the remaining operation/envelope/release
allocations and allocator bookkeeping, then separate runnable queue handoff
from idle-worker wake latency. Ready tasks currently use a shared FIFO, while
actor activations use the work-stealing deques; neither queue should be replaced
on the strength of these aggregate timings alone. The next compiler pass should
explain the small-growth regression and helper/snapshot interactions, keeping
growth and reuse as controls when changing inlining or storage placement.

The [evidence archive](https://storage.cloud.google.com/hew-bench-results/20261025t152139z-5734f0-performance-pass2/evidence.tar.gz)
and [SHA-256](https://storage.cloud.google.com/hew-bench-results/20261025t152139z-5734f0-performance-pass2/evidence.tar.gz.sha256)
use the existing results bucket and its access permissions.
It contains matched SDK manifests, workload sources/binaries/IR, raw paired
samples, clocked distributions, PMU records and profiles, allocation logs and
service validation results. Runtime comparisons are under
`build/results/calibration/comparison`, latency distributions under `latencies`,
Vec and late-inline series under `vec-comparison` and `late-inline-comparison`,
and authenticated service trials under
`network-near/results/service-comparison`. These measurements cover Linux
x86-64 on the selected machines; other targets were not performance-tested.

The C4 allocation window lasted 55m05s from launch through verified cleanup.
The builder creation-to-confirmed-removal bound is 54m37s, the client bound
7m49s. The main paired task series took 107.63s, clocked latency series 50.45s,
and authenticated service series 79.71s; setup, builds, profiling and other
controls account for the rest. Bounded on-VM IR development stayed below ten
minutes. The retained-rate estimate for VM, disk and IP time is US$0.92,
excluding transfers, stored evidence and billing adjustments. The four-hour
forecast was US$11.24 including a conservative export allowance and reserve.
Cleanup audit and independent instance/disk/reserved-address lists are empty;
the existing project and bucket remain.

Validation includes 2,623 runtime tests, 106 codegen tests, eight calibration
validator tests, O0/O2 checked fixture execution, zero Valgrind errors in eight
allocation probes, `make lint` and the harness checks. Cancellation tests cover
owned pending sends, last-producer EOF and endpoint finish before first poll;
compiler checks exercise a live vector at loop budget cancellation in O0/O2.
