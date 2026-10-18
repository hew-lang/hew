# Task and actor lifecycle calibration: 18 October 2026

This follow-up attributes task and actor overhead before changing it. The patch
removes about two native allocations per ready task and two per successful actor
request in these probes. It also removes notifications for task observations
that are already complete. Cancellation, ownership, completion and scheduler
contracts remain in place.

## Revisions and boundaries

The baseline is main revision `91b3ae64682736071c3785fe7828ce13684f9b90`;
the optimized runtime is `eabf14d4876ef260d8c16cd0f5d648cce8e0979f`.
Both compiler/runtime pairs were rebuilt with `make hew` and frozen separately.
The [profile receipt](2026-10-18-lifecycle-profiles.json) records compiler,
runtime archive and symbolized probe hashes. Ordinary comparison receipts record
the suite checkout revision separately from the compiler version. The optimized
compiler has a dirty suffix because the profiling Make target was uncommitted
during its build; its runtime sources match the optimized revision above.

The 16 October report used an older runtime. Its approximately 14/17 task/request
allocation counts are not the baseline for this change. A compiler version alone
is insufficient provenance when its native archive can be rebuilt separately.

Execution was on Linux x86_64, with two CPUs available and one or two runtime
workers. Native allocation growth is the difference between 32 and 256
operations, divided by 224. It includes workload collections and completion
machinery, rather than a universal cost for every task or request. Allocated
bytes are cumulative allocator traffic, not peak live memory.

## Allocation attribution

DHAT stacks from the O2 symbolized probe identify these costs. Counts are rounded;
surrounding work and scheduling produce small amortized differences.

| Ready-task allocation group                        | Baseline events per task | Guarantee or purpose                                                        |
| -------------------------------------------------- | -----------------------: | --------------------------------------------------------------------------- |
| Task, result storage and execution owner           |                        3 | Scope/handle/worker lifetimes; output remains owned beyond frame completion |
| Cancellation tokens                                |                        2 | Independent task and invocation cancellation ancestry                       |
| Cancellation observer and subscriber buffers       |                        3 | Wake parked descendants when their token or an ancestor is cancelled        |
| Invocation context, state and budget               |                        3 | Retained wake target, execution context and cooperative polling             |
| Continuation frame                                 |                        1 | State across suspension                                                     |
| Callable environment                               |                        1 | Ownership of transferred captures                                           |
| Task-wait operation                                |                        1 | Observe completion and take the selected result once                        |
| Batched collections and pending-wait registrations |                Amortized | Preserve source result order and retain pending observations                |

The first cancellation subscriber now lives inline in SmallVec. Multiple
subscribers spill to dynamic storage. The same mutex serializes registration
with cancellation, and the same identity-based detach removes subscriptions.
This removes subscriber buffers without removing tokens or observers.

| Successful actor-request allocation group   | Baseline events per request | Guarantee or purpose                                                 |
| ------------------------------------------- | --------------------------: | -------------------------------------------------------------------- |
| Caller continuation                         |                           1 | Resume the suspended call and its cleanup                            |
| Typed request payload and envelope          |                           2 | Transfer arguments; preserve rejected request ownership and identity |
| Mailbox node                                |                           1 | Own admitted delivery and route an exclusive actor turn              |
| Reply channel                               |                           1 | Single result delivery, cancellation and late-reply ownership        |
| Call and admission operations               |                           2 | Own admission, reply, deadline and cleanup state                     |
| Capacity subscription                       |                           1 | Wake a caller waiting for mailbox space                              |
| Cleanup input vector                        |                           1 | Temporary list of three release cursors                              |
| Dispatch recovery registry and state escrow |                           2 | Recover owned actor state and lexical resources after a fault        |
| Reply storage                               |                           1 | Transfer the typed reply to its receiver                             |

Capacity readiness is now registered only after admission finds a full mailbox.
The operation then retries admission before returning pending. A capacity change
before registration is seen by the retry; a later change notifies the retained
wake target. Full-mailbox callers retain the subscription and pay an extra
initial admission probe. Immediate admission avoids the subscription allocation
and registry lookup.

Successful calls commonly have three empty cleanup cursors. Passing an array
through the existing reverse-order join removes their temporary heap vector;
actual request, reply and asynchronous resource cleanup is unchanged.

These purposes explain retained state, but do not prove every retained
allocation is minimal. Coallocating state or changing recovery storage would
need a separate lifetime and fault-recovery argument.

| Symbolized O2 allocation probe   | Allocations per operation before / after | Allocated bytes before / after |
| -------------------------------- | ---------------------------------------: | -----------------------------: |
| Ready task, batch size 16        |                            about 15 / 13 |              about 1,060 / 996 |
| Successful root-to-actor request |                            about 12 / 10 |                about 948 / 900 |

The ordinary optimized Memcheck probes in the full matrices show the same
reduction of about two allocation events. Exact bytes and amortized pending-wait
counts differ slightly between builds and scheduling runs.

## Handoffs and readiness

| Workload            | Execution path                                                           |
| ------------------- | ------------------------------------------------------------------------ |
| Ready task          | Submit task, run on a worker, publish completion, notify the root latch  |
| Root actor call     | Admit request, activate actor, publish reply, notify the root latch      |
| Worker actor client | Admit request, activate actor, publish reply, enqueue/resume client task |

Nested suspending calls resume directly through their frames; they do not add
independent scheduler queue entries. Stable wake targets, publication before
observation, exclusive activation and task/actor scheduling alternation continue
to protect lifetimes, progress and fairness.

In the Callgrind probe with 256 ready tasks, batch size 16 and one worker, the
baseline made 497 blocking-wake callbacks from constructors observing completed
tasks, plus 17 callbacks from pending task completion. After the patch, the
constructor edge disappeared and the 17 completion callbacks remained. These
include warmup and scope drain. They are readiness notifications, not 497 actual
context switches: the latch can coalesce them.

All task-wait consumers poll status before parking. Removing an already-complete
constructor notification therefore preserves progress. Registration and pending
completion still share their synchronization point, and pending waiters are
still notified.

## Runtime comparisons

The full comparison matrices used seven measured processes after one discarded
warmup, with the same unchanged fixtures and compiler/runtime build profile.
They passed all output and allocator checks without skips. Retained evidence:

- [Baseline tables](2026-10-18-lifecycle-before-tables.md) and [raw samples](2026-10-18-lifecycle-before.json).
- [Optimized tables](2026-10-18-lifecycle-after-tables.md) and [raw samples](2026-10-18-lifecycle-after.json).

Sequential task measurements varied substantially even for unchanged Tokio
controls. A short follow-up interleaved the same baseline, optimized and Tokio
binaries in seeded randomized order, with 21 measured processes per case after
warmup. Its [raw samples and binary hashes](2026-10-18-lifecycle-interleaved.json)
retain that check separately from the full matrices.

Batch elapsed time in milliseconds: median, with the interquartile range in
parentheses. These are whole batches, rather than per-operation latencies.

| Interleaved workload         | Workers |       Hew before |        Hew after | Tokio control |
| ---------------------------- | ------: | ---------------: | ---------------: | ------------: |
| 4,096 ready tasks, batch 64  |       1 |    4.4 (4.3–4.6) |    4.6 (4.6–6.0) | 0.8 (0.7–0.9) |
| 4,096 ready tasks, batch 64  |       2 |    9.3 (6.5–9.4) |    7.8 (4.3–8.2) | 1.4 (1.3–1.5) |
| 64 clients × 256 actor calls |       2 | 26.9 (19.2–31.1) | 29.6 (15.1–34.7) | 1.1 (1.0–1.6) |

The two-worker task regression suggested by the sequential matrices did not
reproduce in the interleaved check. The one-worker task median moved slightly
upward, while the two-worker median moved downward; actor fanout distributions
were broad and overlapped. These runs establish reduced allocator and
notification traffic, without establishing a general elapsed-time improvement.
The sizeable worker-client throughput gap against Tokio remains.

The full matrices also retain Go and BEAM comparisons, primitive controls,
timer distributions, application builds and compilation scaling. Their distinct
contracts and cache states remain as described in the suite methodology.

## Behavioural validation

The complete runtime unit/integration target passed. New coverage forces a
dequeue or close between the initial full result and readiness registration,
checks admission/dequeue/close/cancellation on a real mailbox, exercises inline
and spilled cancellation subscribers with detach, and observes completed tasks
through direct await, select and scope drain.

Full `make lint` passed. Targeted native O0/O2 acceptance passed for completion
(including unit handlers), full-mailbox rejection and request ownership, ordered
vector awaits, first-completion selection, tasks outliving a timed select,
cancellation cleanup and faulting actor-child resource cleanup. Only Linux
execution was tested; these results do not establish overload latency or fairness.

## Reproduce the attribution

Build each revision in a separate checkout and retain its matching compiler and
native archive. Run the ordinary comparison suite with `--allocations` for
checked outputs, timing distributions and native Memcheck counts.

For allocation stacks at O2, preserve symbols with the suite's
`concurrency-profile` Make target. Run Valgrind DHAT with `--num-callers=32`
and `HEW_WORKERS=1` on `task_ready 32 16`, then repeat at 256 operations.
Use `actor_roundtrip_alloc` with size 1 for actor requests; it omits latency
recording and formatting. Compare allocation stacks and counts across operation
counts. Valgrind timing is excluded from ordinary runtime comparisons.
Use Callgrind on `task_ready 256 16` to inspect the incoming edges to the
blocking wake callback.
