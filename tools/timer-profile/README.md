# Timer precision profile

Measure timer callback lateness, CPU time per timer, batch throughput and idle
blocking on Linux:

```sh
make profile-timers
```

This builds a small optimized host that imports the production wheel and OS
poller directly. It needs Rust and the existing `libc` dependency; it does not
build the compiler. `latency` compares whole-millisecond and exact deadline
policies, both without load and with four busy CPU threads. `exact` selects only
the exact policy. The `kernel` profile separately compares `epoll_wait` with
`epoll_pwait2`. Every callback measurement checks that the deadline did not fire
early. Actor mailbox and worker dispatch latency are outside this measurement.

## Decision and measurements

Use exact deadlines with high-resolution OS waits. Keep the existing
millisecond wheel buckets as an index: finer deadlines require neither smaller
buckets nor a periodic ticker.

On the recorded Linux x86-64 host, three comparison runs gave these callback
latenesses without background load. Each cell is the median of three runs of
300 timers; p99 columns are medians of the per-run p99s.

| Requested delay | Millisecond p50 | Exact p50 | Millisecond p99 | Exact p99 |
| --------------- | --------------: | --------: | --------------: | --------: |
| 50 µs           |        1,026 µs |     62 µs |        2,036 µs |    157 µs |
| 250 µs          |          828 µs |     72 µs |        1,848 µs |    162 µs |
| 1 ms            |        1,079 µs |     76 µs |        1,191 µs |    131 µs |
| 10 ms           |        1,094 µs |     94 µs |        1,224 µs |    188 µs |

With four load threads, exact p50 lateness was 62–92 µs across these delays.
CPU time per timer showed no consistent increase. Median batch costs for
32/512/8,192 timers were 39/34/49 ns per timer before and 37/35/50 ns after.
Native 64-bit entries remain 48 bytes. Both versions blocked for the entire
250 ms idle experiment in one wait; neither polled periodically.

The historical baseline uses the wheel and poller from `ea3d0604d`, imported
into the same optimized host with millisecond API adapters. Both versions arm
an absolute deadline that respects the requested delay; the baseline rounds it
up to milliseconds. The candidate preserves it exactly. The normal `latency`
command provides a repeatable policy comparison within the current source.
[Raw results and source fingerprints](results/) record the historical run.

These results support the Linux default. OS scheduling, timer slack, CPU
contention and hardware still determine actual delivery lateness. Older Linux
kernels fall back to rounded-up millisecond waits; Windows and the deterministic
host clock retain their millisecond boundaries. This benchmark is opt-in and
has no timing assertions in ordinary CI.
