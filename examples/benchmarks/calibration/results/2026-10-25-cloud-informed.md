# Cloud-informed runtime comparison

[Machine-readable results](2026-10-25-cloud-informed.json) · [Evidence bundle](https://storage.cloud.google.com/hew-bench-results/runs/20261025t013117z-ec6f16/local-followup/e43397fb0e8ca37234134e1bbff67a2ea96251be/evidence.tar.zst)

Baseline `d828d316274f0b7d0b05ca40d41283bf1c8da636`; candidate `e43397fb0e8ca37234134e1bbff67a2ea96251be`. AMD Ryzen 9 7950X, Linux, 16 physical cores; 15 measurement CPUs. LLVM 22.1.8; Rust 1.96.0.

Timing compares paired baseline/candidate runs at identical rounds and CPU affinity (21 pairs per row). Intervals are pointwise 95% paired-bootstrap intervals. CPU user and system time are reported separately. The two self controls are retained in JSON. Shared-host services were not isolated; separate PMU and allocation captures are diagnostic evidence, not timing samples. Full-mode wall time includes profiler startup and shutdown.

The cloud baseline identified unnecessary plain-value Vec cleanup, allocation-counter
contention and profiler shutdown waits. This follow-up implements those changes
and improves scalar Vec loop lowering, then compares the combined revision with
an untouched current-main baseline on one local development machine. These are
same-language revision comparisons; they do not update the cloud Rust/Hew ratios.

The final revision gives 1.26–1.27× scan speed ratios and 1.35×/1.93× reuse ratios
at 4,096/65,536 elements. Eight-worker fanout gives 1.18× with observation off and
1.25× in hot mode. One- and two-worker wall-time effects are mostly unresolved.
Full-mode process ratios of 7.52×/7.90× primarily reflect prompt shutdown of the
sampler and server, not cheaper tracing per event. Short full-mode runs can
finish before the periodic sampler's first sample.

Fresh growth is a measured tradeoff: paired elapsed time rises 6.94% at 4,096
elements and 3.66% at 65,536, with both intervals excluding parity. User cycles
per element rise 5.1%/4.5% even though instructions fall 4.8%/4.9%. Allocation
counts are unchanged. The growth IR still calls `hew_vec_push_owned`, outlines
the readback scan and enlarges its coroutine frame from 128 to 288 bytes. This
establishes worse execution efficiency for growth on this host; the available
counters cannot attribute it to frame traffic, code layout, cache stalls or
push operations individually. Fewer instructions alone are not a speedup claim.

The strongest work-removal result is large reuse: at 189 passes, allocation
calls fall from 2,988 to 138 and allocated bytes from 200,279,224 to 1,056,022.
The plain-layout path changes length without constructing per-element release
cursors; capacity and cleanup for owned elements remain intact. Scan instructions
fall from 23.056 to 16.071 per element. Scalar reads can use existing bounded
loop helpers, retaining bounds/overflow checks and the shared scheduling budget;
immutable range facts remain visible across outlining. An earlier trial that
made those facts opaque raised scan instructions to about 31 and was rejected.

Cumulative allocation totals now use the existing worker identity and padded
counter records, while live/peak counters retain their shared atomic semantics.
Hot heap counters use the same worker separation. Eight-worker cycles fall
while instructions rise slightly, consistent with reduced accounting contention;
this does not identify the cost of individual atomics. Non-worker and offload
threads share shard zero, and their scaling was not measured. Lifecycle
allocation counts are essentially unchanged; this is not a lifecycle-allocation
removal or leak-fix claim.

All reported timing rows use the final frozen pair. Native O0/O2 acceptance covers
Vec widths, nested ownership, out-of-bounds traps, transfer, parked teardown,
cancellation and scheduling safepoints. MIR/codegen, runtime, HTTP integration,
Observe and `make lint` checks pass. Linux x86-64 was executed; Windows, macOS
and wasm paths were inspected, not executed.

The next local compiler investigation is growth's helper/frame interaction and
remaining owned-push/capacity work, using these growth cases as regression
controls. The next runtime investigation should distinguish remaining shared
live/peak accounting from lifecycle work before changing either contract.
Network code is unchanged: the cloud RTT distributions remain useful, but noisy
mode effects and heterogeneous cross-zone hardware do not justify a transport
rewrite. Any later paid validation should run the affected Vec and Observe
cases serially on one PMU-capable VM, after a local improvement warrants it.
No VM was allocated for this follow-up.

The evidence archive includes the frozen SDK pairs, checked workload binaries,
LLVM IR, raw samples, runner sources and `REPRODUCE.md`. Replay the existing
paired engine locally before returning to cloud; it checks every result and
retains same-binary controls. Vec-only replay needs one physical core; the full
matrix uses eight. The Vec self ratio is 1.007 [0.997, 1.045] and the actor self
ratio is 1.000 [0.979, 1.087]. These controls limit small-effect claims; intervals
are pointwise, not simultaneous, and shared-host frequency/contention can drift.
PMU and allocation phases are separate from timing. No local cache/stall or
sampled-call-stack attribution is claimed from the four architectural events.

The final incremental release build took 5m04s (compiler 3m29s; runtime archive
1m33s). Timing collection took 4m11.50s, PMU collection 47.72s and allocation
build/capture 30.91s: 5m30.13s for the three serial collection phases. Baseline
and final build times have different cache states and are not a compilation
speed comparison. Ratio estimates below are medians of paired ratios, which
need not equal the quotient of the separately reported wall-time medians.

| Workload / mode          | Rounds | CPUs              |        Wall ms B/C | B/C speed ratio [95% CI] |   CPU user ms B/C | CPU system ms B/C |
| ------------------------ | -----: | ----------------- | -----------------: | -----------------------: | ----------------: | ----------------: |
| vec-scan-4096 / off      |  28795 | `1`               |  166.064 / 142.300 |     1.255 [1.240, 1.493] | 162.400 / 138.127 |     1.930 / 1.902 |
| vec-grow-4096 / off      |   4860 | `1`               |  218.049 / 231.089 |     0.935 [0.931, 0.966] | 216.149 / 228.470 |     1.793 / 1.763 |
| vec-reuse-4096 / off     |   4682 | `1`               |  274.455 / 163.787 |     1.351 [1.333, 1.448] | 273.297 / 162.390 |     1.738 / 1.647 |
| vec-scan-65536 / off     |   3391 | `1`               |  328.235 / 264.563 |     1.267 [1.253, 1.359] | 325.544 / 262.171 |     1.867 / 1.995 |
| vec-grow-65536 / off     |    187 | `1`               |  162.411 / 176.112 |     0.965 [0.938, 0.976] | 109.259 / 117.468 |   50.907 / 56.956 |
| vec-reuse-65536 / off    |    189 | `1`               |   200.885 / 96.254 |     1.927 [1.858, 2.058] |  147.170 / 92.945 |    50.909 / 1.752 |
| actor-roundtrip-w1 / off |  12792 | `1`               |  144.190 / 143.096 |     1.002 [0.973, 1.053] |   28.124 / 29.287 |   67.420 / 66.768 |
| actor-roundtrip-w1 / hot |  13350 | `1`               |  148.555 / 141.835 |     1.015 [0.991, 1.052] |   28.621 / 28.830 |   71.212 / 69.043 |
| actor-fanout64-w1 / off  |    453 | `1`               |  142.914 / 145.981 |     0.987 [0.969, 1.014] |   41.855 / 46.541 |   56.045 / 53.908 |
| actor-fanout64-w1 / hot  |    454 | `1`               |  168.353 / 162.754 |     1.059 [0.984, 1.081] |   61.851 / 55.503 |   60.289 / 58.711 |
| actor-fanout64-w2 / off  |    722 | `1,2`             |  145.856 / 147.432 |     0.996 [0.959, 1.038] |  95.787 / 101.662 |   83.408 / 86.948 |
| actor-fanout64-w2 / hot  |    738 | `1,2`             |  157.235 / 152.596 |     1.009 [0.953, 1.051] |  105.663 / 98.489 |   90.596 / 86.835 |
| actor-fanout64-w2 / full |    321 | `1,2`             | 1011.860 / 134.268 |     7.521 [7.350, 7.697] |   81.001 / 73.093 |   89.242 / 92.886 |
| actor-fanout64-w8 / off  |   1840 | `1,2,3,4,5,6,7,8` |  197.364 / 171.885 |     1.180 [1.145, 1.225] | 811.166 / 630.925 | 259.011 / 257.608 |
| actor-fanout64-w8 / hot  |   1239 | `1,2,3,4,5,6,7,8` |  163.426 / 133.612 |     1.248 [1.162, 1.288] | 563.109 / 426.165 | 183.553 / 168.088 |
| actor-fanout64-w8 / full |    738 | `1,2,3,4,5,6,7,8` | 1009.875 / 128.427 |     7.900 [7.733, 8.025] | 423.770 / 331.268 | 232.258 / 230.498 |
| task-ready64-w8 / off    |     16 | `1,2,3,4,5,6,7,8` |    46.892 / 46.860 |     1.002 [0.983, 1.018] |     4.960 / 4.473 |     5.226 / 6.917 |

## Selected allocation and PMU diagnostics

Allocation values are medians of three separate diagnostic runs per edition. The instrumented fixture snapshots counters immediately before checksum formatting and runtime teardown; these are workload-process totals, not steady-state exact counts. PMU medians are normalized by checked work units from three captures per edition; all four architectural events ran without multiplexing.

| Workload / mode          |   Alloc calls B/C |   Bytes allocated B/C |    Live bytes B/C |    Peak bytes B/C |       Cycles/unit B/C | Instructions/unit B/C | Branch misses % B/C |
| ------------------------ | ----------------: | --------------------: | ----------------: | ----------------: | --------------------: | --------------------: | ------------------: |
| vec-scan-65536 / off     |         136 / 136 |     1055679 / 1055829 |       1332 / 1329 |   788036 / 788201 |         4.937 / 3.583 |       23.056 / 16.071 |     0.0069 / 0.0081 |
| vec-reuse-65536 / off    |        2988 / 138 |   200279224 / 1056022 |       1332 / 1329 |  1312396 / 788289 |       58.044 / 40.617 |     191.093 / 136.527 |     0.0185 / 0.0104 |
| actor-fanout64-w8 / off  | 1300027 / 1299913 | 138994201 / 138874468 |   707705 / 707702 |   821871 / 819305 | 32461.160 / 24182.411 | 10079.038 / 10204.420 |     1.3617 / 1.3801 |
| actor-fanout64-w8 / hot  |   876289 / 876236 |   93953002 / 93903021 |   707853 / 707850 |   819556 / 819966 | 38577.237 / 26355.611 | 10661.455 / 10779.062 |     1.3577 / 1.4425 |
| actor-fanout64-w8 / full |   617619 / 617573 |   61525313 / 61483588 | 5281736 / 5281805 | 5390920 / 5390459 | 40739.625 / 20720.202 | 14449.074 / 14520.806 |     2.3819 / 2.2086 |

B/C means baseline/candidate. The JSON contains all 17 measured rows, both self controls, source and evidence hashes, CPU affinity, timing summaries, PMU medians, and allocation medians.
