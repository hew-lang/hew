# Official SDK efficiency, 24 October 2026

The matched 0.9.1 SDK checks these programs substantially faster than 0.7.1, with no scaling cliff through 512 scalar functions. Source identity reuse removed most of the repeated filesystem work. The shipped runtime also avoids allocating unused trace storage and sharply reduces ordinary actor completion bookkeeping. These results do not justify a global type interner or a persistent query database for the completed programme. Some unoptimized loops cost more; optimized vector construction is also slower in this sample. Those results remain visible below.

The [receipt](2026-10-24-efficiency-0.9.1.json) contains all samples, commands, source hashes, generated inputs and profile summaries. The [0.7.1 report](2026-10-23-efficiency.md) and its raw receipt remain unchanged.

## Provenance and method

- Official **0.9.1 Linux x86_64 SDK**; protected source `17e06579cb14a167be1e369bdda5388e14e95f80`, signed tag object `11f233f6c5a8e859e6b3d8fe0e46d68edc964a73`, producer run `40088759442`. Archive checksum and GitHub provenance were verified before measurement.
- Archive SHA-256: `eee8b45ef5e576678ff5cff17c38f7d37fd7c3b45f13d0ab1d7f02999acd8242`; compiler: `769aab4947b78d451e2f847774acbb782ffd8a7fc247b3ae545f3abc50cbe2fa`; matching `libhew.a`: `f3e68552f9c524d787791dbfb0c2e06d43637771f5ebe10c2ae28bb6a5d0c0b7`.
- SDK standard-library manifest SHA-256: `0c6d6d8c996a413842f3d4494d9ff9f0f2ccdc5e7f249268faceee72a4fe3fb0`. `HEW_STD` explicitly selects that SDK. Both packaged runtime archive locations have the same hash.
- The fixture checkout was already at a metadata-only 0.9.2 development commit. Every maintained fixture was checked byte-for-byte against protected source `17e06579`; that checkout is not the compiler source identity. Compared with 0.7.1, the two workload sources only migrate argument parsing to the ordinary `string.to_int()` method. The generated mixed package now checks the successful machine-step `Result` with `.expect("valid lifecycle transition")`. The obsolete unchecked scratch input was refused before measurement; its diagnostic is retained. Kernels, sizes, checksums and expected outputs are preserved.
- Same Linux x86_64 Ryzen 9 7950X host, CPUs 30/31, one or two Hew workers. Load averages were 5.2/4.6/5.8 initially and 5.7/5.1/5.8 finally; the retained baseline had materially higher shared-host load. CPU and instruction counts help interpretation, but elapsed-time differences alone do not establish causation.
- Three retained samples per native case. Runtime workloads discard one warmup and alternate O0/O2 order. CPU includes child processes; wall time uses a high-resolution process timer; peak RSS comes from GNU time. Allocation traffic, live heap and RSS are distinct quantities. Instrumented runs are excluded from native timing.
- The first command invocation and repeated independent processes are identified separately. OS page caches were not dropped, and SDK identity/tool checks precede samples. Thus this is neither a cold-machine nor a cold compiler-build measurement. There is no persistent incremental session in this experiment. Hardware sampling was unavailable under the host perf policy; Valgrind 3.26 and strace provide bounded attribution. No sandbox, other operating system or language ranking is measured here.

## Compilation

Median process **wall / CPU milliseconds**. Check is independent of optimization level. Object emission includes checking, lowering, LLVM optimization and object writing. RSS is the maximum among the three check or full-build samples.

| Program              |         Check |     O0 object |     O2 object |      O0 build |      O2 build | Check / build RSS MiB |
| -------------------- | ------------: | ------------: | ------------: | ------------: | ------------: | --------------------: |
| Print 42             |   81.2 / 80.9 |   84.4 / 84.1 |   85.2 / 80.5 | 166.6 / 168.4 | 162.7 / 167.5 |          34.4 / 125.2 |
| 32 scalar functions  |   80.0 / 79.6 |   92.8 / 92.5 | 104.7 / 104.2 | 181.4 / 184.9 | 192.0 / 195.3 |          37.6 / 125.4 |
| 128 scalar functions |   91.0 / 90.7 | 119.3 / 119.0 | 181.8 / 181.5 | 208.7 / 213.5 | 271.0 / 274.0 |          41.1 / 125.3 |
| 512 scalar functions | 137.3 / 136.8 | 248.6 / 248.3 | 486.3 / 486.0 | 336.2 / 340.1 | 600.7 / 598.9 |          54.9 / 126.1 |
| Mixed package        |   99.9 / 99.7 | 135.6 / 135.3 | 210.6 / 210.2 | 232.7 / 236.1 | 311.1 / 314.1 |          39.8 / 131.6 |

For context, 0.7.1 check / O2-build wall medians were 200/292 ms for Print 42, 202/318 ms for 32 functions, 220/416 ms for 128, 266/736 ms for 512, and 240/445 ms for the mixed package. The check floor is reduced, while O2/object work still grows with program size. The tiny first check reports 71.8 ms frontend and 10.8 ms semantic lowering; the 512-function first check reports 90.3/34.9 ms. Timing hooks are retained per sample.

Full build minus independently sampled object emission estimates **78–114 ms wall** and **84–113 ms CPU** for linking and its surrounding work. This is an estimate, not an isolated linker phase timer. The mixed package estimates 101 ms wall / 104 ms CPU at O2. The object and complete build columns prevent this estimate from being mistaken for execution cost.

| First vs repeated invocation | First wall ms | Repeated wall median ms |
| ---------------------------- | ------------: | ----------------------: |
| Print 42 check               |          98.9 |                    80.2 |
| Print 42 O0 build            |         347.8 |                   166.0 |
| Print 42 O2 build            |         165.2 |                   162.0 |
| Mixed-package check          |         102.0 |                    99.1 |
| Mixed-package O2 build       |         310.5 |                   311.4 |

The first O0 link is an outlier at 348 ms (302 ms CPU), followed by approximately 166 ms wall. These samples distinguish initial and repeated invocations without assigning the difference to a semantic cache.

For the same `42` output, the already-built executable takes **2.12 ms wall / 1.87 ms CPU**, peak RSS 2.42 MiB. `hew run` takes **187.02 ms wall / 179.25 ms CPU**, peak RSS 126.81 MiB. Baseline values were 1.55/1.15 ms and 302/299 ms respectively. The one-shot command still incurs compilation and linking on every invocation.

### Bounded edit probe

This probe is additional to the matched comparison. Scratch `source-edit.hew` changes only `println(42)` to `println(43)` at O2. Both outputs are executed and checked after each build. The exact sources and hashes are in the receipt; initial hash `330fa4ad428488ccd5e4a65d656b93dba46204ad8487767537f044b2d5bd69f1`, edited hash `f4a085b3fa174d16e0ec143a63f7e82486786b6da10a51a1fd9287dd4e7d7b6f`.

| Source version | First wall / CPU ms | Two unchanged repeats, median wall / CPU ms | Peak RSS MiB |
| -------------- | ------------------: | ------------------------------------------: | -----------: |
| initial        |       170.9 / 174.3 |                               168.0 / 171.1 |        126.9 |
| edited         |       169.7 / 173.6 |                               176.2 / 180.4 |        126.7 |

The small edit costs about the same as an unchanged independent build. Each CLI invocation performs a complete build; this probe measures that workflow.

## Compiler allocation and attribution

The mixed check allocates **189.47 MB in 786,507 blocks**, with **14.49 MB peak live heap** and **0.335 MB at exit** under DHAT. The baseline was 178.84 MB / 774,704 blocks, 12.15 MB peak and 0.328 MB at exit. Traffic rises about 6% and peak live heap about 19%; those values are not RSS or leaks. The language/prelude and explicit result handling have changed, and the release compiler is stripped. This profile cannot attribute the increase specifically to type construction, cloning or equality.

Callgrind records **988.8 million** instructions for the check and **1.965 billion** for the O2 build, versus 924.3 million and 1.875 billion previously. The build count excludes linker subprocess work. Lower wall/CPU time alongside slightly higher instruction traffic is consistent with less filesystem overhead; it is not a claim that all compiler internals execute fewer instructions.

The tiny check now makes **5,843 `readlink` calls** (5,842 errors), versus **175,418** previously: a **96.7% reduction**. It also makes 331 `statx` and 42 `openat` calls. Ordinary non-symlink probes return `EINVAL`; the prior problem was repetition. The original symbolized diagnostic profile attributed that repetition to `scope_site` and bound/parameter lookups calling `module_for_source → intern_source_key → canonicalize`. Source-key reuse (`addf814107`) preserves the canonical identity/fallback authority while removing repeated work. The final stripped SDK and syscall counts substantiate that fix; no fresh debug compiler was substituted for this comparison.

## Executable work

One-worker medians. Batch time excludes setup/output; process CPU and RSS include them. Every O0/O2 sample uses the existing checksum oracle.

| Workload               | Operations / size | 0.7.1 O2 batch ms | 0.9.1 O0 batch ms | 0.9.1 O2 batch ms | O2 process CPU / wall ms | O2 peak RSS MiB |
| ---------------------- | ----------------- | ----------------: | ----------------: | ----------------: | -----------------------: | --------------: |
| Scalar recurrence      | 20,000,000 / 1    |             51.46 |            122.80 |             50.23 |            52.22 / 52.49 |            3.10 |
| Vector scan            | 4,096 / 4,096     |             10.13 |            138.78 |              9.95 |            11.84 / 12.09 |            2.95 |
| Vector build/read/drop | 1,024 / 4,096     |             28.38 |             88.33 |             32.77 |            34.76 / 35.03 |            3.10 |
| Shared bytes           | 2,000,000 / 4,096 |              6.24 |             35.26 |              6.59 |              8.69 / 8.99 |            2.98 |
| COW bytes              | 16,384 / 4,096    |              0.77 |              1.39 |              0.78 |              2.67 / 3.77 |            3.00 |
| Ready tasks (batch 1)  | 4,096 / 1         |             39.09 |             45.34 |             37.72 |            30.37 / 91.47 |            5.44 |
| Ready tasks (batch 64) | 4,096 / 64        |             15.57 |              5.18 |              4.68 |            10.22 / 48.14 |            5.70 |
| Actor request/reply    | 1,000 / 1         |             13.21 |             14.79 |             15.23 |            14.94 / 58.86 |            4.49 |
| Actor fanout           | 256 / 16          |             13.79 |             13.31 |             13.19 |            16.02 / 56.48 |            4.26 |
| Sequential timers      | 100 / 1           |            109.79 |            112.42 |            109.69 |            5.57 / 152.78 |            4.24 |

O2 scalar, vector scan and byte workloads remain near baseline. Vector construction rises from 28.38 to 32.77 ms batch time, and from 29.99 to 34.76 ms CPU; its cause needs attribution. O0 scalar recurrence rises from 72.91 to 122.80 ms, and O0 vector scan from 107.09 to 138.78 ms.

Ready-task batching remains useful: batch 64 takes 4.68 ms versus 37.72 ms for sequential batch 1 here. The baseline was noisier, with higher shared-host load, so the size of this wall-time improvement is not a causal scheduler claim. Small requests and fanout remain dominated by lifecycle/scheduling work. The receipt retains both worker counts and all sample ranges; it does not justify changing the worker default.

Pooled O2 one-worker request latency p50/p95/p99 is 13.06/15.44/22.56 µs. These are diagnostic samples, not latency guarantees.

Pooled O2 one-worker timer overshoot p50/p95/p99 is 80.43/127.92/921.03 µs. These are diagnostic samples, not latency guarantees.

## Allocation and lifecycle

Differences between 32 and 256 operations estimate traffic per operation. Startup, formatting and scheduler races can alter small residuals; these are not exact ABI allocation counts.

| Operation             | Approx. allocations / operation | Allocated bytes / operation |
| --------------------- | ------------------------------: | --------------------------: |
| vector_build          |                           12.04 |                      65,706 |
| bytes_shared          |                            0.04 |                           2 |
| bytes_cow             |                            1.04 |                       4,122 |
| task_ready            |                           13.44 |                       1,095 |
| actor_roundtrip_alloc |                           11.06 |                       1,170 |

Vector and byte allocation slopes are effectively unchanged. Sharing does not allocate proportionally to operations; mutating a preserved byte snapshot allocates approximately one buffer. Tasks still allocate about 13–14 times per operation, and successful actor requests about 11 times. DHAT stacks locate task execution/invocation contexts and continuation ownership, and actor request/admission/reply/call state. These provide distinct readiness, completion, cancellation and cleanup lifetimes. Storage coallocation would need to preserve those guarantees; this report does not equate the allocations with dispensable work.

| 256-operation O2 profile | Allocated bytes / blocks | Peak live bytes / blocks | Exit live bytes / blocks | Callgrind instructions |
| ------------------------ | -----------------------: | -----------------------: | -----------------------: | ---------------------: |
| actor                    |          411,818 / 3,096 |              98,015 / 91 |               2,590 / 20 |              3,786,177 |
| task                     |          390,094 / 3,660 |            162,703 / 840 |               2,422 / 18 |              4,052,195 |

The final SDK confirms the two bounded runtime fixes:

- **Trace reset:** the prior 4,456,448-byte `TraceStore` capacity allocation is absent from both profiles with tracing disabled. Actor/task exit live heap is 2,590/2,422 bytes, rather than retaining that unused capacity. The original `84a236d300` control used the runtime test profile; these are separate matching release-SDK observations. Reachable capacity is neither RSS nor a leak.
- **Completion watermark:** actor `observe_dispatch_close` falls from **534,272 to 4,608 inclusive instructions** (about 99%). Total actor instructions fall from 4,132,173 to 3,786,177; task total changes from 3,942,040 to 4,052,195. The earlier `c216d3c39e` focused control established the scan removal while retaining Observe completion barriers; this final SDK quantifies its contribution without claiming all runtime work decreased.
- **Allocator attribution:** actor allocation/deallocation paths use 337,849/245,343 inclusive instructions; task paths use 467,818/357,857. These include the allocator and overlap caller totals. They are not additive percentages of native elapsed time, nor an isolated measurement of profiling atomics.

## Idle and cleanup

The actor-idle/500 ms sleep probe takes **543.70 ms wall / 2.80 ms CPU** at O2, peak RSS 4.15 MiB and 20 voluntary context switches. strace records 26 futex calls and two epoll waits. It parks rather than continuously polling. The original baseline was 549 ms wall / 2.52 ms CPU, so there is no established idle CPU improvement.

The existing cancellation/defer/resource fixture runs 256 times per process. All O0/O2 runs preserve exactly one defer and close per losing task in the expected order. O2 median is **56.20 ms wall / 15.01 ms CPU**, peak RSS 4.14 MiB, versus 15.33 ms CPU previously. Memcheck reports zero invalid-access errors and zero definite/indirect leaks in the allocation and cleanup probes. Idle/cleanup retain only **1,438 reachable bytes**, versus 4,457,886 previously. They still report **656–1,152 possibly-lost bytes** in thread/TLS-related state. Those diagnostics are unresolved; this is not an unqualified leak-free or long-running-service claim.

## Decisions for the remaining compiler proposals

**Type interning (#3564): do not implement a global representation rewrite for this programme.** Close the current implementation proposal as not justified by the measured evidence. The actionable source-identity duplication was fixed in its owner and the check floor fell substantially. Higher allocation traffic remains visible, but these stripped profiles do not show that type equality/cloning is the dominant cost. Reopen only with a symbolized representative profile, a measurable target and a bounded design that preserves type/context identity.

**Persistent query database (#3565): defer implementation and close this programme proposal.** The current one-shot checks scale smoothly, and the small edit probe does not establish a need for a persistent graph. No measured workspace/edit target has been shown to exceed what bounded invocation-scoped immutable reuse could meet. If such a target becomes a product requirement, first define source/configuration/compiler/stdlib/dependency identities and invalidation semantics, then evaluate the smallest reuse boundary. Root-specific name resolution, generics, reachability, spans, diagnostics and ownership checks must remain authoritative. A database is a separate design decision, not the default response to repeated CLI compilation.

The retained O0 and vector-construction costs are concrete future profiling inputs, while unchanged `hew run` continues to pay for a full build. Investigate those measured costs before choosing a broader compiler rewrite.

## Reproduction and evidence

Use the exact SDK identities above with the retained calibration inputs and oracles, CPUs 30/31, `HEW_STD` from the SDK, `HEW_OBSERVE=0`, and one/two `HEW_WORKERS` as recorded. The receipt retains the complete compiler/object/build commands and generated scalar sources (32/128/512 functions), mixed-package source, edit inputs, allocation/cleanup commands, tool versions and hashes. Native workload binaries are built through the existing calibration Makefile. GNU time, strace, DHAT, Callgrind and Memcheck use the commands in the receipt; instrumented runs are never pooled with native timing. Raw profiles and bounded diagnostic logs are retained separately for inspection; executable scratch outputs are disposable.
