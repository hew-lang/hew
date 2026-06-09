# WASM parity baseline — cross-target inventory

Canonical inventory of every `WASM-TODO` site in `hew-runtime/src/` at the
pinned base SHA, plus the results of three orthogonal greps, a live
`cargo check --target wasm32-wasip1` probe, and a Rust wasm integration-test
audit. Results consumed by follow-on work covering layout-backed
HashMap/HashSet codegen parity, `HewTypeLayout` usize-shape, supervisor/duplex
codegen parity, cooperate-cancellation, and `HewTypeLayout` wasm32 codegen.

## Reference SHA

**Pinned base:** `0b955839667bc0d31409c9dbe362457366f3ed88`
(`feat(checker): infer closure capture modes and classify escape kind`)

## Three orthogonal greps (Tenet 5 — audit-completeness-via-multiple-greps)

### Grep 1 — `WASM-TODO` exhaustive

```
grep -rn "WASM-TODO" hew-runtime/src/ hew-lib/src/ hew-codegen-rs/src/ hew-mir/src/ hew-types/src/
```

Results at 0b955839:

| Scope | File count | Site count |
|---|---|---|
| `hew-runtime/src/` | 12 | **33** |
| `hew-lib/src/` | 0 | 0 |
| `hew-codegen-rs/src/` | 1 (`llvm.rs`) | 22 |
| `hew-mir/src/` | 0 | 0 |
| `hew-types/src/` | 1 (`check/types.rs`) | 9 |

**Total cross-codebase:** 64 sites. This audit focuses on `hew-runtime/src/`
(33 sites) — the primary gap surface; codegen and checker sites are
catalogued separately below (§3).

**Count delta vs plan prediction:** The wasm parity plan (authored at `fed11980`)
predicted 32 sites in `hew-runtime/src/`. The actual count is **33** at both
`fed11980` and `0b955839` — the plan prediction was an undercount by one site.
The 33-site count was already present at plan-authoring time; no new WASM-TODO
sites were added between `fed11980` and `0b955839`. The additional site is
`actor.rs:2736` (the `drain_actors` wasm32 stub), present since `8c52b924`
(`feat(runtime): add drain_actors primitive`, which predates `fed11980`). The
plan prediction appears to have been based on an incomplete grep. The multi-grep
approach in this audit (Tenet 5) catches this kind of planning-time undercount.

### Grep 2 — `#[cfg(…target_arch = "wasm32"…)]` attribute gates in hew-runtime

```
grep -rnE "#\[cfg\(.*target_arch\s*=\s*\"wasm32\"" hew-runtime/src/
```

342 attribute-gate lines across 31 files. All WASM-divergent runtime functions
use compile-time `#[cfg(…)]` attributes; no runtime `cfg!(…)` macro calls were
found (Grep 3 below confirms).

Key module-level gates in `hew-runtime/src/lib.rs`:

| Module | Gate | Effect on wasm32 |
|---|---|---|
| `blocking_pool` | `#[cfg(not(target_arch = "wasm32"))]` | excluded |
| `task_scope` | `#[cfg(not(target_arch = "wasm32"))]` | excluded |
| `transport` | `#[cfg(not(target_arch = "wasm32"))]` | excluded |
| `scheduler` | `#[cfg(not(target_arch = "wasm32"))]` | excluded |
| `mailbox` | `#[cfg(not(target_arch = "wasm32"))]` | excluded |
| `arena` | `#[cfg(not(target_arch = "wasm32"))]` / `#[cfg(target_arch = "wasm32")]` → `arena_wasm` | redirected |
| `bridge` | `#[cfg(any(target_arch = "wasm32", test))]` | wasm32-only |
| `scheduler_wasm` | `#[cfg(any(target_arch = "wasm32", test))]` | wasm32-only |
| `mailbox_wasm` | `#[cfg(any(target_arch = "wasm32", test))]` | wasm32-only |
| `timer_periodic_wasm` | `#[cfg(any(target_arch = "wasm32", test))]` | wasm32-only |
| `actor` | *(no module-level gate)* | always compiled |
| `hashmap`, `hashset`, `vec`, `string`, `stream`, `duration`, `bytes` | *(no module-level gate)* | always compiled |

### Grep 3 — `cfg!(target_arch = "wasm32")` runtime branches

```
grep -rnE "cfg!\(target_arch\s*=\s*\"wasm32\"\)" hew-runtime/src/ hew-codegen-rs/src/
```

**Zero hits.** The runtime uses `#[cfg(…)]` compile-time attributes exclusively;
no `cfg!()` macro runtime branches exist. This is the correct pattern:
divergence is determined at compile time, not at runtime.

---

## Critical finding: `cargo check --target wasm32-wasip1` FAILS at base

```
cargo check --target wasm32-wasip1 -p hew-runtime
```

**Fails with:**

```
error[E0080]: evaluation panicked: assertion failed:
  offset_of!(HewExecutionContext, supervisor_child_index) ==
  HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX
  --> hew-runtime/src/execution_context.rs:250:5
```

**Root cause:** `HewExecutionContext` embeds raw pointer fields (`actor`,
`parent_supervisor`, `cancel_token`, `task_scope`, `arena`, etc.) whose sizes
differ between 64-bit native (8 bytes each) and wasm32 (4 bytes each). The
`HEW_CTX_OFFSET_*` constants are hardcoded for 64-bit layout:

```
HEW_CTX_OFFSET_ACTOR              = 0
HEW_CTX_OFFSET_ACTOR_ID           = 8
HEW_CTX_OFFSET_PARENT_SUPERVISOR  = 16
HEW_CTX_OFFSET_SUPERVISOR_CHILD_INDEX = 24  ← fails on wasm32
```

On wasm32, `actor` is 4 bytes → `actor_id` aligns to offset 8 → `parent_supervisor`
is 4 bytes starting at 16 → `supervisor_child_index` lands at **20**, not 24.
The `const` assertion at `execution_context.rs:247-265` fires during
monomorphization on the wasm32 target.

**This site has NO `WASM-TODO` comment.** It is a **previously-undocumented
broken site** discovered by this baseline audit's live compile probe.

**Implication for 13 "stale-comment" sites (hashmap/hashset):** The claim in
the plan that "symbols compile on wasm32" cannot be verified in the current
baseline — the crate doesn't compile on wasm32-wasip1 at all. The stale-comment
classification is still the correct intent (those functions have no behavioral
wasm32 gap beyond the codegen call-site gap), but the `execution_context.rs`
layout fix must land before any wasm32-wasip1 verification can proceed.

---

## WASM-TODO site inventory — hew-runtime/src/ (33 sites, 12 files)

### Classification key

| Category | Meaning |
|---|---|
| **A — Stale comment** | Symbol compiles on wasm32 (no `cfg` gate); comment "not yet ported" is misleading — function body exists and would work if called; actual gap is in codegen (no call-site emitted for wasm32 Hew programs) |
| **B — Real-but-unreachable** | Compiles on wasm32 (module included), behavioral divergence or known gap, no test exercises it on wasm32; or wasm32-only impl with documented limitation |
| **C — Cfg-gated out** | Module or function excluded from wasm32 by `#[cfg(not(target_arch = "wasm32"))]`; WASM-TODO documents why |

### Tier R1 — Stdlib types (every Hew program reaches these)

| File:Line | Symbol / Description | Category | Current state | Recommended action |
|---|---|---|---|---|
| `hashmap.rs:1293` | `hew_hashmap_new_with_layout` | **A — Stale comment** | No `cfg` gate; compiles on wasm32. Comment says "not yet ported." Codegen gap: `llvm.rs:166` emits `WASM-TODO(layout-backed HashMap wasm parity not yet designed)` | Remove stale comment; add wasm32-wasip1 round-trip test (blocked on layout fix) |
| `hashmap.rs:1365` | `hew_hashmap_insert_layout` | **A — Stale comment** | Same pattern as above | Same |
| `hashmap.rs:1468` | `hew_hashmap_get_layout` | **A — Stale comment** | Same pattern | Same |
| `hashmap.rs:1514` | `hew_hashmap_contains_key_layout` | **A — Stale comment** | Same pattern | Same |
| `hashmap.rs:1530` | `hew_hashmap_remove_layout` | **A — Stale comment** | Same pattern | Same |
| `hashmap.rs:1579` | `hew_hashmap_len_layout` | **A — Stale comment** | Same pattern | Same |
| `hashmap.rs:1619` | `hew_hashmap_free_layout` | **A — Stale comment** | Same pattern | Same |
| `hashset.rs:609` | `hew_hashset_new_with_layout` | **A — Stale comment** | No `cfg` gate; compiles on wasm32. Comment says "not yet ported." Codegen gap: `llvm.rs:171` | Remove stale comment; add test (blocked on layout fix) |
| `hashset.rs:662` | `hew_hashset_insert_layout` | **A — Stale comment** | Same pattern | Same |
| `hashset.rs:687` | `hew_hashset_contains_layout` | **A — Stale comment** | Same pattern | Same |
| `hashset.rs:712` | `hew_hashset_remove_layout` | **A — Stale comment** | Same pattern | Same |
| `hashset.rs:735` | `hew_hashset_len_layout` | **A — Stale comment** | Same pattern | Same |
| `hashset.rs:757` | `hew_hashset_free_layout` | **A — Stale comment** | Same pattern | Same |
| `vec.rs:1553` | `hew_vec_remove_at_layout` | **B — Real behavioral gap** | No `cfg` gate; compiles on wasm32. Comment documents a genuine issue: `HewTypeLayout.size` is `usize`-width; on wasm32 (`usize = u32`) the elem-size arithmetic and descriptor struct shape differ from native. Not merely a codegen-callsite gap — the function body itself contains `usize`-dependent arithmetic that produces wrong results on wasm32 | Add `#[cfg(target_arch = "wasm32")]` guard or add wasm32-specific arithmetic path; add round-trip test per LESSONS Wire Protocol rule |

**Stale-comment count: 13 sites** (hashmap×7 + hashset×6).
Plan prediction: 13. **Confirmed.**

### Tier R2 — Scheduler / actor / timer (every actor program reaches these)

| File:Line | Symbol / Description | Category | Current state | Recommended action |
|---|---|---|---|---|
| `scheduler_wasm.rs:54` | Drain-time `actor_type_id` zero | **B — Real behavioral gap** | Inside `#[cfg(any(target_arch = "wasm32", test))]` module. `actor_type_id` field is not threaded from codegen registration on wasm32; trace lifecycle events carry zero actor type ID. WASM-R37-S9 cross-reference | Thread `actor_type_id` through WASM codegen registration path |
| `scheduler_wasm.rs:306` | `SLEEP_QUEUE: Vec<(u64, *mut HewActor)>` | **B — Real behavioral gap** | Sorted-Vec sleep queue; insert/drain/cancel are O(n)/O(n²). LESSONS `silent-leak-on-timeout` cites this site explicitly | Add bounded cap + leak counter (option-b per §sleep-queue in the parity plan) |
| `scheduler_wasm.rs:1421` | `hew_actor_cooperate` depth-cap yield | **B — Real behavioral gap** | Returns `1` (yielded) without a real scheduler tick; native cooperate yields to OS scheduler. WASM never returns `2` (cancellation), so codegen's cancel-on-cooperate path (`llvm.rs:5646-5695`) is dead on wasm32 | Implement cooperate cancellation (return `2` when actor-stop injected) |
| `scheduler_wasm.rs:1465` | Cross-task cancel/task_scope missing | **B — Real behavioral gap** | Inside `#[cfg(target_arch = "wasm32")]` block; `cancel_token` and `scope` are `let _ =`-suppressed. No per-actor cancellation path | Implement per-actor cancel observation (not cross-task; checker gates that) |
| `actor.rs:2736` | `drain_actors` stub | **B — Real behavioral gap** | `#[cfg(target_arch = "wasm32")]` impl returns `DrainOutcome::Incomplete` unconditionally for any non-empty actor list. Native counterpart at `:2700` actually drains | Implement drain using WASM scheduler tick loop |
| `actor.rs:4523` | `actor_type_id` zero at `hew_actor_close` trace | **B — Real behavioral gap** | WASM-R37-S9 cross-reference; no handler-name/type registration in WASM codegen yet | Same fix: thread `actor_type_id` through WASM codegen registration path |
| `actor.rs:4546` | `actor_type_id` zero at SLEEPING→STOPPED | **B — Real behavioral gap** | Same as above | Same |
| `actor.rs:4581` | `actor_type_id` zero at `hew_actor_stop` trace | **B — Real behavioral gap** | Same as above | Same |
| `actor.rs:4602` | `actor_type_id` zero at stop/timeout path | **B — Real behavioral gap** | Same as above | Same |
| `timer_periodic_wasm.rs:48` | `PERIODIC_QUEUE: Vec<PeriodicEntry>` | **B — Real behavioral gap** | `#[cfg(any(target_arch = "wasm32", test))]` module. Cooperative Vec-backed timer queue; insert/drain/cancel are O(n)/O(n²). Correlates with #1679 (wasm32 periodic timer test gap) | Same bounded-cap approach as sleep queue |
| `bridge.rs:155` | `handler_names` trace metadata incomplete | **B — Real behavioral gap** | `#[cfg(any(target_arch = "wasm32", test))]` module. Comment describes that actor metadata is produced by `hew_wasm_register_actor_meta` — this is the mechanism, not a gap per se. But trace actor metadata is incomplete until codegen emits handler registrations (linked to actor_type_id zero) | Verify handler registration path works end-to-end; annotate as resolved or escalate |

### Tier R3/R4 — Internal / explicitly deferred (checker-gated or transport-only)

| File:Line | Symbol / Description | Category | Current state | Recommended action |
|---|---|---|---|---|
| `task_scope.rs:1467` | `hew_task_scope_cancel_one` | **C — Cfg-gated out** | Module-level `#[cfg(not(target_arch = "wasm32"))]` in `lib.rs:466`. Correctly excluded. Comment documents the checker rule that enforces this | Verify checker rejects `task_scope` calls on wasm32 with typed `PlatformLimitation`; update comment to cite the specific check rule |
| `task_scope.rs:1660` | Task-reaper thread | **C — Cfg-gated out** | Same module-level gate. The comment is in a `#[cfg(test)]` block inside the native-only module | No code action needed; note is for test-path only |
| `blocking_pool.rs:186` | `hew_blocking_pool_run_until` | **C — Cfg-gated out** | Module-level `#[cfg(not(target_arch = "wasm32"))]` in `lib.rs:461`. Comment documents that no WASM blocking pool exists | Verify checker gates all `blocking_pool` callers on wasm32 |
| `blocking_pool.rs:314` | `shared_blocking_pool` | **C — Cfg-gated out** | Same module-level gate | Same as above |
| `transport.rs:321` | `TcpCounters` | **C — Cfg-gated out** | Module-level `#[cfg(not(target_arch = "wasm32"))]` in `lib.rs:480`. Comment notes TcpCounters always returns zeros on WASM — but since the module is excluded, the comment is in dead code | Remove the misleading comment (dead code doc) |
| `stream.rs:860` | `hew_tcp_stream_from_conn` (native) | **C — Cfg-gated out** | `#[cfg(not(target_arch = "wasm32"))]`; comment explains the wasm32 stub exists at `:919` | No action needed; paired with stub below |
| `stream.rs:919` | `hew_tcp_stream_from_conn` (wasm stub) | **B — Real behavioral gap** | `#[cfg(target_arch = "wasm32")]`; compiled on wasm32, returns `null`. This IS the wasm32 implementation — a correctly-typed stub with no real TCP transport. WASM-TODO documents that TCP transport is unavailable | Verify `null` return is accepted at all call sites (checker should gate TCP stream usage on wasm32) |
| `lib.rs:183` | Profiler `WASM-TODO` | **C — Cfg-gated out** | Profiler module: `#[cfg(all(feature = "profiler", not(target_arch = "wasm32")))]`. The WASM-TODO comment appears after the cfg-gated module declaration. pprof requires OS threads + HTTP; deferral is correct and documented | No action needed (deferred is correct); `lib.rs:186-187` provides no-op stubs |

---

## Site count summary

| Category | Count | Files |
|---|---|---|
| A — Stale comment (compiles, misleading comment) | **13** | hashmap.rs (7), hashset.rs (6) |
| B — Real behavioral gap | **13** | vec.rs (1), scheduler_wasm.rs (4), actor.rs (5), timer_periodic_wasm.rs (1), bridge.rs (1), stream.rs (1) |
| C — Cfg-gated out | **7** | task_scope.rs (2), blocking_pool.rs (2), transport.rs (1), stream.rs (1), lib.rs (1) |
| **Total hew-runtime/src/** | **33** | 12 files |

`stream.rs` appears in both B and C: `:919` (`#[cfg(target_arch = "wasm32")]`, returns null) is B; `:860` (`#[cfg(not(target_arch = "wasm32"))]`, excluded) is C. Each is counted once in its respective category.

**Vs plan prediction (32 sites):** +1 delta. The additional site is
`actor.rs:2736` (the `drain_actors` wasm32 stub, present since `8c52b924`
which predates plan authoring at `fed11980`). No new WASM-TODO sites were
introduced between plan authoring and the audit base SHA. The plan prediction
undercount was a grep-time omission at authoring.

---

## User-reachable rank

| Rank | Description | Sites | Count |
|---|---|---|---|
| **R1** | Stdlib types — every Hew program using HashMap/HashSet/Vec reaches these | hashmap×7, hashset×6, vec×1 | 14 |
| **R2** | Scheduler / actor / timer — every actor program hits these | scheduler_wasm×4, actor×5, timer_periodic_wasm×1, bridge×1 | 11 |
| **R3** | Internal concurrency substrate — checker-rejected on wasm32 | task_scope×2, blocking_pool×2 | 4 |
| **R4** | Transport / observability — TCP stack, pprof; no wasm32 path planned | transport×1, stream×2, lib.rs×1 | 4 |

**User-reachable (R1+R2):** 25 sites.
**Internal / deferred (R3+R4):** 8 sites.

Plan stated R1+R2 = 24. Delta of +1 is the additional `actor.rs` R2 site.

---

## Adjacent gap surface (no WASM-TODO, but cited in parity matrix)

These sites have divergent behavior on wasm32 but carry no `WASM-TODO` comment.
Confirmed at 0b955839:

| Site | Gap | Parity matrix ref | Severity |
|---|---|---|---|
| `execution_context.rs:247-265` | **Compile failure on wasm32-wasip1**: `HEW_CTX_OFFSET_*` constants hardcoded for 64-bit; `supervisor_child_index` offset assertion panics at monomorphization. **NEW — not in WASM-TODO list, not in parity matrix** | *(not previously tracked)* | **should-close-before-0.5** (blocks all wasm32 verification) |
| `scheduler_wasm.rs:310-315,950-959,1374-1383` | Global `CURRENT_REPLY_CHANNEL` instead of ctx-carried channel (silent-divergence) | §1 row 4 | should-close-before-0.5 |
| `arena_wasm.rs:364-368` | `HeapExceeded` returns `null` instead of typed trap | §2 row 3 | should-close-before-0.5 |
| `hew-codegen-rs/src/llvm.rs:6340-6364` | WASM linker imports `hew_trap_with_code` as undefined symbol | §4 row 1 | should-close-before-0.5 |
| `actor.rs:3392-3477,3508-3534` | `hew_panic` on WASM falls through to process exit 101 with no supervisor recovery | §4 row 2 | audit-needed |
| `jit-symbol-classification.toml` | No `target` bucket; all symbols in `stable`/`codegen-stable`/`internal` without wasm32 vs native distinction | §6 row 1 | silent-divergence |
| `duration.rs` | **Not a gap** — `hew_seconds` / `hew_milliseconds` / `HewDuration` ARE present with no cfg gate; parity matrix §7 row 2 appears stale for this sub-item. Verified at 0b955839. | §7 row 2 (partial) | stale parity matrix row |

---

## WASM integration-test audit

**Status correction vs plan:** The plan described tests named
`wasm_e2e_toml_toml_try_parse`, `wasm_e2e_timeout_timeout`, and
`wasm_e2e_actors_actor_periodic_timer` as "pre-existing failing tests." Those
names were CMake ctest selectors from a prior build-system era.

**`hew-codegen/tests/CMakeLists.txt` does not exist at `0b955839`.** Confirmed:

```
git ls-tree -r --name-only 0b955839 | grep -i 'CMakeLists'
# (empty — CMake ctest infrastructure not present)
```

The build system migrated from CMake ctest to cargo/nextest. The current WASM
integration-test infrastructure at `0b955839` is:

| File | Coverage |
|---|---|
| `hew-cli/tests/compile_wasm_parity.rs` | Wasm parity compilation via `hew` CLI; one test is `#[ignore]` pending HIR→MIR duplex surface (`WASM-TODO(#1451)`) |
| `hew-codegen-rs/tests/wasm_duplex_classification.rs` | Duplex classification on wasm32 builds |
| `hew-codegen-rs/tests/wasm_actor_metadata_emission.rs` | Actor metadata emission tests |
| `hew-wasm/tests/v05_wasm_coverage.rs` | v0.5 fixture coverage via `hew_wasm` API (runs native target only) |
| `hew-runtime/src/wasm_parity_tests.rs` | Runtime parity tests (actor stop lifecycle, added by `06a481de`) |

**Test gaps still open:** #1678 (include `hew-std-encoding-toml` in wasm32-wasip1
runtime archives) and #1679 (periodic timer and timeout e2e wasm tests) have no
direct Rust integration-test equivalents yet. These gaps are tracked in the
respective issues and are not blocked by this audit.

**The `execution_context.rs` compile failure** would prevent any `cargo check
--target wasm32-wasip1 -p hew-runtime` from succeeding regardless of test
infrastructure, so all wasm32 verification is blocked until that fix lands (see
§Critical finding above).

**Reproduce commands:**

```bash
# Confirm no CMakeLists.txt exists
git ls-tree -r --name-only 0b955839 | grep -i CMakeLists
# Expected: (empty)

# Run current WASM parity integration tests (native target)
cargo nextest run -p hew-cli --test compile_wasm_parity
cargo nextest run -p hew-codegen-rs --test wasm_duplex_classification
cargo nextest run -p hew-codegen-rs --test wasm_actor_metadata_emission

# Confirm runtime wasm_parity_tests module is present
grep -n "mod wasm_parity_tests" hew-runtime/src/lib.rs
# Expected: hew-runtime/src/lib.rs:477:mod wasm_parity_tests;
```

---

## Codegen and checker WASM-TODO sites (informational)

Not primary scope of this baseline audit, but inventoried for follow-on work:

### hew-codegen-rs/src/llvm.rs (22 sites)

| Lines | Topic | Follow-on work |
|---|---|---|
| 130,153,159,162,174,362,365,369,385,406,413,418,422,431 | `#1451` duplex/supervisor/TCP/task-scope platform classification | supervisor/duplex codegen parity |
| 155 | `#1475` supervisor restart on wasm32 | supervisor/duplex codegen parity |
| 166,171,464 | Layout-backed HashMap/HashSet call-site codegen | layout-backed HashMap/HashSet codegen parity |
| 8294,9612,9923,9967 | `#1819` `HewTypeLayout` usize/struct shape on wasm32 | `HewTypeLayout` usize-shape parity |
| 16234 | WASM host-imports instead of linker errors | supervisor/duplex codegen parity |

### hew-types/src/check/types.rs (9 sites)

All `#1451`. I/O stream adapters, HTTP server, socket listener, subprocess,
TLS bridge, QUIC bridge, OS resolver, POSIX signal, entropy. All correctly
`#[cfg(not(target_arch = "wasm32"))]`-gated at the checker level; WASM-TODO
documents the design path. These are R4-equivalent; no action before 0.5.

---

## Plan filename reconciliation

The plan that spawned this audit was authored at `fed11980` under a working
filename (using a `-matrix-` infix). The canonical plan filename uses the
`-wasm-parity-` infix. Any tooling that cross-references the plan by the older
working filename will produce a stale reference. **Action required (not in this
audit commit; file a follow-up edit):** update any index or cross-reference
from the old working filename to the canonical one.

---

## Recommended sequencing for follow-on work

The `execution_context.rs` compile failure is a **prerequisite blocker** that
must be resolved before any wasm32-wasip1 verification can run. Suggested
insertion before existing R1 work:

**Prerequisite fix — `HewExecutionContext` wasm32 layout**

Options:
1. Add `#[cfg(target_arch = "wasm32")]` offsets computed for 32-bit pointer
   sizes, protected by conditional `HEW_CTX_OFFSET_*` constants.
2. Compute offsets via `offset_of!` at definition time (no hardcoded constants),
   removing the const assertions entirely. This is the more robust approach.

Without this fix, `cargo check --target wasm32-wasip1 -p hew-runtime` fails
and no WASM parity progress can be verified.

Revised work order:

| Prerequisite | Primary targets | Description |
|---|---|---|
| — | `docs/audits/wasm-parity-baseline/inventory.md` | This baseline audit |
| This audit | `execution_context.rs` | Fix wasm32 layout constants (prerequisite — blocks all wasm32 verification) |
| Layout fix | hashmap.rs, hashset.rs, vec.rs | R1 stale-comment cleanup + wasm32 call-site codegen |
| R1 cleanup | scheduler_wasm.rs:306, timer_periodic_wasm.rs:48 | Bounded sleep/periodic queues |
| Bounded queues | scheduler_wasm.rs:1421,1465 | Cooperate cancellation |
| Cooperate cancel | actor.rs:2736,4523,4546,4581,4602 | Drain + actor_type_id threading |
| Actor drain | hew-codegen-rs llvm.rs:8294,9612,9923,9967 | `HewTypeLayout` usize-shape codegen |
| Actor drain | task_scope.rs, blocking_pool.rs | Checker-gate verification |
| Checker gates | transport.rs, stream.rs, lib.rs | R4 doc refresh |
| R4 refresh | `ci-preflight-dispatcher.sh` | wasm-parity CI gate |
| CI gate | scheduler_wasm.rs, arena_wasm.rs, jit-symbol-classification.toml | Silent-divergence fixes |

Layout-backed HashMap/HashSet codegen parity, supervisor/duplex codegen,
cooperate-cancellation, and `HewTypeLayout` wasm32 codegen work all depend on
the `execution_context.rs` layout fix (see §Critical finding above) being
merged, because any `cargo check --target wasm32-wasip1` in those work items
will fail until the layout constants are corrected.

---

## Validation commands

```bash
# 1. Reproduce Grep 1 (must match the 33-site count above)
grep -rn "WASM-TODO" hew-runtime/src/ | wc -l
# Expected: 33

# 2. Confirm Grep 3 zero-hits
grep -rnE "cfg!\(target_arch\s*=\s*\"wasm32\"\)" hew-runtime/src/ hew-codegen-rs/src/
# Expected: (empty)

# 3. Reproduce wasm32-wasip1 compile failure
cargo check --target wasm32-wasip1 -p hew-runtime
# Expected: error[E0080] in execution_context.rs:250

# 4. Native runtime regression guard
cargo test -p hew-runtime --release 2>&1 | tail -5
# Expected: test result: ok

# 5. Confirm CMake ctest infrastructure absent (migrated to cargo/nextest)
git ls-tree -r --name-only 0b955839 | grep -i CMakeLists
# Expected: (empty)

# 6. Confirm current WASM parity Rust tests compile
cargo nextest run -p hew-cli --test compile_wasm_parity --no-run
# Expected: success (some tests may be #[ignore] at runtime)
```
