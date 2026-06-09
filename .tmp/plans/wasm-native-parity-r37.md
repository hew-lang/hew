# R37 — WASM ↔ Native runtime parity

- **Lane id:** `wasm-native-parity-r37`
- **Status:** dispatchable (phased — each phase below is one PR-sized slice)
- **Recommended executor:** mixed — see per-slice Handoff
- **Cross-ecosystem reviewer:** runtime/codegen reviewer; opposite ecosystem per slice (codegen↔runtime, runtime↔checker)
- **Worktree base:** `.claude/worktrees/v05-integration` @ `986fa474`
- **User ratification:** keep documented WASM gaps in v0.5 ship; launch this lane in the background to converge on native parity. Bridge into v0.6 if it does not converge before v0.5 cuts.

## Substrate ownership claimed

This lane owns the **WASM runtime fail-closed contract** — the rule that the wasm32 target enforces the same observable error/lifecycle semantics as native, or surfaces a typed `PlatformLimitation` diagnostic before codegen. Specifically it owns:

- A single source of truth for which native behaviours are mirrored vs. checker-rejected vs. silently degraded.
- The per-surface parity invariant tests in `hew-runtime/src/wasm_parity_tests.rs`.
- The WASM ABI catalogue (`scripts/jit-symbol-classification.toml`) target-availability bucket.
- Trap/exit-code attribution for the WASI host so user-observable failure modes don't divergently silent-exit on wasm32.

Future features that compose on this substrate: WASI threads (#1451 closure), browser-runtime semantics for the analysis WASM, cluster/quic ports to wasm32, and any v0.6 capability that wants to ride the "either parity or named WASM-TODO" contract.

## Native ↔ WASM parity contract (authoritative)

For every runtime surface, exactly one of the following must hold:

1. **Parity** — the wasm32 target produces the same observable outcome as native, modulo documented performance cost. Verified by a row in `wasm_parity_tests.rs`.
2. **Fail-closed with typed diagnostic** — checker emits `PlatformLimitation` (`hew-types/src/check/types.rs:734-758`) before codegen, OR the WASM runtime traps with a documented `ExitReason`. Silent degradation (returning a default, `None`, zeroed metric, or `Ok(())` where native errors) is **forbidden**.
3. **Named `WASM-TODO(#<issue>)`** with: (a) a github issue, (b) a `#[cfg(target_arch = "wasm32")]`-gated panic, trap, or typed error, (c) a row in `docs/wasm-capability-matrix.md`, (d) a `#[should_panic]` or `#[ignore = "WASM-TODO(#N)"]` regression test that will flip on closure.

No fourth option exists. Reviewers reject PRs that introduce new wasm-side `cfg(target_arch = "wasm32")` divergence without picking one of the three.

## Surface inventory

Source: `.tmp/orchestration/wasm-native-parity-matrix-r37-2026-05-19.md` (Slice 0 refresh; supersedes `.tmp/orchestration/wasm-parity-matrix-2026-05-19.md` for this lane).

| # | Surface | Class today | Native cite | WASM cite |
|---|---|---|---|---|
| S1 | Reply-channel lookup (global vs ctx) | parity (Slice 0 refresh: stale row closed) | `hew-runtime/src/execution_context.rs:74-194`, `scheduler.rs:106-121` | `scheduler_wasm.rs:310-322`, `scheduler_wasm.rs:1372-1374` |
| S2 | `on(stop)` observability | named-gap, narrower | `actor.rs:1263-1373` (signal-recovery/longjmp + trace) | `actor.rs:1375-1413` (catch_unwind only); `scheduler_wasm.rs:1198-1276` (lifecycle trace omitted) |
| S3 | Cooperate / cancellation propagation | named-gap | `scheduler.rs:1192-1252`; codegen branch on `2` at `llvm.rs:5646-5695` | `scheduler_wasm.rs:1387-1491` (`WASM-TODO(#1451)`) |
| S4 | HeapExceeded trap attribution | partial parity (Slice 0 refresh: actor attribution closed; process-exit attribution remains via S5/S12) | `arena.rs:315-366` longjmp HEW_TRAP_HEAP_EXCEEDED | `arena_wasm.rs:364-430` stamp + panic; `trap_code.rs:40-63`; `wasm_parity_tests.rs:976-1155` |
| S5 | `hew_trap_with_code` / panic / exit mapping | named-gap (partially handled for actor-dispatch trap code) | `actor.rs:3392-3535`; `supervisor.rs:330-456` exit codes 200-206 | `trap_code.rs:40-63`; `actor.rs:3509-3535` (`hew_panic` falls to exit 101 outside native longjmp); `llvm.rs:5225-5250` emits `hew_trap_with_code` + `llvm.trap` |
| S6 | Supervisor restart | accept-as-known-gap → re-affirm | `supervisor.rs:1-5,673-690` | `lib.rs:366-442` runtime gated native-only; checker rejects (`check/types.rs:739-747`) |
| S7 | Duplex substrate | named-gap, gated | `duplex.rs` (native-only) | `hew-codegen-rs/src/llvm.rs:127-149,231-310` `WasmUnsupportedSubstrate` |
| S8 | `select {}` cooperative tick | parity-with-cost | `reply_channel.rs:484` | `reply_channel_wasm.rs:317-425` |
| S9 | Trace span / actor_type_id | named-gap | `scheduler.rs:816-835,1503-1574` | `scheduler_wasm.rs:950-959,1273-1276`; `tracing.rs:801-817` (`WASM-TODO(#1451)`) |
| S10 | JIT symbol classification | silent-divergence | `scripts/jit-symbol-classification.toml:1-9,57,170-180,526-622` | no target-availability bucket |
| S11 | Duration helpers ABI | parity (Slice 0 refresh: stale row closed) | `duration.rs:1-35`; `io_time.rs:16-20` re-export | `lib.rs:223-247` always compiles `duration`; `scripts/jit-symbol-classification.toml:314,456` |
| S12 | Exit-code / stdout WASI parity | named-gap | `hew-cli/tests/eval_e2e.rs:1291-1296` | `eval_e2e.rs:1297-1380` (ignored during cutover); `wasi_run_e2e.rs:47-103` |

## Evidence

1. **Discovery matrix.** `.tmp/orchestration/wasm-native-parity-matrix-r37-2026-05-19.md` is the Slice 0 refreshed grounding document. It supersedes `.tmp/orchestration/wasm-parity-matrix-2026-05-19.md` for this lane and carries local tracker IDs (`WASM-R37-S*`) instead of GitHub issue references.

2. **Live cite sample (verified by view at plan time):**
   - `hew-runtime/src/scheduler_wasm.rs:310-322` and `hew-runtime/src/execution_context.rs:147-194` — Slice 0 refresh verified S1 is already ctx-backed parity; Slice 1, if retained, is a test ratchet rather than a migration.
   - `hew-runtime/src/wasm_parity_tests.rs:976-1155`, `hew-runtime/src/arena_wasm.rs:364-430`, and `hew-runtime/src/trap_code.rs:40-63` — HeapExceeded actor attribution already lands; S4 splits into closed actor attribution plus S5/S12 process-exit attribution.
   - `hew-runtime/src/duration.rs:1-35` and `hew-runtime/src/lib.rs:223-247` — Slice 0 refresh verified S11 duration helper ABI is always compiled, not native-only; Slice 7 is now only an optional link-test ratchet.
   - `hew-runtime/src/tracing.rs:801-817` — single concrete `WASM-TODO(#1451)` for actor_type_id zeroing on WASM dispatch.
   - `hew-runtime/src/bridge.rs:154` — `WASM-TODO(#1451)` hew_register_handler_name ABI not emitted by WASM codegen.
   - `hew-runtime/src/blocking_pool.rs:186,314` — `WASM-TODO(#1451)` blocking pool absent.
   - `hew-runtime/src/task_scope.rs:1189,1381` — `WASM-TODO(#1451)` task_scope native-only.

3. **Test count:** `grep "wasm32" hew-runtime/src/actor.rs | wc -l` → 137 hits. The wasm lane spans 14 032 lines across the wasm-tagged files; this is **not** a single-PR refactor.

4. **Reproducible commands:**
   - `cargo test -p hew-runtime --target wasm32-wasip1 wasm_parity` — current passing wasm parity tests
   - `make ci-preflight` after each slice
   - `cargo build -p hew-runtime --target wasm32-wasip1 --release` — sanity wasm builds

## LESSONS triggers

- **`native-wasm-parity`** (P1, row 124) — **applies to every slice in this lane**. Each slice must either land both lanes or land a named WASM-TODO; each must add contract tests at timeout/cancel/budget edges. `session_reset` symmetry between `hew_sched_shutdown` paths is a checklist item.
- **`cleanup-all-exits`** (P0, row 98) — S2 (on(stop)), S5 (trap/panic), and the supervisor work all touch cleanup paths. Actor terminate callbacks are the only WASM-safe teardown until #1451 closes — any slice that asks otherwise is rejected.
- **`ffi-ownership-contracts`** (P0, row 99) — S1 (reply channel), S5 (trap codes), S10 (JIT symbol catalogue) touch FFI surfaces with last-error/ownership semantics. Idempotence and last-error-clear-on-success invariants apply.
- **`behavioral-regression-not-just-test-pass`** (P1, row 193) — applies to **every slice that converts a silent degradation into a typed error**. Reviewers ask: "did this change observable error/diagnostic behaviour and is the new behaviour the right semantic?" — slice worker-return must answer.
- **`audit-completeness-via-multiple-greps`** (P1, row 191) — Slice 0 (re-verify matrix) and the symbol classification slice (S10) require ≥3 grep angles each.
- **`wasm-diagnostic-json-contract`** (P1, row 256) — S5 / S12 changes that surface new typed wasm diagnostics into the analysis JSON must add field-presence + population tests in both error paths.
- **`producer-bridge-before-codegen`** (P1, row 195) — S5 lands a runtime trap consumer and a codegen producer; they must land in the same slice with the producer test gating the consumer.
- **`plan-prescribes-helper-implementer-honors`** (P1, row 192) — S2 must route terminate-with-trace through the existing `terminate_fn` helper, not reimplement.

## Validation candidates

Each slice must add at least one row to `wasm_parity_tests.rs` (or equivalent) **before** flipping behaviour.

- **Slice 0 (matrix refresh):** dry-run grep audit; produces a corrected `wasm-parity-matrix-2026-05-19.md` (or supersedes it with a new dated file). Slice 0 refresh published `.tmp/orchestration/wasm-native-parity-matrix-r37-2026-05-19.md`.
- **Slice 1 (S1 reply-channel):** Slice 0 refresh verified migration is already present. If this slice is dispatched, make it a regression-test ratchet asserting WASM `hew_get_reply_channel` reads from `HewExecutionContext`, not a global, by ablating the ctx and confirming a panic/diagnostic — **not** a silent null read.
- **Slice 2 (S2 on(stop) observability):** add a parity test asserting that `on(stop)` on wasm32 emits the same lifecycle trace events (begin/end + actor_type_id) as native, OR explicitly documents the omitted field with a typed `WASM-TODO(#N)` and `#[ignore]` regression test.
- **Slice 3 (S3 cooperate cancel):** add cancellation token parity test in wasm32; until #1451 closes, the test asserts `hew_actor_cooperate` returns the cancel code `2` from at least one cooperative cancel surface (e.g. mailbox close), not silently `0`.
- **Slice 4 (S5 trap/panic/exit):** add `wasm_parity_tests.rs` rows for `hew_trap_with_code` on wasm32 producing the correct WASI exit code (200-206 mapping), and `hew_panic` producing a non-101 attributed exit on wasm32.
- **Slice 5 (S9 trace + actor_type_id):** accepted with followups. Slice 5a added registration and non-zero actor attribution; the Slice 5b sweep found no remaining WASM scheduler trace-span implementation needed before R42.
- **Slice 6 (S10 JIT classification):** add a parser test on `scripts/jit-symbol-classification.toml` asserting every symbol has a target-availability bucket; ratchet by adding the bucket to ~50-80 entries.
- **Slice 7 (S11 duration ABI):** Slice 0 refresh verified `hew_seconds`/`hew_milliseconds` already live in the always-compiled `duration` module. Treat this as optional wasm32 link-test ratchet, not an implementation slice.
- **Slice 8 (S12 WASI eval/stdout):** un-`#[ignore]` the `eval_e2e.rs:1297-1380` tests behind the now-implemented trap mapping.
- **Slice 9 (supervisor/duplex re-affirm):** add checker test that asserts `PlatformLimitation` is emitted with a stable error code for each rejected surface (supervisor, duplex, task_scope, link/monitor) — fail-closed contract test, not a new runtime path.

## Staged execution order

Each slice = one commit-sized PR. Slices 1-9 stack on slice 0; within 1-9 they are **mostly independent** unless noted.

**Slice 0 — Refresh & ratify the parity matrix.** *(Sonnet, 1-2 days)*
- Re-audit each S-row above against current HEAD with ≥3 grep patterns each (LESSONS row 191).
- Correct S1 (likely already parity), S4 (partially handled — split into "trap stamp done" + "panic recovery open").
- Publish `wasm-native-parity-matrix-r37.md` (dated, supersedes 2026-05-19 doc).
- Local-only Slice 0 refresh: do **not** open GitHub tracker issues. Use stable local tracker IDs in the matrix (`WASM-R37-S*`) and have Slices 1-9 cite those IDs.
- **Out of scope for this slice:** any runtime/codegen change.

**Slice 1 — Confirm S1 closure or land the migration.** *(Sonnet)*
- If S1 is already parity (likely): delete the stale `CURRENT_REPLY_CHANNEL`-claim, add a regression test that ctx-less reply read is fail-closed, update LESSONS evidence row.
- If not: move WASM reply-channel into `HewExecutionContext` mirroring native `execution_context.rs:74-113`.

**Slice 2 — `on(stop)` observability parity (S2).** *(hew-implementer-opus)*
- Wire WASM terminate dispatch through the same lifecycle trace span as native (`scheduler.rs:1503-1574`).
- Must route through existing `terminate_fn` (LESSONS row 192) — no reimplementation.
- Test: parity row in `wasm_parity_tests.rs` asserting same span sequence + non-zero actor_type_id (depends on Slice 5 or carries a WASM-TODO).
- Risk: catch_unwind vs longjmp asymmetry — if signal-recovery is structurally incompatible on wasm32, slice degrades to typed WASM-TODO with regression test.

**Slice 3 — Cooperate cancellation propagation (S3).** *(hew-implementer-opus)*
- Tied to #1451 (task_scope on wasm). If #1451 stays open: minimal slice that propagates cooperate-return `2` from mailbox-close / scheduler-shutdown cancellation surfaces and keeps task_scope cancel as the named gap.
- Codegen on wasm32 must already branch on `2` (it does — `llvm.rs:5646-5695` is target-neutral). Verify codegen output.
- Test: cancel a wasm32 actor via mailbox close; assert dispatch sees cooperate=2.

**Slice 4 — Trap / panic / exit-code WASI attribution (S5).** *(hew-implementer-opus)*
- This is the largest slice. Implement `hew_trap_with_code` wasm32 host import that maps trap codes 200-206 to WASI exit codes matching native `supervisor.rs:330-456`.
- `hew_panic` on wasm32 must surface the attributed exit, not fall through to 101.
- Co-locate producer (codegen `llvm.rs:6340-6364` host-import declarations) and consumer (wasm32 runtime trap handler) in the same slice (LESSONS row 195).
- Test: wasm parity rows for each trap code; un-`#[ignore]` the wasi exit-code tests in slice 8.

**Slice 5 — Trace + actor_type_id registration on WASM codegen (S9).** *(hew-implementer-opus)*
- Close `WASM-TODO(#1451)` at `bridge.rs:154` and `tracing.rs:816` by emitting `hew_register_handler_name` ABI from WASM codegen.
- Producer (codegen) + consumer (runtime tracing) co-located (LESSONS row 195).
- Test: actor_type_id non-zero on wasm32 after spawn.

**Slice 6 — JIT symbol catalogue target-availability bucket (S10).** *(Sonnet)*
- Add `wasm_availability = "native_only" | "wasm_available" | "wasm_gated"` field to each row in `scripts/jit-symbol-classification.toml`.
- Add parser/test that asserts every entry has the field set (fail-closed schema).
- No runtime behaviour change — pure classification cleanup; unblocks future linker-time fail-closed checks.

**Slice 7 — Duration helper ABI on WASM (S11).** *(Sonnet)*
- Add `hew_seconds`/`hew_milliseconds` to the WASM runtime stub set in `lib.rs:247-325`.
- Test: link-test fixture compiling user code that uses these on wasm32.

**Slice 8 — WASI eval/stdout exit-code parity (S12).** *(Sonnet, depends on Slice 4)*
- Un-`#[ignore]` the wasi/eval tests at `eval_e2e.rs:1297-1380`, `wasi_run_e2e.rs:47-103`.
- Adjust expected exit codes to match the now-implemented trap mapping from Slice 4.

**Slice 9 — Supervisor / duplex / task_scope checker fail-closed contract tests (S6, S7).** *(Sonnet)*
- These remain "accept-as-known-gap" surfaces, but the lane re-affirms them with a stable `PlatformLimitation` error code per surface.
- Test: checker rejection test per surface with a pinned error code; reviewer guidance that v0.6 lifts these via #1451 or successor.

Dependency graph:
```
0 → {1, 2, 3, 4, 5, 6, 7, 9}
4 → 8
5 ↔ 2 (Slice 2 either pulls Slice 5 in or carries the actor_type_id WASM-TODO)
```

## Risks

- **Scope explosion.** This lane touches 6-9 PRs. Risk: the lane never converges and bleeds into v0.6 anyway. Mitigation: Slice 0 produces sharp tracker issues; each slice ships independently; v0.5 ships without requiring any of slices 1-9.
- **Signal/longjmp asymmetry (Slice 2, 4).** wasm32 has no longjmp. Some native cleanup paths are structurally incompatible. Mitigation: every Opus slice has a fall-back arm that converts the slice into a named WASM-TODO + regression test rather than failing closed-as-open.
- **#1451 dependency (Slices 3, 5).** Both reference the same open task_scope/WASM-codegen-registration tracker. If #1451 stays open across the lane, Slice 3 minimal-cancellation and Slice 5 actor_type_id are still independently valuable, but full task_scope parity is explicitly **out of scope** (v0.6).
- **Stale grounding (S1).** Slice 0 already suspects matrix S1 is wrong. Risk: other rows are also stale. Mitigation: Slice 0 re-audits with three grep angles per row.
- **Behavioural regression.** Converting silent-null returns into typed errors (S1, S5, S11) will fail tests that depended on the silent path. LESSONS row 193 applies — each slice's worker-return must enumerate observable error changes.
- **Sonnet vs. Opus mis-routing.** Slices 2, 3, 4, 5 are runtime+codegen co-changes with subtle trap/cooperate semantics; Sonnet will flail. Routed to Opus. Slices 0, 1, 6, 7, 8, 9 are auditable / surface-level; Sonnet is fine.

## Handoff

- **Slice 0:** `hew-implementer` (Sonnet). Outcome: refreshed matrix doc + tracker issues. Cross-ecosystem reviewer: runtime reviewer.
- **Slice 1:** `hew-implementer` (Sonnet). Cross-ecosystem reviewer: runtime reviewer.
- **Slice 2:** `hew-implementer-opus`. Lifecycle observability + terminate_fn helper discipline. Cross-ecosystem reviewer: codegen reviewer (since the trace span emission is target-neutral codegen).
- **Slice 3:** `hew-implementer-opus`. Cooperate/cancel semantics. Cross-ecosystem reviewer: codegen reviewer.
- **Slice 4:** `hew-implementer-opus`. Trap producer + consumer co-located. Cross-ecosystem reviewer: runtime reviewer.
- **Slice 5:** `hew-implementer-opus`. Codegen ABI emission + runtime tracing consumer. Cross-ecosystem reviewer: runtime reviewer.
- **Slice 6:** `hew-implementer` (Sonnet). Schema-only. Cross-ecosystem reviewer: tooling reviewer.
- **Slice 7:** `hew-implementer` (Sonnet). ABI surface. Cross-ecosystem reviewer: runtime reviewer.
- **Slice 8:** `hew-implementer` (Sonnet, depends on Slice 4 landing). Cross-ecosystem reviewer: CLI/test reviewer.
- **Slice 9:** `hew-implementer` (Sonnet). Checker test slice. Cross-ecosystem reviewer: types/checker reviewer.

Each slice's dispatch brief must include:
- "Your code MUST honour the three-option parity contract in this plan. Silent degradation is fail-closed reject."
- "Call `terminate_fn` / `emitFieldDropsForUserStruct` / existing helpers — do not reinvent (LESSONS row 192)."
- "Worker-return must answer the behavioural-regression question (LESSONS row 193)."

## Out of scope

- **Full `task_scope` / structured-concurrency parity on wasm32** — tracked at #1451; this lane explicitly does not close it. Slice 3 ships partial cooperate-cancel without waiting on #1451.
- **WASI threads** — pre-requisite to several deeper parity items; v0.6+ work.
- **Cluster / QUIC / TLS / DNS on wasm32** — already documented WASM-TODOs in `docs/wasm-capability-matrix.md`; out of this lane.
- **Browser-runtime VM semantics** — permanent non-goal per project mission. The analysis WASM gets fail-closed semantics via this lane; the browser runtime VM does not.
- **Re-architecting the WASM scheduler to be multi-threaded** — explicit Tier-2 design, not a parity gap.
- **TLS/QUIC capability classification** — covered by `docs/wasm-capability-matrix.md` lane separately.
- **Migrating `hew-codegen-rs` LLVM IR away from target-neutral emission** — out of scope; the target-neutral shape is what makes Slices 2/5 tractable.

## Dispatch-readiness checklist

- [x] Surface inventory present (12 rows)
- [x] Evidence cites concrete artefacts (matrix doc + live file:line cites verified at HEAD)
- [x] LESSONS triggers cited with row ids
- [x] Validation candidates named per slice
- [x] Staged execution order with one-commit-per-slice scope and dependency graph
- [x] Risks enumerated with mitigations
- [x] Handoff names executor tier per slice
- [x] Out of scope closes at least one drift door (closes 6: task_scope, WASI threads, cluster/quic, browser VM, scheduler re-arch, TLS capability)
- [x] Native↔WASM parity contract stated authoritatively
- [x] Substrate ownership section answers "what does this enable?"
