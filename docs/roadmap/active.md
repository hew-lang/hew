# Hew v0.6 — Active Development State

> Authoritative, tracked record of in-progress v0.6 work. **Inspect before starting work; update
> before claiming work done; verify completion criteria before merge; run the post-merge checklist
> after merge.** This file — not memory, chat history, or branch names — is the source of truth for
> what is in progress, who owns it, and what remains.

## v0.6 objective ordering (fundamentals first)

Later work benefits from a stronger core, so work is sequenced:

1. **Language semantics & parser correctness**
2. **Typechecking & diagnostics**
3. **Runtime execution model**
4. **Actor / state-machine / runtime lifecycle**
5. **Standard library & user-facing polish**
6. **Cleanup, deduplication, and final cutovers**

All known, planned, relevant work is v0.6-valid. Sequencing orders by dependency, risk, and impact;
it is never a reason to defer, forget, or abandon known work. There is no separate deferral list.

## Branch naming

`v0.6-<area>-<outcome>` — names link clearly to the outcome. Examples:
`v0.6-runtime-timer-cutover`, `v0.6-typechecker-generic-fn-value`, `v0.6-remove-legacy-diagnostics`.

## Post-merge lifecycle — run after EVERY merge

1. Switch to the integration branch (`main`).
2. Pull/rebase the latest state.
3. Delete/archive the merged branch locally and prune its worktree.
4. Confirm the PR is merged/closed.
5. Update this file (move the entry to **Completed**).
6. Remove the completed item from **Active**.
7. Record any discovered follow-up only if it is real, specific, and still v0.6-relevant.
8. Search for duplicate or legacy implementations of the merged outcome.
9. Complete the cutover to the cleaner implementation; remove the obsolete path.
10. Run the relevant tests before declaring the work done.

## Duplicate / legacy policy

For every implementation, actively look for a competing system that produces the same outcome. If a
legacy and a current implementation both exist, complete the cutover and remove the obsolete path. Do
not leave two competing implementations unless there is a documented, temporary migration reason with
a concrete removal step recorded here.

---

## Active work

### feat/array-repeat-owned-clone  *(rename target: v0.6-runtime-vec-string-drop)*
- **PR:** none yet
- **Outcome:** owned-element array-repeat `[s; N]` (clone-per-element) + fix a pre-existing Vec<string> element-drop leak.
- **v0.6 objective:** 3 — runtime execution model (memory safety).
- **Files/modules:** hew-mir/src/lower.rs (string `==`/`!=` operand drop), hew-runtime/src/vec.rs, hew-cli leak oracle.
- **Legacy paths replaced:** none (correctness fix).
- **Completion criteria:** `["x";3]` and `Vec<string>` index-into-compare are leak-clean (`leaks --atExit` = 0); record/scalar/`println(xs[i])` paths stay clean (no double-free); Clone-gate rejects uncloneable.
- **Required tests:** array-repeat fixtures + a new constant-leak oracle (the slope-only oracle missed this) + `make test-hew-ratchet`.
- **Post-merge cleanup:** prune worktree+branch; close the leak as resolved.
- **Follow-up state updates:** mark the Vec<string> drop leak fixed in the runtime notes.

### feat/actor-ctor-init-coexist  *(rename target: v0.6-runtime-actor-ctor-init)*
- **PR:** none yet (rebasing onto current main)
- **Outcome:** actor spawn accepts state-field names as spawn args alongside `init()` params, routed into state.
- **v0.6 objective:** 4 — actor/runtime lifecycle.
- **Files/modules:** hew-mir/src/lower.rs (`lower_spawn_actor`).
- **Legacy paths replaced:** none (loosens a too-strict gate).
- **Completion criteria:** state-field spawn arg initializes the field (no UnresolvedPlace); pure-init + no-init paths unchanged; restart-safe.
- **Required tests:** hew-mir + hew-types lanes; E2E (coexist=110, init=42, no-init=42); flake 3/3.
- **Post-merge cleanup:** prune worktree+branch.
- **Follow-up state updates:** none.

### fix/v060-delete-timer-list  *(rename target: v0.6-runtime-timer-cutover)*
- **PR:** none yet (committed + pushed, ready for review)
- **Outcome:** delete the legacy sorted-list timer ABI; the tickless wheel is the sole timer.
- **v0.6 objective:** 3 — runtime execution model. **Cutover: removes a legacy implementation.**
- **Files/modules:** hew-runtime timer.rs (deleted), timer_wheel (inlined HewTimerCb), JIT classification table.
- **Legacy paths replaced:** the sorted-list timer (382 lines, 6 extern-C symbols) — REMOVED.
- **Completion criteria:** 0 references to the deleted ABI; tickless wheel idle-ticks correctly (no busy-spin); timer/scheduler tests pass.
- **Required tests:** hew-runtime lane + timer/scheduler E2E.
- **Post-merge cleanup:** prune worktree+branch.
- **Follow-up state updates:** confirm the timer idle-tick objective satisfied.

### feat/generic-fn-value  *(rename target: v0.6-typechecker-generic-fn-value)*
- **PR:** none yet (~60% — checker done, HIR pending)
- **Outcome:** cross-module generic function used as a value (monomorphic shim selection from context).
- **v0.6 objective:** 2 — typechecking. 
- **Files/modules:** hew-types/src/check/calls.rs, hew-mir/src/lower.rs.
- **Legacy paths replaced:** none (removes a NYI rejection).
- **Completion criteria:** context-determined generic-fn-value compiles + runs; ambiguous fails closed; direct-call/lambda-wrap unaffected.
- **Required tests:** hew-types + hew-mir lanes; cross-module fn-value E2E.
- **Post-merge cleanup:** prune worktree+branch.
- **Follow-up state updates:** none.

---

## Planned (sequenced; all v0.6-valid, no deferral)

**Tier 1–2 (semantics/parser, typecheck/diagnostics):** match-destructure remaining NYI (R14/R15/R20); regex-as-value + regex-match-arm; generic Display/println of T (static dispatch); the 4 task/scope NYIs (generic free-fn spawn, contextless top-level spawn, non-unit fork result, scope-deadline contextless); structural-eq managed-field + sandbox-WASM aggregate compare.

**Tier 3–4 (runtime, actor lifecycle):** consolidate the monotonic-clock epochs to one process epoch; confirm the activation CAS/marker-gap residual + send pre-check; deglob actor-runtime residuals; value-task result channel; await/net per-operation leaks; teardown/poll yield-starvation.

**Tier 6 (cleanup/dedup/cutover):** codebase-wide duplicate-implementation / incomplete-cutover / dedup audit (produces concrete cutover lanes); split the oversized LLVM emission module into concern-modules; sret/byval ABI-classification layer; textual MIR dump + FileCheck goldens; enable the -O2 middle-end pipeline; refresh checked-MIR goldens + wire the verifier into CI.

**Dev-tools / CI:** asan-gate fixture-binary instrumentation; squash-merge detection fix (gh-confirm, not the advancing-main diff test); enable the shared compilation cache by default for local builds.

---

## Completed (recent)
- L6 CI coverage→nightly — #2051 — merged 2026-06-19.
- shorthand record destructure `let {a,b}=rec` — #2049 — merged 2026-06-19.
- generic `Vec<T>.get` element methods — #2050 — merged 2026-06-19.
- activation CAS/marker-gap UAF fix — #2043 — merged 2026-06-19.
- remove dead generator-state-layout vestige — #2046 — merged 2026-06-19.
- connection-lifecycle teardown races — #2048 — merged.
- runtime deglobalization (per-instance state) — #2045 — merged.
