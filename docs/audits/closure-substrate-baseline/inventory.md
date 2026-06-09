# Closure substrate — baseline inventory

A reproducible numeric inventory of every code surface that subsequent
closure-substrate work will touch. Each row pins a `git grep` count to a
specific SHA so that follow-up commits can prove non-regression by re-running
the same probe and comparing.

## Reference SHAs

- **Inventory pin:** `fed11980` — the tree the count table below was originally
  authored against.
- **Tree tip at audit time:** `6e6456add4e36df9850e49c90f8dffc0cb7c0212` —
  one commit later (canonical LLVM fat-pointer type for `dyn Trait` values).
  The audit re-runs every probe at this SHA and documents the delta.

Raw `git grep` outputs for every probe are committed at `raw-output.txt`
alongside this file; the reviewer can reproduce any cell by running the
same command against the named SHA.

## Numeric inventory

| ID  | Surface                                                  | Pinned baseline @ `fed11980`                          | Observed @ `fed11980` | Observed @ `6e6456ad` | Delta |
|-----|----------------------------------------------------------|-------------------------------------------------------|-----------------------|-----------------------|-------|
| A1  | `ClosureCaptureFact` refs (hew-types + hew-hir)          | 12 hits / 5 files                                     | 12 / 5                | 12 / 5                | 0     |
| A2  | `ClosureCaptureMode` enum site                           | 1 hit @ `hew-types/src/check/types.rs:357` (variants: `Copy`, `Move`) | confirmed             | confirmed             | 0     |
| A3  | `Lambda` anchored refs (parser+ast+fmt)                  | 7 hits / 3 files; `ast.rs:{172,183,644}`              | 7                     | 7                     | 0     |
| A4  | `DropKind` enum + refs (hew-mir)                         | enum @ `hew-mir/src/model.rs:2740`, 5 variants; 143 total refs | enum confirmed; 143   | enum confirmed; 143   | 0     |
| A5  | `hew_dyn_box_(alloc\|free)` refs (whole tree)            | 71 hits / 10 files; runtime def `trait_object.rs:318/348`; symbol-decl site `hew-mir/src/runtime_symbols.rs:114-128` | 71                    | **72**                | **+1** |
| A5b | `hew_dyn_box_(alloc\|free)` lines inside `hew-mir/src/runtime_symbols.rs` (allocator-decl invariant) | 4 lines                                               | 4                     | 4                     | **0** |
| A6  | `closure` refs in hew-mir                                | 109 hits / 8 files                                    | 109 / 8               | 109 / 8               | 0     |
| A7  | `closure` refs in hew-codegen-rs                         | 96 hits / 4 files                                     | 96 / 4                | 96 / 4                | 0     |
| A8  | `HirExprKind::Scope`/`ForkBlock` visit sites             | `dump.rs:{416,465}`, `lower.rs:{2349,6907,6915,6969,10684}`, `verify.rs:236` | confirmed             | confirmed             | 0     |
| A9  | `fn is_sync` (hew-types)                                 | 1 def @ `hew-types/src/traits.rs:688`                 | confirmed             | confirmed             | 0     |
| A10 | WASM closure surface files                               | 2 files (`hew-wasm/src/lib.rs`, `hew-wasm/tests/v05_wasm_coverage.rs`) | confirmed             | confirmed             | 0     |
| A11 | **Allocator-fork sentinel** — references to either of the two `hew_closure_env_*` symbol names reserved for the would-be fork (probe pattern documented in `raw-output.txt`; must be 0) | 0 hits                                                | **0**                 | **0**                 | **0** |

## A5 delta provenance

The single new `hew_dyn_box_*` reference between `fed11980` and the tree tip is
**not** an allocator-decl fork. It is content (comment / doc / source text)
introduced by the canonical-fat-pointer commit at `6e6456ad`, referencing the
already-existing allocator symbol. The structural invariants hold:

1. **A5b (allocator-decl invariant)** — the unique allocator-symbol-decl site
   inside `hew-mir/src/runtime_symbols.rs` remains 4 lines (the two extern
   decls plus signatures). Single-source allocator entry preserved.
2. **A11 (fork sentinel)** — zero references to either of the two reserved
   `hew_closure_env_*` allocator symbol names anywhere in the tree. The
   defense-in-depth gate is
   clean.

Follow-up work that touches closure-environment heap allocation must re-run
both `A5b` and `A11` on its tip and either probe firing rejects the change.

## Critical preconditions

- **C0** — `ClosureCaptureMode` is a single enum at
  `hew-types/src/check/types.rs:357` with exactly two variants today (`Copy`,
  `Move`). Follow-up work that introduces additional capture modes must extend
  the existing enum, not introduce a sibling type.
- **C1** — `hew_dyn_box_alloc`/`free` are single-source. The share-the-allocator
  contract is mechanically verifiable by the two complementary sentinels above.
- **C2** — `HirExprKind::Scope` + `HirExprKind::ForkBlock` already exist with
  diagnostic precedent `TaskCannotEscape` at `hew-hir/src/diagnostic.rs:169`.
  Any follow-up escape analysis composes with this substrate.
- **C3** — `trait_registry.is_sync(&Ty)` already exists at
  `hew-types/src/traits.rs:688`. Follow-up `Sync`-based reasoning reuses it; no
  parallel implementation.

## Downstream consumer survey (informational)

Pre-flight inventory of where the existing `ClosureCaptureMode` is consumed —
useful for impact analysis of any future enum extension:

- `hew-types/src/check/expressions.rs:986, 4170-4173` — election sites.
- `hew-types/src/check/tests.rs:16414` — test assertion.
- `hew-hir/src/node.rs:1515` — `HirClosureCapture.mode` carrier.
- `hew-hir/src/dump.rs:616` — debug print only.
- `hew-hir/src/lower.rs:15454` — round-trip assertion.
- `hew-hir/tests/closure_capture_lower.rs:150` — test assertion.

**No `match` on `ClosureCaptureMode` exists anywhere in MIR, codegen, HIR
lowering, or HIR verify** at base SHA. Extending the enum is mechanically
non-breaking; any future fail-closed `unreachable!` arms on the wildcard will
be the **first** exhaustive matches on this enum.

## Reproducibility

Every numeric cell above is recomputable by running the corresponding probe
against the named SHA. Raw outputs at `raw-output.txt`.
