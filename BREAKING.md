# Hew v0.5 Compiler-Foundation Cutover — Freeze Notice

## What is happening

Hew is entering the v0.5 compiler-foundation cutover. The goal is a clean,
typed intermediate-representation stack — Resolved HIR → THIR → Raw MIR —
that replaces the current MLIRGen walker and the wrapper-shape registry that
grew up around it.

v0.5 prioritises landing that new foundation correctly. It does **not**
prioritise keeping the v0.4 internal surfaces stable. Any code that drives
the MLIRGen walker, the wrapper-shape registry, or the `expr_types`-keyed
ownership side tables should be treated as transitional until the cutover
steps that replace those surfaces have landed.

The compiler-foundation reference document is
[`docs/internal/v05-ir-ladder.md`](docs/internal/v05-ir-ladder.md).

---

## Main-branch freeze rules (effective immediately)

The following categories of change are **frozen on `main`** until the
corresponding cutover step lands:

1. **No new structural MLIRGen walker patches** unless the change qualifies
   as a bridge fix under the criteria below.

2. **No new wrapper-shape registry expansions.** Adding entries to the
   wrapper-shape registry extends a surface that the cutover deletes. Changes
   in this category must wait for the relevant cutover step.

3. **No new `expr_types`-keyed ownership side tables.** New side tables of
   this form are rejected on `main` for the same reason.

---

## Bridge-fix admission criteria

An in-flight `main` change may proceed if and only if it satisfies **all
three** of the following:

1. It fixes a reproducible memory-safety or fail-closed defect (e.g.,
   null-after-move, field-alias fail-closed, cleanup-all-exits) that
   directly affects the ability to develop or test on `main`.

2. It touches at most one walker call site and does **not** extend any
   registry's wrapper-shape coverage.

3. The same defect is captured as a reproduction fixture under
   `tests/hew/` or `hew-cli/tests/` so the cutover branch can consume it
   as both a target and a regression check.

Changes that do not satisfy all three criteria **pause** until the relevant
cutover step on the cutover branch picks them up.

---

## Public API and language surface

The freeze rules above apply to **compiler internals only**. Public language
semantics, standard-library APIs, and wire-protocol surfaces follow their
normal deprecation and stability policy. See `CHANGELOG.md` for user-visible
changes.

---

## Removed in v0.5

### `std::collections::hashset` module

- **Removed:** the Hew-source module `std::collections::hashset` and its
  opaque `HashSet` handle API (`hashset.new()`, `insert_int`,
  `insert_string`, `contains_int`, `contains_string`, `remove_int`,
  `remove_string`, `clear`, and `free`).
- **Replacement:** use built-in `HashSet<T>` directly, for example
  `HashSet::<i64>::new()` or `HashSet::<String>::new()`, with `.insert()`,
  `.contains()`, `.remove()`, `.len()`, and `.is_empty()`.
- **Behaviour change:** built-in `HashSet<T>` releases through RAII; callers
  do not call `free()`.

### `HewScope` runtime substrate + `hew_scope_*` C ABI

- **Removed:** the `hew-runtime::scope` module (`HewScope`, `hew_scope_new`,
  `hew_scope_create`, `hew_scope_destroy`, `hew_scope_free`, `hew_scope_spawn`,
  `hew_scope_cancel`, `hew_scope_is_cancelled`, `hew_scope_wait_all`).
- **Removed:** `hew-runtime-testkit::TestScope` (RAII wrapper around the
  legacy ABI).
- **Replacement:** `HewTaskScope` (`hew_task_scope_*`) is the canonical
  structured-concurrency substrate; `scope { … }` source syntax is preserved
  unchanged and now lowers exclusively to `hew_task_scope_*`.
- **Behaviour change (looser):** the legacy `MAX_ACTORS=64` ceiling
  (`HEW_SCOPE_MAX_ACTORS`) is gone; `HewTaskScope` is unbounded.
- **Rationale:** A244 substrate-first / A250 one-canonical-name.
  No compat shim per the pre-1.0 policy.
