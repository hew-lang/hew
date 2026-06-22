# Fail-Closed ast-grep rules — validation report

Rules enforcing Hew's fail-closed invariants in the Rust compiler pipeline
(`hew-types` → `hew-hir` → `hew-mir` → `hew-codegen-rs`).

- **Invariant source:** CLAUDE.md §2 (Fail-Closed Codegen), §3 (Type Inference Boundary).
- **Run one rule:** `ast-grep scan --rule rules/rust/fail-closed/<file>.yml`
- **Gate:** `scripts/ast-grep-lint.sh` runs `ast-grep scan` and exits non-zero on any
  `error`-severity match.
- **Suppression convention (§2):** an intentional hit is annotated in source with a
  `// JUSTIFIED: <reason>` line comment directly above it. Both rules below honour this
  via a `not.any.[follows | inside>follows]` clause, so the comment suppresses the hit
  whether the flagged node is a statement, a tail expression, or nested inside a call.

All hit counts below are against the tree at authoring time. Test code is excluded by the
`ignores` globs (`**/tests/**`, `**/test/**`, `**/*_test.rs`, `**/*_tests.rs`, `**/tests.rs`).

---

## SHIPPED

### 1. `ok-question-in-lowering` — severity: **error**

- **Invariant:** CLAUDE.md §2 — *"the pattern `something.ok()?` in codegen is almost
  always wrong — propagate the error."* `Result::ok()?` throws away the `Err` value and
  silently turns it into an early `None`.
- **Scope:** `hew-codegen-rs/**`, `hew-mir/**`, `hew-hir/**` (tests excluded).
- **Pattern:** `$E.ok()?`
- **Current hits: 6** (all true matches — every hit is a real `.ok()?` in an
  `Option`-returning lowering/codegen function; zero spurious/structural matches. Doc
  comments mentioning `.ok()?` are correctly *not* matched because matching is AST-based.)

| # | Location | Enclosing fn (`-> Option<..>`) | Discarded `Result` | Classification |
|---|----------|------------------------------|--------------------|----------------|
| 1 | `hew-mir/src/lower.rs:21296` | `lower_spawn_actor` | `lower_spawn_actor_state(..)` | **TP — clearest.** Sibling line uses `lower_spawn_actor_init_args(..)?` (propagates); state lowering swallows its error. |
| 2 | `hew-mir/src/lower.rs:6336` | `owned_aggregate_record_field_kinds_for_key` | `classify_actor_state_fields_with_opaque_handles(..)` | **TP (borderline).** Adjacent comment claims fail-closed happens at the value-class gate; the `.ok()?` still hides *why* classification failed. Fix or `// JUSTIFIED:`. |
| 3 | `hew-codegen-rs/src/llvm.rs:29571` | `resolve_di_type` | `di_builder.create_basic_type(..)` | **TP.** DWARF DI builder error discarded → debug info silently degrades. |
| 4 | `hew-codegen-rs/src/llvm.rs:29821` | `resolve_enum_di_type` | `di_builder.create_basic_type("u32", ..)` | **TP.** Same as #3. |
| 5 | `hew-codegen-rs/src/llvm.rs:29693` | `resolve_di_type` | `u32::try_from(i)` | **TP (should be `unreachable!`).** Index→u32 conversion error discarded; the §2 "propagate or `unreachable!`" case. |
| 6 | `hew-codegen-rs/src/llvm.rs:31648` | `actor_name_from_handler_symbol` | `parse::<usize>()` | **Intentional → needs `// JUSTIFIED:`.** Pure symbol-demangler; `None` is the deliberate "this symbol doesn't match" signal, not a swallowed error. The only non-bug in the set. |

- **Representative example** (`hew-mir/src/lower.rs:21296`):
  ```rust
  let init_args = self.lower_spawn_actor_init_args(.., expr)?;     // propagates
  let state = self
      .lower_spawn_actor_state(actor_name, &layout, ..)
      .ok()?;                                                       // swallows the error
  ```
- **Adoption note (important):** at `error` severity this rule makes `ast-grep-lint.sh`
  exit non-zero on the 6 existing hits. Adopting it means triaging them first: propagate
  the error (or `unreachable!("…")`) for #1–#5, and add `// JUSTIFIED: <reason>` above #6
  (the demangler). That triage *is* the rule's purpose (§2: "fix it or add `// JUSTIFIED:`").
- **Known limitations:** matches the canonical `.ok()?` form only. It does **not** flag
  other silent-error idioms (`let _ = result;`, `.ok();`, `if let Ok(_) = ..`) — those
  were evaluated and rejected (see below). One in-scope `hew-types` analogue exists
  (`hew-types/src/check/admissibility.rs:47`) but `hew-types` is deliberately out of this
  rule's scope per the candidate spec.

### 2. `ty-var-constructed-post-inference` — severity: **error** (tripwire)

- **Invariant:** CLAUDE.md §3 — *"After type checking completes, no `Ty::Var` should
  survive unresolved into codegen."* Inference variables are built and unified in
  `hew-types` only; fabricating a `Ty::Var(..)` in a post-inference crate is a direct leak.
- **Scope:** `hew-codegen-rs/**`, `hew-mir/**`, `hew-hir/**` (tests excluded). Dependency
  direction confirms these are post-inference: `hew-mir → hew-hir → hew-types`.
- **Pattern:** `$TY::Var($$$ARGS)` with `kind: call_expression` and `constraints: TY ~ '(^|::)Ty$'`.
  - `kind: call_expression` matches **constructions** only, never the match-arm
    *detector* `Ty::Var(v) => …` (a pattern), e.g. `hew-mir/src/lower.rs:418` — which is
    the boundary working correctly and must not be flagged.
  - The `$TY` metavariable + regex catches bare **and** fully-qualified forms
    (`Ty::Var`, `crate::ty::Ty::Var`, `hew_types::ty::Ty::Var`) while rejecting unrelated
    `MyEnum::Var` / `Pattern::Variant`.
- **Current hits: 0.** This is intentional — the boundary currently holds. It is a
  **regression tripwire**: the moment anyone constructs a `Ty::Var(..)` in hir/mir/codegen
  it fails the gate. 0 hits ⇒ 0 false positives on the current tree.
- **Validation (mechanism proven on fixtures, since there is no live positive):**
  flags bare construction `Ty::Var(TypeVar::fresh())` and qualified
  `crate::ty::Ty::Var(..)`; does **not** flag the match-arm detector `Ty::Var(v) => …`,
  the unrelated `MyEnum::Var(..)`, or any construction preceded by `// JUSTIFIED:`
  (tail / statement / nested positions all covered).
- **Representative example** (the shape it guards against, not present in the tree):
  ```rust
  // hew-mir or hew-codegen-rs — would fail the gate:
  fn ty_of(..) -> Ty { Ty::Var(TypeVar::fresh()) }   // leaks an inference var past the checker
  ```
- **Known limitations:** detects *construction* of a fresh `Ty::Var`, not a `Ty::Var`
  that flows in through a variable/field already built upstream — that class is caught by
  the runtime post-inference validation pass in `check_program`, not statically. Scoped to
  the bare/qualified `Ty::Var(..)` call form; a `Ty::Var` built via an indirection helper
  (none exist today) would not match.

---

## REJECTED (recorded so the decision is auditable)

### R1. `.unwrap_or_default()` in codegen/mir (candidate #2) — **rejected: all false positives**
- 22 code hits across `hew-codegen-rs` + `hew-mir`. Every one is legitimate:
  - **13** in `hew-mir/src/dump.rs` — debug/`Display` dump formatting (not an output path).
  - `hew-mir/src/dataflow.rs:857` — absent dataflow entry state defaults to the empty
    (bottom) lattice value; standard and correct.
  - `hew-mir/src/lower.rs` (1423, 1494, 1499, 3841, 3891, 4741, 4747) and
    `hew-codegen-rs/src/llvm.rs:38918` — `Option<collection>`/`Option<map>` → empty
    default (no init params ⇒ empty vec, absent map key ⇒ empty deps, `None` suspend
    kind ⇒ empty map). No bug is masked.
- There is no structural sub-pattern that isolates a genuine "default papered over a
  failed computation" case, because **there are zero such cases in the tree**. A blanket
  rule would be 22/22 FP — pure noise. Per "a noisy rule is worse than no rule," dropped.

### R2. Silent error/`None` discard (candidate #3) — **rejected: imprecise / no signal**
- `.ok();` (discard-as-statement): **0 hits** in the pipeline — nothing to catch.
- `let _ = <expr>;`: **188 hits**, overwhelmingly legitimate intentional discards. ast-grep
  has no type information, so it cannot tell which RHS is a `Result`/`#[must_use]` (the
  only interesting case, already covered by rustc's `unused_must_use`). No precise variant
  exists; dropped.

### R3. `Ty::Error` in expected-type / pre-seed position (candidate #4b) — **rejected: all FP today**
- `$E.unwrap_or(Ty::Error)` / `vec![Ty::Error; n]`: 4 hits, all in
  `hew-types/src/check/patterns.rs` (1022, 1029, 1464, 1471). All are **error-recovery**
  fallbacks: when `lookup_variant_types` has *already failed*, payload field types are
  filled with the `Ty::Error` poison value to prevent cascading diagnostics — the
  documented-correct use, the opposite of "pre-seeding an expected type." Whether a given
  site is a forbidden "pre-seed/expected-type" position vs. legitimate recovery is
  semantic and not structurally distinguishable; current instances are 4/4 legitimate.
  Dropped (would be all-FP).

### R4. `Ty::Var` construction inside `hew-types` (candidate #4a, broad form) — **rejected: FP-prone**
- `$E.unwrap_or(Ty::Var(TypeVar::fresh()))`: 13 hits, all in `hew-types/src/check/`
  (`expressions.rs`, `methods.rs`, `calls.rs`, `admissibility.rs`) — seeding *fresh*
  inference variables *during* checking, which is exactly what the checker/unifier must
  do. §3 forbids a `Ty::Var` *surviving into codegen*, not being *built during inference*.
  Scoping a construction ban inside `hew-types` (309 legitimate `Ty::Var` uses) is
  hopelessly FP-prone. The precise, shippable form of §3 is the post-inference tripwire
  shipped as rule #2 above.

---

## Summary

| Rule | Severity | Hits | Verdict |
|------|----------|------|---------|
| `ok-question-in-lowering` | error | 6 | ship — 5 real fail-closed concerns + 1 needs `// JUSTIFIED:` |
| `ty-var-constructed-post-inference` | error | 0 | ship — §3 regression tripwire, 0 FP |
| unwrap_or_default (codegen/mir) | — | 22 | reject — all legitimate |
| silent discard (`let _ =` / `.ok();`) | — | 188 / 0 | reject — no type info / no hits |
| `unwrap_or(Ty::Error)` pre-seed | — | 4 | reject — all error-recovery |
| `Ty::Var` build inside `hew-types` | — | 13 | reject — legitimate inference seeding |
