# panics-nyi — ast-grep rule report

Domain: panic / not-yet-implemented (NYI) hygiene in the Hew Rust compiler.
Invariant sources: CLAUDE.md §2 (Fail-Closed Codegen — prefer `unreachable!("desc")`,
never silently swallow), and the repo's "no new NYI" gate.

All rules are `language: rust`, exclude test code via
`ignores: ['**/tests/**','**/*test*.rs','**/benches/**']`, and additionally exclude
inline `#[cfg(test)]` / `#[cfg(all(test, …))]` modules that the globs miss, via a
relational `not: inside mod_item follows attribute_item(/cfg\(.*\btest\b/)` guard.

Validated with ast-grep 0.44.0:
`ast-grep scan --rule rules/rust/panics-nyi/<file>.yml --json=compact`.

---

## SHIPPED

### 1. `unreachable-without-message` — severity: warning
**Rationale.** CLAUDE.md §2 prefers `unreachable!("description")` so a violated
invariant is self-documenting in the panic message / backtrace. This rule flags the
bare no-argument form only; the documented `unreachable!("…")` form is intentionally
**not** matched (verified: `pattern: unreachable!()` matches an empty `token_tree`,
not one containing a string).

**Current hits: 8 — all true positives, 0 false positives.**

| # | Location | Context |
|---|----------|---------|
| 1 | `hew-parser/src/parser.rs:6479` | `_ => unreachable!(),` in `parse_expr_bp` |
| 2 | `hew-parser/src/parser.rs:8276` | `_ => unreachable!(),` |
| 3 | `hew-runtime/src/mailbox.rs:1512` | `HewOverflowPolicy::Coalesce => unreachable!(),` (send path) |
| 4 | `hew-lexer/src/lib.rs:610` | `_ => unreachable!(),` in a `Display` impl |
| 5 | `hew-sandbox-wasm/src/emit.rs:1784` | `_ => unreachable!(),` (Some/Ok/None/Err disc.) |
| 6 | `hew-sandbox-wasm/src/emit.rs:2368` | `_ => unreachable!(),` |
| 7 | `hew-sandbox-wasm/src/emit.rs:2703` | `_ => unreachable!(),` (regex method map) |
| 8 | `hew-types/src/check/items.rs:368` | `SupervisorStrategy::SimpleOneForOne => unreachable!(),` |

**Representative example:** `hew-runtime/src/mailbox.rs:1512` — a match arm assumed
dead because `Coalesce` is meant to be handled earlier; a `unreachable!("coalesce
handled above")` would document that invariant.

**FP triage (excluded correctly, verified by inspection):**
- `hew-runtime/src/util.rs:147`, `hew-runtime/src/lifetime/live_actors.rs:389-390`,
  `hew-mir/src/lower.rs:{32697,32821,32845,32965}`, `hew-codegen-rs/src/llvm.rs:43325`
  — all inside `#[cfg(test)]` / `#[cfg(all(test, …))]` modules; excluded by the
  relational `cfg(test)` guard (globs alone would miss these inline modules).
- `hew-hir/src/lower.rs:23721`, `hew-hir/src/node.rs` — `unreachable!()` appears only
  inside a `///` doc comment, never matched by the AST macro pattern.
- `mailbox.rs` has a `#[cfg(test)]` item at line 83 but the line-1512 hit is in the
  file's production module, so it is (correctly) **kept** — confirms the guard uses
  immediate-neighbor `follows` and does not over-exclude on an earlier sibling.

**Limitations.** A bare `unreachable!()` inside a *top-level* `#[test]` / `#[cfg(test)]`
function that is **not** wrapped in a `mod` and lives in a non-`*test*`-named file
would not be excluded. No such case exists in the current tree. `#[cfg(not(test))]`
modules (production-only) are conservatively skipped because their attribute text
contains `test`; this trades a rare false negative for zero false positives.

---

### 2. `no-todo-macro` — severity: warning  (preventive guard)
**Rationale.** The repo gates against new NYI markers. `todo!()` panics if reached —
it is unfinished work, not a fail-closed boundary. Matches both `todo!()` and
`todo!("msg")` (pattern `todo!($$$)`); does **not** match look-alikes such as
`my_todo!()` / `todoish!()` (verified on scratch).

**Current hits: 0.** The single raw-grep occurrence (`hew-hir/src/node.rs:2181`) is a
`///` doc comment referencing `todo!("…")`, correctly not matched by the AST pattern.
Ships as a regression guard so new `todo!` cannot land in production code unnoticed.

---

### 3. `no-unimplemented-macro` — severity: warning  (preventive guard)
**Rationale.** Same NYI gate as above for `unimplemented!()`. Matches both
`unimplemented!()` and `unimplemented!("msg")`; excludes look-alikes.

**Current hits: 0** in non-test code. Ships as a regression guard.

---

### 4. `expect-empty-message` — severity: warning  (preventive guard)
**Rationale.** `.expect("")` panics with no diagnostic context, defeating the point of
`expect` over `unwrap` (CLAUDE.md values diagnostic quality). Matches empty **and**
whitespace-only string-literal messages via `constraints: MSG: /^"\s*"$/`; does not
match non-empty messages or non-literal arguments like `.expect(SOME_CONST)`
(verified on scratch: matched `.expect("")` and `.expect("   ")`, skipped
`.expect("good reason")`, `.expect(SOME_CONST)`, `.unwrap()`).

**Current hits: 0.** Ships as a regression guard.

---

## REJECTED

### R1. `panic!(…)` in runtime / codegen — REJECTED (noisy; intentional guards)
Measured non-test `panic!($$$)` in `hew-runtime/**` + `hew-codegen-rs/**` (test mods
excluded): **39 hits**. (The raw `hew-codegen-rs/src/llvm.rs:76` collapses to ~0
non-test — they are inside a large `#[cfg(test)] mod tests`.) Sampling shows the
surviving 39 are overwhelmingly **intentional** fail-closed / invariant guards, not
"should-return-an-error" smells:
- `hew-runtime/src/hashmap.rs` (19) — C-ABI precondition checks on `HewLayoutHashMap`
  (`key_layout is null`, `align is not a power of two`, `ownership_kind=Bytes is not
  valid`, …). Panicking on malformed FFI input *is* the fail-closed behaviour.
- `hew-runtime/src/trait_object.rs` (3) — vtable dispatch out-of-bounds (hard
  invariant violation).
- `hew-runtime/src/task_scope.rs:3112` — literally `panic!("cancel_cleanup intentional
  panic")`; `actor.rs:4659` — `panic!("hew_panic: actor panic")` (the panic impl).
- `hew-codegen-rs/src/arith.rs:55` — ICE guard for an impossible LLVM intrinsic case.

No structural signal separates the rare genuine smell from the many deliberate guards,
so any rule here flags ~39 intentional sites → noise. Per the task's own caveat
("validate it isn't noisy"), rejected. If pursued later, the right move is per-site
`// JUSTIFIED:` annotations plus an `error`-severity rule that skips annotated lines,
not a blanket warning.

### R2. `.unwrap()` in codegen/runtime hot paths — REJECTED (no high-signal scope)
Measured non-test `$X.unwrap()` in `hew-codegen-rs/**` + `hew-mir/**`: **11 hits**
(`coro.rs` 7, `llvm.rs` 4). (The often-cited ~34 are mostly in `state_clone.rs` /
`abi_class.rs` `#[cfg(test)]` modules.) None of the 11 is structurally distinguishable
from an invariant-safe unwrap (e.g. lookup immediately after insert), so a rule would
emit warnings the team must individually justify. The task pre-flags this as "likely
too noisy … only ship if you can scope it to be high-signal; otherwise REJECT" — no
tight high-signal sub-module exists. Note: CLAUDE.md §2 targets `.ok()?`,
`.unwrap_or_default()`, and silent `None` specifically (owned by the `fail-closed`
rule dir), not bare `.unwrap()`.

---

## Summary
- Shipped: `unreachable-without-message` (8 hits, all TP), `no-todo-macro` (0),
  `no-unimplemented-macro` (0), `expect-empty-message` (0). The three zero-hit rules
  are precise regression guards enforcing the "no new NYI" / diagnostic-quality
  policy; the unreachable rule has live, fully-triaged true positives.
- Rejected: blanket `panic!` (R1) and blanket `.unwrap()` (R2) — both validated as
  noisy with no high-signal scoping available.
