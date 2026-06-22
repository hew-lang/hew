# Rust hygiene ast-grep rules — validation report

Domain: `rules/rust/hygiene/` — broad Rust hygiene guards for the Hew compiler/runtime.
Each section below is appended by the agent that authored the rule; validated against the
tree at authoring time with `ast-grep 0.44.0`
(`ast-grep scan --rule rules/rust/hygiene/<file>.yml --json=compact`).

---

## `transmute-audit` — severity: `hint`

- **Invariant / intent:** `transmute` is the most dangerous `unsafe` primitive — it reinterprets
  bits with zero size/alignment/lifetime checking. This rule is a deliberately complete *review
  inventory* (not a bug signal), modelled on the existing `explicit-leak-review` rule: a small,
  durable list of sites a reviewer must re-verify whenever a source/target type changes.
- **Pattern:** an `any:` over the path variants — fully-qualified (`std::mem::` / `core::mem::`),
  module-qualified (`mem::`), and imported bare (`transmute`) — each in **both** the plain
  `transmute($$$)` and turbofish `transmute::<$$$T>($$$)` spellings.
- **Scope:** workspace `**/src/**`; tests/benches/examples excluded via `ignores` globs **and** an
  inline `#[cfg(test)]`-module guard (`not: inside mod_item follows attribute_item /cfg\(.*\btest\b/`).

### Final hit count: **10** (all true positives, 0 false positives)

Reconciliation: a textual `rg transmute` finds **13** occurrences; **3 are comments/doc-comments**
(`reply_channel.rs:243`, `profiler/actor_registry.rs:189`, `cont.rs:221`) which ast-grep correctly
does **not** match because it is AST-based. That leaves **10 real call sites**, all matched:

| file:line | form | transmute | triage |
|---|---|---|---|
| `actor.rs:3581` | plain | `void* dispatch → Option<HewDispatchFn>` | **TP** — FFI dispatch fn-ptr reconstruction (`unsafe`, SAFETY-commented). |
| `actor.rs:3644` | plain | `void* dispatch → Option<HewDispatchFn>` | **TP** — same, handler-name registration path. |
| `reply_channel.rs:329` | **turbofish** | `*mut c_void → HewReplyDropFn` | **TP** — drop-fn reconstruction; ties into §1 Drop Safety (the transmuted ptr *is* the drop path). |
| `reply_channel.rs:787` | **turbofish** | `*mut c_void → HewReplyDropFn` | **TP** — same, second free leg. |
| `stream.rs:1942` | plain | `fp → StringMapFn` | **TP** — stream-transform fn-ptr. |
| `stream.rs:1973` | plain | `fp → StringFilterFn` | **TP** — stream-transform fn-ptr. |
| `stream.rs:2006` | plain | `fp → BytesMapFn` | **TP** — stream-transform fn-ptr. |
| `stream.rs:2037` | plain | `fp → BytesFilterFn` | **TP** — stream-transform fn-ptr. |
| `task_scope.rs:1350` | plain | `fn_raw → TaskFn` | **TP** — spawned-task fn-ptr across the C ABI. |
| `task_scope.rs:1453` | plain | `fn_raw → ContextTaskFn` | **TP** — spawned context-task fn-ptr. |

- **Precision:** 10/10 are genuine `transmute` calls (every one inside an `unsafe` block, all
  raw/`void*`→typed **function-pointer** reinterpretations — the highest-risk transmute category).
  0 false positives: no identifier named `transmute`, no `foo.transmute(..)` method call, and
  `transmute_copy` is **intentionally not matched** (distinct, non-aliasing op; 0 in tree anyway).
  None sit inside a test module. The `core::`/`mem::`/bare path variants match 0 today — kept as
  preventive coverage so a future import-style transmute is still caught.
- **Representative example:** `reply_channel.rs:329`
  `let drop_fn = std::mem::transmute::<*mut c_void, HewReplyDropFn>(drop_raw); drop_fn(value);`
  — the turbofish spelling, which a naive `std::mem::transmute($$$)` pattern **misses** (turbofish
  parses as a `generic_function` node, verified via `--debug-query=ast`); the dedicated
  `::<$$$T>($$$)` variant is what catches it. It is also the most safety-critical: the transmuted
  pointer is the value's destructor.
- **Severity rationale:** `hint` (lowest) — this is an audit list of currently-intentional sites,
  not a defect. Mirrors `explicit-leak-review`.
- **Limitations:** ast-grep is single-file/syntactic — it cannot prove the source/target layouts
  actually match; it only enumerates the sites to re-verify. Does not honor a `// JUSTIFIED:`
  comment (tree-sitter treats comments as extras).

---

## `dbg-macro` — severity: `warning`

- **Invariant / intent:** `dbg!(..)` writes to stderr and is meant only for transient local
  debugging — it must never ship. Currently **0** uses in the tree, so this is a **preventive
  regression guard** (a `0`-hit rule, like `no-todo-macro`).
- **Pattern:** `dbg!($$$)` (matches every arity/spelling). Tests/benches/examples excluded via
  `ignores` globs + the inline `#[cfg(test)]`-module guard — consistent with the sibling macro
  guards in `panics-nyi/`; a throwaway `dbg!` while debugging a test is lower-risk and is
  self-evident in test output.

### Final hit count: **0** (preventive — confirmed 0 across the whole tree, tests included)

- **Triage:** N/A (no hits). `rg dbg! --type rust` repo-wide = 0, so the guard starts clean.
- **Scratch-test proof it matches** (snippet written to a temp `.rs`, scanned, then deleted):

  ```rust
  fn f() {
      dbg!();                                   // match 1
      let x = dbg!(compute());                  // match 2
      dbg!(a, b, c);                            // match 3
      let y = dbg!( nested(inner(1)), other, ); // match 4 (multiline)
      let z = debug!(notme);                    // NOT matched (different macro)
      println!("dbg! in a string, not a call"); // NOT matched (string literal)
      foo.dbg(x);                               // NOT matched (method call)
  }
  ```

  `ast-grep run --lang rust -p 'dbg!($$$)'` → **4 matches** (the four real `dbg!` invocations,
  including the empty and multiline forms); the three negative controls (`debug!`, the string
  literal, and the `.dbg(..)` method call) are correctly **not** matched.
- **Severity rationale:** `warning` (non-gating but visible) — `dbg!` is unambiguously
  ship-blocking debug residue, stronger than a `hint`; matches `no-todo-macro`'s severity.
- **Limitations:** a `dbg!` inside a top-level `#[test]` fn that is *not* wrapped in a `mod`, in a
  non-`*test*`-named file, would not be excluded — no such case exists today.

---

## `unsafe-without-safety` — severity: `warning`

- **Invariant / intent:** CLAUDE.md §1 — every `unsafe { … }` **block** documents why its
  invariants hold with a `// SAFETY:` comment. This is already the overwhelming convention
  (98.55% of non-test blocks carry one); the rule surfaces the ~1.5% residual gap so it can be
  backfilled. It is the gap, not the convention, that fires — 54 hits, **not** the ~6300 justified
  blocks.
- **Matcher:** `kind: unsafe_block` with a `not.any[…]` of the exclusion clauses below. Flags the
  **block expression only** — `unsafe fn` / `unsafe impl` / `unsafe trait` are different node kinds
  (`function_item`/`impl_item`/`trait_item`) and are never matched.
- **Scope:** `hew-*/src/**`; tests/benches excluded via `ignores` globs **and** the inline
  `#[cfg(test)]`-module guard (`not: inside mod_item follows attribute_item /cfg\(.*\btest\b/`,
  with a contiguous-attribute `stopBy` so intervening `#[allow]`/`#[expect]` don't break the walk).

### Final hit count: **54** (high precision — 52 clean true positives + 2 borderline)

**Precision (the point of the rule):**

| Population (`hew-*/src`, ast-grep `unsafe_block`) | Count |
|---|---:|
| In-scope total (tests excluded by globs) | **8024** |
| …inside inline `#[cfg(test)]` modules (also excluded) | 4311 |
| **Non-test denominator** | **3713** |
| Carry a `// SAFETY:` (justified, excluded) | 3659 — **98.55%** |
| **Flagged gap (rule hits)** | **54 — 1.45%** |

A naive `not: follows line_comment(/SAFETY/) stopBy: neighbor` yields **1328** hits; the
SAFETY-adjacency logic below is what reduces that to the true 54-block gap. Per crate:
`hew-codegen-rs` 42 · `hew-runtime` 11 · `hew-cli` 1. Per file: `llvm.rs` 30, `coro.rs` 5,
`thunks.rs` 5, `runtime_abi.rs` 2; `actor.rs` 5, `registry.rs` 4, `arena.rs` 1, `env.rs` 1;
`process.rs` 1.

**SAFETY-placement variants handled (each was an FP class found and fixed):**

1. **Comment immediately above** the block *or its enclosing statement* (`// SAFETY: …` then
   `let x = unsafe { … };`). Two clauses — a direct `follows` for the tail-expression position,
   and an `inside: { stopBy: end, follows: … }` for an unsafe nested in a `let`/call/`return`/closure.
2. **Multi-line SAFETY.** tree-sitter makes each `//` line its own `line_comment`, so a 2–20 line
   justification is a *run* of siblings; the `stopBy` walks back over the contiguous comment run and
   stops at the first real node (so the SAFETY on the run's first line is still seen). `neighbor`
   only saw the last continuation line — the original 1328-hit bug; `end` over-matched and hid real
   gaps. Live proof: `process.rs:664` (`// SAFETY:` + `// NTSTATUS:` continuation, then
   `let status = unsafe { NtResumeProcess(handle) }`) is correctly **not** flagged.
3. **Attribute/operator separation.** The same continue-set skips `attribute_item` (so
   `#[cfg(…)] unsafe { … }`, e.g. `actor.rs:1360`, stays justified) and the `&&`/`||` tokens (via
   `regex: '^(&&|\|\|)$'`, since anonymous tokens can't be named as a `kind`) so a
   `cond && unsafe { … }` guard keeps its SAFETY.
4. **In-block justification.** A `// SAFETY`/`/* SAFETY … */` placed *inside* the block — two
   `has: { …, stopBy: end }` clauses — legitimately excludes exactly 3 blocks (`coro.rs:327/373/419`,
   SAFETY on the `IntValue::new(raw)` tail). Removing them yields 57 hits.
5. **Inline `#[cfg(test)]` modules** the globs miss (4311 blocks, >half the in-scope total).

**Representative true positives** (verified — genuinely no SAFETY):

| file:line | context |
|---|---|
| `hew-codegen-rs/src/llvm.rs:7579` | `unsafe { ret_null_bb.delete().expect("delete empty bb") };` |
| `hew-codegen-rs/src/llvm.rs:11173` | `let field_ptr = unsafe { … build_struct_gep … };` |
| `hew-codegen-rs/src/coro.rs:218` | `let save_token = unsafe { … };` (LLVM intrinsic emit) |
| `hew-codegen-rs/src/thunks.rs:1274` | actor-thunk slot pointer, no SAFETY |
| `hew-codegen-rs/src/runtime_abi.rs:2524` | ABI shim builder call, no SAFETY |
| `hew-runtime/src/registry.rs:334` | `unsafe { cstr_key(name, len) }` (FFI, only a `///` contract) |
| `hew-runtime/src/actor.rs:666` | wasm lock-release extern call, no SAFETY |
| `hew-cli/src/process.rs:665` | `let code = unsafe { GetLastError() };` (sibling `NtResumeProcess` block *is* justified) |

- **Precision / FP triage:** of 3713 non-test blocks, 3659 carry SAFETY and are excluded —
  spot-checked across all five placement variants; none leak into the hit set. A SAFETY belonging to
  an *earlier* statement does **not** suppress a later gap (the `stopBy` halts at the first real code
  node), so `actor.rs:1279` / `process.rs:665` — a second undocumented block after a justified one —
  are correctly flagged. **Zero false negatives:** every excluded block was cross-checked against a
  text heuristic; the only no-SAFETY-within-12-lines blocks were the 3 `coro.rs` in-block cases and 7
  runtime blocks with long contiguous multi-line SAFETY — all correctly excluded. Of the 54 hits, 50
  have no SAFETY at all within 8 lines above (unambiguous TP); 4 have a nearby SAFETY, 2 of which
  (`process.rs:665`, `actor.rs:1279`) are still true per-block gaps.
- **Representative example:** `process.rs:664` vs `:665` — the `NtResumeProcess` block carries a
  two-line `// SAFETY:`/`// NTSTATUS:` comment and is **not** flagged, while the immediately
  following `let code = unsafe { GetLastError() };` (no SAFETY of its own) **is** — demonstrating both
  the multi-line walk and the per-block discrimination in one function.
- **Severity rationale:** `warning` — backfill debt, not a build gate. High precision (1.45% gap)
  means no downgrade to `hint` is warranted.
- **Limitations (residual, not "fixed" because the fix reintroduces FNs):**
  - **Setup-statement separation (2 borderline hits).** `env.rs:317` (`// SAFETY` → `let mut len = 0;`
    → `unsafe { … }`) and `arena.rs:319` (`// SAFETY` → `let layout = …expect();` →
    `unsafe { dealloc }`): the SAFETY describes the block but sits above a *setup* statement. Crossing
    a statement to find it would let an earlier unsafe's SAFETY leak forward onto a later gap, so the
    rule requires adjacency; the clean fix is to move the SAFETY next to the block.
  - **`cfg(not(test))` conservative skip.** The `cfg\(.*\btest\b` guard also matches `cfg(not(test))`
    / feature-`test`; verified no `mod` in the tree is gated that way (those sit on functions) — same
    convention as `panics-nyi`. Trades a rare theoretical FN for zero test-code FPs.
  - **In-block `has stopBy: end` breadth** (bounded to the 3 `coro.rs` blocks) and the enclosing-
    statement **`inside: stopBy: end`** could in principle hide a deeper nested undocumented unsafe;
    no such occurrence exists in the tree (all candidates enumerated and explained).

