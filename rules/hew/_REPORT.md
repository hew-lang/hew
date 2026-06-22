# Hew-language ast-grep rules — validation report

Idiom / redundant-construct lints for `.hew` source. These are the unique value
ast-grep adds to this repo: semgrep cannot parse Hew, but the tree-sitter-hew
grammar (compiled to `.ast-grep/hew-lang.so`, wired via `sgconfig.yml`) can.

- **Run one rule:** `ast-grep scan --rule rules/hew/<file>.yml`
- **Gate:** `scripts/ast-grep-lint.sh` runs `ast-grep scan` and exits non-zero only on
  `error`-severity matches. Every rule here is `warning`/`hint`, so none gate CI — they are
  advisory idiom lints.
- **Hit counts** below are against `std/` + `examples/` at authoring time (the corpus the
  spec named: 17 std files, 508 example files).

### Authoring notes (Hew + ast-grep mechanics)

- A **bare expression is invalid at Hew top level**, so expression/statement patterns must
  be wrapped with `pattern: { context: 'fn _p() { … }', selector: <node_kind> }`. Item
  patterns (`fn`, `impl`, …) match directly.
- ast-grep **cannot unify one metavariable across a pattern-binder position and an
  expression position** (they are different node kinds). This is why `match o { Some(v) =>
  v, None => d }` (the `unwrap_or` shape) cannot be matched with the binder tied to the
  returned value — see REJECTED R1.
- Method-call syntax is `receiver.method(args)`; the suggested rewrites use it directly.

---

## SHIPPED

### 1. `match-bool-predicate` — severity: **warning** — hits: **2**

- **Flags:** a `match` that maps each variant of `Option`/`Result` to a boolean literal —
  i.e. an open-coded copy of a built-in predicate:
  | match shape | idiom |
  |---|---|
  | `match o { Some(_) => true,  None => false }`  | `o.is_some()` |
  | `match o { Some(_) => false, None => true  }`  | `o.is_none()` |
  | `match r { Ok(_)   => true,  Err(_) => false }`| `r.is_ok()` |
  | `match r { Ok(_)   => false, Err(_) => true  }`| `r.is_err()` |
- **Precision:** literal `true`/`false` arms and literal `_` wildcards mean the only
  metavariable is the scrutinee → structurally exact, no cross-category metavar.
- **Scope:** `ignores: **/option.hew, **/result.hew` — those two std files *define* these
  predicates; linting them would tell the stdlib to call itself.
- **No `fix:` (deliberate).** Every site that currently matches is itself a *predicate
  definition*, where rewriting to `x.is_ok()` would be self-recursive. The rule's value is
  as a guard against the same redundancy appearing in ordinary application code.
- **Triage — both hits TP:**
  | # | Location | Shape | Classification |
  |---|----------|-------|----------------|
  | 1 | `examples/ux/14_error_test.hew:12` | `fn is_ok`  → `Ok(_)=>true,  Err(_)=>false` | **TP.** Teaching example re-implements `Result::is_ok`; could call `r.is_ok()`. Fix withheld (recursion). |
  | 2 | `examples/ux/14_error_test.hew:19` | `fn is_err` → `Ok(_)=>false, Err(_)=>true`  | **TP.** Same, for `is_err`. |
- **Representative** (`examples/ux/14_error_test.hew:11`):
  ```hew
  fn is_ok(r: Result<i64, i64>) -> bool {
      match r { Ok(_) => true, Err(_) => false }   // ⇒ r.is_ok()
  }
  ```
- **Limitations:** matches the canonical arm order (`Some`/`Ok` first) and the `_` wildcard
  only; a reversed-order or `Some(x) => true` (unused binder) variant would be missed. All
  16 hits inside `std/{option,result}.hew` are correctly excluded.

### 2. `redundant-clone-literal` — severity: **warning** — hits: **0** (guard, fix-carrying)

- **Flags:** `clone <literal>` where the literal is the *direct* operand — `clone 5`,
  `clone "x"`, `clone true`, `clone 3.14`, `clone 'c'`. A literal is already a fresh owned
  value (and scalar literals are Copy), so the `clone` is pure overhead.
- **Precision:** the nested `has` requires the literal to be the immediate operand, so
  `clone x` (identifier), `clone foo(5)` (call) and `clone (a + b)` are **not** flagged.
  Interpolated strings parse as `interpolated_string` (not a literal) and are skipped.
- **`fix: '$L'` — safe.** Replacing `clone <literal>` with `<literal>` is value-identical.
- **Current hits: 0** — no such code exists in the corpus; this is a regression guard.
  **Mechanism proven** on a crafted fixture: fires on all five literal kinds
  (`5`,`"hi"`,`true`,`3.14`,`'z'`) and rejects `clone x` / `clone name` / `clone foo(5)` /
  `clone (5 + 3)` (5 hits, 0 spurious).
- **Representative** (the shape it guards against):
  ```hew
  let a = clone 5;     // ⇒ let a = 5;
  ```
- **Limitations:** intentionally narrower than the memory note "`clone x` on a Copy/scalar
  is redundant" — deciding whether a *variable* is Copy needs type information ast-grep
  doesn't have. Only the always-safe **literal** form is shipped (see REJECTED R5).

### 3. `len-zero-is-empty` — severity: **hint** — hits: **50**

- **Flags:** `x.len() == 0`, the long-hand emptiness test; the idiom is `x.is_empty()`.
- **Precision:** the pattern requires a `.len()` **call**, so a pre-computed length variable
  (`slen == 0`) is correctly *not* flagged. ast-grep also skips a doc-comment occurrence
  (`/// empty.len() == 0` in `unicode.hew`) that a text grep would falsely catch — AST
  matching gives 50 vs grep's 51.
- **No `fix:` (deliberate).** `is_empty` availability is type-dependent and ast-grep is
  type-blind, so an auto-rewrite could produce non-compiling code for a custom type that
  has `len` but not `is_empty`.
- **Triage — 50/50 TP, 0 FP.** Every receiver is a `string` or `Vec` (or `bytes`/`Deque`/
  regex match type), all of which provide `is_empty`. Notably `std/vec.hew` itself uses
  `v.len() == 0` **9 times**, and the `examples/datastruct/*` "stack/heap/queue" receivers
  are all `Vec<i64>` under the hood (e.g. `stack.hew`'s `s: Vec<i64>`). Distribution: std 26
  (vec ×9, csv ×6, regex ×3, scanner ×2, template ×2, dns/fs/cron/datetime ×1), examples 24
  (datastruct ×19, text_stats ×2, curl/string_rotation/selfhost ×1).
- **Representative** (`std/vec.hew:137`):
  ```hew
  if v.len() == 0 { return None; }     // ⇒ if v.is_empty() { … }
  ```
- **Limitations:** only the `== 0` orientation (→ `is_empty()`); the non-empty forms
  `.len() > 0` / `.len() != 0` (→ `!x.is_empty()`) are left out to keep the message
  unambiguous and would roughly double the hit count if added.

### 4. `empty-string-is-empty` — severity: **hint** — hits: **3** (fix-carrying)

- **Flags:** `s == ""`; the idiom is `s.is_empty()`. Comparing to the `""` literal
  guarantees the operand is a `string`, and `string` always provides `is_empty`, so the
  rewrite is type-safe.
- **`fix: '$X.is_empty()'` — safe.** The receiver is constrained to a plain identifier or
  field access (covers every corpus hit); postfix `.is_empty()` binds tighter than `==`, so
  no parentheses are needed. A complex left operand (`a + b == ""`) is intentionally not
  matched, keeping the fix unambiguous.
- **Triage — 3/3 TP:**
  | # | Location | Before → after |
  |---|----------|----------------|
  | 1 | `std/net/dns/dns.hew:82`  | `if ip == ""` → `if ip.is_empty()` |
  | 2 | `std/path.hew:91`         | `if a == ""` → `if a.is_empty()` |
  | 3 | `std/process.hew:125`     | `if message == ""` → `if message.is_empty()` |
- **Limitations:** only the `$X == ""` orientation (`"" == $X` does not occur in the
  corpus); only identifier/field receivers (so the fix never needs parens).

---

## REJECTED (recorded so the decision is auditable)

### R1. `match → unwrap_or` (spec candidate #1, second bullet) — **rejected: not precisely matchable**
- Target shape `match o { Some(v) => v, None => d }` ⇒ `o.unwrap_or(d)` requires tying the
  returned value to the **binder** `v`. ast-grep cannot unify a metavariable across the
  pattern-binder (`Some($V)`) and the expression (`=> $V`) positions — they are different
  node kinds — so the reused-metavar form matches **nothing**.
- The only expressible form drops that tie (`Some($V) => $X, None => $D`), which matches
  **every** two-arm `Some/None`+`Ok/Err` match in the tree: **208 hits**, the overwhelming
  majority not `unwrap_or` at all (`Some(v) => v + 1`, `Some(v) => other`, arbitrary `None`
  bodies, etc.). No structural sub-pattern isolates the identity-return case. Per "a noisy
  rule is worse than no rule," dropped.

### R2. `x == true` / `x == false` redundant boolean comparison — **rejected: fixture-only + un-fixable as one rule**
- `== true`/`== false` is genuine redundancy (`x` / `!x`), but the **only** corpus hits are
  **6, all in one file** — `examples/v05/checked-mir/option_result_predicates.hew`, a MIR
  fixture that *deliberately* writes `if a.is_some() == false { … }` to pin the `== false`
  lowering. Firing there is noise against intentional test code (the fail-closed rules
  likewise `ignore` test code). The two directions also need different fixes
  (`==true`→`x`, `==false`→`!x`), which a single rule can't express. No application-code
  hits → dropped. Easy to revisit if real hits appear (exclude the fixture, split fixes).

### R3. `if c { true } else { false }` ⇒ `c` — **rejected: 0 hits, no demonstrated need**
- **0 occurrences** in `std/` + `examples/`. A pure-style rule with no demonstrated case;
  not worth the maintenance/false-positive surface. (Would also need care: an `if/else`
  with side-effecting arms is not equivalent.)

### R4. Double negation `!!x` ⇒ `x` — **rejected: 0 hits**
- **0 occurrences** in the corpus. No signal.

### R5. `clone <Copy variable>` (broad form of the memory note) — **rejected: needs types**
- `clone x` is redundant only when `x`'s type is Copy/scalar. ast-grep has no type
  information, so it cannot tell `clone x` (i64, redundant) from `clone x` (String, a real
  deep copy). Shipping the broad form would be FP-prone; the always-safe **literal** subset
  is shipped as rule #2 instead.

---

## Summary

| Rule | Severity | Hits | Fix | Verdict |
|------|----------|------|-----|---------|
| `match-bool-predicate`    | warning | 2  | — (recursion) | ship — 2 TP (predicate re-impls); guard for app code |
| `redundant-clone-literal` | warning | 0  | **safe** | ship — regression guard, mechanism proven |
| `len-zero-is-empty`       | hint    | 50 | — (type-dep) | ship — 50/50 TP, 0 FP |
| `empty-string-is-empty`   | hint    | 3  | **safe** | ship — 3/3 TP |
| match → `unwrap_or`       | —       | 208 (loose) | — | reject — cross-category metavar; can't isolate |
| `== true` / `== false`    | —       | 6   | — | reject — fixture-only, two-direction fix |
| `if{true}else{false}`     | —       | 0   | — | reject — no hits |
| double negation `!!`      | —       | 0   | — | reject — no hits |
| `clone <Copy var>`        | —       | n/a | — | reject — needs type info |
