# ast-grep rule set for Hew

Structural lint rules run through the repository-pinned ast-grep tool (wired via
`sgconfig.yml` `ruleDirs`).

## Running

```sh
make structural-lint-bootstrap  # first use: acquire pinned tool + grammar, then scan
make structural-lint            # later: cache-only pinned scan + authority ratchets
.ast-grep/tool/bin/ast-grep scan --rule rules/rust/fail-closed/ok-question-in-lowering.yml  # one pinned rule
```

Bootstrap is explicit so ordinary local lint never downloads dependencies. The
complete scan reports the two tracked baseline error rules as warnings; all other
error-severity rules and the authority inventory remain fail-closed. `warning`/
`info`/`hint` rules report without failing.

## Layout

| Path | Domain | Invariant |
|------|--------|-----------|
| `rules/rust/fail-closed/` | codegen + checker fail-closed | CLAUDE.md §2 (Fail-Closed Codegen), §3 (Type Inference Boundary) |
| `scripts/structural-authority-audit.py` | cross-stage semantic authority | Parsed AST inventory plus intraprocedural owner-shortening flow into registry/ID/call-target keys. |
| `rules/rust/panics-nyi/` | panic / NYI hygiene | no new NYI; `unreachable!("desc")`; propagate errors |
| `rules/rust/concurrency-drop/` | concurrency + drop safety | CLAUDE.md §1 (Drop Safety), §9 (Concurrency Safety) |
| `rules/rust/hygiene/` | unsafe / debug hygiene | `// SAFETY:` justification, `transmute` audit, no `dbg!` |
| `rules/hew/` | Hew-language patterns (`.hew`) | idiomatic / redundant-construct lints |

## Conventions

- Each rule is one YAML file with `id`, `language`, `severity`, `message`, `rule`, and a `files`
  scope. Prefer **high precision** (few/no false positives on the current tree) over coverage.
- Rust rules exclude test code (via `ignores` globs and an enclosing `#[cfg(test)]`-mod guard).
- Every rule is validated against the current tree: see the per-domain `_REPORT.md` for the
  hit count and true/false-positive triage at the time it was written.

## Rule catalog

Counts are findings on the tree when written; `0` rules are regression guards.

### Rust — fail-closed (`error`, gates CI)
| Rule | Hits | Catches |
|------|------|---------|
| `ok-question-in-lowering` | 0 | `$E.ok()?` in codegen/mir/hir — silently returns `None`, swallowing the error (CLAUDE.md §2). |
| `ty-var-constructed-post-inference` | 0 | Building `Ty::Var(..)` in post-inference crates (CLAUDE.md §3). |
| `semantic-owner-shortening-sink` (authority audit) | inventory-ratcheted | `short_name(owner)`, qualified-path leaf extraction, or a module alias flowing through local bindings into registry, `DefId`, `NominalId`, or `CallTarget` keys. Display/diagnostic formatting and ordinary collection `.last()` calls are controls. |

### Rust — panics / NYI (`warning`)
| Rule | Hits | Catches |
|------|------|---------|
| `unreachable-without-message` | 8 | bare `unreachable!()` — wants `unreachable!("why")`. |
| `no-todo-macro` | 0 | `todo!()` / `todo!("…")` (no-new-NYI). |
| `no-unimplemented-macro` | 0 | `unimplemented!()`. |
| `expect-empty-message` | 0 | `.expect("")` with an empty/whitespace message. |

### Rust — concurrency / drop
| Rule | Hits | Catches |
|------|------|---------|
| `no-lifecycle-state-drop-suppression` (`error`) | 0 | Restart/lifecycle-specific actor-free helpers or state-drop suppression options that bypass explicit borrowed/consumed incarnation authority. |
| `lock-unwrap` | 10 | `$M.lock().unwrap()/.expect()` — unwraps a poisoned lock (CLAUDE.md §9; prefer the poison-safe accessor). |
| `explicit-leak-review` | 3 | `mem::forget` / `Box::leak` — RAII escapes to audit for drop-safety (CLAUDE.md §1). |

### Rust — hygiene (`warning` / `hint`)
| Rule | Hits | Catches |
|------|------|---------|
| `unsafe-without-safety` | 54 | `unsafe { … }` block lacking a `// SAFETY:` justification (the repo convention; ~98.5% already carry one). |
| `transmute-audit` | 10 | `mem::transmute(…)` / `transmute::<…>(…)` — the most dangerous `unsafe` op; review inventory. |
| `dbg-macro` | 0 | `dbg!(…)` left in code — preventive guard against shipping debug output. |

### Hew (`.hew`)
| Rule | Hits | Fix | Catches |
|------|------|-----|---------|
| `len-zero-is-empty` | 53 | — | `x.len() == 0` → `x.is_empty()`. |
| `match-bool-predicate` | 2 | — | `match o { Some(_) => true, None => false }` → `.is_some()`. |
| `empty-string-is-empty` | 3 | ✅ | `s == ""` → `s.is_empty()`. |
| `redundant-clone-literal` | 0 | ✅ | `clone <literal>` — the clone is a no-op. |
| `while-counter-loop` | 99 | — | `var i = 0; while i < n { … i = i + 1 }` → `for i in 0..n { … }` (advisory). |

## Suppressing a finding

The fail-closed rules honor an annotation on the **preceding line**:

```rust
// JUSTIFIED: this None is the intended signal, not a swallowed error
let x = maybe().ok()?;
```

ast-grep's native `// ast-grep-ignore` (or `// ast-grep-ignore: <rule-id>`) suppresses the next line for any rule.

## Gating status

`make structural-lint` runs the complete pinned scan and the fail-closed authority
inventory. The two historical error-rule baselines are reported as warnings until
their findings are triaged; all other error findings fail the gate. The
`warning`/`info`/`hint` rules remain advisory.
