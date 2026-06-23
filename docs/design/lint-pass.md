# Design: a compiler lint pass for Hew (semantic / dataflow idiom lints)

Status: M1 implemented (this change) · M2–M3 planned · Audience: Hew compiler
maintainers · Companion to the ast-grep rule set

M1 has landed: the lint infrastructure (`LintId` / `LintLevel` / `LintLevels`),
the checker `run_lints` sweep, the first lint (`needless_range_loop`), the CLI
`--allow` / `--warn` / `--deny` flags, and in-source `// hew:allow(...)`
suppression. M2 (more checker lints) and M3 (MIR liveness + dataflow lints)
remain future work; see §10.

## 1. Goal

Add a **lint layer in the Hew compiler** for idiom/code-smell findings that need more than
syntax — the cases ast-grep (purely syntactic) cannot do precisely: "is this index only used to
index that collection?", "is this value ever read again?", "does this branch always diverge?".
Model: Clippy (HIR/MIR lints on top of the compiler's own analysis). Lints are **non-fatal
warnings**, **suppressible**, and surfaced in the CLI, editors, and the website.

Non-goals: replacing the ast-grep rules (they stay as the cheap, build-free, CI/grep layer);
a general plugin system; cross-crate/interprocedural analysis.

## 2. Why the compiler (not ast-grep, Semgrep, CodeQL)

ast-grep has no CFG / def-use. Semgrep can't parse Hew. CodeQL would need a bespoke Hew
extractor that just re-encodes analysis the compiler already has. The compiler owns the typed
AST + HIR + MIR (with a CFG and move/init dataflow) **and** a warning channel that already
reaches every surface. That is the right and cheapest home.

## 3. Key finding — surfacing is already free

A warning pushed into the checker's `warnings` vector reaches all three surfaces with **no new
plumbing**:

- Channel: `TypeCheckOutput { errors, warnings: Vec<TypeError> }` (`hew-types/src/check/types.rs`),
  drained by `check_program` (`hew-types/src/check/mod.rs`).
- CLI: rendered via `render_warning` / `render_frontend_diagnostics`; warnings do **not** fail the
  build unless `--Werror` (`hew-cli/src/compile.rs`, `hew-cli/src/main.rs`).
- Editors: `hew-lsp` converts `tc.errors.chain(tc.warnings)` → LSP `Diagnostic`, `Warning`→WARNING,
  republished on every edit (`hew-lsp/src/server/analysis.rs`, `mod.rs`).
- Website: `hew-wasm` `type_check()/analyze()` return diagnostics via `convert_diagnostics`
  (`hew-wasm/src/lib.rs`).

Therefore **checker-stage lints need zero surfacing work.** MIR-stage lints (phase 2) need a
small amount of wiring (see §7).

## 4. Emission template (the clone-on-Copy precedent)

Today's non-fatal lints are just `TypeError { severity: Warning }` pushed onto `self.warnings`.
The cleanest precedent is the `clone`-on-a-Copy-type style warning
(`hew-types/src/check/methods.rs`): detect → `self.warnings.push(TypeError { severity:
Warning, kind: StyleSuggestion, span, message, suggestions, .. })`. Other examples: `UnusedImport`,
`DeadCode` (`check/diagnostics.rs`), `UnreachableCode` (`check/statements.rs`).
`TypeError` already carries `span`, `notes`, and `suggestions`
(`hew-types/src/error.rs`). A test pins the invariant that lint warnings have
`Severity::Warning` (`check/tests.rs`).

## 5. The dataflow reality (scope the ambition honestly)

The existing MIR pass `hew-mir/src/dataflow.rs` is **bespoke move/init legality**
(`InitialisedBeforeUse`, `UseAfterConsume`, `MustConsume`) over a real CFG
(`BasicBlock::successors`, `model.rs`), **not** a generic liveness/reaching-defs
framework — there is no "is local L live at point P" query. So split the lints by what they need:

- **Scoped-structural + type info** (no new dataflow): `needless_range_loop`, `redundant-else-
  after-return`, `match {Some(x)=>…, None=>{}} → if let`, `if c {true} else {false} → c`,
  `len()>0 → !is_empty()`. Do these in the **checker** on the typed AST with small visitors.
- **Global liveness** (needs new analysis): dead-store ("assigned then never read"),
  clean-counter ("`i` never read after the loop"), redundant-clone ("cloned then never mutated").
  These need a liveness/reaching-defs pass added to MIR (phase 2).

Note: `for i in 0..xs.len()` is **lowered away** in MIR into a counter loop
(`hew-mir/src/lower.rs`, `lower_for_range`). The high-level `for` only survives at
AST/HIR — another reason `needless_range_loop` belongs in the checker, not MIR.

## 6. Architecture

### 6.1 Lint infrastructure (`hew-types/src/check/lints/`)
- `LintId` enum (M1: `NeedlessRangeLoop`; future: `RedundantElseAfterReturn`, `LenGtZero`, …).
  Each lint has a stable `as_str()` name, a `from_name()` parser, and a `default_level()`
  (`Warn`).
- `LintLevel { Allow, Warn, Deny }` + a `LintLevels` map, built from (a) defaults, (b) the CLI flag
  (§7), (c) in-source suppression (§7). Resolve level at emit time; `Allow` drops the diagnostic,
  `Deny` routes to `output.errors`, `Warn` to `output.warnings`.
- The lint id is carried on the diagnostic: `TypeErrorKind::Lint(LintId)` so suppression,
  `--Werror`, and docs can key off it. This also lets the existing ad-hoc warnings (clone-on-Copy,
  dead-code) migrate onto the registry over time.

### 6.2 Checker lint sweep (M1)
- A `check::lints` module with `Checker::run_lints(&self, program, levels, out)`, invoked in
  `check_program`'s finalization (after inference + defaulting settle, so each lint trusts
  fully-resolved expression types). It walks every function/method/trait/actor body read-only,
  builds a `LintCtx` carrying the checker's resolved type facts, and runs each enabled lint. Lints
  are pure read-only visitors over the typed AST (so `.len()`/`.get()` receivers are known to be
  collections).

### 6.3 MIR liveness + MIR lints (M3)
- Add a generic-enough **liveness** (backward) and/or **reaching-defs** (forward) analysis in
  `hew-mir`, modelled on the existing worklist/RPO scaffolding (`dataflow.rs`,
  `compute_rpo`, `build_preds`). Expose `live_at(point) -> BitSet<Local>` and `defs_reaching(point)`.
- MIR lints run at the §7 seam (after `check_function`, before codegen). They emit through the MIR
  diagnostic path that the CLI deep-gates already render; **wire them into LSP/wasm too** (those
  currently stop at typecheck+HIR — see §7).

## 7. Flags, suppression, surfacing

- **CLI flag (implemented):** `CommonBuildArgs` (`hew-cli/src/args.rs`) carries repeatable
  `-W/--warn <lint>`, `-A/--allow <lint>`, `-D/--deny <lint>`, each value parsed by
  `LintId::from_name` (an unknown name is a hard CLI error listing the known lints) plus the `all`
  wildcard. They thread through `base_compile_options()` → `CompileOptions` (`compile.rs`) →
  `FrontendOptions` (`hew-compile/src/lib.rs`) → `Checker::set_lint_levels` before `check_program`.
  Because clap collects the three flags into independent lists (their interleaved argv order is not
  recoverable through the derive API), conflicts resolve by **specificity then severity**: a
  specific lint name overrides an `all` wildcard, and within a specificity a stronger level wins
  (`deny` > `warn` > `allow`). `--Werror` already promotes warnings→errors and is unchanged.
- **In-source suppression (implemented):** `// hew:allow(needless_range_loop)` (or
  `// hew:allow(all)`) on the line directly above the flagged construct — or as a trailing comment
  on the construct's own line — drops the finding. The checker only carries byte-offset spans, so
  the front end installs the program's source text via `Checker::set_lint_sources`
  (`hew-types/src/check/types.rs`); the lint resolves the directive against the source line(s)
  preceding the diagnostic's span. A local `allow` wins even over a command-line `--deny`, mirroring
  the rustc/Clippy rule.
- **Surfacing:** M1 (checker) lints are free on all three surfaces (§3). M3 (MIR) lints:
  confirm the LSP/wasm analysis runs the MIR gates or extend `convert_diagnostics`
  (`hew-wasm/src/lib.rs`) and the LSP analysis (`analysis.rs`) to include MIR-stage
  diagnostics as **warnings** (note: HIR diagnostics are currently mapped as LSP *errors* — keep lints
  out of that path).

## 8. First lint, end-to-end: `needless_range_loop` (84 candidates)

The ast-grep probe found 84 `for i in 0 .. <x>.len()` loops by grep. The compiler lint flags
the precise subset that is actually convertible.

- **Detect** (typed AST, in the checker): a `for $I in 0 .. $X.len() { body }` where `$X`'s type is a
  known indexable collection (M1: `Vec<_>`) with `.len()` and `.get()`/index.
- **Confirm** (scoped visitor over `body`, the Clippy `needless_range_loop` algorithm — this is a use
  check, not global dataflow):
  - every use of `$I` is exactly `$X.get($I)` / `$X[$I]` (index-only);
  - `$I` is not reassigned in the body; `$X` is not reassigned or length-mutated (`push`/`pop`/`clear`…)
    in the body.
- **Suggest**: `for <elem> in $X { … }`, or `.enumerate()` if the index is needed elsewhere.
- **Emit**: `TypeError { Warning, Lint(NeedlessRangeLoop), span = the for-loop, suggestion }`.
- Rationale for home: the `for` exists only pre-MIR; the checker has the structure + types + the
  warning channel. No MIR or liveness needed for this one.

## 9. Testing

- Unit: extend the `parse_and_check → assert warnings` style (`check/tests.rs`). Keep the
  `lint_warnings_have_warning_severity` invariant.
- Positive fixtures: representative true positives (drawn from the 84 candidates).
- Negative fixtures (no false positives): `i` used for more than indexing; `xs` mutated in the loop;
  `i` reassigned; nested loops sharing `i`. These guard precision.
- Suppression: `// hew:allow(needless_range_loop)` silences it; a directive for a *different* lint
  does not; `--deny` promotes to error; an in-source allow overrides `--deny`.
- End-to-end (`hew-cli/tests/lint_pass_e2e.rs`): the lint renders through real `hew check`, and the
  flags / directive re-level or suppress it as specified.
- Cross-check: run the ast-grep C8 candidate set (84) and confirm the compiler lint is a precise
  subset — the gap between them measures the value the dataflow/visitor adds over pure syntax.

## 10. Staging

- **M1 — infra + first lint. (Implemented in this change.)** `LintId`/`LintLevels`, the
  `--warn/--allow/--deny` flags, in-source `// hew:allow(...)`, the checker `run_lints` sweep, and
  `needless_range_loop`. Self-contained, high value, free surfacing. (No MIR work.)
- **M2 — more checker lints.** `redundant-else-after-return` (reuse the checker's existing
  `UnreachableCode` reachability to know the `then` diverges), `match-empty → if let`, `len()>0 →
  !is_empty()`, `if c {true} else {false} → c`. Migrate the existing ad-hoc warnings onto the registry.
- **M3 — MIR liveness + dataflow lints.** Add liveness/reaching-defs to `hew-mir`; implement
  `dead-store` and `clean-counter` (counter not read after the loop); wire MIR lints into LSP/wasm.

## 11. Division of labour with ast-grep

ast-grep stays the cheap syntactic/CI/IDE-grep layer (the 15+ rules). The compiler lint pass is the
authoritative semantic layer. They compose: ast-grep cheaply flags candidate shapes; the compiler
lint confirms with structure+types (and, in M3, liveness) before suggesting the rewrite. The
ast-grep candidate counts are a ready-made precision oracle for each compiler lint.
