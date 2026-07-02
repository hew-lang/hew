# Design: a compiler lint pass for Hew (semantic / dataflow idiom lints)

Status: M1–M3 implemented · Audience: Hew compiler
maintainers · Companion to the ast-grep rule set

M1 + M2 have landed: the lint infrastructure (`LintId` / `LintLevel` /
`LintLevels`), the checker `run_lints` sweep, the CLI `--allow` / `--warn` /
`--deny` flags, and in-source `// hew:allow(...)` suppression — plus nine
checker lints (`needless_range_loop`, `redundant_else_after_return`,
`needless_match_to_if_let`, `len_zero_comparison`, `needless_bool`,
`must_use`, `sleep_loop_blocks_mailbox`,
`text_direction_codepoint_in_comment`, `invisible_codepoint_in_comment`) and the
two ad-hoc warnings (`clone_on_copy`, `dead_code`) migrated onto the registry so
they are now re-levelable and suppressible. M3 has landed too: a backward
liveness dataflow pass in `hew-mir`, the `dead_store` MIR lint built on it, and
the CLI plumbing that surfaces MIR-stage lints as level-controlled, suppressible
warnings (see §10). M4 has landed too: comment-side Trojan-Source scanning over
raw module source, with the text-direction tier denied by default and the broader
invisible-codepoint tier warning by default (see §11). `clean_counter` is
deferred and intentionally **not registered** — so `-D clean_counter` fails
closed as an unknown lint rather than silently no-opping; the reasoning is in §10
(tracked in issue #2178). Editor/web surfacing of MIR lints is deferred to issue
#2176.

## 1. Goal

Add a **lint layer in the Hew compiler** for idiom/code-smell findings that need more than
syntax — the cases ast-grep (purely syntactic) cannot do precisely: "is this index only used to
index that collection?", "is this value ever read again?", "does this branch always diverge?".
Model: Clippy (HIR/MIR lints on top of the compiler's own analysis). Lints are **level-controlled**,
**suppressible**, and surfaced in the CLI, editors, and the website; most default to warnings, while
narrow security/correctness lints may be denied by default.

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
- `LintId` enum (`NeedlessRangeLoop`, `RedundantElseAfterReturn`,
  `NeedlessMatchToIfLet`, `LenZeroComparison`, `NeedlessBool`, plus the migrated
  `CloneOnCopy` and `DeadCode`). Each lint has a stable `as_str()` name, a
  `from_name()` parser, and a `default_level()` (`Warn`).
- `LintLevel { Allow, Warn, Deny }` + a `LintLevels` map, built from (a) defaults, (b) the CLI flag
  (§7), (c) in-source suppression (§7). Resolve level at emit time; `Allow` drops the diagnostic,
  `Deny` routes to `output.errors`, `Warn` to `output.warnings`.
- The lint id is carried on the diagnostic: `TypeErrorKind::Lint(LintId)` so suppression,
  `--Werror`, and docs can key off it. The two ad-hoc warnings (clone-on-Copy,
  dead-code) now route through `Checker::emit_main_pass_lint`, which applies the
  same level/`// hew:allow` resolution to warnings emitted inline during body
  checking (outside the post-inference sweep).
- Checker lints that share a body walk implement the read-only `NodeVisitor`
  trait (`visit_block` / `visit_stmt` / `visit_expr`) driven by the exhaustive
  `walk_body` helper, so a new AST node forces every visitor to make a decision
  rather than silently dropping out.

### 6.2 Checker lint sweep (M1)
- A `check::lints` module with `Checker::run_lints(&self, program, levels, out)`, invoked in
  `check_program`'s finalization (after inference + defaulting settle, so each lint trusts
  fully-resolved expression types). It walks every function/method/trait/actor body read-only,
  builds a `LintCtx` carrying the checker's resolved type facts, and runs each enabled lint. Lints
  are pure read-only visitors over the typed AST (so `.len()`/`.get()` receivers are known to be
  collections).
- The sweep runs only over **user-authored** bodies. Builtin/standard-library modules (`std::`,
  `hew::`, `ecosystem::` — the `is_builtin_module` set) ship with the compiler, so a finding inside
  them is noise the user cannot act on; `run_lints` advances its module index over those modules (to
  keep span tagging aligned with the checker's module walk) but skips emitting on their items. The
  inline main-pass lints follow the same rule (e.g. `dead_code` already skips `std.`-prefixed names).

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
- **Surfacing:** M1/M2 (checker) lints are free on all three surfaces (§3). M3 (MIR) lints are
  surfaced **through the CLI only**. They ride a dedicated `IrPipeline.lint_warnings` channel
  (separate from the hard-error `diagnostics` vector) and are rendered by
  `render_pipeline_mir_lints` (`hew-cli/src/main.rs`), which threads the resolved `LintLevels` into
  every MIR-lowering seam (`hew check`, `build`/`run`, eval, `hew compile`), applies
  `// hew:allow(...)` via `hew_types::directive_suppresses`, renders `Warn` as a warning and `Deny`
  as a build error, drops `Allow`, and structures `--format json` output via `from_mir_lint`. The
  LSP and wasm front ends deliberately stop at HIR and never lower to MIR, so they never reach this
  channel; extending them to run the MIR gates and map MIR-stage diagnostics as LSP/web *warnings*
  (note: HIR diagnostics are currently mapped as LSP *errors* — keep lints out of that path) is
  **deferred to issue #2176**.

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
- **M2 — more checker lints. (Implemented in this change.)** Four new checker-stage lints plus the
  migration of two ad-hoc warnings onto the registry. Each lint stays silent unless its rewrite is
  provably meaning-preserving (precision over recall):
  - **`redundant_else_after_return`** — an `if` whose then-branch diverges on *every* path (ends in
    `return` / `break` / `continue`, or a `!`-typed call such as `panic`) yet still carries an
    `else`; the `else` body can be de-indented. *Reuses* the checker's recorded `Ty::Never` for the
    divergence decision (terminator statements are matched structurally). *Guards:* fires only in
    statement / block-tail position (never a value-position `if` feeding a `let`/arg); skips
    `if let` / `while let` and `else if` chains (not a clean de-indent).
  - **`needless_match_to_if_let`** — a two-arm `match` on an `Option` where one arm is
    `Some(binding)` and the other is a trivial `None => {}` / `None => ()`. Suggests
    `if let Some(binding) = … { … }`. *Guards:* `Option` only (scrutinee type confirmed via
    `resolved_type_at`; `Result` is out of scope); no guards; the `None` body must be empty/unit;
    the `Some` sub-pattern must be a plain identifier or `_`; statement / block-tail position only
    (a value-position match is never rewritten). Note a bare `None` parses as an *identifier*
    pattern, so both spellings are accepted.
  - **`len_zero_comparison`** — a comparison of `<recv>.len()` against `0` / `1` that is really an
    emptiness test: `len() == 0` / `len() <= 0` / `len() < 1` → `is_empty()`; `len() != 0` /
    `len() > 0` / `len() >= 1` → `!is_empty()` (either operand order). *Guards:* one side must be
    literally a no-arg `.len()` method call (a `len` *field* never qualifies) and the other the
    integer literal; the receiver's resolved type must expose `is_empty()` (`Vec`, `HashMap`,
    `HashSet`, `string`). The precise, type-checked, suppressible companion to the syntactic
    `rules/hew/len-zero-is-empty.yml` ast-grep rule.
  - **`needless_bool`** — `if c { true } else { false }` → `c`; `if c { false } else { true }` →
    `!c`. *Guards:* each branch must be exactly one boolean literal (no other statements); the two
    branches must be opposite polarities (a matching pair is a constant, not this lint); `else if`
    chains never collapse the outer `if`. Position-agnostic (the rewrite is valid anywhere).
  - **`must_use`** — a discarded value carrying a write/send/ask error that must not be ignored:
    `WriteError` / `SendError` / `AskError`, bare or as the error arm of a `Result<_, E>`.
    *Guards:* statement position only (a trailing block value, a `let`/`var` binding, a
    `match`/`if let` scrutinee, and `expr?` are all "used" and never flagged); the resolved type
    must be exactly a must-use error or a `Result` over it, matched by canonical name so the
    builtins `SendError` / `AskError` and the stdlib `WriteError` all qualify. Discarding any
    fails open — a dropped backpressure/disconnect signal, an unnoticed undelivered send, or a
    timed-out / mailbox-full / stopped-actor `ask` (`await actor.msg()`) mistaken for a reply.
    Opt out with `let _ = …` or `// hew:allow(must_use)`.
  - **`sleep_loop_blocks_mailbox`** — an actor `receive fn` contains a `loop`, `while true`,
    `while flag`, or `while !flag` whose body directly reaches `sleep` or `sleep_until`, has no
    reachable `break` for that loop, and does not assign the bare guard name inside the loop body.
    The lint runs only through the receive-function dispatch seam, never on free functions, actor
    helper methods, impl methods, or trait methods, because the hazard is actor mailbox
    run-to-completion. *Guards:* compound or derived-progress conditions (`while i < count`,
    `while running && !paused`) are not candidates; `loop_body_has_break` suppresses loops with an
    in-handler break path; the sleep scan stops at nested loop and lambda/generator boundaries so an
    inner loop owns its own finding; the guard-assignment scan is permissive and crosses nested
    bodies to avoid false positives. The suggestion points users at `#[every(duration)]` periodic
    receive handlers plus a boolean flag, where each tick is a separate mailbox dispatch.
  - **Migrations.** `clone_on_copy` (clone on a Copy/BitCopy type; `hew-types/src/check/methods.rs`)
    and `dead_code` (unreachable functions; `hew-types/src/check/diagnostics.rs`) now emit through
    `Checker::emit_main_pass_lint`, giving them a `LintId` with `default_level() = Warn`. Default
    behaviour is unchanged (they still warn), but they are now re-levelable (`-A/-W/-D`) and
    suppressible (`// hew:allow`). The LSP keeps tagging migrated `dead_code` as
    `DiagnosticTag::UNNECESSARY`. Unused-import / unreachable-code were left un-migrated this pass.
- **M3 — MIR liveness + dataflow lints. (Implemented in this change.)** A backward liveness
  dataflow pass in `hew-mir` plus the `dead_store` lint built on it, surfaced through the CLI.
  - **Liveness pass (`hew-mir/src/liveness.rs`).** A backward "may-be-live" analysis over
    `Place::Local(u32)`. It *reuses* the existing forward dataflow scaffolding rather than
    reinventing CFG plumbing: `build_preds` / `successors` / `compute_rpo` and the worklist-fixpoint
    shape from `dataflow.rs`, `instr_reads_writes` as the per-instruction gen/kill source, and
    `terminator_source_places` for terminator reads. Direction: `live_out(b) = ⋃ live_in(s)` over
    successors `s`; the per-block transfer applies the terminator first (its reads become live; its
    writes are deliberately *not* killed — a conservative under-kill) and then the instructions in
    reverse, each as `live = (live − full_defs) ∪ reads`. **Soundness is by over-approximation:** the
    gen set is a superset of the true reads (every referenced place, via `place_local`) and the kill
    set is a subset of the true kills (only a `Place::Local` *full* def via `full_def_local`; anything
    we cannot prove is a total overwrite stays live). So whenever the pass reports a local as **not**
    live-after a point, it is genuinely dead — there are no false "dead" verdicts. When liveness is
    uncertain, the local is treated as **live**. Query API: `analyze_liveness(func) -> Liveness` with
    `Liveness::{is_live_in, is_live_out, live_after}`.
  - **`dead_store`.** Flags `Instr::Move { dest: Place::Local(N), .. }` whose written value is never
    read on any path before being overwritten or going out of scope — i.e. `N` is not live-after the
    move. *Guards (skip → no fire), tuned for precision over recall:* (1) skip parameters
    (`N ≥ params.len()`); (2) user-named locals only (`local_names[N]` present and non-empty —
    compiler-synthetic temporaries and the for-range counter machinery, which carries the sentinel
    `SiteId(0)`, are excluded); (3) no-drop scalar locals only (integers / floats / `bool` / `char`
    via `is_no_drop_scalar`) — this sidesteps drop semantics entirely: no `Drop` ever observes such a
    value, so removing the store is always sound and the lint never fights the drop-safety invariant
    (CLAUDE.md §1); (4) only the pure `Move` form is trapped, never checked arithmetic or any
    instruction whose uses `instr_reads_writes` does not fully model (those are treated as a use). The
    span is recovered from `instr_spans`, falling back to `local_decl_bytes`; findings are de-duped by
    `(lint, span)` to collapse monomorphization duplicates, and synthetic / unreachable functions are
    skipped. Message: *the value assigned to `x` is never read before it is overwritten or goes out of
    scope.* A normal `for i in 0..n` loop does **not** fire — the back-edge keeps the counter
    live-into the header, so the increment store is live and the guards exclude the synthetic counter
    regardless.
  - **`clean_counter` — deferred and NOT registered.** Considered, but intentionally left out of the
    `LintId` registry entirely, for three compounding reasons. (1) *Shape recovery:* a manual
    `c = c + 1` does not lower to a single self-update instruction — it lowers through a temporary and
    a checked `IntArithChecked` followed by `Move c = temp`, so the "increment" is not directly
    recognizable from one instruction. (2) *Liveness cannot separate the two populations:* a dead
    accumulator `c` and a legitimate for-range counter `i` are *both* "live throughout the loop, dead
    at the exit"; distinguishing "the counting itself is dead work" from "a value genuinely consumed
    each iteration" needs faint-variable / strong-liveness analysis (a second dataflow pass asking
    whether a variable's *value*, not merely its liveness, can ever influence an observable) — out of
    scope for this milestone. (3) *Checked arithmetic defeats the premise:* the increment's overflow
    flag feeds a trap branch, so `c` is *strongly* live — its value decides whether the program traps
    — and removing `c = c + 1` would be semantics-changing exactly where the lint looks most
    applicable, i.e. the naive lint would be unsound. Per the precision-first bar that governs M1/M2
    (one excellent lint beats two noisy overlapping ones), `dead_store` ships alone; `clean_counter`
    is revisited once a faint-variable pass exists (tracked in issue #2178). It would also overlap
    `dead_store` on the straight-line `c = c + 1` case, so shipping both today would risk
    double-firing. **It is deliberately not registered** rather than registered-but-unemitted: an
    un-emitted registered lint would make `-D/-W/-A clean_counter` and `// hew:allow(clean_counter)`
    silently no-op, a fail-open that defeats `LintId::from_name`'s fail-closed contract (an unknown
    lint name must surface as a CLI error). With it unregistered, `hew check -D clean_counter` exits
    non-zero with an "unknown lint" diagnostic — locked by a test in `lint_pass_e2e.rs`.
  - **Severity + surfacing.** MIR lints are level-agnostic in `hew-mir`: each finding is a
    `MirLint { lint, span, message }` pushed onto `IrPipeline.lint_warnings`, a channel kept separate
    from the hard-error `diagnostics` vector so the existing move/init checks stay errors. The CLI
    owns policy (§7): `render_pipeline_mir_lints` applies the resolved `LintLevels` and
    `// hew:allow(...)`, renders `Warn` as a warning / `Deny` as a build error / drops `Allow`, and is
    threaded into all four MIR-lowering seams. `hew compile` exposes no lint flags, so it surfaces at
    default levels. Surfacing is **CLI-only**; editor/web is issue #2176.
  - **Tests.** Liveness unit + integration fixtures (`hew-mir/tests/liveness.rs`) pin the query API
    and the over-approximation contract; `dead_store` positives and precision-guard negatives live
    alongside them and in the CLI e2e suite (`hew-cli/tests/lint_pass_e2e.rs`), including the
    `for i in 0..n` regression that must stay silent even under `-D dead_store`.
- **M4 — comment-side Trojan-Source lints. (Implemented in this change.)** Two source-text checker
  lints scan comments (`//`, `///`, `//!`, `/* */`) for codepoints that can make source review lie
  about the bytes the compiler sees. See §11.

## 11. M4: comment-side Trojan-Source lints

The formatter already escapes invisible and bidirectional codepoints when it emits string, f-string,
byte-string, char, and regex literal content. M4 covers the remaining checker-stage source surface:
comments, including doc-comments.

- **Detect:** `hew-parser::fmt::extract_comments(source, true)` scans raw module source once per
  module, including doc-comments for the checker while `hew fmt` still excludes them for formatting.
  Each non-ASCII comment codepoint is classified with the same readability predicate the formatter
  uses for literal emission. The deny tier, `text_direction_codepoint_in_comment`, is exactly the nine
  Unicode explicit bidirectional formatting controls (U+202A–U+202E and U+2066–U+2069). The warning
  tier, `invisible_codepoint_in_comment`, catches other non-printable/default-ignorable scalars such
  as zero-width spaces and variation selectors.
- **Guards:** readable non-ASCII prose (`café`, CJK, emoji, arrows, em dashes) is silent. The comment
  scanner skips string/char/regex literal bodies, so a `//` inside a literal is not a comment start.
  Literal content remains out of scope for `hew check`; formatter escaping owns literal output.
- **Suggest:** remove the hidden codepoint, or spell out the intended directionality/codepoint in
  visible text.
- **Emit:** both lints use the shared `LintId`/`LintLevels`/`LintCtx::emit` path, so `--allow`,
  `--warn`, `--deny`, and `// hew:allow(...)` work like every other registry lint. The text-direction
  tier defaults to `Deny` because the fixed UAX #9 control set has no legitimate Hew-comment use and
  maps to the CVE-2021-42574 class. The broader invisible-codepoint tier defaults to `Warn` because it
  is still suspicious but less specifically a reordering attack.
- **Surfacing:** CLI and wasm already installed lint sources. The LSP analysis path now installs the
  root buffer and resolved module sources on the checker before type-checking, so comment-scanning
  diagnostics and in-source allow directives reach editor diagnostics too.
- **Tests:** checker unit tests cover all comment forms, deny/warn defaults, allow/warn/deny
  overrides, suppression, readable-Unicode negatives, literal-boundary negatives, and once-per-module
  source scanning. CLI e2e tests run real `hew check` subprocesses for default denial, `--allow`,
  `--warn`, `--deny`, directive suppression, and unknown-lint fail-closed behaviour. LSP tests pin the
  editor diagnostic and suppression path.

## 12. Division of labour with ast-grep

ast-grep stays the cheap syntactic/CI/IDE-grep layer (the 15+ rules). The compiler lint pass is the
authoritative semantic layer. They compose: ast-grep cheaply flags candidate shapes; the compiler
lint confirms with structure+types (and, in M3, liveness) before suggesting the rewrite. The
ast-grep candidate counts are a ready-made precision oracle for each compiler lint.
