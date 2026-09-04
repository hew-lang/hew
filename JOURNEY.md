# Core integration

## Initial checkpoint

- Preserve the profiler port-only address fix; focused profiler unit tests pass.
- Remove the unused LLVM metadata scraper and the help renderer's
  implementation-derived expectation test. Retain actual runner failure and
  malformed-input coverage.
- `make test-build-harness`, `make help` and `git diff --check` pass.
- Native ownership cutover and whole-compiler acceptance remain unfinished.

## Owned SSA availability

- Add a finite CFG fixed-point check for owned SSA availability, including
  incoming parameters, consuming operations, edge transfers, call/unwind
  results and loop-local dynamic definitions.
- Negative lifetime tests failed before implementation; all SIR crate unit
  and integration tests pass with the verifier hook enabled.
- Borrow-region and place-initialization checks remain separate unfinished
  components. Ownership operations remain closed in the relation table until
  those checks and the executable lowering are ready.

## Core acceptance manifest

- Added the initial six audited native value-semantics cases and their exact
  O0/O2 output and exit outcomes. `make core-acceptance` builds the native
  compiler once, then asks `xtask core-acceptance` to compile and execute each
  case at both profiles.
- The runner reports compiler version, host, profile and instrumentation and
  distinguishes source diagnostics, compiler/program crashes, timeouts, wrong
  exit/output and environment failures. It intentionally has no green safety
  or native-smoke placeholder while those manifest cases are absent.
- `make test-core-acceptance-runner`, the full `make core-acceptance`, and the
  focused `--case entry-return` recipe all pass with the isolated target tree.
- Repaired the existing `quick-xml` text-event inference ambiguity in the
  nextest ratchet without changing its whitespace or role checks; this lets
  the xtask runner compile on the current dependency graph.
- Factored the existing Rust format and JSON Clippy invocation into
  `make lint-rust`; `make lint` still runs that target and every pre-existing
  lint gate. A focused `make lint-rust CLIPPY_ARGS='-p hew-sir'` run passes.
- Removed the duplicate whole-workspace Clippy pass from the formatter hook;
  it still formats and restages staged Rust. `make lint-rust` and CI remain
  the mandatory Clippy authority.
- Consolidated each case to one expected outcome shared by O0 and O2. The
  value cases now print computed values, bytes mutation uses a `var` binding,
  and the runner uses RAII temporary directories with Unicode-safe summaries
  and explicit output-capture read failures.

## Call-result visibility

- Represent call results as normal-edge definitions in the SSA verifier,
  distinct from block-entry values and ordinary operation results. A result
  must be forwarded to a continuation argument before the body can use it.
- The new negative test first demonstrated acceptance of a call result as
  its own input. It now rejects operation, input, unwind and direct
  continuation uses while retaining valid normal-edge forwarding.
- The integrated native acceptance cases pass at O0 and O2; these exercise
  the existing native route, not a completed ownership cutover or sanitizers.

## Borrowed-input boundary

- Guaranteed inputs may be read or copied explicitly, but cannot be consumed,
  transferred into an owned block argument or returned without a copy.
  A call's borrow operand remains permitted; its operation contract must
  independently establish synchronous, non-retaining use.
- The consumption and return negatives failed before the check. Focused SIR
  verification also covers an owned copy and normal/unwind call borrowing.
- This does not admit local borrow regions, suspension or new ownership
  operations. Their operation contracts and executable lowering remain work
  for the first native owned-value slice.

## Result consumer intake

- Restacked the complete 19-commit Result consumer migration from PR #3349
  onto integration checkpoint `6892e1017` without textual conflicts.
- Regenerated the C ABI surface after the integration base and PR both added
  `hew_ask_error_translate_for_public_result`; the generator removed only the
  duplicate manifest row. `make cabi-surface-check test-cabi-surface` passes.
- `make test-vertical-slice` reproduced the stale transition after
  `vec_iter_free_fold_unwind`: `fs.read` now returned structured `IoError`,
  while its old fixture still forced a generic `Result.unwrap` panic.
- Replaced that panic assertion with an executable `IoError.NotFound` branch
  and removed the obsolete `os.args(index)` panic case. The replacement
  `os.args() -> Vec<string>` surface is already exercised on its success path,
  and ordinary Vec out-of-bounds traps have dedicated fixtures.
- The complete vertical-slice run reaches its final fixture after passing the
  replacement Result case. Existing #3127 and #3226 runtime failures remain
  explicitly classified by the harness rather than being migration failures.
- `make checked-mir-run` passes. `make checked-mir-verify` reports composed
  dump drift: the Result migration removes the obsolete `AskError` "no error"
  display arm, while the integration base changes ownership classes for actor
  handles and aggregates. Leave these generated files for one regeneration on
  the final combined integration base rather than recapturing them here.
- `make hew-fmt-check` and `make lint` pass with an isolated target directory,
  sccache and an eight-job build cap.
