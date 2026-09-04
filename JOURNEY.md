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
