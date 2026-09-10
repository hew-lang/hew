# Native core acceptance

Each case is a supported native program with an exact observable outcome, held
as one `.toml` file in `cases/` next to its `.hew` source; the file name must
match the case's `id`. `make core-acceptance` builds the native compiler,
loads every `cases/*.toml` in sorted order and checks each case's expected
outcome. A new case is one `.hew` file plus one `.toml` file, so adding a case
never conflicts with another case's file.

## Expectation kinds

A case's `kind` says what it proves. Omitting `kind` is `kind = "run"`, so
every existing case is unaffected.

- `kind = "run"` (the default): the runner compiles and executes the source
  at O0 and O2, and `[case.expected]` holds `stdout`, optional `stderr`
  (default empty) and `exit`.
- `kind = "check"`: the runner runs `hew check --format json` against the
  source once — no build, no execution — and asserts it exits 1 with exactly
  the expected diagnostics, nothing extra and nothing missing. `[[case.expected.diagnostics]]`
  is a list of tables, each with:
  - `code` — the JSON diagnostic's `code` field, verbatim. This is the
    checker's stable `kind` discriminant (see `hew-cli/src/diagnostic_json.rs`),
    not always an `E_*`/`W_*` token: a diagnostic whose specific check lives
    under a generic discriminant (`InvalidOperation` covers several distinct
    `E_*` checks today) reports that generic string here, and its `E_*`/`W_*`
    token lives only in `message`.
  - `line`, `column` — the diagnostic's 1-based `span.start_line` and
    `span.start_col` from the JSON output, not a text-rendered position.
  - `message` (optional) — a substring the diagnostic's `message` must
    contain. This is how a case pins down a specific check when `code` is
    one of the generic discriminants above.

  A `check` case cannot join the `safety` suite — safety (ASan/LSan) stays a
  suite selected by `suites`, never a case kind, since a `check` case is
  never sanitizer-compiled.

Ground the exact `code`/`line`/`column`/`message` for a new `check` case by
running `hew check <source> --format json` against the built debug compiler
and reading the JSON array it prints on stdout, rather than guessing from the
text renderer or from the source-level `E_*` name.

`make core-safety` builds the compiler and runtime together with nightly Rust
AddressSanitizer, then runs the safety cases with generated LLVM instrumentation
and leak detection. This target requires Linux and a compatible clang toolchain;
missing tools or instrumentation fail the run. No leak suppressions are used.
The safety cases include bounds faults and nested-call failure with live owners;
their expected fault reports do not excuse leaks or post-failure execution.

Use `CORE_ACCEPTANCE_ARGS='--case bytes-copy-mutate --case string-trim-values'` to focus either
command on one or more cases.
A case must belong to the requested suite. A passing focused run does not prove
the rest of the suite. `CORE_SAFETY_TARGET_DIR` selects the sanitizer build cache.

The cases describe the implemented language, not the entire language. Expand
them as aggregate, resource and actor semantics become executable. Runner
self-tests are separate: `make test-core-acceptance-runner`.
