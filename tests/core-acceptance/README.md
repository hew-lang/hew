# Native core acceptance

Each case is a supported native program with an exact observable outcome, held
as one `.toml` file in `cases/` next to its `.hew` source; the file name must
match the case's `id`. `make core-acceptance` builds the native compiler,
loads every `cases/*.toml` in sorted order and checks each case's expected
stdout, stderr and exit status at O0 and O2. A new case is one `.hew` file
plus one `.toml` file, so adding a case never conflicts with another case's
file.

`make core-safety` builds the compiler and runtime together with nightly Rust
AddressSanitizer, then runs the safety cases with generated LLVM instrumentation
and leak detection. This target requires Linux and a compatible clang toolchain;
missing tools or instrumentation fail the run. No leak suppressions are used.
The safety cases include bounds faults and nested-call failure with live owners;
their expected fault reports do not excuse leaks or post-failure execution.

Use `CORE_ACCEPTANCE_ARGS='--case bytes-copy-mutate'` to focus either command.
A case must belong to the requested suite. A passing focused run does not prove
the rest of the suite. `CORE_SAFETY_TARGET_DIR` selects the sanitizer build cache.

The cases describe the implemented language, not the entire language. Expand
them as aggregate, resource and actor semantics become executable. Runner
self-tests are separate: `make test-core-acceptance-runner`.
