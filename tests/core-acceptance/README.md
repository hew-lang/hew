# Native core acceptance

`make core-acceptance` builds the native compiler and checks the manifest's
expected stdout, stderr and exit status at O0 and O2.

`make core-safety` builds the compiler and runtime together with nightly Rust
AddressSanitizer, then runs the safety cases with generated LLVM instrumentation
and leak detection. This target requires Linux and a compatible clang toolchain;
missing tools or instrumentation fail the run. No leak suppressions are used.

Use `CORE_ACCEPTANCE_ARGS='--case bytes-copy-mutate'` to focus either command.
A case must belong to the requested suite. A passing focused run does not prove
the rest of the suite. `CORE_SAFETY_TARGET_DIR` selects the sanitizer build cache.

The manifest describes the implemented cases, not the entire language. Expand
it as aggregate, resource and actor semantics become executable. Runner
self-tests are separate: `make test-core-acceptance-runner`.
