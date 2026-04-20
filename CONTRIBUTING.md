# Contributing to Hew

Thank you for your interest in contributing to Hew! This document covers how to get started.

## Getting Started

1. Fork and clone the repository
2. Install the [prerequisites](README.md#prerequisites)
3. Build from source: `make`
4. Run the tests: `make test`

See the [Building from Source](README.md#building-from-source) section of the README for detailed setup instructions.

## Development Workflow

1. Create a branch for your work
2. Make your changes
3. Run `make test` to verify nothing is broken
4. Run `make lint` to check for warnings
5. Submit a pull request

### Using LESSONS.md

[`LESSONS.md`](LESSONS.md) is a structured decision aid for contributors. Before merging a change, match it against the **trigger** column in LESSONS.md and apply every matching row's **apply** checklist. Start with **P0** rows (correctness and boundary safety), then **P1** (parity, tests, diagnostics), then **P2** (architecture and cleanup). When two rules conflict, keep the stricter fail-closed, ownership-preserving, or parity-preserving rule.

Key boundary checks most contributors encounter:

- **`serializer-fail-closed` (P0):** Any Rust-to-C++ or wire boundary must hard-error on unsupported shapes — never silently omit.
- **`checker-output-boundary` (P0):** Reject unresolved `Ty::Var` and missing checker metadata at `check_program` output. Serialize/codegen should consume checker-authoritative types instead of reconstructing them from AST fallbacks.
- **`native-wasm-parity` (P1):** New runtime behaviour (channels, timers, actors) needs both a native and a WASM implementation, or an explicit `// WASM-TODO:` comment plus a PR note.  See [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the authoritative Tier 1 / Tier 2 feature table and disposition of each unsupported feature.
- **`test-runner-trust` (P1):** Changes to discovery, reporting, or timeout in `hew test` must keep the runner fail-closed on parse errors and preserve stable ordering.

## What to Work On

- Check [open issues](https://github.com/hew-lang/hew/issues) for tasks labeled `good first issue` or `help wanted`
- Bug reports and fixes are always welcome
- For larger features or design changes, please open an issue first to discuss the approach

## Code Style

- **Rust:** Follow standard `rustfmt` conventions. Run `cargo clippy --workspace` before submitting.
- **C++:** LLVM style (see `hew-codegen/.clang-format`). Use C++20 features where appropriate.
- **Commit messages:** Use imperative mood ("Add feature" not "Added feature"). Keep the first line under 72 characters.
- **Stdlib `int` surface:** Every `pub fn` parameter and return type in `std/**/*.hew` must use `int`, not `i32`/`i64`. Width-specific types are only allowed inside `extern "C" { ... }` blocks or on lines marked `// INTERNAL-ABI: <reason>`. See [`docs/stdlib-style-contract.md`](docs/stdlib-style-contract.md) for the full contract and examples. The rule is enforced by `scripts/lint-stdlib-int-surface.sh` and the `stdlib-lint` CI workflow.

## Formatting

All code should pass the project's formatters (`rustfmt`, `clang-format`, `taplo`, `shfmt`, `prettier`). The easiest way to enforce this automatically is to symlink the formatting script into your local hooks:

```bash
mkdir -p .git/hooks/pre-commit.d
ln -sf ../../../scripts/pre-commit-fmt.sh .git/hooks/pre-commit.d/format
```

This formats staged files and re-stages them on every commit.

## Build System

Always use `make` targets instead of running `cargo`, `cmake`, or `ctest` directly. See the [Makefile](Makefile) header for all available targets.

## Testing

### Test suite overview

| Suite | Command | Scope | Speed |
|---|---|---|---|
| Full (default) | `make test` | Rust workspace + native codegen E2E + Hew test files + C++ unit | slow |
| Extended | `make test-all` | Rust workspace + native codegen E2E + stdlib type-check sweep + Hew test files + WASM E2E (no `test-cpp`) | slowest |
| Rust only | `make test-rust` | All Rust workspace crates | fast |
| Parser / lexer | `make test-parser` | `hew-parser` + `hew-lexer` | fast |
| Type checker | `make test-types` | `hew-types` + `hew-parser` + `hew-lexer` | fast |
| CLI | `make test-cli` | `hew-cli` + `adze-cli` | fast |
| Codegen E2E | `make test-codegen` | Native CMake/ctest suite (builds runtime first) | slow |
| WASM E2E | `make test-wasm` | Same ctest suite, `wasm`-labelled tests only; requires `wasmtime` | slow |
| C++ unit | `make test-cpp` | `mlir_dialect`, `mlirgen`, `translate`, `codegen_capi`, `msgpack_reader` | medium |
| Hew test files | `make test-hew` | `tests/hew/` via `hew test` | medium |

Use the fast narrow suites (`test-parser`, `test-types`, `test-cli`) during inner-loop iteration and `make test` before opening a PR.

`make ci-preflight` dispatches a conservative local preflight from your current diff. Pass `ARGS="--dry-run"` to preview without running.

### AST codegen self-test

`hew-astgen` generates `hew-codegen/src/msgpack_reader.cpp` from the Rust AST types. If you modify `hew-parser/src/ast.rs` or `hew-parser/src/module.rs`, regenerate the C++ reader and verify it matches:

```bash
make astgen                    # regenerate msgpack_reader.cpp
cargo test -p hew-astgen       # verify checked-in file matches generator output
```

There is no `make test-astgen` target; the verification runs through `cargo test`.

### E2E test workflow

When adding new language features, add an end-to-end test:

1. Create a `.hew` source file under `hew-codegen/tests/examples/`.
2. Register it in `hew-codegen/tests/CMakeLists.txt` using `add_e2e_test`:
   ```cmake
   add_e2e_test(my_feature e2e_my_feature/my_feature.hew "expected output\n")
   ```
3. **WASM parity** (see `native-wasm-parity` in LESSONS.md): if the feature is supported on WASM, also register it with `add_wasm_file_test` so the WASM suite exercises the same path:
   ```cmake
   add_wasm_file_test(my_feature e2e_my_feature my_feature)
   ```
   The `add_wasm_file_test` macro requires that you create the corresponding `examples/e2e_my_feature/my_feature.expected` file (read by the WASM CMake helpers) containing the expected output.
   If WASM support is deferred, add a `// WASM-TODO: <reason>` comment at the registration site.
4. Add type-checker tests in `hew-types/src/check/tests.rs` for any new type rules.

### WASM / native parity

New runtime behaviour — channels, ask/reply, timers, schedulers, bounded execution — must ship with a WASM implementation or an explicit tracked gap. Per LESSONS.md `native-wasm-parity` (P1):

- Implement both native and WASM paths, or add `// WASM-TODO: <reason>` where the WASM path is deferred.
- New `hew_*` runtime exports must be classified `jit: stable` or `jit: internal` in `scripts/jit-symbol-classification.toml` alongside their WASM disposition declaration; `scripts/verify-ffi-symbols.py --classify stable --validate` rejects unclassified exports.
- Add contract tests for timeout, cancel, and budget edges.
- Document intentional divergence where parity cannot land yet.
- Register new E2E tests in `CMakeLists.txt` with both `add_e2e_test` and `add_wasm_file_test` where applicable.
- Consult [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the canonical Tier 1 / Tier 2 split and the current disposition (pass / warn / reject) for each feature.  The checker enforces these dispositions automatically when `--target=wasm32-wasi` is used.

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project: MIT OR Apache-2.0.
