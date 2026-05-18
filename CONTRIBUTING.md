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

PR titles, PR bodies, and commit messages are part of the permanent project history after squash merge.
Keep them free of model names, orchestration jargon, and internal-only path references such as `.claude/`.

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
- **Commit messages:** Use imperative mood ("Add feature" not "Added feature"). Keep the first line under 72 characters.
- **Stdlib `int` surface:** Every `pub fn` parameter and return type in `std/**/*.hew` must use `int`, not `i32`/`i64`. Width-specific types are only allowed inside `extern "C" { ... }` blocks or on lines marked `// INTERNAL-ABI: <reason>`. See [`docs/stdlib-style-contract.md`](docs/stdlib-style-contract.md) for the full contract and examples. The rule is enforced by `scripts/lint-stdlib-int-surface.sh` and the `stdlib-lint` CI workflow.

## Formatting

All code should pass the project's formatters (`rustfmt`, `clang-format`, `taplo`, `shfmt`, `prettier`). Run `make install-hooks` after cloning. This wires pre-commit formatting/clippy and a pre-push fast gate.

The installer is worktree-safe and targets the shared git common dir, so linked worktrees inherit the same hooks; run it once from the main checkout.

#### Pre-push gate

The pre-push hook runs `cargo fmt --all -- --check` — it is intentionally fast. Its job is to catch unformatted code before it reaches review; it is not a substitute for CI.

For substantive changes, run `make ci-preflight` yourself before opening a PR. CI runs `make ci-preflight` (or its dispatcher) on every PR regardless, so formatting errors, clippy violations, and test failures will be caught there. The pre-push hook just keeps the signal fast and local.

If `cargo fmt --check` fails: run `cargo fmt --all` and re-push. There is no environment-based exemption and no `--no-verify` bypass.

## Build System

Always use `make` targets instead of running `cargo` directly. See the [Makefile](Makefile) header for all available targets.

## Testing

### Test suite overview

| Suite | Command | Scope | Speed |
|---|---|---|---|
| Full (default) | `make test` | Rust workspace (via nextest) | medium |
| Extended | `make test-all` | `make test` + stdlib type-check sweep + Hew test files | slow |
| Rust only | `make test-rust` | All Rust workspace crates | medium |
| Parser / lexer | `make test-parser` | `hew-parser` + `hew-lexer` | fast |
| Type checker | `make test-types` | `hew-types` + `hew-parser` + `hew-lexer` | fast |
| CLI | `make test-cli` | `hew-cli` + `adze-cli` | fast |
| Runtime / net | `make test-runtime-net` | `hew-runtime` + `hew-analysis` + `hew-lsp` + `hew-std-net-*` | fast |
| Runtime (no-net) | `make test-runtime-unit` | `hew-runtime` unit + integration tests, without QUIC/TLS/profiler stack (~3× faster compile) | fast |
| Hew test files | `make test-hew` | `tests/hew/` via `hew test` | medium |

Use the fast narrow suites (`test-parser`, `test-types`, `test-cli`, `test-runtime-net`, `test-runtime-unit`) during inner-loop iteration and `make test` before opening a PR.

`make test-runtime-unit` is the recommended target when iterating on `hew-runtime` logic that does not touch QUIC, TLS, or the profiler. It runs the full `hew-runtime` test suite (lib unit tests + all integration tests) with `--no-default-features`, cutting compile time roughly 3× (measured: ~32 s vs ~85 s per integration test binary on a warm build cache). The two profiler allocator tests in `transport.rs` are excluded under this target because they require active allocation counters to be meaningful; they still run under `cargo test -p hew-runtime` (default features).

`make ci-preflight` dispatches a conservative local preflight from your current diff and is the recommended manual gate before opening a PR or tagging a release. Pass `ARGS="--dry-run"` to preview without running. CI runs this on every PR regardless.

### E2E test workflow

When adding new language features, add an end-to-end test:

1. Create a `.hew` source file under `tests/hew/`.
2. Run it via `make test-hew` (`hew test tests/hew/`).
3. **WASM parity** (see `native-wasm-parity` in LESSONS.md): if the feature is supported on WASM, exercise it via the `wasi_run_e2e` integration tests under `hew-cli/tests/`. If WASM support is deferred, add a `// WASM-TODO(#NNN): <reason>` comment at the registration site, where `#NNN` is a GitHub issue tracking the gap (use [#1451](https://github.com/hew-lang/hew/issues/1451) for general WASM parity work).
4. Add type-checker tests in `hew-types/src/check/tests.rs` for any new type rules.

### WASM / native parity

New runtime behaviour — channels, ask/reply, timers, schedulers, bounded execution — must ship with a WASM implementation or an explicit tracked gap. Per LESSONS.md `native-wasm-parity` (P1):

- Implement both native and WASM paths, or add `// WASM-TODO(#NNN): <reason>` where the WASM path is deferred. The `#NNN` must be an open GitHub issue; use [#1451](https://github.com/hew-lang/hew/issues/1451) for general WASM parity gaps. `make lint-wasm-todo` rejects bare `WASM-TODO:` markers without an issue reference.
- New `hew_*` runtime exports must be classified `jit: stable` or `jit: internal` in `scripts/jit-symbol-classification.toml` alongside their WASM disposition declaration; `scripts/verify-ffi-symbols.py --classify stable --validate` rejects unclassified exports.
- Add contract tests for timeout, cancel, and budget edges.
- Document intentional divergence where parity cannot land yet.
- Consult [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the canonical Tier 1 / Tier 2 split and the current disposition (pass / warn / reject) for each feature.  The checker enforces these dispositions automatically when `--target=wasm32-wasi` is used.

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project: MIT OR Apache-2.0.
