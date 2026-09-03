# Contributing to Hew

Thank you for your interest in contributing to Hew! This document covers how to get started.

## Getting Started

1. Fork and clone the repository
2. Install the [prerequisites](README.md#prerequisites), including Python 3.12 or newer
3. Build from source: `make`
4. Run the tests: `make test`

See the [Building from Source](README.md#building-from-source) section of the README for detailed setup instructions.
Makefile targets fail fast if `python3` resolves to an older interpreter; point them at
the installed interpreter with `make PYTHON=/path/to/python3.12 <target>` if necessary.

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
- **`native-wasm-parity` (P1):** New runtime behaviour (channels, timers, actors) needs native and WASM execution coverage wherever the target supports it. An intentional target limitation belongs in the typed feature disposition in `wasm-capability-manifest.toml` and needs a focused negative compile/runtime test; source comments are not a substitute for behavior. See [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the generated Tier 1 / Tier 2 feature table.
- **`test-runner-trust` (P1):** Changes to discovery, reporting, or timeout in `hew test` must keep the runner fail-closed on parse errors and preserve stable ordering.

## What to Work On

- Check [open issues](https://github.com/hew-lang/hew/issues) for tasks labeled `good first issue` or `help wanted`
- Bug reports and fixes are always welcome
- For larger features or design changes, please open an issue first to discuss the approach

## Code Style

- **Rust:** Follow standard `rustfmt` conventions. Run `cargo clippy --workspace` before submitting.
- **Commit messages:** Use imperative mood ("Add feature" not "Added feature"). Keep the first line under 72 characters.
- **Commit style:** Follow [COMMIT_STYLE.md](COMMIT_STYLE.md) for the required Conventional Commit header shape, imperative subject voice, and rationale-first body style.
- **Stdlib integer surface:** Every `pub fn` parameter and return type in `std/**/*.hew` uses an explicit-width integer (`i64`, `u64`, `isize`, etc.). The removed aliases `int`/`uint` are not valid type names. See [`docs/stdlib-style-contract.md`](docs/stdlib-style-contract.md) for the full contract and examples.

## Formatting

All code should pass the project's formatters (`rustfmt`, `clang-format`, `taplo`, `shfmt`, `prettier`). Run `make install-hooks` after cloning. This wires pre-commit formatting/clippy and a pre-push fast gate.

The installer is worktree-safe and targets the shared git common dir, so linked worktrees inherit the same hooks; run it once from the main checkout.

#### Pre-push gate

The pre-push hook runs `cargo fmt --all -- --check`, the tracked shell-script
lint, and `actionlint`. It is intentionally fast: its job is to catch local
format/script errors and malformed workflows before they reach review, not to
duplicate the full CI suite.

For substantive changes, run `make preflight` yourself before opening a PR. It is the standard unconditional, fail-fast gate and CI runs the same exhaustive shard assignment on every PR, so formatting errors, clippy violations, and test failures will be caught there. The pre-push hook just keeps the signal fast and local.

If formatting fails, run `cargo fmt --all`; if a script or workflow check
fails, run `make shell-script-lint` or `make actionlint` for the focused
diagnostic. There is no environment-based exemption and no `--no-verify`
bypass.

## Build System

Cargo is authoritative for compiling individual Rust crates. The Makefile is
authoritative for complete Hew artifacts and verification gates; use its
targets instead of reconstructing package/profile combinations by hand.
`xtask` is intentionally limited to automation that consumes Hew's Rust APIs
directly and does not define a competing build graph. See the
[Makefile](Makefile) header for available targets.

## Testing

### Test suite overview

| Suite             | Command                       | Scope                                                                                        | Speed  |
| ----------------- | ----------------------------- | -------------------------------------------------------------------------------------------- | ------ |
| Full (default)    | `make test`                   | Rust workspace (via nextest)                                                                 | medium |
| Stdlib type-check | `make test-stdlib-ratchet`    | `std/` type-check sweep, ratcheted against `scripts/stdlib-expected-failures.txt`            | medium |
| Compiler pipeline | `make test-compiler-pipeline` | Lexer through CLI and package consumers                                                      | medium |
| Runtime (no-net)  | `make test-runtime-unit`      | `hew-runtime` unit + integration tests, without QUIC/TLS/profiler stack (~3× faster compile) | fast   |
| Hew test files    | `make test-hew-ratchet`       | `tests/hew/` via `hew test`, ratcheted against `scripts/hew-suite-expected-failures.txt`     | medium |

Use `test-runtime-unit` for no-network runtime iteration and `test-compiler-pipeline` for compiler iteration. Run `make test` before opening a PR.

`make test-runtime-unit` is the recommended target when iterating on `hew-runtime` logic that does not touch QUIC, TLS, or the profiler. It runs the full `hew-runtime` test suite (lib unit tests + all integration tests) with `--no-default-features`, cutting compile time roughly 3× (measured: ~32 s vs ~85 s per integration test binary on a warm build cache). The two profiler allocator tests in `transport.rs` are excluded under this target because they require active allocation counters to be meaningful; they still run under `cargo test -p hew-runtime` (default features).

`make preflight` runs the lint graph and the same Make-owned test groups as Linux CI; it is the standard manual gate before opening a PR. `make ci-preflight` remains a compatibility alias.

`make test-journeys JOURNEY=day-one|day-two|week-one-local` runs one of the newcomer-facing acceptance journeys under `repros/journeys/` against `HEW_BIN` (the built `hew` by default) and ratchets its step-by-step outcome against `scripts/journeys-expected.tsv`; it fails when a step outside that file fails or a listed step starts passing (the fixing lane deletes that row). `make check-time-ratchet` fails when the median wall-clock of `hew check` on a fixed std fixture exceeds twice the baseline recorded in `scripts/check-time-baseline.tsv`; `make check-time-ratchet-record` writes that baseline for the current host. `make size-ratchet` fails when a workspace crate's `wc -l` over `<crate>/src/**/*.rs` exceeds its ceiling in `scripts/size-ratchet.tsv`; `make size-ratchet-record` writes fresh per-crate counts as ceilings. Ceilings are only ever lowered by deleting code, never raised to meet a count.

### E2E test workflow

When adding new language features, add an end-to-end test:

1. Create a `.hew` source file under `tests/hew/`.
2. Run it via `make test-hew-ratchet` (`hew test tests/hew/`, compared against the tracked expected-failure set).
3. **WASM parity** (see `native-wasm-parity` in LESSONS.md): run the same `.hew` behavior through native and `wasi_run_e2e` coverage wherever the target supports it. If a capability is intentionally unavailable on WASM, classify it in `wasm-capability-manifest.toml` and add a focused test proving the compiler rejects or diagnoses it before link/runtime failure.
4. Add type-checker tests in `hew-types/src/check/tests.rs` for any new type rules.

### WASM / native parity

New runtime behaviour — channels, ask/reply, timers, schedulers, bounded execution — must ship with native and WASM behavior coverage wherever the target supports it. Per LESSONS.md `native-wasm-parity` (P1):

- Exercise supported behavior through shared source corpora and WASI E2E tests. For an intentional platform limitation, add or update the typed feature disposition in `wasm-capability-manifest.toml` and prove its diagnostic with a focused negative test.
- New `hew_*` runtime exports must be classified `jit: stable` or `jit: internal` in `scripts/jit-symbol-classification.toml` alongside their WASM disposition declaration; `scripts/verify-ffi-symbols.py --classify stable --validate` rejects unclassified exports.
- Add contract tests for timeout, cancel, and budget edges.
- Document intentional divergence where parity cannot land yet.
- Consult [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the canonical Tier 1 / Tier 2 split and the current disposition (pass / warn / reject) for each feature. The checker enforces these dispositions automatically when `--target=wasm32-wasi` is used.

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project: MIT OR Apache-2.0.
