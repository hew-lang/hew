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
3. Run the focused checks for the changed behaviour, then the relevant integration suites
4. Run `make lint` to check for warnings
5. Submit a pull request

PR titles, PR bodies, and commit messages are part of the permanent project history.
Keep them free of model names, orchestration jargon, and internal-only path references such as `.claude/`.

### Engineering invariants

[`docs/internal/engineering-invariants.md`](docs/internal/engineering-invariants.md)
records durable principles for design and review. Language and runtime
semantics belong in the specifications; contribution process belongs here and
in the applicable skills.

Key boundary checks most contributors encounter:

- **`serializer-fail-closed` (P0):** FFI and wire boundaries must reject unsupported shapes rather than silently omit values.
- **`checker-output-boundary` (P0):** Reject unresolved `Ty::Var` and missing checker metadata at `check_program` output. Serialize/codegen should consume checker-authoritative types instead of reconstructing them from AST fallbacks.
- **`native-wasm-parity` (P1):** New runtime behaviour (channels, timers, actors) needs native and WASM execution coverage wherever the target supports it. An intentional target limitation belongs in the typed feature disposition in `wasm-capability-manifest.toml` and needs a focused negative compile/runtime test; source comments are not a substitute for behavior. See [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the generated Tier 1 / Tier 2 feature table.
- **`test-runner-trust` (P1):** Changes to discovery, reporting, or timeout in `hew test` must keep the runner fail-closed on parse errors and preserve stable ordering.

## What to Work On

- Check [open issues](https://github.com/hew-lang/hew/issues) for tasks labeled `good first issue` or `help wanted`
- Bug reports and fixes are always welcome
- For larger features or design changes, please open an issue first to discuss the approach

## Code Style

- **Rust:** Follow standard `rustfmt` conventions. Use `make lint-rust` for formatting checks and Clippy.
- **Commit messages:** Use imperative mood ("Add feature" not "Added feature"). Keep the first line under 72 characters.
- **Commit style:** Follow [COMMIT_STYLE.md](COMMIT_STYLE.md) for the required Conventional Commit header shape, imperative subject voice, and rationale-first body style.
- **Stdlib integer surface:** Every `pub fn` parameter and return type in `std/**/*.hew` uses an explicit-width integer (`i64`, `u64`, `isize`, etc.). The removed aliases `int`/`uint` are not valid type names. See [`docs/stdlib-style-contract.md`](docs/stdlib-style-contract.md) for the full contract and examples.

## Formatting

All code should pass the project's formatters (`rustfmt`, `clang-format`,
`taplo`, `shfmt`, `prettier`). Hooks are managed by the development environment;
linked worktrees share that configuration. Do not install replacement hooks or
set a repository-local `core.hooksPath`. Project formatting behaviour belongs
in the existing formatting scripts.

#### Pre-push gate

The pre-push hook runs `cargo fmt --all -- --check`, the tracked shell-script
lint, and `actionlint`. It is intentionally fast: its job is to catch local
format/script errors and malformed workflows before they reach review, not to
duplicate the full CI suite.

Use `make preflight` at a broad integration boundary. During implementation,
run checks that exercise the changed behaviour and `make lint` before pushing
code. Report the revision, failures and skips actually observed; a passing
subset does not establish that the full compiler or CI is green.

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

| Suite             | Command                       | Scope                                                                                                                             | Speed  |
| ----------------- | ----------------------------- | --------------------------------------------------------------------------------------------------------------------------------- | ------ |
| Rust ratchet      | `make test`                   | Rust tests compared with the known-failure set                                                                                    | medium |
| Strict Rust       | `make test-strict`            | Selected Rust tests with no known-failure allowance                                                                               | medium |
| Native acceptance | `make core-acceptance`        | Retained source/ABI cases with their specified outcomes at O0 and O2                                                              | medium |
| Native safety     | `make core-safety`            | Instrumented ownership and lifecycle cases                                                                                        | medium |
| Stdlib type-check | `make test-stdlib-ratchet`    | `std/` type-check sweep; unexpected failures are fatal                                                                            | medium |
| Compiler pipeline | `make test-compiler-pipeline` | Lexer through CLI and package consumers                                                                                           | medium |
| Runtime (no-net)  | `make test-runtime-unit`      | `hew-runtime` unit + integration tests, without QUIC/TLS/profiler stack (~3× faster compile)                                      | fast   |
| Hew test files    | `make test-hew-ratchet`       | `tests/hew/` via `hew test`, ratcheted against `scripts/hew-suite-expected-failures.txt`                                          | medium |
| Grammar parity    | `make grammar-parity`         | Vertical-slice accept fixtures, `std/**`, `examples/**` parsed with the pinned tree-sitter-hew grammar; fails on any `ERROR` node | fast   |

Use `test-runtime-unit` for no-network runtime iteration and `test-compiler-pipeline` for compiler iteration. Run `make test` before opening a PR.

### Changing Hew syntax

`hew-lexer`/`hew-parser` are the grammar authority; `tree-sitter-hew` (a
sibling repo used by editor tooling) is a mirror kept honest by `make
grammar-parity`, which parses the accepted corpus with the commit pinned in
`tools/downstream/tree-sitter.lock`. A PR that adds or changes syntax:

1. Updates `tree-sitter-hew/grammar.js` to match (a separate repo; see
   ["Downstream grammar sync"](docs/release-runbook.md#downstream-grammar-sync)
   for the full sibling chain: tree-sitter-hew, vscode-hew, vim-hew, hew.sh,
   hew.run), pushes that change, and bumps `tools/downstream/tree-sitter.lock`'s
   `commit` (and `npm` once a new package version is published) to match —
   in the same PR, not a follow-up.
2. Runs `make grammar-parity` locally to confirm the pinned commit parses
   the new syntax cleanly.
3. Runs `make downstream-check` (`scripts/sync-downstream.sh --check`),
   which reports drift against `docs/syntax-data.json` for whichever
   sibling repos are checked out next to this one.

`make test-runtime-unit` runs runtime tests with `--no-default-features` for
work that does not require QUIC, TLS or profiler features. To exercise the
default runtime features, use
`make test-strict NEXTEST_WORKSPACE_SELECTION_ARGS='-p hew-runtime'`.
Neither selection substitutes for source-level native execution when changing
compiler/runtime interactions.

`make preflight` runs the lint graph and the same Make-owned test groups as Linux CI; it is the standard manual gate before opening a PR. `make ci-preflight` remains a compatibility alias.

### E2E test workflow

When adding new language features, add an end-to-end test:

1. Create a `.hew` source file under `tests/hew/`.
2. Run it via `make test-hew-ratchet` (`hew test tests/hew/`, compared against the tracked expected-failure set).
3. **WASM parity** (see the parity principle in [`docs/internal/engineering-invariants.md`](docs/internal/engineering-invariants.md)): run the same `.hew` behaviour through native and `wasi_run_e2e` coverage wherever the target supports it. If a capability is intentionally unavailable on WASM, classify it in `wasm-capability-manifest.toml` and add a focused test proving the compiler rejects or diagnoses it before link/runtime failure.
4. Add focused type-checker tests under `hew-types/src/check/tests/` for new type rules.

For the native core, put focused executable contracts in
`tests/core-acceptance/cases/` and run them through `make core-acceptance`.
Use `make test-doc-examples` when changing guide examples or standard-library
docblocks. Do not turn an obsolete spelling or diagnostic expectation into a
permanent language restriction to satisfy an old fixture.

### WASM / native parity

New runtime behaviour — channels, ask/reply, timers, schedulers, bounded execution — must ship with native and WASM behaviour coverage wherever the target supports it. Apply the parity principle in [`docs/internal/engineering-invariants.md`](docs/internal/engineering-invariants.md):

- Exercise supported behavior through shared source corpora and WASI E2E tests. For an intentional platform limitation, add or update the typed feature disposition in `wasm-capability-manifest.toml` and prove its diagnostic with a focused negative test.
- Classify new runtime exports in `scripts/jit-symbol-classification.toml`
  and record their target disposition. The filename is historical; it does
  not imply a user-facing JIT. Source-declarable exports and compiler-private
  entry points have different contracts. Use the Make-owned ABI checks and
  describe ownership from the actual boundary, not from a symbol count.
- Add contract tests for timeout, cancel, and budget edges.
- Document intentional divergence where parity cannot land yet.
- Consult [`docs/wasm-capability-matrix.md`](docs/wasm-capability-matrix.md) for the canonical Tier 1 / Tier 2 split and the current disposition (pass / warn / reject) for each feature. The checker enforces these dispositions automatically when `--target=wasm32-wasi` is used.

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project: MIT OR Apache-2.0.
