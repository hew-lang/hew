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

## What to Work On

- Check [open issues](https://github.com/hew-lang/hew/issues) for tasks labeled `good first issue` or `help wanted`
- Bug reports and fixes are always welcome
- For larger features or design changes, please open an issue first to discuss the approach

## Code Style

- **Rust:** Follow standard `rustfmt` conventions. Run `cargo clippy --workspace` before submitting.
- **C++:** LLVM style (see `hew-codegen/.clang-format`). Use C++20 features where appropriate.
- **Commit messages:** Use imperative mood ("Add feature" not "Added feature"). Keep the first line under 72 characters.

## Build System

Always use `make` targets instead of running `cargo`, `cmake`, or `ctest` directly. See the [Makefile](Makefile) header for all available targets.

## Testing

- `make test` runs Rust workspace tests and native codegen E2E tests
- `make test-wasm` runs WASM E2E tests (requires wasmtime)
- `make test-rust` runs only Rust tests
- `make test-codegen` runs only codegen/E2E tests

When adding new language features, add E2E tests in `hew-codegen/tests/examples/` and type checker tests in `hew-types/src/check.rs`.

## License

By contributing, you agree that your contributions will be licensed under the same terms as the project: MIT OR Apache-2.0.
