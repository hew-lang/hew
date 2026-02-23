# Changelog

## v0.1.0 — 2026-02-22

**Hew** is a statically-typed, actor-oriented programming language for concurrent and distributed systems. It features Erlang-inspired supervision trees, first-class async/await, and message-passing concurrency — compiled to native code via MLIR and LLVM.

### Added

#### Language

- Full compilation pipeline: `.hew` → Rust frontend → MLIR → LLVM → native binary
- Core language: functions, variables (`let`/`var`), control flow (`if`/`else`, `while`, `for`, `loop`), match expressions, closures/lambdas (including mutable capture), generics, traits with vtable dispatch, tuples, string interpolation (f-strings), range expressions, `defer`
- Actors: `spawn`, `send`, `receive`, `ask`/`await`, lambda actors
- Supervision trees: `supervisor` keyword with `one_for_one`, `one_for_all`, `rest_for_one` strategies
- Fault propagation: `link`, `monitor`, `unlink`, `demonitor`
- Actor priorities and mailbox policies (`block`, `drop_new`, `drop_old`, `coalesce`)
- Collections: `Vec<T>`, `HashMap<K,V>`, `bytes`
- Structured concurrency with `scope` and `launch`
- Streams: `stream.channel()` returning `(Sink, Stream)` tuples, `stream.pipe()`, `for await` loops, file-backed streams with `Result` error handling

#### Compilation Targets

- x86_64-linux
- x86_64-macos
- aarch64-macos
- wasm32-wasi (single-threaded programs only)

#### Standard Library

- **Standard:** `std::fs`, `std::log`, `std::os`, `std::net`, `std::encoding::json`, `std::text::regex`, `std::process`, `std::misc::uuid`, `std::time::datetime`, `std::net::url`, `std::path`, `std::encoding::base64`, `std::encoding::hex`, `std::crypto::crypto`, `std::encoding::compress`, `std::stream`
- **Extended:** `std::net::http`, `std::crypto::jwt`, `std::encoding::yaml`, `std::encoding::toml`, `std::encoding::csv`, `std::encoding::msgpack`
- **Ecosystem (separate repo):** `db::postgres`, `db::redis`, `db::sqlite`, `misc::glob` — see [hew-lang/ecosystem](https://github.com/hew-lang/ecosystem)

#### Tooling

- **LSP**: diagnostics, completion, hover, symbols, semantic tokens
- **VS Code extension**: syntax highlighting and language support
- **REPL**: `hew eval` for interactive expression evaluation
- **Test runner**: `hew test` for `.hew` test files
- **Doc generator**: `hew doc` for generating documentation from source
- **Package manager**: `adze` — init, install, publish, search, with single-fallback registry resilience
- **Observability**: `hew-observe` TUI for live actor inspection (connects to runtime profiler endpoint)

#### Installation

- Shell installer (`curl | bash`)
- Homebrew
- Docker
- Debian, RPM, Arch, Alpine, and Nix packages
- Shell completions for bash, zsh, and fish

#### Developer Experience

- Clear, actionable compiler error messages for common mistakes

### Known Limitations

- WASM target is single-threaded: basic actors (spawn, send, ask/await) work, but supervision trees, link/monitor, and scoped concurrency are not supported
- No incremental compilation
- `unsafe` blocks are parsed but not yet enforced (treated as regular blocks)

### Getting Started

```bash
# Install Hew
curl -fsSL https://hew.sh/install.sh | bash

# Create and run your first program
echo 'fn main() { println("Hello from Hew!"); }' > hello.hew
hew run hello.hew

# Try the REPL
hew eval

# Start a new project
adze init my_project
```

Visit [hew.sh](https://hew.sh) for documentation and examples.
