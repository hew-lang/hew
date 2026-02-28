# Hew

A statically-typed, actor-oriented programming language for concurrent and distributed systems.

**[Website](https://hew.sh)** | **[Documentation](https://hew.sh/docs)** | **[Playground](https://hew.sh/playground)** | **[Tutorial](https://hew.sh/learn)**

## Install

```bash
curl -fsSL https://hew.sh/install.sh | bash
```

Pre-built binaries for Linux (x86_64) and macOS (x86_64, ARM) are available on the [Releases](https://github.com/hew-lang/hew/releases) page. Also available via [Homebrew, Docker, and system packages](https://hew.sh/docs/install).

## Quick Start

```bash
# Hello world
echo 'fn main() { println("Hello from Hew!"); }' > hello.hew
hew run hello.hew

# Start a new project
adze init my_project
cd my_project
hew run src/main.hew

# Interactive REPL
hew eval
```

See the [Getting Started Guide](https://hew.sh/docs/getting-started) for more.

## Architecture

The compiler has three layers: **Rust frontend** → **MLIR middle layer** → **LLVM backend**.

```
source.hew → Lexer → Parser → Type Checker → MessagePack Serialize
               (hew-lexer) (hew-parser) (hew-types)    (hew-serialize)
                                                             │
                                        ┌────────────────────┘
                                        ▼ stdin (MessagePack AST)
               hew-codegen (C++): MLIRGen → Hew dialect → LLVM dialect → LLVM IR → .o
               hew (Rust):        cc .o + libhew_runtime.a → executable
```

> **Detailed diagrams:** See [`docs/diagrams.md`](docs/diagrams.md) for Mermaid sequence diagrams, state machines, and architecture visuals covering the full compilation pipeline, MLIR lowering stages, actor lifecycle, message flow, runtime layers, and wire protocol format.

## Repository Structure

### Compiler

- **hew-cli/** — Compiler driver (`hew` binary)
- **hew-lexer/** — Tokenizer
- **hew-parser/** — Recursive-descent + Pratt precedence parser
- **hew-types/** — Bidirectional type checker with Hindley-Milner inference
- **hew-serialize/** — MessagePack AST serialization
- **hew-codegen/** — MLIR middle layer + LLVM backend (Hew dialect ops, lowering, code generation)
- **hew-astgen/** — Generates C++ msgpack deserialization from AST definitions
- **hew-runtime/** — Pure Rust actor runtime (`libhew_runtime.a`); also compiles for WASM targets
- **hew-cabi/** — C ABI bridge for stdlib FFI bindings

### Package Manager & Tooling

- **adze-cli/** — Package manager (`adze` binary) — init, install, publish, search
- **hew-lsp/** — Language server (tower-lsp)
- **hew-observe/** — Runtime observability TUI (`hew-observe`)
- **hew-wasm/** — Frontend compiled to WASM for in-browser diagnostics

### Standard Library & Build Support

- **std/** — Standard library modules (`.hew` source files + Rust FFI crates)
- **hew-export-macro/** — Proc macro for stdlib export declarations
- **hew-export-types/** — Shared types for the export system
- **hew-stdlib-gen/** — Generates stdlib module descriptors for the type checker

### Distribution

- **editors/** — Editor support (Emacs, Nano, Sublime)
- **completions/** — Shell completions (bash, zsh, fish)
- **installers/** — Package installers (Homebrew, Debian, RPM, Arch, Alpine, Nix, Docker)
- **examples/** — Example programs and benchmarks
- **scripts/** — Development scripts
- **docs/** — Language specification and API references

## Documentation

Full documentation at **[hew.sh/docs](https://hew.sh/docs)**

Website source: **[github.com/hew-lang/hew.sh](https://github.com/hew-lang/hew.sh)**

## Building from Source

### Prerequisites

| Dependency    | Version                 | Purpose                                     |
| ------------- | ----------------------- | ------------------------------------------- |
| Rust          | stable (latest)         | Frontend compiler, runtime, package manager |
| LLVM          | 21.1                    | MLIR code generation and LLVM backend       |
| MLIR          | (bundled with LLVM 21)  | Hew dialect and lowering passes             |
| CMake         | >= 3.20                 | Builds hew-codegen (C++ MLIR backend)       |
| Ninja         | any                     | CMake build generator                       |
| clang/clang++ | any (LLVM 21 preferred) | C/C++ compilation of hew-codegen            |

**Install on Ubuntu/Debian:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 21 + MLIR
sudo mkdir -p /etc/apt/keyrings
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
  | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
echo "deb [signed-by=/etc/apt/keyrings/llvm.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-21 main" \
  | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
sudo apt-get update
sudo apt-get install -y cmake ninja-build \
  llvm-21-dev libmlir-21-dev mlir-21-tools clang-21
```

**Install on macOS:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 21 + MLIR
brew install llvm@21 ninja cmake
```

### Build

```bash
make          # Build everything (debug)
make release  # Build everything (optimized)
make test     # Run Rust + native codegen tests
make lint     # cargo clippy
```

See `make help` or the [Makefile](Makefile) header for all targets.

### Optional Dependencies

These are only needed for specific workflows:

| Dependency           | Install                                             | Purpose                                        |
| -------------------- | --------------------------------------------------- | ---------------------------------------------- |
| wasmtime             | `curl https://wasmtime.dev/install.sh -sSf \| bash` | Run WASM tests (`make test-wasm`)              |
| wasm32-wasip1 target | `rustup target add wasm32-wasip1`                   | Build WASM runtime (`make wasm-runtime`)       |
| Python 3             | system package manager                              | Visualization and fuzzing scripts (`scripts/`) |
| Java 21 + ANTLR4     | system package manager                              | Grammar validation (`make grammar`)            |
| cargo-fuzz           | `cargo install cargo-fuzz`                          | Parser fuzzing (`hew-parser/fuzz/`)            |

## License

Hew is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
