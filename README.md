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
# adze init creates main.hew in the project root
cd my_project
hew run main.hew

# Interactive REPL
hew eval
```

See the [Getting Started Guide](https://hew.sh/docs/getting-started) for more.

### Language Basics

**`println` and `print` are plain function calls, not macros.**  Coming from Rust, you might reach for `println!` — Hew has no macro system; these are ordinary built-in functions that are auto-imported into every file:

```hew
fn main() {
    print("hello ");     // no trailing newline
    println("world");    // appends newline
    println(42);         // works with any type that implements Display
}
```

To use modules beyond the builtins, add an `import` statement at the top of your file:

```hew
import std::fs;
import std::encoding::json;

fn main() {
    let data = fs::read_to_string("config.json");
    let obj = json::parse(data);
    println(obj);
}
```

See [`std/README.md`](std/README.md) for the full module list.

### Distributed Actors

Actors communicate across nodes with the same syntax used locally. The runtime handles transport, registry gossip, and remote dispatch transparently.

```hew
// server node
Node::set_transport("quic");
Node::start("127.0.0.1:9000");
let counter = spawn Counter;
Node::register("counter", counter);

// client node (separate process)
Node::set_transport("quic");
Node::start("127.0.0.1:9001");
Node::connect("127.0.0.1:9000");
let counter: Counter = Node::lookup("counter");
counter.increment(42);   // remote message — same syntax as local
```

See [`examples/quic_mesh/`](examples/quic_mesh/) for a complete two-process QUIC mesh demo.

## Architecture

The compiler has three layers: **Rust frontend** → **embedded MLIR middle layer** → **LLVM backend**.

```
source.hew → Lexer → Parser → Type Checker → MessagePack Serialize
               (hew-lexer) (hew-parser) (hew-types)    (hew-serialize)
                                                              │
                                         ▼ in-process C API
               hew (Rust + C++):  hew_codegen_compile_msgpack
                                         │
                                         ▼
                                  MLIRGen → Hew dialect → LLVM dialect → LLVM IR → .o
               hew (Rust):        cc .o + libhew.a → executable
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
- **hew-runtime/** — Pure Rust actor runtime (`libhew_runtime.a`) with node mesh networking, QUIC transport, SWIM cluster membership, and cross-node actor registry; also compiles for WASM targets
- **hew-cabi/** — C ABI bridge for stdlib FFI bindings

### Package Manager & Tooling

- **adze-cli/** — Package manager (`adze` binary) — init, install, publish, search
- **hew-lsp/** — Language server (tower-lsp)
- **hew-observe/** — Runtime observability TUI (`hew-observe`)
- **hew-wasm/** — Frontend compiled to WASM for in-browser diagnostics

### Standard Library & Build Support

- **std/** — Standard library modules (`.hew` source files + Rust FFI crates)

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
| LLVM          | 22.1                    | MLIR code generation and LLVM backend       |
| MLIR          | (bundled with LLVM 22)  | Hew dialect and lowering passes             |
| CMake         | >= 3.20                 | Builds the embedded C++ MLIR backend        |
| Ninja         | any                     | CMake build generator                       |
| clang/clang++ | any (LLVM 22 preferred) | C/C++ compilation of the MLIR backend       |

**Install on Ubuntu/Debian:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 22 + MLIR
sudo mkdir -p /etc/apt/keyrings
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
  | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
echo "deb [signed-by=/etc/apt/keyrings/llvm.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-22 main" \
  | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
sudo apt-get update
sudo apt-get install -y cmake ninja-build \
  llvm-22-dev libmlir-22-dev mlir-22-tools clang-22
```

**Install on macOS:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 22 + MLIR + build tools
brew install llvm ninja cmake
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
