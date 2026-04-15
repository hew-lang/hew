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
# adze init creates hew.toml, main.hew, and .gitignore
hew check main.hew
hew run main.hew

# Interactive REPL
hew eval
```

### Evaluation & REPL

`hew eval` can run as an interactive REPL, evaluate a file in REPL context, or
evaluate a one-off inline expression. The REPL remembers top-level items and
`let`/`var` bindings across inputs.

```bash
hew eval
hew eval -f script.hew
hew eval "1 + 2"
hew eval --json -f script.hew
```

For non-interactive runs, `-f -` reads from stdin and `--target wasm32-wasi`
uses the WASI eval path.

Use `:help` inside the REPL to see the command list. Common commands include
`:help` / `:h`, `:session` / `:show`, `:items`, `:bindings`, `:type <expr>`,
`:load <file>`, `:clear` / `:reset`, and `:quit` / `:q`.

Need a lighter source-only scaffold instead? `hew init my_project` writes
`main.hew` + `README.md`, but no `hew.toml`.

See the [Getting Started Guide](https://hew.sh/docs/getting-started) for more.

### Learning Paths

The [`examples/`](examples/) directory contains structured learning paths for new users:

- **[`examples/ux/`](examples/ux/)** — 15 quick-start lessons (hello world through hashmaps), each paired with an `.expected` output file; ideal for a first 20-minute tour
- **[`examples/progressive/`](examples/progressive/)** — 11 numbered lessons building from variables to actors, also with `.expected` files
- **[`examples/playground/`](examples/playground/)** — Topic-grouped snippets covering basics, concurrency, and types, with checked-in metadata in [`manifest.json`](examples/playground/manifest.json)

See [`examples/README.md`](examples/README.md) for the complete directory guide.
If you're looking specifically for multi-file/module layouts, start with
[`examples/directory_module_demo/README.md`](examples/directory_module_demo/README.md)
and then [`examples/multifile/README.md`](examples/multifile/README.md).

When you move from language lessons to library APIs, use [`std/README.md`](std/README.md), the canonical index of shipped stdlib modules.

### Language Basics

**`println` and `print` are plain function calls, not macros.**  Coming from Rust, you might reach for `println!` — in Hew these are ordinary built-in functions written without a `!` suffix, auto-imported into every file:

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
    let data = fs.read("config.json");
    let obj = json.parse(data);
    println(obj);
}
```

See [`std/README.md`](std/README.md) for the canonical index of shipped stdlib modules.

### Multi-file programs & modules

When you compile or typecheck a multi-file program with `hew check`,
`hew build`, or `hew run`, pass one entry `.hew` file. Imports and
directory-form modules pull in the rest, so pass `main.hew`, not every file in
the tree.

- `import foo;` prefers the directory-form module at `foo/foo.hew`, then falls
  back to `foo.hew` beside the importer.
- Other top-level `.hew` files inside `foo/` merge into the same module
  automatically.
- Child directories stay separate submodules, so import them explicitly — for
  example `import foo::bar;`.
- Start with
  [`examples/directory_module_demo/README.md`](examples/directory_module_demo/README.md)
  for the smallest working layout, then
  [`examples/multifile/README.md`](examples/multifile/README.md) for selective
  imports and nested module hierarchies.

For the current wildcard-import warning caveat, see
[`docs/troubleshooting.md`](docs/troubleshooting.md).

### Wire Types

Wire types define versioned serialization schemas for use with actors and distributed protocols. Each field carries an explicit numeric tag (`@1`, `@2`, …) that is the field's stable identity across schema versions. You can safely add new tagged fields or rename existing ones; decoders that encounter an unknown tag skip it. **Never reuse a tag number for a different field.**

```hew
wire type UserMessage {
    name: String @1;
    age:  i32    @2;
    // Adding a new @3 field later is backwards-compatible; reusing @1 is not.
}
```

See [`examples/playground/types/wire_types.hew`](examples/playground/types/wire_types.hew) for a runnable example.

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
- **hew-types/** — Bidirectional type checker with Hindley-Milner inference; warnings carry source-module attribution so diagnostics in multi-module programs identify which module triggered each warning
- **hew-serialize/** — MessagePack AST serialization
- **hew-codegen/** — MLIR middle layer + LLVM backend (Hew dialect ops, lowering, code generation); built as a C++ library and **embedded inside the `hew` binary** — not a separately shipped executable
- **hew-astgen/** — Generates C++ msgpack deserialization from AST definitions
- **hew-runtime/** — Pure Rust actor runtime (`libhew_runtime.a`) with node mesh networking, QUIC transport, SWIM cluster membership, and cross-node actor registry; also compiles for WASM targets
- **hew-cabi/** — C ABI bridge for stdlib FFI bindings

### Package Manager & Tooling

- **adze-cli/** — Package manager (`adze` binary) — init, install, publish, search
- **hew-lsp/** — Language server (tower-lsp)
- **hew-observe/** — Runtime observability TUI (`hew-observe`)
- **hew-wasm/** — Analysis-only frontend compiled to WASM for in-browser diagnostics and editor tooling (not browser runtime/codegen/execution)

### Standard Library & Build Support

- **std/** — Standard library modules (`.hew` source files + Rust FFI crates)

### Distribution

- **editors/** — Editor support (Emacs, Nano, Sublime)
- **installers/** — Package installers (Homebrew, Debian, RPM, Arch, Alpine, Nix, Docker) plus install-time shell completion generation
- **examples/** — Example programs and benchmarks
- **scripts/** — Development scripts
- **docs/** — Language specification and API references

## Documentation

Full documentation at **[hew.sh/docs](https://hew.sh/docs)**

- Local troubleshooting guide: [`docs/troubleshooting.md`](docs/troubleshooting.md)
- Website source: **[github.com/hew-lang/hew.sh](https://github.com/hew-lang/hew.sh)**

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
make ci-preflight  # Dispatch a conservative local preflight from your current diff
make test     # Run Rust + native codegen tests
make lint     # cargo clippy
```

See the [Makefile](Makefile) header for all targets.

Use `make ci-preflight ARGS="--dry-run"` to inspect the selected commands before
running them. The first slice stays conservative: known docs/parser/types/CLI
diffs get narrower checks, and everything else falls back to broader local
preflight commands.

### Browser / Playground Validation

This repo does not build the downstream browser app or a full in-browser Hew
runtime. The repo-local browser/playground slice here is analysis-only tooling:
`hew-wasm` plus the curated
[`examples/playground/manifest.json`](examples/playground/manifest.json)
consumed by downstream browser tooling.

```bash
make playground-manifest        # regenerate examples/playground/manifest.json
make playground-manifest-check  # cheap freshness check for manifest.json only
make playground-check           # repo-local preflight: manifest freshness + curated analyze smoke + build hew-wasm
make playground-wasi-check      # focused manifest-driven WASI runtime preflight
```

Use `make playground-manifest-check` when you only need to confirm the checked-in manifest is current. Use `make playground-check` for the repo-local browser/tooling slice: curated `hew-wasm` analysis smoke plus the repo-local `hew-wasm` build (`make wasm`) that powers browser-side analysis tooling. Use `make playground-wasi-check` in codegen-capable environments when you also want the focused manifest-driven WASI runtime proof. Browser coverage remains analysis-only; this repo does not claim downstream browser execution exists.

### Optional Dependencies

These are only needed for specific workflows:

| Dependency           | Install                                             | Purpose                                        |
| -------------------- | --------------------------------------------------- | ---------------------------------------------- |
| wasmtime             | `curl https://wasmtime.dev/install.sh -sSf \| bash` | Run WASM tests (`make test-wasm`)              |
| wasm32-wasip1 target | `rustup target add wasm32-wasip1`                   | Build WASM runtime (`make wasm-runtime`)       |
| wasm-pack            | `cargo install wasm-pack`                           | Build browser analysis bindings (`make wasm`, `make playground-check`) |
| Python 3             | system package manager                              | Playground manifest + other scripts (`scripts/`) |
| Java 21 + ANTLR4     | system package manager                              | Grammar validation (`make grammar`)            |
| cargo-fuzz           | `cargo install cargo-fuzz`                          | Parser fuzzing (`hew-parser/fuzz/`)            |

## License

Hew is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
