# Hew

A statically-typed, actor-oriented programming language for concurrent and distributed systems.

**[Website](https://hew.sh)** | **[Documentation](https://hew.sh/docs)** | **[Playground](https://hew.sh/playground)** | **[Tutorial](https://hew.sh/learn)**

## Install

```bash
curl -fsSL https://hew.sh/install | bash
```

Pre-built binaries for Linux (x86_64, ARM64), macOS (x86_64, ARM64), FreeBSD (x86_64, ARM64), and Windows (x86_64) — plus `.deb`, `.rpm`, and Arch packages — are published on the [Releases](https://github.com/hew-lang/hew/releases) page. Also available via [Homebrew, Docker, and system packages](https://hew.sh/docs/install).

## Quick Start

```bash
# Hello world
echo 'fn main() { println("Hello from Hew!"); }' > hello.hew
hew run hello.hew

# Start a new project
hew init my_project
cd my_project
# hew init scaffolds hew.toml + main.hew + a merged .gitignore
hew check main.hew
hew fmt --check main.hew
hew doc main.hew --output-dir doc
hew run main.hew

# Interactive REPL
hew eval
```

### Evaluation & REPL

`hew eval` can run as an interactive REPL, evaluate a file in REPL context, or
evaluate a one-off inline expression. Top-level items (`fn`, `type`, `enum`,
`actor`, `impl`, `trait`) persist across REPL inputs so you can define a
function then call it later; `let`/`var` bindings and bare statements are
evaluated fresh each line and do not carry over. (Hew has no `struct`
keyword — `type Name { ... }` declares a record.)

```bash
hew eval
hew eval -f script.hew
hew eval "1 + 2"
hew eval --json -f script.hew
```

For non-interactive runs, `-f -` reads from stdin and `--target wasm32-wasi`
uses the WASI eval path.

Use `:help` inside the REPL to see the command list. Common commands include
`:help` / `:h`, `:session` / `:show`, `:items`, `:type <expr>`,
`:load <file>`, `:clear` / `:reset`, and `:quit` / `:q`.

`hew init` scaffolds a manifest-first project: `hew.toml`, a starter
`main.hew`, and a merged `.gitignore`.

For a reusable package, `hew init --lib local_dep` creates
`local_dep/local_dep.hew`. The filename matches the package name, so after a
consumer adds and installs the dependency, its root is imported directly with
`import local_dep;`.

Package names are module paths. A dotted library such as `hew.selfqualtype`
uses every segment for its installed directory and the final segment for its
root file: `hew/selfqualtype/selfqualtype.hew`, imported with
`import hew.selfqualtype;`. The generated manifest records
`main = "selfqualtype.hew"`.

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

## Sandbox VM

The sandbox VM runs admitted Hew programs in a deterministic browser-hosted runtime with a virtual clock, seeded randomness, logical heap accounting, and page-owned streams. See the public [sandbox VM divergence catalog](docs/sandbox-vm-divergences.md) for the accepted differences from native execution and the native-only APIs rejected by the sandbox profile.

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
import std.fs;
import std.encoding.json;

fn main() {
    let data = fs.read("config.json");
    let obj = json.parse(data);
    println(obj.stringify());
}
```

`json.Value` has no `Display` impl, so print it through `stringify()` rather
than passing the value straight to `println`.

See [`std/README.md`](std/README.md) for the canonical index of shipped stdlib modules.

### Multi-file programs & modules

When you compile or typecheck a multi-file program with `hew check`,
`hew compile`, or `hew run`, pass one entry `.hew` file. Imports and
directory-form modules pull in the rest, so pass `main.hew`, not every file in
the tree.

`hew doc` is different: it accepts either one `.hew` file or a directory tree
of `.hew` files to document.

- `import foo;` resolves to the directory-form module at `foo/foo.hew` or to
  `foo.hew` beside the importer — whichever exists. If **both** exist the
  import is a hard error (``import `foo` is ambiguous: both ... exist``);
  rename or remove one.
- Other top-level `.hew` files inside `foo/` merge into the same module
  automatically.
- Child directories stay separate submodules, so import them explicitly — for
  example `import foo.bar;`.
- Start with
  [`examples/directory_module_demo/README.md`](examples/directory_module_demo/README.md)
  for the smallest working layout, then
  [`examples/multifile/README.md`](examples/multifile/README.md) for selective
  imports and nested module hierarchies.

### Module search paths & stdlib discovery

Hew resolves imported modules through three tiers; the first tier that
produces a result wins and lower tiers are not consulted:

1. **Explicit override** — `HEWPATH` (colon-separated entries; each entry is
   the parent directory that contains `std/`) or `HEW_STD` (the path to the
   `std/` directory itself; Hew uses its parent as a search root). If either
   is set, only those paths are used.
2. **In-worktree development** — otherwise, Hew walks up from the source
   file (or the current directory) looking for an enclosing Hew checkout (a
   directory containing `std/builtins.hew`). This anchors a file inside one
   Hew worktree to that worktree's own `std/`, even when the binary running
   it was built in a different worktree.
3. **Installed / external project** — otherwise Hew searches, in order: the
   FHS layout beside the binary (`<prefix>/share/hew`), XDG
   (`~/.local/share/hew`), `~/.hew`, `/usr/local/share/hew`,
   `/usr/share/hew`, and a development fallback to the repo root when
   `std/` exists two levels above the binary.

`hew.toml` does not configure module search paths. Use `HEWPATH` or `HEW_STD`
when you need Hew to search a non-default stdlib or module root.

To browse shipped stdlib modules, generate docs for the stdlib tree:

```bash
hew doc std/ --output-dir doc/std
```

This writes a browsable index page for the modules under `std/`. The canonical
module list also lives in [`std/README.md`](std/README.md).

For import-resolution problems, see
[`docs/troubleshooting.md`](docs/troubleshooting.md).

### Wire Types

Wire types define versioned serialization schemas for use with actors and distributed protocols. Each field carries an explicit numeric tag (`@1`, `@2`, …) that is the field's stable identity across schema versions. You can safely add new tagged fields or rename existing ones; decoders that encounter an unknown tag skip it. **Never reuse a tag number for a different field.**

```hew
#[wire]
type UserMessage {
    name: string @1,
    age:  i32    @2,
    // Adding a new @3 field later is backwards-compatible; reusing @1 is not.
}
```

See [`examples/playground/types/wire_types.hew`](examples/playground/types/wire_types.hew) for a runnable example.

### Distributed Actors

Actors communicate across nodes with a wire-tagged message enum and `.send()`, transparently across the network. The runtime handles transport, registry gossip, and remote dispatch.

```hew
// shared by both nodes: the wire-tagged message enum, bound to the actor
#[wire]
enum CounterMsg { Increment(i64); }

actor Counter {
    var count: i64;
    receive fn handle(msg: CounterMsg) {
        match msg { CounterMsg::Increment(n) => { count = count + n; }, }
    }
}

impl ActorMsg for Counter {
    type Msg = CounterMsg;
    type Reply = ();
}

// server node
Node.set_transport("quic-mesh");
Node.load_keys("node.key");     // mints/loads this node's stable identity
Node.start("127.0.0.1:9000");
let counter = spawn Counter;
Node.register("counter", counter);

// client node (separate process)
Node.set_transport("quic-mesh");
Node.load_keys("client.key");
Node.start("127.0.0.1:9001");
Node.connect("127.0.0.1:9000");
let found: Result<RemotePid<Counter>, LookupError> = Node.lookup("counter");
match found {
    .Ok(counter) => { let _ = counter.send(CounterMsg.Increment(42)); },  // remote message
    .Err(_) => println("counter actor not found"),
}
```

`impl ActorMsg for Counter { type Msg = CounterMsg; ... }` is what makes
`RemotePid<Counter>::send` accept a `CounterMsg` — without it the remote send
does not typecheck.

See [`examples/quic_mesh/`](examples/quic_mesh/) for a complete two-process QUIC mesh demo, and [`examples/distributed_hello.hew`](examples/distributed_hello.hew) for the full key-backed identity and peer-pinning sequence.

## Architecture

The compiler is a Rust pipeline: **frontend** → **HIR/MIR** →
**codegen-rs LLVM emission**. `hew-codegen-rs` is the sole backend and is
linked into the `hew` binary as a normal Cargo dependency.

```
source.hew → Lexer → Parser → Type Checker → HIR → MIR → LLVM IR/object
               (hew-lexer) (hew-parser) (hew-types) (hew-hir/hew-mir)
                                                       │
                                                       ▼
                               hew-codegen-rs (Rust/Inkwell)
                                                       │
                                                       ▼
                               hew links object + libhew.a → executable
```

> **Detailed diagrams:** See [`docs/diagrams.md`](docs/diagrams.md) for Mermaid diagrams of the compilation pipeline, actor/supervisor state machines, runtime architecture, and wire format.

## Repository Structure

### Compiler

- **hew-cli/** — Compiler driver (`hew` binary)
- **hew-lexer/** — Tokenizer
- **hew-parser/** — Recursive-descent + Pratt precedence parser
- **hew-types/** — Bidirectional type checker with Hindley-Milner inference; warnings carry source-module attribution so diagnostics in multi-module programs identify which module triggered each warning
- **hew-hir/**, **hew-mir/** — High-level and middle-level intermediate representations of the typed program
- **hew-codegen-rs/** — LLVM-backed code generation via inkwell (the compiler backend, embedded in the `hew` binary)
- **hew-runtime/** — Pure Rust actor runtime (`libhew_runtime.a`) with node mesh networking, QUIC transport, SWIM cluster membership, and cross-node actor registry; also compiles for WASM targets
- **hew-cabi/** — C ABI bridge for stdlib FFI bindings

### Package Manager & Tooling

- **hew-pkg/** — Package-manager library behind the `hew` subcommands (init, add, install, publish, search)
- **hew-lsp/** — Language server (tower-lsp)
- **hew-observe/** — Runtime observability TUI (`hew-observe`)
- **hew-wasm/** — Analysis-only diagnostics frontend compiled to WASM (lexer/parser/type-checker for in-browser editor tooling); the full browser execution runtime is a v0.6.0 deliverable

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

- Observability guide: [`docs/observe.md`](docs/observe.md)
- Local troubleshooting guide: [`docs/troubleshooting.md`](docs/troubleshooting.md)
- Website source: **[github.com/hew-lang/hew.sh](https://github.com/hew-lang/hew.sh)**

## Building from Source

### Prerequisites

| Dependency | Version         | Purpose                                               |
| ---------- | --------------- | ----------------------------------------------------- |
| Rust       | stable (latest) | Compiler, runtime, package manager                    |
| LLVM       | 22.1            | Native/WASM object emission through inkwell/llvm-sys  |

**Install on Ubuntu/Debian:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 22 development libraries
sudo mkdir -p /etc/apt/keyrings
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
  | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
echo "deb [signed-by=/etc/apt/keyrings/llvm.asc] http://apt.llvm.org/noble/ llvm-toolchain-noble-22 main" \
  | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
sudo apt-get update
sudo apt-get install -y llvm-22-dev clang-22
```

**Install on macOS:**

```bash
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# LLVM 22 development libraries
brew install llvm

# Repository scripts and Makefile gates
brew install python@3.12
```

### Build

```bash
make          # Build everything (debug)
make release  # Build everything (optimized)
make preflight     # Unconditional, fail-fast pre-PR gate
make test     # Run Rust + native codegen tests
make lint     # cargo clippy
```

See the [Makefile](Makefile) header for all targets.

The standard pre-PR gate runs the same exhaustive static assignment as Linux CI
and fails fast after the first failed command. Reserve `make ci-preflight` for
integration and release moments, when reporting every failure is useful.

### Browser / Playground Validation

The sandbox VM (`hew-sandbox-vm`) runs admitted Hew programs in a deterministic browser-hosted runtime with a virtual clock, seeded randomness, M4–M7 actor/channel/supervision semantics, and structured-concurrency coordination. Almost all of Hew runs in a browser today; the native-only class today is features that depend on OS threads (production supervision trees, real-time network I/O). A scoped browser runtime for those thread-dependent features (channel/select/sleep/supervisor/TCP) is a ratified v0.6.0 goal. Parallel work-stealing is a permanent native-only limitation — cooperative single-threaded execution is the final shape for the browser target, not an interim state.

This repo carries the analysis-side browser tooling (`hew-wasm`) plus the sandbox bytecode emission crate (`hew-sandbox-wasm`); the downstream browser app and the `hew-sandbox-vm` TypeScript worker are in `hew-lang/playground`.

```bash
make baselines                  # regenerate every derived artefact, manifest.json included
make playground-manifest-check  # cheap freshness check for manifest.json only
make playground-check           # repo-local preflight: manifest freshness + curated analyze smoke + build hew-wasm
make playground-wasi-check      # focused manifest-driven WASI runtime preflight
```

Use `make playground-manifest-check` when you only need to confirm the checked-in manifest is current. Use `make playground-check` for the repo-local browser/tooling slice: curated `hew-wasm` analysis smoke plus the repo-local `hew-wasm` build (`make wasm`) that powers browser-side diagnostics tooling. Use `make playground-wasi-check` in codegen-capable environments when you also want the focused manifest-driven WASI runtime proof. The `hew-wasm` crate in this repo is analysis-only; the sandbox VM execution target and downstream browser app live in `hew-lang/playground`.

### Optional Dependencies

These are only needed for specific workflows:

| Dependency           | Install                                             | Purpose                                        |
| -------------------- | --------------------------------------------------- | ---------------------------------------------- |
| wasmtime             | `curl https://wasmtime.dev/install.sh -sSf \| bash` | Run the WASI end-to-end tests (`make playground-wasi-check`, and the `wasi_run_e2e` / `eval_wasm_*` cases inside `make test`) |
| wasm32-wasip1 target | `rustup target add wasm32-wasip1`                   | Build WASM runtime (`make wasm-runtime`)       |
| wasm-pack            | `cargo install wasm-pack`                           | Build browser analysis bindings (`make wasm`, `make playground-check`) |
| Python 3.12+         | system package manager                              | Required for Makefile gates and repository scripts (`scripts/`) |
| cargo-fuzz           | `cargo install cargo-fuzz`                          | Parser fuzzing (`hew-parser/fuzz/`)            |

## License

Hew is distributed under the terms of both the MIT license and the Apache License (Version 2.0).

See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE) for details.
