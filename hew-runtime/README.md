# hew-runtime

The Hew actor runtime library (`libhew_runtime.a`).

Every compiled Hew program links against this runtime, which provides:

- **Actor scheduler** — work-stealing, M:N threading for lightweight actors
- **Arena allocator** — fast bump allocation for actor-local memory
- **Stream I/O** — file streams, byte streams, channels, and pipe operations
- **String operations** — UTF-8 string primitives exposed via C ABI
- **Networking** — TCP/UDP sockets, HTTP server primitives
- **Concurrency** — mailboxes, actor groups, supervisors

## Build targets

```sh
# Native static library
cargo build -p hew-runtime

# WebAssembly (for browser/WASI targets)
cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features
```

## C ABI

All public functions use `#[no_mangle] pub extern "C"` calling convention so they can be called from LLVM-generated code. The runtime's C interface is the contract between the MLIR code generator and the Rust runtime.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
