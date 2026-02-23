# hew-astgen

Build tool that auto-generates the C++ MessagePack deserialization code from the Rust AST definitions.

Reads `hew-parser/src/ast.rs`, parses the Rust type definitions using `syn`, and generates `msgpack_reader.cpp` for the C++ MLIR code generator. This keeps the Rust AST and C++ reader in sync automatically, eliminating a class of bugs from hand-maintaining the C++ deserialization code.

## Usage

```sh
cargo run -p hew-astgen
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
