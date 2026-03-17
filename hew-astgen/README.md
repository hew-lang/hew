# hew-astgen

Build tool that auto-generates the C++ MessagePack deserialization code from the Rust AST definitions.

Reads `hew-parser/src/ast.rs`, parses the Rust type definitions using `syn`, and generates `msgpack_reader.cpp` for the C++ MLIR code generator. This keeps the Rust AST and C++ reader in sync automatically, eliminating a class of bugs from hand-maintaining the C++ deserialization code.

## Usage

```sh
make astgen
# or:
cargo run -q -p hew-astgen -- \
  --ast hew-parser/src/ast.rs \
  --module hew-parser/src/module.rs \
  --output hew-codegen/src/msgpack_reader.cpp
```

`cargo test -p hew-astgen` verifies that the checked-in
`hew-codegen/src/msgpack_reader.cpp` matches the generator output.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
