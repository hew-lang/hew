# hew-serialize

AST serialization for the Hew compiler.

Serializes the type-checked AST into MessagePack format for consumption by the C++ MLIR code generator (`hew-codegen`). Also supports JSON output for debugging (`hew build --emit-json`).

The serialization layer enriches the AST with resolved type information, mangled function names, and vtable metadata needed by the backend.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain. It bridges the Rust frontend and the C++ MLIR backend.
