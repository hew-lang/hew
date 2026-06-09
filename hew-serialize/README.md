# hew-serialize

AST serialization for the Hew compiler.

Serializes the type-checked AST into MessagePack format for consumption by downstream Hew compiler stages (`hew-hir`, `hew-mir`, `hew-codegen-rs`). Also supports JSON output for debugging (`hew build --emit-json`).

The serialization layer enriches the AST with resolved type information, mangled function names, and vtable metadata needed by the backend.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain. It feeds the Rust IR ladder.
