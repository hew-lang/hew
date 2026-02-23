# hew-cabi

C ABI helper types shared between the Hew runtime and standard library native packages.

Provides the low-level type definitions (vectors, strings, byte slices, sink ABI types) that stdlib Rust crates use to expose functions to Hew programs via `extern "C"` interfaces. This crate exists to avoid circular dependencies between `hew-runtime` and the individual `std/` packages.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
