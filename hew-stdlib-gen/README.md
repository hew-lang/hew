# hew-stdlib-gen

Build tool that auto-generates Hew standard library stubs from `#[hew_export]` metadata.

Reads the compiled export metadata from Rust stdlib crates and generates:

- `.hew` stub files (the Hew-visible API declarations in `std/`)
- Type checker entries for `hew-types/src/stdlib.rs`

This ensures the Hew-language view of the standard library stays in sync with the actual Rust implementations.

## Usage

```sh
cargo run -p hew-stdlib-gen
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
