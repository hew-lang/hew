# hew-wasm

WebAssembly bindings for the Hew compiler frontend.

Compiles the lexer, parser, and type checker to WebAssembly using `wasm-bindgen`, enabling browser-based tooling such as:

- Online playground with real-time diagnostics
- In-browser syntax highlighting
- Client-side type checking

## Build

From the repo root:

```sh
make wasm
```

To validate the repo-local browser/playground slice without building the downstream browser app:

```sh
make playground-manifest-check  # cheap manifest freshness check
make playground-check           # same check + build hew-wasm
```

`examples/playground/manifest.json` is the curated source of truth for the downstream browser catalog. Use `make playground-manifest-check` when you only need to confirm that manifest is current, or `make playground-check` when you also want the repo-local `hew-wasm` build.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
