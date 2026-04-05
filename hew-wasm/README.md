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
make playground-check
```

`make playground-check` checks `examples/playground/manifest.json` freshness and then builds the `hew-wasm` package.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
