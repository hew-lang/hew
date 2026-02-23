# hew-wasm

WebAssembly bindings for the Hew compiler frontend.

Compiles the lexer, parser, and type checker to WebAssembly using `wasm-bindgen`, enabling browser-based tooling such as:

- Online playground with real-time diagnostics
- In-browser syntax highlighting
- Client-side type checking

## Build

```sh
wasm-pack build hew-wasm --target web
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
