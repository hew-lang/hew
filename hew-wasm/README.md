# hew-wasm

WebAssembly bindings for the Hew compiler frontend's analysis-only surface.

Compiles the lexer, parser, and type checker to WebAssembly using
`wasm-bindgen`. This browser-facing surface is analysis-only: it provides
diagnostics and editor/playground feedback, but it does not include native
codegen, linking, runtime execution, or full Hew program execution in the
browser.

It powers browser-based tooling such as:

- Online playground with real-time diagnostics
- In-browser syntax highlighting
- Client-side type checking

## Build

From the repo root:

```sh
make wasm
```

To validate the repo-local browser/playground analysis slice without building
the downstream browser app:

```sh
make playground-manifest-check  # cheap manifest freshness check
make playground-check           # manifest freshness + curated analyze smoke + build hew-wasm
```

`examples/playground/manifest.json` is the curated source of truth for the
downstream browser catalog. Use `make playground-manifest-check` when you only
need to confirm that manifest is current, or `make playground-check` when you
also want the curated `analyze()` smoke over every manifest entry plus the
repo-local `hew-wasm` build that backs browser-side analysis.

CI protects this surface with the dedicated `playground-wasm-build` lane. That
lane intentionally stays repo-local and runs only:

```sh
cargo test -p hew-wasm --lib
make playground-check
```

It covers manifest drift, curated analysis-only smoke, and `hew-wasm` build
breakage for browser tooling, but it does not build downstream browser apps or
claim browser/runtime/codegen execution coverage.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
