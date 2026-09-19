# hew-wasm

WebAssembly bindings for Hew source analysis and verified sandbox compilation.

Compiles the shared frontend and SIR package exporter to WebAssembly using
`wasm-bindgen`. Browser execution uses the accompanying TypeScript sandbox VM.

It powers browser-based tooling such as:

- Online playground with real-time diagnostics
- In-browser syntax highlighting
- Client-side type checking
- AST, formatter, symbol, hover, and inlay-hint support for editor integrations

## Browser API surface

The package exports JSON-string APIs for the current developer-tooling surface:

- `parse_source(source)` returns `{ ast, diagnostics }`.
- `type_check(source)` returns `{ diagnostics, type_info }`, where `type_info`
  contains byte-span keyed, user-facing resolved types for hover/editor UI.
- `format_source(source)` returns `{ formatted, diagnostics }`; `formatted` is
  `null` when fatal parser diagnostics are present.
- `analyze(source)` returns `{ diagnostics, tokens, symbols, type_info }`.
- Editor helpers include `hover`, `complete`, `document_symbols`,
  `semantic_tokens`, `folding_ranges`, `signature_help`, `inlay_hints`,
  `goto_definition`, `find_references`, `prepare_rename`, `rename`,
  `code_actions`, and `get_keywords`.

Diagnostics include stable `phase`, `kind`, nested `span`, `message`, `notes`,
and `suggestions` fields. The legacy `start_offset` / `end_offset` fields remain
for existing browser bridges.

## Compilation and execution

`compileToSandboxBytecode(source, "sandbox-vm-export")` returns a JSON string
containing `{ diagnostics, bytecode }`. The shared `hew_compile::Session`
frontend checks the source and builds verified ownership SIR. The package is a
projection of that SIR, including ownership transfers, cleanup edges, actor
protocols and suspension points. The historical target argument is retained for
browser callers; it does not select a separate source-language profile.

Execute the package with `runBytecode` from `@hew-lang/sandbox-vm`. Its loader
admits the package's declared operations before running any instruction. A
refusal distinguishes `native_only`, `not_implemented` and `invalid_package`.
The package carries `hew_version` and `compiler_version` for consumer provenance.

`@hew-lang/wasm` replaces the separate `@hew-lang/sandbox-wasm` package and
exports both editor analysis and `compileToSandboxBytecode`. The VM package
remains separate. These browser packages do not contain native LLVM codegen.

## Build and validation

From the repository root:

```sh
make npm-packages       # stage compiler and VM packages and execute their smoke
make sandbox-parity    # compare source execution with native Hew
make playground-check  # browser analysis and Wasm build
```

Packages are staged under `target/npm/@hew-lang/{wasm,sandbox-vm}`. The compiler
entry is `wasm.js` with its accompanying `wasm_bg.wasm`. Use the same workspace
version for compiler and VM. Set `HEW_NPM_STAGE_ROOT` to build and smoke-test a
separate candidate directory without replacing an existing consumer stage.

For WASI execution (`hew build --target=wasm32-wasi`), consult the separate
[WASM capability matrix](../docs/wasm-capability-matrix.md).

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
