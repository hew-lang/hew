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

## Capability tier

`hew-wasm` is the **Tier 1** WASM surface: `wasm32-unknown-unknown` compiled
via `wasm-bindgen` for browser consumption.  It exposes the analysis pipeline
(lex / parse / typecheck) only.

For **Tier 2** (WASI execution via `hew build --target=wasm32-wasi`), see
[`docs/wasm-capability-matrix.md`](../docs/wasm-capability-matrix.md) for the
authoritative feature disposition table, including which runtime features are
currently rejected at compile time (channels, timers, streams) and which emit
diagnostic warnings (supervision trees, structured concurrency, link/monitor).

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
make playground-wasi-check      # focused manifest-driven WASI runtime preflight
```

`examples/playground/manifest.json` is the curated source of truth for the
downstream browser catalog. Use `make playground-manifest-check` when you only
need to confirm that manifest is current, `make playground-check` when you also
want the curated `analyze()` smoke over every manifest entry plus the repo-local
`hew-wasm` build that backs browser-side analysis, or
`make playground-wasi-check` in codegen-capable environments when you want the
same manifest capability truth proven against the real WASI runtime path.

Each manifest entry carries a `capabilities` object:

```jsonc
{
  "capabilities": {
    "browser": "analysis-only",  // invariant — Tier 1 is analysis-only
    "wasi":    "runnable"        // or "unsupported"
  }
}
```

`browser: "analysis-only"` is invariant for all curated entries: `hew-wasm`
exposes lex/parse/typecheck only; no in-browser execution exists.  The `wasi`
value reflects whether the example runs correctly under
`hew build --target=wasm32-wasi` (`"runnable"`) or triggers a known WASM32
diagnostic path (`"unsupported"`).  See
[`docs/wasm-capability-matrix.md § Playground capability contract`](../docs/wasm-capability-matrix.md#playground-capability-contract)
for the full per-entry table and the rationale behind each disposition.

The `cargo test -p hew-wasm --lib curated_playground_manifest_smoke` step
verifies that every manifest entry carries well-formed capability metadata in
addition to running the `analyze()` smoke. `make playground-wasi-check` runs
the focused `hew-cli/tests/wasi_run_e2e.rs` curated-manifest preflight so
`capabilities.wasi` is proven against the real WASI runtime path in
codegen-capable environments rather than against `hew-wasm` alone.

CI protects this surface with the dedicated `playground-wasm-build` lane. That
lane intentionally stays repo-local and runs only:

```sh
cargo test -p hew-wasm --lib
make playground-check
```

It covers manifest drift, curated analysis-only smoke, and `hew-wasm` build
breakage for browser tooling, but it does not build downstream browser apps or
claim browser execution exists. The focused WASI proof remains available via
`make playground-wasi-check` where embedded codegen is present.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
