# hew-capability-gen

Typed generator for the repository-root
`wasm-capability-manifest.toml` authority.

The manifest is the sole editable source of checker Reject/Warn feature
identity, native-only stdlib module classification, stable Reject/Warn and
backlog capability IDs, and curated playground WASI exclusions. The generator emits:

- `hew-types/src/wasm_capabilities_generated.rs`, consumed by the checker,
  sandbox module gate, and codegen exclusion diagnostics;
- `examples/playground/wasm-capabilities.json`, consumed by
  `scripts/gen-playground-manifest.py`;
- the feature-policy and current WASI summary tables in
  `docs/wasm-capability-matrix.md`. The latter combines typed unsupported rows
  with runnable truth from `examples/playground/manifest.json`.

Generate or verify the checked-in consumers with:

```sh
cargo run -p hew-capability-gen
cargo run -p hew-capability-gen -- --check
```

`--check` is byte-exact. The generator tests mutate every output plus omitted,
unknown, duplicate, and disposition-mismatched checker variants to prove the
green gate can fail.

Only non-runnable playground decisions are declarative. An example absent from
`[[playground_wasi]]` is not accepted merely because the manifest says pass;
`hew-cli/tests/wasi_run_e2e.rs` must compile it, execute it under WASI, and
compare its real output.
