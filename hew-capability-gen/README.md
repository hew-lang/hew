# hew-capability-gen

Generator scaffolding for the Hew WASM capability manifest.

Reads `wasm-capability-manifest.toml` at the repository root — the source of
truth for which Hew features are supported on each WASM target tier — and is
intended to emit, in subsequent stages:

- the `WasmUnsupportedFeature` enum consumed by the type checker,
- the `_WASM_CAPABILITY_*` CMake lists consumed by the codegen e2e harness,
- the `WASI_CAPABILITY` block consumed by the playground manifest generator,
- a rendered copy of `docs/wasm-capability-matrix.md`.

This initial revision only exposes the manifest shape and a row-count
integration test that locks the TOML and the prose matrix into cardinality
lock-step. Byte-exact rendering and the drift gate follow in subsequent
changes; until then, every edit to either file must keep both in step.
