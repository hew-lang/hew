# Hew Sandbox WASM

`hew-sandbox-wasm` is the browser-facing compile-and-export crate for the
educational sandbox VM. It is intentionally separate from `hew-wasm`, whose
Tier 1 contract remains analysis-only.

## Capability tier

This crate defines the **sandbox-vm-export** browser tier. Callers provide Hew
source, the crate runs parsing, type checking, sandbox profile admission, and
bytecode package emission, then returns a `hew.sandbox.bytecode.v0` package when
all compile-time gates succeed.

The public API accepts the human-facing profile alias `sandbox-vm-export`.
Serialized bytecode records the schema-valid canonical profile
`sandbox.sandbox-vm-export.v0`.

## API

Rust callers use:

```rust
compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
```

Browser callers use the wasm-bindgen export `compileToSandboxBytecode(source,
profile)`, which returns a JSON-encoded `CompileOutput`.

Expected parse, type-check, and profile failures are reported as diagnostics in
`CompileOutput` with `bytecode: null`. The API reserves `CompileError`/thrown
JavaScript errors for internal serialization failures.

## Fail-closed profile gate

The sandbox profile is explicit allowlist based:

- learner code, user-defined functions, records, enums, pattern matching,
  arithmetic, strings, vectors, and deterministic literals are admitted;
- shimmed browser symbols such as stdout and regex are recorded as sandbox
  symbols/capabilities;
- host filesystem, networking, process, native FFI, OS environment, and
  real-clock APIs are rejected before bytecode emission;
- unknown stdlib modules, unknown native symbols, extern declarations, unsafe
  blocks, and unresolved profile decisions reject by default.
