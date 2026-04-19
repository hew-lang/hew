# JIT host ABI classification

JIT session dylibs must only see the stable Hew host ABI. Runtime lifecycle and other process-global hooks stay out of the JIT session allow-list so a JIT-compiled module cannot reinitialize, drain, or tear down shared runtime state.

## Source of truth

The source of truth lives in `scripts/jit-symbol-classification.toml`.

We use an out-of-band allow-list instead of inline annotations because the same reviewed classification file feeds three consumers:

- `scripts/verify-ffi-symbols.py --classify …`
- the CI validation gate (`--classify stable --validate`)
- the generated `hew-codegen` stable-symbol header consumed by `HewJitSymbolMap`

That keeps the ABI review surface centralized while still failing closed: the verifier rejects any `#[no_mangle] extern "C" fn` in `hew-runtime/src/` that is missing from the file or classified more than once.

## Classification rule

- `stable`: handle-oriented, user-visible runtime operations that JIT modules may call directly
- `internal`: lifecycle, scheduler bootstrap, shutdown/reset/drain, session/global-state, and other conservative runtime-control hooks that must not be injected into JIT session symbol maps

When a runtime export is ambiguous, prefer `internal` first and promote it later with an explicit review.
