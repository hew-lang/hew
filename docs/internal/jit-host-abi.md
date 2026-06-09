# JIT host ABI classification

JIT session dylibs must only see the stable and codegen-stable Hew host ABI.
Runtime lifecycle and other process-global hooks stay out of the JIT session
allow-list so a JIT-compiled module cannot reinitialize, drain, or tear down
shared runtime state.

## Source of truth

The source of truth lives in `scripts/jit-symbol-classification.toml`.

We use an out-of-band allow-list instead of inline annotations because the
same reviewed classification file feeds three consumers:

- `scripts/verify-ffi-symbols.py --classify …`
- the required CI lint gate (`make verify-ffi`, which runs `--classify stable --validate`)
- the stable runtime-symbol set consumed by `hew-mir::runtime_symbols` and
  codegen-rs JIT/runtime lowering

That keeps the ABI review surface centralized while still failing closed: the
verifier rejects any `#[no_mangle] extern "C" fn` in `hew-runtime/src/` that
is missing from the file or classified more than once.

## Three-tier model

### `stable`

Handle-oriented, user-visible runtime operations that `extern "rt"` declarations
in Hew source code may name, and that JIT hosts must expose in their symbol map.
The type-checker enforces this boundary: any symbol named in an `extern "rt"`
block that is not in `stable` is a hard compile-time error.

### `codegen-stable`

Symbols the Hew compiler emits into generated LLVM IR for non-trivial actor
programs. Examples: cooperate safepoints (`hew_actor_cooperate`), task-scope
wiring (`hew_task_scope_set_current`), actor-state locking
(`hew_actor_state_lock_acquire` / `_release`), execution-context access
(`hew_require_execution_context`), and scheduler bootstrap (`hew_sched_init`).

These symbols are **NOT** user-callable via `extern "rt"`: the checker rejects
any attempt. They can only appear in compiler-emitted IR. JIT hosts must
provide them alongside the `stable` tier — a JIT host that loads only `stable`
will fail to link any non-trivial actor program.

### `internal`

Lifecycle, session/global-state, scheduler shutdown/reset/drain, and
conservative runtime-control hooks. These are **never JIT-reachable** — not
by user code and not by compiler-emitted IR. They are AOT-linkable only.

When a runtime export is ambiguous, prefer `internal` first and promote it
later with an explicit review.

## Classification decision flowchart

```
Is this symbol named by user extern "rt" blocks?
  Yes → stable
  No  →
    Is this symbol emitted by the Hew compiler into IR?
      Yes → codegen-stable
      No  → internal
```

## JIT host requirements

A compliant JIT host **must** expose `stable ∪ codegen-stable`. The `internal`
tier must never appear in a JIT session symbol map.
