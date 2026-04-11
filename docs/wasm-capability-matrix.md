# Hew WASM Capability Matrix

This document is the **authoritative** source for which Hew features are
available on each WASM target tier.  It is referenced by the type checker
(`hew-types/src/check/types.rs :: WasmUnsupportedFeature`), the runtime stubs
(`hew-runtime/src/lib.rs :: wasm_stubs`), and the spec
(`docs/specs/HEW-SPEC.md §8.0`).

When a feature is listed as **Reject** or **Warn**, the type checker enforces
that disposition at compile time before any code reaches LLVM/WASM codegen.

---

## Target tiers

| Tier | Crate / Target | Use case |
|------|----------------|----------|
| **Tier 1** | `hew-wasm` compiled to `wasm32-unknown-unknown` via `wasm-bindgen` | Browser playground, editor analysis, in-browser type checking |
| **Tier 2** | `hew-runtime` compiled to `wasm32-wasip1` (formerly `wasm32-wasi`) | WASI execution — `hew build --target=wasm32-wasi` |

**Tier 1** is analysis-only: lexer, parser, and type checker only.  It never
executes Hew programs; it only provides diagnostics.

**Tier 2** is a genuine execution runtime on top of the WASI ABI.  It uses a
single-threaded cooperative actor scheduler and provides a meaningful subset of
the native runtime capabilities.

---

## Feature disposition table

The **Checker disposition** column documents what the type checker emits when
`enable_wasm_target()` is set (i.e., when compiling for Tier 2).

| Feature | Checker disposition | Runtime status | Tracking |
|---------|-------------------|----------------|----------|
| Basic actors (`spawn`, `send`, `receive`, `ask/await`) | ✅ Pass | Implemented | — |
| Generators / async streams | ✅ Pass | Implemented | — |
| Pattern matching, ADTs, generics | ✅ Pass | Implemented | — |
| Standard collections, arithmetic | ✅ Pass | Implemented | — |
| Actor ask/reply (`reply_channel_wasm`) | ✅ Pass | Implemented | — |
| WASI socket I/O (HTTP/TCP clients) | ✅ Pass | Implemented via WASI | — |
| `select {}` (literal timeout, any arm count) | ✅ Pass | Implemented | — |
| `select {}` (computed timeout expression) | ⚠️ Warn (`Select`) | Diagnostic path | WASM-TODO |
| Supervision trees (`supervisor`, `supervisor_child`, `supervisor_stop`) | ⚠️ Warn (`SupervisionTrees`) | Diagnostic path | WASM-TODO |
| Actor `link` / `unlink` / `monitor` / `demonitor` | ⚠️ Warn (`LinkMonitor`) | Diagnostic path | WASM-TODO |
| Structured concurrency (`scope {}`, `scope.launch`, `scope.await`) | ⚠️ Warn (`StructuredConcurrency`) | Diagnostic path | WASM-TODO |
| Scope-spawned `Task` handles | ⚠️ Warn (`Tasks`) | Diagnostic path | WASM-TODO |
| **`channel.new`, `Sender<T>::*`, `Receiver<T>::*`** | 🚫 Error (`Channels`) | `unreachable!()` trap | WASM-TODO |
| **`sleep_ms`, `sleep`** | 🚫 Error (`Timers`) | Silent no-op shim | WASM-TODO |
| **`stream.*` constructors, `Stream<T>::*` methods** | 🚫 Error (`Streams`) | Module not compiled | WASM-TODO |
| Generators on WASM | ✅ Pass (basic syntax) | Cooperative scheduler | Note below |

---

## Disposition rationale

### ⚠️ Warn (warning-level)

Features in the **Warn** group are architecturally incompatible with the
single-threaded cooperative WASM scheduler, but they have a controlled
diagnostic path: the type checker emits a `PlatformLimitation` warning, and
codegen produces grouped diagnostics if they reach lowering.

These exist as warnings (not errors) to allow gradual migration: a program can
be partially WASM-compatible and still get useful analysis feedback.

### 🚫 Error (compile-time reject)

Features in the **Error** group are rejected at compile time because their
runtime stubs are **silent traps** or **silent no-ops**:

- **Channels**: All `hew_channel_*` C symbols call `unreachable!()` on wasm32
  (see `hew-runtime/src/lib.rs :: wasm_stubs`).  A WASM program that calls
  `channel.new` compiles but traps immediately at runtime with an unhelpful
  `unreachable` instruction.  Making this a compile-time error gives a
  descriptive diagnostic at the right time.
  - WASM-TODO: implement single-threaded channel queues backed by the actor
    mailbox infrastructure.

- **Timers** (`sleep_ms`, `sleep`): The wasm32 shim for `hew_sleep_ms` returns
  immediately (intentional noop, see `wasm_stubs`).  Code that expects a delay
  silently runs without it.  Making this an error forces callers to use the
  host-driven reschedule model instead.
  - WASM-TODO: integrate with `wasi::clock_time_get(CLOCK_MONOTONIC)` or
    `setTimeout` for host-driven rescheduling.

- **Streams**: The `stream` runtime module is entirely gated out on wasm32
  (`#[cfg(not(target_arch = "wasm32"))]` in `hew-runtime/src/lib.rs`).  Any
  call to a stream constructor or stream method would produce a linker error or
  undefined symbol.  Rejecting at compile time gives a clear diagnostic.
  - WASM-TODO: implement I/O stream adapters over WASI fd/socket APIs.

---

## Generators on WASM — note

The spec previously implied generators might be unsupported on WASM.  As of the
current implementation, **basic generator syntax and the cooperative scheduler
do work on Tier 2**.  Generators that depend on blocking I/O (e.g. a generator
that calls `stream.next()` internally) will encounter the Streams reject above
at the point of the stream call, not at the generator declaration itself.

If a generator is purely computational (no I/O, no blocking calls), it works
on WASM unchanged.

---

## Checker enforcement

The `WasmUnsupportedFeature` enum in `hew-types/src/check/types.rs` is the
single source of truth for feature labels and rejection reasons.  The
`warn_wasm_limitation` and `reject_wasm_feature` methods in
`hew-types/src/check/diagnostics.rs` implement the two disposition levels.

The distinction:

```
warn_wasm_limitation  → Severity::Warning  → self.warnings
reject_wasm_feature   → Severity::Error    → self.errors
```

**Warn group** is wired in:
- `hew-types/src/check/expressions.rs :: maybe_warn_wasm_expr` (scope/select/tasks)
- `hew-types/src/check/calls.rs :: warn_if_wasm_incompatible_call` (link/monitor/supervisor)
- `hew-types/src/check/registration.rs` (supervisor actor declarations)

**Reject group** is wired in:
- `hew-types/src/check/calls.rs :: warn_if_wasm_incompatible_call` (sleep_ms, sleep → Timers)
- `hew-types/src/check/methods.rs :: check_method_call` (channel.* → Channels, stream.* → Streams)
- `hew-types/src/check/methods.rs` Sender/Receiver match arms (Channels)
- `hew-types/src/check/methods.rs` Stream match arm (Streams)

---

## WASM-TODO backlog

These gaps are explicitly deferred and tracked here:

| Gap | Blocker | Tracking label |
|-----|---------|----------------|
| Single-threaded MPSC channel queues | Actor mailbox integration | `WASM-TODO: channels` |
| Host-driven timer rescheduling | WASI `clock_time_get` / `setTimeout` | `WASM-TODO: timers` |
| I/O stream adapters | WASI fd/socket APIs | `WASM-TODO: streams` |
| `select {}` with computed timeouts | Dynamic timeout lowering for WASM select/ask paths | `WASM-TODO: select` |
| Supervision tree restart strategies | OS-thread-free supervision design | `WASM-TODO: supervision` |
| Actor link/monitor fault propagation | OS-thread-free exit propagation | `WASM-TODO: link-monitor` |
| Structured concurrency scopes | Thread-free scope scheduler | `WASM-TODO: scope` |

---

## Playground capability contract

`examples/playground/manifest.json` carries a `capabilities` object on every
entry that maps the feature disposition from this table into a machine-readable
form consumed by browser/playground tooling and the WASI e2e test suite.

```jsonc
{
  "capabilities": {
    "browser": "analysis-only",  // always — Tier 1 is analysis-only
    "wasi":    "runnable"        // or "unsupported" — see table below
  }
}
```

### Capability values

| Field | Value | Meaning |
|-------|-------|---------|
| `browser` | `"analysis-only"` | The entry can be analysed (lex/parse/typecheck) in-browser via `hew-wasm`, but no in-browser program execution is available yet. This value is invariant for all curated entries. |
| `wasi` | `"runnable"` | The example compiles and executes correctly under `hew build --target=wasm32-wasi` (Tier 2). |
| `wasi` | `"unsupported"` | The example triggers a Warn or Reject disposition in the type checker when targeting WASM32 (see the feature table above). It will produce a non-zero exit code or a diagnostic under WASI. |

### Current WASI capability summary

| Example | `capabilities.wasi` | Reason |
|---------|---------------------|--------|
| `basics/*` (4 entries) | `runnable` | No WASM-limited features |
| `concurrency/actor_pipeline` | `runnable` | Basic actors supported |
| `concurrency/async_await` | `runnable` | Async/await supported |
| `concurrency/counter_actor` | `runnable` | Basic actors supported |
| `concurrency/supervisor` | `unsupported` | Uses `supervisor`/`supervisor_child` → Warn disposition |
| `types/*` (3 entries) | `runnable` | No WASM-limited features |

The `WASI_CAPABILITY` table in `scripts/gen-playground-manifest.py` is the
single source of truth for these per-entry values.  Entries absent from that
table default to `"runnable"`.  The `--check` mode of that script validates
that every checked-in manifest entry carries well-formed capability metadata.

The WASI e2e test (`hew-cli/tests/wasi_run_e2e.rs`) reads `capabilities.wasi`
from the manifest to determine which examples to run vs. which to verify on the
diagnostic path, eliminating hard-coded runnable example IDs from the test
logic. Run it via `make playground-wasi-check` in codegen-capable environments
to prove the curated playground contract against the real WASI runtime path
without implying any in-browser runtime.
