# Hew WASM Capability Matrix

This document is the **authoritative** source for which Hew features are
available on each WASM target tier.  It is referenced by the type checker
(`hew-types/src/check/types.rs :: WasmUnsupportedFeature`), the runtime stubs
(`hew-runtime/src/lib.rs :: wasm_stubs`), and the spec
(`docs/specs/HEW-SPEC-2026.md §8.0`).

When a feature is listed as **Reject** or **Warn**, the type checker enforces
that disposition at compile time before any code reaches LLVM/WASM codegen.

---

## Target tiers

| Tier | Crate / Target | Use case |
|------|----------------|----------|
| **Tier 1** | `hew-wasm` compiled to `wasm32-unknown-unknown` via `wasm-bindgen` | Browser playground, editor analysis, in-browser type checking |
| **sandbox-vm-export** | `hew-sandbox-wasm` compiled to `wasm32-unknown-unknown` via `wasm-bindgen` | Browser sandbox bytecode export for the educational VM |
| **sandbox-vm** | `hew-sandbox-vm` TypeScript worker | Browser educational sandbox execution for deterministic sequential bytecode |
| **Tier 2** | `hew-runtime` compiled to `wasm32-wasip1` (formerly `wasm32-wasi`) | WASI execution — `hew build --target=wasm32-wasi` |

**Tier 1** is analysis-only: lexer, parser, and type checker only.  It never
executes Hew programs; it only provides diagnostics.

**sandbox-vm-export** sits alongside Tier 1 rather than replacing it.  The
`hew-sandbox-wasm` crate runs parse, type-check, explicit sandbox profile
admission, and deterministic bytecode package emission for browser callers.  It
does not execute Hew programs.

**sandbox-vm** executes admitted sandbox bytecode in a Web Worker.  The current
interpreter covers deterministic sequential code, checked arithmetic, records,
enums, lowered match dispatch, direct monomorphized calls, strings, vectors, an
educational JavaScript `RegExp` subset for curated regex fixtures, the M4
single-threaded actor scheduler (`spawn`, `send`, `receive`, root `ask/reply`,
actor crash hooks, bounded mailboxes, seeded chaos scheduling, and replay via
trace inputs), and the M5 educational coordination subset: bounded in-memory
channels, in-memory stream/sink/duplex handles layered on those channels,
async task spawn/await, structured scopes with cancellation observation at await
and channel boundaries, deterministic `select`, virtual-time timer arms, and
the M6 educational failure-philosophy subset: declarative supervisor specs,
visible child slots, deterministic one-for-one / one-for-all / rest-for-one
restart decisions, virtual-time restart windows, linked exit messages, monitor
notifications, and `#[on(crash)]` observation before supervisor decisions.
M7 adds a machine-readable sandbox stdlib profile, conservative pure/page-I/O
shims, virtual-clock `time.now` / `time.sleep` / `time.deadline` shims, typed
fail-closed diagnostics for unsupported stdlib symbols, and a DOM-free
playground JSON contract for diagnostics, trace views, controls, share links,
lesson virtual files, and trace export.
This is intentionally reduced sandbox semantics rather than production runtime
parity: link-to-dead traps fail closed, monitor-to-dead fires immediately, and
supervisor restart budget exhaustion escalates through typed runtime failures.
Machine runtime parity, file-backed streams, network-backed streams, and broader
host I/O remain fail-closed with structured `unsupported_instruction` runtime
failures for the post-M7/native-runtime milestones.

**Tier 2** is a genuine execution runtime on top of the WASI ABI.  It uses a
single-threaded cooperative actor scheduler and provides a meaningful subset of
the native runtime capabilities.

For CLI eval, `hew eval --target wasm32-wasi <expr>` and
`hew eval --target wasm32-wasi -f <file>` run through Tier 2. Interactive REPL
mode (`hew eval --target wasm32-wasi` with no file or expression) is rejected,
and `--json` is supported only for those non-interactive eval modes.

---

## Feature disposition table

The **Checker disposition** column documents what the type checker emits when
`enable_wasm_target()` is set (i.e., when compiling for Tier 2), or
`WASM-TODO` when a surface is documented as unresolved / not yet checker-gated.

| Feature | Checker disposition | Runtime status | Tracking |
|---------|-------------------|----------------|----------|
| Basic actors (`spawn`, `send`, `receive`, `ask/await`) | ✅ Pass | Implemented | — |
| Generators (`gen fn`) | ✅ Pass (not checker-gated — Tier 2 has no dedicated `WasmUnsupportedFeature` guard) | Scalar-parameter and fn-typed-parameter `gen fn` forms execute and tear down correctly on Tier 2 via the unified `llvm.coro` switched-resume substrate (identical IR to native) | Note below |
| Pattern matching, ADTs, generics | ✅ Pass | Implemented | — |
| Arithmetic and wasm-safe collection surfaces | ✅ Pass | Implemented | — |
| Layout-backed `HashMap` / `HashSet` | ✅ Pass | Supported on Tier 2; descriptor ABI uses target-width layout fields and descriptor hook pointers are value-correct under wasmtime | #1820 |
| Actor ask/reply (`reply_channel_wasm`) | ✅ Pass | Implemented | — |
| Raw WASI socket capability (host-provided, no stable Hew stdlib surface yet) | ⚠️ WASM-TODO (not checker-gated) | Host-/runtime-dependent; Hew does not yet expose a supported cross-target socket layer | WASM-TODO |
| `select {}` (any timeout expression, any arm count) | ✅ Pass | Implemented | — |
| Supervision trees (`supervisor`, `supervisor_child`, `supervisor_stop`) | 🚫 Error (`SupervisionTrees`) | Educational sandbox subset implements deterministic restart trees; native runtime parity remains gated | M6 |
| Actor `link` / `unlink` / `monitor` / `demonitor` | 🚫 Error (`LinkMonitor`) | Educational sandbox subset implements deterministic graph state, exit signals, and monitor notifications; native runtime parity remains gated | M6 |
| Structured concurrency (`scope {}`, `scope.launch`, `scope.await`) | 🚫 Error (`StructuredConcurrency`) | Native thread/condvar task runtime only; wasm32 has no cooperative task work queue or join | WASM-TODO |
| Scope-spawned `Task` handles | 🚫 Error (`Tasks`) | Task spawn is thread-based and no cooperative task executor drives forked bodies on wasm32 | WASM-TODO |
| **`channel.new`, `Sender<T>::send/clone/close`, `Receiver<T>::try_recv/close`** | ✅ Pass | Bounded non-blocking slice implemented; `send` traps on full queue | v0.3.2 |
| **`Receiver<T>::recv`, `for await item in rx` over `Receiver<T>`** | 🚫 Error (`BlockingChannelRecv`) | `unreachable!()` trap | WASM-TODO |
| **`semaphore.new`, `Semaphore::try_acquire/release/count/free`** | ✅ Pass | Non-blocking semaphore subset only | — |
| **`Semaphore::acquire`, `Semaphore::acquire_timeout`** | 🚫 Error (`BlockingSemaphoreAcquire`) | No cooperative blocking wait implementation | WASM-TODO |
| **`sleep`, `sleep_until`** | ⚠️ Warn (`Timers`) | Cooperative park at message boundary | Implemented |
| **`#[every(duration)]` periodic handlers** | ⚠️ Warn (`Timers`) | Cooperative periodic dispatch via host-driven timer queue | Implemented |
| **`stream.*` constructors, `Stream<T>::*` methods** | 🚫 Error (`Streams`) | Module not compiled | WASM-TODO |
| **`std::net::http::http_client.*`, `http_client.Response.*`** | 🚫 Error (`HttpClient`) | Native-only wrapper module | WASM-TODO |
| **`std::net::smtp.*`, `smtp.Conn.*`** | 🚫 Error (`Smtp`) | Native-only transport wrapper | WASM-TODO |
| **`http.listen`, `http.Server.*`, `http.Request.*`** | 🚫 Error (`HttpServer`) | Native-only runtime module | WASM-TODO |
| **`net.listen`, `net.connect`, `net.*`, `net.Listener.*`, `net.Connection.*`** | 🚫 Error (`TcpNetworking`) | Native-only runtime module | WASM-TODO |
| **`process.run`, `process.start`, `process.*`, `process.Child.*`** | 🚫 Error (`ProcessExecution`) | Native-only runtime module | WASM-TODO |
| **`std::net::tls.connect/read/write/close`, `tls.TlsStream.*`** | 🚫 Error (`Tls`) | Native TLS-over-TCP stack today; no documented wasm32 path | WASM-TODO |
| **`std::net::quic.*`, `quic.QUICEndpoint.*`, `quic.QUICConnection.*`, `quic.QUICStream.*`, `quic.QUICEvent.*`** | 🚫 Error (`Quic`) | `quic_transport` is feature-gated and not compiled for wasm32 | WASM-TODO |
| **`std::net::dns.resolve`, `dns.lookup_host`** | 🚫 Error (`Dns`) | Native OS resolver; not compiled for wasm32 | WASM-TODO |
| **`std::os.*`** | 🚫 Error (`OsEnv`) | Hew OS/env helpers are native-only today even where WASI may offer host data | WASM-TODO |
| **`Node::*`, `RemotePid<T>::send` / `::ask`, remote monitor/link operations** | 🚫 Error (`Distributed`) | Key-derived identity, durable sessions, authenticated mesh routing, registry/SWIM, and cross-node lifecycle are native-only | WASM-TODO |
| **`std::crypto::crypto.random_bytes`** | 🚫 Error (`CryptoRandom`) | Secure entropy source is native-only; fail-closed rejection until wasm32 cryptographic entropy exists | WASM-TODO |

---

## Disposition rationale

### ⚠️ Warn (warning-level)

Features in the **Warn** group are architecturally different on the
single-threaded cooperative WASM scheduler, but they still have a meaningful
runtime implementation.

These exist as warnings (not errors) to allow gradual migration: a program can
be partially WASM-compatible and still get useful analysis feedback.

This group includes `sleep`/`sleep_until` and `#[every(duration)]`, which now
have cooperative semantics on WASM: `sleep` parks the actor at the
**message boundary** (not mid-handler), while periodic handlers are delivered
when the host advances the timer queue.  The warning reminds callers that code
after `sleep` in the same receive handler still executes before the actor
parks, and that periodic dispatch depends on `hew_wasm_timer_tick` /
`hew_wasm_sched_tick` driving the queue.

### 🚫 Error (compile-time reject)

Features in the **Error** group are rejected at compile time because wasm32 has
no coherent runtime support for them and allowing them through the checker
would otherwise end in a trap or linker failure:

- **Supervision trees / link-monitor / structured concurrency / tasks**: the
  production runtime modules are gated behind
  `#[cfg(not(target_arch = "wasm32"))]` in `hew-runtime/src/lib.rs`.  The
  sandbox VM separately teaches reduced M6 supervision/link/monitor semantics
  with deterministic virtual time and typed fail-closed traps; Tier 2 still
  rejects the production surface at check time to avoid undefined-symbol linker
  failures.

  **Release decision:** Tier 2 `scope {}` / `Task` support is deferred from
  v0.6.0-rc1 to a later v0.6.x. LLVM lowering is shared across native and
  wasm32, but every `SpawnTask*` site still emits
  `hew_task_spawn_thread_with_inherited_context`; completion and `await task`
  use the native thread/condvar plus `hew_read_slot_*` wakeup path. The wasm32
  scheduler has an actor run queue, but no task work queue, task continuation
  driver, or non-blocking scope join. A synchronous shim is not parity: it
  erases concurrency and can deadlock task/actor coordination, so the compiler
  continues to reject the surface before LLVM/linking.

- **Channels (bounded subset)**: `channel.new`, sender clone/close,
  `Receiver::try_recv`, and typed `send` are available on wasm32 via the
  single-threaded queue in `hew-runtime/src/channel_wasm.rs`.
  `try_recv` preserves the native ABI contract (`None` on both empty and
  closed), while `send` fails closed by trapping with an explicit message when
  the bounded queue is full rather than silently dropping or spin-polling.

- **Blocking channel recv**: `Receiver<T>::recv`, `recv_int`, and `for await`
  over `Receiver<T>` still trap on wasm32 because the cooperative scheduler
  does not yet yield and resume when a channel is empty but still live. The
  checker rejects these operations at compile time with `BlockingChannelRecv`.

- **Blocking semaphore acquire**: `Semaphore::acquire` and
  `Semaphore::acquire_timeout` can block waiting for a permit. Native builds do
  that with condvar-style parking, but wasm32 has no cooperative permit-wait
  path yet. The checker rejects only those blocking methods and still allows
  the non-blocking semaphore subset (`new`, `try_acquire`, `release`, `count`,
  `free`).

- **Timers** (`sleep`, `sleep_until`, `#[every(duration)]`): The runtime now
  parks sleeping actors at the message boundary, re-enqueues them once the
  deadline passes, and delivers periodic handler messages through the same
  host-driven timer loop.  The checker emits a **warning** (not an error) to
  inform callers of the cooperative semantics difference.  Code after
  `sleep` in the same receive handler still executes before the park, and
  periodic dispatch depends on `hew_wasm_timer_tick(now_ms)` /
  `hew_wasm_sched_tick(...)` advancing the timer queue.

- **Streams**: The `stream` runtime module is entirely gated out on wasm32
  (`#[cfg(not(target_arch = "wasm32"))]` in `hew-runtime/src/lib.rs`).  Any
  call to a stream constructor or stream method would produce a linker error or
  undefined symbol.  Rejecting at compile time gives a clear diagnostic.
  - WASM-TODO: implement I/O stream adapters over WASI fd/socket APIs.

- **`std::net::http::http_client` / `std::net::smtp`**: these stdlib wrappers
  are still native-only today (`WASM-TODO` in the module sources).  Letting
  them through type checking on wasm32 only defers the failure to link time.
  The checker now rejects both the module helper calls and their handle methods
  (`http_client.Response.*`, `smtp.Conn.*`) with feature-specific diagnostics.
  This is distinct from any raw WASI socket support a host runtime may expose:
  Hew does not currently treat those low-level capabilities as a supported,
  portable stdlib networking surface.

- **HTTP server**: The `std::net::http` server surface is backed by native
  sockets and `tiny_http`. Its runtime symbols are unavailable on wasm32, so
  `http.listen` plus `http.Server` / `http.Request` methods are rejected at
  compile time instead of failing later in codegen or at link time.
  - WASM-TODO: design a cooperative WASI-hosted HTTP server surface.

- **TCP networking**: The `std::net` listener/connection runtime lives in the
  native transport module, which is gated out on wasm32. Rejecting `net.*`
  constructors and `net.Listener` / `net.Connection` methods keeps the checker
  fail-closed instead of leaking to undefined-symbol linker failures.
  - WASM-TODO: expose socket-backed listener/connection adapters over WASI.

- **Process execution**: The `std::process` module depends on the host OS
  process model, and its runtime module is not compiled for wasm32. Rejecting
  `process.*` helpers and `process.Child` methods at check time gives a
  feature-specific diagnostic rather than a native-symbol failure downstream.
  - WASM-TODO: define a host capability model for subprocess execution.

- **Key-backed distributed identity and remote actors**: The `Node::*` cluster
  API, `RemotePid<T>::send` / `RemotePid<T>::ask`, registry/SWIM, durable
  session fencing, and cross-node monitor/link delivery lower to the native
  authenticated mesh runtime. That runtime is absent from the wasm32 link set.
  The checker rejects the whole distributed surface with `Distributed`, so
  wasm32 never receives a success-shaped networking shim. Basic single-process
  actors (`spawn` / local `send` / local `ask`) remain available.
  - WASM-TODO: define a wasm peer transport and identity/session authority
    before exposing any distributed surface.

- **`std::crypto::crypto.random_bytes`**: Secure randomness is backed by
  `ring::SystemRandom`, which is native-only and absent from the wasm32 link set.
  The checker rejects `crypto.random_bytes` on wasm32 so key material generation
  fails closed instead of compiling to a non-cryptographic fallback or host
  import. WASM-TODO: plumb a secure host entropy capability such as WASI
  `random_get`.

### ⚠️ WASM-TODO (documented gap / not yet checker-gated)

Rows whose **Checker disposition** is **WASM-TODO** are intentionally *not*
claimed as supported. They remain in the matrix because the current wasm32
story is either unresolved or native-only, but Hew does not yet emit a
dedicated checker warning/error for them.

- **Raw WASI socket capability**: some WASI hosts may expose socket APIs, but
  Hew does not yet provide a stable socket abstraction that spans browser Tier 1,
  WASI Tier 2, and native builds. That is why the matrix rejects Hew's current
  high-level networking modules while keeping raw host socket capability in the
  backlog instead of marking it ✅ Pass.

---

## Generators on WASM — note

`gen fn` lowers onto the unified `llvm.coro` switched-resume continuation
substrate (`hew-runtime/src/cont.rs`), which emits identical IR on native and
wasm32. The wasm32 backend synthesizes its own `hew_gen_coro_destroy` that
routes coro-frame teardown through `llvm.coro.destroy`, so construction,
`.next()` consumption, and scope-exit teardown all release exactly one frame —
verified end-to-end under `wasmtime`
(`hew-codegen-rs/tests/exec/wasm_generator_exec.rs`).

Generator forms not yet implemented on any target are tracked in
HEW-FUTURE.md §1.6 — that is a language-surface gap, not a WASM-specific one.

Generators that depend on blocking I/O (e.g. a generator that calls
`stream.next()` internally) are additionally covered by the Streams reject
above at the point of the stream call.

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
- `hew-types/src/check/calls.rs :: reject_if_wasm_incompatible_call` (`sleep`/`sleep_until` → `Timers` warning arm)

**Reject group** is wired in:
- `hew-types/src/check/expressions.rs :: reject_if_wasm_incompatible_expr` (scope/tasks)
- `hew-types/src/check/calls.rs :: reject_if_wasm_incompatible_call` (link/monitor/supervisor/`random_bytes`/`Node::*`)
- `hew-types/src/check/registration.rs` (supervisor actor declarations)
- `hew-types/src/check/methods.rs :: check_method_call` (stream.* / `http_client.*` / `smtp.*` / http.* / net.* / process.* / tls.* / quic.* / dns.* / os.* / `crypto.random_bytes` module calls)
- `hew-types/src/check/methods.rs` Receiver match arm (`recv` → `BlockingChannelRecv`)
- `hew-types/src/check/methods.rs` semaphore handle gate (`acquire` / `acquire_timeout` → `BlockingSemaphoreAcquire`)
- `hew-types/src/check/methods.rs` Stream / http.Server / http.Request / net.Listener / net.Connection / process.Child / tls.TlsStream / quic.QUIC* handle match arms
- `hew-types/src/check/methods.rs` RemotePid match arm (`send` / `ask` → `Distributed`)

Rows marked **WASM-TODO (not checker-gated)** currently have no dedicated
`WasmUnsupportedFeature` guard point. As of main, that bucket includes raw WASI
socket capability only. `std::net::tls`, `std::net::quic`, `std::net::dns`,
`std::os`, and `std::crypto::crypto.random_bytes` are all checker-gated.

---

## WASM-TODO backlog

These gaps are explicitly deferred and tracked here:

| Gap | Blocker | Tracking label |
|-----|---------|----------------|
| Blocking channel recv / full-queue backpressure parity | Cooperative-scheduler recv yield/resume + send backpressure beyond the bounded fail-closed slice in `channel_wasm.rs` | `WASM-TODO: channels` |
| Blocking semaphore acquire parity | Cooperative permit wait / timeout semantics for `Semaphore::acquire*` on wasm32 | `WASM-TODO: semaphore` |
| Raw socket-backed Hew networking surface | Stable cross-host socket abstraction above host-provided WASI sockets | `WASM-TODO: wasi-sockets` |
| I/O stream adapters | WASI fd/socket APIs | `WASM-TODO: streams` |
| HTTP server parity | Cooperative WASI-hosted request accept/respond runtime | `WASM-TODO: http-server` |
| TCP listener / connection parity | WASI socket-backed accept/read/write abstractions | `WASM-TODO: tcp-networking` |
| TLS client parity | wasm-capable TLS-over-sockets design plus checker/runtime classification | `WASM-TODO: tls` |
| QUIC parity | wasm-capable UDP/QUIC transport plus feature-gated runtime support | `WASM-TODO: quic` |
| DNS resolver parity | WASI-backed resolver shim; current native OS resolver is not compiled for wasm32 | `WASM-TODO: dns` |
| `std::os` parity | WASI-backed args/env/path/system shims for the current stdlib surface | `WASM-TODO: os` |
| `crypto.random_bytes` parity | Secure wasm32 entropy source and explicit capability classification | `WASM-TODO: crypto-random` |
| Process execution parity | Explicit host capability model for subprocesses | `WASM-TODO: process-execution` |
| Key-backed distributed identity, remote-actor messaging, and lifecycle parity | wasm peer transport plus authenticated identity/session, registry, SWIM, full-Location routing, and cross-node monitor/link authorities | `WASM-TODO: distributed` |
| Supervision tree restart strategies | OS-thread-free supervision design | `WASM-TODO: supervision` |
| Actor link/monitor fault propagation | OS-thread-free exit propagation | `WASM-TODO: link-monitor` |
| Structured concurrency scopes | Cooperative task work queue integrated with `hew_sched_run`, `TaskEntry` continuation/resume, completion wakeups, and non-blocking scope joins | `WASM-TODO: scope` |

> **Stackless suspension substrate (R326/R327, W6.007).** The shared LLVM
> `llvm.coro.*` switched-resume carrier is built, and native `await task` now
> emits a suspend point. That does not by itself provide wasm32 task scopes:
> task spawn still targets the native thread entry point, task readiness still
> wakes through the native-only `hew_read_slot_*` path, and `hew_sched_run` has
> no task work queue to drive. The gate can relax only when those three pieces
> and non-blocking scope join are wired together; relaxing it around a
> synchronous stub would be fail-open.

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
| `browser` | `"analysis-only"` | The entry can be analysed (lex/parse/typecheck) in-browser via `hew-wasm`. This value tracks the `hew-wasm` analysis crate and is invariant for all curated entries in this manifest; the sandbox VM execution target is tracked separately in `hew-lang/playground`. |
| `wasi` | `"runnable"` | The example compiles and executes correctly under `hew build --target=wasm32-wasi` (Tier 2). |
| `wasi` | `"unsupported"` | The example uses a Warn / Reject surface or relies on a documented `WASM-TODO` capability that Hew does not currently treat as runnable on WASM32. |

### Current WASI capability summary

| Example | `capabilities.wasi` | Reason |
|---------|---------------------|--------|
| `basics/*` (4 entries) | `runnable` | No WASM-limited features |
| `concurrency/actor_pipeline` | `unsupported` | Actor runtime ABI unavailable on WASI (#1821) |
| `concurrency/async_await` | `unsupported` | Actor runtime ABI unavailable on WASI (#1821) |
| `concurrency/counter_actor` | `unsupported` | Actor runtime ABI unavailable on WASI (#1821) |
| `concurrency/supervisor` | `unsupported` | Uses `supervisor`/`supervisor_child` → Reject disposition |
| `types/collections`, `types/pattern_matching`, `types/structural_bounds` | `runnable` | No WASM-limited features |
| `types/wire_types` | `unsupported` | Wire enum codec not yet lowered on WASI (#1822) |

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
