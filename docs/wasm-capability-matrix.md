# Hew WASM Capability Matrix

Use this matrix to choose between browser analysis, browser sandbox execution,
and WASI execution. The feature table is generated from
`wasm-capability-manifest.toml`, the same data the type checker uses for WASI
warnings and refusals.

When a feature is listed as **Reject** or **Warn**, the type checker enforces
that disposition at compile time before any code reaches LLVM/WASM codegen.

---

## Target tiers

| Tier | Crate / Target | Use case |
|------|----------------|----------|
| **Tier 1** | `hew-wasm` compiled to `wasm32-unknown-unknown` via `wasm-bindgen` | Browser source analysis and verified SIR package compilation |
| **sandbox-vm** | `hew-sandbox-vm` TypeScript worker | Deterministic execution of verified SIR packages |
| **Tier 2** | `hew-runtime` compiled to `wasm32-wasip1` (formerly `wasm32-wasi`) | WASI execution — `hew build --target=wasm32-wasi` |

**Tier 1** exposes editor APIs and `compileToSandboxBytecode` in the canonical
`@hew-lang/wasm` package. It runs the shared frontend through verified ownership
SIR and projects the result into the VM package.

**sandbox-vm** loads that package and checks its declared operations against
implemented runtime shims before executing any instruction. Actors, tasks,
selection and pipes resume through explicit SIR edges on a deterministic
scheduler. Native host I/O is rejected with `native_only`; missing VM operations
use `not_implemented`; malformed packages use `invalid_package`. Native/VM
parity is established by source execution tests, not matching refusals.

**Tier 2** is a genuine execution runtime on top of the WASI ABI.  It uses a
single-threaded cooperative actor scheduler and provides a meaningful subset of
the native runtime capabilities.

Native `hew eval` compiles each submission ahead of time and executes a child
process. Hew supports native AOT, WASI runtime execution, and browser sandbox
execution.

For CLI eval, `hew eval --target wasm32-wasi <expr>` and
`hew eval --target wasm32-wasi -f <file>` run through Tier 2. Interactive REPL
mode (`hew eval --target wasm32-wasi` with no file or expression) is rejected,
and `--json` is supported only for those non-interactive eval modes.

---

## Feature disposition table

The **Checker disposition** column documents what the type checker emits when
compiling for Tier 2. An unsupported capability without a dedicated diagnostic
is identified directly; its runtime-status cell states the current behavior.

| ID | Feature surface | Diagnostic label | Checker disposition | Diagnostic reason | Runtime status |
|----|-----------------|------------------|---------------------|-------------------|----------------|
| `basic-actors` | Basic actors (`spawn`, `send`, `receive`, `ask/await`) | Basic actors (`spawn`, `send`, `receive`, `ask/await`) | Pass | — | Implemented on the wasm32 process driver: the same actor core the native target runs, with the parked root draining the run queue in place of worker threads |
| `exit-status-range` | Process exit statuses above 125 | Process exit statuses above 125 | Pass | — | WASI `proc_exit` refuses a status above 125, so `exit(n)` for n greater than 125 reaches the host as 1 rather than n. Every other status matches native, including the exit-1 rule an unrecovered fault follows (HEW-SPEC-2026 5.8), whose trap code is a diagnostic tag and never the status |
| `actor-crash-containment` | Actor-local panic/trap containment and restart | Actor-local panic/trap containment and restart | Pass | — | Checked handler, init and start faults release owned state and return through failure edges; supervised faults use the shared restart and settlement policy |
| `generators` | Generators (`gen fn`) | Generators (`gen fn`) | Pass | — | Scalar-parameter and `fn[clone]`-parameter `gen fn` forms execute through checked continuation frames on Tier 2; full consumption and early-break cleanup verified under wasmtime at O0 and O2 |
| `patterns-adts-generics` | Pattern matching, ADTs, generics | Pattern matching, ADTs, generics | Pass | — | Implemented |
| `collections-arithmetic` | Standard collections, arithmetic | Standard collections, arithmetic | Pass | — | Implemented |
| `layout-hashmap-hashset` | Layout-backed `HashMap` / `HashSet` | Layout-backed `HashMap` / `HashSet` | Pass | — | Supported on Tier 2; descriptor ABI uses target-width layout fields and descriptor hook pointers are value-correct under wasmtime |
| `actor-ask-reply` | Actor ask/reply | Actor ask/reply | Pass | — | Implemented |
| `wasi-sockets` | Raw host-provided WASI socket capability | Raw host-provided WASI socket capability | Unsupported (no dedicated diagnostic) | — | Host- and runtime-dependent; Hew's stdlib has no cross-target socket layer |
| `select` | `select {}` (any arm kind, any timeout expression) | `select {}` operations | Pass | — | Checked readiness waitsets for tasks, actor calls, streams and timers on the cooperative driver |
| `supervision-trees` | Supervision trees (`supervisor`, `supervisor_child`, `supervisor_stop`) | Supervision tree operations | Pass | — | Checked child construction, restart strategies, stable roles and cooperative teardown use the shared actor runtime |
| `link-monitor` | Actor `link` / `unlink` / `monitor` / `demonitor` | Link/monitor operations | Pass | — | Local actor and role observations share typed LINK/DOWN delivery and owned subscription cleanup |
| `structured-concurrency` | Structured concurrency (`scope {}`, `fork`, `await`) | Structured concurrency scopes | Pass | — | Checked scope joins, cancellation and cleanup on the cooperative driver |
| `tasks` | Scope-spawned `Task` handles | Task handles spawned from scopes | Pass | — | Checked task continuations, ordered batches and races on the cooperative driver |
| `semaphore-non-blocking` | `semaphore.new`, `Semaphore.clone/try_acquire/release/count/close` | `semaphore.new`, `Semaphore.clone/try_acquire/release/count/close` | Pass | — | Explicitly cloned affine handles share a permit count and close independently |
| `semaphore-blocking-acquire` | `Semaphore.acquire`, `Semaphore.acquire_timeout` | `Semaphore.acquire`, `Semaphore.acquire_timeout` | Pass | — | Owned permit acquisition with cancellation and shared-wheel deadlines |
| `timers-sleep` | `sleep_ms`, `sleep` | Timer operations | Warn (`Timers`) | timers are cooperative on wasm32: a sleep parks the coroutine and an #[every(duration)] handler fires when the process driver next ticks the shared timer wheel, so granularity follows the driver steps rather than a dedicated ticker | Cooperative coroutine suspension, including within actor handlers |
| `timers-every` | `#[every(duration)]` periodic handlers | Timer operations | Warn (`PeriodicTimers`) | timers are cooperative on wasm32: a sleep parks the coroutine and an #[every(duration)] handler fires when the process driver next ticks the shared timer wheel, so granularity follows the driver steps rather than a dedicated ticker | Cooperative periodic dispatch on WASI and checked periodic handlers in the browser VM |
| `streams` | Pipe operations (`stream.pipe`, `Stream<T>.*`, `Sink<T>.*`) | Pipe operations (`stream.pipe`, `Stream<T>.*`, `Sink<T>.*`) | Pass | — | Implemented on the wasm32 process driver: the same suspending queue core the native target runs, with a parked producer or consumer resumed from the driver's run queue instead of a worker thread. The file and socket backings of the same handle types are separately classified |
| `filesystem-streams` | `std.fs.read`, `std.stream.open`, `std.fs.open_read`, `std.fs.open_write` | Ambient files and file-backed streams | Reject (`FilesystemStreams`) | ambient file constructors and file-backed streams are unavailable on wasm32; WASI directory resources are separately classified | File-backed stream runtime is native-only; path-independent fs helpers remain separately classified |
| `directory-resources` | Host-supplied directory resources | Host-supplied directory resources | Pass | the host must explicitly supply a directory preopen; no ambient directory is granted by default, and a synchronous host syscall cannot be interrupted after entry | WASI preview1 uses explicitly preopened host directories with existing directional owners and synchronous regular-file operations; the browser VM has no real directory adapter yet |
| `termination-signals` | `std.os.shutdown_signal` | Termination signal subscriptions | Reject (`TerminationSignals`) | termination requests arrive through native signal handlers and a dispatcher thread; a wasm32 module receives no SIGTERM or console control events | Native-only signal dispatcher |
| `descriptor-readiness` | `std.io.watch_readable` | Descriptor readiness watches | Reject (`DescriptorReadiness`) | a readiness watch waits on the native I/O reactor, which is not compiled for wasm32 | Native-only reactor watch |
| `http-client` | `std.net.http.http_client.*`, `http_client.Response.*` | std.net.http.http_client operations | Reject (`HttpClient`) | the std.net.http.http_client wrappers are native-only and have no wasm32 networking bridge | Native-only wrapper module |
| `smtp` | `std.net.smtp.*`, `smtp.Conn.*` | std.net.smtp operations | Reject (`Smtp`) | the std.net.smtp transport is native-only and has no wasm32 SMTP bridge | Native-only transport wrapper |
| `websocket` | `std.net.websocket.*`, `websocket.Conn.*`, `websocket.Server.*` | std.net.websocket operations | Reject (`WebSocket`) | the std.net.websocket transport uses native sockets and OS threads and has no wasm32 WebSocket bridge | Native-only socket and reader-thread transport |
| `http-server` | `http.listen`, `http.Server.*`, `http.Request.*` | HTTP server operations | Reject (`HttpServer`) | the std.net.http server is backed by native sockets and tiny_http and has no cooperative wasm32 implementation | Native-only runtime module |
| `tcp-networking` | `net.listen`, `net.connect`, `net.*`, `net.Listener.*`, `net.Connection.*` | TCP networking operations | Reject (`TcpNetworking`) | hew_tcp_listen / hew_tcp_connect require the native OS socket layer; the transport runtime module is not compiled for wasm32 | Native-only runtime module |
| `process-execution` | `process.run`, `process.start`, `process.*`, `process.Child.*` | Process execution operations | Reject (`ProcessExecution`) | hew_process_start_shell / hew_process_start require the native OS process model; the process runtime module is not compiled for wasm32 | Native-only runtime module |
| `tls` | `std.net.tls.connect/read/write/close`, `tls.TlsStream.*` | std.net.tls operations | Reject (`Tls`) | the std.net.tls transport is backed by rustls over native sockets and has no wasm32 TLS bridge | Native TLS-over-TCP stack today; no documented wasm32 path |
| `quic` | `std.net.quic.*`, `quic.QUICEndpoint.*`, `quic.QUICConnection.*`, `quic.QUICStream.*`, `quic.QUICEvent.*` | std.net.quic operations | Reject (`Quic`) | the std.net.quic transport is backed by quinn over native sockets and has no wasm32 QUIC bridge | native QUIC is feature-gated and not compiled for wasm32 |
| `dns` | `std.net.dns.resolve`, `dns.lookup_host` | std.net.dns resolver operations | Reject (`Dns`) | the std.net.dns resolver uses the native OS resolver and has no wasm32 implementation | Native OS resolver; not compiled for wasm32 |
| `std-os` | `std.os.*` (args, env, cwd, home, hostname, pid, temp-dir) | std.os environment and path operations | Reject (`OsEnv`) | the std.os helpers rely on native POSIX APIs; the os runtime layer is not compiled for wasm32 | Hew OS/env helpers are native-only today even where WASI may offer host data |
| `distributed` | `Node.*` (`start`, `shutdown`, `connect`, `set_transport`, `load_keys`, `identity_key`, `id`, `allow_peer`, `register`, `lookup`), remote actor calls, and remote monitor/link operations | Distributed node and remote-actor operations | Reject (`Distributed`) | the Node cluster API and remote actor calls route through the native mesh transport (hew_node_api_* / hew_remote_call_*), which is not compiled for wasm32 | Key-derived identity, durable sessions, authenticated mesh routing, registry/SWIM, and cross-node lifecycle are native-only |
| `stdin-input` | Standard input reads | Standard input reads | Pass | — | Native and WASI standard input is host-provided |
| `crypto-random` | `std.crypto.crypto.random_bytes` | std.crypto.crypto.random_bytes operations | Reject (`CryptoRandom`) | the std.crypto.random_bytes secure entropy source (ring::SystemRandom) is native-only and absent from the wasm32 link set; generating key material without a cryptographic entropy source would be unsafe | Secure entropy source is native-only; fail-closed rejection until wasm32 cryptographic entropy exists |
| `crypto-encrypt` | `std.crypto.encrypt.*` | std.crypto.encrypt operations | Reject (`CryptoEncrypt`) | the std.crypto.encrypt module is backed by a native-only staticlib companion crate (std/crypto/encrypt) that is absent from the wasm32 link set | Native-only AES-256-GCM companion crate; not linked for wasm32 |
| `crypto-sign` | `std.crypto.sign.*` | std.crypto.sign operations | Reject (`CryptoSign`) | the std.crypto.sign module is backed by a native-only staticlib companion crate (std/crypto/sign) that is absent from the wasm32 link set | Native-only Ed25519 companion crate; not linked for wasm32 |
| `unseeded-random` | Unseeded UUID and password-salt generation | Unseeded UUID and password-salt generation | Pass | — | Native UUID and password helpers use host entropy rather than the seeded program PRNG |
| `filesystem-inspection` | Filesystem inspection outside std.fs | Filesystem inspection outside std.fs | Pass | — | Path metadata and glob expansion read the host filesystem directly |

---

## Disposition rationale

### ⚠️ Warn (warning-level)

Warnings identify supported operations whose timing differs on the
single-threaded cooperative WASM scheduler. `sleep`, `sleep_until`, and
`#[every(duration)]` use the shared timer wheel. The process driver ticks that
wheel against the WASI clock while the root waits.

Local actors, supervision trees, links, monitors, structured task scopes,
`select` and in-memory pipes share the cooperative runtime on Tier 2.
Supervisor construction and restart retain the same configuration and stable
roles, and terminal hooks complete before child state is released. Semaphore
acquisition, timed acquisition and cancellation also share their owned permit
operation with native execution.

### 🚫 Error (compile-time reject)

An error means the compiler rejects the operation before code generation. The
diagnostic reason in the table names the missing WASI runtime facility and,
where available, a supported alternative. In particular:

- File and socket streams depend on the native I/O reactor. Networking,
  subprocess, and distributed-node APIs are native-only.
- `crypto.random_bytes`, encryption, and signing require native cryptographic
  implementations. Tier 2 rejects them rather than substituting an insecure
  source or algorithm.

### ⚠️ Unclassified host capability

Some unsupported rows have no dedicated checker diagnostic. Raw sockets are
the current example: a WASI host may provide them, while Hew's stdlib
networking surface remains native-only. Treat these rows as unsupported on
Tier 2.

---

## Generators on WASM — note

`gen fn` uses LLVM's switched-resume coroutine lowering, emitted by
`hew-codegen-rs/src/physical_coro.rs`, with checked polling and cleanup through
`hew-runtime/src/generator_checked.rs` and `cont.rs`. LLVM constructs and
optimizes frames using the target data layout, and these runtime modules
are compiled for wasm32. WASI execution at O0 and O2 verifies scalar parameters
and a named function supplied to a `fn[clone]` parameter through full consumption
and early break, including producer defer cleanup.

Generators that depend on stream I/O are additionally covered by the Streams
reject above at the point of the stream call.

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

<!-- BEGIN GENERATED: playground-wasi-capability-summary -->
| Example | `capabilities.wasi` | Reason |
|---------|---------------------|--------|
| `basics/hello_world` | `runnable` | Verified by WASI end-to-end execution |
| `basics/fibonacci` | `runnable` | Verified by WASI end-to-end execution |
| `basics/float_arithmetic` | `runnable` | Verified by WASI end-to-end execution |
| `basics/float_division` | `runnable` | Verified by WASI end-to-end execution |
| `basics/float_nonfinite_compare` | `runnable` | Verified by WASI end-to-end execution |
| `basics/higher_order_functions` | `runnable` | Verified by WASI end-to-end execution |
| `basics/if_let_value` | `runnable` | Verified by WASI end-to-end execution |
| `basics/mixed_numeric` | `runnable` | Verified by WASI end-to-end execution |
| `basics/stmt_control_flow` | `runnable` | Verified by WASI end-to-end execution |
| `basics/stmt_if` | `runnable` | Verified by WASI end-to-end execution |
| `basics/stmt_if_let` | `runnable` | Verified by WASI end-to-end execution |
| `basics/stmt_match` | `runnable` | Verified by WASI end-to-end execution |
| `basics/string_interpolation` | `runnable` | Verified by WASI end-to-end execution |
| `basics/clone_value` | `runnable` | Verified by WASI end-to-end execution |
| `basics/display_scalars` | `runnable` | Verified by WASI end-to-end execution |
| `concurrency/actor_pipeline` | `runnable` | Verified by WASI end-to-end execution |
| `concurrency/async_await` | `runnable` | Verified by WASI end-to-end execution |
| `concurrency/counter_actor` | `runnable` | Verified by WASI end-to-end execution |
| `concurrency/supervisor` | `runnable` | Verified by WASI end-to-end execution |
| `language/arithmetic_operators` | `runnable` | Verified by WASI end-to-end execution |
| `language/array_indexing` | `runnable` | Verified by WASI end-to-end execution |
| `language/string_slicing` | `runnable` | Verified by WASI end-to-end execution |
| `language/while_loop` | `runnable` | Verified by WASI end-to-end execution |
| `language/wildcard_match` | `runnable` | Verified by WASI end-to-end execution |
| `language/match_guard` | `runnable` | Verified by WASI end-to-end execution |
| `language/compound_assign` | `runnable` | Verified by WASI end-to-end execution |
| `language/f64_nonfinite_render` | `runnable` | Verified by WASI end-to-end execution |
| `language/f64_finite_render` | `runnable` | Verified by WASI end-to-end execution |
| `machines/traffic_light` | `runnable` | Verified by WASI end-to-end execution |
| `types/collections` | `runnable` | Verified by WASI end-to-end execution |
| `types/pattern_matching` | `runnable` | Verified by WASI end-to-end execution |
| `types/record_types` | `runnable` | Verified by WASI end-to-end execution |
| `types/field_shorthand` | `runnable` | Verified by WASI end-to-end execution |
| `types/wire_types` | `runnable` | Verified by WASI end-to-end execution |
| `types/structural_bounds` | `runnable` | Verified by WASI end-to-end execution |
| `types/record_equality` | `runnable` | Verified by WASI end-to-end execution |
| `types/tuple_values` | `runnable` | Verified by WASI end-to-end execution |
| `types/generic_aggregate_eq` | `runnable` | Verified by WASI end-to-end execution |
| `types/option_result_methods` | `runnable` | Verified by WASI end-to-end execution |
| `types/vec_operations` | `runnable` | Verified by WASI end-to-end execution |
| `types/vec_f64_nonfinite_contains` | `runnable` | Verified by WASI end-to-end execution |
| `types/vec_inclusive_slice` | `runnable` | Verified by WASI end-to-end execution |
| `types/record_clone` | `runnable` | Verified by WASI end-to-end execution |
| `types/fn_field_call` | `runnable` | Verified by WASI end-to-end execution |
| `types/method_clone` | `runnable` | Verified by WASI end-to-end execution |
<!-- END GENERATED: playground-wasi-capability-summary -->

Each `runnable` example is compiled and executed by the WASI end-to-end test,
which compares its output with the checked expectation. Each `unsupported`
example is checked for the capability-specific diagnostic shown above.

Run `make playground-wasi-check` in a codegen-capable environment to verify
the curated examples against the Tier 2 runtime.
