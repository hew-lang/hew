# Changelog

## [Unreleased]

### Fixed — `hew eval`

- **Runtime failures are no longer silent.** A child that died from a signal
  (e.g. divide-by-zero) while producing no stderr now yields a synthesized
  message naming the terminating signal and its likely cause, in both raw and
  `--json` output (`stderr` and `diagnostics` are populated identically).
- **`--jit auto` falls back to AOT.** `auto` now selects the best available
  backend (today the AOT path) instead of failing; the explicit
  `--jit inprocess` mode still fails closed with a named diagnostic because the
  in-process LLJIT bridge is not implemented yet. (#1227, #1235)
- **No whole-program lints against REPL fragments.** Defining and then using a
  function, binding or import across separate inputs no longer raises
  "never called", "unused variable", "unused import" or "unused mut" — these
  completeness lints are suppressed for the synthetic single-fragment program
  while `hew check`/`hew build` keep reporting them.

### Changed — `hew eval`

- `hew eval` is presented as a lightweight, one-shot evaluator — a single
  expression, a piped snippet, or a `-f <file>` program, plus an interactive
  session for exploration — rather than a stateful interpreter. The `--help`
  and interactive `:help` say so and point at `-f <file>` or `hew run` for a
  multi-statement program.
- The startup notice about a prior session that ended without `:quit` is now a
  calm, actionable note explaining that each input runs in its own subprocess,
  rather than an alarming uncaught-signal warning.

## [0.5.0] — 2026-06-11

v0.5.0 is the user-trust release: the language, runtime, and toolchain are
aligned around a single substrate that distributed-systems engineers can build
service backends and real-time pipelines on without reaching through the C ABI
for the load-bearing parts. The companion narrative release notes live at
[`docs/releases/v0.5.0.md`](docs/releases/v0.5.0.md). This entry is
the structured changelog.

### Added — language surface

- **Generator blocks — `gen { ... }`:** `gen` blocks type-check as
  `Generator<Yield, Return>` from their yielded values and final expression.
  Empty generator blocks fail with `E_EMPTY_GENERATOR`, and `yield` is valid
  only inside a `gen` block.
- **Unified concurrency substrate — `Duplex<S,R>`, `Sink<T>`, `Stream<T>`:**
  Three typed channel primitives replace the legacy send-operator surface.
  `Duplex<S,R>` is a full-duplex channel; `Sink<T>` and `Stream<T>` are the
  directional halves, constructed via `duplex_pair<S,R>(capacity)` and
  `channel<T>(capacity)`. In 0.5.0 the lowered surface is tell-shaped
  `.send(msg)` on these primitives plus the full `std::channel`
  `Sender`/`Receiver` pair (`channel.new(capacity)`, `await rx.recv()`,
  deadline-aware recv); the remaining Duplex/Sink methods (`.try_send`,
  `.try_recv`, `.close`, ask-shaped send) type-check but refuse with named
  diagnostics until their lowering lands.
- **Lambda-actor form — `actor |params| { body }`:** A lambda-actor literal
  evaluates to a `Duplex<Msg, Reply>` handle running in a supervised child
  context. Both bare-call syntax `handle(msg)` and `.send(msg)` are accepted.
- **Structured concurrency — `scope` block + `fork` verb:** `scope { ... }` is
  the lexical lifetime bracket for child tasks; `fork { call(); }` starts a
  child inside it, and the scope joins all children at its closing brace.
  Scopes are available in suspendable contexts (actor handlers, closures,
  task entries). The name-bound form `fork name = call(...)`, argument-
  bearing forks, and awaiting a child's value parse but are rejected with
  named diagnostics — they land in a follow-up release.
- **Actor lifecycle hooks — `#[on(start)]`, `#[on(stop)]`, `#[on(crash)]`:**
  `#[on(start)]` runs before the message loop; `#[on(stop)]` runs after it
  exits; `#[on(crash)]` runs on the supervisor when a child faults. Multiple
  `#[on(stop)]` hooks execute in declaration order; the others are each allowed
  at most once per actor. `#[on(upgrade)]` is reserved and rejected with a
  diagnostic pending the upgrade-runtime track.
- **Periodic receive handlers — `#[every(duration)]`:** A receive handler
  annotated with `#[every(duration)]` fires automatically at the specified
  interval for the actor's lifetime. The timer is armed at spawn and repeats
  inside the actor's ordinary message loop — single-threaded actor semantics are
  preserved. Previously the attribute parsed but did nothing; it is now end-to-end
  in checker, HIR, MIR, codegen, and runtime (`hew_actor_schedule_periodic`).
- **Receive deadlines:** Awaiting a message now accepts a deadline. The outcome
  is a typed `Result<Option<T>, TimeoutError>`: a value, a clean end-of-stream,
  or an explicit timeout. The same deadline discipline applies to socket
  await-read, `accept`, channel and stream `recv`, and cross-node `ask` via the
  `| after` combinator.
- **Qualified actor identity:** Actor identities are now carried as module-
  qualified names through the entire pipeline — checker, HIR, MIR, runtime
  message routing and supervision — so two actors sharing a short name in
  different modules are genuinely distinct everywhere they are referenced.
- **Bind-and-propagate sugar — `let r? = expr`:** Sugar for `let r = expr?;`.
  Works with `Result<T, E>` and `Option<T>`.
- **String methods raised into Hew — `.len()`, `.slice(a, b)`, `.find(needle)`,
  indexing, concatenation:** String operations are now first-class Hew methods.
  Semantics are byte-oriented in 0.5.0: `.len()` counts bytes, `.slice(a, b)`
  and indexing address byte offsets (slicing inside a multi-byte codepoint
  yields replacement output), and `.find` returns a byte index as `i64` (`-1`
  when absent). Indexing and the `+` concatenation operator desugar through
  the same method surface.
- **Display trait + f-string interpolation lowering:** `Display` is the
  user-facing formatting trait; `f"{x}"` lowers through it. User types
  participate by implementing `Display` with `fn fmt(val: Self) -> string`.
- **Record types — named-field decl, literal, functional update:**
  `record Point { x: f64, y: f64 }` declares a named-field record with
  literal construction and functional update (`Point { x: 3.0, ..base }`).
  Structural equality, hashing, the positional tuple-record form, and `is`
  shape checks on records are specified but refuse with diagnostics in
  0.5.0.
- **Primitive width canonicalisation:** Integer widths are
  explicit: `i8 / i16 / i32 / i64 / u8 / u16 / u32 / u64` for fixed widths
  (`isize`/`usize` are reserved names whose arithmetic surface is not yet
  usable). Untyped integer literals inherit
  context width with a defined defaulting rule. Overflow, divide-by-zero,
  modulo-by-zero, and shift-out-of-range now **trap** rather than silently wrap.
  Wrapping arithmetic is opt-in via `.wrapping_add()`, `.wrapping_sub()`,
  `.wrapping_mul()`, `.wrapping_shl()`, `.wrapping_shr()`.
- **`u8` width and `bytes[]` / `b"..."` literals:** A first-class `u8` integer
  width, `bytes[]` collection literals, and `b"..."` byte-string literals now
  lower through the full pipeline.
- **Vec bounds — OOB traps:** `Vec<T>` indexing traps on out-of-bounds
  access instead of reading out of range. (A `.slice(a, b)` range view is
  specified but not in the 0.5.0 method surface.)
- **Split actor identity types — `LocalPid<T>` and `RemotePid<T>`:** Builtins
  distinguish local from remote process identifiers; split makes remote dispatch,
  serialization, and same-node fast paths explicit in the type surface.
- **Associated types, initial surface:** Edition 2026 admits one associated type
  per trait: `type Item;` in traits, `type Item = ...;` in impls, `Self::Item`
  in type position. Multi-type trait families and associated-type bounds remain
  deferred.
- **`Result` / `Option` constructors + match-on-enum-variant lowering:** `Ok(x)`,
  `Err(e)`, `Some(x)`, and `None` are first-class constructors; their match-arm
  patterns lower through the canonical HIR enum path.
- **`extern` / `unsafe` blocks (substrate L0):** Foreign and authority-bearing
  surfaces are gated by `extern` and `unsafe` blocks; the type checker refuses
  raw-pointer or foreign-call operations outside them.
- **Value-typed state machines (`machine` declarations):** `machine` is a new
  nominal type declaring a closed set of states, named events (`events {}` header),
  an optional output vocabulary (`emits {}` header), and transition rules
  (`on E: Source => Target`). The `reenter` modifier opts a self-transition into
  entry/exit lifecycle re-execution. Depth-1 composite state blocks and inline
  `entry {}` / `exit {}` hooks ship in v0.5. Machines execute natively:
  `MachineName::State` constructs a value, `.step(Event)` drives transitions,
  and `.state_name()` reads the current state. The checker enforces total
  event coverage — every declared state must handle every declared event.
  `hew machine diagram` renders any `machine` declaration as a Mermaid,
  Graphviz DOT, or JSON schema diagram.
- **Cross-module function values:** Non-generic function values cross module
  boundaries cleanly; imported actors, impl blocks, and private record types
  resolve correctly through the module graph. Module-scoped span keys prevent
  cross-module fact collisions in large programs.
- **Literal match patterns:** `match` arms now accept integer, boolean,
  character, and string literals — including nested literal patterns with binding
  subpatterns — so value dispatch no longer requires guard expressions or
  enum-only encodings.
- **Loop control flow:** Bare `loop` expressions, `break`, and `continue` now
  lower through the full compiler pipeline.
- **Top-level constants:** `const` declarations lower through HIR, MIR, and
  native codegen as LLVM globals.
- **`Result` and `Option` propagation with `?`:** The `?` operator desugars for
  both `Result<T, E>` and `Option<T>`, complementing the `let r? = expr` form
  with expression-level propagation.
- **Collection iteration and layout-backed collections:** `Vec<T>` participates
  in `IntoIterator` / `Iterator`, including mutable `Iterator::next` receivers.
  `Vec<T>`, `HashMap<K, V>`, and `HashSet<T>` gained layout-backed runtime and
  codegen support for non-primitive element, key, and value shapes.
- **Composite values and tuple literals:** Tuple literal construction lowers end
  to end; `BitCopy` composite return values travel through return slots rather
  than being restricted to scalar paths.
- **Opaque handles and borrow markers:** `#[opaque]` type declarations model
  runtime handles as `BitCopy` values; immutable `&T` borrow markers support
  copy-on-write value semantics without transferring ownership.
- **String and bytes operations:** String/bytes indexing and slicing use
  codepoint-aware semantics; string concatenation lowers to the runtime concat
  ABI; `Bytes::push` is wired through native codegen.
- **Numeric casts and discard bindings:** Numeric `as` casts lower through native
  codegen; top-level wildcard `let _` bindings are treated as explicit discards.
- **Generic, trait, and machine dispatch:** Machine declarations gained
  where-clause and const-generic support; generic actor spawns accept explicit
  type arguments; static trait calls, resolved impl calls, dynamic trait vtables,
  and machine-generic bound enforcement flow through canonical checker, HIR, MIR,
  and codegen metadata.
- **Cancellation and resource cleanup:** `CancellationToken` is available as a
  frontend value type; cancellation observation lowers to the runtime; `defer`
  lowers through scope-exit control flow; user resource `close` operations route
  through typed drop dispatch.
- **Editor diagnostics:** HIR diagnostics surface through LSP editor diagnostics;
  editor syntax definitions recognise the newer record and actor-decorator forms.
- **`let`-field immutability enforcement (BREAKING — migration is `var`):** A
  field declared `let` is immutable; the checker now enforces it with a compile
  error that points at the fix. Fields intended for reassignment must be declared
  `var`.

### Added — runtime, mesh, and observability

- **Native mesh substrate (`Node::*`):** The runtime exposes `Node::start`,
  `Node::connect`, `Node::shutdown`, `Node::set_transport`, `Node::register`, and
  `Node::lookup`. `RemotePid<T>` names a remote actor; `LookupError`
  (`NotFound`, `WrongType`, `NodeUnreachable`, …) replaces stringly-typed lookup
  errors. A two-node cross-process integration test exercises
  register-on-A → lookup-from-B → ask round-trip end-to-end.
- **mTLS via QUIC mesh:** Inter-node traffic flows over QUIC with mutual TLS.
  Peer identity is pinned by an SPKI allowlist; anonymous inbound mesh connections
  are refused; the handshake hardens against downgrade, version-mismatch, and
  identity-mismatch.
- **Supervisor surface — `max_heap`, restart strategies:**
  A per-actor `max_heap` cap triggers a supervised restart on
  violation rather than an OOM-killed process. Restart strategies
  (`one_for_one`, `one_for_all`, `rest_for_one`) are declared alongside supervised
  children.
- **`PartitionDetected` mesh signal:** When the local node loses connectivity to
  a peer, monitors of remote actors receive a `PartitionDetected { node, last_seen
  }` notification distinct from ordinary actor-exit signals.
- **Actor `Blocked` state removed:** The scheduler now models awaits and blocking
  I/O through receive-cursor state on the actor's mailbox/scope, simplifying the
  runtime and the traces it emits.
- **`CrashInfo` and `TrapInfo` at the language surface:** `#[on(crash)]` handlers
  receive structured `CrashInfo` (`reason`, `source_location`, `actor_id`,
  `restart_count`) and the underlying `TrapInfo` (trap kind, faulting instruction,
  backtrace) rather than an opaque string.
- **Profiler schema versioning + trace taxonomy v0.5:** Profiler output carries a
  `schema_version` field and emits the v0.5 concurrency-event taxonomy (`fork`,
  `join`, `cancel`, `ask`, `reply`, `partition`, `restart`, `upgrade`). Older
  trace consumers receive a typed mismatch error rather than silent drift.
- **`path` and `glob` runtime surface:** `hew_glob` expands POSIX glob patterns
  and returns an owned `HewGlobResult`; `hew_glob_count`, `hew_glob_get`, and
  `hew_glob_free` complete the surface. Filesystem metadata helpers live in the
  same `path` module. On Windows, glob expansion records unsupported rather than
  silently failing.
- **Typed runtime-call descriptors:** Runtime calls are now described by typed
  descriptor structs passed through the compilation pipeline, replacing the
  untyped shim dispatch used in earlier releases.
- **Single-owner timer free protocol:** The periodic-timer runtime was rewritten
  to a single-owner model, verified clean under AddressSanitizer; the deferred-
  free threads spawned during restart are now joined on supervisor teardown.

### Added — sandbox runtime parity

- **Canonical-frontend bytecode emission:** The sandbox bytecode emitter goes
  through the canonical typed-IR frontend; bytecode from the sandbox path and the
  native LLVM path share lowering through MIR.
- **Control-flow lowering parity:** `match`, `if`/`else`, `while`, `for`,
  `?`-propagation, `scope`/`fork`, and `gen`-block lowering share control-flow
  shape between native and sandbox targets.
- **`make sandbox-parity` harness:** Runs every parity fixture through both
  backends, diffs observable outputs, and fails closed on divergence. The
  accepted-divergence catalog lives at
  [`docs/sandbox-vm-divergences.md`](docs/sandbox-vm-divergences.md).

### Added — wire format doctrine

- **CDDL + CBOR for inter-process messaging:** The runtime actor envelope is a
  CBOR `wire-frame` keyed by small integers, schema fixed at
  `hew-runtime/schemas/envelope.cddl`. Fields: `version`, `frame_type`,
  `target_actor_id`, `source_actor_id`, `msg_type`, `payload`, `request_id`,
  `source_node_id`. Decoders must reject unknown `version` values with
  `UnknownVersion`; best-effort parsing of an unrecognised version is forbidden.
- **User-facing encoding under `std::encoding::*`:** Programs reading or writing
  JSON, CBOR, or other formats use the stdlib encoding modules; the runtime
  envelope is not a user-extension surface.
- **Wire-format anti-doctrine:** The full rationale for what is out of scope is
  in
  [`docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md`](docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md).

### Added — developer trust

- **Test suite ratchets in CI and preflight:** `make test-hew-ratchet` and
  `make test-stdlib-ratchet` compare failing tests against a tracked set and fail
  closed if any new failures appear. Both ratchet snapshots are empty for this
  release: 745/745 `.hew` suite tests pass, stdlib type-check 71/71.
- **Per-crate lane test tier (`make test-lane`):** A narrower nextest profile
  (`profile.lane`) covers in-process tests only and excludes the subprocess-
  spawning exec/e2e/oracle corpus, enabling fast per-crate iteration without
  paying the full suite.
- **WASM-LSP analyser fixture coverage:** Thirty new v0.5-substrate fixtures
  cover the WASM-hosted language-server analyser: hover, goto-definition,
  find-references, signature help, and diagnostics. The browser-LSP compile path
  (parse → check → diagnostic emission) is covered alongside.
- **Editor grammar keyword updates:** Sublime, Emacs, and Nano syntax bundles
  updated for the v0.5 keyword set (`actor`, `scope`, `fork`, `gen`, `#[on(...)]`,
  `record`, `isize`, `usize`, and the removed-keyword diagnostics).

### Changed

- **`let`-field mutability is now a compile error:** A `let` field assigned after
  declaration fails with a checker diagnostic pointing at the fix. The migration
  is `let` → `var` on any field you intend to reassign. (See also: Breaking.)
- **Collection dispatch authority:** Collection methods moved from legacy
  dual-dispatch shims to resolved-call, layout-witness, and declarative FFI
  descriptors; legacy untyped `HashMap` / `HashSet` and primitive Vec families
  are retired behind the new layout-managed path.
- **String ownership model:** Runtime `String` is now copy-on-write and
  refcounted, with header-aware C-string allocation and container ingress rules
  that distinguish move ownership from copy-in semantics.
- **Actor message ownership:** Actor send and receive paths carry an alias-mode
  discriminant and support shareable-value borrowing with retain-on-escape,
  replacing implicit ownership assumptions with explicit single-release envelope
  behaviour.
- **Machine transition resource lifetimes:** Machine state changes release
  `@resource`-typed payload fields when leaving a state or taking a `reenter`
  transition. Plain self-transitions without `reenter` keep payloads live.
- **State-field overwrite releases:** When an actor or machine overwrites a state
  field that owns heap memory — a collection, record, enum payload, `bytes`, or
  `string` value — the prior contents are now released. Local `HashMap` and
  `HashSet` handles are dropped at scope exit; heap-owning `Option` bindings drop
  correctly across loop back-edges; per-frame byte buffers release on stream
  receive loops.
- **Lambda-actor handles accept `.send()`:** Both `handle(msg)` (bare call) and
  `handle.send(msg)` are accepted; actors and channels share a uniform method
  surface. The prior `E_LAMBDA_NO_SEND_METHOD` restriction is lifted.
- **WASM and target gates:** Unsupported coroutine, blocking receive, scope,
  memory intrinsic, and sandbox-profile paths now fail closed at the frontend or
  lowering boundary instead of depending on backend behaviour.
- **Legacy runtime substrate:** Deprecated `HewScope` infrastructure and obsolete
  MLIR-era scripts removed; scope and task behaviour routes through the current
  actor/task substrate.

### Fixed

- **Fail-closed compiler boundaries:** The checker, HIR, MIR, and codegen front
  reject ambiguous module resolution, unsupported collection layouts, missing
  record layouts, unresolved named-type readiness, and stale lowering shapes
  instead of silently continuing with partial metadata.
- **Recursive value types:** Recursive value-type definitions are rejected during
  checking, preventing invalid self-contained layouts from reaching lowering or
  runtime code.
- **Runtime memory safety:** Actor state clone/drop wiring, scheduler teardown,
  lambda-actor drop, task-scope cancellation cleanup, layout `HashSet` clone/drop,
  actor-state offsets, C-string allocation, and string-container ownership now use
  correct lifetime and allocator discipline. All findings held to AddressSanitizer-
  clean as a hard bar.
- **Actor and mesh ABI correctness:** Remote actor `tell`, link/monitor,
  supervisor stop, shareable-value send and receive, mailbox envelopes,
  `Node::register`, typed `Node::lookup`, and mesh errors route through typed
  runtime ABIs with fail-closed envelope modes.
- **Wire-format hardening:** CBOR envelope frames conform to the schema; unknown
  ask-rejection reason bytes are rejected; mailbox payload classes and cross-node
  envelope fields are pinned at the ABI boundary.
- **Builtin and imported call resolution:** Bare builtin enum constructors,
  builtin `None`, imported public impl methods, imported free-function helpers,
  where-clause associated bindings, and same-module helper calls resolve through
  checker-authoritative metadata.
- **Trait-object and composite lowering:** Trait-object drops, dynamic vtable
  calls, borrow and trait-object type-parameter substitution, enum-payload
  clone/drop/eq synthesis, composite cancel-exit returns, and unreachable-
  predecessor initialisation checks preserve correct value semantics.
- **CLI and editor feedback:** `hew check` runs through the HIR, MIR, and
  codegen-front gates and renders MIR diagnostics. LSP diagnostics use the
  accepted v0.5 surface rather than stale parser-only assumptions.
- **Build and platform reliability:** Native macOS objects carry the correct
  deployment target; sanitizer and WASI lanes gate on available runners; fuzz
  smoke propagates target crash exit codes; the WASM channel-full path reports a
  typed last-error instead of panicking.
- **Cross-module monomorphisation:** Imported actor spawns, impl methods, and
  generic types from other modules monomorphise correctly; module-scoped span keys
  eliminate the cross-module fact collisions that made large programs fragile in
  prior releases.
- **Supervision teardown ordering:** Supervisor teardown joins the threads it is
  responsible for, including deferred-free threads spawned during restart, so
  shutdown is orderly rather than racing the actor sweep.

### Removed (breaking)

- **`int` / `uint` type aliases.** Use explicit-width types: `i64` / `u64` for
  fixed 64-bit integers. The compiler emits a diagnostic with the correct
  suggestion when these removed names appear in type annotations.
- **`pure fn` modifier.** `pure fn` now fails during parsing; ordinary `fn`
  declarations are unchanged.
- **Legacy CLI compile surface.** `hew compile` is the single v0.5 IR-ladder
  entry point, and `hew build` is its native-binary front end (like `go
  build`). The dormant legacy run/build bodies and the old
  `compile::compile()` entry point are gone.
- **`hew-codegen` C++/MLIR subtree retired.** The C++ MLIR-based code generator,
  its generated msgpack reader, and the `hew-astgen` helper crate are deleted.
  `hew-codegen-rs` (LLVM via inkwell) is the sole compiler backend, linked into
  the `hew` binary as a normal Cargo dependency. The CMake/Ninja/build-script
  path and C++ codegen sanitizer path are gone.
- **`scope |s| { s.launch / s.spawn / s.cancel }`.** The binding form exposing a
  scope handle `s` with imperative launch/spawn/cancel methods no longer parses.
  Migrate to `scope { ... }` with `fork { call(); }` children inside.
- **`<-` send operator.** The parser emits `E_OPERATOR_REMOVED` on adjacent `<`
  and `-` in expression position. Use `handle.send(msg)` or call-syntax
  `handle(msg)`.
- **`spawn (params) => body` lambda-actor syntax.** The parser emits
  `E_SPAWN_LAMBDA_SYNTAX_REMOVED` with a fixit note. Use `actor |params| { body
  }` instead.
- **`terminate { }` block.** Migrate cleanup logic to
  `#[on(stop)] fn <name>() { ... }` inside the actor body.
- **`=~` and `!~` regex operators.** Use `p.is_match(s)` for a boolean match
  result or `p.matches(s)` to iterate over all matches.
- **`fs.read_line` compatibility alias.** Callers must use `io.read_line()`
  directly.
- **`ActorStream<Y>` type alias.** Use `Stream<Y>` instead.
- **`legacy-wire-msgpack` Cargo feature.** The `hew-serialize` crate no longer
  exposes the feature flag or the `serialize_wire_decl_legacy` function. Migrate
  to the standard wire serializer.
- **`hew_file_last_error` C ABI export.** The errno value is surfaced through
  the standard `LAST_ERRNO` path.
- **`hew_duplex_new` C ABI export.** The self-loopback duplex without a peer is
  gone. Use `hew_duplex_pair` to create a matched bidirectional pair.

### Known WASM behavioural gaps

- **Reply-channel state is process-global on WASM:** cooperative scheduling on
  WASM still routes ask reply-channel state through process-global storage, so
  concurrent ask races can diverge from native per-execution-context behaviour.
- **`#[on(stop)]` observability is narrower on WASM:** the WASM terminate path
  runs without the native signal-recovery and lifecycle-tracing path, so stop-hook
  semantics hold but lifecycle tracing is omitted.

## [0.4.0] - 2026-05-03

See [migration guide](docs/migrations/v0.4.0.md) for upgrade steps.

### Changed

- **`hew-wasm` empty-result encoding (BREAKING for browser consumers):** WASM
  exports that returned an empty string `""` to indicate "no result" now return
  `"null"` (optional scalar exports — `hover`, `goto_definition`,
  `find_references`, `prepare_rename`, `signature_help`) or `"[]"` (collection
  exports — `rename`, `inlay_hints`). Browser/editor integrations that special-
  cased `result === ""` must update to handle the canonical JSON literals
  (#1506).
- **Explicit HTTP/regex handle teardown:** `http.Server` and `regex.Pattern`
  no longer auto-release on scope exit. Callers must invoke `close()` /
  `free()` explicitly before those values go out of scope to avoid leaks
  (#1314).
- **Explicit HTTP request and JSON value teardown:** `http.Request` and
  `json.Value` no longer auto-release on scope exit. Callers must invoke
  `free()` explicitly before those values go out of scope. This mirrors
  the `Server`/`Pattern` migration and decouples handle release from the
  codegen drop-slot null-after-move path (#1500).
- **`std/encoding/compress` decompression functions require `max_output_len`:**
  `gzip_decompress`, `deflate_decompress`, and `zlib_decompress` each gained a
  required second argument `max_output_len: int`. Old shape:
  `gzip_decompress(data: bytes) -> bytes`. New shape:
  `gzip_decompress(data: bytes, max_output_len: int) -> bytes`. Migration: add
  a `max_output_len` argument at every call site; callers without a tighter
  bound should pass `64 * 1024 * 1024` (64 MiB). The function fails closed when
  the decompressed output would exceed the limit (#1471).
- **`http_client` string helpers return `Option<String>`:** `request_string`,
  `get_string`, and `post_string` changed return type from `String` to
  `Option<String>`. Old shape: `get_string(url: String) -> String`. New shape:
  `get_string(url: String) -> Option<String>`. Migration: pattern-match on the
  result — `None` is returned on transport failure (mirrors how other fallible
  HTTP operations return) (#1030).
- **stdlib public API uses `int` uniformly (i32/i64 removed from public
  surfaces):** All public function signatures across `std/**/*.hew` now use
  `int` instead of `i32` or `i64`. Affected modules include `channel`,
  `encoding/json`, `encoding/toml`, `encoding/yaml`, `encoding/csv`,
  `encoding/xml`, `encoding/protobuf`, `encoding/msgpack`, `encoding/wire`,
  `fs`, `net/http`, `net/http_client`, `net/smtp`, `net/tls`, `net/websocket`,
  `net/ipnet`, `net/quic`, `net/net`, `path`, `semaphore`, `text/semver`, and
  `time/cron`. Migration: update bindings and variable declarations that
  previously used `i32` or `i64` to use `int`. The full invariant and
  exemption rules are documented in `docs/stdlib-style-contract.md` (#1218).

### Added

- **LSP document formatting:** `textDocument/formatting` is now implemented in
  the language server, delegating to `hew_parser::fmt`. Editors that send
  formatting requests receive correctly formatted source without needing a
  separate `hew fmt` invocation (#1614).
- **LSP stdlib navigation:** goto-definition and find-references now resolve
  symbols from the standard library across file boundaries, including cross-file
  find-references and stdlib goto-definition (#1616).
- **Trait method dispatch on primitives:** trait methods can now be called on
  primitive and builtin-generic receivers (e.g., `int`, `String`, `Vec<T>`),
  enabling user-defined extension traits over stdlib types (#1596).
- **Per-call network deadlines:** DNS lookup, TCP connect, QUIC connections, and
  WebSocket I/O now accept an optional deadline argument. Calls that exceed the
  deadline fail closed rather than blocking indefinitely (#1557).
- **Structured parse diagnostics:** `ParseDiagnosticKind` discriminant is now
  available in WASM and LSP code-action routing, giving tooling consumers a
  machine-readable parse error classification (#1583, #1592).
- **HTTP + JSON demo example:** `examples/http_json_demo.hew` demonstrates a
  complete HTTP server that parses JSON request bodies and responds with JSON,
  using the v0.4.0 explicit teardown pattern (#1618).
- **LSP transitive goto-definition:** `find_cross_file_definition` now follows
  one import hop with a cycle guard, so goto-definition works through re-export
  and glob chains (#1073).
- **Module search-path documentation:** `HEWPATH`, `HEW_STD`, the four-step
  resolution order, and the `hew.toml` non-role are documented across the
  user-facing module discovery docs (#1074).
- **Eval WASM + `--json` ok-path coverage:** integration coverage now exercises
  `hew eval --json --target wasm32-wasi` on the success path, and the WASM
  capability matrix documents the non-interactive eval contract (#1075).
- **stdlib URL percent-encoding proof surface:** `url.encode`, `url.decode`,
  and `url.encode_query` are now confirmed end-to-end on native and WASM, and
  `hew-runtime` exports bounded `hew_bytes_to_string` support so `url.decode`
  works correctly under `wasm32-wasip1` (#1077).

### Fixed

- **JIT inprocess SIGSEGV:** the inprocess JIT mode (`--jit`) no longer
  crashes on programs that use the search-generator pattern. Root causes were a
  missing runtime-symbol export and a use-after-free in the search-generator
  lambda capture (#1613).
- **LSP stdlib navigation:** stdlib symbols now resolve correctly in
  goto-definition and find-references responses — previously these returned
  empty results for cross-file stdlib lookups (#1616, see also Added above).
- **Cross-module enum variant construction:** enum variants defined in one
  module can now be constructed with a payload from another module without a
  parser error (#1605).
- **Type checker `Ok(())` pattern:** the type checker now accepts `Ok(())` as a
  valid unit-payload variant pattern in match arms (#1617).
- **Formatter inline comments:** `hew fmt` preserves inline comments inside
  enum bodies, struct field lists, match arm bodies, and around `else if`
  branch headers instead of dropping them (#1535).
- **REPL piped-mode flush:** the interactive REPL now flushes stdout after each
  submission when running in piped mode, and `--jit` is forwarded correctly to
  interactive mode (#1553).
- **`hew doc` publish pipeline:** stdlib doc generation is cleaned up and a
  `make publish-docs` target wires the output to Cloudflare Pages (#1555).
- **Release binary macOS SIGABRT fix:** three function-local
  `static const std::regex` declarations in MLIR codegen triggered a libc++ ABI
  mismatch at process exit (Homebrew-compiled locale freed by the system
  allocator), aborting the release binary on every `hew run`. Removed `static`
  from those declarations so each `std::regex` is destroyed within the call
  frame (#1607).
- **`fs.try_read_bytes` binary-safety:** `try_read_bytes` now calls
  `hew_file_read_bytes` directly with proper `hew_file_last_error` handling
  instead of routing through the UTF-8 string path, so non-UTF-8 and
  NUL-containing binary files round-trip correctly (#1076).

### Documentation

- HEW-SPEC.md re-audited for v0.4.0 surface deltas (#1588).
- v0.4.0 migration guide added at `docs/migrations/v0.4.0.md` (#1590).
- Compiler-stack audit and admission finalization tests committed (#1591).
- linux-aarch64 pre-release gate added to CI (#1608).

## [0.3.0] - 2026-04-06

### Added

- **Cross-target object emission (`--emit-obj`):** `hew build --target <triple> --emit-obj`
  now emits correctly-formatted object files for arm64-apple-darwin, x86_64-unknown-linux-gnu,
  and x86_64-pc-windows-gnu without requiring a separate `-o` flag (output name defaults to
  `<stem><target-object-suffix>`). Foreign native executable linking is rejected early with a
  clear error directing users to `--emit-obj`. Verified by e2e tests that inspect object-file
  format and architecture via the `object` crate (#730, closes phase-1 of #254).
- **HTTP client surface:** `std::net::http` now includes bounded client wrapper helpers plus
  request/response header accessors for Hew programs, making it easier to build clients without
  dropping into Rust glue (#722, #747, #750).

### Fixed

- **Non-root module typechecking:** module graph bodies are now typechecked end-to-end while
  keeping local non-root types visible inside their own module, preventing imported private helper
  leaks, preserving static type methods, and restoring preregistered QUIC handle method
  enrichment (#756).
- **HTTP request typing precision:** request-building header inputs now stay aligned with the
  tuple-based header model exposed elsewhere in the HTTP surface (#758).
- **Composite generic codegen fail-closed behavior:** the remaining composite type-argument
  struct-init path now rejects unsupported inputs instead of reaching a late build-time crash
  during code generation (#769).
- **Builtin collection clone support:** `clone()` now works correctly for `Vec`, `HashMap`, and
  `HashSet`, including chained receiver expressions such as `v.clone().len()` and the missing
  `hew_hashset_clone` runtime hook (#772).
- **FreeBSD embedded codegen builds:** FreeBSD now uses the correct static LLVM/MLIR embedded
  codegen path, and the CLI fails closed when embedded codegen was explicitly requested but could
  not be configured instead of linking undefined embedded symbols (#775).
- **Release smoke portability and reliability:** the Darwin release path keeps its deployment
  target pin scoped correctly, pre-release validation no longer depends on GNU-only `mktemp`
  behavior, sanitizer OOM tests are hardened, and the QUIC remote-service smoke gate is
  stabilized (#763, #764, #771, #773).

### Changed

- **Release gating:** the pre-tag release gate is stronger, and the rust-runtime TSan lane is now
  explicitly documented as advisory until upstream toolchain support is restored (#757, #765).
- **Runtime/WASM parity proof:** closed-mailbox send behavior plus mailbox/scheduler/reply-channel
  parity are now explicitly covered in the native and wasm test matrix (#754).
- **CLI/docs truth surfaces:** the CLI/playground docs, first-run examples, and `hew fmt`/`hew
  doc` guidance now match the actual user-facing behavior more closely (#720, #748, #752, #755).

### Known Limitations

- **Nightly C++ sanitizer advisory:** the nightly `Codegen C++ ASan+UBSan` lane still reports a
  retired-backend container-overflow false-positive candidate in teardown. It is tracked as #774
  and carried as an explicit advisory for v0.3.0; ordinary release builds and focused validation
  are otherwise green.

### Fixed

- **Fail-closed type metadata boundaries:** reject unresolved type-checker output holes and serializer-side explicit `-> _` survivors instead of leaking unresolved inference variables or reconstructing missing type data downstream (#838, #848, #849)
- **Trait default `-> _` resolution:** default trait methods with bodies now resolve explicit `-> _` from checker signatures and fail closed when the return type remains unresolved (#849)
- **CLI E2E bootstrap hardening:** serialize shared `hew-lib` bootstrap in the CLI test harness so concurrent integration runs do not race codegen setup (#851)
- **Codegen metadata hardening:** MLIR lowering now fails closed on missing indirect-enum scrutinee metadata and preserves bytes-stream ABI selection through tracked stream-metadata fallbacks (#852, #853)

## [0.2.2] - 2026-03-29

### Added

- **hew-observe unix socket discovery:** profiler binds to a per-user unix domain socket when `HEW_PPROF=auto`, with auto-discovery so `hew-observe` finds running programs without specifying ports (#380)
- **hew-observe CLI modes:** `--list` prints discovered profilers, `--pid N` connects to a specific process, auto-reconnects when the observed program restarts (#380)
- **WebSocket server support:** `websocket.listen(addr)` → `server.accept()` → `Conn` in std::net::websocket, with the same send/recv/close API as the client (#379)
- **Observe showcase example:** `examples/observe_showcase.hew` exercises Overview, Actors, Cluster, Messages, and Timeline tabs (#380)

### Fixed

- **Reply channel convention rewrite:** route reply channels through `HewMsgNode.reply_channel` field instead of embedding in the data buffer, eliminating SIGSEGV when actors receive messages with arguments (#380, fixes #382)
- **Duplicate trait impl generation:** track type-defining module so imported impls use the correct mangled names, preventing duplicate functions that executed as static constructors before `main()` (#386, fixes #384)
- **Vec.remove() semantics:** `v.remove(value)` is always value-based removal; the new `VecRemoveAtOp` is reserved for index-based `remove_at()` (#379)
- **hew-observe connection status:** only the metrics probe sets the connection indicator; secondary endpoint failures no longer poison it (#380)
- **Supervisor label JSON escaping:** child names containing quotes or backslashes no longer produce malformed JSON in `/api/supervisors` (#380)
- **Orphaned reply channels:** `hew_msg_node_free` sends an empty reply for undispatched ask messages so callers don't deadlock (#380)
- **Crash recovery reply handling:** scheduler sends an empty reply when an actor crashes during an ask dispatch (#380)
- **Silent analysis skips:** fix bugs where AST analysis passes silently skipped nodes (#377)
- **Diagnostic quality:** use operator symbols in error messages, correct LSP severity levels (#378)
- **Codegen type validation:** validate `convertType` results for generic types and thunk lookup (#372)
- **Runtime aliasing violation:** eliminate aliasing violation in `hew_connmgr_add` (#371)

### Changed

- **Profiler server:** replace tiny_http with hyper 1.x on a single-threaded tokio runtime, enabling unix domain socket support and cleaner shutdown (#380)
- **Pre-commit hook:** fix Bash 3.2 compatibility (macOS default) — clippy now runs on commit (#380)
- **CLI:** migrate hew to clap derive with auto-generated shell completions (#373)
- **Clippy clean:** resolve all clippy warnings across the workspace (`-D warnings` clean) (#380)
- Remove the unused export-metadata toolchain (`hew-stdlib-gen`, `hew-export-macro`, `hew-export-types`, and the `export-meta` Cargo feature) now that `hew-types` loads canonical stdlib `.hew` sources directly
- Quality consolidation: DRY deduplication, dead code removal, YAGNI cleanup (#374, #376)
- Update workspace dependencies (#375)

## [0.2.1] - 2026-03-23

### Added

- Generic `Channel<T>` for String and int types (#271)
- `Stream<T>` and `Sink<T>` monomorphization for String and bytes (#265)
- Stream bytes API exposed in stdlib (#265)
- Actor terminate cleanup blocks — run teardown logic when an actor stops (#268)
- Temporary materialization for RAII drop of unbound heap values (#274)
- `Option<T>` return from Channel `try_recv` (#272)
- 17 new test modules covering wire protocol, file I/O, timers, routing, crypto, generators, HTTP, iterators, streams, and Result/Option FFI wrappers (#295–#350)
- DateTime and JWT E2E tests (#345), JSON/YAML/TOML encoding E2E tests (#342, #343)
- C++ unit tests for codegen_capi and msgpack_reader (#291)
- 562 E2E tests (up from 451 in v0.2.0)
- Pre-release cross-platform validation script for Linux, macOS, FreeBSD, and Windows (#360–#362)

### Fixed

- **Runtime hardening (memory safety):** prevent use-after-free in actor lifecycle cleanup (#311), cache actor data before supervisor trap in crash handler (#303), drain queued values on generator free (#317), make `HewTask.state` atomic to prevent data races (#314), sync WASM `HewActor` struct with native layout (#322)
- **Runtime hardening (concurrency):** correct atomic orderings and consolidate CStr conversion (#332), eliminate lock unwraps and panics from production code (#333), use poison-recovery for blocking pool, registry, and link RwLocks (#318, #320), route supervisor restarts through mailbox to prevent budget race (#313), make actor state copy fallible on allocation failure (#326)
- **Runtime hardening (shutdown):** harden profiler shutdown and timer cancel protocol (#353), add graceful ticker shutdown to prevent timer wheel UAF (#307), prevent shutdown self-deadlock and spawn-failure stall (#309), close shutdown lifecycle and env-lock gaps (#330), shutdown profiler thread before freeing node resources (#308), shutdown sockets before dropping to prevent reader thread deadlock (#292)
- **Runtime hardening (networking):** harden Noise crypto — zeroize handshake buffers and return full keypair (#329), clean up connection on decrypt failure (#300), harden stream buffering — cap line buffer and add buffer-reuse API (#328)
- **Runtime hardening (scheduling):** propagate worker spawn failures during scheduler init (#315), treat Stopping as non-terminal in scope wait (#316), synchronise environment variable access with RwLock (#302), auto-seed MT19937 from OS entropy (#304)
- **Runtime hardening (observability):** preserve trace context in mailbox MPSC dequeue (#301), add `set_last_error` calls to FFI functions that silently return null (#335), standardise FFI string ownership on `malloc_cstring` (#324)
- **Codegen:** RAII drop system hardening with null-guard and infrastructure (#273), align actor send drop semantics with deep-copy transport (#275), path-specific drops for struct return from nested scopes (#284), handle Result return type in actor receive functions (#267), systematic RAII memory leak audit
- **Analysis:** classify function calls as FUNCTION in semantic tokens (#354), classify type annotations as TYPE in semantic tokens (#269)
- **Types:** prevent non-trivial wrappers from being treated as pass-throughs (#293), resolve channel.hew standalone type-check failures (#294), default unresolved Range type variable to i64 (#266)
- **CLI:** use cross-platform home directory resolution in adze (#351), handle `--help` flag in hew-lsp before server initialisation (#349)
- **Build:** fix static link ordering for aarch64 Linux release builds — include libstdc++ and libgcc in archive group for GNU ld circular dependency resolution (#359, #363–#369), fix macOS build detection and Make 3.81 segfault (#355), register encoding/hex and fix crypto interface location in stdlib (#338)

### Changed

- Embed MLIR/LLVM codegen into single `hew` binary — `hew-codegen` is no longer a separate executable (#261)
- Decompose `compile()` into pipeline stages (#287)
- Decompose `synthesize_inner` into focused helpers (#285)
- Simplify type checker and enrich passes (#283)
- Simplify enrich.rs tree-walkers and eliminate duplication (#286, #299)
- Decompose method dispatch and extract helpers (#282)
- Unify let/var drop registration and extract helpers (#280)
- Extract shared overflow-policy logic from mailbox send functions (#334)
- Replace LIVE_ACTORS linear scan with HashMap index (#325)
- Auto-discover E2E tests and consolidate test categories
- Strip `hew_` prefix from internal connection helpers (#331)

## [0.2.0] - 2026-03-15

### Added

- QUIC transport for inter-node messaging in the Node mesh (#152, #163)
- Happy Eyeballs (RFC 8305) TCP connector in adze package manager (#162)
- Distributed reply channels for remote `await` across nodes (#169)
- Cross-node registry gossip for `Node::lookup` (#168)
- Remote actor dispatch through the Node mesh
- Two-process QUIC mesh demo with transport cleanup
- Node API builtins wired through typechecker and MLIR codegen
- Comprehensive doc comments added to stdlib modules; improved doc renderer template and markdown rendering (#170)
- Duration as a distinct primitive type (#104)
- `hew-analysis` crate extracted; `hew-wasm` brought to feature parity with `hew-lsp` (#111)
- Stdlib gap fill: `string.split`/`lines`/`join`, filesystem directory helpers, URL encoding, HTTP response accessors (#132)
- Schema version field added to MessagePack AST boundary (#124)
- Grammar fuzzer for TextMate and tree-sitter grammars (#120)
- Centralized downstream generator and sync script for editor integrations
- Duration type design document and implementation plan

### Fixed

- Allow `await` on void receive handlers (#156, #159)
- Mark module as used when spawning `module.Actor()` (#160)
- MLIR codegen lowering for `to_float()` builtin (#158)
- Forward direct SIGTERM/SIGINT to `hew run` child process (#151)
- Emit error instead of silently defaulting `None` to `Option<i32>` (#115)
- Eliminate silent type fallbacks in codegen (#117)
- Close type-checker inference gaps found in audit (#116)
- Improve integer literal type inference and coercion (#114)
- Array literal to `Vec` coercion for enum variant elements (#112)
- Ecosystem package resolution for `--pkg-path` and lib search; module resolution and actor field access in codegen (#146)
- Suppress unused-import warnings for sub-module imports
- Replace catch-all match arms in `enrich.rs` with exhaustive variants (#122)
- Eliminate silent fallbacks in deserializer, codegen, and parser (#129)
- Convert warnings to errors and remove dead string dispatch (#126)
- Scope `zlib`/`zstd` static link and `-static-libstdc++` to Linux only
- Static-link `zlib`/`zstd`/`libstdc++` in hew-codegen; simplify distro packaging
- Strip phantom z3 dependency from hew-codegen; add multi-distro test script
- Correct release packaging — stdlib sources, static libs, and codegen static build
- Resolve all pre-existing Clippy warnings in workspace (#133)
- Remove stale `isolated`/`and`/`or` keywords from tmLanguage generator (#131)
- Refactor `convertType` validation into `convertTypeOrError` helper (#119)
- Unused import fixes (#140, #141)

### Changed

- Migrate actor examples away from sleep-based synchronization (#157)
- Homogeneous module resolution and stdlib pure-Hew migration (#128)
- Observer TUI polish — theme, clamping, and UX fixes (#130)
- Deduplicate `Ty` mapping and add FFI symbol verification (#127)
- Replace raw `is_null()` + return boilerplate with `cabi_guard!` macro (#125)
- Unify `vecElemSuffix` and `vecElemSuffixWithPtr` into a single function (#123)
- Canadian English `-our` spellings adopted across codebase (#113)
- Sync spec and grammars to v0.9.0 against implementation (#103)
- Prune superseded lessons and consolidate duplicates (#121)
- Remove stale plan docs for completed features (#110)

## [0.1.9] - 2026-03-07

### Added

- FreeBSD platform support (x86_64-freebsd compilation target)
- FreeBSD CI and release pipelines
- kqueue-based I/O poller for FreeBSD and macOS (replaces epoll on those platforms)

### Fixed

- WASM runtime: fix HewError import path
- Runtime: handle ask send failures explicitly (prevents hangs on failed sends)
- Serializer: report unsupported inferred types instead of silently dropping them
- Serializer: harden type conversion diagnostics in enrich pass

### Changed

- Refactor: consolidate Linux/FreeBSD ELF linker flags into shared path
- Refactor: extract shared `exe_suffix()` helper across CLI
- Refactor: extract shared signal recovery logic
- Refactor: remove dead `linkExecutable` from hew-codegen

## [0.1.8] - 2026-03-06

### Added

- Numeric literal type coercion: integer and float literals automatically coerce to the expected type with range validation (e.g., `var t: i32 = 4` works without explicit cast)
- Untyped const coercion: `const N: Int = 10` can be used as i32, u8, etc.
- `indirect enum` for recursive data types (expression trees, linked lists, etc.) with automatic heap allocation and RAII cleanup
- Named supervisor child access via field syntax (`sup.child_name` resolves to `supervisor_child(sup, idx)` at compile time)
- WASM platform capability documentation
- `s.spawn {}` parallel task syntax for structured concurrency
- Custom type indexing via `get()` method (`obj[key]` desugars to `obj.get(key)`)
- HashSet data structure with insert, contains, remove, len operations
- Granular visibility modifiers: `pub(package)` and `pub(super)`
- Bare `self` parameter in methods (no type annotation required)
- Label support on `for` loops (@label: for ...)
- Char literal support (`'a'`, `'\n'`, escape sequences)
- Associated type declarations inside `impl` blocks, including trait defaults and `Self::Alias` resolution
- `Self::Type` syntax in type position for associated type references
- Negative literal patterns in match expressions (-1, -3.14)
- `if let` conditional pattern syntax
- Array repeat syntax `[value; count]` for initializing arrays
- Struct-like enum variants with named fields (Variant { field: Type })
- Trait bound enforcement at call sites for generic functions
- Unsafe block enforcement: extern FFI calls require `unsafe { }` wrapper
- Multi-trait dyn objects: `dyn (Trait1 + Trait2)`
- Range expressions as first-class values (variable-bound ranges in for loops)
- Timeout expression codegen support (`expr | after duration`)
- Generic lambda syntax support (<T>(x: T) => expr)
- Coroutine support for aarch64 (ARM64) architecture
- String predicate methods: `.is_digit()`, `.is_alpha()`, `.is_alphanumeric()`, `.is_empty()`
- `String.lines()` method: split string on newlines (strips `\r`) returning `Vec<String>`
- `Vec<String>.join(sep)` method: join elements with separator string
- `Vec<T>.map((x) => expr)` method: transform each element, returns new `Vec<U>`
- `Vec<T>.filter((x) => expr)` method: keep elements where closure returns true, returns new `Vec<T>`
- `Vec<T>.fold(init, (acc, x) => expr)` method: reduce to a single value
- LLVM coroutine-based `gen fn` codegen: `yield` inside `while`/`for`/`loop` now works correctly (loop variables preserved across yields)
- `HashMap.keys()` method: return `Vec<K>` of all keys (type checker; codegen already existed)
- `join` is now a contextual keyword, usable as a method name in dot-call position

### Fixed

- Type checker: reject match arms using variants from wrong enum type
- Type checker: bare `return;` in non-unit functions now produces error
- Type checker: `dyn (A + B)` now unifies with `dyn (B + A)` (order-independent)
- Type checker: match statements now check exhaustiveness (missing variants warn)
- Type checker: `Self` in generic impls now resolves to full type (e.g., `Pair<T>` not bare `Pair`)
- Type checker: `dyn Trait<Args>` method dispatch now substitutes bound type args into signatures
- Type checker: lambda arity mismatch now detected (1-param lambda can't pass as `fn(int,int)->int`)
- Type checker: OR-patterns (`Some(x) | None`) now counted in exhaustiveness checks
- Type checker: guarded wildcard/identifier patterns no longer count as exhaustive (`_ if false => ...` warns)
- Parser: missing parameter type annotation now reports error instead of silent drop
- Parser: `pub(invalid)` now defaults to private instead of silently promoting to pub
- Parser: string interpolation sub-parser errors now propagated to parent
- Parser: empty struct literal `Foo {}` now parses correctly for zero-field structs
- Parser: invalid escape sequences now report error instead of silent failure
- Parser: positional args after named args now skipped instead of producing malformed AST
- Codegen: string ordering operators (`<`, `<=`, `>`, `>=`) now use lexicographic comparison instead of pointer comparison
- Codegen: `if let` statements now fully implemented (pattern matching with variable binding)
- Codegen: array repeat expressions `[val; count]` now generate Vec with runtime loop
- Codegen: struct variant patterns as last match arm now check tag (fixes UB with wrong variant)
- Codegen: `if let` bodies with `return`/`break`/`continue` now correctly guard subsequent code
- Codegen: lambda capture analysis now respects pattern-bound variables in match/for/if-let
- Codegen: `ExprIfLet` and `ExprArrayRepeat` inside lambdas now correctly tracked for capture analysis
- Codegen: `..=` inclusive range now accepts all integer widths (was only i64/index)
- Codegen: ordering operators on actor pointers now emit error instead of calling string compare (prevents UB)
- Codegen: indexed compound assignment (`v[i] += 1`) now applies the operator (was silently dropping it)
- Codegen: HashSet insert/contains/remove no longer double-evaluate argument expressions
- Codegen: Vec<bool> now uses consistent runtime suffixes (fixes data corruption)
- Codegen: tuple patterns in match expressions now destructure correctly (was silently skipped)
- Codegen: return statements now evaluate expression before dropping locals (fixes use-after-free)
- Codegen: labeled break/continue now deactivates ALL intermediate loops (fixes infinite loop with 3+ nesting)
- Codegen: labeled break/continue now drops resources in ALL intermediate scopes (fixes leaks)
- Codegen: scope binding (`scope |s| { }`) now declares variable for body access (fixes scope_spawn)
- Codegen: scope spawn now captures mutable variables via heap cells (cross-task mutation works)
- Codegen: compound assignment switches now have default case (prevents UB)
- Codegen: IfLet, ArrayRepeat, generic lambda type_params now deserialize without crash
- Codegen: TypeExpr::Infer now deserializes in C++ (was crashing with unknown variant)
- Runtime: HashMap/Vec string getters return owned copies (prevents use-after-free)
- Runtime: HashMap strdup calls now abort on NULL (prevents silent corruption on OOM)
- Runtime: `hew_string_compare` added for correct lexicographic string ordering
- Codegen: f32 values now print correctly (promoted to f64, was falling through to i32 printer)
- Codegen: f32↔f64 float coercion now handled via ExtFOp/TruncFOp (was missing)
- Codegen: nested constructor patterns like `Some((a, b))` now destructure tuple payloads
- Codegen: `char_at` index extension uses zero-extend (prevents signed misinterpretation)
- Type checker: type variables resolved before pattern matching (fixes false mismatches on generics)
- Type checker: unknown fields in struct patterns now report `UndefinedField` error with suggestions
- Codegen: for-loop over stored ranges uses ExtSIOp instead of IndexCastOp (fixes MLIR verification)
- Codegen: for-loop over stored ranges now uses continue guards and MutableTableScopeT
- Codegen: constructor pattern guards now bind PatTuple sub-patterns (e.g., `Some((a,b)) if a > 0`)
- Codegen: `loop {}` now checks returnFlag before re-entering body (fixes infinite loop on return)
- Codegen: `var` declaration with failed expression no longer leaks pendingDeclaredType into subsequent expressions
- Codegen: stream/generator/for-await loops now respect `continue` via continue guards
- Codegen: log emit now drops temporary string after hew_log_emit call (fixes leak)
- Runtime: integer overflow checks in string replace_all, string repeat, and hashmap resize (prevents UB)
- Parser: `expect()` and `parse_identifier()` no longer panic on unexpected EOF (returns error)
- Serialization: `rewrite_builtin_calls` now traverses all expression variants (InterpolatedString, PostfixTry, Await, Yield, Send, Range, Unsafe, Join, Timeout, ScopeLaunch, ScopeSpawn, Scope, SpawnLambdaActor, Match, Lambda, Spawn, StructInit, Select)
- Zero compiler warnings across entire Rust workspace
- All 333 codegen e2e tests pass (up from 321)
- Codegen: log emit double-free fixed — non-string args no longer freed twice in ownedTemps cleanup
- Codegen: labeled loop flags (activeFlags/continueFlags) now cleaned up in all 5 for-loop variants
- Codegen: `for await` stream loops now support labeled break/continue
- Codegen: or-pattern with enum unit variants now generates correct tag comparison (was always-true)
- Codegen: inclusive range `..=` now accepts all integer widths (was only i64/index)
- Codegen: range type mismatch between start and end now coerced (was silently ignored)
- Runtime: Vec strdup calls abort on NULL (OOM safety, consistent with HashMap)
- Serialization: normalization now covers Trait, TypeBodyItem::Variant, Const, TypeAlias items
- Codegen: ToStringOp now promotes f32 to f64 before calling hew_float_to_string (fixes garbled output)
- Codegen: AssertOp/AssertEqOp/AssertNeOp now handle i8, i16, and f32 types (fixes ABI mismatch)
- Codegen: VecNewOp struct layout now correct for f32 fields (was using 8-byte size instead of 4)
- Codegen: lambda capture analysis now covers Spawn, SpawnLambdaActor, Scope, ScopeLaunch, ScopeSpawn, Select, Join, Range, Timeout, Yield, Unsafe expressions
- Codegen: Vec<bool>/Vec<i8>/Vec<i16> inline push/get/set now use correct i32 element stride (fixes memory corruption)
- Codegen: SleepOp saturates i64→i32 truncation at INT32_MAX (prevents silent wrap)
- Codegen: Vec<f32> now uses \_f64 runtime path with f32↔f64 promotion/truncation (fixes crash)
- Codegen: VecPop now handles f32 and narrow int return type conversion
- Codegen: Vec push/get/set/pop fallback paths now promote/truncate for f32 and narrow ints
- Codegen: trait object default value uses null pointer for vtable (was i32(0), type violation)
- Codegen: collectFreeVarsInStmt now handles StmtMatch, StmtBreak, StmtDefer
- Runtime: string concat overflow check (checked_add before malloc)
- Runtime: string split NULL check after malloc_cstring
- Runtime: TCP framing overflow check before u32 cast
- Codegen: VecRemoveOp now promotes i1/i8/i16 to i32 and f32 to f64 (fixes type mismatch in runtime calls)
- Codegen: HashMapInsertOp now promotes f32 to f64 and i1/i8/i16 to i32 (fixes silent miscompile for narrow types)
- Codegen: HashMapGetOp now declares correct return type and narrows result (i32→i1/i8/i16, f64→f32)
- Codegen: PrintOp now emits error for unhandled types instead of silent i32 fallback
- Runtime: added hew_vec_remove_f64 for Vec<f64>/Vec<f32> remove-by-value
- Codegen: return inside loop body now sets continueFlag (prevents side effects after return in same iteration)
- Codegen: labeled break across 3+ nesting levels now sets continue flags for ALL intermediate loops
- Codegen: AssertEqOp/AssertNeOp fallthrough replaced with explicit i64/index check + error for unknown types
- Codegen: unhandled match pattern now emits error instead of silent skip with warning
- Runtime: Vec append overflow check (checked_add before ensure_cap)
- Codegen: HashMap.get() now returns Option<T> at MLIR level (fixes let-binding + match pattern)
- Codegen: non-exhaustive match now traps at runtime instead of silently returning zero
- Type checker: non-exhaustive match warning now covers all types (int, float, string), not just enums
- Runtime: Vec push functions use checked_add for overflow protection
- Runtime: Vec append validates elem_size/elem_kind match before memcpy
- Runtime: added hew_vec_set_ptr and hew_vec_pop_ptr for pointer-type vectors
- All 335 codegen e2e tests pass (up from 321)
- Type checker: implicit integer narrowing (e.g., i64→i32) now rejected; only widening allowed
- Type checker: Vec/HashMap/HashSet/String method indices and lengths use int (i64) instead of i32
- Type checker: array/index expressions check index against int (i64) instead of i32
- Codegen: StmtReturn now included in stmtMightContainBreakOrContinue guard (fixes incorrect SCF yield)
- Codegen: LitChar uses char32_t for full Unicode codepoint preservation
- Codegen: msgpack char deserializer decodes multi-byte UTF-8 sequences
- Codegen: var reassignment now drops old owned value (prevents memory leak for String/Vec/HashMap)
- Codegen: function argument coercion now passes isUnsigned flag (u32→u64 uses extui not extsi)
- Codegen: `generateLiteral()` now emits literals at correct MLIR width from resolved type (was always i64/f64)
- All 338 codegen e2e tests pass (100%, up from 263/335 = 79%)

### Changed

- **BREAKING**: Collection indices and lengths are now `int` (i64) instead of `i32`
- Test files updated: 80+ test files changed from i32 to int for function signatures and variables

- Function call results can be silently discarded — no more `let _ =` required
- Parser: deduplicated function modifier handling (extracted `parse_fn_with_modifiers`)
- Improved WASM target error messages for unsupported concurrency operations
- **Breaking**: `HashMap.get(key)` now returns `Option<T>` instead of raw `T` — use `match` to unwrap

## v0.1.5 — 2026-02-28

### Added

- 112 algorithm and data structure examples (sorting, searching, graphs, trees, heaps, etc.)
- 57 Go-comparison benchmarks with ops/sec measurements
- Inline Vec get/set/len/push for primitive types (2–5× speedup on native)
- Inline string `char_at` (direct GEP+load with bounds check)
- While-loop invariant hoisting (pre-evaluates loop-invariant conditions)
- Inline Vec push fast path (store+len-increment when capacity allows)
- `<-` send operator now works correctly in codegen

### Fixed

- Break stack desync in `for..in` Vec/HashMap loops (loopBreakValueStack push/pop)
- `hew_actor_free` manifest ABI mismatch (void → i32)
- Vec `<String>` double-free (exclude VecGetOp/HashMapGetOp from temporary drop)
- HashMap.get() match wraps raw value in Option for Some/None patterns
- WASM32 inline lowering: skip on non-64-bit targets (struct layout mismatch)
- Windows `libc::write` type mismatch in string abort handler
- Silent codegen fallbacks replaced with warnings/errors
- Parser silent token skips replaced with error messages

### Changed

- Converted all counting `while` loops to idiomatic `for i in 0..N` range syntax
- Code audit: 25 Mutex poison-recovery fixes, 5 unsafe UTF-8 fixes, ~70 Clippy warnings
- Parser deduplication: extracted `contextual_keyword_name()` and `collect_doc_comments_with_prefix()`

## v0.1.4 — 2026-02-26

### Fixed

- Eliminate false 'function is never called' warnings
- Properly static-link MLIR/LLVM in hew-codegen release builds
- Resolve all codegen test failures and WASM build warnings
- Windows test compatibility and end-to-end build pipeline

### Changed

- Simplify over-engineering: parser, runtime, std libraries

## v0.1.3 — 2026-02-25

### Fixed

- Resolve all codegen test failures and WASM build warnings

### Changed

- Bump version to 0.1.3

## v0.1.2 — 2026-02-24

### Added

- Windows x86_64 build support (#13)
- Distributed actors v2: bridge, transport synchronization, cluster membership (#14)
- Distributed observer HTTP API endpoints
- Distributed TUI: Cluster, Messages, and Timeline tabs

### Fixed

- Runtime: actor sync hardening, free wait
- Runtime: transport connection slot synchronization
- Runtime: close transport before dropping connection actor
- Runtime: supervisor delayed restart safety
- Runtime: pool lifecycle and mailbox sys backlog
- Runtime: hew_node start cleanup and actor ID encoding
- Wire varint validation
- Cluster membership message handling hardening
- Allowlist strict mode and zeroize key copies
- Bridge.rs post-rebase build errors

## v0.1.1 — 2026-02-23

### Fixed

- Release packaging: adze completions, RPM spec, Docker image, macOS signing
- macOS notarization: switch from Apple ID to App Store Connect API keys
- Non-Linux builds: continue-on-error for cross-platform CI
- Secret checks: use job-level env vars for signing conditions

## v0.1.0 — 2026-02-22

**Hew** is a statically-typed, actor-oriented programming language for concurrent and distributed systems. It features Erlang-inspired supervision trees, first-class async/await, and message-passing concurrency — compiled to native code via the Rust hew-codegen-rs backend over LLVM (inkwell).

### Added

#### Language

- Full compilation pipeline: `.hew` → Rust frontend → hew-codegen-rs → LLVM → native binary
- Core language: functions, variables (`let`/`var`), control flow (`if`/`else`, `while`, `for`, `loop`), match expressions, closures/lambdas (including mutable capture), generics, traits with vtable dispatch, tuples, string interpolation (f-strings), range expressions, `defer`
- Actors: `spawn`, `send`, `receive`, `ask`/`await`, lambda actors
- Supervision trees: `supervisor` keyword with `one_for_one`, `one_for_all`, `rest_for_one` strategies
- Fault propagation: `link`, `monitor`, `unlink`, `demonitor`
- Actor priorities and mailbox policies (`block`, `drop_new`, `drop_old`, `coalesce`)
- Collections: `Vec<T>`, `HashMap<K,V>`, `bytes`
- Structured concurrency with `scope` and `launch`
- Streams: `stream.channel()` returning `(Sink, Stream)` tuples, `stream.pipe()`, `for await` loops, file-backed streams with `Result` error handling

#### Compilation Targets

- x86_64-linux
- x86_64-macos
- aarch64-macos
- wasm32-wasi (single-threaded programs only)

#### Standard Library

- **Standard:** `std::fs`, `std::log`, `std::os`, `std::net`, `std::encoding::json`, `std::text::regex`, `std::process`, `std::misc::uuid`, `std::time::datetime`, `std::net::url`, `std::path`, `std::encoding::base64`, `std::encoding::hex`, `std::crypto::crypto`, `std::encoding::compress`, `std::stream`
- **Extended:** `std::net::http`, `std::crypto::jwt`, `std::encoding::yaml`, `std::encoding::toml`, `std::encoding::csv`, `std::encoding::msgpack`
- **Ecosystem (separate repo):** `db::postgres`, `db::redis`, `db::sqlite`, `misc::glob` — see [hew-lang/ecosystem](https://github.com/hew-lang/ecosystem)

#### Tooling

- **LSP**: diagnostics, completion, hover, symbols, semantic tokens
- **VS Code extension**: syntax highlighting and language support
- **REPL**: `hew eval` for interactive expression evaluation
- **Test runner**: `hew test` for `.hew` test files
- **Doc generator**: `hew doc` for generating documentation from source
- **Package manager**: `adze` — init, install, publish, search, with single-fallback registry resilience
- **Observability**: `hew-observe` TUI for live actor inspection (connects to runtime profiler endpoint)

#### Installation

- Shell installer (`curl | bash`)
- Homebrew
- Docker
- Debian, RPM, Arch, Alpine, and Nix packages
- Shell completions for bash, zsh, and fish

#### Developer Experience

- Clear, actionable compiler error messages for common mistakes

### Known Limitations

- WASM target is single-threaded: basic actors (spawn, send, ask/await) work, but supervision trees, link/monitor, and scoped concurrency are not supported
- No incremental compilation
- `unsafe` blocks are parsed but not yet enforced (treated as regular blocks)

### Getting Started

```bash
# Install Hew
curl -fsSL https://hew.sh/install | bash

# Create and run your first program
echo 'fn main() { println("Hello from Hew!"); }' > hello.hew
hew run hello.hew

# Try the REPL
hew eval

# Start a new project
hew init my_project
```

Visit [hew.sh](https://hew.sh) for documentation and examples.
