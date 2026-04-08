# Distributed Actor Infrastructure — Journey Log

## Module System Reliability (2026-03-29)

### Analysis

Traced the full import pipeline (parser → CLI resolver → module graph → flattening → typechecker → codegen). The architecture has a fundamental design pattern that causes bugs: imported module items are processed by EVERY codegen pass, and each pass must independently handle cross-module mangling. Missing the swap in any pass creates name mismatches.

PR #386 fixed the most visible instance (`generateImplDecl` and Pass 1d/1j), but Pass 1f (Drop impl pre-registration) was missed. The pre-registration at line 2330 stores `mangleName(currentModulePath, ...)` while the actual generation at line 3444 stores `mangleName(typeDefModulePath, ...)`, creating a mismatch in `userDropFuncs`.

The durable outcome was to treat defining-module mangling as a per-pass
invariant across codegen, not as a one-off fix in the visibly failing pass.

## Phase 8: Completion recursion through expression containers (2026-03-25)

### Goal

Fix local-variable completion lookup when the cursor sits inside a block nested under
an expression statement, such as a call argument or method-call receiver.

### Decisions

- Reproduced the bug against `hew-analysis/src/completions.rs` and confirmed the walk
  stopped at `Stmt::Expression`, so nested block scopes inside expression statements
  were never visited.
- Added a span-checked helper for recursing through child expressions so pass-through
  containers like calls, method calls, tuples, arrays, and string interpolations only
  descend into the branch that actually covers the cursor.
- Added focused completion tests for positive and negative scope behaviour: locals are
  visible inside nested call/receiver blocks and do not leak after the expression
  statement finishes.

## Phase 8: Runtime shutdown test isolation (2026-03-24)

### Goal

Keep the consolidation branch green after the Phase 8 cherry-picks by making the
shutdown tests independent under `cargo test --workspace`.

### Decisions

- Reproduced the workspace failure, then re-ran the two failing shutdown tests in
  isolation and serially to confirm the assertions only failed under parallel test
  execution.
- Added a test-only mutex plus reset helper in `hew-runtime/src/shutdown.rs` so
  every shutdown test starts from a clean phase/supervisor state and cannot race
  other tests mutating the same globals.
- Kept the fix inside the existing shutdown test module instead of adding a new
  dependency, because the problem is localized to one group of stateful tests.

## Phase 7–8: Deduplication wave (2026-03-24)

A concentrated cleanup pass on 2026-03-24 removed repeated helper logic without
changing behavior at the semantic boundary:

- Parser trait-bound parsing now shares `parse_optional_super_traits()` and
  `parse_trait_bound_list()` so trait, actor, generic, associated-type, and
  where-clause bounds stay aligned.
- `hew-cabi` now owns the shared lossy C-string conversion used by the runtime
  bridge, keeping UTF-8 rejection and lossy decoding as separate explicit
  helpers.
- `hew-wasm` uses one local `parse_and_type_check()` scaffold instead of
  repeating parse-plus-analysis setup in `hew-wasm/src/lib.rs`.
- CLI helper duplication was removed in `adze-cli`, the REPL/test runner
  `find_hew_binary()` logic, `hew-types` actor field binding, and
  `hew-serialize`'s top-level `TypedProgram` wrappers so future behavior changes
  land in one place.
- String escape decoding is shared between plain and interpolated strings, with
  interpolation-only escapes kept explicit at the call site.
- Wire-field parsing now shares field/modifier parsing and outer JSON/YAML name
  extraction while leaving auto-numbering semantics with the specific parser
  path that owns them.
- Runtime profiler snapshots reuse the same small JSON array/string helpers
  while keeping per-record formatting local.

## Phase 7–10: Dead-code trim and surface cleanup (2026-03-24)

The same wave also deleted no-op or superseded surfaces instead of leaving
compatibility shims behind:

- `hew-types` helper constructors and unqualified `ModuleRegistry` predicates
  that only self-tests still referenced were removed; display-oriented tests
  were retargeted to `TypeError::new(...)`, and the cleanup stayed covered by
  `cargo test -p hew-types` plus `cargo clippy -p hew-types -- -D warnings`.
- `hew --Werror`, `adze init --bin`, and `adze namespace list` were removed so
  help text, specs, and completions match the live CLI.
- The unused QUIC `load_ca_cert()` placeholder and its placeholder test were
  deleted.
- The public `SinkBacking` trait was replaced with inherent `HewSink` methods
  and constructor helpers so the shared ABI keeps exact-item and explicit-close
  behavior without exposing a bespoke trait.
- The dead `hew-stdlib-gen` / export metadata pipeline was removed after
  confirming stdlib loading now comes directly from source trees.

## Phase 9: Slim stdlib packaging (2026-03-15)

### Goal

Shrink release distributions by removing runtime object duplication from every
stdlib static library before assembling tarballs.

### Rationale

- Cargo `staticlib` archives embed transitive object files, so every
  `libhew_std_*.a` was shipping runtime support objects that were already
  present in `libhew_runtime.a`.
- The linker flags that suppressed duplicate definitions were hiding that waste
  and could also mask real symbol-collision bugs.
- Repacking each stdlib package as a thin archive over only its non-runtime
  objects keeps runtime-first link order unchanged while making release
  artifacts much smaller.

## Phase 8: Runtime observe wiring (2026-03-15)

### Goal

Replace hew-observe demo-only Messages, Timeline, Supervisors, and Crashes data with
live runtime data from the actor scheduler and profiler HTTP server.

### Changes made

- Wired `hew_trace_begin`/`hew_trace_end` into the scheduler dispatch loop so each
  dispatched message now produces real span timing events.
- Captured trace context in mailbox nodes at enqueue time and restored it before
  dispatch so trace IDs survive actor-to-actor message hops.
- Emitted lifecycle events for actor spawn, crash, stop, and message send from the
  runtime paths that already own those transitions.
- Enabled tracing automatically when `HEW_PPROF` starts the profiler so
  `hew-observe --addr ...` shows data without an extra tracing flag.
- Added `/api/supervisors` and `/api/crashes` endpoints and taught hew-observe to
  fetch them instead of relying on demo placeholders.

## Phase 0.5: `hew test` hardening audit (2026-03-15)

### Findings

- **Silent parse failures:** malformed `_test.hew` files were reported as
  "No test functions found." with exit code 0 because discovery discarded parser
  diagnostics.
- **Non-deterministic execution order:** discovered tests were grouped in a
  `HashMap`, so multi-file runs printed results in unstable order.
- **Ambiguous empty-suite reporting:** an empty directory and a valid file with no
  `#[test]` functions produced the same message.
- **Timeouts existed but were fixed at 30 seconds:** the runner supported timeouts
  internally, but there was no CLI control, which made the behaviour harder to
  exercise and slower to test.

### Decisions

- Treat parser errors during discovery as fatal test-runner diagnostics and exit
  non-zero before executing the suite.
- Preserve discovery order all the way through execution and reporting so repeated
  runs are stable and trustworthy.
- Distinguish "No test files found." from "No test functions found." for clearer UX.
- Add `--timeout <seconds>` so timeout behaviour is explicit, configurable, and
  practical to test end-to-end.

## Phase 0: Multi-Agent Audit (2026-02-24)

### Key findings

**Runtime (33,851 LOC across 69 files):**

- 2,375 unsafe blocks; 796 without SAFETY documentation
- 314 instances of duplicated C-ABI null-guard pattern across 47 files
- Wire envelope send path duplicated in 3 modules (~90 lines each)
- Critical: `connection.rs:334` passes stack pointer to spawned thread → potential UB
- Noise XX encryption exists but has no key management, identity, or authorization
- SWIM cluster membership exists but is not wired to connection management or routing
- Local name registry exists but no cross-node propagation

**Codegen (C++):**

- ~200 LOC of duplicated string-based type dispatch (now superseded by typed MLIR ops)
- Large monolithic functions (generateCallExpr, generateForStmt) need decomposition
- ~300 LOC of dead string-parsing code removable

**Distributed actors:**

- Transport: TCP/Unix with length-prefixed framing, pluggable vtable
- Security: Noise XX only, no identity/auth model
- Discovery: SWIM gossip exists but isolated from routing
- Supervision: Local trees work; remote_sup.rs is a skeleton
- Node API: Type checker declares Node::\* builtins but codegen doesn't lower them
- Example explicitly marks distribution as "FUTURE FEATURE"

**API contracts:**

- 🔴 Unversioned msgpack schema between Rust serializer and C++ codegen
- 🔴 Hardcoded HewActor byte offsets (MAILBOX_OFFSET=48) in bridge.rs
- 🔴 ast_types.h manually mirrors ast.rs with no sync mechanism
- No formal ABI specification for stdlib FFI

### Design iteration 1: Scope definition

The work divides into two tracks:

**Track A — Runtime Hardening (pre-production quality)**

1. Fix critical safety issues (connection.rs dangling pointer, undocumented unsafe)
2. Deduplicate C-ABI boilerplate with macros
3. Unify wire send/recv paths
4. Add safety documentation to all unsafe blocks
5. Version the msgpack schema

**Track B — Distributed Actor Infrastructure**

1. Unify node runtime (transport + connmgr + cluster + registry)
2. Connection handshake protocol (version, node ID, schema hash, features)
3. Node API codegen lowering (Node::start/register/lookup)
4. Security: key management, peer authorization, allowlisting
5. Remote actor lifecycle (spawn, monitor, restart)
6. Actor pools with routing strategies

### Design iteration 2: Architecture decisions

**Decision 1: Single Node struct as integration point**
Rather than keeping transport.rs, connection.rs, cluster.rs, and registry.rs as
independent modules, we unify them under a `HewNode` that owns all distributed
state. The Node is created by `Node::start(addr)` and torn down by `Node::shutdown()`.

**Decision 2: Connection handshake before actor traffic**
Every new connection exchanges:

```
HandshakeReq {
  protocol_version: u32,    // HBF version
  node_id: u16,             // PID node component
  schema_hash: u64,         // FNV hash of wire schema
  features: u32,            // bitfield: encryption, compression
  static_key: [u8; 32],     // Noise static public key (if encrypted)
}
HandshakeResp {
  protocol_version: u32,
  node_id: u16,
  schema_hash: u64,
  features: u32,
  static_key: [u8; 32],
  status: u8,               // 0=ok, 1=version_mismatch, 2=schema_mismatch
}
```

**Decision 3: Route by (node_id, actor_id), not by connection handle**
Actor references encode `(node_id, actor_id)`. The node maintains a routing table
`node_id → conn_id`. References survive reconnections.

**Decision 4: Security model — Noise XX + allowlist**

- Every peer has a static Noise keypair (generated or loaded from file)
- Nodes maintain an allowlist of trusted static public keys
- Noise XX handshake authenticates both sides
- Post-handshake: all traffic encrypted with ChaChaPoly
- Future: SPIFFE integration as an alternative identity source

**Decision 5: Service discovery via gossip**

- Extend SWIM gossip to carry service registrations
- `Node::register(name, actorRef)` publishes to local + gossip
- `Node::lookup(name)` checks local registry, then queries cluster
- Registrations have TTL; nodes must re-register periodically

## Phase 1: Critical Safety Fixes

**Commit:** b5d7481

- **Dangling pointer UB fixed** in connection.rs: Replaced raw `AtomicU64` pointer passed
  to spawned reader thread with `Arc<AtomicU64>`. The original code captured a pointer to a
  stack-local field, then moved the owning struct, creating a dangling pointer.
- **Hardcoded byte offsets replaced** in bridge.rs: `MAILBOX_OFFSET=48` and
  `ACTOR_STATE_OFFSET=56` replaced with `std::mem::offset_of!(HewActor, ...)` plus
  compile-time assertions.
- **Wire send path unified**: Extracted `wire_send_envelope()` in transport.rs, eliminating
  ~90 LOC of triplicated envelope encode+send logic from node.rs and connection.rs.
- **SAFETY documentation added** to critical unsafe blocks in string.rs, vec.rs, actor.rs.

## Phase 2: Code Deduplication

**Commit:** 8162756

- **`cabi_guard!` macro** created to replace null-check boilerplate: applied to 43+ sites
  across vec.rs, string.rs, hashmap.rs.
- **TCP/Unix accept loops** unified via `accept_with_optional_timeout` helper in transport.rs.
- **Tagged-union module** created (`tagged_union.rs`) factoring shared Option/Result
  construction/destruction patterns.
- Bonus: `cargo fmt` applied across workspace.

## Phase 3: Codegen Cleanup

**Commit:** cc1f1bc

- **Extracted `generateBuiltinMethodCall`** from monolithic method-call path in
  MLIRGenExpr.cpp. Clean function boundary using `std::optional<mlir::Value>`.
- **Split `generateForStmt`** into `generateForRange`, `generateForVec`,
  `generateForString`, `generateForHashMap` helpers in MLIRGenStmt.cpp.
- **Removed ~270 LOC dead string-based type dispatch** fallbacks superseded by typed MLIR ops.
- Caught and fixed a regression where the extraction refactor re-introduced a `CmpIOp` for
  contains_key that changed the return type from i32 to i1.

## Phase 4: Unified Node Runtime

**Commit:** 5e00c71

- **HewNode struct** created in `hew_node.rs` — integrates transport, connection manager,
  SWIM cluster, and registry into a single coherent entry point.
- **48-byte connection handshake** protocol: magic/version/node_id/schema_hash/feature_flags/
  noise_key. Automatic Noise XX upgrade when both peers support encryption.
- **PID-based routing table** in `routing.rs`: thread-safe `RwLock<HashMap<u16, c_int>>`
  mapping node_id → connection. Messages routed by destination PID, surviving reconnections.
- **SWIM ↔ connection lifecycle bridge**: connection lost triggers suspect, connection
  established triggers alive. Membership callbacks for event-driven integration.

## Phases 5-6: Node API, Security & Identity

**Commit:** c03dd10

- **Runtime manifest updated**: 9 `hew_node_*` functions added for codegen integration.
- **Integration tests**: Node lifecycle, local registry, routing table, two-node connect.
- **Noise key management**: `hew_noise_keygen/key_save/key_load` with X25519 keypairs,
  raw 64-byte file format, 0o600 permissions on Unix.
- **Peer allowlist**: `HewPeerAllowlist` with Open/Strict modes, integrated into Noise
  handshake for post-authentication peer verification.

## Phase 7: Supervision & Pools

- **Actor pools**: Round-robin and random routing strategies via `HewPool`.
- **Remote supervisor**: `remote_sup.rs` wired to unified node lifecycle.
- **Distributed tests**: Multi-node spawn, supervision tree, pool routing.

## Phase 8: v2 Audit & Hardening (2026-02-25)

### Second-round audit findings

A second-round audit against the Phase 7 codebase found several issues the
first rollout had missed:

**Critical findings:**

- **PID/id dual identity**: `HewActor.pid` was a plain counter while `id` was
  location-transparent `(node_id<<48|serial)`. `hew_actor_self_pid()` returned the
  counter, breaking all cross-node routing. Fixed by unifying: `pid = id` at all 6
  spawn sites.
- **Noise used ephemeral keys only**: Persistent static key was never bound to the
  transport session, making Noise XX authentication meaningless. Fixed by threading
  persistent key through handshake and validating remote static against allowlist.
- **Allowlist never enforced**: The check function existed but was never called during
  handshake. Now wired into post-handshake validation.
- **Connection drops didn't clean up**: Reader loop exit didn't remove routes, notify
  SWIM, or clean connmgr. Now wired: drop → remove route → SWIM notify → connmgr
  cleanup.

**High-priority fixes:**

- Deleted orphan `node.rs` (317 LOC dead code with conflicting symbols).
- Added thread-local `set_last_error`/`hew_last_error` C-ABI error diagnostic API.
- Converted 20+ panic-in-runtime paths to proper error returns (actor_group, link,
  mailbox, registry).
- Synced `runtime_manifest.json` with actual exported functions.
- Schema hash validation in handshake (reject incompatible peers).
- Connection reconnection with exponential backoff.
- Bounded outbound message queue for backpressure.
- **Wired remote send routing**: `hew_actor_send_by_id` now falls through to
  `try_remote_send → hew_node_send` when target PID is non-local, completing the
  end-to-end distributed message path.
- Added `hew_node_unregister` C-ABI function.
- Expanded `cabi_guard!` to 15 more functions across supervisor/transport/wire/encryption.

### Decisions

- **PID unification over PID removal**: Kept both fields but made them identical,
  minimizing struct layout changes while fixing the routing bug.
- **Thread-local errors over global**: `set_last_error` uses `thread_local!` so
  concurrent C-ABI calls don't clobber each other's diagnostics.
- **Delete over deprecate**: `node.rs` had conflicting symbols with `hew_node.rs` —
  deprecation would have caused linker errors, deletion was the only safe option.
- **CURRENT_NODE global**: A single `AtomicPtr<HewNode>` set on `hew_node_start` and
  cleared on `hew_node_stop`. This allows `hew_actor_send_by_id` to route remote PIDs
  without requiring callers to pass a node handle. One active node per process — matches
  Erlang/OTP's single-node model.

---

## Feature Completeness Sprint — February 2026

A February feature wave closed the parser/type/codegen gaps identified by the
audit: char literals, negative patterns, self parameter sugar, loop labels,
struct enum variants, array repeat, visibility modifiers, trait bound
enforcement, multi-trait `dyn`, first-class ranges, unsafe enforcement,
timeout codegen, HashSet, generic lambdas, if-let, custom indexing,
associated types, `s.spawn`, ARM coroutines, and WASM platform warnings.

## Quality & Correctness Sprint — February 2026

A long-running audit-and-fix campaign hardened the compiler across parser, type
system, codegen, runtime, and tests. The useful history here is the bug classes
it kept exposing, not the round-by-round cadence.

- Control flow and cleanup were normalized so returns evaluate before teardown,
  intermediate loops deactivate correctly, and failure paths clean partially
  initialized state instead of only cleaning the happy path.
- Pattern matching now fails closed: statement and expression `match` use the
  same checks, guarded wildcards no longer count as exhaustive, fallthrough
  traps instead of fabricating defaults, and constructor/or-pattern handling
  distinguishes bindings from variants.
- Traversal and capture logic now reaches the expression and statement variants
  that had been silently skipped, which closed several hidden semantics gaps in
  capture analysis, builtin rewriting, normalization, and enrichment.
- A systematic operation x type audit closed recurring `f32`,
  narrow-integer, `bool`, and pointer gaps across lowering and turned
  warning-and-skip paths into explicit errors.
- Runtime and boundary hardening added overflow/null checks, ownership fixes,
  explicit ask/select/join submit-status handling, `HashMap.get() -> Option<T>`,
  and preserved inferred type information across the compiler/runtime seam.

The campaign ended with the full codegen suite green and substantially better
fail-closed behavior across the compiler/runtime seam.

## 2026-03-06 — Audit remediation follow-ups

A short remediation wave closed the remaining audit gaps:

- validation now walks nested example trees and regenerates downstream syntax
  outputs before diffing them;
- wire syntax preserves `since N` across both parser paths and normalized
  formatter output;
- serializer enrichment now converts supported `Ty` forms explicitly, reports
  unsupported nested forms contextually, and deduplicates inferred-type
  diagnostics at the CLI boundary;
- select/join/ask cleanup now has explicit cancellation and submit-status
  handling across MLIR lowering and reply-channel runtimes, preventing hangs
  and late-reply use-after-free paths.


## Structured Error Types — Error Handling Story

### Design rationale

Hew already had `Result<T, E>`, payload-carrying enums, and the `?` operator.
What it was missing was a convention: stdlib modules were returning bare `i32`
codes or `Result<T, String>`, which made it hard for callers to branch on
stable error kinds.

### What shipped

- Chose plain Hew enums over a new language feature so error kinds stay
  pattern-matchable without changing the type system.
- Shipped `IoError` in `std/fs.hew` as the canonical file-system error enum and
  added end-to-end coverage for `?` propagation, variant matching, and
  qualified enum construction.

### Known limitation — `Result<T, UserEnum>` in imported modules

The codegen still cannot lower `Result<T, UserEnum>` when the user-defined
error type appears in an imported-module signature, so the standalone pattern
is proven before full stdlib export support is available.

## Implicit generic monomorphization

- Generic call sites were already being inferred during type checking, but the
  resolved type arguments were never written back into the AST.
- Added `call_type_args` to `TypeCheckOutput` and backfilled missing
  `call.type_args` during enrichment so the C++ codegen sees one explicit
  monomorphization path instead of re-inferring types later.
- Added end-to-end coverage for single-parameter and multi-parameter implicit
  specialization.

## Single-binary codegen cutover

- Embedded the C++ MLIR/LLVM backend directly into the `hew` Rust binary
  instead of spawning a separate `hew-codegen` process.
- Let CMake remain the source of truth for link directives by generating raw
  `.cargo` lines that `build.rs` prints verbatim.
- Kept static and shared build modes behind the same Rust-side integration
  while treating host/toolchain mismatches as configuration errors rather than
  silent fallbacks.
- Removed the standalone `hew-codegen` binary from release and installer flows
  once the embedded path was complete.

## Stream/Sink RAII Auto-Close and Drop Exclusion Fixes

- **What:** Three fixes for the stream/bytes API: (1) let-bound match return double-frees
  in bytes map, (2) Stream/Sink RAII auto-close on scope exit, (3) type guard for
  `lines()` on `Stream<bytes>`.

- **Why:** Without RAII auto-close, every Stream/Sink handle requires an explicit
  `.close()` call. Forgetting one causes consumer deadlocks (waiting for EOF that
  never arrives). The let-bound match fix prevents double-frees when match arms
  flow through let bindings to function returns.

- **RAII approach:** Each Stream/Sink variable gets a `closeAlloca` (`memref<llvm.ptr>`)
  storing the handle pointer. On scope exit, codegen loads from the alloca, null-checks
  via `scf::IfOp`, and calls close. Explicit `.close()`, consuming runtime functions,
  and user function calls null the alloca so scope-exit close is a no-op.

- **Key decisions:**
  - Ownership transfer heuristic: user functions receiving stream/sink args are assumed
    to take ownership (alloca nulled). Functions that only observe would leak, but
    double-frees are worse. Proper fix requires move semantics in the type system.
  - Block expression tail values need alloca null-out before scope pop, or the
    scope-exit close fires on a handle being returned to the outer scope.
  - Non-consuming function allowlist uses actual runtime names (`hew_sink_write_string`,
    not `hew_sink_write`) discovered by checking enricher rewriting.
  - The `funcLevelReturnVarNames` flat name set handles the depth mismatch caused by
    the enricher's `unsafe {}` wrapper (variables at depth 1, return expressions at
    depth 2+).

## RAII Phase 1: Parameter Drops and the Null-After-Move Prerequisite

- **What:** Investigated adding automatic drop calls for function parameters at scope
  exit, as the first step toward proper Rust-style RAII across the codegen.

- **Approach 1 (abandoned): dropValue in DropEntry.** Stored the raw `mlir::Value`
  from the parameter declaration site. Failed because `declareVariable` promotes
  `let` bindings to `alloca+store` when `returnFlag` is active — the raw SSA value
  lives inside an `scf.if(!returnFlag)` guard and doesn't dominate the drop site.
  `lookupVariable` is the correct mechanism because it creates a fresh `memref::LoadOp`
  from the alloca, which always dominates.

- **Approach 2 (double-free): pendingFunctionParamDrops.** Registered params for drop
  via the existing `registerDroppable` mechanism. All 522 tests passed on compile, but
  13+ tests hit double-frees at runtime. Root cause: the codegen has no ownership
  transfer tracking. When a param is consumed by match destructuring (`match t { ... }`
  frees the indirect enum), callee move (`sink.write(data)` frees `data` in the runtime),
  or return, the param drop at function exit frees already-freed memory.

- **Decision:** Param drops require **null-after-move** as a prerequisite. Without
  nulling out a variable's storage after consumption, any new drop registration is
  unsafe. This applies to all owned values, not just params.

- **Key discovery: declareVariable alloca promotion.** When `returnFlag` is active
  (non-void functions), `declareVariable` (MLIRGen.cpp:878-916) promotes `let` bindings
  to `memref` alloca+store in the function entry block. This means `lookupVariable`
  always returns a value that dominates any point in the function. This is the correct
  mechanism for drop emission and is the foundation for null-after-move (store null
  into the alloca after consumption).

- **Path forward:** Implement null-check-before-drop in `emitDropEntry`, then add
  null-after-move at consumption sites (match destructuring, callee args, return).
  Only then re-enable param drops.

## Phase 11: Tail-call marking through match arms (2026-03-25)

### Goal

Catch the parser's missed tail-call optimization when a tail-position expression or
nested return flows through a `match` arm.

### Decisions

- Read `hew-parser/src/ast.rs` first and confirmed `MatchArm.body` is always an
  expression, so statement-form `match` arms need expression recursion rather than
  a direct statement walk.
- Reworked the tail-call pass around an explicit tail-position flag so block
  trailing expressions are only marked when the enclosing block expression itself
  is tail-position, avoiding false positives in ordinary statement bodies.
- Extended the pass and its defer guard to recurse through `Stmt::Match`,
  `Expr::Match`, and other expression containers so nested `return foo()` cases in
  block-valued arms are handled consistently.
