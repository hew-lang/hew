# Distributed Actor Infrastructure — Journey Log

## Phase 0: Multi-Agent Audit (2026-02-24)

### Agents deployed

- **GPT-5.3-Codex** → Runtime quality audit (stability, duplication, unsafe patterns)
- **Gemini 3 Pro** → Codegen duplication and simplification audit
- **GPT-5.2** → Distributed actor infrastructure gap analysis
- **Sonnet 4.6** → API contracts and module boundary audit

### Key findings across all agents

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

Based on agent consensus, the work divides into two tracks:

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

### Design iteration 3: Implementation plan

The implementation is split into 7 phases, with each phase independently
testable and committable.

---

## Phase 1: Critical Safety Fixes

**Commit:** b5d7481

- **Dangling pointer UB fixed** in connection.rs: Replaced raw `AtomicU64` pointer passed
  to spawned reader thread with `Arc<AtomicU64>`. The original code captured a pointer to a
  stack-local field, then moved the owning struct, creating a dangling pointer. GPT-5.3-Codex
  agent identified and fixed this with minimal restructuring.
- **Hardcoded byte offsets replaced** in bridge.rs: `MAILBOX_OFFSET=48` and
  `ACTOR_STATE_OFFSET=56` replaced with `std::mem::offset_of!(HewActor, ...)` plus
  compile-time assertions. Gemini 3 Pro agent handled this cleanly.
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
- Caught and fixed a regression where the extraction agent re-introduced a `CmpIOp` for
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

### Second-round multi-agent audit

Deployed 4 audit agents (GPT-5.3-Codex, GPT-5.2, Sonnet 4.6, Gemini 3 Pro) against
the Phase 7 codebase. Found critical issues the first round missed:

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

Multi-model analysis (Claude Opus 4.6, GPT-5.1 Codex, Claude Sonnet 4.5, Gemini 3 Pro,
GPT-5.2 Codex) identified 25 gaps across parser, type system, codegen, and runtime.
Implementation proceeded in parallel batches using frontier models.

### Batch 1: Parser Foundations (6 features)

- **Char literals**: Added `CharLit` lexer token, parser production, and type checking.
  End-to-end from `'a'` through codegen.
- **Negative patterns**: `-1` and `-3.14` now work in match arms.
- **Self parameter sugar**: `fn method(self)` infers Self type from impl block.
- **For loop labels**: `@outer: for i in 0..N { break @outer; }` now works.
- **Struct enum variants**: `Variant { field: Type }` with named fields.
- **Array repeat**: `[0; 256]` syntax for initializing arrays.

### Batch 2: Type System & Codegen (6 features)

- **Visibility modifiers**: `pub(package)` and `pub(super)` replace boolean `is_pub`.
  Full refactor across parser, type checker, serialization, and C++ codegen.
- **Trait bound enforcement**: Generic function calls now verify concrete types
  implement required traits.
- **Multi-trait dyn**: `dyn (Printable + Measurable)` with method resolution across
  all specified traits.
- **Range values**: Ranges are first-class tuples; `for i in start..end` works with
  variable bounds.
- **Unsafe enforcement**: Extern FFI calls require `unsafe { }` wrapper. Type checker
  tracks `in_unsafe` state.
- **Test migration**: 8 e2e tests updated to use `unsafe { }` for extern calls.

### Current State

- 311/312 codegen e2e tests passing (scope_spawn MLIR verification needs work)
- Full workspace Rust tests passing (530+ tests)
- Clean builds across all crates and C++ codegen
- 26 feature gaps identified, 24 fully implemented, 2 partial (scope_spawn codegen, WASM ops)

### Features Implemented

All 20 planned features from the multi-model analysis are now in place:
char literals, negative patterns, self parameter sugar, for loop labels,
struct enum variants, array repeat, visibility modifiers, trait bound enforcement,
multi-trait dyn, range first-class, unsafe enforcement, timeout codegen, HashSet,
generic lambdas, if-let patterns, custom indexing, associated types, s.spawn syntax,
ARM coroutines, and WASM platform warnings.

## Quality & Correctness Sprint — February 2026

Five frontier models analyzed the codebase in parallel, identifying 25+ issues
across parser, type system, codegen, runtime, and tests. Priority: correctness
bugs over style concerns.

### Correctness Fixes

- **Cross-enum variant matching**: Match arms from wrong enums silently type-checked.
  Fixed `lookup_variant_types` to only search the scrutinee's enum.
- **Bare return in non-unit fns**: `return;` in `fn -> int` compiled silently.
  Added `ReturnTypeMismatch` error kind.
- **Trait object ordering**: `dyn (A + B)` != `dyn (B + A)` in unification.
  Changed to set-based comparison.

### Safety Fixes

- **Compound assign UB**: Three codegen switch statements had no default case,
  leaving `mlir::Value` uninitialized. Added error-emitting defaults.
- **HashMap/Vec string getters**: Returned internal pointers that become dangling
  after mutation. Now return `strdup` copies.

### Parser Quality

- Missing param type annotation: silent drop → error message
- `pub(invalid)`: silent promotion to pub → defaults to private
- String interpolation errors: silently lost → propagated to parent

### Test Coverage

- Registered 4 unregistered e2e test directories (ranges, chars, for labels, tail call)
- Added negative type checker tests (mutability, arity)
- Added trait object ordering unification test

### Current State

- 314/316 codegen e2e tests passing
- 870+ Rust workspace tests passing
- Clean builds across all crates
