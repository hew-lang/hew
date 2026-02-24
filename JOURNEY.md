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

See plan.md for the detailed task breakdown. The implementation is split into
7 phases, with each phase independently testable and committable.

---

## Phase 1: Runtime Hardening

_(To be filled as work progresses)_
