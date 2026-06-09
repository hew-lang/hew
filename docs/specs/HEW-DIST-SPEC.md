# HEW-DIST-SPEC v0

**Status:** v0 distributed `Node::*` specification. This is a pre-v1 contract: it defines what Hew may claim publicly about distributed actor messaging before behavior-changing runtime work proceeds.
**Audience:** language designers, runtime implementers, codegen/ABI reviewers, docs reviewers, and example authors.
**Stance:** Hew may allow local-looking syntax for remote actor operations, but it must not claim local-only semantics. Every remote boundary exposes typed failure.

---

## 1. Status & scope

This document is the normative distributed companion to [`HEW-SPEC-2026.md`](./HEW-SPEC-2026.md) section 11.

In scope for v0:

- distributed node identity and pid-reference shape
- delivery, ordering, lookup, ask, timeout, cancellation, monitoring, failure detection, backpressure, wire negotiation, authorization, observability, testing, and WASM policy
- the public honesty bar for docs and examples

Out of scope for v0:

- durable workflows, distributed transactions, or placement durability
- stronger delivery guarantees than at-most-once
- automatic ratification of remote ownership rules still deferred to [`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)

This spec gates future `Node::*` implementation work. It does not claim that every requirement here is already shipped.

## 2. Glossary

| Term | Meaning |
| --- | --- |
| Node | A Hew runtime instance participating in distributed actor messaging. |
| NodeId | The stable distributed node identity; key-backed and UUID-class, not a process-global slot. |
| LocalPid / RemotePid | A reference to an actor endpoint; distributed identity includes node and incarnation data. |
| PID | Runtime routing identity used to reach an actor instance. |
| Incarnation | A generation value that changes when a node, actor, or registration is replaced. |
| Name | A human-readable registration key. |
| Namespace | The authorization and lookup scope that bounds names. |
| Ask | A request-response remote operation expecting a typed reply or typed failure. |
| Send | A fire-and-forget remote operation. |
| Link | A supervision relationship that couples failure propagation. |
| Monitor | A one-way failure observation relationship. |
| Supervisor | The policy owner for restart and failure handling. |
| Peer | Another node in the distributed topology. |
| Partition | A connectivity failure where peers cannot currently exchange the traffic required for the operation. |
| Suspect | Failure-detector state meaning a peer may be unavailable but is not yet declared dead. |
| Dead | Failure-detector state meaning the peer is considered unreachable until a fresh session is established. |
| Schema id | The identifier for a payload contract carried on the wire where required. |
| ALPN | The negotiated application protocol name used to distinguish incompatible transport families or major versions. |

## 3. Identity & incarnation

`NodeId` is the distributed identity root. It is UUID-class and key-backed. An implementation may cache a smaller routing slot internally, but the public identity contract is not a process-global singleton and is not satisfied by a bare integer slot.

`RemotePid` identity is defined as `(NodeId, slot, incarnation)`:

- `NodeId` identifies the node authority.
- `slot` identifies the actor location within that node's routing domain.
- `incarnation` distinguishes stale references from live replacements.

Name registration is scoped by namespace. Re-registering a name creates a new incarnation for that registration. A lookup that resolves through stale node, actor, or registration identity must fail closed with `StaleRef`; it must not silently bind to a replacement actor with a different incarnation.

The implementation may expose local convenience APIs, but distributed identity must remain inspectable enough for latency-aware, failure-aware, and authorization-aware code.

### 3.1 Pid reference ownership

`LocalPid<A>` and `RemotePid<A>` are **refcounted identity references**
(`Rc`/`Arc`-shaped), consistent with [`HEW-SPEC-2026.md`](./HEW-SPEC-2026.md) §3.7.8.
They are `Frozen` and `Send`: the same identity may be addressed concurrently
from many holders, locally and across node boundaries. They are **not** affine
per-call handles in the sense of
[`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
§7 tier-1; they are the *identity* side of the identity-vs-authority split that
governs distributed references.

- Identity may be cloned freely (refcount bump) and shared. Cloning a
  pid reference does not duplicate authority, mailbox capacity, supervision
  power, or any other resource — it only duplicates the means to *name*
  the actor.
- Cycles between actors that hold strong identity references to one
  another are broken with weak pid references, per `HEW-SPEC-2026.md` §3.7.8.
  Supervision trees naturally avoid cycles: parents hold strong identity
  references to children; children, when they need to address their
  parent, hold a weak pid reference or use an explicit message protocol.
- No user-visible `close()`, `free()`, or `release()` is ever required on
  a pid reference in normal Hew code. This satisfies the prime invariant of
  `handle-safety-and-resource-lifetime.md` §1 ("no user-visible manual
  free in normal Hew code") for distributed identity.
- Identity does **not** confer authority. Holding a pid reference lets the
  holder *address* the actor; whether a given message, observation, or
  capability invocation is permitted is governed by §12 (security &
  capabilities). A capability transferred over a pid reference is a
  separate value with its own ownership shape (§12); revoking that
  capability does not invalidate the underlying identity.
- Stale identity — a reference whose `NodeId` is gone, whose `slot` is
  vacated, or whose `incarnation` has been replaced — must fail closed
  with `StaleRef` (§3, §6). This is consistent with the fail-closed
  posture of `handle-safety-and-resource-lifetime.md` §3.1 and with the
  general distributed prohibition on sentinel substitutes in §4.
- The checker is the authority for ownership classification of
  pid-reference values, per `handle-safety-and-resource-lifetime.md` §5
  (single ownership oracle). Codegen and the runtime consume that
  classification fail-closed and do not re-derive it.

## 4. Delivery semantics

Distributed fire-and-forget send is **at-most-once**. If an application needs retry behavior, it must opt into an idempotence-aware wrapper at a higher layer; retry is not implied by the base `send` semantics.

Remote `ask` operations return typed success or typed failure. Remote `lookup` operations return typed success or typed failure. Sentinel values, zero values, or panic-shaped substitutes are forbidden at distributed boundaries.

The language may permit the same call syntax for local and remote actor interactions. The semantics are still different:

- remote operations can time out, be cancelled, be unauthorized, or fail version negotiation
- delivery success is bounded by transport, peer health, and mailbox/backpressure state
- the caller must be able to observe failure explicitly

Per `(sender, receiver)` FIFO ordering holds only while both parties communicate over one live connection/session. This spec does not promise duplicate suppression beyond the at-most-once base guarantee.

## 5. Ordering & connection lifetime

Ordering and liveness guarantees end at the connection lifetime boundary.

- Reconnect does not preserve in-flight ordering.
- Incarnation bumps do not preserve message continuity.
- A fresh connection/session is a fresh ordering domain.

When a connection drops mid-flight:

- outstanding `ask` operations resolve to a typed failure such as `Partition`, `Timeout`, `Cancelled`, `LocalShutdown`, or `OrphanedAsk`, depending on the failure surface observed by the caller
- the implementation must not synthesize a successful reply
- callers may observe order gaps after reconnect and must not treat reconnect as transparent continuation

Returning peers re-enter the system through a fresh negotiated session. If the peer or target actor now presents a newer incarnation, older references remain invalid and fail with `StaleRef`.

## 6. Lookup & ask error taxonomy

Every remote operation must expose typed failure. Downstream consumers must handle the variants explicitly; wildcard/default fallthrough at the public boundary is not the intended contract.

| Variant | Applies to | Trigger |
| --- | --- | --- |
| `NotFound` | lookup | The namespace is reachable, but no live registration matches the requested name/incarnation. |
| `Partition` | lookup, send, ask, monitor | The node cannot currently establish or maintain the route required for the operation. |
| `Timeout` | lookup, ask | The caller-supplied deadline expires before a terminal result arrives. |
| `VersionMismatch` | lookup, send, ask | Peer or payload negotiation rejects the operation because versions, features, schema id, or ALPN are incompatible. |
| `Unauthorized` | lookup, send, ask | Authorization policy denies the namespace, name, capability, or message type. |
| `DecodeFailure` | ask, send, monitor | The payload, envelope, or reply cannot be decoded according to the negotiated contract. |
| `OrphanedAsk` | ask | A reply or terminal event arrives after the ask correlation state has been removed or superseded. |
| `StaleRef` | lookup, send, ask, monitor | The referenced node/slot/incarnation no longer names a live target. |
| `Cancelled` | lookup, ask, send | The caller or owning scope cancelled the operation before it completed. |
| `LocalShutdown` | lookup, send, ask, monitor | The local node is stopping or stopped before the operation can complete. |
| `Backpressure` / `WouldBlock` | send, ask | The operation cannot currently enter the required queue or credit window without violating the published bound. |
| `MonitorLost` | monitor | The monitor relationship itself cannot be maintained across restart, partition, or teardown. |

The exact type split between `LookupError`, `AskError`, `SendError`, and `MonitorError` may vary, but the public surface must preserve these named failure conditions or tighter typed equivalents.

## 7. Timeout & cancellation

Every remote operation has a caller-supplied timeout or deadline. v0 forbids a hidden global distributed timeout as the public contract.

Cancellation is cooperative:

- the caller may cancel before completion
- cancellation is observed at the next safe point in the operation lifecycle
- cancellation resolves to `Cancelled`, not to a sentinel success shape

Shutdown and cancellation obligations include all exit paths that own distributed work: ask tables, accept loops, peer sessions, gossip windows, and inbound worker state. Partial cleanup that only drains a user mailbox is insufficient.

## 8. Supervision: link & monitor across nodes

`monitor` crosses node boundaries by default. It is the default mechanism for observing remote failure without coupling restart policy.

`link` does **not** cross node boundaries by default. Cross-node link, if offered, is opt-in and must document partition behavior explicitly so that a transient network split does not masquerade as a local crash cascade.

Remote restart policy must specify:

- who owns restart authority
- where the replacement actor is placed
- whether the replacement increments actor or registration incarnation
- which typed failure is observed by outstanding asks and monitors during the transition

Monitor delivery is itself best-effort across distributed failures. If the monitor relationship cannot be maintained, the observer receives `MonitorLost`; the implementation must not fabricate a successful completion event for the monitored operation.

### 8.1 Supervision ownership

A parent supervisor's *reference* to a remote child is a refcounted `RemotePid`
(the §3.1 identity shape). The reference may be cloned and used for messaging
without affecting restart authority.

A parent supervisor's *restart authority* over a remote child is a **supervision
token** in the sense of
[`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
§3.1 mechanism D (adopted for shutdown / cancellation / session lifetime). The
supervision token:

- **Must** be held by exactly one supervisor at a time. Transfer is explicit;
  the token is not duplicated on clone.
- **Must not** be implicitly copied across a cross-node `link` boundary. Cross-node
  `link` remains opt-in (per §8 above); if offered, an opt-in cross-node link
  *transfers* (does not duplicate) restart authority to the accepting end.
- **Must** be invalidated on any of the following exit paths: supervisor stop,
  explicit `unlink`, child incarnation bump, peer reaching `Dead`, or session
  reset. This enumeration is exhaustive for v0; implementations must not treat
  partial teardown as sufficient (LESSONS `cleanup-all-exits`).
- After invalidation, any attempt to exercise restart authority **must** resolve
  to a typed failure. The typed failure is the existing `MonitorLost` variant
  (§6); no new `SupervisionLost` variant is introduced in v0. This keeps the §6
  taxonomy stable and the soundness matrix untouched.

The checker is the authority for ownership classification of supervision tokens,
per [`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
§5 (single ownership oracle).

**Implementation gap:** Single-holder restart-authority enforcement at the type
level is not yet implemented. This clause is normative spec intent; runtime
enforcement is tracked under issue #1228 (RuntimeContext handle-shaped API) and
issue #1399 (move-checker substrate). No existing supervision scaffolding claims
to enforce single-holder tokens; the gap is absence of enforcement, not wrong
enforcement.

**WASM policy:** Supervision token obligations are native-only; see §15 for WASM
policy.

## 9. Failure detector model

Hew distributed nodes use an adaptive detector model in the φ-accrual / SWIM-Lifeguard family. The public observation contract is two-state:

- `Suspect`
- `Dead`

Applications may observe the following:

- while a peer is `Suspect`, new remote work may still succeed, but callers must be prepared for `Partition` or `Timeout`
- once a peer is `Dead`, new remote work to that peer fails with typed failure; the detector must not convert uncertainty into synthetic success
- when a peer returns, it does so by establishing a fresh session; if identity or incarnation changed, old references remain stale

The detector is advisory for liveness, never authoritative for success. A detector transition may accelerate failure, but it may not manufacture a reply that no remote actor produced.

## 10. Backpressure

Distributed messaging requires end-to-end backpressure beyond transport-level flow control.

The v0 contract requires:

- explicit credits or an equivalent bounded admission mechanism for remote send
- a bounded system mailbox, or quota-controlled priority lanes if system traffic is separated from user traffic
- typed backpressure on the send surface via `Backpressure` or `WouldBlock`
- no silent drop as the default overflow behavior for distributed hot paths

Backpressure must be observable in operator surfaces such as per-peer stats, mailbox watermarks, queue depth, or credit state.

## 11. Wire & version negotiation

Connection setup must negotiate enough information to reject incompatible peers before application traffic is misinterpreted.

The handshake contract includes:

- `NodeId`
- Hew version
- HBF major/minor version
- feature flags
- capability proof appropriate to the transport/security mode

Payloads that require typed compatibility must carry schema identity/version. Incompatible ALPN major versions are rejected at negotiation time. Incompatible payload or feature negotiation resolves to `VersionMismatch`; malformed envelopes or payloads resolve to `DecodeFailure`.

This spec records a validation obligation: CI and release qualification must exercise at least `vN`/`vN+1` wire compatibility for supported distributed versions.

## 12. Security & capabilities

Non-loopback distributed mode requires authenticated peers by default. Key-backed identity and mutually authenticated transport are the baseline contract for that mode.

Authorization is explicit and scoped:

- by namespace
- by registered name
- by message type or capability

Named lookup is not ambient authority. A string name is valid only within an opened or granted namespace. Public docs and examples must not present the global registry as an unrestricted authority surface.

Sealed actor references and explicit capability transfer are the design target for untrusted or partially trusted boundaries. Their ownership mechanics are ratified in §12.1.

### 12.1 Sealed-reference and capability ownership

A **sealed actor reference** is a refcounted identity object — it has the same
§3.1 shape as a pid reference. Sealing limits *who* may mint exercise of authority
through the reference; it does not make the identity affine. Sealed references
may be cloned and shared, and no user-visible `close()` or `free()` is required.

A **capability** is an authority token in the sense of
[`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
§3.1 mechanism D. Key ownership properties:

- Capability transfer between principals (local or remote) is **move-by-default**.
  Duplicating authority requires an explicit re-grant by the original grantor.
  Cloning a sealed reference does not duplicate the capability.
- Capabilities are **revocable** by the grantor. After revocation, any attempt
  to exercise the capability **must** resolve to the existing `Unauthorized`
  typed failure variant (§6). No new `Revoked` variant is introduced in v0;
  this keeps the §6 taxonomy and soundness matrix stable.
- Wire-encoded capability frames are subject to the existing fail-closed
  serialization posture (LESSONS `serializer-fail-closed`). A malformed or
  expired capability frame **must** resolve to `DecodeFailure` or `Unauthorized`,
  never to a silent local-shape success. Partial or best-effort wire encoding
  of capability proofs is forbidden.

The checker is the authority for ownership classification of capability tokens,
per [`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
§5 (single ownership oracle). Cross-linking §11: capability proof appropriate
to the transport/security mode is carried in the handshake contract; §12.1 does
not restate those mechanics, only the ownership shape of tokens once transferred.

**Implementation gap:** Capability transfer and grantor-side revocation are not
yet runtime-enforced. This clause is normative spec intent; runtime enforcement
is tracked under issue #1706 (capability transfer ownership and revocation per
HEW-DIST-SPEC §12).

**WASM policy:** Capability token obligations are native-only; see §15 for WASM
policy.

Authorization failure resolves to `Unauthorized`; authn/authz teardown during an active operation must not collapse into a local-shaped success.

## 13. Observability

Distributed envelopes must carry trace context sufficient to correlate cross-node hops. A `traceparent`-equivalent field is the minimum contract.

`Node::stats()` or its successor surface must expose, at minimum:

- per-peer health
- drops and decode failures
- mailbox watermarks
- ask depth
- flow-control or credit state

Implementations must also provide a recent-event flight recorder. Size policy and retention strategy are implementation-defined, but the obligation to retain recent distributed events for debugging is normative.

## 14. Deterministic distributed testing

`SimTransport` is a required test artifact for distributed Hew. The spec requires it to support:

- drop
- duplicate
- reorder
- partition
- delay
- clock-skew

Distributed property tests must name and check stable invariants, including:

1. **Typed-failure invariant:** remote lookup/ask/send never collapse partition or timeout into sentinel success.
2. **Stale-ref invariant:** an incarnation bump makes old references fail closed.
3. **Live-session FIFO invariant:** per-sender/per-receiver FIFO holds only inside one live negotiated session.
4. **Reconnect-gap invariant:** reconnect may expose ordering gaps, but must not claim continuity it did not preserve.
5. **Cancellation invariant:** cancellation resolves to `Cancelled` or another typed terminal failure, never a fabricated reply.
6. **Backpressure invariant:** bounded queues surface `Backpressure`/`WouldBlock` rather than silently dropping hot-path traffic.
7. **Detector-honesty invariant:** suspect/dead transitions may surface failure sooner, but never synthesize success.

Tests should assert final stable invariants rather than one exact transient sequence.

## 15. WASM policy

`Node::*` is native-only in v0. The normative policy is a compile-time gate:

- programs targeting WASM must be rejected when they use `Node::*`
- the diagnostic must name that distributed `Node::*` requires the native runtime
- docs and examples must not imply browser/WASM distributed support unless and until a later spec ratifies it

This policy is the distributed counterpart to Hew's explicit parity rules: native-only behavior must be named, not implied.

## 16. Examples policy

Every distributed example is part of the contract surface.

Distributed examples must:

- handle the error branch on remote lookup, send, ask, monitor, or shutdown boundaries
- show caller-supplied timeout/deadline where a remote reply is awaited
- avoid teaching sentinel comparisons such as `!= 0` for remote lookup success
- avoid implying that remote operations are local-equivalent

Examples that omit typed-failure handling on remote boundaries are bugs against this spec.

## 17. Anti-patterns

The following are forbidden in distributed Hew docs, examples, stdlib surface design, and future `Node::*` claims:

1. claiming that local-looking syntax implies local-equivalent semantics
2. claiming delivery guarantees stronger than at-most-once
3. hiding remote blocking behavior or relying on a fixed global remote timeout
4. assuming GC-style lifetime rescue for remote handles or remote state
5. making a partition look like ordinary local absence
6. leaving distributed hot paths on unbounded mailboxes
7. treating a global string registry as ambient authority
8. treating single-process global runtime state as the permanent cluster identity model
9. layering QUIC framing in a way that recreates head-of-line blocking
10. changing wire compatibility without explicit negotiation and version policy

## 18. Coordination & dependency graph

This v0 spec is stage 0 of the distributed roadmap and gates later implementation work.

| Stage | Meaning | Dependency from this spec |
| --- | --- | --- |
| 0 | Spec and honesty pass | This document and `HEW-SPEC-2026.md` section 11 define the public contract. |
| 1 | Identity and ownership foundations | Gated by sections 3, 6, and 7; ownership ratification depends on [`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md). |
| 2 | Wire and version negotiation | Gated by section 11. |
| 3 | Failure detector and supervision | Gated by sections 8 and 9. |
| 4 | Backpressure and quiescence | Gated by sections 7 and 10. |
| 5 | Deterministic distributed testing | Gated by section 14. |
| 6 | Security and capabilities | Gated by section 12. |
| 7 | Placement and virtual actors | Informed by this spec, but placement semantics remain a later design lane. |
| 8 | Production hardening | Depends on stages 1-7 being implemented and validated. |

The concrete follow-on work named by the distributed research record maps to this v0 contract as follows:

1. generation-checked `RemotePid` + node incarnation — gated by sections 3 and 5
2. typed result on `Node::lookup` and remote await — gated by sections 4 and 6
3. caller-supplied timeout and cancellation for remote await — gated by section 7
4. per-call QUIC streams and handshake/version negotiation — gated by section 11
5. bound system mailbox and priority quotas — gated by section 10
6. adaptive failure detector — gated by section 9
7. `SimTransport` and distributed property tests — gated by section 14
8. authenticated nodes by default plus per-name authorization — gated by section 12
9. distributed observability (`traceparent`/stats/flight recorder) — gated by section 13
10. end-to-end backpressure credits for fire-and-forget — gated by section 10
11. `RuntimeContext` replacing permanent process-global node ownership — gated by sections 3 and 18; §3 pid-reference ownership is ratified and matches already-shipped runtime behaviour; §8 supervision-token and §12 capability-transfer ownership are ratified as normative spec intent, pending runtime enforcement under issues #1228 and #1399
12. defining link versus monitor across node boundaries — gated by section 8
13. `Node::*` WASM policy — defined by section 15
14. virtual-actor placement (`Cluster::activate`) — explicitly later than v0, stage 7
15. production hardening and soak/version-skew readiness — stage 8, after the earlier stages land

Ratification of the deferred ownership clauses in sections 3, 8, and 12 is
complete. All deferred-ownership markers have been resolved:

- **§3 (pid-reference ownership):** Ratified. `LocalPid` and `RemotePid` are
  refcounted identity references (`Frozen`, `Send`) consistent with
  `HEW-SPEC-2026.md` §3.7.8. This
  matches already-shipped runtime behaviour; no implementation obligation
  survives.
- **§8 (supervision ownership):** Ratified as normative spec intent. Runtime
  enforcement of single-holder restart-authority tokens is tracked under issue
  #1228 (RuntimeContext handle-shaped API) and issue #1399 (move-checker
  substrate).
- **§12 (capability transfer):** Ratified as normative spec intent. Runtime
  enforcement of capability transfer ownership and revocation is tracked under
  issue #1706.
