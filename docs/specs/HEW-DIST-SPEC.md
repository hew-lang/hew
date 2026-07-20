# Hew Distributed Runtime Specification

**Status:** normative for v0.6.0-rc1
**Protocol epoch:** 2
**Targets:** native only

This document defines node identity, peer admission, actor locations, routing,
registry discovery, messaging, monitors, links, and failure handling for Hew's
distributed runtime.

## 1. Conformance language

The terms **MUST**, **MUST NOT**, **SHOULD**, and **MAY** are normative.

Implementations MUST reject malformed, unauthenticated, stale, or
version-incompatible input before publishing routes or mutating distributed
lifecycle state. There is no protocol-epoch fallback.

## 2. Identity and routing terms

### 2.1 `NodeId`

A `NodeId` is an immutable 128-bit identity derived from the node's stable
authenticated public credential. It is not configured independently and is not
derived from an operating-system process identifier, address, or route slot.

The canonical derivation is:

```text
digest = SHA-256(
    "hew-node-id-v1\0"
    || credential_kind:u8
    || credential_length:u32-big-endian
    || canonical_credential_bytes
)
NodeId = digest[0..16]
```

Credential kinds are:

| Value | Credential |
|---:|---|
| `1` | Noise-XX X25519 static public key, exactly 32 bytes |
| `2` | Canonical DER TLS leaf SubjectPublicKeyInfo |

The text representation is exactly 32 lowercase hexadecimal digits. Parsers MAY
accept uppercase hexadecimal input but MUST render lowercase.

Rotating the stable key rotates the `NodeId`. Reusing the same stable key
preserves the `NodeId`.

### 2.2 Route slots

A route slot is a non-zero `u16` alias local to one receiving node. Slot `0` is
reserved for local dispatch and MUST NOT be assigned to a peer.

Route slots are not distributed identity and do not appear in `Location`.
Different nodes MAY assign different route slots to the same peer. APIs that
accept a route-slot argument, including `Node::allow_peer`, interpret it only in
the caller's local routing namespace.

The peer-binding table is one-to-one:

- one route slot maps to exactly one credential;
- one credential maps to exactly one route slot;
- repeated identical pins are idempotent;
- conflicting pins fail closed.

### 2.3 Session incarnation

Each stable node identity owns a durable, non-zero `u32` session incarnation.
Starting a node burns and persists the next incarnation before the node becomes
routable. The journal is locked for exclusive use, recovers from a torn latest
record, rejects a key-derived identity mismatch, and refuses overflow.

A same-key restart therefore keeps the `NodeId` and advances the session
incarnation. A carried location from an earlier session is stale.

### 2.4 `Location`

A remote actor is identified by:

```text
Location {
    node: NodeId,          // 16 bytes
    slot: u64,             // non-zero actor slot
    incarnation: u32,      // non-zero durable node session
    reserved: u32 = 0
}
```

The native C ABI representation is 32 bytes. The reserved field MUST be zero.
Actor slot `0` and session incarnation `0` are invalid.

`RemotePid<T>` is a phantom-typed inline `Location`. It owns no heap allocation,
has no reference count, and does not keep either the remote node or actor alive.
Moving, storing, or duplicating the location metadata does not grant authority
and does not affect actor lifetime.

There is no public provenance-free constructor. A program obtains a
`RemotePid<T>` only from authenticated runtime discovery or another typed value
that already carries a validated location.

## 3. Stable credentials and peer configuration

`Node::load_keys(path)` loads or creates the stable credential for the selected
native transport. `Node::identity_key()` returns the corresponding public
credential as lowercase hexadecimal for out-of-band exchange.

`Node::allow_peer(route_slot, credential_hex)` pins a peer credential to a
receiver-local route slot. It MUST be called before `Node::start`.

For TCP, the credential is the peer's 32-byte Noise static public key. For
quic-mesh, it is the peer certificate's canonical SPKI.

The runtime derives the local `NodeId` from the loaded local credential and
derives the peer `NodeId` from the credential authenticated by the transport.
Admission MUST compare the authenticated credential, derived `NodeId`, current
session, configured route slot, and publication state before exposing the peer
to routing, registry gossip, SWIM, asks, monitors, or links.

An unbound credential, mismatched credential, duplicate identity owner,
retired-identity replay, malformed key, or identity collision MUST fail closed.

## 4. Protocol-epoch 2 handshake

Every connection exchanges exactly 72 bytes before actor traffic:

| Offset | Size | Field |
|---:|---:|---|
| `0` | 4 | magic `HEW\x02` |
| `4` | 2 | protocol version, big-endian, value `2` |
| `6` | 2 | reserved, zero |
| `8` | 4 | schema hash, big-endian |
| `12` | 4 | feature flags, big-endian |
| `16` | 16 | key-derived `NodeId` |
| `32` | 4 | durable session incarnation, big-endian |
| `36` | 4 | reserved, zero |
| `40` | 32 | Noise static public key, or zero when the transport authenticates by SPKI |

The receiver MUST reject:

- any length other than 72 bytes;
- any magic or protocol version other than epoch 2;
- non-zero reserved bytes;
- an all-zero `NodeId`;
- session incarnation `0`;
- schema incompatibility;
- a `NodeId` that differs from the authenticated credential derivation;
- a credential that is not pinned to the selected receiver-local route slot;
- a lower-session replay;
- an equal-session attempt to revive a buried member.

An equal-session reconnect is valid only for a live, non-buried member. A
strictly higher durable session can replace an earlier connection and readmit a
buried member.

## 5. Wire framing and schema

Transport framing carries CBOR wire frames defined by
`hew-runtime/schemas/envelope.cddl`. The wire schema version is `2`.

A wire `location` is a CBOR map containing:

```text
{
    1: node-id bstr of exactly 16 bytes,
    2: non-zero actor slot,
    3: non-zero session incarnation no greater than u32::MAX
}
```

Unknown versions, duplicate or unknown map keys, malformed lengths, zero fields,
non-zero reserved fields, invalid reason tags, and truncated CBOR MUST be
rejected without state mutation.

Actor-message bodies use the per-type `#[wire]` CBOR schema. Message and reply
types crossing a node boundary MUST satisfy the compiler's serializability
requirements.

## 6. Route publication and stale checks

The routing table maps a key-derived `NodeId` to the receiver-local route slot,
connection owner, and durable peer session. Publication occurs only after
transport authentication and handshake admission complete.

Every send, ask, monitor, or link operation resolves the carried `NodeId` to the
current published route, compares the carried session incarnation, and validates
the exact actor slot. It MUST fail closed if any component differs.

A same-key restart with a higher session fences every earlier location, even if
the replacement process deliberately reuses the same actor slot. The exact
public error is `StaleRef`.

When a connection is superseded, the previous connection loses data-plane and
control-plane authority immediately. Payload-carried route hints never restore
that authority.

## 7. Registry and names

`Node::register(name, actor)` publishes the actor's exact `Location`.
`Node::lookup<T>(name)` returns a typed `RemotePid<T>` discovered through the
authenticated registry.

Names are discovery aliases, not identity:

- registering the same name for the same live actor is idempotent;
- repointing a name changes future lookup results only;
- unregistering a name does not revoke an already issued location;
- actor death makes every location for that actor slot stale;
- registry add and remove operations carry the exact `Location`.

Registry gossip from an unauthenticated, unverified, stale, or superseded
connection MUST be rejected.

## 8. Messaging and asks

`RemotePid<T>.send(message)` is fire-and-forget delivery. `RemotePid<T>.ask`
creates a request identifier, sends the typed request, suspends the caller, and
resumes with either the typed reply or `AskError`.

Pending asks are owned by one reply table. Connection loss, SWIM death, local
shutdown, cancellation, timeout, version mismatch, stale identity, and explicit
peer rejection resolve through that authority. A terminal resolution removes
the request exactly once; a later reply or failure signal is ignored.

Distributed operations MUST return typed failure rather than fabricate success
or wait indefinitely after the route is known dead.

## 9. Monitors and `DOWN`

Watcher-side monitor and link registrations share one `MonitorState`
observation authority. Target-side remote watcher entries exist only to fan
terminal control frames back to the owning watcher.

`monitor(remote_pid)` returns `Result<MonitorRef, MonitorError>`.
`MonitorRef::id()` borrows the resource and returns its stable `MonitorId`;
`MonitorRef::close()` removes the registration. Observation identifiers are
non-zero `u64` values, never reused, and exhaust only after issuing
`u64::MAX`.

An actor receives monitor termination through one typed hook:

<!-- doctest: skip -->
```hew
import std::link_monitor::{DownNotification, DownReason, DownTarget};

actor Watcher {
    #[on(down)]
    fn on_down(note: DownNotification) {
        match note.target {
            DownTarget::Remote(location) => {
                println(f"remote actor down: {location}");
            },
            DownTarget::Local(slot) => {
                println(f"local actor {slot} down");
            },
        }
        match note.reason {
            DownReason::Exited => println("clean exit"),
            DownReason::Crashed(_) => println("crash"),
            DownReason::MonitorLost => println("partition"),
            DownReason::LocalShutdown => println("local shutdown"),
        }
    }
}
```

The delivered `DownNotification.monitor` MUST equal the originating
`MonitorRef::id()`. Clean exit, crash, partition, SWIM death, and local shutdown
each produce at most one terminal claim. The first valid terminal claim removes
the observation before mailbox delivery, so duplicate frames and close-versus-
DOWN races cannot produce a second notification.

The copied mailbox payload has one owner and is freed exactly once after hook
dispatch, no-hook discard, or mailbox shutdown drain.

Closing a monitor delivers no `DOWN`. Close, watcher-node death, actor teardown,
connection replacement, and node shutdown MUST leave the corresponding
watcher-side and target-side tables empty.

## 10. Links

Cross-node links use the same observation identifier allocator and route/session
validation as monitors. A link request carries the exact linker and target
locations plus the `PartitionPolicy`.

`CrashLinked` delivers a system exit to the local linked actor. Other policies
retain their documented non-fatal behavior. Link clean exit, crash, partition,
unlink, and shutdown cleanup are claimed by the same lifecycle authority used
for monitor observations.

## 11. Membership, burial, and rejoin

SWIM membership is keyed by `NodeId` and durable node session. SWIM's own
incarnation orders membership updates within that node session.

A DEAD or LEFT member is buried. Equal- or lower-session traffic cannot revive
it. A strictly higher durable session can be admitted, clears quarantine, and
then resumes registry and routing publication.

Quarantine blocks sends, asks, monitors, and links until a valid higher-session
readmission.

## 12. Native-only target posture

Distributed nodes, remote messaging, authenticated transports, registry gossip,
SWIM, monitors, and cross-node links are native-only. The wasm32 checker rejects
these surfaces. There is no wasm networking shim and no success-shaped fallback.

## 13. Configuration example

Each operator exchanges the output of `Node::identity_key()` out of band and
assigns the peer any free local route slot:

<!-- doctest: skip -->
```hew
Node::set_transport("tcp");
Node::load_keys("server.key");
println(f"pin this credential on peers: {Node::identity_key()}");

// Slot 0 is reserved. Slot 1 is this process's local alias for this peer.
Node::allow_peer(1, "8f4c...64-hex-digits-total");
Node::start("0.0.0.0:9000");
```

A client that pinned the server at local route slot `1` connects with:

<!-- doctest: skip -->
```hew
Node::connect("1@127.0.0.1:9000");
let found: Result<RemotePid<Counter>, LookupError> = Node::lookup("counter");
```

The route-slot prefix selects the local credential pin. The authenticated key
derives the peer's `NodeId`; the numeric prefix is never the peer identity.
