# Hew Distributed Runtime Specification

> Actor-call surface: ordinary calls wait for completion; `fork` creates a task
> and `await` joins it. Protocol operations below do not introduce a public
> send keyword or actor-specific await. See [the actor guide](../hew-language-guide.md#actors)
> for completion, mailbox and admission views, including the pending typed
> request-recovery contract. Protocol descriptions are not native cutover
> acceptance evidence.


**Status:** normative for edition 2026. For a released compiler, use the
[specification at its release tag](https://github.com/hew-lang/hew/blob/v0.6.1/docs/specs/HEW-DIST-SPEC.md)
and its [migration notes](../releases/v0.6.1.md).
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
Different nodes MAY assign different route slots to the same peer. A route slot
is assigned by position: the slot a peer occupies is its one-based position in
the `NodeConfig.peers` vector the local node started with, so the first entry is
slot `1` and slot `0` stays reserved (§3). APIs that accept a route-slot
argument, including the `slot@addr` form of `Node.connect`, interpret it only in
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

`Location` is discovery metadata. Moving, storing or duplicating it does not
grant authority and does not affect actor lifetime. `Node.lookup(name)` obtains
locations from authenticated registry discovery. An imported actor value `A`
comes from an authenticated export offer (§7.1), carrying the authority needed
for calls and observations.

### Identity files

Released v0.6.1 loads an existing identity file or creates one when absent.
`NodeConfig.key` must be non-empty; `NodeConfig.at(address)` leaves it empty.
Preserve that file across upgrades and keep it private. Replacing the key
changes the node's identity and the credentials its peers must pin. For TCP,
unreadable files and files with the wrong length are refused rather than regenerated.

**Unreleased hardening on `main`:** the following stricter file checks and
publication guarantees are not part of v0.6.1.

Native node identities are created exclusively and published only after their complete
contents are validated and synced. Concurrent creators adopt the same validated winner;
loading never replaces or truncates an existing identity. Loads reject symbolic links,
Windows reparse points, non-regular files, unsafe ownership and non-private permissions
or ACLs. Public and private key material must agree. Private mesh keys share zeroizing
storage and are omitted from diagnostic formatting.

The supported node transports are TCP with Noise and mutually authenticated QUIC mesh.
The legacy unbound QUIC selector and insecure opt-in are unavailable on `main`.
In released v0.6.1, the legacy `HEW_TRANSPORT=quic` selector is still parsed,
but cannot satisfy authenticated node startup. Its removal from the runtime
is an [unreleased change](../releases/UNRELEASED.md). Ordinary
`std.net.quic` endpoints remain a separate socket API.

## 3. Stable credentials and peer configuration

Node setup is one call. `Node.start(config: NodeConfig) -> Result<(), NodeError>`
is the only start, and a refusal is an `Err` the caller must handle, never a
message on stderr under a program that keeps running.

`NodeConfig` is a prelude record:

```text
NodeConfig {
    bind: string,          // listen address
    transport: NodeTransport, // Tcp | QuicMesh
    key: string,           // path to the stable credential
    peers: Vec<string>,    // pinned peer credentials, slot = position
    seeds: Vec<string>     // addresses dialled once at start
}
```

`NodeConfig.at(addr)` fills the defaults around a bind address.
`NodeConfig.load() -> Result<NodeConfig, NodeError>` reads `[cluster]` plus
`HEW_NODE_*` overrides and is deferred to v0.8.0.

No field is inert. `Node.start` loads or creates the stable credential named by
`key` for the transport named by `transport`, pins every `peers` entry to the
route slot that is its one-based position in that vector, dials every `seeds`
entry once while skipping its own `bind` address. Both transports authenticate
pinned peers. `NodeError.Config` reports invalid configuration, `Key` invalid
identity or peer credentials, `Unreachable` a failed dial, and `Refused` denied
admission or an unavailable node or transport. Re-dial with backoff is v0.8.0.

`Node.identity_key()` returns the local public credential as lowercase
hexadecimal for out-of-band exchange. There is no separate `Node.load_keys`,
`Node.set_transport`, or `Node.allow_peer`: each carried one fact that is now
a `NodeConfig` field, and a setup sequence whose steps can be reordered or
skipped is a second configuration authority.

For TCP, the credential is the peer's 32-byte Noise static public key. For
quic-mesh, it is the peer certificate's canonical SPKI.

The runtime derives the local `NodeId` from the loaded local credential and
derives the peer `NodeId` from the credential authenticated by the transport.
Admission MUST compare the authenticated credential, derived `NodeId`, current
session, configured route slot, and publication state before exposing the peer
to routing, registry gossip, SWIM, asks, monitors, or links.

An unbound credential, mismatched credential, duplicate identity owner,
retired-identity replay, malformed key, or identity collision MUST fail closed.

### 3.1 Channel protection

Every node connection is authenticated and encrypted. No plaintext node
transport exists, and the transport alone selects the protection; nothing a
peer sends can select a weaker one.

- **TCP** runs `Noise_XX_25519_ChaChaPoly_BLAKE2s` with the node's stable
  static key directly after the record of §4. A TCP peer whose record does not
  set feature bit `0` (Noise) MUST be refused before any actor traffic, as MUST
  a failed Noise handshake.
- **quic-mesh** relies on its mutually pinned TLS 1.3 session. Its record
  carries a zero Noise key.
- **Any other transport**, including plain QUIC and Unix sockets, carries no
  node traffic.

Every frame is sealed when sent and opened when received. A frame that fails
authentication, or a TCP send that fails after spending its nonce, ends the
connection; there is no plaintext retry and no nonce reuse. One sealed TCP frame
carries at most 65 519 bytes. A dial that is refused returns
`Err(NodeError.Config)` from `Node.connect`.

## 4. Protocol-version 5 handshake

Version 5 adds authenticated service offers and grant-bearing requests. Upgrade
communicating nodes together: version 4 peers fail the handshake before route
publication, and version 5 never downgrades a grant-bearing request to a raw ask.

Every connection exchanges exactly 72 bytes before actor traffic. The layout
magic remains `HEW\x02`; the independently checked protocol version is `5`:

| Offset | Size | Field |
|---:|---:|---|
| `0` | 4 | magic `HEW\x02` |
| `4` | 2 | protocol version, big-endian, value `5` |
| `6` | 2 | reserved, zero |
| `8` | 4 | schema hash, big-endian |
| `12` | 4 | feature flags, big-endian |
| `16` | 16 | key-derived `NodeId` |
| `32` | 4 | durable session incarnation, big-endian |
| `36` | 4 | reserved, zero |
| `40` | 32 | Noise static public key, or zero when the transport authenticates by SPKI |

The receiver MUST reject:

- any length other than 72 bytes;
- magic other than `HEW\x02` or protocol version other than `5`;
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
`hew-runtime/schemas/envelope.cddl`. The wire schema version is `4`.

Envelope key `5` names the canonical dotted handler declaration for a request,
or carries reply status `0` (success) / `1` (rejection), or withdrawal tag `2`. Local dispatch indices
do not cross the wire. A receiver resolves the handler within the target actor
type before decoding the payload. Root declarations have no portable module
prefix; shared protocols use their declared module/package path.

Envelope key `8` is a remaining request budget in nanoseconds in
`0..=i64::MAX`, or `nil` for an untimed request. One-way frames and replies
MUST carry `nil`. A withdrawal has an exact target, positive request id, nil
source and budget, and an empty payload. Other combinations are rejected.

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

`Node.register(name, actor: A) -> Result<(), RegisterError>` publishes
the actor's exact `Location` and records `A`'s declaration identity beside it.
`Node.lookup(name) -> Result<Location, LookupError>` returns discovery metadata
from the authenticated registry. A location cannot authorize actor operations.
Obtain an imported actor value from an authenticated export offer (§7.1).

`Node.register` is the one registration verb. It registers locally whether or
not a node has started, and publishes cluster-wide once one has.
`Node.unregister(name)` withdraws the name. `whereis<A>(name) ->
Result<A, LookupError>` is the local view of the same registry and
performs the same comparison.

The current registry records and compares declaration identity on the
registering node. Cluster-wide lookup type comparison remains deferred.
Wire version 5 resolves handler declarations when a message is dispatched.

Names are discovery aliases, not identity:

- registering the same name for the same live actor is idempotent;
- repointing a name changes future lookup results only;
- unregistering a name does not revoke an already issued location;
- actor death makes every location for that actor slot stale;
- registry add and remove operations carry the exact `Location`.

Registry gossip from an unauthenticated, unverified, stale, or superseded
connection MUST be rejected.

## 7.1 Supplied service grants

`Node.peer(credential: string) -> Result<net.PeerSession, NodeError>` selects a
configured credential on a currently authenticated connection. It does not dial.
The peer handle binds the local and remote `NodeId` and session incarnations.
`Node.connect(address)` retains its existing discovery and connection behaviour.

`peer.export(actor)` suspends until the recipient acknowledges admission of an
offer, then returns a resource `net.Export<A>`. The issuer owns a bounded grant
record naming the recipient, both sessions, exact actor incarnation and portable
handler wire identities, including canonical generic arguments. Keep the returned export resource alive while the
recipient needs admission: discarding it or leaving its scope revokes the grant
through ordinary affine cleanup. For example:

```hew
import std.net;

actor Store {
    #[wire]
    receive fn get(key: string) -> string {
        key
    }
}

fn offer(peer: net.PeerSession, store: Store, done: Stream<i64>) {
    let export = peer.export(store).expect("offer admission");
    let _ = done.recv();
    export.close();
}
```

The recipient's cancellable
`peer.receive_service<A>()` waits for an offer and returns an ordinary actor
value `A` inside `Result<A, ActorError<Never, Never>>`. An offered incompatible
interface fails promptly; an absent offer waits under ordinary scope cancellation
and deadlines. Different generic
specialisations are incompatible even when they share a handler declaration.
Imported actor methods use ordinary named or positional arguments and the same
completion-call results as local actor methods. Every
request and reply uses the actor's checked wire codec. Native handles cannot be
transferred as payloads or forwarded as service descriptors.

The receiver MUST validate the current authenticated publication, recipient and
sessions, active grant, live actor incarnation and exact handler before decoding
or mailbox admission. Queued dispatch MUST recheck the same authority when it
claims the request. Closing `Export` revokes future claims; an already claimed
request may finish. An actor becomes capability-only at its first export and
remains so for that incarnation: raw locations, names, asks, sends and new link
or monitor requests cannot bypass a grant after revocation. Previously admitted
observations settle through their existing owners. Local calls retain their
normal actor contracts.

Equal-session reconnect permits new calls through the current publication. A
pending call or setup acknowledgement remains bound to its original publication;
it cannot replay or migrate its reply onto a replacement connection. A node
session change, actor retirement or node shutdown makes old descriptors stale.
Service acquisition and remote service calls are native-only; sandbox targets
report the distributed-platform refusal at source.

## 8. Messaging and asks

Calling a handler on an imported actor value uses the same completion-call
syntax as a local actor value. The call creates a request identifier, sends the
typed request, suspends the caller and resumes with its `Result` and
`ActorError` envelope. `mailbox(actor)` selects submission-only delivery and
returns `ActorError<Never, Req>` on failure. Only a definite rejection returns
the original request.

An ask's effective expiry is the earliest incoming request budget, active
scope expiry and explicit timeout. Setup, payload encoding and transport send
admission consume the budget. The sender stamps the remaining budget after
send admission; the receiver establishes a local monotonic expiry on receipt,
before payload decoding and actor admission. Absolute clocks are never sent
or compared. Transit and a blocked transport write cannot be measured across
independent clocks, so this is a relative budget, not a shared-clock deadline.

A caller's expiry or cancellation MUST NOT wait for another sender or for the
peer to accept bytes. Request submission rechecks its remaining budget after
acquiring the send channel. Withdrawal is submitted without waiting for network
completion; a frame already being written finishes in order or faults the
channel.

An expired request MUST NOT start a handler. On caller abandonment or expiry,
the sender also attempts one authenticated withdrawal on the original
connection publication. The receiver matches the exact local target, request
id and authenticated connection publication in its existing inbound owner.
A different peer, node session or replacement connection cannot withdraw that
request. Withdrawal is idempotent and best-effort; receiver-side expiry still
applies if it is lost. It removes queued ownership, while a claimed handler
remains actor-owned and any late reply is released. No retry is automatic.

A claimed handler carries the remaining expiry through suspension, ordinary
calls and child tasks. Nested local, local-Node and remote calls cannot extend
it and report ordinary `ActorError.TimedOut` once exhausted. Required payload
and late-reply cleanup is shielded from the abandoned request expiry; cleanup
may establish its own explicit deadline. Timeout and
partition leave the remote outcome uncertain; neither undoes a mutation.

Inbound asks use the actor-call readiness protocol without retaining an OS
thread while waiting. A node admits at most 64 pending inbound asks for one
exact actor incarnation; saturation returns `WorkerAtCapacity` without
dispatching the request. Another destination retains its own capacity. A
declared mailbox bound still refuses full-mailbox admission independently.
Node shutdown drains completed replies, then cancels remaining observations
after its bounded drain period and releases their request/reply ownership.

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

Monitor and link notifications share `std.failure.DownTarget`: `.Local(u64)`
for a process-local actor and `.Remote(Location)` for the complete remote
identity. `CrashNotification.target` replaces the earlier `actor_id` field;
imports of `DownTarget` move from `std.link_monitor` to `std.failure`.

An actor receives monitor termination through one typed hook:

```hew,ignore
import std.failure.{DownTarget};
import std.link_monitor.{DownNotification, DownReason};

actor Watcher {
    #[on(down)]
    fn on_down(note: DownNotification) {
        match note.target {
            DownTarget.Remote(location) => {
                println(f"remote actor down: {location}");
            }
            DownTarget.Local(slot) => {
                println(f"local actor {slot} down");
            }
        }
        match note.reason {
            DownReason.Exited => println("clean exit"),
            DownReason.Crashed(_) => println("crash"),
            DownReason.MonitorLost => println("partition"),
            DownReason.LocalShutdown => println("local shutdown"),
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

Each operator exchanges the output of `Node.identity_key()` out of band and
lists the peer's credential in `NodeConfig.peers`. A peer's route slot is its
one-based position in that vector, so the single peer below occupies slot `1`
and slot `0` stays reserved for local dispatch:

```hew,no_run
import std.os;

fn main() {
    var config = NodeConfig.at("127.0.0.1:9000");
    config.transport = NodeTransport.Tcp;
    config.key = "server.key";
    config.peers = [os.env("HEW_PEER_CREDENTIAL").expect("supply the peer's public credential")];
    Node.start(config).expect("node starts");
    println(f"pin this credential on peers: {Node.identity_key()}");
    Node.shutdown();
}
```

A client that pinned the server at local route slot `1` connects with:

```hew,ignore
match Node.connect("1@127.0.0.1:9000") {
    .Ok(_) => {},
    .Err(e) => println(f"dial refused: {e}"),
}
let found: Result<Location, LookupError> = Node.lookup("counter");
```

The route-slot prefix selects the local credential pin. The authenticated key
derives the peer's `NodeId`; the numeric prefix is never the peer identity. The
discovered location is metadata; the client receives callable actor authority
from the server's authenticated export offer (§7.1).
