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
Different nodes MAY assign different route slots to the same peer. The canonical
identity-claim owner assigns an unused slot at admission, retains that alias
across the peer's session changes, and rejects exhaustion. Slots never establish
identity or cluster membership and are absent from programmer configuration.

### 2.3 Session incarnation

Each stable node identity owns a durable, non-zero `u32` session incarnation.
Starting a node burns and persists the next incarnation before the node becomes
routable. The journal is locked for exclusive use, recovers from a torn latest
record, rejects a key-derived identity mismatch, and refuses overflow.

A same-key restart therefore keeps the `NodeId` and advances the session
incarnation. A carried location from an earlier session is stale.

### 2.4 `Location`

A carried destination identifies a direct actor allocation or a stable supervised role:

```text
Location {
    node: NodeId,          // 16 bytes
    target_id: u64,        // non-zero exact actor serial or canonical role token
    incarnation: u32,      // non-zero durable node session
    target_kind: u32       // 0 = direct actor, 1 = stable role
}
```

The native C ABI representation is 32 bytes. Target identity `0`, session
incarnation `0` and unknown target kinds are invalid. Direct and role targets
remain distinct even when their scalar identities coincide. Role tokens retain
all 64 bits; receiver-local actor packing never interprets them as actor slots.

Source code compares complete `Location` values and may read `node_id()` and
`incarnation()`. Display includes `actor:<serial>` or `role:<token>`; there is no
public slot projection.

`Location` is discovery metadata. Moving, storing or duplicating it does not
grant authority and does not affect actor lifetime. `Node.lookup(name)` obtains
locations from authenticated registry discovery. An imported actor value `A`
comes from an authenticated export offer (§7.1), carrying the authority needed
for calls and observations.

### Identity files

Released v0.6.1 loads an existing identity file or creates one when absent.
`NodeConfig.key` must be non-empty; `NodeConfig.at(address)` leaves it empty.
Preserve that file across upgrades and keep it private. Replacing the key
changes the node's identity and requires a new cluster enrolment. For TCP,
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
    cluster: string,       // path to this node's cluster enrolment
    seeds: Vec<string>     // addresses discovered and rejoined while the node lives
}
```

`NodeConfig.at(addr)` fills the defaults around a bind address.
Import `std.net` to use `NodeConfig.load() -> Result<NodeConfig, ConfigError>`.
The loader reads `[cluster]` from `hew.toml` in the working directory, or the
file selected by `HEW_NODE_CONFIG`, then applies `HEW_NODE_BIND`,
`HEW_NODE_TRANSPORT`, `HEW_NODE_KEY`, `HEW_NODE_CLUSTER` and `HEW_NODE_SEEDS`.
`bind` and `key` are required non-empty strings; `transport` defaults to `tcp`
and also accepts `quic-mesh`. The cluster path and seed list default to empty. Environment
lists use TOML array syntax, so `HEW_NODE_SEEDS='[]'` clears the configured list.
An absent default file permits environment-only configuration; an explicitly
selected file must exist. Malformed TOML, unknown cluster fields and wrong
field types are errors even when an environment override replaces that field.
File identity and enrolment paths are relative to the configuration file's
directory; environment paths are relative to the process working directory.

Configuration errors carry ordinary Hew values:

```hew
pub type ConfigError {
    source: string;
    field: string;
    message: string;
}
```

`ConfigError` implements `Display` and `Error`; for example,
`hew.toml: cluster.seeds: expected an array of strings`. Loading configuration
does not start a node, create keys or advance a session. Runtime admission
continues to report `NodeError` through `Node.start` and `Node.connect`.

```toml
[cluster]
bind = "127.0.0.1:9000"
transport = "tcp"
key = "node.key"
cluster = "node.enrollment"
seeds = ["127.0.0.1:9001"]
```

Use `hew node identity --key node.key --transport tcp` to load or create a
persistent identity and print its public credential alone. `tcp` is the default;
`quic-mesh` prints the TLS SPKI credential. The command reuses the node's secure
identity storage without starting a node or changing session state. Paste this
credential into `hew node admit` to obtain its cluster enrolment. Admission
allocates internal routing aliases separately from identity.

No field is inert. `Node.start` loads or creates the stable credential named by
`key` for the transport named by `transport`, validates the enrolment named by
`cluster` against that credential, and supervises each
configured seed while skipping its own `bind` address and duplicate targets.
Seed discovery retries initial absence, refused authentication and later
connection loss with capped exponential backoff and jitter until shutdown.
`Node.start` succeeds once the local node and seed supervision are running;
seed availability is not a startup requirement. Both transports authenticate
enrolled peers before publishing routes. Rejoin preserves cluster authority and
session fences and never replays an admitted actor request. Shutdown cancels
retry waits and joins discovery workers. Numeric-address dial and authenticated
admission share a bounded attempt budget; QUIC hostname resolution still uses
the platform resolver, whose completion can delay shutdown.
`NodeError.Config` reports invalid configuration and `Key` invalid identity or
cluster enrolment. Explicit `Node.connect` still reports `Unreachable` for a
failed dial and `Refused` for denied admission or an unavailable transport.

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
- **quic-mesh** relies on its mutually authenticated TLS 1.3 session. Its record
  carries a zero Noise key.
- **Any other transport**, including plain QUIC and Unix sockets, carries no
  node traffic.

Every frame is sealed when sent and opened when received. A frame that fails
authentication, or a TCP send that fails after spending its nonce, ends the
connection; there is no plaintext retry and no nonce reuse. One sealed TCP frame
carries at most 65 519 bytes. A dial that is refused returns
`Err(NodeError.Config)` from `Node.connect`.

## 4. Protocol-version 12 handshake

Version 12 adds checked relay-open, byte-chunk and close controls. An admitted
carrier may introduce another enrolled node's identity and session as a hint.
The recipient establishes a separate end-to-end authenticated logical session
through the carrier before accepting that origin's names, grants or calls.
Noise uses the same transcript and credential verification as direct TCP; TLS
uses the direct mesh certificate verifier and identity material. Both feed the
same checked enrolment, session, publication and grant authorities.
The carrier forwards opaque protected bytes; its own credential cannot attest
to another node's identity. Circuit identifiers, ordered byte offsets and the
carrier's exact publication fence replay and endpoint reuse. Carrier loss
closes its dependent logical sessions and pending admission attempts.

A new authenticated publication sends the origin's current local name snapshot,
including names whose transition gossip has aged out. Name updates are accepted
only from their exact authenticated origin session and publication. Delayed
outbound name intents read the canonical local map under the connection send
gate. Relay carriage preserves the selected channel's frame capacity and actor
request semantics.

Version 11 introduced checked handshake metadata, including cluster enrolment,
through the same generated node schema and codec as other internal controls.
It retains direct-actor and stable-role destinations and typed route-size
failures. Upgrade communicating nodes together: earlier peers fail before route
publication.

Each connection exchanges one bounded CBOR `NodeHandshake` record from
`node_controls.hew`: protocol version, schema hash, feature flags, key-derived
`NodeId`, durable session incarnation, Noise public key (zero for mesh), and
cluster enrolment bytes. Generated bindings own the field tags and integer
widths. Fixed-offset handshake records are retired. Noise binds the ordered canonical
initiator and responder records into its handshake transcript before key proof;
changing a node/session identity, negotiated contract or enrolment invalidates
the handshake. TLS carries the same checked metadata inside its protected session.

The receiver MUST reject:

- oversized, malformed, duplicate, unknown, missing or trailing fields;
- protocol version other than `12`;
- an all-zero `NodeId`;
- session incarnation `0`;
- schema incompatibility;
- a `NodeId` that differs from the authenticated credential derivation;
- a missing, forged or foreign-authority enrolment, or one for a different
  proved transport credential;
- a lower-session replay;
- an equal-session attempt to revive a buried member.

An equal-session reconnect is valid only for a live, non-buried member. A
strictly higher durable session can replace an earlier connection and readmit a
buried member.

## 5. Wire framing and schema

Transport framing carries CBOR wire frames defined by
`hew-runtime/schemas/envelope.cddl` together with
`hew-runtime/schemas/node-controls.generated.cddl`. The wire schema version
is `11`.

Service offers, registry gossip, SWIM and monitor/link DOWN payloads use checked
internal Hew data declarations in `hew-runtime/protocol/node_controls.hew`. The frontend's closed
data plans generate concrete Rust value walks and CDDL through
`make node-protocol`. These walks use the same format-neutral `hew_codec`
Sink/Source implementation as developer wire types. Internal controls opt into
recursive unknown-field refusal; ordinary evolving user data retains its
existing policy. Runtime adapters continue to enforce grants, identity, terminal
reasons and size limits. Valid CBOR bytes remain stable; shared readers reject
malformed or trailing input. Other frame and handshake structures remain separate
pending the next protocol cutover.

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
    2: non-zero exact actor serial or canonical role token,
    3: non-zero session incarnation no greater than u32::MAX,
    4: target kind (0 = direct actor, 1 = stable role)
}
```

Unknown versions, duplicate or unknown map keys, malformed lengths, zero fields,
unknown target kinds, invalid reason tags, and truncated CBOR MUST be
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
the semantic target. Direct actors resolve by exact allocation serial. Roles
resolve through the canonical owning supervisor and select one exact worker
incarnation. Every admitted call freezes that worker and its codecs; dispatch
rechecks the grant without resolving a replacement or replaying the request.
It MUST fail closed if identity, session or authority differs.

A same-key restart with a higher session fences every earlier location, even if
the replacement process deliberately reuses the same scalar target identity. The exact
public error is `StaleRef`.

When a connection is superseded, the previous connection loses data-plane and
control-plane authority immediately. Payload-carried route hints never restore
that authority.

### 6.1 Message size and route capacity

Local actor calls transfer ordinary values without wire serialization or a
transport-derived size cap. `#[wire]` supplies the portable codec; it does not
restrict a local value to a network frame's capacity.

A remote route checks the complete encoded frame, including the body, handler
identity, locations and envelope metadata, before consuming a cipher nonce or
writing bytes. Encoded expansion counts. TCP Noise currently admits one
plaintext frame of at most 65,519 bytes; QUIC mesh admits a framed payload of
at most 16 MiB. These are transport capacities, not a public body-size API.
Transparent segmentation of large Noise messages remains future transport work.

An oversized request reports `ActorError.MessageTooLarge`. A reject-policy
call or mailbox submission instead returns its sealed unaccepted request as
`ActorError.Rejected`, with `SendError.MessageTooLarge`. It can be redirected
to a compatible local actor through the existing `.to(target)` operation.
The destination handler has not claimed that request. Other pending or later
calls keep their connection and authority.

An oversized successful reply or declared handler error reports
`ActorError.MessageTooLarge` through a bounded terminal failure. The handler
has already run; its reply is released once and is neither truncated nor
replayed. Only `Rejected` returns an unaccepted request for safe resubmission.
Other error variants alone establish neither handler execution nor permission
to repeat a call. Hostile inbound frames still obey the bounded transport
length checks and authenticated decoding rules.

## 7. Registry and names

`Node.register(name, actor: A) -> i32` publishes
the actor's direct or stable-role `Location`; zero indicates success.
`Node.lookup(name) -> Result<Location, LookupError>` returns discovery metadata
from the authenticated registry. A location cannot authorize actor operations.
Obtain an imported actor value from an authenticated export offer (§7.1).

`Node.register` requires a running node and publishes through its registry.
Completed direct actor shutdown withdraws its names even while handles remain.
A role name survives worker replacement and is withdrawn when its stable owning
supervisor completes shutdown. Explicit unregister is currently a native runtime
operation; a `Node.unregister` Hew spelling is not implemented. Lookup returns
metadata; it does not reconstruct an actor handle or compare actor interfaces.
Authenticated grants validate portable handler declarations.

Names are discovery aliases, not identity:

- registering the same name for the same live actor is idempotent;
- repointing a name changes future lookup results only;
- unregistering a name does not revoke an already issued location;
- direct actor death makes its direct location stale;
- role worker replacement preserves the role location and existing live grants;
- registry add and remove operations carry the exact `Location`.

Registry gossip from an unauthenticated, unverified, stale, or superseded
connection MUST be rejected.

### Membership and naming observation

`std.net.cluster.snapshot()` returns a copied `Snapshot` containing the local
`Member`, known `members` and all known `registrations`. A `Member` carries the
canonical `NodeId`, durable `session` and `State` (`Alive`, `Suspect`, `Dead` or
`Left`). A `Registration` carries a name and exact discovery `Location`.
`Snapshot.local_registrations()` selects registrations of the local identity.
These are current local observations, not quorum or liveness guarantees. The
membership and name collections do not form a cross-collection transaction.

`cluster.watch()` returns an instance-bound `Subscription` with `initial`,
`changes: Stream<()>` and `snapshot()`. Subscription precedes the initial read:
a concurrent change is visible in that read or leaves readiness queued,
possibly both. Readiness coalesces and consumers re-read snapshots; this is not
a lossless departure/join event log. Each node admits at most 64 live
subscriptions and reports `ObserveError.Limit` beyond that bound.

`cluster.wait_registered(name)` waits cancellably for a current discovery
location. `cluster.wait_departed(member)` waits until that exact identity and
durable session is absent, dead or left; a higher-session rejoin does not hide
the predecessor's departure. Neither operation mints actor authority. Both
subscribe before checking, use ordinary stream waits and release their
subscriptions on completion or scope cancellation.

A subscription never adopts a replacement node instance. Snapshot reads report
`ObserveError.NotRunning` when admission closes. The changes stream reaches EOF
after owned network workers and teardown finish, with any queued readiness
consumed first. `subscription.wait_shutdown()` drains that stream to completion
and remains cancellable by its enclosing scope. It does not request shutdown.
Global snapshot and watch creation require a running node.

## 7.1 Supplied service grants

`Node.peer(id: NodeId) -> Result<net.PeerSession, NodeError>` selects a
currently authenticated node session. It does not dial. Obtain the identity from
a discovered `Location.node_id()` or an authenticated membership snapshot;
cluster enrolment owns the corresponding credential. The peer handle binds the
local and remote `NodeId` and session incarnations. A discovered identity alone
grants no actor access: the issuer must still offer an actor interface.
`Node.connect(address)` retains its existing discovery and connection behaviour.

`peer.export(actor)` suspends until the recipient acknowledges admission of an
offer, then returns a resource `net.Export<A>`. The issuer owns a bounded grant
record naming the recipient, both sessions, semantic target and portable
handler wire identities, including canonical generic arguments. A direct export
binds its exact actor incarnation; a role export binds the canonical role and
its owning lifetime. Role interface checks use the declared actor specification,
so an empty worker slot does not require a substitute worker. Keep the returned export resource alive while the
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
and deadlines. Different generic specializations are incompatible even when
they share a handler declaration.
Imported actor methods use ordinary named or positional arguments and the same
completion-call results as local actor methods. Every
request and reply uses the actor's checked wire codec. Native handles cannot be
transferred as payloads or forwarded as service descriptors.

A new export through a nested supervisor requires its containing supervisor's
declared interface metadata to be available. While that owner is absent during
replacement, export returns `StaleRef`; stable role identity and existing grants
remain valid. New export can succeed again after owner restart. Making descendant
interface metadata available throughout that gap remains follow-up work.

The receiver MUST validate the current authenticated publication, recipient and
sessions, active grant, selected live actor incarnation and exact handler before decoding
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
            DownTarget.Role(token) => {
                println(f"local supervised role {token} has no occupant");
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

SWIM control payloads (`SwimControl` in `node_controls.hew`) carry
`msg_type`, the sender's session and incarnation, an optional `target`
session and piggybacked gossip. A receiver accepts a frame only when `from`
is the connection's authenticated peer and `target`, when present, is an
admitted session:

- `PING` (1) is answered with an `ACK` (2).
- `PING_REQ` (3) names the probed member in `target`. The relay sends that
  member a `PING` and, when the member's `ACK` arrives within
  `ping_timeout_ms`, forwards an `ACK` to the requester with `target` set to
  the member, once per request.
- An `ACK` with `target` set is a forwarded `ACK`. It is an ordinary `ACK`
  from the relay, and it counts as contact with the target only when it
  answers a `PING_REQ` the receiver sent that relay about that target within
  `ping_timeout_ms`, once per request; an unsolicited one is not contact.
  Its `incarnation` is the relay's and is never applied to the target.

A node that predates forwarded ACKs never forwards, so a requester probing
through it sees no indirect answer, as before. It validates `target` on a
forwarded `ACK` like any other and treats the frame as a plain `ACK` from the
relay. The payload schema is unchanged.

A PING, ACK or `PING_REQ` is an ALIVE claim by its sender at its stated
incarnation. A member suspected or declared DEAD at or above its own
incarnation refutes by advancing its incarnation. Every SWIM frame sent to a
member its sender holds DEAD leads its gossip with that member's DEAD entry.
The entry takes one of the frame's `max_gossip_per_msg` slots but not the
event's retransmit budget, so a buried member that stays connected learns of
its burial and refutes it even when every gossiped copy was lost.

A member is suspected when a direct probe stays unanswered past
`ping_timeout_ms`. With `N` members and protocol period `P`, a member silent
since its last contact is probed within `(N-1)·P`, and its probe is overdue
at the first period strictly past the timeout, so it is suspected within
`(N-1)·P + (⌊ping_timeout_ms / P⌋ + 1)·P` of the last contact while the
failure detector is cold. A warm detector (at least 10 contact intervals)
also requires `φ > 8`, which can delay suspicion further; see
[failure-philosophy](./failure-philosophy.md#detection-time). A SUSPECT member
becomes DEAD at the first period strictly past `suspect_timeout_ms` after its
last contact, and never in the period that suspected it.

A DEAD or LEFT member is buried. Within its durable session it returns only
through its own SWIM traffic or gossip at a strictly higher incarnation; an
equal-session reconnect, or any claim at or below the buried incarnation,
cannot revive it. A strictly higher durable session can be admitted. Either
readmission clears quarantine and then resumes registry and routing
publication; a member returning from DEAD re-announces its registered names.

Quarantine blocks sends, asks, monitors, and links until a valid readmission.
SWIM frames are not blocked, so the protocol can reach a buried member.

## 12. Native-only target posture

Distributed nodes, remote messaging, authenticated transports, registry gossip,
SWIM, monitors, and cross-node links are native-only. The wasm32 checker rejects
these surfaces. There is no wasm networking shim and no success-shaped fallback.

## 13. Configuration example

An operator creates one cluster authority with `hew node authority --key authority.key`,
loads each public credential with `hew node identity --key node.key`, and issues
an enrolment with `hew node admit --authority authority.key --credential HEX
--output node.enrollment`. Add `--transport quic-mesh` to both identity and admit
for mesh nodes. The authority key remains with the operator. Each node installs
its own identity and enrolment; peers authenticate under that same root.

```hew,no_run
import std.os;

fn main() {
    var config = NodeConfig.at("127.0.0.1:9000");
    config.transport = NodeTransport.Tcp;
    config.key = "server.key";
    config.cluster = os.env("HEW_NODE_CLUSTER").expect("supply the cluster enrolment path");
    Node.start(config).expect("node starts");
    println(f"node credential: {Node.identity_key()}");
    Node.shutdown();
}
```

An enrolled client connects with:

```hew,ignore
match Node.connect("127.0.0.1:9000") {
    .Ok(_) => {},
    .Err(e) => println(f"dial refused: {e}"),
}
let found: Result<Location, LookupError> = Node.lookup("counter");
```

The enrolled transport key derives the peer's `NodeId`. A discovered location
is metadata; the client receives callable actor authority
from the server's authenticated export offer (§7.1).
