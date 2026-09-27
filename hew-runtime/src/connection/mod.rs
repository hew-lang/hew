// native-only: reader thread model requires OS threads; not available on WASM
//! Per-connection transport actors for the Hew runtime.
//!
//! Replaces the global-mutex-protected connection array in [`crate::node`]
//! with individual actors per connection. Each connection actor owns a
//! transport connection handle and runs a dedicated reader thread for
//! inbound messages.
//!
//! # Architecture
//!
//! ```text
//! ConnectionManager
//!   ├── ConnectionActor[0] ─── reader thread ─── transport recv
//!   ├── ConnectionActor[1] ─── reader thread ─── transport recv
//!   └── ConnectionActor[N] ─── reader thread ─── transport recv
//! ```
//!
//! Each `ConnectionActor` has:
//! - A transport connection ID
//! - A reader thread that calls `recv` and routes to local actors
//! - Heartbeat tracking (last activity timestamp)
//! - Connection state (connecting, active, draining, closed)
//!
//! # C ABI
//!
//! - [`hew_connmgr_new`] — Create a connection manager.
//! - [`hew_connmgr_free`] — Destroy a connection manager.
//! - [`hew_connmgr_add`] — Add a connection (spawns reader thread).
//! - [`hew_connmgr_remove`] — Remove and close a connection.
//! - [`hew_connmgr_send`] — Send a message over a connection.
//! - [`hew_connmgr_set_outbound_capacity`] — Legacy API (returns error).
//! - [`hew_connmgr_count`] — Number of active connections.
//! - [`hew_connmgr_broadcast`] — Send to all connections.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_int;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};

use crate::cluster::HewCluster;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::node_identity::NodeId;
use crate::peer_binding::{LiveClaim, PeerAuthSnapshot, PeerCredential, Posture};
use crate::routing::HewRoutingTable;
use crate::transport::HewTransport;
use crate::util::{CondvarExt, MutexExt};

mod admission;
mod control;
mod gossip;
mod handshake;
mod identity_claim;
mod manager;
mod peer;
mod reader;
mod reconnect;
mod send;
mod swim;

pub use admission::*;
pub(crate) use gossip::*;
pub(crate) use handshake::*;
pub use manager::*;
pub(crate) use peer::*;
pub use send::*;
pub(crate) use swim::*;

// ── Connection states ──────────────────────────────────────────────────

/// Connection is being established.
pub const CONN_STATE_CONNECTING: i32 = 0;
/// Connection is active and ready for I/O.
pub const CONN_STATE_ACTIVE: i32 = 1;
/// Connection is draining (no new sends, waiting for in-flight messages).
/// Currently recognised by the state machine but not automatically entered
/// during shutdown — callers must set this state explicitly.
pub const CONN_STATE_DRAINING: i32 = 2;
/// Connection is closed.
pub const CONN_STATE_CLOSED: i32 = 3;

const HEW_HANDSHAKE_SIZE: usize = 72;
const HEW_HANDSHAKE_MAGIC: [u8; 4] = *b"HEW\x02";
const HEW_PROTOCOL_VERSION: u16 = 2;
// Advertised only when the `encryption` feature is compiled in; both consumers
// (`local_feature_flags` and `supports_encryption`) are encryption-gated.
#[cfg(feature = "encryption")]
const HEW_FEATURE_SUPPORTS_ENCRYPTION: u32 = 1 << 0;
const HEW_FEATURE_SUPPORTS_GOSSIP: u32 = 1 << 1;
// Bit 2 (HEW_FEATURE_SUPPORTS_REMOTE_SPAWN) is reserved; not advertised until a
// bootstrap-based remote-spawn path is implemented.
/// Indicates that this node understands `HEW_REPLY_REJECT_MSG_TYPE = 65535` in
/// reply envelopes.  A node MUST only send the rejection sentinel to peers that
/// advertise this flag; old nodes would misinterpret it as a void-success reply.
pub(crate) const HEW_FEATURE_SUPPORTS_ASK_REJECTION: u32 = 1 << 3;
const MAX_REGISTRY_GOSSIP_FLUSH_EVENTS: usize = 64;

/// Ceiling on retry ATTEMPTS for a parked registry-gossip flush (finding:
/// unbounded retries). After this many failed drains the parked frames are
/// dropped with a loud diagnostic — fail-closed name loss on a connection
/// whose sends persistently fail (it is about to be torn down anyway) beats
/// an unbounded retry stream.
const MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS: u32 = 8;
const FNV1A32_OFFSET_BASIS: u32 = 2_166_136_261;
const FNV1A32_PRIME: u32 = 16_777_619;

const NOISE_STATIC_PUBKEY_LEN: usize = 32;
#[cfg(feature = "encryption")]
const NOISE_PATTERN: &str = "Noise_XX_25519_ChaChaPoly_BLAKE2s";
#[cfg(feature = "encryption")]
const NOISE_MAX_MSG_SIZE: usize = 65_535;

const RECONNECT_DEFAULT_MAX_RETRIES: u32 = 5;
const RECONNECT_INITIAL_BACKOFF_MS: u64 = 1_000;
const RECONNECT_MAX_BACKOFF_MS: u64 = 30_000;
const RECONNECT_SLEEP_SLICE_MS: u64 = 100;
const RECONNECT_JITTER_MIN_PERCENT: u64 = 90;
const RECONNECT_JITTER_MAX_PERCENT: u64 = 110;

// ── Connection actor ───────────────────────────────────────────────────

/// Fixed-size protocol handshake exchanged before actor traffic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HewHandshake {
    protocol_version: u16,
    schema_hash: u32,
    feature_flags: u32,
    node_id: NodeId,
    session_incarnation: u32,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
}

impl HewHandshake {
    fn serialize(self) -> [u8; HEW_HANDSHAKE_SIZE] {
        let mut out = [0u8; HEW_HANDSHAKE_SIZE];
        out[0..4].copy_from_slice(&HEW_HANDSHAKE_MAGIC);
        out[4..6].copy_from_slice(&self.protocol_version.to_be_bytes());
        // 6..8 and 36..40 are reserved and remain zero.
        out[8..12].copy_from_slice(&self.schema_hash.to_be_bytes());
        out[12..16].copy_from_slice(&self.feature_flags.to_be_bytes());
        out[16..32].copy_from_slice(&self.node_id.to_bytes());
        out[32..36].copy_from_slice(&self.session_incarnation.to_be_bytes());
        out[40..72].copy_from_slice(&self.static_noise_pubkey);
        out
    }

    fn deserialize(buf: &[u8]) -> Option<Self> {
        if buf.len() != HEW_HANDSHAKE_SIZE
            || buf[0..4] != HEW_HANDSHAKE_MAGIC
            || buf[6..8] != [0, 0]
            || buf[36..40] != [0, 0, 0, 0]
        {
            return None;
        }
        let mut node_id = [0u8; 16];
        node_id.copy_from_slice(&buf[16..32]);
        if node_id == [0; 16] {
            return None;
        }
        let mut static_noise_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
        static_noise_pubkey.copy_from_slice(&buf[40..72]);
        let handshake = Self {
            protocol_version: u16::from_be_bytes([buf[4], buf[5]]),
            schema_hash: u32::from_be_bytes([buf[8], buf[9], buf[10], buf[11]]),
            feature_flags: u32::from_be_bytes([buf[12], buf[13], buf[14], buf[15]]),
            node_id: NodeId::from_bytes(node_id),
            session_incarnation: u32::from_be_bytes([buf[32], buf[33], buf[34], buf[35]]),
            static_noise_pubkey,
        };
        (handshake.session_incarnation != 0).then_some(handshake)
    }
}

/// Per-connection actor state.
///
/// Each connection actor owns a transport connection handle and tracks
/// connection health via heartbeat timestamps.
struct ConnectionActor {
    /// Transport connection ID (index into transport's internal array).
    conn_id: c_int,
    /// Monotonic token used to suppress stale connection-lifecycle callbacks.
    publication_token: u64,
    /// Serializes establish/remove publication for this specific connection.
    publication_sync: Arc<Mutex<()>>,
    /// Set once removal begins so delayed establish publication can abort.
    publication_removed: Arc<AtomicBool>,
    /// Pins this transport slot against manager removal/reuse while a deferred
    /// setup result is being sent. Teardown closes first, then waits for idle.
    claimed_send_lifecycle: Arc<ReaderLifecycle>,
    /// Remote node identity from handshake.
    peer_node_id: u16,
    /// Authenticated key-derived identity from the v2 handshake.
    peer_identity: Option<NodeId>,
    /// Authenticated durable peer session from the v2 handshake.
    peer_session_incarnation: u32,
    /// Remote capability bitfield from handshake.
    peer_feature_flags: u32,
    /// Per-connection admission posture (issue #2652). `Strict` connections
    /// carry authenticated identity + control-plane authority; `Unverified`
    /// connections are delivery-only (no cluster/gossip/ask authority). Defaults
    /// to `Strict` (fail-closed) until admission classifies the endpoint.
    posture: Posture,
    /// The authenticated credential this connection presented (issue #2652).
    /// `Some` only for a `Strict` admission bound to the claimed `NodeId`;
    /// `None` under `Unverified` posture. Used as part of the exact-owner key
    /// `(credential, conn_id, publication_token)` for claim publish/retire.
    credential: Option<PeerCredential>,
    /// Current connection state.
    state: AtomicI32,
    /// Monotonic timestamp (ms) of last successful send or recv.
    last_activity_ms: Arc<AtomicU64>,
    /// Optional per-connection Noise transport state.
    #[cfg(feature = "encryption")]
    noise_transport: Arc<Mutex<Option<snow::TransportState>>>,
    /// Handle to the reader thread (if running).
    reader_handle: Option<JoinHandle<()>>,
    /// Signal to stop the reader thread.
    reader_stop: Arc<AtomicI32>,
    /// The same-credential `Published` claim this admission superseded at
    /// reserve time (issue #2652, D3), if any. Stashed here (in addition to
    /// the admission thread's local copy) so `hew_connmgr_remove` can restore
    /// it when it aborts a still-`Reserved` reservation — the
    /// remove-before-publication path, where `publish_connection_established`
    /// early-returns on `publication_removed` and would otherwise leave the
    /// reservation dangling and the reader parked in the admission wait until
    /// the `CLAIM_RESERVE_WAIT_MS` backstop. Consumed at most once: the
    /// remove-side abort is guarded by the exact `(conn_id, token, Reserved)`
    /// owner check, so a published or superseded claim never restores from it.
    superseded_claim: Mutex<Option<LiveClaim>>,
    /// Optional reconnect settings for this connection.
    reconnect: Option<ReconnectSettings>,
    /// Transport pointer for defense-in-depth close in `Drop`.
    ///
    /// Null for test-only actors created without a manager; [`close_transport_conn`]
    /// is null-safe so drop is unconditionally safe.
    transport: *mut HewTransport,
    /// Exactly-once close ownership for this connection's transport handle.
    ///
    /// Shared by `Arc` so a caller that loses the claim can still wait for the
    /// winner's close to land after the actor has been unlinked and dropped.
    transport_close: Arc<TransportClose>,
}

// ── Connection manager ─────────────────────────────────────────────────

/// Manages a dynamic set of connection actors.
///
/// Replaces the fixed `[c_int; 64]` array in [`crate::node::HewNode`]
/// with a growable `Vec` of per-connection actors.
#[derive(Debug)]
pub struct HewConnMgr {
    // native-only: ConnectionActor reader threads do not exist on WASM
    /// Active connections (protected by [`PoisonSafe`] for concurrent add/remove).
    connections: PoisonSafe<Vec<ConnectionActor>>,
    /// Optional caller-supplied peer identity expectations, consumed exactly
    /// once by `hew_connmgr_add` after the protocol handshake.
    expected_peer_ids: PoisonSafe<HashMap<c_int, u16>>,
    /// Transport used for I/O operations.
    pub(crate) transport: *mut HewTransport,
    /// Callback for routing inbound messages to local actors.
    /// Signature: `fn(target_actor_id: u64, msg_type: i32, data: *mut u8, size: usize)`.
    inbound_router: Option<InboundRouter>,
    /// Optional shared routing table for node-id -> connection routes.
    routing_table: *mut HewRoutingTable,
    /// Optional cluster handle for SWIM connection notifications.
    cluster: *mut HewCluster,
    /// Whether automatic reconnect attempts are enabled.
    reconnect_enabled: AtomicBool,
    /// Default maximum retries for newly configured reconnecting connections.
    reconnect_max_retries: AtomicU32,
    /// Global shutdown signal shared with reconnect workers and stop-time
    /// ask-reply teardown guards.
    reconnect_shutdown: Arc<AtomicBool>,
    /// Spawn gate for inbound workers, distinct from `reconnect_shutdown`.
    ///
    /// `hew_node_stop` sets this FIRST — before draining in-flight workers —
    /// so the drain terminates (no new workers spawn) while already-running
    /// `handle_inbound_ask` threads still flush their computed replies to the
    /// wire (those threads bail only on `reconnect_shutdown` / `CURRENT_NODE`,
    /// which are set AFTER the drain). Separating the gate from the teardown
    /// guard is what lets a graceful stop deliver in-flight replies instead of
    /// abandoning them as a spurious `ConnectionDropped`. Reverse-link setup
    /// workers use the same gate and drain because they also access node state.
    ///
    /// This flag and `inbound_ask_active` form a Dekker pair accessed under
    /// `SeqCst` (see `node_inbound_router` and `drain_inbound_ask_workers`): the
    /// store here is ordered with the counter load in the drain such that a
    /// router which passes the gate is always visible to a concurrent drain, so
    /// no worker can spawn after the drain observed a zero counter.
    inbound_spawn_closed: Arc<AtomicBool>,
    /// Count of inbound ask and reverse-link setup workers active for this manager.
    ///
    /// Incremented before each worker's gate re-check (so a concurrent drain
    /// always sees a spawning worker), then decremented by its RAII guard. Used
    /// by `hew_node_stop` to drain workers before freeing node resources.
    /// Accessed under `SeqCst` on the spawn/drain path.
    pub(crate) inbound_ask_active: Arc<AtomicUsize>,
    /// Background reconnect worker handles.
    reconnect_workers: PoisonSafe<Vec<JoinHandle<()>>>,
    /// Deferred reverse-link setup workers, joined before manager teardown.
    reverse_link_workers: PoisonSafe<Vec<JoinHandle<()>>>,
    /// Counts every spawned reader until its thread function fully returns.
    ///
    /// A reader can remove and drop its own [`ConnectionActor`] on an unexpected
    /// peer close. That self-drop cannot join the current thread, so the actor
    /// disappearing from `connections` is not a sufficient teardown barrier.
    /// `hew_connmgr_free` waits on this lifecycle before the manager's owner
    /// frees routing/cluster state that reader cleanup may still touch.
    reader_lifecycle: Arc<ReaderLifecycle>,
    /// Monotonic token generator for connection-lifecycle publications.
    next_publication_token: AtomicU64,
    /// The node ID advertised in the handshake for this manager's node.
    /// Stored explicitly so multi-node tests (two nodes in one process) get the
    /// correct ID in their outgoing handshake even when `LOCAL_NODE_ID` refers
    /// to a different (`CURRENT_NODE`) node.
    pub(crate) local_node_id: u16,
    /// The frozen per-node peer-authentication authority this manager admits
    /// connections against. Installed from the owning node's `PeerAuthSnapshot`
    /// at construction; never the process-global `ACTIVE_*` credential statics.
    /// Concurrent managers hold independent snapshots, so there is no shared
    /// admission authority across nodes.
    pub(crate) auth: PeerAuthSnapshot,
    /// Key-derived local identity advertised by the v2 handshake.
    local_identity: Option<NodeId>,
    /// Durable local session incarnation advertised by the v2 handshake.
    local_session_incarnation: Option<u32>,
    /// Live `NodeId` claim table (issue #2652, D3). The single serializing guard
    /// for the reserve → publish → retire window: exactly one connection may own
    /// a `NodeId`'s route + cluster token at a time. The condvar coordinates the
    /// reserve/publish handoff so a concurrent admission for the same `NodeId`
    /// waits rather than racing. Per-manager, so two concurrent nodes hold
    /// independent claim tables (a `NodeId` on node1 never collides with node2).
    pub(crate) claims: (Mutex<HashMap<NodeId, LiveClaim>>, Condvar),
    /// Encoded registry-gossip flush frames whose initial send failed, parked
    /// for retry, keyed by connection and bound to that admission's
    /// publication token. The connection-establish flush is one-shot — the
    /// drained events age out of the cluster queue after eight disseminations
    /// — so a transiently failed send would otherwise leave an
    /// already-connected peer permanently without the cluster's registered
    /// names (a lookup-unresolved loss). The connection's reader retries on
    /// its next inbound frame (SWIM keeps frames flowing, so retry latency is
    /// bounded by the protocol period); entries are dropped when the
    /// connection is removed or superseded (fail-closed: a successor's own
    /// flush carries current state). Bounded per connection at
    /// `MAX_REGISTRY_GOSSIP_FLUSH_EVENTS` frames.
    pending_registry_flush: PoisonSafe<HashMap<c_int, PendingRegistryFlush>>,
    /// Fast-path mirror of `pending_registry_flush.len()` so the per-frame
    /// reader check is one atomic load when nothing is parked.
    pending_registry_flush_count: AtomicUsize,
}

/// A parked registry-gossip flush awaiting retry (see
/// [`HewConnMgr::pending_registry_flush`]).
#[derive(Debug)]
struct PendingRegistryFlush {
    /// Publication token of the admission that parked these frames; a stale
    /// entry (token mismatch) is never consumed by a successor connection
    /// reusing the transport `conn_id`.
    token: u64,
    /// Encoded control frames, in original flush/broadcast order (per-
    /// connection FIFO: later registry events park BEHIND earlier unsent ones,
    /// so an ADD can never be replayed after a newer REMOVE for the same name
    /// — ordering is preserved end to end).
    frames: Vec<Vec<u8>>,
    /// Failed drain attempts so far (bounded by
    /// [`MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS`]).
    attempts: u32,
}

#[derive(Debug, Default)]
struct ReaderLifecycle {
    active: Mutex<usize>,
    idle: Condvar,
}

impl ReaderLifecycle {
    fn register(self: &Arc<Self>) -> ReaderLifecycleGuard {
        let mut active = self.active.lock_or_recover();
        *active = active
            .checked_add(1)
            .expect("reader lifecycle active count overflow");
        ReaderLifecycleGuard {
            lifecycle: Arc::clone(self),
        }
    }

    fn wait_for_idle(&self) {
        let mut active = self.active.lock_or_recover();
        while *active > 0 {
            active = self.idle.wait_or_recover(active);
        }
    }
}

struct ReaderLifecycleGuard {
    lifecycle: Arc<ReaderLifecycle>,
}

impl Drop for ReaderLifecycleGuard {
    fn drop(&mut self) {
        let mut active = self.lifecycle.active.lock_or_recover();
        *active = active
            .checked_sub(1)
            .expect("reader lifecycle active count underflow");
        if *active == 0 {
            self.lifecycle.idle.notify_all();
        }
    }
}

/// Which stage of its one and only close a connection's transport handle is in.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum TransportCloseState {
    /// Nobody owns the close yet.
    #[default]
    Open,
    /// Exactly one caller has claimed the close and is inside it.
    Claimed,
    /// The claiming caller's `close_transport_conn` has returned.
    Closed,
}

/// Exactly-once close ownership for one connection's transport handle.
///
/// The handle is one-shot: closing it twice frees a freed connection. Several
/// teardown paths can reach the same connection concurrently (`remove_connection`
/// from an explicit removal, from a refusal and from a woken reader;
/// `hew_connmgr_free`; the actor's own `Drop`), so the close cannot be a flag
/// that each of them stores and then acts on — it has to be *acquired*.
///
/// [`TransportClose::claim`] is that acquisition: it succeeds for exactly one
/// caller across all of them. Every other caller must NOT close. It must,
/// however, call [`TransportClose::wait_closed`] before it drops the actor,
/// because dropping joins the reader thread and only the real close wakes a
/// reader parked in `recv()`.
#[derive(Debug, Default)]
struct TransportClose {
    state: Mutex<TransportCloseState>,
    closed: Condvar,
    /// Callers currently parked in [`TransportClose::wait_closed`] behind the
    /// owner's close. Observability only — the teardown-race tests use it to
    /// tell "the racing teardown is blocked behind the claim" apart from "the
    /// racing teardown has not arrived yet" without guessing at a sleep.
    waiters: AtomicUsize,
}

impl TransportClose {
    /// Acquire the right to close this transport handle.
    ///
    /// Returns `true` for exactly one caller, ever. A `false` return means
    /// another caller owns the close and this caller must leave the handle
    /// alone — fail closed rather than close a handle we do not own.
    fn claim(&self) -> bool {
        let mut state = self.state.lock_or_recover();
        if *state == TransportCloseState::Open {
            *state = TransportCloseState::Claimed;
            true
        } else {
            false
        }
    }

    /// Publish that the claimed close has returned, releasing every waiter.
    fn finish(&self) {
        let mut state = self.state.lock_or_recover();
        *state = TransportCloseState::Closed;
        drop(state);
        self.closed.notify_all();
    }

    /// Block until the owner's close has returned.
    ///
    /// Returns immediately when the handle was never claimed (nothing will
    /// close it) or when the close already landed.
    fn wait_closed(&self) {
        let mut state = self.state.lock_or_recover();
        self.waiters.fetch_add(1, Ordering::Release);
        while *state == TransportCloseState::Claimed {
            state = self.closed.wait_or_recover(state);
        }
        self.waiters.fetch_sub(1, Ordering::Release);
    }

    #[cfg(test)]
    fn waiters(&self) -> usize {
        self.waiters.load(Ordering::Acquire)
    }
}

struct ClaimedSendLease {
    _guard: ReaderLifecycleGuard,
    publication_removed: Arc<AtomicBool>,
    #[cfg(feature = "encryption")]
    noise_transport: Arc<Mutex<Option<snow::TransportState>>>,
}

#[derive(Clone, Debug)]
struct ReconnectSettings {
    target_addr: String,
    max_retries: u32,
    /// Pinned peer `NodeId` from the original `<node_id>@addr` connect target,
    /// if any. Replayed via `hew_connmgr_expect_peer` on every reconnect
    /// attempt so the pin survives repeated drops, not just the first one.
    expected_node_id: Option<u16>,
}

#[derive(Clone, Debug)]
struct ReconnectPlan {
    target_addr: String,
    max_retries: u32,
    expected_node_id: Option<u16>,
}

/// Inbound message routing callback.
///
/// Parameters: `(target_actor_id, msg_type, data, size, request_id, source_node_id, conn_mgr)`.
/// `request_id` > 0 with `source_node_id` > 0 means this is an ask that expects
/// a reply. `request_id` == 0 is fire-and-forget.
/// `conn_mgr` is the manager that received the message; the callback must
/// use it to route replies so they go out on the correct connection regardless
/// of which node is stored in the process-global `CURRENT_NODE`.
type InboundRouter = unsafe extern "C" fn(u64, i32, *mut u8, usize, u64, u16, *mut HewConnMgr);

// SAFETY: HewConnMgr is only accessed through C ABI functions that
// serialize access via the internal Mutex. The transport pointer is
// valid for the lifetime of the manager (caller guarantees this).
unsafe impl Send for HewConnMgr {}
// SAFETY: Access to connections is serialized by the internal Mutex.
// The transport pointer is only read through function pointer calls.
unsafe impl Sync for HewConnMgr {}

// SAFETY: ConnectionActor contains a JoinHandle (Send but not Sync)
// and AtomicI32/AtomicU64 (both Sync). Access is serialized by the
// parent HewConnMgr's Mutex.
unsafe impl Send for ConnectionActor {}

impl std::fmt::Debug for ConnectionActor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ConnectionActor")
            .field("conn_id", &self.conn_id)
            .field("peer_node_id", &self.peer_node_id)
            .field("peer_feature_flags", &self.peer_feature_flags)
            .field("state", &self.state.load(Ordering::Relaxed))
            .field(
                "last_activity_ms",
                &self.last_activity_ms.load(Ordering::Relaxed),
            )
            .finish_non_exhaustive()
    }
}

impl ConnectionActor {
    fn new(conn_id: c_int) -> Self {
        Self {
            conn_id,
            publication_token: 0,
            publication_sync: Arc::new(Mutex::new(())),
            publication_removed: Arc::new(AtomicBool::new(false)),
            claimed_send_lifecycle: Arc::new(ReaderLifecycle::default()),
            peer_node_id: 0,
            peer_identity: None,
            peer_session_incarnation: 0,
            peer_feature_flags: 0,
            posture: Posture::Strict,
            credential: None,
            state: AtomicI32::new(CONN_STATE_CONNECTING),
            last_activity_ms: Arc::new(AtomicU64::new(0)),
            #[cfg(feature = "encryption")]
            noise_transport: Arc::new(Mutex::new(None)),
            reader_handle: None,
            reader_stop: Arc::new(AtomicI32::new(0)),
            superseded_claim: Mutex::new(None),
            reconnect: None,
            transport: std::ptr::null_mut(),
            transport_close: Arc::new(TransportClose::default()),
        }
    }

    /// Close this actor's transport connection if — and only if — this call
    /// wins the close claim.
    ///
    /// A caller that loses waits for the winner's close to land instead of
    /// closing a handle it does not own, so it is safe for it to go on and
    /// join the reader.
    ///
    /// # Safety
    ///
    /// `self.transport` must be valid (or null).
    unsafe fn close_transport(&self) {
        if self.transport_close.claim() {
            // SAFETY: caller guarantees transport pointer is valid or null.
            unsafe { close_transport_conn(self.transport, self.conn_id) };
            self.transport_close.finish();
        } else {
            self.transport_close.wait_closed();
        }
    }
}

impl Drop for ConnectionActor {
    fn drop(&mut self) {
        // Signal the reader to stop first so that when the transport close
        // unblocks a blocked recv(), the reader exits via the expected-stop
        // path rather than the unexpected-drop / reconnect path.
        self.reader_stop.store(1, Ordering::Release);
        // Defense-in-depth: close the transport before joining so a reader
        // blocked inside recv() unblocks rather than hanging indefinitely.
        // Closes only if this drop wins the close claim; otherwise it waits for
        // the owner's close to land, because the join below cannot complete
        // until some close has woken the reader.
        //
        // SAFETY: self.transport is valid for the connection lifetime (set in
        // hew_connmgr_add) or null for test-only actors created without a manager.
        unsafe { self.close_transport() };
        // Wait for reader thread (best-effort).
        if let Some(handle) = self.reader_handle.take() {
            if handle.thread().id() != thread::current().id() {
                crate::util::report_join_panic("connection reader thread", handle.join());
            }
        }
    }
}

struct ConnectionInstallPublication {
    token: u64,
    sync: Arc<Mutex<()>>,
    removed: Arc<AtomicBool>,
}

enum ConnectionInstallError {
    Shutdown,
    Duplicate,
}

// ── Send wrappers for raw pointers ─────────────────────────────────────

/// Wrapper to send a `*mut HewTransport` across threads.
///
/// # Safety
///
/// The transport must be valid for the entire duration it is used
/// by the reader thread.
struct SendTransport(*mut HewTransport);
// SAFETY: Transport implementations use Mutex or fd-based I/O,
// which are inherently thread-safe.
unsafe impl Send for SendTransport {}

/// Wrapper to send a `*mut HewConnMgr` across threads.
///
/// # Safety
///
/// The manager must remain valid for the lifetime of spawned reader threads.
struct SendConnMgr(*mut HewConnMgr);
// SAFETY: manager internals are synchronized and pointer validity is
// guaranteed by the manager lifecycle contract.
unsafe impl Send for SendConnMgr {}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Build a JSON array of active connections for the profiler HTTP API.
///
/// Each element: `{"conn_id":N,"peer_node_id":N,"state":"S","last_activity_ms":N}`
#[cfg(feature = "profiler")]
pub fn snapshot_connections_json(mgr: &HewConnMgr) -> String {
    use std::fmt::Write as _;

    mgr.connections.access(|connections| {
        crate::util::json_array(connections.iter(), |json, c| {
            let state_val = c.state.load(Ordering::Acquire);
            let state_str = match state_val {
                CONN_STATE_CONNECTING => "connecting",
                CONN_STATE_ACTIVE => "active",
                CONN_STATE_DRAINING => "draining",
                CONN_STATE_CLOSED => "closed",
                _ => "unknown",
            };
            let last_activity = c.last_activity_ms.load(Ordering::Acquire);
            let _ = write!(
                json,
                r#"{{"conn_id":{},"peer_node_id":{},"state":"{}","last_activity_ms":{}}}"#,
                c.conn_id, c.peer_node_id, state_str, last_activity,
            );
        })
    })
}

#[cfg(test)]
mod tests;
