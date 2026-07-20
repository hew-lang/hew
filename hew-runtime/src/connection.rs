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
use std::ffi::{c_char, c_int, CStr};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::cluster::HewCluster;
// Used by both plain and encryption-enabled envelope send paths.
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::envelope::{
    decode_link_down_payload, decode_link_req_payload, decode_monitor_down_payload,
    decode_monitor_req_payload, decode_registry_gossip_payload, decode_setup_result_payload,
    decode_swim_payload, decode_wire_frame, encode_control_frame, encode_registry_gossip_payload,
    encode_setup_result_payload, encode_swim_payload, ControlFrame, EnvelopeFrame,
    RegistryGossipPayload, SetupResultPayload, SwimControlPayload, SwimGossipEntry, WireFrame,
    CTRL_DEMONITOR, CTRL_LINK_DOWN, CTRL_LINK_REQ, CTRL_LINK_SETUP_RESULT, CTRL_MONITOR_DOWN,
    CTRL_MONITOR_REQ, CTRL_MONITOR_SETUP_RESULT, CTRL_REGISTRY_GOSSIP, CTRL_SWIM, CTRL_UNLINK,
    FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE, REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE,
    SETUP_STATUS_ACCEPTED, SETUP_STATUS_TARGET_GONE, WIRE_VERSION,
};
use crate::lifetime::poison_safe::PoisonSafe;
use crate::mailbox_envelope::{validate_cross_node_send_params, MailboxPayloadClass};
use crate::node_identity::{HewLocation, Location, NodeId};
use crate::peer_binding::{ClaimState, LiveClaim, PeerAuthSnapshot, PeerCredential, Posture};
use crate::routing::{hew_routing_add_route, hew_routing_remove_route_if_conn, HewRoutingTable};
use crate::set_last_error;
use crate::transport::{HewTransport, HEW_CONN_INVALID};
use crate::util::{CondvarExt, MutexExt};

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
#[cfg(feature = "encryption")]
use zeroize::Zeroizing;

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
    /// Guards against double-close when callers close the transport explicitly
    /// before the actor is dropped.
    transport_closed: AtomicBool,
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
            transport_closed: AtomicBool::new(false),
        }
    }

    /// Close this actor's transport connection, guarding against double-close.
    ///
    /// Sets `transport_closed` so subsequent calls (including from `Drop`) are
    /// no-ops. Safe to call when `self.transport` is null (test-only actors).
    ///
    /// # Safety
    ///
    /// `self.transport` must be valid (or null).
    unsafe fn close_transport(&self) {
        if !self.transport_closed.swap(true, Ordering::AcqRel) {
            // SAFETY: caller guarantees transport pointer is valid or null.
            unsafe { close_transport_conn(self.transport, self.conn_id) };
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
        // Guards against double-close via transport_closed; null-safe.
        //
        // SAFETY: self.transport is valid for the connection lifetime (set in
        // hew_connmgr_add) or null for test-only actors created without a manager.
        unsafe { self.close_transport() };
        // Wait for reader thread (best-effort).
        if let Some(handle) = self.reader_handle.take() {
            if handle.thread().id() != thread::current().id() {
                let _ = handle.join();
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

fn normalize_max_retries(max_retries: c_int) -> u32 {
    if max_retries <= 0 {
        RECONNECT_DEFAULT_MAX_RETRIES
    } else {
        u32::try_from(max_retries).unwrap_or(RECONNECT_DEFAULT_MAX_RETRIES)
    }
}

fn jittered_backoff_ms(base_ms: u64) -> u64 {
    let mut rng = rng();
    let jitter_pct = rng.random_range(RECONNECT_JITTER_MIN_PERCENT..=RECONNECT_JITTER_MAX_PERCENT);
    let jittered = base_ms.saturating_mul(jitter_pct) / 100;
    jittered.max(1)
}

fn sleep_until_retry(shutdown: &AtomicBool, delay_ms: u64) -> bool {
    let mut remaining = delay_ms;
    while remaining > 0 {
        if shutdown.load(Ordering::Acquire) {
            return false;
        }
        let slice = remaining.min(RECONNECT_SLEEP_SLICE_MS);
        thread::sleep(Duration::from_millis(slice));
        remaining -= slice;
    }
    !shutdown.load(Ordering::Acquire)
}

fn collect_finished_reconnect_workers(mgr: &HewConnMgr) {
    mgr.reconnect_workers.access(|workers| {
        let mut idx = 0usize;
        while idx < workers.len() {
            if workers[idx].is_finished() {
                let handle = workers.swap_remove(idx);
                let _ = handle.join();
            } else {
                idx += 1;
            }
        }
    });
}

fn next_publication_token(mgr: &HewConnMgr) -> u64 {
    mgr.next_publication_token.fetch_add(1, Ordering::Relaxed)
}

/// Bounded wait for an in-flight `Reserved` claim to resolve, in ms. Mirrors the
/// handshake timeout ceiling: a stuck reservation must not wedge admission.
const CLAIM_RESERVE_WAIT_MS: u64 = 5_000;

/// Outcome of reserving a `NodeId` claim during admission (issue #2652, D3).
enum ClaimReservation {
    /// The reservation succeeded. Carries the superseded same-credential
    /// `Published` claim (if any) to demote after our publish.
    Reserved { superseded: Option<LiveClaim> },
    /// The reservation was rejected fail-closed (a live different-credential
    /// owner, or an in-flight reservation that did not resolve in time). Carries
    /// the diagnostic detail.
    Rejected(String),
}

/// Reserve a `NodeId` claim for a connection mid-admission (issue #2652, D3
/// step 1). Runs under the claims mutex; the condvar coordinates the
/// reserve/publish handoff.
///
/// On an existing entry for `node_id`:
/// - `Published` with a **different** credential ⇒ reject fail-closed (a live
///   authenticated owner is never taken over by a different credential).
/// - `Published` with the **same** credential ⇒ same-peer reconnect: supersede.
/// - `Reserved` (another admission in flight) ⇒ wait on the condvar until it
///   publishes/aborts, then re-evaluate; on timeout ⇒ reject.
/// - absent ⇒ insert `Reserved`; no supersede.
fn reserve_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    credential: Option<&PeerCredential>,
    conn_id: c_int,
    publication_token: u64,
) -> ClaimReservation {
    if route_slot == 0 || session_incarnation == 0 {
        return ClaimReservation::Rejected(format!(
            "invalid route slot/session for NodeId {node_id} (route_slot={route_slot}, session={session_incarnation})"
        ));
    }
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    loop {
        match map.get(&node_id) {
            Some(existing) if existing.state != ClaimState::Reserved => {
                if existing.credential.as_ref() == credential && existing.route_slot == route_slot {
                    if session_incarnation < existing.session_incarnation {
                        return ClaimReservation::Rejected(format!(
                            "lower-session replay for NodeId {node_id}: received {session_incarnation}, current {}",
                            existing.session_incarnation
                        ));
                    }
                    if session_incarnation == existing.session_incarnation
                        && !mgr.cluster.is_null()
                        && matches!(
                            // SAFETY: cluster is owned by the live manager.
                            unsafe { (&*mgr.cluster).member_state(route_slot) },
                            crate::cluster::MEMBER_DEAD | crate::cluster::MEMBER_LEFT
                        )
                    {
                        return ClaimReservation::Rejected(format!(
                            "equal session {session_incarnation} cannot revive buried NodeId {node_id}"
                        ));
                    }
                    // Same-credential reconnect or a higher durable session.
                    let superseded = Some(existing.clone());
                    map.insert(
                        node_id,
                        LiveClaim {
                            credential: credential.cloned(),
                            route_slot,
                            session_incarnation,
                            conn_id,
                            publication_token,
                            state: ClaimState::Reserved,
                        },
                    );
                    return ClaimReservation::Reserved { superseded };
                }
                // A NodeId is key-derived, so another credential or route slot is
                // either a collision or a wrong local pin.
                return ClaimReservation::Rejected(format!(
                    "NodeId {node_id} is already bound to another credential or route slot (conn {conn_id})"
                ));
            }
            Some(_reserved) => {
                // Another admission is mid-flight for this NodeId. Wait for it to
                // publish or abort, then re-evaluate. Bounded to avoid wedging.
                let (guard, timeout) = condvar.wait_timeout_or_recover(
                    map,
                    std::time::Duration::from_millis(CLAIM_RESERVE_WAIT_MS),
                );
                map = guard;
                if timeout.timed_out() {
                    return ClaimReservation::Rejected(format!(
                        "timed out waiting on in-flight reservation for node id {node_id} (conn {conn_id})"
                    ));
                }
                // Loop and re-evaluate the (possibly changed) entry.
            }
            None => {
                map.insert(
                    node_id,
                    LiveClaim {
                        credential: credential.cloned(),
                        route_slot,
                        session_incarnation,
                        conn_id,
                        publication_token,
                        state: ClaimState::Reserved,
                    },
                );
                return ClaimReservation::Reserved { superseded: None };
            }
        }
    }
}

/// Abort a reservation on install failure (issue #2652, D3 step 2). If
/// `claims[node_id]` is still our exact `Reserved` claim, restore the superseded
/// claim (if any) or remove it, then wake any waiter.
fn abort_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
    superseded: Option<LiveClaim>,
) {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    if let Some(current) = map.get(&node_id) {
        if current.state == ClaimState::Reserved
            && current.conn_id == conn_id
            && current.publication_token == publication_token
        {
            match superseded {
                Some(prev) => {
                    map.insert(node_id, prev);
                }
                None => {
                    map.remove(&node_id);
                }
            }
        }
    }
    drop(map);
    condvar.notify_all();
}

/// Transition our reservation `Reserved → Published` (issue #2652, D3 step 3).
/// Returns `true` iff `claims[node_id]` is still our exact reservation (so the
/// caller may write route + cluster token under the same lock). A superseding
/// claim arriving meanwhile ⇒ `false` (abort publish, write nothing).
fn publish_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let still_ours = map.get(&node_id).is_some_and(|c| {
        c.state == ClaimState::Reserved
            && c.conn_id == conn_id
            && c.publication_token == publication_token
    });
    if still_ours {
        if let Some(claim) = map.get_mut(&node_id) {
            claim.state = ClaimState::Published;
        }
    }
    drop(map);
    condvar.notify_all();
    still_ours
}

/// Retire our published claim (issue #2652, D3 retire). Removes `claims[node_id]`
/// iff it exactly matches this connection's `(conn_id, publication_token)`,
/// then wakes any waiter. Returns `true` iff this connection was the exact owner
/// (drives the real route removal / `MonitorLost` fan-out). A non-matching /
/// superseded connection removes nothing and reports "not owner".
fn retire_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let (lock, condvar) = &mgr.claims;
    let mut map = lock.lock_or_recover();
    let is_owner = map
        .get(&node_id)
        .is_some_and(|c| c.conn_id == conn_id && c.publication_token == publication_token);
    if is_owner {
        if let Some(claim) = map.get_mut(&node_id) {
            claim.state = ClaimState::Retired;
        }
    }
    drop(map);
    condvar.notify_all();
    is_owner
}

/// Test-only: reserve an `Unverified` (credential-free) claim, mirroring what
/// admission does before `publish_connection_established`. Returns the
/// superseded claim (if any) to thread into publish. Panics on rejection — the
/// unit tests below only reserve fresh or same-`NodeId` reconnect claims.
#[cfg(test)]
fn reserve_unverified_identity_claim(
    mgr: &HewConnMgr,
    node_id: NodeId,
    route_slot: u16,
    session_incarnation: u32,
    conn_id: c_int,
    token: u64,
) -> Option<LiveClaim> {
    match reserve_identity_claim(
        mgr,
        node_id,
        route_slot,
        session_incarnation,
        None,
        conn_id,
        token,
    ) {
        ClaimReservation::Reserved { superseded } => superseded,
        ClaimReservation::Rejected(detail) => {
            panic!("unexpected claim rejection in test (node {node_id}, conn {conn_id}): {detail}")
        }
    }
}

#[cfg(test)]
fn test_node_identity(route_slot: u16) -> NodeId {
    let mut bytes = [0_u8; 16];
    bytes[14..].copy_from_slice(&route_slot.to_be_bytes());
    NodeId::from_bytes(bytes)
}

#[cfg(test)]
fn reserve_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    credential: Option<&PeerCredential>,
    conn_id: c_int,
    publication_token: u64,
) -> ClaimReservation {
    reserve_identity_claim(
        mgr,
        test_node_identity(route_slot),
        route_slot,
        1,
        credential,
        conn_id,
        publication_token,
    )
}

#[cfg(test)]
fn abort_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    superseded: Option<LiveClaim>,
) {
    abort_identity_claim(
        mgr,
        test_node_identity(route_slot),
        conn_id,
        publication_token,
        superseded,
    );
}

#[cfg(test)]
fn publish_claim(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) -> bool {
    let identity = test_node_identity(route_slot);
    mgr.connections.access(|connections| {
        if let Some(connection) = connections
            .iter_mut()
            .find(|connection| connection.conn_id == conn_id)
        {
            connection.peer_identity = Some(identity);
            connection.peer_session_incarnation = 1;
            connection.publication_token = publication_token;
        }
    });
    publish_identity_claim(mgr, identity, conn_id, publication_token)
}

#[cfg(test)]
fn retire_claim(mgr: &HewConnMgr, route_slot: u16, conn_id: c_int, publication_token: u64) -> bool {
    retire_identity_claim(
        mgr,
        test_node_identity(route_slot),
        conn_id,
        publication_token,
    )
}

#[cfg(test)]
fn test_reserve_unverified(
    mgr: &HewConnMgr,
    route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
) -> Option<LiveClaim> {
    reserve_unverified_identity_claim(
        mgr,
        test_node_identity(route_slot),
        route_slot,
        1,
        conn_id,
        publication_token,
    )
}

/// Test helper: reserve **and** publish a claim binding `node_id` to `conn_id`,
/// modelling a fully-admitted authenticated peer. Unit tests that construct a
/// [`ConnectionActor`] by hand (bypassing `hew_connmgr_add`) must seed this so
/// the issue #2652 exact-owner gate ([`authenticated_peer_node_id_for_conn`])
/// recognises the connection as the current published owner of its `NodeId`.
#[cfg(test)]
fn test_publish_claim(mgr: &HewConnMgr, route_slot: u16, conn_id: c_int) -> u64 {
    let node_id = test_node_identity(route_slot);
    let token = next_publication_token(mgr);
    reserve_unverified_identity_claim(mgr, node_id, route_slot, 1, conn_id, token);
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if let Some(claim) = guard.get_mut(&node_id) {
        claim.state = ClaimState::Published;
    }
    drop(guard);
    condvar.notify_all();
    // Stamp the connection entry with the same publication token so the
    // generation-bound waiting gate recognises the hand-built actor as this
    // admission (production sets `actor.publication_token` before install).
    mgr.connections.access(|conns| {
        if let Some(conn) = conns.iter_mut().find(|c| c.conn_id == conn_id) {
            conn.peer_identity = Some(node_id);
            conn.peer_session_incarnation = 1;
            conn.publication_token = token;
        }
    });
    token
}

#[expect(
    clippy::too_many_arguments,
    reason = "publication threads route/cluster/gossip metadata plus the issue #2652 superseded claim; bundling them would obscure the call site"
)]
fn publish_identity_connection_established(
    mgr: &HewConnMgr,
    peer_identity: NodeId,
    peer_route_slot: u16,
    peer_session_incarnation: u32,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
    superseded: Option<LiveClaim>,
) {
    if publication_removed.load(Ordering::Acquire) {
        return;
    }

    // Transition our reservation Reserved → Published (issue #2652, D3 step 3).
    // A superseding admission arriving in the reserve window ⇒ we are no longer
    // the map owner ⇒ abort publish (write no route / cluster token / gossip).
    if !publish_identity_claim(mgr, peer_identity, conn_id, publication_token) {
        return;
    }

    if let Some(previous) = superseded.as_ref().filter(|previous| {
        previous.state == ClaimState::Published
            && previous.session_incarnation != peer_session_incarnation
    }) {
        crate::hew_node::fan_out_monitor_lost_for_session(
            peer_identity,
            previous.session_incarnation,
        );
    }

    let published = if mgr.cluster.is_null() {
        true
    } else {
        // SAFETY: cluster is owned by the live manager.
        // SAFETY: pointer validity is checked by the callee.
        let result = unsafe {
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                mgr.cluster,
                peer_route_slot,
                peer_session_incarnation,
                publication_token,
                publication_sync,
                publication_removed,
            )
        };
        if result < 0 {
            let _ = retire_identity_claim(mgr, peer_identity, conn_id, publication_token);
            return;
        }
        result == 1
    };
    if !published {
        return;
    }

    if !mgr.routing_table.is_null() {
        let _publication = publication_sync.lock_or_recover();
        if publication_removed.load(Ordering::Acquire) {
            return;
        }
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe {
            hew_routing_add_route(
                mgr.routing_table,
                peer_identity,
                peer_route_slot,
                peer_session_incarnation,
                conn_id,
            )
        };
    }

    flush_registry_gossip_to_connection(mgr, conn_id, publication_token, peer_feature_flags);

    // Publication is COMPLETE: clear the actor's stashed superseded claim
    // before closing the superseded connection below, so a later remove of
    // THIS connection can never restore a claim for a connection whose
    // transport is closed here. The stash is restorable state only while the
    // publication is still pending or suppressed.
    mgr.connections.access(|conns| {
        if let Some(conn) = conns.iter().find(|c| c.conn_id == conn_id) {
            conn.superseded_claim.lock_or_recover().take();
        }
    });

    // Same-credential reconnect (issue #2652, D3 step 3): our claim overwrote a
    // live Published claim from the same peer credential. The superseded
    // connection already lost its authority at the map overwrite (D9 owner
    // check); close its transport so it can no longer emit SWIM/gossip/control
    // frames. Its actor tears down via the normal reader-exit path.
    if let Some(superseded) = superseded {
        if superseded.state == ClaimState::Published && superseded.conn_id != conn_id {
            // SAFETY: mgr.transport is valid while the manager is alive.
            unsafe { close_transport_conn(mgr.transport, superseded.conn_id) };
        }
    }
}

#[cfg(test)]
#[expect(
    clippy::too_many_arguments,
    reason = "legacy test adapter mirrors the pre-v2 publication surface"
)]
fn publish_connection_established(
    mgr: &HewConnMgr,
    peer_route_slot: u16,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
    superseded: Option<LiveClaim>,
) {
    let identity = test_node_identity(peer_route_slot);
    mgr.connections.access(|connections| {
        if let Some(connection) = connections
            .iter_mut()
            .find(|connection| connection.conn_id == conn_id)
        {
            connection.peer_identity = Some(identity);
            connection.peer_session_incarnation = 1;
            connection.publication_token = publication_token;
        }
    });
    publish_identity_connection_established(
        mgr,
        identity,
        peer_route_slot,
        1,
        conn_id,
        peer_feature_flags,
        publication_token,
        publication_sync,
        publication_removed,
        superseded,
    );
}

fn retire_identity_connection_publication(
    mgr: &HewConnMgr,
    peer_identity: Option<NodeId>,
    peer_route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
) {
    let Some(peer_identity) = peer_identity else {
        return;
    };

    // Retire our claim iff we are still the exact owner (issue #2652, D3 retire).
    // A superseded / non-matching connection removes nothing and must NOT drive
    // route removal, a cluster-lost notification, or a MonitorLost fan-out — the
    // peer is still live under a newer claim.
    let is_owner = retire_identity_claim(mgr, peer_identity, conn_id, publication_token);
    if !is_owner {
        return;
    }

    {
        let _publication = publication_sync.lock_or_recover();
        if !mgr.routing_table.is_null() {
            // SAFETY: pointer validity is checked by the callee.
            let _ = unsafe {
                hew_routing_remove_route_if_conn(mgr.routing_table, peer_identity, conn_id)
            };
        }
    }
    if !mgr.cluster.is_null() {
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe {
            crate::cluster::hew_cluster_notify_connection_lost_if_current(
                mgr.cluster,
                peer_route_slot,
                publication_token,
            )
        };
    }

    // Cross-node monitor connection-drop fan-out: the connection to
    // `peer_node_id` is gone, so every local watcher of an actor on that node
    // gets a MonitorLost DOWN — unless it already received a definitive
    // clean-exit / crash DOWN (only Pending slots are armed; exactly-once).
    crate::hew_node::fan_out_monitor_lost_for_identity(peer_identity);
}

#[cfg(test)]
fn retire_connection_publication(
    mgr: &HewConnMgr,
    peer_route_slot: u16,
    conn_id: c_int,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
) {
    retire_identity_connection_publication(
        mgr,
        Some(test_node_identity(peer_route_slot)),
        peer_route_slot,
        conn_id,
        publication_token,
        publication_sync,
    );
}

fn reconnect_plan(mgr: &HewConnMgr, conn_id: c_int) -> Option<ReconnectPlan> {
    if !mgr.reconnect_enabled.load(Ordering::Acquire)
        || mgr.reconnect_shutdown.load(Ordering::Acquire)
    {
        return None;
    }
    mgr.connections.access(|conns| {
        let conn = conns.iter().find(|c| c.conn_id == conn_id)?;
        let reconnect = conn.reconnect.as_ref()?;
        Some(ReconnectPlan {
            target_addr: reconnect.target_addr.clone(),
            max_retries: reconnect.max_retries.max(1),
            expected_node_id: reconnect.expected_node_id,
        })
    })
}

unsafe fn connect_addr(mgr: *mut HewConnMgr, target_addr: &CStr) -> Result<c_int, String> {
    if mgr.is_null() {
        return Err("manager is null".to_owned());
    }
    // SAFETY: caller guarantees `mgr` remains valid for this call.
    let mgr = unsafe { &*mgr };
    if mgr.transport.is_null() {
        return Err("transport is null".to_owned());
    }
    // SAFETY: transport pointer is valid per manager contract.
    let t = unsafe { &*mgr.transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return Err("transport ops are null".to_owned());
    };
    let Some(connect_fn) = ops.connect else {
        return Err("transport connect op missing".to_owned());
    };
    // SAFETY: transport impl and C string are valid.
    let conn_id = unsafe { connect_fn(t.r#impl, target_addr.as_ptr()) };
    if conn_id == HEW_CONN_INVALID {
        return Err("transport connect failed".to_owned());
    }
    Ok(conn_id)
}

fn spawn_reconnect_worker(mgr: *mut HewConnMgr, conn_id: c_int, plan: ReconnectPlan) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid when scheduling workers.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.reconnect_shutdown.load(Ordering::Acquire) {
        return;
    }
    collect_finished_reconnect_workers(mgr_ref);
    let mgr_send = SendConnMgr(mgr);
    let shutdown = Arc::clone(&mgr_ref.reconnect_shutdown);
    let thread_name = format!("hew-reconnect-{conn_id}");
    let handle = thread::Builder::new().name(thread_name).spawn(move || {
        reconnect_worker_loop(mgr_send, shutdown, conn_id, plan);
    });
    match handle {
        Ok(worker) => {
            mgr_ref
                .reconnect_workers
                .access(|workers| workers.push(worker));
        }
        Err(_) => {
            set_last_error(format!(
                "hew_connmgr_reconnect: failed to spawn worker for dropped conn {conn_id}"
            ));
        }
    }
}

/// Outcome of a single [`reconnect_attempt`].
#[derive(Debug, PartialEq, Eq)]
enum ReconnectAttemptOutcome {
    /// The reconnected connection was installed and reconnect was re-armed.
    Installed,
    /// The attempt reached `hew_connmgr_expect_peer`/`hew_connmgr_add` and
    /// was rejected there (e.g. a pinned-identity mismatch). The specific
    /// reason is already in `hew_last_error` — set by whichever of those two
    /// calls rejected the attempt.
    Rejected,
    /// The attempt failed before reaching the identity gate (invalid
    /// address, transport connect failure).
    TransportError,
}

/// Runs one reconnect attempt: connect, replay the pinned peer identity
/// (`plan.expected_node_id`) if any, install via `hew_connmgr_add`, and
/// re-arm reconnect on success.
///
/// Extracted from the retry loop so both the worker and tests can drive the
/// exact install sequence deterministically and same-thread. Bare-address
/// reconnects (`plan.expected_node_id.is_none()`) skip straight to
/// `hew_connmgr_add`, unchanged from before this pin was threaded through.
fn reconnect_attempt(
    mgr_ptr: *mut HewConnMgr,
    plan: &ReconnectPlan,
    dropped_conn_id: c_int,
    attempt: u32,
) -> ReconnectAttemptOutcome {
    let Ok(target_addr) = std::ffi::CString::new(plan.target_addr.as_str()) else {
        set_last_error(format!(
            "hew_connmgr_reconnect: invalid reconnect address for dropped conn {dropped_conn_id}"
        ));
        return ReconnectAttemptOutcome::TransportError;
    };
    // SAFETY: mgr_ptr was checked non-null and remains valid for the connection lifetime.
    let new_conn_id = match unsafe { connect_addr(mgr_ptr, &target_addr) } {
        Ok(new_conn_id) => new_conn_id,
        Err(err) => {
            set_last_error(format!(
                "hew_connmgr_reconnect: attempt {attempt}/{} failed for dropped conn {dropped_conn_id}, addr={}: {err}",
                plan.max_retries, plan.target_addr
            ));
            return ReconnectAttemptOutcome::TransportError;
        }
    };

    if let Some(expected) = plan.expected_node_id {
        // SAFETY: mgr_ptr is valid (connect_addr above already dereferenced
        // it), and new_conn_id was just returned by this manager's transport
        // and is not yet tracked by the manager.
        if unsafe { hew_connmgr_expect_peer(mgr_ptr, new_conn_id, expected) } != 0 {
            // hew_connmgr_expect_peer does not take ownership on failure, and
            // hew_connmgr_add is never reached on this path, so this call
            // owns closing the transport connection.
            // SAFETY: mgr_ptr is valid; new_conn_id is not yet installed, so
            // no reader thread is racing this close.
            unsafe { close_transport_conn((&*mgr_ptr).transport, new_conn_id) };
            return ReconnectAttemptOutcome::Rejected;
        }
    }

    // SAFETY: manager pointer is valid until shutdown and join in free.
    if unsafe { hew_connmgr_add(mgr_ptr, new_conn_id) } != 0 {
        // hew_connmgr_add owns conn_id cleanup on failure (closes the
        // transport connection on all failure paths, including a
        // pinned-identity mismatch) and has already recorded the specific
        // rejection reason via set_last_error.
        return ReconnectAttemptOutcome::Rejected;
    }

    let retries = i32::try_from(plan.max_retries).unwrap_or(i32::MAX);
    // SAFETY: manager and conn_id are valid after successful add.
    let _ = unsafe {
        hew_connmgr_configure_reconnect(
            mgr_ptr,
            new_conn_id,
            target_addr.as_ptr(),
            1,
            retries,
            plan.expected_node_id.map_or(0, i32::from),
        )
    };
    ReconnectAttemptOutcome::Installed
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "FFI callback signature requires owned values"
)]
fn reconnect_worker_loop(
    mgr: SendConnMgr,
    shutdown: Arc<AtomicBool>,
    dropped_conn_id: c_int,
    plan: ReconnectPlan,
) {
    let mgr_ptr = mgr.0;
    let mut base_backoff_ms = RECONNECT_INITIAL_BACKOFF_MS;

    for attempt in 1..=plan.max_retries {
        if shutdown.load(Ordering::Acquire) {
            return;
        }
        let delay_ms = jittered_backoff_ms(base_backoff_ms);
        if !sleep_until_retry(&shutdown, delay_ms) {
            return;
        }
        if shutdown.load(Ordering::Acquire) {
            return;
        }

        if reconnect_attempt(mgr_ptr, &plan, dropped_conn_id, attempt)
            == ReconnectAttemptOutcome::Installed
        {
            return;
        }

        base_backoff_ms = base_backoff_ms
            .saturating_mul(2)
            .min(RECONNECT_MAX_BACKOFF_MS);
    }

    set_last_error(format!(
        "hew_connmgr_reconnect: giving up after {} attempts for dropped conn {dropped_conn_id}, addr={}",
        plan.max_retries, plan.target_addr
    ));
}

fn local_feature_flags() -> u32 {
    #[cfg_attr(
        not(feature = "encryption"),
        allow(
            unused_mut,
            reason = "mut is only exercised by the encryption-gated flags |= below"
        )
    )]
    let mut flags = HEW_FEATURE_SUPPORTS_GOSSIP | HEW_FEATURE_SUPPORTS_ASK_REJECTION;
    #[cfg(feature = "encryption")]
    {
        flags |= HEW_FEATURE_SUPPORTS_ENCRYPTION;
    }
    flags
}

pub(crate) fn supports_ask_rejection(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ASK_REJECTION != 0
}

pub(crate) fn supports_gossip(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_GOSSIP != 0
}

fn is_ask_rejection_reply(msg_type: i32, peer_feature_flags: u32) -> bool {
    msg_type == crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE
        && supports_ask_rejection(peer_feature_flags)
}

fn local_schema_hash() -> u32 {
    fn fnv1a32_update(mut hash: u32, bytes: &[u8]) -> u32 {
        for &byte in bytes {
            hash ^= u32::from(byte);
            hash = hash.wrapping_mul(FNV1A32_PRIME);
        }
        hash
    }

    let mut hash = FNV1A32_OFFSET_BASIS;
    hash = fnv1a32_update(hash, b"CBOR-ENVELOPE");
    hash = fnv1a32_update(hash, &[WIRE_VERSION]);
    hash = fnv1a32_update(hash, &[FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE]);
    fnv1a32_update(hash, &[1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
}

fn local_handshake(
    local_node_id: NodeId,
    session_incarnation: u32,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
) -> HewHandshake {
    HewHandshake {
        protocol_version: HEW_PROTOCOL_VERSION,
        schema_hash: local_schema_hash(),
        feature_flags: local_feature_flags(),
        node_id: local_node_id,
        session_incarnation,
        static_noise_pubkey,
    }
}

fn version_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.protocol_version == peer.protocol_version
}

fn schema_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.schema_hash == peer.schema_hash
}

fn peer_identity_compatible(local_node_id: NodeId, peer_node_id: NodeId) -> bool {
    peer_node_id != local_node_id
}

unsafe fn send_frame(transport: *mut HewTransport, conn_id: c_int, payload: &[u8]) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return false;
    };
    let Some(send_fn) = ops.send else {
        return false;
    };
    let Ok(expected) = c_int::try_from(payload.len()) else {
        return false;
    };
    // SAFETY: payload pointer is valid for payload.len() bytes.
    unsafe { send_fn(t.r#impl, conn_id, payload.as_ptr().cast(), payload.len()) == expected }
}

unsafe fn recv_frame_exact(
    transport: *mut HewTransport,
    conn_id: c_int,
    payload: &mut [u8],
) -> bool {
    if transport.is_null() {
        return false;
    }
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let Some(ops) = (unsafe { t.ops.as_ref() }) else {
        return false;
    };
    let Some(recv_fn) = ops.recv else {
        return false;
    };
    let Ok(expected) = c_int::try_from(payload.len()) else {
        return false;
    };
    // SAFETY: payload pointer is valid for payload.len() writable bytes.
    unsafe {
        recv_fn(
            t.r#impl,
            conn_id,
            payload.as_mut_ptr().cast(),
            payload.len(),
        ) == expected
    }
}

unsafe fn handshake_send(
    transport: *mut HewTransport,
    conn_id: c_int,
    handshake: HewHandshake,
) -> c_int {
    // SAFETY: transport and conn_id are validated by caller; serialize returns a fixed-size buffer.
    -c_int::from(!unsafe { send_frame(transport, conn_id, &handshake.serialize()) })
}

unsafe fn handshake_recv(transport: *mut HewTransport, conn_id: c_int) -> Option<HewHandshake> {
    let mut buf = [0u8; HEW_HANDSHAKE_SIZE];
    // SAFETY: transport and conn_id are validated by caller; buf is stack-allocated with correct size.
    if !unsafe { recv_frame_exact(transport, conn_id, &mut buf) } {
        set_last_error(format!(
            "hew_connmgr_add: failed to receive handshake for conn {conn_id}"
        ));
        return None;
    }
    let Some(handshake) = HewHandshake::deserialize(&buf) else {
        set_last_error(format!(
            "hew_connmgr_add: invalid handshake payload for conn {conn_id}"
        ));
        return None;
    };
    Some(handshake)
}

unsafe fn handshake_exchange(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: HewHandshake,
) -> Option<HewHandshake> {
    // SAFETY: transport and conn_id validated by caller contract.
    if unsafe { handshake_send(transport, conn_id, local) } != 0 {
        set_last_error(format!(
            "hew_connmgr_add: failed to send handshake for conn {conn_id}"
        ));
        return None;
    }
    // SAFETY: same contract — transport remains valid through handshake sequence.
    let peer = unsafe { handshake_recv(transport, conn_id) }?;
    if !version_compatible(&local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake protocol mismatch for conn {conn_id} (local={}, peer={})",
            local.protocol_version, peer.protocol_version
        ));
        return None;
    }
    if !schema_compatible(&local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake schema hash mismatch for conn {conn_id} (local={:#010x}, peer={:#010x})",
            local.schema_hash, peer.schema_hash
        ));
        return None;
    }
    Some(peer)
}

unsafe fn close_transport_conn(transport: *mut HewTransport, conn_id: c_int) {
    if transport.is_null() {
        return;
    }
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(close_fn) = ops.close_conn {
            // SAFETY: conn_id is a transport-provided handle.
            unsafe { close_fn(t.r#impl, conn_id) };
        }
    }
}

fn install_connection_actor(
    mgr: &HewConnMgr,
    actor: ConnectionActor,
) -> Result<ConnectionInstallPublication, ConnectionInstallError> {
    let conn_id = actor.conn_id;
    let mut actor = Some(actor);
    let install = mgr.connections.access(|conns| {
        if mgr.reconnect_shutdown.load(Ordering::Acquire) {
            Err(ConnectionInstallError::Shutdown)
        } else if conns.iter().any(|c| c.conn_id == conn_id) {
            Err(ConnectionInstallError::Duplicate)
        } else {
            let actor_ref = actor
                .as_ref()
                .expect("actor should remain available until install succeeds");
            let publication = ConnectionInstallPublication {
                token: actor_ref.publication_token,
                sync: Arc::clone(&actor_ref.publication_sync),
                removed: Arc::clone(&actor_ref.publication_removed),
            };
            conns.push(
                actor
                    .take()
                    .expect("actor should be consumed exactly once during install"),
            );
            Ok(publication)
        }
    });
    if install.is_err() {
        // Mark the actor's transport as closed so Drop does not double-close
        // after the explicit close_transport_conn below.
        if let Some(ref a) = actor {
            a.transport_closed.store(true, Ordering::Release);
        }
        // SAFETY: mgr.transport is valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
    }
    install
}

// Used by both plain and encryption-enabled send paths in `hew_connmgr_send`.
unsafe fn encode_envelope(
    target: Location,
    msg_type: i32,
    payload: *mut u8,
    payload_len: usize,
    payload_class: u8,
    cancel_token_handle: u64,
) -> Option<Vec<u8>> {
    // Cross-node send gates (Gate 1 before Gate 2; fail-closed in all build
    // profiles via set_last_error + return None, not debug_assert).
    validate_cross_node_send_params(payload_class, cancel_token_handle)?;
    // SAFETY: caller guarantees `payload` is valid for `payload_len` bytes.
    match unsafe {
        encode_envelope_frame_from_raw_parts(
            Some(target),
            None,
            msg_type,
            payload.cast_const(),
            payload_len,
            0,
        )
    } {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!("hew_connmgr_send: {err}"));
            None
        }
    }
}

fn encode_registry_gossip_control(name: &str, location: Location, is_add: bool) -> Option<Vec<u8>> {
    let op = if is_add {
        crate::cluster::GOSSIP_REGISTRY_ADD
    } else {
        crate::cluster::GOSSIP_REGISTRY_REMOVE
    };
    let payload = RegistryGossipPayload {
        op,
        name: name.to_owned(),
        location,
    };
    let payload = match encode_registry_gossip_payload(&payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "registry gossip control payload encode failure: {err}"
            ));
            return None;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_REGISTRY_GOSSIP,
        payload,
    };
    match encode_control_frame(&frame) {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!(
                "registry gossip control frame encode failure: {err}"
            ));
            None
        }
    }
}

fn handle_control_frame(
    mgr: *mut HewConnMgr,
    peer_feature_flags: u32,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    match control.ctrl_kind {
        CTRL_REGISTRY_GOSSIP => {}
        CTRL_SWIM => {
            handle_swim_control_frame(mgr, peer_feature_flags, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_REQ => {
            handle_monitor_req_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_DEMONITOR => {
            handle_demonitor_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_DOWN => {
            handle_monitor_down_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_MONITOR_SETUP_RESULT => {
            handle_setup_result_frame(
                mgr,
                conn_id,
                claim_token,
                control,
                crate::hew_node::RemoteSetupKind::Monitor,
            );
            return;
        }
        CTRL_LINK_REQ => {
            handle_link_req_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_UNLINK => {
            handle_unlink_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_LINK_DOWN => {
            handle_link_down_frame(mgr, conn_id, claim_token, control);
            return;
        }
        CTRL_LINK_SETUP_RESULT => {
            handle_setup_result_frame(
                mgr,
                conn_id,
                claim_token,
                control,
                crate::hew_node::RemoteSetupKind::Link,
            );
            return;
        }
        other => {
            set_last_error(format!(
                "connection reader unknown control frame kind {other}"
            ));
            return;
        }
    }
    if !supports_gossip(peer_feature_flags) {
        set_last_error("connection reader rejected registry gossip from non-gossip peer");
        return;
    }
    // D9/D12: only an authenticated (`Strict`) peer may inject registry gossip.
    // An `Unverified` (delivery-only) connection carries no control-plane
    // authority — drop its gossip with a diagnostic and apply no registry
    // mutation (fail-closed).
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "registry gossip")
    else {
        return;
    };

    let payload = match decode_registry_gossip_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader registry gossip payload decode failure: {err}"
            ));
            return;
        }
    };
    let is_add = match payload.op {
        REGISTRY_GOSSIP_OP_ADD => true,
        REGISTRY_GOSSIP_OP_REMOVE => false,
        op => {
            set_last_error(format!("connection reader registry gossip unknown op {op}"));
            return;
        }
    };
    if mgr.is_null() {
        set_last_error("connection reader registry gossip missing manager");
        return;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        set_last_error("connection reader registry gossip missing cluster");
        return;
    }
    if !location_matches_node_session(payload.location, peer_identity) {
        set_last_error("registry gossip rejected a Location not owned by the authenticated peer");
        return;
    }
    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running.
    unsafe {
        (&*mgr_ref.cluster).apply_registry_event(&payload.name, payload.location, is_add);
    }
}

fn authenticated_peer_node_id(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    context: &str,
) -> Option<u16> {
    if mgr.is_null() {
        set_last_error(format!("connection reader {context}: missing manager"));
        return None;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    // Waiting variant: an inbound control frame can race this connection's own
    // Reserved → Published publication (the reader thread starts before
    // `publish_connection_established`); one-shot frames like the peer's
    // registry-gossip flush must not be dropped inside that window.
    let authenticated = wait_authenticated_peer_node_id_for_conn(mgr_ref, conn_id, claim_token);
    if authenticated == 0 {
        set_last_error(format!(
            "connection reader {context}: missing authenticated peer for conn {conn_id} \
             (unverified/delivery-only connections carry no control-plane authority)"
        ));
        return None;
    }
    Some(authenticated)
}

fn authenticated_peer_identity(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    context: &str,
) -> Option<(u16, crate::envelope::NodeSessionIdentity)> {
    let route_slot = authenticated_peer_node_id(mgr, conn_id, claim_token, context)?;
    // SAFETY: `authenticated_peer_node_id` verified the live manager.
    let mgr = unsafe { &*mgr };
    let identity = mgr.connections.access(|connections| {
        connections
            .iter()
            .find(|connection| {
                connection.conn_id == conn_id
                    && connection.publication_token == claim_token
                    && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
            })
            .and_then(|connection| {
                Some(crate::envelope::NodeSessionIdentity {
                    node_id: connection.peer_identity?,
                    session_incarnation: connection.peer_session_incarnation,
                })
            })
    });
    let Some(identity) = identity else {
        set_last_error(format!(
            "connection reader {context}: authenticated connection is missing its NodeId/session"
        ));
        return None;
    };
    Some((route_slot, identity))
}

fn location_matches_node_session(
    location: Location,
    identity: crate::envelope::NodeSessionIdentity,
) -> bool {
    location.node() == identity.node_id
        && location.incarnation() == identity.session_incarnation
        && crate::pid::actor_slot_fits_internal_alias(location.slot())
}

fn location_matches_local(mgr: &HewConnMgr, location: Location) -> bool {
    if Some(location.node()) != mgr.local_identity
        || Some(location.incarnation()) != mgr.local_session_incarnation
        || !crate::pid::actor_slot_fits_internal_alias(location.slot())
    {
        return false;
    }
    let actor_id = crate::pid::hew_pid_make(mgr.local_node_id, location.slot());
    crate::lifetime::live_actors::get_actor_ptr_by_id(actor_id).is_some()
}

fn location_matches_local_session(mgr: &HewConnMgr, location: Location) -> bool {
    Some(location.node()) == mgr.local_identity
        && Some(location.incarnation()) == mgr.local_session_incarnation
        && crate::pid::actor_slot_fits_internal_alias(location.slot())
}

fn send_setup_result_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    ctrl_kind: u64,
    result: &SetupResultPayload,
) {
    let payload = match encode_setup_result_payload(result) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result payload encode failure: {err}"
            ));
            return;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind,
        payload,
    };
    let bytes = match encode_control_frame(&frame) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result frame encode failure: {err}"
            ));
            return;
        }
    };
    // SAFETY: the reader owns this exact manager/connection publication claim;
    // the send copies the encoded frame before returning.
    let _ = unsafe {
        hew_connmgr_send_preencoded_claimed(mgr, conn_id, claim_token, bytes.as_ptr(), bytes.len())
    };
}

/// Handle an inbound `CTRL_MONITOR_REQ`: a remote node is monitoring
/// one of our local actors. Record a target-side remote-watcher entry so the
/// terminal sweep can fan out a `CTRL_MONITOR_DOWN` when that actor dies.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a watcher — no fabricated state from untrusted bytes.
fn handle_monitor_req_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "monitor req")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if payload.setup_id == 0
        || !location_matches_node_session(payload.watcher, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader monitor req Location mismatch");
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("connection reader monitor req: no runtime installed");
        return;
    };
    let target_actor_id = crate::pid::hew_pid_make(mgr_ref.local_node_id, payload.target.slot());
    crate::hew_node::remote_setup_race_probe_wait(
        crate::hew_node::RemoteSetupKind::Monitor,
        target_actor_id,
    );
    let status =
        match rt
            .monitors
            .register_remote_watcher(target_actor_id, payload.watcher, payload.ref_id)
        {
            crate::monitor::RemoteWatcherSetup::Registered => SETUP_STATUS_ACCEPTED,
            crate::monitor::RemoteWatcherSetup::TargetGone => SETUP_STATUS_TARGET_GONE,
        };
    send_setup_result_frame(
        mgr,
        conn_id,
        claim_token,
        CTRL_MONITOR_SETUP_RESULT,
        &SetupResultPayload {
            setup_id: payload.setup_id,
            ref_id: payload.ref_id,
            target: payload.target,
            status,
        },
    );
}

/// Handle an inbound `CTRL_DEMONITOR`: a remote node retracted its
/// monitor of one of our local actors. Remove the target-side remote-watcher
/// entry. Idempotent / fail-closed on malformed input.
fn handle_demonitor_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader demonitor payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "demonitor")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if !location_matches_node_session(payload.watcher, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader demonitor Location mismatch");
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.monitors
        .remove_remote_watcher(payload.target.slot(), payload.watcher, payload.ref_id);
}

fn handle_setup_result_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
    kind: crate::hew_node::RemoteSetupKind,
) {
    let payload = match decode_setup_result_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader setup result payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "setup result")
    else {
        return;
    };
    if payload.setup_id == 0
        || payload.ref_id == 0
        || !location_matches_node_session(payload.target, peer_identity)
    {
        set_last_error("connection reader setup result Location mismatch");
        return;
    }
    if !crate::hew_node::complete_remote_setup_reply(
        mgr,
        conn_id,
        claim_token,
        payload.setup_id,
        kind,
        &control.payload,
    ) {
        set_last_error("connection reader setup result did not match a pending setup");
    }
}

/// Handle an inbound `CTRL_MONITOR_DOWN`: the node owning an actor we
/// monitor reports that actor reached a terminal state. Atomically claim the
/// observation and enqueue DOWN in the local watcher's system mailbox.
///
/// Fail-closed on malformed input. Removing the observation before enqueue is
/// the exactly-once disambiguation: a definitive DOWN beats a later partition
/// signal for the same registration.
fn handle_monitor_down_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_monitor_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor down payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "monitor down")
    else {
        return;
    };
    if !location_matches_node_session(payload.target, peer_identity) {
        set_last_error("connection reader monitor down target Location mismatch");
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("connection reader monitor down: no runtime installed");
        return;
    };
    let crashed = payload.reason == crate::internal::types::HewActorState::Crashed as i32;
    if !crashed && payload.reason != crate::internal::types::HewActorState::Stopped as i32 {
        set_last_error("connection reader monitor down invalid terminal reason");
        return;
    }
    if !(0..=2).contains(&payload.crash_kind) || (!crashed && payload.crash_kind != 0) {
        set_last_error("connection reader monitor down invalid crash kind");
        return;
    }
    if let Some(down) = rt
        .monitors
        .deliver_monitor_to_ref(payload.ref_id, payload.target)
    {
        rt.monitors.enqueue_down(
            down.watcher_actor_id,
            down.monitor_id,
            down.target,
            payload.reason,
            payload.crash_kind.cast_unsigned(),
        );
    }
}

/// Handle an inbound `CTRL_LINK_REQ`: a remote node is linking one of our local
/// actors. Establish the bidirectional cross-node link.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a link — no fabricated state from untrusted bytes. The
/// decode bar is HIGHER than monitor because a registered link can later crash a
/// real actor.
fn handle_link_req_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "link req")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if payload.setup_id == 0
        || !location_matches_node_session(payload.linker, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader link req Location mismatch");
        return;
    }
    let target_actor_id = crate::pid::hew_pid_make(mgr_ref.local_node_id, payload.target.slot());
    if payload.reciprocate == 1 {
        crate::hew_node::remote_setup_race_probe_wait(
            crate::hew_node::RemoteSetupKind::Link,
            target_actor_id,
        );
    }
    if let Some(status) = crate::hew_node::handle_inbound_link_req(
        &payload,
        target_actor_id,
        mgr,
        conn_id,
        claim_token,
    ) {
        send_setup_result_frame(
            mgr,
            conn_id,
            claim_token,
            CTRL_LINK_SETUP_RESULT,
            &SetupResultPayload {
                setup_id: payload.setup_id,
                ref_id: payload.ref_id,
                target: payload.target,
                status,
            },
        );
    }
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Idempotent / fail-closed on malformed input.
fn handle_unlink_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader unlink payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some((_, peer_identity)) = authenticated_peer_identity(mgr, conn_id, claim_token, "unlink")
    else {
        return;
    };
    // SAFETY: the authenticated identity helper verified the manager.
    let mgr_ref = unsafe { &*mgr };
    if !location_matches_node_session(payload.linker, peer_identity)
        || !location_matches_local_session(mgr_ref, payload.target)
    {
        set_last_error("connection reader unlink Location mismatch");
        return;
    }
    crate::hew_node::handle_inbound_unlink(&payload);
}

/// Handle an inbound `CTRL_LINK_DOWN`: the node owning an actor we LINK
/// reports it reached a terminal state. Fire the cross-node link cascade —
/// synthesize a `SYS_MSG_EXIT` into the LOCAL linked actor's mailbox and crash it
/// (for `CrashLinked`). A monitor DOWN queues a typed mailbox notification,
/// while a link DOWN queues EXIT and applies the link policy. Fail-closed on
/// malformed input; the EXIT fires exactly once and ONLY for a link entry
/// this node registered AND ONLY when the handshake-authenticated sender of this
/// connection is the same peer that entry is linked to — otherwise neither a
/// forged `ref_id` this node never linked NOR a different, genuinely-connected
/// peer that merely guessed/learned a pending link `ref_id` can crash an actor
/// linked to another, still-alive peer.
fn handle_link_down_frame(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    let payload = match decode_link_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link down payload decode failure: {err}"
            ));
            return;
        }
    };
    // issue #2652: a link DOWN crashes the LOCAL linked actor, so it must be
    // authorised by the exact handshake-authenticated owner of the peer's
    // published claim — NOT the posture-agnostic delivery id. Using
    // `peer_node_id_for_conn` here was a bypass: it returns the *self-declared*
    // node id even for an `Unverified` (delivery-only) or a superseded
    // connection, so such a peer whose declared id happened to match a stored
    // link's `remote_node_id` could crash an actor linked to the genuine,
    // still-alive owner (`deliver_link_down_to_ref`'s `== 0` guard never fires
    // because the delivery id is nonzero). Route through the exact-owner gate so
    // an `Unverified`/superseded connection yields `None` and is dropped with a
    // diagnostic before any cross-node exit cascade.
    let Some((_, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "link down")
    else {
        return;
    };
    if !location_matches_node_session(payload.target, peer_identity) {
        set_last_error("connection reader link down target Location mismatch");
        return;
    }
    crate::hew_node::handle_inbound_link_down(payload.ref_id, payload.target, payload.reason);
}

fn active_gossip_connection_ids(mgr: &HewConnMgr) -> Vec<c_int> {
    // Candidate connections: ACTIVE + gossip-capable. Snapshot conn_ids first so
    // the authenticated-owner check below (which locks `claims`) never runs
    // nested inside the `connections` read guard.
    let candidates: Vec<c_int> = mgr.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| {
                c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && supports_gossip(c.peer_feature_flags)
            })
            .map(|c| c.conn_id)
            .collect()
    });
    // issue #2652 (D9, outbound): an `Unverified` (delivery-only) or superseded
    // connection carries no control-plane authority in EITHER direction — it must
    // not RECEIVE registry gossip any more than it may inject it. Keep only
    // connections that are the exact authenticated owner of a published claim.
    candidates
        .into_iter()
        .filter(|&conn_id| authenticated_peer_node_id_for_conn(mgr, conn_id) != 0)
        .collect()
}

fn flush_registry_gossip_to_connection(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    peer_feature_flags: u32,
) {
    if !supports_gossip(peer_feature_flags) || mgr.cluster.is_null() {
        return;
    }
    // issue #2652 (D9, outbound): never flush registry gossip onto an
    // `Unverified` (delivery-only) or superseded connection — it carries no
    // control-plane authority in either direction. Only the exact authenticated
    // owner of a published claim receives cluster state. This gate runs BEFORE
    // the drain, so a denial retains the events for other connections.
    if authenticated_peer_node_id_for_conn(mgr, conn_id) == 0 {
        return;
    }

    // SAFETY: cluster pointer belongs to this live connection manager.
    let events = unsafe { (&*mgr.cluster).take_registry_gossip(MAX_REGISTRY_GOSSIP_FLUSH_EVENTS) };
    let frames: Vec<Vec<u8>> = events
        .into_iter()
        .filter_map(|event| {
            encode_registry_gossip_control(&event.name, event.location, event.is_add)
        })
        .collect();
    send_registry_flush_frames(mgr, conn_id, publication_token, frames, 0);
}

/// Send encoded registry-gossip flush frames in order. On a failed send, park
/// the unsent remainder (including the failed frame) for retry on the
/// connection's next inbound frame — the flush is one-shot at the cluster
/// level (drained events age out of the queue), so dropping here would leave
/// the peer permanently without those names.
fn send_registry_flush_frames(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    mut frames: Vec<Vec<u8>>,
    attempts: u32,
) {
    let mut sent = 0;
    while let Some(bytes) = frames.get(sent) {
        // SAFETY: mgr is live and `bytes` is a complete encoded control frame.
        if unsafe { send_preencoded_on_manager(mgr, conn_id, None, bytes.as_ptr(), bytes.len()) }
            != 0
        {
            set_last_error(format!(
                "registry gossip flush send failed for conn {conn_id}; \
                 parking {} frame(s) for retry",
                frames.len() - sent
            ));
            // Re-park AT FRONT: a concurrent broadcast may have appended newer
            // frames to the entry while this drain ran; the unsent remainder
            // predates them, so it must go back ahead of them (FIFO).
            park_pending_registry_flush(
                mgr,
                conn_id,
                publication_token,
                frames.split_off(sent),
                attempts,
                true,
            );
            return;
        }
        sent += 1;
    }
}

/// Park unsent flush frames for retry, bound to the admission's publication
/// token. Bounded at [`MAX_REGISTRY_GOSSIP_FLUSH_EVENTS`] frames per
/// connection (oldest evicted with a diagnostic).
fn park_pending_registry_flush(
    mgr: &HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    frames: Vec<Vec<u8>>,
    attempts: u32,
    at_front: bool,
) {
    if frames.is_empty() {
        return;
    }
    mgr.pending_registry_flush.access(|map| {
        let entry = map.entry(conn_id).or_insert_with(|| PendingRegistryFlush {
            token: publication_token,
            frames: Vec::new(),
            attempts: 0,
        });
        if entry.token != publication_token {
            // A previous admission's leftovers never survive into a successor
            // on a reused conn_id.
            entry.token = publication_token;
            entry.frames.clear();
            entry.attempts = 0;
        }
        entry.attempts = entry.attempts.max(attempts);
        if at_front {
            entry.frames.splice(0..0, frames);
        } else {
            entry.frames.extend(frames);
        }
        if entry.frames.len() > MAX_REGISTRY_GOSSIP_FLUSH_EVENTS {
            let excess = entry.frames.len() - MAX_REGISTRY_GOSSIP_FLUSH_EVENTS;
            entry.frames.drain(..excess);
            set_last_error(format!(
                "registry gossip retry buffer overflow for conn {conn_id}: \
                 evicted {excess} oldest frame(s)"
            ));
        }
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
    });
}

/// Retry a parked registry-gossip flush on inbound traffic from `conn_id`.
/// Consumes the parked entry only for the SAME admission (publication token
/// match); re-checks the outbound authority gate first, dropping the frames
/// fail-closed if the connection lost its claim (superseded / removed — the
/// successor's own establish flush carries current state).
fn retry_pending_registry_flush(mgr: &HewConnMgr, conn_id: c_int, claim_token: u64) {
    if mgr.pending_registry_flush_count.load(Ordering::Acquire) == 0 {
        return;
    }
    let Some((frames, attempts)) = mgr.pending_registry_flush.access(|map| {
        let matches = map
            .get(&conn_id)
            .is_some_and(|pending| pending.token == claim_token);
        if !matches {
            return None;
        }
        let pending = map.remove(&conn_id);
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
        pending.map(|p| (p.frames, p.attempts + 1))
    }) else {
        return;
    };
    if attempts > MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS {
        // Fail closed, loudly: this connection's sends have failed
        // MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS drains in a row — drop the parked
        // frames instead of retrying forever on a connection that is
        // evidently broken.
        set_last_error(format!(
            "registry gossip retry budget exhausted for conn {conn_id}: dropping \
             {} parked frame(s) after {MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS} failed attempts",
            frames.len()
        ));
        return;
    }
    if authenticated_peer_node_id_for_conn(mgr, conn_id) == 0 {
        set_last_error(format!(
            "registry gossip retry dropped for conn {conn_id}: connection no longer \
             holds its published claim"
        ));
        return;
    }
    send_registry_flush_frames(mgr, conn_id, claim_token, frames, attempts);
}

// ── SWIM failure-detection transport ────────────────────────────────────

/// The posture-agnostic receiver-local route slot assigned to an ACTIVE
/// connection, or `0` if none. It carries no control-plane authority: an
/// unverified or superseded connection can still retain its delivery slot.
/// Security-relevant callers must use
/// [`authenticated_peer_node_id_for_conn`] instead. Retained as a test-only
/// accessor for route-publication tests; production inbound delivery routes via
/// the routing table populated at admission, not this lookup.
#[cfg(test)]
pub(crate) fn peer_node_id_for_conn(mgr: &HewConnMgr, conn_id: c_int) -> u16 {
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map_or(0, |c| c.peer_node_id)
    })
}

/// The *authenticated* peer `NodeId` for a control frame arriving on `conn_id`
/// (issue #2652, D9/D12). Returns `Some(node_id)` only when the connection is
/// ACTIVE, carries `Strict` (credential-authenticated) posture, and advertised
/// a nonzero `NodeId` — i.e. it is the exact current owner of a published,
/// credential-bound claim. Returns `None` for an `Unverified` (loopback-dev /
/// opt-out) connection: such a peer is delivery-only and carries no
/// control-plane authority, so it can never inject registry/SWIM/cluster
/// membership gossip, monitor/link control, complete another peer's ask, NOR
/// receive outbound gossip/SWIM traffic. Inbound user-message delivery (D9's one
/// intentional `Unverified` capability) routes through the admission-populated
/// routing table keyed by the self-declared delivery id, so it does not consult
/// this posture gate.
pub(crate) fn authenticated_peer_node_id_for_conn(mgr: &HewConnMgr, conn_id: c_int) -> u16 {
    // (a) the connection must be ACTIVE, carry `Strict` posture, and advertise a
    // nonzero NodeId.
    let claimed = mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && c.posture == Posture::Strict
                    && c.peer_node_id != 0
            })
            .map(|c| (c.peer_identity, c.peer_node_id))
    });
    let Some((Some(identity), route_slot)) = claimed else {
        return 0;
    };
    // (b)+(c) the connection must be the *exact current owner* of the published
    // claim for its NodeId (issue #2652, D9 / BLOCK-4 point 2): the claim must be
    // `Published` AND owned by THIS `conn_id`. A superseded connection (D3 point
    // 2) still carries `Strict` posture but its `conn_id` no longer matches the
    // claim owner, so it loses control authority the instant the map is
    // overwritten — before its actor is even closed.
    let (lock, _condvar) = &mgr.claims;
    let guard = lock
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    match guard.get(&identity) {
        Some(claim) if claim.state == ClaimState::Published && claim.conn_id == conn_id => {
            route_slot
        }
        _ => 0,
    }
}

pub(crate) fn local_location_for_actor(mgr: &HewConnMgr, actor_id: u64) -> Option<Location> {
    let slot = crate::pid::hew_pid_serial(actor_id);
    Location::new(mgr.local_identity?, slot, mgr.local_session_incarnation?).ok()
}

/// As [`authenticated_peer_node_id_for_conn`], but tolerant of this
/// connection's own admission window. `hew_connmgr_add` spawns the reader
/// thread BEFORE `install_connection_actor` pushes the `ConnectionActor` into
/// `mgr.connections` and BEFORE `publish_connection_established` transitions
/// the claim `Reserved → Published`, so an inbound control frame can arrive
/// while the connection is missing from the connections list, or listed but
/// with its claim still `Reserved`. Some of those frames are one-shot — the
/// peer's registry-gossip flush at ITS publish drains the peer's event queue
/// and is never retransmitted — so an instant deny anywhere in that window
/// permanently loses cluster state (the lookup-unresolved race).
///
/// The gate therefore keys on the CLAIMS map first, not the connections list:
/// `reserve_claim` runs strictly before the reader spawn, so a `Reserved`
/// claim owned by THIS admission — identified by `(conn_id, claim_token)`,
/// the publication token minted for this admission before the reserve — is
/// present for the entire admission window and is the complete "admission in
/// flight" signal. While that claim is `Reserved`, block on the claims
/// condvar until the local admission decision resolves; every claim
/// transition (`publish_claim` / `abort_claim` / `retire_claim` /
/// `reserve_claim`) notifies the condvar, so this is a readiness signal on a
/// local bounded step, never a poll. Once the claim is `Published` — which
/// happens strictly after the connections-list install — the connections
/// entry decides (posture, ACTIVE state, self-declared node), matched by the
/// same publication token. An absent claim or a claim from a different
/// admission (superseded / aborted / a successor on a REUSED transport
/// `conn_id`) denies immediately and fail-closed, and the wait itself fails
/// closed on the `CLAIM_RESERVE_WAIT_MS` backstop.
///
/// The token binding matters because transport `conn_id`s are recycled: the
/// TCP transport hands out the first free slot index (`store_conn`), so a
/// reader still processing an already-read frame after its connection was
/// removed could otherwise observe a successor connection's claim under the
/// same `conn_id` and adopt that successor's authority. The publication token
/// is unique per admission (`next_publication_token`), so a stale reader's
/// gate resolves to deny the moment its own claim is gone.
pub(crate) fn wait_authenticated_peer_node_id_for_conn(
    mgr: &HewConnMgr,
    conn_id: c_int,
    claim_token: u64,
) -> u16 {
    let deadline = std::time::Instant::now() + Duration::from_millis(CLAIM_RESERVE_WAIT_MS);
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock.lock_or_recover();
    loop {
        let own_claim = guard
            .iter()
            .find(|(_, claim)| claim.conn_id == conn_id && claim.publication_token == claim_token)
            .map(|(node_id, claim)| (*node_id, claim.route_slot, claim.state));
        match own_claim {
            Some((claimed, route_slot, ClaimState::Published)) => {
                // Our admission published. Publication happens strictly after
                // the connections-list install, so the entry is present; bind
                // it by the same publication token (never bare `conn_id`) and
                // apply the posture / ACTIVE / self-declared-node checks the
                // non-waiting gate applies.
                drop(guard);
                return mgr.connections.access(|conns| {
                    conns
                        .iter()
                        .find(|c| {
                            c.conn_id == conn_id
                                && c.publication_token == claim_token
                                && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                                && c.posture == Posture::Strict
                                && c.peer_identity == Some(claimed)
                                && c.peer_node_id == route_slot
                        })
                        .map_or(0, |c| c.peer_node_id)
                });
            }
            Some((_, _, ClaimState::Reserved)) => {
                // Our own admission is mid-flight (pre-install or
                // pre-publication); wait for it to publish or abort rather
                // than dropping the frame.
                let remaining = deadline.saturating_duration_since(std::time::Instant::now());
                if remaining.is_zero() {
                    return 0;
                }
                let (next_guard, _timeout) = condvar.wait_timeout_or_recover(guard, remaining);
                guard = next_guard;
            }
            // No claim owned by this admission: superseded, aborted, or a
            // claimless (node_id 0) connection — no authority, deny now.
            Some((_, _, ClaimState::Retired)) | None => return 0,
        }
    }
}

/// D12 data-plane gate: decide whether an inbound *ask* on `conn_id` must be
/// dropped because the source connection carries no reply/route authority.
///
/// Returns `true` (deny) when the envelope is an ask (`request_id > 0`) and the
/// connection is not the exact current owner of a published, credential-bound
/// claim — i.e. an `Unverified` (loopback-dev / opt-out) or superseded peer. A
/// fire-and-forget delivery (`request_id == 0`) is never denied here: it flows
/// on the D9 self-declared delivery route, the sole allowed `Unverified`
/// capability.
#[cfg(test)]
fn inbound_ask_denied_unverified(mgr: *mut HewConnMgr, conn_id: c_int, request_id: u64) -> bool {
    if request_id == 0 {
        return false;
    }
    if mgr.is_null() {
        return true;
    }
    // SAFETY: reader_loop holds a live manager pointer for this connection while
    // the reader thread runs; the null case is handled above.
    let mgr_ref = unsafe { &*mgr };
    authenticated_peer_node_id_for_conn(mgr_ref, conn_id) == 0
}

/// Deposit an inbound reply envelope (`request_id > 0`, `source_node_id == 0`)
/// into the reply routing table, bypassing the normal inbound router. The
/// completion validates the originating `(conn_mgr, conn_id)` (issue #2652 D12)
/// so a peer cannot complete or reject another peer's ask.
fn deposit_reply_envelope(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    peer_feature_flags: u32,
    envelope: &EnvelopeFrame,
) {
    if is_ask_rejection_reply(envelope.msg_type, peer_feature_flags) {
        // Rejection reply: the remote node hit its inbound ask path and sent an
        // encoded AskError reason. Mark the pending ask as failed so the
        // originating caller gets the precise remote rejection reason. The
        // `supports_ask_rejection` guard ensures old nodes (which never send
        // this sentinel) cannot trigger this path even if they happen to send a
        // message with msg_type = 65535.
        crate::hew_node::fail_remote_reply(
            mgr.cast_const(),
            conn_id,
            envelope.request_id,
            envelope.payload.as_slice(),
        );
    } else {
        crate::hew_node::complete_remote_reply(
            mgr.cast_const(),
            conn_id,
            envelope.request_id,
            envelope.payload.as_slice(),
        );
    }
}

/// Encode a SWIM control frame from a payload.
fn encode_swim_control(payload: &SwimControlPayload) -> Option<Vec<u8>> {
    let payload_bytes = match encode_swim_payload(payload) {
        Ok(bytes) => bytes,
        Err(err) => {
            set_last_error(format!("SWIM control payload encode failure: {err}"));
            return None;
        }
    };
    let frame = ControlFrame {
        version: WIRE_VERSION,
        ctrl_kind: CTRL_SWIM,
        payload: payload_bytes,
    };
    match encode_control_frame(&frame) {
        Ok(bytes) => Some(bytes),
        Err(err) => {
            set_last_error(format!("SWIM control frame encode failure: {err}"));
            None
        }
    }
}

/// Drain the cluster's pending membership gossip as wire entries for
/// piggybacking on an outbound SWIM frame (C6).
fn node_session_for_route(
    mgr: &HewConnMgr,
    cluster: &HewCluster,
    route_slot: u16,
) -> Option<crate::envelope::NodeSessionIdentity> {
    if route_slot == mgr.local_node_id {
        return Some(crate::envelope::NodeSessionIdentity {
            node_id: mgr.local_identity?,
            session_incarnation: mgr.local_session_incarnation?,
        });
    }
    Some(crate::envelope::NodeSessionIdentity {
        node_id: mgr.auth.node_id_for_route_slot(route_slot)?,
        session_incarnation: cluster.member_session(route_slot)?,
    })
}

#[allow(
    clippy::unnecessary_lazy_evaluations,
    reason = "cfg(test) block inside closure may return Some; not lazy-evaluable"
)]
fn route_slot_for_identity(mgr: &HewConnMgr, node_id: NodeId) -> Option<u16> {
    if Some(node_id) == mgr.local_identity {
        Some(mgr.local_node_id)
    } else {
        mgr.auth.route_slot_for_node_id(node_id).or_else(|| {
            #[cfg(test)]
            {
                let bytes = node_id.to_bytes();
                if bytes[..14].iter().all(|byte| *byte == 0) {
                    return Some(u16::from_be_bytes([bytes[14], bytes[15]]));
                }
            }
            None
        })
    }
}

fn collect_swim_gossip(mgr: &HewConnMgr, cluster: &HewCluster) -> Vec<SwimGossipEntry> {
    cluster
        .take_swim_gossip(cluster.max_gossip_per_msg())
        .into_iter()
        .filter_map(|(route_slot, state, incarnation)| {
            Some(SwimGossipEntry {
                member: node_session_for_route(mgr, cluster, route_slot)?,
                state,
                incarnation,
            })
        })
        .collect()
}

/// Build a SWIM frame of `msg_type` to `target_node`, stamped with the local
/// node's identity and incarnation, carrying a fresh piggybacked gossip batch.
fn build_swim_frame(
    mgr: &HewConnMgr,
    cluster: &HewCluster,
    msg_type: i32,
    target_node: u16,
) -> Option<Vec<u8>> {
    let payload = SwimControlPayload {
        msg_type,
        from: node_session_for_route(mgr, cluster, mgr.local_node_id)?,
        incarnation: cluster.local_incarnation(),
        target: if target_node == 0 {
            None
        } else {
            Some(node_session_for_route(mgr, cluster, target_node)?)
        },
        gossip: collect_swim_gossip(mgr, cluster),
    };
    encode_swim_control(&payload)
}

/// Send a single SWIM protocol message to a specific peer node.
///
/// Used by the SWIM driver to issue direct PINGs and by the indirect-probe
/// path (C4) to relay `PING` / `PING_REQ`. `target_node` is the
/// indirect-probe subject for `PING_REQ` frames; `0` for direct PING / ACK.
///
/// Returns `0` on a successful send, `-1` if no active gossip-capable
/// connection to `peer_node_id` exists or the send failed.
///
/// # Safety
///
/// `mgr` must be a valid connection manager pointer for the duration of the
/// call when non-null.
pub(crate) unsafe fn hew_connmgr_send_swim(
    mgr: *mut HewConnMgr,
    peer_node_id: u16,
    msg_type: i32,
    target_node: u16,
) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees manager pointer validity for this call.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        return -1;
    }

    // SAFETY: mgr validity is the caller's contract, re-checked non-null above.
    let conn_id = unsafe { hew_connmgr_conn_id_for_node(mgr, peer_node_id) };
    if conn_id < 0 {
        return -1;
    }
    // SAFETY: mgr validity is the caller's contract, re-checked non-null above.
    let flags = unsafe { hew_connmgr_feature_flags_for_node(mgr, peer_node_id) };
    if !supports_gossip(flags) {
        return -1;
    }
    // issue #2652 (D9, outbound): SWIM membership traffic goes only to the exact
    // authenticated owner of a published claim. An `Unverified` (delivery-only)
    // or superseded connection resolves to 0 here and is refused — symmetric
    // with the inbound SWIM cross-attribution gate.
    if authenticated_peer_node_id_for_conn(mgr_ref, conn_id) == 0 {
        return -1;
    }

    // SAFETY: cluster pointer is owned by the live manager.
    let cluster = unsafe { &*mgr_ref.cluster };
    let Some(bytes) = build_swim_frame(mgr_ref, cluster, msg_type, target_node) else {
        return -1;
    };
    // SAFETY: manager is live, bytes is a complete encoded control frame.
    if unsafe { send_preencoded_on_manager(mgr_ref, conn_id, None, bytes.as_ptr(), bytes.len()) }
        == 0
    {
        0
    } else {
        -1
    }
}

/// Return the node IDs of all active gossip-capable peer connections.
///
/// The SWIM driver uses this to choose indirect-probe relays (K random peers
/// excluding the probe target).
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_active_swim_peers(mgr: *const HewConnMgr) -> Vec<u16> {
    if mgr.is_null() {
        return Vec::new();
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref
        .connections
        .access(|conns| {
            conns
                .iter()
                .filter(|c| {
                    c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                        && supports_gossip(c.peer_feature_flags)
                        && c.peer_node_id != 0
                })
                .map(|c| (c.conn_id, c.peer_node_id))
                .collect::<Vec<(c_int, u16)>>()
        })
        .into_iter()
        // issue #2652 (D9, outbound): SWIM is control-plane traffic — an
        // `Unverified` (delivery-only) or superseded peer must not be probed or
        // relayed to. Keep only exact authenticated claim owners. The snapshot above
        // ensures the claim-lock check never nests inside the `connections` guard.
        .filter(|&(conn_id, _)| authenticated_peer_node_id_for_conn(mgr_ref, conn_id) != 0)
        .map(|(_, node)| node)
        .collect()
}

/// Handle an inbound SWIM control frame.
///
/// 1. Decode + bound the payload (fail-closed).
/// 2. Reject cross-attribution: the claimed `from_node` MUST match the
///    handshake-authenticated identity of the connection it arrived on.
/// 3. Apply piggybacked membership gossip (C6 import) and run incarnation
///    self-refutation (C5).
/// 4. Run the SWIM state machine via `process_message`.
/// 5. Respond per message type: PING -> ACK; `PING_REQ` -> forward a real PING
///    to the indirect-probe target (C4); ACK -> no response.
fn handle_swim_control_frame(
    mgr: *mut HewConnMgr,
    peer_feature_flags: u32,
    conn_id: c_int,
    claim_token: u64,
    control: &ControlFrame,
) {
    if !supports_gossip(peer_feature_flags) {
        set_last_error("connection reader rejected SWIM frame from non-gossip peer");
        return;
    }
    let payload = match decode_swim_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader SWIM payload decode failure: {err}"
            ));
            return;
        }
    };
    if mgr.is_null() {
        set_last_error("connection reader SWIM frame missing manager");
        return;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.cluster.is_null() {
        set_last_error("connection reader SWIM frame missing cluster");
        return;
    }

    // Cross-attribution defence + D9/D12 control-plane gate: the frame's
    // claimed sender must equal the *authenticated* (`Strict`) handshake
    // identity of the connection it arrived on. An `Unverified` (delivery-only)
    // connection resolves to 0 here and is rejected — it cannot inject SWIM
    // membership state. Waiting variant: the first inbound SWIM frame can race
    // this connection's own Reserved → Published publication.
    let Some((authenticated, peer_identity)) =
        authenticated_peer_identity(mgr, conn_id, claim_token, "SWIM")
    else {
        return;
    };
    if payload.from != peer_identity {
        set_last_error("connection reader SWIM sender does not match authenticated peer");
        return;
    }
    // SAFETY: cluster pointer is owned by the live node/manager.
    let cluster = unsafe { &*mgr_ref.cluster };

    let mut gossip = Vec::with_capacity(payload.gossip.len());
    for entry in &payload.gossip {
        let Some(route_slot) = route_slot_for_identity(mgr_ref, entry.member.node_id) else {
            set_last_error("connection reader SWIM gossip contains unknown NodeId");
            return;
        };
        gossip.push((
            route_slot,
            entry.member.session_incarnation,
            entry.state,
            entry.incarnation,
        ));
    }
    let target_node = match payload.target {
        Some(target) => {
            let Some(route_slot) = route_slot_for_identity(mgr_ref, target.node_id) else {
                set_last_error("connection reader SWIM target contains unknown NodeId");
                return;
            };
            let expected_session = if route_slot == mgr_ref.local_node_id {
                mgr_ref.local_session_incarnation
            } else {
                cluster.member_session(route_slot)
            };
            if expected_session != Some(target.session_incarnation) {
                set_last_error("connection reader SWIM target session mismatch");
                return;
            }
            route_slot
        }
        None => 0,
    };

    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running. All driven methods
    // (apply_swim_gossip / refute_if_suspected / process_message) take &self
    // and synchronize internally via Mutex/Atomic, so a shared reference is
    // sound even with the SWIM driver thread concurrently calling tick().
    // C6: fold piggybacked membership gossip into our view.
    cluster.apply_swim_gossip(&gossip);
    // C5: if any gossip suspects us, refute by bumping our incarnation. The
    // refutation rides out on our next PING/ACK as an ALIVE-about-self event.
    let _ = cluster.refute_if_suspected(&gossip);

    // Run the SWIM state machine for this message. The authenticated peer ID
    // is supplied as the source-connection identity so the cluster's
    // source-mismatch guard validates the claim.
    cluster.process_message(
        payload.msg_type,
        authenticated,
        payload.incarnation,
        authenticated,
    );

    // Respond per message type.
    match payload.msg_type {
        crate::cluster::SWIM_MSG_PING => {
            // Direct probe: ACK the sender.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, authenticated, crate::cluster::SWIM_MSG_ACK, 0)
            };
        }
        crate::cluster::SWIM_MSG_PING_REQ
            if target_node != 0 && target_node != mgr_ref.local_node_id =>
        {
            // C4 indirect probing: forward a real PING to the probe target so
            // the relay actually exercises the target's liveness, then the
            // target's ACK propagates membership back via gossip. A no-op if
            // we have no active connection to the target.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, target_node, crate::cluster::SWIM_MSG_PING, 0)
            };
        }
        _ => {
            // ACK (and any other type) requires no response; state already
            // updated via process_message above.
        }
    }
}

#[cfg(feature = "encryption")]
fn supports_encryption(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ENCRYPTION != 0
}

#[cfg(feature = "encryption")]
fn noise_is_initiator(local: &HewHandshake, peer: &HewHandshake) -> Option<bool> {
    if local.node_id != peer.node_id {
        return Some(local.node_id.to_bytes() < peer.node_id.to_bytes());
    }
    match local.static_noise_pubkey.cmp(&peer.static_noise_pubkey) {
        std::cmp::Ordering::Less => Some(true),
        std::cmp::Ordering::Greater => Some(false),
        std::cmp::Ordering::Equal => None,
    }
}

#[cfg(feature = "encryption")]
unsafe fn upgrade_noise(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Option<(snow::TransportState, [u8; NOISE_STATIC_PUBKEY_LEN])> {
    let initiator = noise_is_initiator(local, peer)?;
    // SAFETY: transport pointer validity is guaranteed by caller.
    let t = unsafe { &*transport };
    // SAFETY: vtable pointer validity is guaranteed by transport construction.
    let ops = unsafe { t.ops.as_ref() }?;
    let send_fn = ops.send?;
    let recv_fn = ops.recv?;

    let builder = snow::Builder::new(NOISE_PATTERN.parse().ok()?);
    let mut handshake = if initiator {
        builder
            .local_private_key(local_private_key)
            .ok()?
            .build_initiator()
            .ok()?
    } else {
        builder
            .local_private_key(local_private_key)
            .ok()?
            .build_responder()
            .ok()?
    };

    // Wrap handshake buffers in Zeroizing so ephemeral key material is
    // zeroised on all exit paths (normal return, early `?`, and unwind).
    let mut msg = Zeroizing::new(vec![0u8; NOISE_MAX_MSG_SIZE]);
    let mut payload = Zeroizing::new(vec![0u8; NOISE_MAX_MSG_SIZE]);

    if initiator {
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        handshake.read_message(&msg[..n], &mut payload).ok()?;
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
    } else {
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        handshake.read_message(&msg[..n], &mut payload).ok()?;
        let n = handshake.write_message(&[], &mut msg).ok()?;
        // SAFETY: msg points to n readable bytes.
        if unsafe { send_fn(t.r#impl, conn_id, msg.as_ptr().cast(), n) } < 0 {
            return None;
        }
        // SAFETY: msg has NOISE_MAX_MSG_SIZE writable bytes.
        let n = unsafe { recv_fn(t.r#impl, conn_id, msg.as_mut_ptr().cast(), msg.len()) };
        if n <= 0 {
            return None;
        }
        #[expect(clippy::cast_sign_loss, reason = "n > 0 checked above")]
        let n = n as usize;
        handshake.read_message(&msg[..n], &mut payload).ok()?;
    }

    let remote_static = handshake.get_remote_static()?;
    if remote_static.len() != NOISE_STATIC_PUBKEY_LEN {
        return None;
    }
    let mut remote_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
    remote_pubkey.copy_from_slice(remote_static);
    let transport = handshake.into_transport_mode().ok()?;
    Some((transport, remote_pubkey))
}

// ── Reader thread ──────────────────────────────────────────────────────

/// Reader thread: loops calling transport recv, decodes envelopes,
/// and routes to local actors via the inbound router callback.
/// Cleanup after reader loop exit: remove from manager and attempt reconnection
/// if the drop was unexpected (not triggered by explicit stop).
fn reader_cleanup(mgr: *mut HewConnMgr, conn_id: c_int, stop_flag: &AtomicI32) {
    // Evict any stashed I/O span context (idempotent; no-op if tracing was disabled).
    crate::tracing::io_span_evict(conn_id);

    let unexpected_drop = stop_flag.load(Ordering::Acquire) == 0;
    if unexpected_drop {
        if !mgr.is_null() {
            crate::hew_node::fail_remote_replies_for_connection(mgr.cast_const(), conn_id);
        }
        // SAFETY: `mgr` and `conn_id` originate from a live connection manager.
        let reconnect_plan = unsafe {
            if mgr.is_null() {
                None
            } else {
                reconnect_plan(&*mgr, conn_id)
            }
        };
        // SAFETY: manager and conn_id come from active reader state.
        let _ = unsafe { hew_connmgr_remove(mgr, conn_id) };
        if let Some(plan) = reconnect_plan {
            spawn_reconnect_worker(mgr, conn_id, plan);
        }
    }
}

#[expect(
    clippy::needless_pass_by_value,
    reason = "SendTransport and Arc values are moved into this thread from spawn closure"
)]
// The 8th argument (`noise_transport`) only exists under the `encryption`
// feature; without it the arg list is under the lint threshold, so the
// expectation must be conditional or it goes unfulfilled.
#[cfg_attr(
    feature = "encryption",
    expect(
        clippy::too_many_arguments,
        reason = "reader_loop captures all per-connection state; splitting into a struct \
                  would require unsafe Send impls for the contained raw pointers"
    )
)]
#[allow(
    clippy::too_many_lines,
    reason = "reader_loop spans the full connection read path; refactoring would require unsafe Send impls"
)]
fn reader_loop(
    mgr: SendConnMgr,
    transport: SendTransport,
    conn_id: c_int,
    claim_token: u64,
    stop_flag: Arc<AtomicI32>,
    last_activity: Arc<AtomicU64>,
    router: Option<InboundRouter>,
    peer_feature_flags: u32,
    #[cfg(feature = "encryption")] noise_transport: Arc<Mutex<Option<snow::TransportState>>>,
) {
    let mgr = mgr.0;
    let transport = transport.0;
    let mut buf = vec![0u8; 65536]; // 64KiB read buffer (heap-allocated)

    while stop_flag.load(Ordering::Acquire) == 0 {
        // SAFETY: transport is valid for the manager's lifetime; conn_id
        // is valid for this connection's lifetime.
        let bytes_read = unsafe {
            let t = &*transport;
            if let Some(ops) = t.ops.as_ref() {
                if let Some(recv_fn) = ops.recv {
                    recv_fn(t.r#impl, conn_id, buf.as_mut_ptr().cast(), buf.len())
                } else {
                    -1
                }
            } else {
                -1
            }
        };

        if bytes_read <= 0 {
            reader_cleanup(mgr, conn_id, &stop_flag);
            break;
        }

        #[expect(clippy::cast_sign_loss, reason = "bytes_read > 0 checked above")]
        let read_len = bytes_read as usize;

        // Decrypt in place when encryption is on; `buf.as_mut_ptr()` is stable
        // across the in-place copy, so only the length can change.
        #[cfg(feature = "encryption")]
        let payload_len = {
            let mut len = read_len;
            let mut decrypted = vec![0u8; read_len];
            let mut guard = noise_transport.lock_or_recover();
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.read_message(&buf[..read_len], &mut decrypted) else {
                    set_last_error("connection decrypt failure".to_string());
                    reader_cleanup(mgr, conn_id, &stop_flag);
                    break;
                };
                len = n;
                buf[..len].copy_from_slice(&decrypted[..len]);
            }
            len
        };
        #[cfg(not(feature = "encryption"))]
        let payload_len = read_len;
        let payload_ptr = buf.as_mut_ptr();

        // Update heartbeat.
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { crate::io_time::hew_now_ms() };
        last_activity.store(now, Ordering::Release);

        // Decode CBOR wire frame and route.
        // SAFETY: buf contains `payload_len` valid bytes from recv/decrypt.
        let frame_bytes =
            unsafe { std::slice::from_raw_parts(payload_ptr.cast_const(), payload_len) };
        let wire_frame = match decode_wire_frame(frame_bytes) {
            Ok(frame) => frame,
            Err(err) => {
                set_last_error(format!(
                    "connection reader CBOR wire-frame decode failure: {err}"
                ));
                continue;
            }
        };

        match wire_frame {
            WireFrame::Control(control) => {
                handle_control_frame(mgr, peer_feature_flags, conn_id, claim_token, &control);
            }
            WireFrame::Envelope(mut envelope) => {
                let Some(router_fn) = router else {
                    continue;
                };

                // Reply envelopes (request_id > 0, no target or source) are
                // deposited directly into the reply routing table, bypassing
                // the normal inbound router.
                if envelope.request_id > 0 && envelope.target.is_none() && envelope.source.is_none()
                {
                    deposit_reply_envelope(mgr, conn_id, peer_feature_flags, &envelope);
                } else {
                    let Some((authenticated, peer_identity)) =
                        authenticated_peer_identity(mgr, conn_id, claim_token, "envelope")
                    else {
                        continue;
                    };
                    // SAFETY: the authenticated identity helper verified the manager.
                    let mgr_ref = unsafe { &*mgr };
                    let Some(target) = envelope.target else {
                        set_last_error("connection reader envelope missing target Location");
                        continue;
                    };
                    if !location_matches_local(mgr_ref, target) {
                        set_last_error("connection reader envelope target Location mismatch");
                        continue;
                    }
                    if envelope
                        .source
                        .is_some_and(|source| !location_matches_node_session(source, peer_identity))
                    {
                        set_last_error("connection reader envelope source Location mismatch");
                        continue;
                    }
                    // I/O recv span: bracket the router call so the
                    // mailbox enqueue captures the io_recv span as the
                    // parent of the actor-dispatch span.
                    // Fast path: `io_recv_span_begin` returns None when
                    // tracing is disabled (a single atomic load).
                    let saved_ctx = crate::tracing::io_recv_span_begin(conn_id);
                    let payload_ptr = if envelope.payload.is_empty() {
                        std::ptr::null_mut()
                    } else {
                        envelope.payload.as_mut_ptr()
                    };
                    // SAFETY: router_fn is the manager's configured inbound
                    // router; payload_ptr is null for empty payloads or points
                    // at envelope-owned bytes valid for this call.
                    unsafe {
                        router_fn(
                            crate::pid::hew_pid_make(mgr_ref.local_node_id, target.slot()),
                            envelope.msg_type,
                            payload_ptr,
                            envelope.payload.len(),
                            envelope.request_id,
                            authenticated,
                            mgr,
                        );
                    }
                    if let Some(saved) = saved_ctx {
                        crate::tracing::io_recv_span_end(saved);
                    }
                }
            }
        }

        // A parked one-shot registry-gossip flush (initial send failed)
        // retries on this connection's next inbound traffic — SWIM keeps
        // frames flowing on an established connection, so retry latency is
        // bounded by the protocol period. One atomic load when nothing is
        // parked.
        if !mgr.is_null() {
            // SAFETY: reader_loop owns a live manager pointer for this connection.
            retry_pending_registry_flush(unsafe { &*mgr }, conn_id, claim_token);
        }
    }
}

// ── C ABI ──────────────────────────────────────────────────────────────

/// Create a new connection manager.
///
/// `transport` must remain valid for the lifetime of the manager.
/// `router` is called for each inbound message; may be null if inbound
/// routing is not needed.
///
/// # Safety
///
/// - `transport` must be a valid, non-null pointer to a [`HewTransport`].
/// - `router` (if non-null) must be a valid function pointer that
///   remains valid for the manager's lifetime.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_new(
    transport: *mut HewTransport,
    router: Option<InboundRouter>,
    routing_table: *mut HewRoutingTable,
    cluster: *mut HewCluster,
    local_node_id: u16,
) -> *mut HewConnMgr {
    // The exported C ABI is unchanged (no Rust type crosses the boundary). An
    // external C caller gets a fail-closed *unconfigured* posture — strict on
    // any non-loopback/`Unknown` connection, `Unverified` only on a
    // demonstrated-loopback endpoint. Production installs a real per-node
    // snapshot via the internal `connmgr_new` constructor below.
    // SAFETY: transport contract forwarded to the internal constructor.
    let mgr = unsafe {
        connmgr_new(
            transport,
            router,
            routing_table,
            cluster,
            local_node_id,
            PeerAuthSnapshot::unconfigured(),
        )
    };
    #[cfg(test)]
    if !mgr.is_null() && local_node_id != 0 {
        // SAFETY: `mgr` was just allocated above and is uniquely owned here.
        unsafe {
            (*mgr).local_identity = Some(test_node_identity(local_node_id));
            (*mgr).local_session_incarnation = Some(1);
        }
    }
    mgr
}

/// Internal constructor taking the per-node [`PeerAuthSnapshot`] by value.
///
/// This is the per-manager authority: production (`hew_node_start`) calls it
/// with the node's installed snapshot; the C `hew_connmgr_new` shim passes
/// [`PeerAuthSnapshot::unconfigured`]. It **never** reads the public
/// `ConfigState` — each manager owns its own admission authority.
///
/// # Safety
///
/// - `transport` must be a valid, non-null pointer to a [`HewTransport`].
/// - `router` (if non-null) must be a valid function pointer valid for the
///   manager's lifetime.
pub(crate) unsafe fn connmgr_new(
    transport: *mut HewTransport,
    router: Option<InboundRouter>,
    routing_table: *mut HewRoutingTable,
    cluster: *mut HewCluster,
    local_node_id: u16,
    auth: PeerAuthSnapshot,
) -> *mut HewConnMgr {
    cabi_guard!(transport.is_null(), std::ptr::null_mut());
    let local_identity = auth.node_identity();
    let local_session_incarnation = auth.session_incarnation();
    let mgr = Box::new(HewConnMgr {
        connections: PoisonSafe::new(Vec::with_capacity(16)),
        expected_peer_ids: PoisonSafe::new(HashMap::new()),
        transport,
        inbound_router: router,
        routing_table,
        cluster,
        reconnect_enabled: AtomicBool::new(false),
        reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
        reconnect_shutdown: Arc::new(AtomicBool::new(false)),
        inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
        inbound_ask_active: Arc::new(AtomicUsize::new(0)),
        reconnect_workers: PoisonSafe::new(Vec::new()),
        reverse_link_workers: PoisonSafe::new(Vec::new()),
        reader_lifecycle: Arc::new(ReaderLifecycle::default()),
        next_publication_token: AtomicU64::new(1),
        local_node_id,
        auth,
        local_identity,
        local_session_incarnation,
        claims: (Mutex::new(HashMap::new()), Condvar::new()),
        pending_registry_flush: PoisonSafe::new(HashMap::new()),
        pending_registry_flush_count: AtomicUsize::new(0),
    });
    Box::into_raw(mgr)
}

/// Bind an outbound transport connection to an expected peer `NodeId`.
///
/// The expectation is consumed by the next [`hew_connmgr_add`] for `conn_id`.
/// A handshake claiming a different `NodeId` is rejected before the connection is
/// installed or published.
pub(crate) unsafe fn hew_connmgr_expect_peer(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    expected_node_id: u16,
) -> c_int {
    if mgr.is_null() || conn_id == HEW_CONN_INVALID || expected_node_id == 0 {
        set_last_error("hew_connmgr_expect_peer: invalid manager, connection, or node id");
        return -1;
    }
    // SAFETY: caller guarantees mgr is valid.
    unsafe { &*mgr }
        .expected_peer_ids
        .access(|expected| expected.insert(conn_id, expected_node_id));
    0
}

/// Destroy a connection manager, closing all connections.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`] and
/// must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_free(mgr: *mut HewConnMgr) {
    if !mgr.is_null() {
        // SAFETY: caller guarantees `mgr` is valid and surrenders ownership.
        let mgr = unsafe { Box::from_raw(mgr) };
        mgr.reconnect_shutdown.store(true, Ordering::Release);
        let transport = mgr.transport;

        // Keep actors published while closing so a transport slot cannot be
        // reused by a new admission until every claimed deferred send has
        // observed the close and returned.
        let closing = mgr.connections.access(|connections| {
            connections
                .iter()
                .map(|connection| {
                    connection
                        .publication_removed
                        .store(true, Ordering::Release);
                    connection.reader_stop.store(1, Ordering::Release);
                    connection.transport_closed.store(true, Ordering::Release);
                    (
                        connection.conn_id,
                        Arc::clone(&connection.claimed_send_lifecycle),
                    )
                })
                .collect::<Vec<_>>()
        });
        for (conn_id, _) in &closing {
            // SAFETY: transport is valid per manager contract.
            unsafe { close_transport_conn(transport, *conn_id) };
        }
        for (_, lifecycle) in &closing {
            lifecycle.wait_for_idle();
        }
        // The slots are now closed with no claimed sender in flight; draining
        // permits actor Drop to join each reader without any slot-reuse race.
        let drained: Vec<ConnectionActor> = mgr.connections.access(std::mem::take);
        drop(drained);
        mgr.reader_lifecycle.wait_for_idle();
        let workers: Vec<JoinHandle<()>> = mgr.reverse_link_workers.access(std::mem::take);
        for worker in workers {
            let _ = worker.join();
        }
        let workers: Vec<JoinHandle<()>> = mgr.reconnect_workers.access(std::mem::take);
        for worker in workers {
            let _ = worker.join();
        }
        // mgr is dropped here, freeing the HewConnMgr.
    }
}

pub(crate) unsafe fn hew_connmgr_mark_stopping(mgr: *mut HewConnMgr) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.reconnect_shutdown.store(true, Ordering::Release);
}

pub(crate) unsafe fn hew_connmgr_shutdown_flag(mgr: *mut HewConnMgr) -> Option<Arc<AtomicBool>> {
    if mgr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    Some(Arc::clone(&mgr_ref.reconnect_shutdown))
}

/// Close the inbound-ask spawn gate so `node_inbound_router` stops spawning new
/// workers. `hew_node_stop` calls this BEFORE draining in-flight workers, so the
/// drain terminates while already-running `handle_inbound_ask` threads still
/// flush their replies (they bail only on the later `reconnect_shutdown` /
/// `CURRENT_NODE` teardown guards).
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
pub(crate) unsafe fn hew_connmgr_close_inbound_spawn(mgr: *mut HewConnMgr) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    // SeqCst (not Release): this store is the drain side of the Dekker pairing
    // in `node_inbound_router`. `hew_node_stop` stores the gate here, then loads
    // the per-manager counter (in `drain_inbound_ask_workers`) under SeqCst; the
    // router increments the counter, then loads this gate under SeqCst. SeqCst on
    // both sides gives a single total order so a router that passes the gate is
    // always visible to the drain — Release/Acquire would let the gate store and
    // the counter load reorder across the two distinct atomics and lose a worker.
    mgr_ref.inbound_spawn_closed.store(true, Ordering::SeqCst);
}

/// Return a clone of the inbound-worker spawn gate.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
pub(crate) unsafe fn hew_connmgr_inbound_spawn_closed_flag(
    mgr: *mut HewConnMgr,
) -> Option<Arc<AtomicBool>> {
    if mgr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    Some(Arc::clone(&mgr_ref.inbound_spawn_closed))
}

/// Return a clone of the per-manager inbound-worker active counter.
///
/// Used by inbound ask and reverse-link setup paths to track workers for this
/// specific manager, and by `hew_node_stop` to drain them before freeing node
/// resources.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
pub(crate) unsafe fn hew_connmgr_inbound_ask_active(
    mgr: *mut HewConnMgr,
) -> Option<Arc<AtomicUsize>> {
    if mgr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    Some(Arc::clone(&mgr_ref.inbound_ask_active))
}

/// Resolve the active publication token only when `conn_id` is still bound to
/// the exact peer node/session owning `target`.
///
/// # Safety
///
/// `mgr` must remain valid for this call.
pub(crate) unsafe fn hew_connmgr_publication_token_for_target(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target: Location,
) -> Option<u64> {
    if mgr.is_null() {
        return None;
    }
    // SAFETY: caller guarantees `mgr` is valid for this call.
    unsafe { &*mgr }.connections.access(|connections| {
        connections
            .iter()
            .find(|connection| {
                connection.conn_id == conn_id
                    && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && connection.peer_identity == Some(target.node())
                    && connection.peer_session_incarnation == target.incarnation()
            })
            .map(|connection| connection.publication_token)
    })
}

/// Track a deferred reverse-link worker so manager teardown joins it.
///
/// # Safety
///
/// `mgr` must remain valid for this call.
pub(crate) unsafe fn hew_connmgr_track_reverse_link_worker(
    mgr: *mut HewConnMgr,
    worker: JoinHandle<()>,
) {
    if mgr.is_null() {
        let _ = worker.join();
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid for this call.
    let mgr_ref = unsafe { &*mgr };
    let finished = mgr_ref.reverse_link_workers.access(|workers| {
        let mut finished = Vec::new();
        let mut index = 0;
        while index < workers.len() {
            if workers[index].is_finished() {
                finished.push(workers.swap_remove(index));
            } else {
                index += 1;
            }
        }
        workers.push(worker);
        finished
    });
    for worker in finished {
        let _ = worker.join();
    }
}

/// Configure manager-wide reconnect policy.
///
/// Reconnect is disabled by default; call with `enabled=1` to opt in.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_set_reconnect_policy(
    mgr: *mut HewConnMgr,
    enabled: c_int,
    max_retries: c_int,
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_set_reconnect_policy: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.reconnect_enabled.store(enabled != 0, Ordering::Release);
    mgr.reconnect_max_retries
        .store(normalize_max_retries(max_retries), Ordering::Release);
    0
}

/// Configure per-connection reconnect target and retry policy.
///
/// Passing `enabled=0` disables reconnect for `conn_id`.
///
/// `expected_node_id` is the pinned peer `NodeId` from a `<node_id>@addr`
/// connect target, or `0` if the connect target was a bare address. `0` is
/// reserved (never a valid assigned node id, consistent with
/// [`hew_connmgr_expect_peer`]) and means "no pin" here: reconnects for this
/// connection stay permissive, matching the original bare-address dial.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
/// - `target_addr` must be a valid NUL-terminated C string when enabling.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_configure_reconnect(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target_addr: *const c_char,
    enabled: c_int,
    max_retries: c_int,
    expected_node_id: c_int,
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_configure_reconnect: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };

    // Validate target_addr before acquiring the lock to avoid holding the
    // connections lock while calling into C string parsing helpers.
    let target_owned: Option<String> = if enabled != 0 {
        // SAFETY: caller guarantees target_addr is a valid C string (or null).
        let Some(target) =
            (unsafe { crate::util::cstr_to_str(&target_addr, "hew_connmgr_configure_reconnect") })
        else {
            return -1;
        };
        if target.is_empty() {
            set_last_error("hew_connmgr_configure_reconnect: target_addr is empty");
            return -1;
        }
        Some(target.to_owned())
    } else {
        None
    };

    let retries = if max_retries > 0 {
        normalize_max_retries(max_retries)
    } else {
        mgr.reconnect_max_retries.load(Ordering::Acquire).max(1)
    };

    mgr.connections.access(|conns| {
        let Some(conn) = conns.iter_mut().find(|c| c.conn_id == conn_id) else {
            set_last_error(format!(
                "hew_connmgr_configure_reconnect: connection {conn_id} not found"
            ));
            return -1;
        };
        if enabled == 0 {
            conn.reconnect = None;
            return 0;
        }
        conn.reconnect = Some(ReconnectSettings {
            target_addr: target_owned.clone().unwrap_or_default(),
            max_retries: retries,
            expected_node_id: u16::try_from(expected_node_id).ok().filter(|&v| v != 0),
        });
        0
    })
}

/// Add a connection to the manager. Spawns a reader thread for inbound
/// messages.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
/// `conn_id` must be a valid connection ID from the transport.
#[no_mangle]
#[expect(
    clippy::too_many_lines,
    reason = "connection event loop handles all states"
)]
pub unsafe extern "C" fn hew_connmgr_add(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    if mgr.is_null() {
        // Cannot close conn_id: no transport pointer available.  This is API
        // misuse; the caller retains ownership of conn_id in this case only.
        set_last_error("hew_connmgr_add: manager is null");
        return -1;
    }
    // Ownership: hew_connmgr_add takes ownership of conn_id on entry.
    // On SUCCESS: ownership transfers to the manager (closed by remove/free).
    // On FAILURE (any path below): conn_id is closed here; callers must not
    // close it themselves after observing a -1 return.
    // Preserve the raw pointer before reborrowing — `SendConnMgr` needs
    // `*mut` for the reader thread, and round-tripping `&T → *mut T`
    // violates Rust aliasing rules.
    let mgr_ptr = mgr;
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let expected_peer_node_id = mgr
        .expected_peer_ids
        .access(|expected| expected.remove(&conn_id));

    if mgr.reconnect_shutdown.load(Ordering::Acquire) {
        // SAFETY: conn_id came from the transport and is not yet tracked by the manager.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error("hew_connmgr_add: manager is shutting down");
        return -1;
    }

    {
        let already_exists = mgr
            .connections
            .access(|conns| conns.iter().any(|c| c.conn_id == conn_id));
        if already_exists {
            // SAFETY: conn_id is a new transport handle that cannot be installed;
            // close it so the caller does not need to clean up on failure.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} already exists"
            ));
            return -1;
        }
    }

    #[cfg_attr(
        not(feature = "encryption"),
        allow(
            unused_mut,
            reason = "filled by copy_from_slice only in the encryption-gated keypair block"
        )
    )]
    let mut local_noise_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
    #[cfg(feature = "encryption")]
    let local_noise_private = {
        #[cfg(feature = "quic")]
        let skip_noise = {
            // SAFETY: mgr.transport is valid while the connection manager is alive.
            unsafe {
                crate::quic_transport::hew_transport_is_quic(mgr.transport)
                    || crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
            }
        };
        #[cfg(not(feature = "quic"))]
        let skip_noise = false;
        if skip_noise {
            Zeroizing::new(Vec::new())
        } else {
            let Ok(pattern) = NOISE_PATTERN.parse() else {
                // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
                unsafe { close_transport_conn(mgr.transport, conn_id) };
                set_last_error("hew_connmgr_add: invalid noise pattern");
                return -1;
            };
            if let Some(identity) = mgr.auth.noise_identity() {
                // Stable per-node Noise identity (issue #2652, D1): present the same
                // static key on every connection and across restarts so peers can
                // pin it via `Node::allow_peer`. Retires per-connection keypair
                // churn, which made a node's Noise public key unbindable.
                local_noise_pubkey.copy_from_slice(&identity.public());
                Zeroizing::new(identity.private().to_vec())
            } else {
                // No stable identity loaded (unconfigured / loopback-dev node): fall
                // back to an ephemeral keypair. Such a node is `Unverified`
                // (delivery-only); no peer pins its key, so churn is harmless.
                let builder = snow::Builder::new(pattern);
                let Ok(keypair) = builder.generate_keypair() else {
                    // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
                    unsafe { close_transport_conn(mgr.transport, conn_id) };
                    set_last_error("hew_connmgr_add: failed to generate noise keypair");
                    return -1;
                };
                local_noise_pubkey.copy_from_slice(&keypair.public);
                Zeroizing::new(keypair.private)
            }
        }
    };

    let (Some(local_identity), Some(local_session_incarnation)) =
        (mgr.local_identity, mgr.local_session_incarnation)
    else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(
            "hew_connmgr_add: v2 distributed handshake requires a loaded authenticated local identity and durable session",
        );
        return -1;
    };
    let local_hs = local_handshake(
        local_identity,
        local_session_incarnation,
        local_noise_pubkey,
    );
    // SAFETY: mgr.transport and conn_id are valid per caller contract; local_hs is stack-local.
    let Some(peer_hs) = (unsafe { handshake_exchange(mgr.transport, conn_id, local_hs) }) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        return -1;
    };
    if !peer_identity_compatible(local_identity, peer_hs.node_id) {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: peer NodeId {} collides with local NodeId for conn {conn_id}",
            peer_hs.node_id
        ));
        return -1;
    }

    // ── Per-connection posture (issue #2652, D1/D2/BLOCK-7) ───────────────
    // Classify the endpoint and derive posture from this manager's per-node
    // snapshot (never a process-global authority). A `Strict` connection must
    // resolve an authenticated credential bound to the claimed NodeId; an
    // `Unverified` connection (demonstrated-loopback dev, or the explicit
    // opt-out) is delivery-only. Credential-free posture rejects fire here,
    // before the credential is resolved:
    //   * an unconfigured node (no peer bindings or stable credential) on a
    //     non-loopback/Unknown endpoint has no way to authenticate the peer, so
    //     the strict connection is rejected rather than silently admitted;
    //   * a strict connection over a transport with no peer-credential channel
    //     (plain quic / stub / Unknown) is rejected fail-closed.
    // SAFETY: mgr.transport is valid while the manager is alive; conn_id is the
    // live handle being admitted.
    let remote_ip_class =
        unsafe { crate::transport::hew_transport_conn_remote_ip_class(mgr.transport, conn_id) };
    let posture = mgr.auth.posture_for(remote_ip_class);
    if posture != Posture::Strict {
        // v2 identity-bearing traffic is never admitted through the diagnostic
        // unverified posture.
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: v2 identity traffic requires authenticated strict posture (conn {conn_id})"
        ));
        return -1;
    }
    if posture == Posture::Strict {
        let unconfigured = !mgr.auth.has_bindings();
        if unconfigured {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: strict connection requires configured peer credentials (conn {conn_id})"
            ));
            return -1;
        }
        if remote_ip_class == crate::peer_binding::RemoteIpClass::Unknown {
            // Unknown transport class (plain quic / stub) has no peer-credential
            // mechanism — a strict admission cannot be authenticated.
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: strict binding unsupported on plain quic transport (use quic-mesh or tcp-noise) (conn {conn_id})"
            ));
            return -1;
        }
    }

    #[cfg(feature = "encryption")]
    let skip_noise = {
        #[cfg(feature = "quic")]
        {
            // QUIC provides TLS 1.3 encryption — skip Noise when using QUIC transport.
            // SAFETY: mgr.transport is valid while the connection manager is alive.
            unsafe {
                crate::quic_transport::hew_transport_is_quic(mgr.transport)
                    || crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
            }
        }
        #[cfg(not(feature = "quic"))]
        {
            false
        }
    };

    #[cfg(feature = "encryption")]
    let upgraded_noise = if !skip_noise
        && supports_encryption(local_hs.feature_flags)
        && supports_encryption(peer_hs.feature_flags)
    {
        // SAFETY: mgr.transport and conn_id are valid per caller contract;
        // local_hs, peer_hs, and local_noise_private are valid stack-local references.
        unsafe {
            upgrade_noise(
                mgr.transport,
                conn_id,
                &local_hs,
                &peer_hs,
                &local_noise_private,
            )
        }
    } else {
        None
    };

    // The authenticated peer credential this connection presents (issue #2652),
    // used to bind the claimed `NodeId` in the claim machine below. Populated
    // from the Noise static key for tcp-noise; mesh SPKI extraction is wired in
    // a later slice. `None` under `Unverified` posture (loopback / opt-out).
    #[cfg(feature = "encryption")]
    let mut peer_credential: Option<PeerCredential> = None;

    #[cfg(feature = "encryption")]
    let upgraded_noise = if !skip_noise
        && supports_encryption(local_hs.feature_flags)
        && supports_encryption(peer_hs.feature_flags)
    {
        let Some((noise, peer_static_pubkey)) = upgraded_noise else {
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: noise upgrade failed for conn {conn_id}"
            ));
            return -1;
        };
        if posture == Posture::Strict && !mgr.auth.noise_pubkey_allowlisted(&peer_static_pubkey) {
            // Per-node Noise pre-gate (issue #2652, D14): under strict posture
            // the peer's stable Noise static key must be bound in THIS node's
            // snapshot (via `allow_peer`) — not a process-global allowlist.
            // `authorize` below then binds the key to the *claimed* NodeId; this
            // pre-gate rejects an entirely unknown key early, before a claim is
            // reserved. Under `Unverified` posture (loopback dev / opt-out)
            // delivery is allowed without a binding.
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: peer key not allowlisted for conn {conn_id}"
            ));
            return -1;
        }
        if peer_hs.static_noise_pubkey != peer_static_pubkey {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: authenticated Noise key does not match the v2 handshake key for conn {conn_id}"
            ));
            return -1;
        }
        peer_credential = Some(PeerCredential::NoiseKey(peer_static_pubkey));
        Some(noise)
    } else {
        None
    };

    // Resolve the credential for the claim machine (issue #2652, D2/D6):
    //  - tcp-noise: the Noise static key recovered above (encryption build);
    //  - quic-mesh: the peer's leaf-certificate SPKI (D6) — the mTLS handshake
    //    already pinned the SPKI at the transport layer, so binding it here ties
    //    the *claimed* NodeId to that authenticated key (an allowlisted key must
    //    not claim a NodeId bound to a different key);
    //  - otherwise (plain tcp without encryption / plain quic / Unknown):
    //    credential-free (delivery-only / loopback dev).
    #[cfg(feature = "encryption")]
    let noise_credential: Option<PeerCredential> = peer_credential;
    #[cfg(not(feature = "encryption"))]
    let noise_credential: Option<PeerCredential> = None;

    #[cfg(feature = "quic")]
    // SAFETY: mgr.transport is valid while the manager is alive; conn_id is the
    // live handle being admitted. A non-mesh/unknown conn yields None.
    let peer_credential: Option<PeerCredential> = if unsafe {
        crate::quic_mesh::hew_transport_is_quic_mesh(mgr.transport)
    } {
        if peer_hs.static_noise_pubkey != [0; NOISE_STATIC_PUBKEY_LEN] {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                    "hew_connmgr_add: quic-mesh v2 handshake carried a non-zero Noise key for conn {conn_id}"
                ));
            return -1;
        }
        // SAFETY: same caller contract as above.
        unsafe { crate::quic_mesh::hew_transport_quic_mesh_peer_spki(mgr.transport, conn_id) }
            .map(PeerCredential::Spki)
    } else {
        noise_credential
    };
    #[cfg(not(feature = "quic"))]
    let peer_credential: Option<PeerCredential> = noise_credential;

    let Some(peer_credential) = peer_credential else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated transport credential unavailable for conn {conn_id}"
        ));
        return -1;
    };
    let derived_peer_identity = peer_credential.node_id();
    if derived_peer_identity != peer_hs.node_id {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated credential derives NodeId {derived_peer_identity}, not advertised {} (conn {conn_id})",
            peer_hs.node_id
        ));
        return -1;
    }
    let Some(peer_route_slot) = mgr.auth.route_slot_for_credential(&peer_credential) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: authenticated credential has no configured route slot (conn {conn_id})"
        ));
        return -1;
    };
    if expected_peer_node_id.is_some_and(|expected| expected != peer_route_slot) {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: peer route slot {peer_route_slot} does not match expected node id {} for conn {conn_id}",
            expected_peer_node_id.unwrap_or_default()
        ));
        return -1;
    }

    // Reserve the authenticated NodeId/session claim before any route, cluster,
    // registry, SWIM, monitor, link, or reply-table publication.
    let claim_token = next_publication_token(mgr);
    let superseded_claim: Option<LiveClaim> = match reserve_identity_claim(
        mgr,
        peer_hs.node_id,
        peer_route_slot,
        peer_hs.session_incarnation,
        Some(&peer_credential),
        conn_id,
        claim_token,
    ) {
        ClaimReservation::Reserved { superseded } => superseded,
        ClaimReservation::Rejected(detail) => {
            // SAFETY: mgr.transport and conn_id are valid per caller contract.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!("hew_connmgr_add: {detail}"));
            return -1;
        }
    };

    let mut actor = ConnectionActor::new(conn_id);
    actor.transport = mgr.transport;
    actor.publication_token = claim_token;
    // Stash the superseded claim on the actor so hew_connmgr_remove can
    // restore it if removal wins the race against publication (the local
    // `superseded_claim` below still feeds the publish path's close of the
    // superseded transport).
    actor
        .superseded_claim
        .lock_or_recover()
        .clone_from(&superseded_claim);
    actor.peer_node_id = peer_route_slot;
    actor.peer_identity = Some(peer_hs.node_id);
    actor.peer_session_incarnation = peer_hs.session_incarnation;
    actor.peer_feature_flags = peer_hs.feature_flags;
    actor.posture = posture;
    actor.credential = Some(peer_credential);
    #[cfg(feature = "encryption")]
    if let Some(noise) = upgraded_noise {
        let Ok(mut guard) = actor.noise_transport.lock() else {
            // Policy: per-connection state (C-ABI) — poisoned noise transport
            // means this connection's encryption state is corrupted.
            set_last_error("hew_connmgr_add: noise_transport mutex poisoned (a thread panicked)");
            return -1;
        };
        *guard = Some(noise);
    }
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);

    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::io_time::hew_now_ms() };
    actor.last_activity_ms.store(now, Ordering::Release);

    // I/O span: record accept span and stash context for reader_loop.
    // Fast path: atomic load only when tracing is disabled.
    crate::tracing::io_accept_span_begin(conn_id);

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = SendTransport(mgr.transport);
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);
    let mgr_send = SendConnMgr(mgr_ptr);
    let peer_feature_flags = actor.peer_feature_flags;
    let reader_lifecycle_guard = mgr.reader_lifecycle.register();
    #[cfg(feature = "encryption")]
    let noise_transport = Arc::clone(&actor.noise_transport);

    let handle = thread::Builder::new()
        .name(format!("hew-conn-{conn_id}"))
        .spawn(move || {
            let _reader_lifecycle_guard = reader_lifecycle_guard;
            reader_loop(
                mgr_send,
                transport_send,
                conn_id,
                claim_token,
                stop,
                activity_send,
                router,
                peer_feature_flags,
                #[cfg(feature = "encryption")]
                noise_transport,
            );
        });

    if let Ok(h) = handle {
        actor.reader_handle = Some(h);
    } else {
        // SAFETY: actor.transport is valid per caller contract of hew_connmgr_add.
        unsafe { actor.close_transport() };
        // Abort the reservation (issue #2652, D3 step 2): restore the superseded
        // claim (if any) or remove our Reserved entry, and wake any waiter.
        abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
        set_last_error(format!(
            "hew_connmgr_add: failed to spawn reader thread for conn {conn_id}"
        ));
        return -1;
    }

    let ConnectionInstallPublication {
        token: publication_token,
        sync: publication_sync,
        removed: publication_removed,
    } = match install_connection_actor(mgr, actor) {
        Ok(publication) => publication,
        Err(ConnectionInstallError::Shutdown) => {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            set_last_error(format!(
                "hew_connmgr_add: manager shutdown won install race for conn {conn_id}"
            ));
            return -1;
        }
        Err(ConnectionInstallError::Duplicate) => {
            abort_identity_claim(mgr, peer_hs.node_id, conn_id, claim_token, superseded_claim);
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} became duplicate during install"
            ));
            return -1;
        }
    };

    publish_identity_connection_established(
        mgr,
        peer_hs.node_id,
        peer_route_slot,
        peer_hs.session_incarnation,
        conn_id,
        peer_hs.feature_flags,
        publication_token,
        &publication_sync,
        &publication_removed,
        superseded_claim,
    );

    0
}

/// Pre-join admission-claim resolution for `hew_connmgr_remove` (see the
/// call site's comment): aborts a still-`Reserved` claim owned by this exact
/// admission (restoring `stashed_superseded`) and wakes any parked reader; a
/// `Published`-but-about-to-be-suppressed claim instead hands the stash back
/// for restoration AFTER retirement. Returns the claim to restore post-retire
/// (if any).
fn resolve_admission_claim_before_join(
    mgr: &HewConnMgr,
    peer_identity: Option<NodeId>,
    conn_id: c_int,
    publication_token: u64,
    stashed_superseded: Option<LiveClaim>,
) -> Option<LiveClaim> {
    let peer_identity = peer_identity?;
    let (lock, condvar) = &mgr.claims;
    let mut guard = lock.lock_or_recover();
    match guard.get(&peer_identity) {
        Some(claim) if claim.conn_id == conn_id && claim.publication_token == publication_token => {
            match claim.state {
                ClaimState::Reserved => {
                    match stashed_superseded {
                        Some(prev) => {
                            guard.insert(peer_identity, prev);
                        }
                        None => {
                            guard.remove(&peer_identity);
                        }
                    }
                    drop(guard);
                    condvar.notify_all();
                    None
                }
                ClaimState::Published => stashed_superseded,
                ClaimState::Retired => None,
            }
        }
        _ => None,
    }
}

/// Remove a connection from the manager and close it.
///
/// Returns 0 on success, -1 if not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
#[allow(
    clippy::too_many_lines,
    reason = "function handles the full connection teardown and claim retirement protocol"
)]
pub unsafe extern "C" fn hew_connmgr_remove(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_remove: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };

    // Phase 1: locate the connection and clone the publication handles — without
    // removing the conn from the list yet. Holding only Arc clones here so we
    // release the connections lock before calling into routing/cluster.
    let publication_data = mgr.connections.access(|conns| {
        let idx = conns.iter().position(|c| c.conn_id == conn_id);
        let Some(idx) = idx else {
            set_last_error(format!(
                "hew_connmgr_remove: connection {conn_id} not found"
            ));
            return None;
        };
        let conn = &conns[idx];
        Some((
            conn.peer_node_id,
            conn.peer_identity,
            conn.publication_token,
            Arc::clone(&conn.publication_sync),
            Arc::clone(&conn.publication_removed),
            Arc::clone(&conn.claimed_send_lifecycle),
        ))
    });
    let Some((
        peer_node_id,
        peer_identity,
        publication_token,
        publication_sync,
        publication_removed,
        claimed_send_lifecycle,
    )) = publication_data
    else {
        return -1;
    };

    // Step 2a: flag the publication as removed and remove the route from the
    // routing table — BEFORE removing the connection from mgr.connections.
    //
    // This eliminates the TOCTOU window where hew_routing_lookup returns
    // route-ok while hew_connmgr_send returns -1 (connection already gone
    // from the list). After the route is removed, any new routing lookup for
    // this peer returns no route, so no new hew_connmgr_send calls are
    // dispatched to this conn_id. Route removal is idempotent; the full
    // retire_connection_publication call at the end of this function repeats
    // the routing step (no-op second time) and then fires the cluster
    // notification — intentionally deferred so that a racing replacement
    // connection can publish first and suppress the notification via the
    // publication-token check.
    {
        let _publication = publication_sync.lock_or_recover();
        publication_removed.store(true, Ordering::Release);
        if !mgr.routing_table.is_null() {
            let Some(peer_identity) = peer_identity else {
                return -1;
            };
            // SAFETY: routing_table is valid per manager contract.
            let _ = unsafe {
                hew_routing_remove_route_if_conn(mgr.routing_table, peer_identity, conn_id)
            };
        }
    }

    // Step 2b: close the transport while the actor remains in the list. That
    // keeps a new admission from reusing this numeric slot until every claimed
    // deferred send has observed the close and released its lifecycle guard.
    let marked = mgr.connections.access(|connections| {
        let Some(connection) = connections.iter().find(|connection| {
            connection.conn_id == conn_id && connection.publication_token == publication_token
        }) else {
            return false;
        };
        connection.reader_stop.store(1, Ordering::Release);
        connection.transport_closed.store(true, Ordering::Release);
        true
    });
    if !marked {
        return 0;
    }
    // SAFETY: transport is valid per manager contract.
    unsafe { close_transport_conn(mgr.transport, conn_id) };
    claimed_send_lifecycle.wait_for_idle();

    // Remove only the exact closed publication. A second concurrent removal
    // finds it gone and returns harmlessly.
    let conn = mgr.connections.access(|conns| {
        let idx = conns.iter().position(|connection| {
            connection.conn_id == conn_id && connection.publication_token == publication_token
        })?;
        let conn = conns.swap_remove(idx);
        conn.state.store(CONN_STATE_CLOSED, Ordering::Release);
        Some(conn)
    });
    let Some(conn) = conn else {
        // Already removed by a concurrent hew_connmgr_remove call (reader re-entry).
        return 0;
    };

    // Resolve a still-pending admission BEFORE joining the reader. When removal
    // wins the race against `publish_connection_established`, the publication
    // early-returns on `publication_removed` without publishing or aborting,
    // and this connection's reader may be parked in the admission wait
    // (`wait_authenticated_peer_node_id_for_conn`) on the still-`Reserved`
    // claim — which only THIS abort (or the `CLAIM_RESERVE_WAIT_MS` backstop)
    // can resolve, because claim retirement runs after the join below. Abort
    // the reservation — restoring any claim this admission superseded — and
    // wake the waiter so the join cannot wedge on the backstop. Exact-owner
    // guarded (`conn_id` + token), so a successor's claim or an already-
    // aborted admission is untouched.
    //
    // If the claim is already PUBLISHED but our removal SUPPRESSES the
    // publication (`publication_removed` was set before the cluster notify),
    // the stashed predecessor claim is still live restorable state — the
    // publish path clears the stash only when publication completes. Carry it
    // past the retirement below and restore it there, so a same-credential
    // reconnect removed mid-publication hands authority back to the prior
    // connection instead of orphaning it claimless.
    let restore_after_retire = resolve_admission_claim_before_join(
        mgr,
        peer_identity,
        conn_id,
        publication_token,
        conn.superseded_claim.lock_or_recover().take(),
    );
    // Drop this admission's parked registry-flush retry frames (token-matched
    // so a successor's parked frames on a reused conn_id are never touched).
    mgr.pending_registry_flush.access(|map| {
        if map
            .get(&conn_id)
            .is_some_and(|pending| pending.token == publication_token)
        {
            map.remove(&conn_id);
        }
        mgr.pending_registry_flush_count
            .store(map.len(), Ordering::Release);
    });
    // Drop joins the reader thread after transport close.
    drop(conn);
    // Full publication retirement: re-removes route (idempotent) and fires the
    // cluster connection-lost notification. Deferred until after drop(conn) so
    // that a racing replacement connection can establish and publish first —
    // its higher publication token then suppresses this notification.
    retire_identity_connection_publication(
        mgr,
        peer_identity,
        peer_node_id,
        conn_id,
        publication_token,
        &publication_sync,
    );

    // Suppressed-publication restore (see the pre-join comment above): with our
    // claim retired, hand the node's authority back to the superseded prior
    // connection — guarded on an EMPTY slot so a racing fresh admission's
    // reservation is never overwritten.
    if let Some(prev) = restore_after_retire {
        let Some(peer_identity) = peer_identity else {
            return 0;
        };
        let (lock, condvar) = &mgr.claims;
        let mut guard = lock.lock_or_recover();
        match guard.entry(peer_identity) {
            std::collections::hash_map::Entry::Occupied(mut occupied)
                if occupied.get().state == ClaimState::Retired
                    && occupied.get().conn_id == conn_id
                    && occupied.get().publication_token == publication_token =>
            {
                occupied.insert(prev);
            }
            std::collections::hash_map::Entry::Vacant(vacant) => {
                vacant.insert(prev);
            }
            std::collections::hash_map::Entry::Occupied(_) => {}
        }
        drop(guard);
        condvar.notify_all();
    }

    0
}

/// Legacy API: outbound queue tuning is no longer supported.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_set_outbound_capacity(
    mgr: *mut HewConnMgr,
    _conn_id: c_int,
    _capacity: usize,
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_set_outbound_capacity: manager is null");
        return -1;
    }
    set_last_error(
        "hew_connmgr_set_outbound_capacity: outbound queue support was removed; sends are synchronous",
    );
    -1
}

/// Send a wire envelope over a specific connection.
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
/// - `target` must point to a valid exact actor location.
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_send(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target: *const HewLocation,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null(), -1);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    // SAFETY: caller guarantees `target` is readable.
    let Ok(target) = Location::try_from(unsafe { *target }) else {
        set_last_error("hew_connmgr_send: target location is invalid");
        return -1;
    };

    // Verify the connection is the active authenticated owner of this exact
    // node/session pair. This prevents conn-id and route-slot reuse from
    // redirecting a carried location.
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let publication_token = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && c.peer_identity == Some(target.node())
                    && c.peer_session_incarnation == target.incarnation()
            });
            #[cfg(feature = "encryption")]
            if let Some(c) = active {
                noise_out = Some(Arc::clone(&c.noise_transport));
            }
            active.map(|connection| connection.publication_token)
        });
        let Some(publication_token) = publication_token else {
            return -1;
        };
        let owns_claim = mgr_ref
            .claims
            .0
            .lock_or_recover()
            .get(&target.node())
            .is_some_and(|claim| {
                claim.state == ClaimState::Published
                    && claim.conn_id == conn_id
                    && claim.publication_token == publication_token
                    && claim.session_incarnation == target.incarnation()
            });
        if !owns_claim {
            return -1;
        }
        #[cfg(feature = "encryption")]
        {
            maybe_noise = noise_out;
        }
    }

    // SAFETY: data is valid for size bytes per caller contract of hew_connmgr_send.
    let Some(encoded) = (unsafe {
        encode_envelope(
            target,
            msg_type,
            data,
            size,
            MailboxPayloadClass::SerializedCrossNode as u8,
            0,
        )
    }) else {
        return -1;
    };

    #[cfg(feature = "encryption")]
    if let Some(noise_transport) = maybe_noise {
        let mut maybe_ciphertext = None;
        {
            let Ok(mut guard) = noise_transport.lock() else {
                // Policy: per-connection state (C-ABI) — poisoned noise transport
                // means this connection's encryption state is corrupted.
                set_last_error(
                    "hew_connmgr_send: noise_transport mutex poisoned (a thread panicked)",
                );
                return -1;
            };
            if let Some(noise) = guard.as_mut() {
                let mut ciphertext = vec![0u8; encoded.len() + 16];
                let Ok(n) = noise.write_message(&encoded, &mut ciphertext) else {
                    return -1;
                };
                ciphertext.truncate(n);
                maybe_ciphertext = Some(ciphertext);
            }
        }
        if let Some(ciphertext) = maybe_ciphertext {
            // SAFETY: mgr_ref.transport is valid per caller contract; conn_id verified active above.
            if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                return 0;
            }
            return -1;
        }
    }

    // SAFETY: mgr_ref.transport is valid per caller contract and conn_id is active.
    if unsafe { send_frame(mgr_ref.transport, conn_id, &encoded) } {
        0
    } else {
        -1
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "keeps claimed-slot validation atomic with encrypted/plaintext transport sends"
)]
unsafe fn send_preencoded_on_manager(
    mgr_ref: &HewConnMgr,
    conn_id: c_int,
    expected_publication_token: Option<u64>,
    data: *const u8,
    len: usize,
) -> c_int {
    if let Some(publication_token) = expected_publication_token {
        let lease = mgr_ref.connections.access(|connections| {
            let active = connections.iter().find(|connection| {
                connection.conn_id == conn_id
                    && connection.publication_token == publication_token
                    && connection.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
            })?;
            Some(ClaimedSendLease {
                _guard: active.claimed_send_lifecycle.register(),
                publication_removed: Arc::clone(&active.publication_removed),
                #[cfg(feature = "encryption")]
                noise_transport: Arc::clone(&active.noise_transport),
            })
        });
        let Some(lease) = lease else {
            return -1;
        };
        if lease.publication_removed.load(Ordering::Acquire) {
            return -1;
        }

        #[cfg(feature = "encryption")]
        {
            // SAFETY: data is valid for `len` bytes per caller contract.
            let slice = unsafe { std::slice::from_raw_parts(data, len) };
            let mut ciphertext = vec![0u8; len + 16];
            let mut guard = lease.noise_transport.lock_or_recover();
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.write_message(slice, &mut ciphertext) else {
                    return -1;
                };
                ciphertext.truncate(n);
                // SAFETY: teardown closes the connection before waiting for this
                // lease, so a blocked send is interrupted without slot reuse.
                return if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                    0
                } else {
                    -1
                };
            }
        }

        // SAFETY: transport remains valid while the manager-owned worker is
        // joined; the claimed lease prevents slot removal/reuse until return.
        let transport = unsafe { &*mgr_ref.transport };
        // SAFETY: `transport` remains valid for the manager lifetime.
        let rc = if let Some(ops) = unsafe { transport.ops.as_ref() } {
            if let Some(send_fn) = ops.send {
                // SAFETY: data is valid for `len` bytes per caller contract.
                unsafe { send_fn(transport.r#impl, conn_id, data.cast_mut().cast(), len) }
            } else {
                -1
            }
        } else {
            -1
        };
        return if rc > 0 { 0 } else { -1 };
    }

    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let ok = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && expected_publication_token.is_none_or(|token| c.publication_token == token)
            });
            #[cfg(feature = "encryption")]
            if let Some(c) = active {
                noise_out = Some(Arc::clone(&c.noise_transport));
            }
            active.is_some()
        });
        if !ok {
            return -1;
        }
        #[cfg(feature = "encryption")]
        {
            maybe_noise = noise_out;
        }
    }

    #[cfg(feature = "encryption")]
    if let Some(noise_arc) = maybe_noise {
        // SAFETY: data is valid for `len` bytes per caller contract.
        let slice = unsafe { std::slice::from_raw_parts(data, len) };
        let mut ciphertext = vec![0u8; len + 16];
        let mut guard = noise_arc.lock_or_recover();
        if let Some(noise) = guard.as_mut() {
            let Ok(n) = noise.write_message(slice, &mut ciphertext) else {
                return -1;
            };
            ciphertext.truncate(n);
            // SAFETY: transport is valid per manager contract; conn_id verified active above.
            return if unsafe { send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
                0
            } else {
                -1
            };
        }
        // No noise state — fall through to plaintext send.
    }

    // Plaintext send path.
    // SAFETY: transport is valid per manager contract.
    let t = unsafe { &*mgr_ref.transport };
    // SAFETY: ops pointer is part of valid transport.
    let rc = if let Some(ops) = unsafe { t.ops.as_ref() } {
        if let Some(send_fn) = ops.send {
            // SAFETY: data is valid for `len` bytes per caller contract; conn_id verified active.
            unsafe { send_fn(t.r#impl, conn_id, data.cast_mut().cast(), len) }
        } else {
            -1
        }
    } else {
        -1
    };
    if rc > 0 {
        0
    } else {
        -1
    }
}

/// Send a pre-encoded wire frame over a specific connection, applying noise
/// encryption when the connection is encrypted.
///
/// Unlike [`hew_connmgr_send`], this function takes bytes that are already
/// encoded into the Hew wire format (e.g., a full ask/reply envelope). The
/// only transformation applied is noise encryption (when enabled).
///
/// Returns 0 on success, -1 on failure.
///
/// # Safety
///
/// - `mgr` must be a valid pointer for the duration of the call.
/// - `data` must point to at least `len` readable bytes.
pub(crate) unsafe fn hew_connmgr_send_preencoded(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    data: *const u8,
    len: usize,
) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid for the duration of the call.
    let mgr_ref = unsafe { &*mgr };
    // SAFETY: forwarded caller contract.
    unsafe { send_preencoded_on_manager(mgr_ref, conn_id, None, data, len) }
}

/// Send a pre-encoded frame only if `conn_id` still names the exact published
/// connection incarnation identified by `publication_token`.
///
/// # Safety
///
/// `mgr` must be valid and `data` readable for `len` bytes.
pub(crate) unsafe fn hew_connmgr_send_preencoded_claimed(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    publication_token: u64,
    data: *const u8,
    len: usize,
) -> c_int {
    if mgr.is_null() || conn_id == HEW_CONN_INVALID || (data.is_null() && len > 0) {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` and `data` validity.
    unsafe { send_preencoded_on_manager(&*mgr, conn_id, Some(publication_token), data, len) }
}

/// Broadcast a registry-gossip control frame to active gossip-capable peers.
///
/// Returns the number of successful sends.
///
/// # Safety
///
/// `mgr` must be a valid connection manager pointer for the duration of the
/// call when non-null.
pub(crate) unsafe fn hew_connmgr_broadcast_registry_gossip(
    mgr: *mut HewConnMgr,
    name: &str,
    location: Location,
    is_add: bool,
) -> c_int {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees manager pointer validity.
    let mgr_ref = unsafe { &*mgr };
    let Some(bytes) = encode_registry_gossip_control(name, location, is_add) else {
        return 0;
    };

    let conn_ids = active_gossip_connection_ids(mgr_ref);
    // Publication tokens for FIFO parking: a broadcast frame for a connection
    // with an undelivered parked flush must queue BEHIND it — never overtake
    // it — so per-connection registry-event ordering (ADD before a newer
    // REMOVE of the same name, and vice versa) is preserved even across send
    // failures. A failed direct send parks the frame the same way.
    let tokens: HashMap<c_int, u64> = mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| conn_ids.contains(&c.conn_id))
            .map(|c| (c.conn_id, c.publication_token))
            .collect()
    });
    let mut success_count: c_int = 0;
    for conn_id in conn_ids {
        let Some(&token) = tokens.get(&conn_id) else {
            continue;
        };
        let has_parked = mgr_ref.pending_registry_flush_count.load(Ordering::Acquire) != 0
            && mgr_ref.pending_registry_flush.access(|map| {
                map.get(&conn_id)
                    .is_some_and(|pending| pending.token == token)
            });
        if has_parked {
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![bytes.clone()], 0, false);
            set_last_error(format!(
                "registry gossip broadcast parked behind an undelivered flush for conn {conn_id}"
            ));
            continue;
        }
        // SAFETY: manager is live and bytes is a complete encoded control frame.
        if unsafe {
            send_preencoded_on_manager(mgr_ref, conn_id, None, bytes.as_ptr(), bytes.len())
        } == 0
        {
            success_count += 1;
        } else {
            set_last_error(format!(
                "registry gossip broadcast send failed for conn {conn_id}; parked for retry"
            ));
            park_pending_registry_flush(mgr_ref, conn_id, token, vec![bytes.clone()], 0, false);
        }
    }
    success_count
}

/// Return the connection ID of the first active connection whose handshake
/// identified the peer as `node_id`. Returns -1 if not found.
///
/// Used by `send_reply_envelope` on the accepting side where `conn_by_node`
/// is not populated (only the initiating side calls `hew_node_connect`).
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_conn_id_for_node(mgr: *const HewConnMgr, node_id: u16) -> c_int {
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.conn_id;
            }
        }
        -1
    })
}

/// Return the negotiated feature flags for the active connection to `node_id`,
/// or `0` if no active connection exists.
///
/// Used by `node_inbound_router` to determine whether a peer understands the
/// ask-rejection sentinel before sending `HEW_REPLY_REJECT_MSG_TYPE`.
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
pub(crate) unsafe fn hew_connmgr_feature_flags_for_node(
    mgr: *const HewConnMgr,
    node_id: u16,
) -> u32 {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                return c.peer_feature_flags;
            }
        }
        0
    })
}

/// Overwrite the recorded feature flags for the active connection to `node_id`.
///
/// Test-only helper used to simulate an older peer after a normal handshake.
///
/// # Safety
///
/// `mgr` must be a valid pointer for the duration of the call.
#[cfg(test)]
pub(crate) unsafe fn hew_connmgr_force_peer_flags_for_node(
    mgr: *mut HewConnMgr,
    node_id: u16,
    flags: u32,
) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };
    mgr_ref.connections.access(|conns| {
        for c in conns.iter_mut() {
            if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
                c.peer_feature_flags = flags;
            }
        }
    });
}

/// Return the number of active connections.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_count(mgr: *mut HewConnMgr) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_count: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let count = mgr.connections.access(|conns| conns.len());
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "connection count will not exceed c_int range in practice"
    )]
    {
        count as c_int
    }
}

/// Send a message to all active connections.
///
/// Returns the number of successful sends.
///
/// # Safety
///
/// - `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
/// - `data` must point to at least `size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_broadcast(
    mgr: *mut HewConnMgr,
    target: *const HewLocation,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null() || target.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Collect active connection IDs under the lock.
    let conn_ids: Vec<c_int> = mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map(|c| c.conn_id)
            .collect()
    });

    let mut success_count: c_int = 0;
    for cid in conn_ids {
        // SAFETY: mgr is valid, data/size from caller.
        let rc = unsafe { hew_connmgr_send(mgr, cid, target, msg_type, data, size) };
        if rc == 0 {
            success_count += 1;
        }
    }

    success_count
}

/// Get the last activity timestamp (ms) for a connection.
///
/// Returns 0 if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_last_activity(mgr: *mut HewConnMgr, conn_id: c_int) -> u64 {
    cabi_guard!(mgr.is_null(), 0);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(0, |c| c.last_activity_ms.load(Ordering::Acquire))
    })
}

/// Get the state of a connection.
///
/// Returns [`CONN_STATE_CLOSED`] if the connection is not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_conn_state(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    cabi_guard!(mgr.is_null(), CONN_STATE_CLOSED);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id)
            .map_or(CONN_STATE_CLOSED, |c| c.state.load(Ordering::Acquire))
    })
}

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

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_node_id(route_slot: u16) -> NodeId {
        test_node_identity(route_slot)
    }

    fn test_location(route_slot: u16, actor_slot: u64) -> Location {
        Location::new(test_node_id(route_slot), actor_slot, 1)
            .expect("test locations use nonzero slots and incarnations")
    }

    fn test_node_session(route_slot: u16) -> crate::envelope::NodeSessionIdentity {
        crate::envelope::NodeSessionIdentity {
            node_id: test_node_id(route_slot),
            session_incarnation: 1,
        }
    }

    #[cfg(feature = "profiler")]
    #[test]
    fn snapshot_connections_json_emits_expected_array() {
        let mut active = ConnectionActor::new(7);
        active.peer_node_id = 42;
        active.state.store(CONN_STATE_ACTIVE, Ordering::Relaxed);
        active.last_activity_ms.store(123, Ordering::Relaxed);

        let mut draining = ConnectionActor::new(8);
        draining.peer_node_id = 9;
        draining.state.store(CONN_STATE_DRAINING, Ordering::Relaxed);
        draining.last_activity_ms.store(456, Ordering::Relaxed);

        let mgr = HewConnMgr {
            connections: PoisonSafe::new(vec![active, draining]),
            expected_peer_ids: PoisonSafe::new(HashMap::new()),
            transport: std::ptr::null_mut(),
            inbound_router: None,
            routing_table: std::ptr::null_mut(),
            cluster: std::ptr::null_mut(),
            reconnect_enabled: AtomicBool::new(false),
            reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
            reconnect_shutdown: Arc::new(AtomicBool::new(false)),
            inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
            inbound_ask_active: Arc::new(AtomicUsize::new(0)),
            reconnect_workers: PoisonSafe::new(Vec::new()),
            reverse_link_workers: PoisonSafe::new(Vec::new()),
            reader_lifecycle: Arc::new(ReaderLifecycle::default()),
            next_publication_token: AtomicU64::new(1),
            local_node_id: 0,
            local_identity: None,
            local_session_incarnation: None,
            auth: PeerAuthSnapshot::unconfigured(),
            claims: (
                std::sync::Mutex::new(std::collections::HashMap::new()),
                std::sync::Condvar::new(),
            ),
            pending_registry_flush: PoisonSafe::new(HashMap::new()),
            pending_registry_flush_count: AtomicUsize::new(0),
        };

        assert_eq!(
            snapshot_connections_json(&mgr),
            r#"[{"conn_id":7,"peer_node_id":42,"state":"active","last_activity_ms":123},{"conn_id":8,"peer_node_id":9,"state":"draining","last_activity_ms":456}]"#
        );
    }

    #[test]
    fn conn_actor_states() {
        let actor = ConnectionActor::new(0);
        assert_eq!(actor.state.load(Ordering::Relaxed), CONN_STATE_CONNECTING);
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Relaxed);
        assert_eq!(actor.state.load(Ordering::Relaxed), CONN_STATE_ACTIVE);
        actor.state.store(CONN_STATE_DRAINING, Ordering::Relaxed);
        assert_eq!(actor.state.load(Ordering::Relaxed), CONN_STATE_DRAINING);
        actor.state.store(CONN_STATE_CLOSED, Ordering::Relaxed);
        assert_eq!(actor.state.load(Ordering::Relaxed), CONN_STATE_CLOSED);
    }

    #[test]
    fn conn_actor_reader_stop_flag() {
        let actor = ConnectionActor::new(5);
        let stop = Arc::clone(&actor.reader_stop);
        assert_eq!(stop.load(Ordering::Relaxed), 0);
        stop.store(1, Ordering::Relaxed);
        assert_eq!(actor.reader_stop.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn peer_identity_validation_rejects_self_connections() {
        assert!(peer_identity_compatible(
            test_node_identity(10),
            test_node_identity(20)
        ));
        assert!(!peer_identity_compatible(
            test_node_identity(10),
            test_node_identity(10)
        ));
    }

    #[test]
    fn reconnect_plan_threads_expected_peer_identity() {
        let mut pinned = ConnectionActor::new(30);
        pinned.reconnect = Some(ReconnectSettings {
            target_addr: "127.0.0.1:1".into(),
            max_retries: 3,
            expected_node_id: Some(7),
        });
        let mut bare = ConnectionActor::new(31);
        bare.reconnect = Some(ReconnectSettings {
            target_addr: "127.0.0.1:1".into(),
            max_retries: 3,
            expected_node_id: None,
        });
        let mgr = HewConnMgr {
            connections: PoisonSafe::new(vec![pinned, bare]),
            expected_peer_ids: PoisonSafe::new(HashMap::new()),
            transport: std::ptr::null_mut(),
            inbound_router: None,
            routing_table: std::ptr::null_mut(),
            cluster: std::ptr::null_mut(),
            reconnect_enabled: AtomicBool::new(true),
            reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
            reconnect_shutdown: Arc::new(AtomicBool::new(false)),
            inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
            inbound_ask_active: Arc::new(AtomicUsize::new(0)),
            reconnect_workers: PoisonSafe::new(Vec::new()),
            reverse_link_workers: PoisonSafe::new(Vec::new()),
            reader_lifecycle: Arc::new(ReaderLifecycle::default()),
            next_publication_token: AtomicU64::new(1),
            local_node_id: 1,
            local_identity: None,
            local_session_incarnation: None,
            auth: PeerAuthSnapshot::unconfigured(),
            claims: (
                std::sync::Mutex::new(std::collections::HashMap::new()),
                std::sync::Condvar::new(),
            ),
            pending_registry_flush: PoisonSafe::new(HashMap::new()),
            pending_registry_flush_count: AtomicUsize::new(0),
        };

        assert_eq!(
            reconnect_plan(&mgr, 30)
                .expect("pinned connection should have a reconnect plan")
                .expected_node_id,
            Some(7),
            "a pinned <node_id>@addr target must thread its expected_node_id into the plan"
        );
        assert_eq!(
            reconnect_plan(&mgr, 31)
                .expect("bare-address connection should have a reconnect plan")
                .expected_node_id,
            None,
            "a bare-address target must remain unpinned in the plan"
        );
    }

    /// Drives [`reconnect_attempt`] same-thread against a stub transport whose
    /// peer handshake always claims `node_id == 20`, so the result depends
    /// entirely on whether `expected_node_id` was replayed into
    /// `hew_connmgr_expect_peer` before `hew_connmgr_add`.
    #[expect(
        clippy::too_many_lines,
        reason = "test stages a full stub-transport connmgr install in one place"
    )]
    fn run_reconnect_attempt_replay_case(
        expected_node_id: Option<u16>,
    ) -> (ReconnectAttemptOutcome, Vec<c_int>, c_int, String) {
        unsafe extern "C" fn stub_connect(
            _impl_ptr: *mut std::ffi::c_void,
            _addr: *const c_char,
        ) -> c_int {
            91
        }

        unsafe extern "C" fn stub_send(
            _impl_ptr: *mut std::ffi::c_void,
            _conn_id: c_int,
            _data: *const std::ffi::c_void,
            len: usize,
        ) -> c_int {
            #[expect(
                clippy::cast_possible_wrap,
                clippy::cast_possible_truncation,
                reason = "test payload length fits c_int"
            )]
            {
                len as c_int
            }
        }

        unsafe extern "C" fn stub_recv(
            _impl_ptr: *mut std::ffi::c_void,
            _conn_id: c_int,
            buf: *mut std::ffi::c_void,
            len: usize,
        ) -> c_int {
            let peer_hs =
                local_handshake(test_node_identity(20), 1, [0u8; NOISE_STATIC_PUBKEY_LEN]);
            let encoded = peer_hs.serialize();
            if len != encoded.len() {
                // A read past the initial handshake (e.g. a post-identity-gate
                // Noise handshake message under the `encryption` feature) is
                // not modeled by this stub: fail it explicitly so the caller
                // sees a transport/upgrade failure, never a length mismatch
                // on a buffer this stub does not own.
                return -1;
            }
            // SAFETY: buf is valid for len bytes; len matches encoded's length above.
            unsafe { std::ptr::copy_nonoverlapping(encoded.as_ptr(), buf.cast::<u8>(), len) };
            #[expect(
                clippy::cast_possible_wrap,
                clippy::cast_possible_truncation,
                reason = "test payload length fits c_int"
            )]
            {
                len as c_int
            }
        }

        unsafe extern "C" fn stub_close(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Mutex<Vec<c_int>> as the transport impl payload.
            let closed = unsafe { &*(impl_ptr.cast::<Mutex<Vec<c_int>>>()) };
            closed
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(conn_id);
        }

        let closed = Box::into_raw(Box::new(Mutex::new(Vec::<c_int>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: Some(stub_connect),
            listen: None,
            accept: None,
            send: Some(stub_send),
            recv: Some(stub_recv),
            close_conn: Some(stub_close),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: closed.cast::<std::ffi::c_void>(),
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the lifetime of the manager in this test.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                1,
            )
        };
        assert!(!mgr.is_null());

        crate::hew_clear_error();
        let plan = ReconnectPlan {
            target_addr: "127.0.0.1:1".into(),
            max_retries: 3,
            expected_node_id,
        };
        let outcome = reconnect_attempt(mgr, &plan, 10, 1);
        // SAFETY: mgr remains valid until the free call below.
        let count = unsafe { hew_connmgr_count(mgr) };
        let error_message = {
            let ptr = crate::hew_last_error();
            if ptr.is_null() {
                String::new()
            } else {
                // SAFETY: hew_last_error returns either null or a valid,
                // NUL-terminated C string owned by this thread's LAST_ERROR.
                unsafe { std::ffi::CStr::from_ptr(ptr) }
                    .to_str()
                    .expect("hew_last_error must be valid UTF-8")
                    .to_owned()
            }
        };

        // SAFETY: test-owned pointers remain valid until this cleanup completes.
        let closed_ids = unsafe {
            hew_connmgr_free(mgr);
            let closed_ids = Box::from_raw(closed)
                .into_inner()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            drop(Box::from_raw(transport_ptr));
            closed_ids
        };
        drop(ops);

        (outcome, closed_ids, count, error_message)
    }

    #[test]
    fn reconnect_attempt_requires_authenticated_credential_before_pin_check() {
        // A v2 reconnect cannot reach the route-slot pin check without a
        // transport-authenticated credential.
        let (outcome, closed_ids, count, error_message) =
            run_reconnect_attempt_replay_case(Some(7));
        assert_eq!(
            outcome,
            ReconnectAttemptOutcome::Rejected,
            "a reconnect pinned to node 7 must reject a peer claiming node 20"
        );
        assert_eq!(
            count, 0,
            "a rejected reconnect attempt must not install a connection"
        );
        assert_eq!(
            closed_ids,
            vec![91],
            "the rejected reconnect's transport connection must be closed"
        );
        assert!(
            error_message.contains("requires configured peer credentials"),
            "rejection must surface the credential gate, got: {error_message}"
        );

        // A bare-address reconnect is rejected at the same credential gate.
        let (_, _, _, error_message) = run_reconnect_attempt_replay_case(None);
        assert!(
            error_message.contains("requires configured peer credentials"),
            "an unpinned reconnect must still require authentication, got: {error_message}"
        );
    }

    #[test]
    fn link_controls_from_wrong_authenticated_peer_are_rejected() {
        let _guard = crate::runtime_test_guard();
        let mut peer = ConnectionActor::new(10);
        peer.peer_node_id = 2;
        peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        let mgr = HewConnMgr {
            connections: PoisonSafe::new(vec![peer]),
            expected_peer_ids: PoisonSafe::new(HashMap::new()),
            transport: std::ptr::null_mut(),
            inbound_router: None,
            routing_table: std::ptr::null_mut(),
            cluster: std::ptr::null_mut(),
            reconnect_enabled: AtomicBool::new(false),
            reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
            reconnect_shutdown: Arc::new(AtomicBool::new(false)),
            inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
            inbound_ask_active: Arc::new(AtomicUsize::new(0)),
            reconnect_workers: PoisonSafe::new(Vec::new()),
            reverse_link_workers: PoisonSafe::new(Vec::new()),
            reader_lifecycle: Arc::new(ReaderLifecycle::default()),
            next_publication_token: AtomicU64::new(1),
            local_node_id: 1,
            local_identity: None,
            local_session_incarnation: None,
            auth: PeerAuthSnapshot::unconfigured(),
            claims: (
                std::sync::Mutex::new(std::collections::HashMap::new()),
                std::sync::Condvar::new(),
            ),
            pending_registry_flush: PoisonSafe::new(HashMap::new()),
            pending_registry_flush_count: AtomicUsize::new(0),
        };
        let mgr_ptr = std::ptr::from_ref(&mgr).cast_mut();
        // No claim exists for conn 10; any reader token denies immediately.
        let conn10_token = 1_u64;
        let payload = crate::envelope::LinkReqPayload {
            linker: test_location(9, 88),
            ref_id: 123,
            target: test_location(1, 77),
            policy_tag: 1,
            reciprocate: 0,
            setup_id: 456,
        };

        let link_req = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_LINK_REQ,
            payload: crate::envelope::encode_link_req_payload(&payload)
                .expect("link request payload must encode"),
        };
        handle_control_frame(mgr_ptr, 0, 10, conn10_token, &link_req);
        assert!(
            crate::runtime::rt_current()
                .monitors
                .take_remote_watchers(payload.target.slot())
                .is_empty(),
            "a link request claiming another node must not register a watcher"
        );

        crate::runtime::rt_current()
            .monitors
            .register_remote_link_watcher_for_test(
                payload.target.slot(),
                test_location(2, payload.linker.slot()),
                payload.ref_id,
            );
        let unlink = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_UNLINK,
            payload: crate::envelope::encode_link_req_payload(&payload)
                .expect("unlink payload must encode"),
        };
        handle_control_frame(mgr_ptr, 0, 10, conn10_token, &unlink);

        let watchers = crate::runtime::rt_current()
            .monitors
            .take_remote_watchers(payload.target.slot());
        assert_eq!(
            watchers.len(),
            1,
            "mismatched unlink must not remove watcher"
        );
        assert_eq!(watchers[0].watcher.node(), test_node_id(2));
        assert_eq!(watchers[0].ref_id, payload.ref_id);
        assert!(watchers[0].is_link);
    }

    #[test]
    fn ask_rejection_reply_requires_negotiated_feature_flag() {
        assert!(is_ask_rejection_reply(
            crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE,
            HEW_FEATURE_SUPPORTS_ASK_REJECTION
        ));
        assert!(
            !is_ask_rejection_reply(crate::hew_node::HEW_REPLY_REJECT_MSG_TYPE, 0),
            "sentinel replies from peers without the feature bit must stay on the normal reply path"
        );
        assert!(
            !is_ask_rejection_reply(0, HEW_FEATURE_SUPPORTS_ASK_REJECTION),
            "normal replies must not be reclassified as rejections"
        );
    }

    unsafe extern "C" fn record_registry_gossip_send(
        impl_ptr: *mut std::ffi::c_void,
        conn_id: c_int,
        data: *const std::ffi::c_void,
        len: usize,
    ) -> c_int {
        // SAFETY: test installs a Mutex<Vec<_>> as the transport impl payload.
        let sends = unsafe { &*(impl_ptr.cast::<Mutex<Vec<(c_int, Vec<u8>)>>>()) };
        // SAFETY: send_preencoded_on_manager passes an encoded frame valid for len bytes.
        let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec();
        sends
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push((conn_id, bytes));
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_possible_wrap,
            reason = "test payload lengths fit c_int"
        )]
        {
            len as c_int
        }
    }

    #[test]
    fn registry_gossip_broadcast_targets_only_active_gossip_peers() {
        let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(record_registry_gossip_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: sends.cast(),
        }));

        // SAFETY: test-owned pointers remain valid until the explicit cleanup below.
        unsafe {
            let mgr = hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                1,
            );
            assert!(!mgr.is_null());

            let mut gossip_peer = ConnectionActor::new(10);
            gossip_peer.peer_node_id = 2;
            gossip_peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            gossip_peer.posture = crate::peer_binding::Posture::Strict;
            gossip_peer
                .state
                .store(CONN_STATE_ACTIVE, Ordering::Release);

            let mut old_peer = ConnectionActor::new(11);
            old_peer.peer_node_id = 3;
            old_peer.peer_feature_flags = 0;
            old_peer.posture = crate::peer_binding::Posture::Strict;
            old_peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);

            let mut draining_peer = ConnectionActor::new(12);
            draining_peer.peer_node_id = 4;
            draining_peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            draining_peer.posture = crate::peer_binding::Posture::Strict;
            draining_peer
                .state
                .store(CONN_STATE_DRAINING, Ordering::Release);

            (&*mgr).connections.access(|conns| {
                conns.push(gossip_peer);
                conns.push(old_peer);
                conns.push(draining_peer);
            });
            // issue #2652 (item 2): outbound gossip goes only to authenticated
            // (`Strict` + published-owner) peers, so each candidate needs a
            // published claim. Only conn 10 also supports gossip and is ACTIVE,
            // so it remains the sole recipient.
            test_publish_claim(&*mgr, 2, 10);
            test_publish_claim(&*mgr, 3, 11);
            test_publish_claim(&*mgr, 4, 12);

            let location = test_location(2, 0x42);
            assert_eq!(
                hew_connmgr_broadcast_registry_gossip(mgr, "worker", location, true),
                1
            );

            {
                let sends_ref = &*sends;
                let sends_guard = sends_ref
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(sends_guard.len(), 1);
                assert_eq!(sends_guard[0].0, 10);
                let WireFrame::Control(control) =
                    decode_wire_frame(&sends_guard[0].1).expect("control frame")
                else {
                    panic!("registry gossip broadcast must send a control frame");
                };
                assert_eq!(control.ctrl_kind, CTRL_REGISTRY_GOSSIP);
                let payload =
                    decode_registry_gossip_payload(&control.payload).expect("gossip payload");
                assert_eq!(payload.op, crate::cluster::GOSSIP_REGISTRY_ADD);
                assert_eq!(payload.name, "worker");
                assert_eq!(payload.location, test_location(2, 0x42));
            }

            hew_connmgr_free(mgr);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(sends));
        }
        drop(ops);
    }

    /// Build a SWIM control frame as it would arrive on the wire.
    fn swim_control_frame_bytes(payload: &SwimControlPayload) -> Vec<u8> {
        encode_swim_control(payload).expect("swim control frame should encode")
    }

    /// A PING SWIM frame is answered with an ACK, and the cross-attribution
    /// guard rejects a frame whose `from` identity does not match the handshake
    /// identity and session of the connection it arrived on.
    #[test]
    fn swim_ping_is_acked_and_cross_attribution_is_rejected() {
        let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(record_registry_gossip_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: sends.cast(),
        }));

        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Active gossip-capable peer node 2 on conn 10.
            let mut peer = ConnectionActor::new(10);
            peer.peer_node_id = 2;
            peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            peer.posture = Posture::Strict;
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));
            // Model node 2 as the admitted, published owner of its NodeId so the
            // issue #2652 exact-owner control gate accepts its SWIM frames.
            let conn10_token = test_publish_claim(&*mgr, 2, 10);

            // Honest PING from node 2 (matches the conn's authenticated id).
            let ping = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_PING,
                from: test_node_session(2),
                incarnation: 1,
                target: None,
                gossip: vec![],
            };
            let frame = decode_wire_frame(&swim_control_frame_bytes(&ping)).expect("frame");
            let WireFrame::Control(control) = frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, conn10_token, &control);

            // An ACK must have been sent back on conn 10.
            {
                let guard = (&*sends)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(guard.len(), 1, "PING must produce exactly one ACK send");
                assert_eq!(guard[0].0, 10);
                let WireFrame::Control(ctrl) = decode_wire_frame(&guard[0].1).expect("ack frame")
                else {
                    panic!("ack must be a control frame");
                };
                assert_eq!(ctrl.ctrl_kind, CTRL_SWIM);
                let ack = decode_swim_payload(&ctrl.payload).expect("ack payload");
                assert_eq!(ack.msg_type, crate::cluster::SWIM_MSG_ACK);
                assert_eq!(
                    ack.from,
                    test_node_session(1),
                    "ACK is stamped with our local node identity and session"
                );
            }

            // Spoofed PING claiming to be node 9 on node 2's connection is
            // rejected — no further send.
            let spoof = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_PING,
                from: test_node_session(9),
                incarnation: 1,
                target: None,
                gossip: vec![],
            };
            let spoof_frame = decode_wire_frame(&swim_control_frame_bytes(&spoof)).expect("frame");
            let WireFrame::Control(spoof_control) = spoof_frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(
                mgr,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                10,
                conn10_token,
                &spoof_control,
            );
            {
                let guard = (&*sends)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(
                    guard.len(),
                    1,
                    "cross-attributed PING must be dropped without an ACK"
                );
            }

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(sends));
        }
        drop(ops);
    }

    /// An inbound SWIM frame carrying piggybacked DEAD gossip about a third
    /// node folds that transition into the local membership view (C6 import).
    #[test]
    fn swim_frame_imports_piggybacked_gossip() {
        let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(record_registry_gossip_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: sends.cast(),
        }));

        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            // Pre-seed node 3 as ALIVE so the gossip can transition it.
            let addr = std::ffi::CString::new("10.0.0.3:9000").unwrap();
            crate::cluster::hew_cluster_join(cluster, 3, addr.as_ptr());

            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            let mut peer = ConnectionActor::new(10);
            peer.peer_node_id = 2;
            peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            peer.posture = Posture::Strict;
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));
            // Model node 2 as the admitted, published owner of its NodeId so the
            // issue #2652 exact-owner control gate accepts its gossip.
            let conn10_token = test_publish_claim(&*mgr, 2, 10);

            // ACK from node 2 carrying DEAD-about-node-3 gossip.
            let ack = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_ACK,
                from: test_node_session(2),
                incarnation: 1,
                target: None,
                gossip: vec![SwimGossipEntry {
                    member: test_node_session(3),
                    state: crate::cluster::MEMBER_DEAD,
                    incarnation: 5,
                }],
            };
            let frame = decode_wire_frame(&swim_control_frame_bytes(&ack)).expect("frame");
            let WireFrame::Control(control) = frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, conn10_token, &control);

            // Node 3 must now be DEAD in our membership view.
            assert_eq!(
                crate::cluster::hew_cluster_member_state(cluster, 3),
                crate::cluster::MEMBER_DEAD,
                "piggybacked DEAD gossip must transition node 3 to DEAD"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(sends));
        }
        drop(ops);
    }

    /// Issue #2652 D9/D12 `unverified_gating`: an `Unverified` (delivery-only)
    /// peer carries no control-plane authority. A SWIM frame it sends — even one
    /// honestly attributed to its own declared `NodeId` — is dropped with no
    /// ACK, and the [`inbound_ask_denied_unverified`] data-plane gate denies its
    /// asks while still permitting fire-and-forget delivery.
    #[test]
    fn unverified_gating_denies_control_and_ask_authority() {
        let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(record_registry_gossip_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: sends.cast(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Unverified (loopback-dev / opt-out) peer node 2 on conn 10:
            // ACTIVE, gossip-capable, but posture is Unverified.
            let mut unverified = ConnectionActor::new(10);
            unverified.peer_node_id = 2;
            unverified.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            unverified.posture = crate::peer_binding::Posture::Unverified;
            unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(unverified));
            // Its delivery claim is published (Unverified peers still route
            // fire-and-forget), but posture keeps it off the control plane.
            let conn10_token = test_publish_claim(&*mgr, 2, 10);

            // Control plane: an honest SWIM PING from the Unverified peer is
            // dropped — no ACK is sent.
            let ping = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_PING,
                from: test_node_session(2),
                incarnation: 1,
                target: None,
                gossip: vec![],
            };
            let frame = decode_wire_frame(&swim_control_frame_bytes(&ping)).expect("frame");
            let WireFrame::Control(control) = frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, conn10_token, &control);
            {
                let guard = (&*sends)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(
                    guard.len(),
                    0,
                    "an Unverified peer's SWIM frame must be dropped with no ACK"
                );
            }
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 10),
                0,
                "an Unverified connection has no control-plane authority"
            );

            // Data plane: an inbound ask (request_id > 0) from the Unverified
            // peer is denied; a fire-and-forget delivery (request_id == 0) is
            // permitted (D9's one intentional Unverified capability).
            assert!(
                inbound_ask_denied_unverified(mgr, 10, 1),
                "an inbound ask from an Unverified peer must be denied"
            );
            assert!(
                !inbound_ask_denied_unverified(mgr, 10, 0),
                "fire-and-forget delivery from an Unverified peer stays allowed"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(sends));
        }
        drop(ops);
    }

    /// Issue #2652 D9 exact-owner authority: an authorised (`Strict` +
    /// published) peer carries control-plane authority, and a **superseded**
    /// owner loses it the instant the claim map is overwritten — even though its
    /// posture is still `Strict` and its actor is not yet closed (D3 point 2).
    #[test]
    fn unverified_gating_supersede_revokes_authority() {
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Exact-owner (Strict + published) peer node 3 on conn 20.
            let mut strict = ConnectionActor::new(20);
            strict.peer_node_id = 3;
            strict.posture = crate::peer_binding::Posture::Strict;
            strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(strict));
            test_publish_claim(&*mgr, 3, 20);
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 20),
                3,
                "an exact-owner Strict connection carries control authority"
            );
            assert!(
                !inbound_ask_denied_unverified(mgr, 20, 1),
                "an inbound ask from an authorised peer is permitted"
            );

            // Supersede node 3's claim onto a new conn 21.
            let mut strict2 = ConnectionActor::new(21);
            strict2.peer_node_id = 3;
            strict2.posture = crate::peer_binding::Posture::Strict;
            strict2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(strict2));
            test_publish_claim(&*mgr, 3, 21);
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 20),
                0,
                "a superseded connection loses control authority"
            );
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 21),
                3,
                "the new exact owner carries control authority"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
        }
        drop(ops);
    }

    /// Install a Strict ACTIVE connection entry, as `install_connection_actor`
    /// does mid-admission. Split out so the pre-install-window test can defer
    /// it until after the gate is already waiting.
    fn install_strict_conn(mgr: &HewConnMgr, node_id: u16, conn_id: c_int, token: u64) {
        let mut strict = ConnectionActor::new(conn_id);
        strict.peer_node_id = node_id;
        strict.peer_identity = Some(test_node_identity(node_id));
        strict.peer_session_incarnation = 1;
        strict.publication_token = token;
        strict.posture = crate::peer_binding::Posture::Strict;
        strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        mgr.connections.access(|conns| conns.push(strict));
    }

    /// Shared scaffold for the admission-window (`Reserved → Published`) gate
    /// tests: a manager with one Strict ACTIVE connection whose claim is still
    /// `Reserved` by that connection, modelling the reader thread starting
    /// before `publish_connection_established` resolves the claim.
    fn with_reserved_strict_conn(
        node_id: u16,
        conn_id: c_int,
        token: u64,
        body: impl FnOnce(*mut HewConnMgr),
    ) {
        with_reserved_claim(node_id, conn_id, token, true, body);
    }

    /// As [`with_reserved_strict_conn`], but with `install` controlling whether
    /// the connection entry is pushed into `mgr.connections` up front. Passing
    /// `false` models the PRE-INSTALL admission window: `hew_connmgr_add`
    /// spawns the reader thread before `install_connection_actor`, so a frame
    /// can be gated while the claim is Reserved and the connections list does
    /// not yet contain the connection at all.
    fn with_reserved_claim(
        node_id: u16,
        conn_id: c_int,
        token: u64,
        install: bool,
        body: impl FnOnce(*mut HewConnMgr),
    ) {
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            if install {
                install_strict_conn(&*mgr, node_id, conn_id, token);
            }
            // Claim is Reserved (mid-admission), NOT yet Published.
            test_reserve_unverified(&*mgr, node_id, conn_id, token);

            body(mgr);

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
        }
        drop(ops);
    }

    /// Admission-window race (the lookup-unresolved CI failure): a control frame
    /// arriving while this connection's OWN claim is still `Reserved` must WAIT
    /// for the local publication and then be granted — not be dropped. The
    /// non-waiting gate denies inside the window (that instant deny permanently
    /// lost the peer's one-shot registry-gossip flush); the waiting gate blocks
    /// on the claims condvar until `publish_claim` fires and then grants.
    #[test]
    fn reserved_claim_same_conn_wait_grants_after_publish() {
        with_reserved_strict_conn(5, 30, 77, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                // Pre-fix behaviour: the non-waiting gate denies mid-window.
                assert_eq!(
                    authenticated_peer_node_id_for_conn(&*mgr, 30),
                    0,
                    "a Reserved claim carries no authority in the non-waiting gate"
                );
                let waiter = SendConnMgr(mgr);
                let handle = std::thread::spawn(move || {
                    let waiter = waiter;
                    // SAFETY: manager outlives the join below.
                    wait_authenticated_peer_node_id_for_conn(&*waiter.0, 30, 77)
                });
                // Give the waiter time to reach the condvar wait, then publish —
                // modelling `publish_connection_established` completing.
                std::thread::sleep(Duration::from_millis(50));
                assert!(publish_claim(&*mgr, 5, 30, 77), "publication must be ours");
                let granted = handle.join().expect("waiter thread");
                assert_eq!(
                    granted, 5,
                    "the waiting gate must grant once the local claim publishes"
                );
            }
        });
    }

    /// Fail-closed half of the admission-window wait: when the mid-window
    /// admission ABORTS (install failure), the waiter must wake and deny — the
    /// wait never fabricates authority for a connection that was never admitted.
    #[test]
    fn reserved_claim_same_conn_wait_denies_after_abort() {
        with_reserved_strict_conn(6, 40, 88, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                let waiter = SendConnMgr(mgr);
                let handle = std::thread::spawn(move || {
                    let waiter = waiter;
                    // SAFETY: manager outlives the join below.
                    wait_authenticated_peer_node_id_for_conn(&*waiter.0, 40, 88)
                });
                std::thread::sleep(Duration::from_millis(50));
                abort_claim(&*mgr, 6, 40, 88, None);
                let granted = handle.join().expect("waiter thread");
                assert_eq!(
                    granted, 0,
                    "an aborted admission must deny the waiting gate (fail-closed)"
                );
            }
        });
    }

    /// The wait applies ONLY to this connection's own in-flight admission: a
    /// claim `Reserved` by a DIFFERENT connection (a superseding admission) is
    /// denied immediately — no blocking, no authority (D3 point 2 preserved).
    #[test]
    fn reserved_claim_other_conn_denied_without_wait() {
        with_reserved_strict_conn(7, 50, 99, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                // A second Strict ACTIVE conn 51 for the same node id, with NO
                // claim of its own — node 7's claim is Reserved by conn 50.
                let mut other = ConnectionActor::new(51);
                other.peer_node_id = 7;
                other.posture = crate::peer_binding::Posture::Strict;
                other.state.store(CONN_STATE_ACTIVE, Ordering::Release);
                (&*mgr).connections.access(|conns| conns.push(other));

                let started = std::time::Instant::now();
                assert_eq!(
                    wait_authenticated_peer_node_id_for_conn(&*mgr, 51, 100),
                    0,
                    "a claim reserved by another connection must deny immediately"
                );
                assert!(
                    started.elapsed() < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                    "the other-owner denial must not consume the wait budget"
                );
            }
        });
    }

    /// Pre-install admission window (the residual lookup-unresolved race):
    /// `hew_connmgr_add` spawns the reader thread BEFORE
    /// `install_connection_actor`, so a frame can be gated while the claim is
    /// `Reserved` by this connection and the connections list does not yet
    /// contain the connection at all. The gate must WAIT (keyed on the claim,
    /// which reserve placed before the spawn), then grant once the install +
    /// publication complete — a connections-list-first gate denies here and
    /// permanently drops the peer's one-shot registry-gossip flush.
    #[test]
    fn reserved_claim_before_install_wait_grants_after_install_and_publish() {
        with_reserved_claim(8, 60, 111, false, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                let waiter = SendConnMgr(mgr);
                let handle = std::thread::spawn(move || {
                    let waiter = waiter;
                    // SAFETY: manager outlives the join below.
                    wait_authenticated_peer_node_id_for_conn(&*waiter.0, 60, 111)
                });
                // Give the waiter time to reach the condvar wait, then install
                // and publish — modelling `install_connection_actor` followed
                // by `publish_connection_established`.
                std::thread::sleep(Duration::from_millis(50));
                install_strict_conn(&*mgr, 8, 60, 111);
                assert!(publish_claim(&*mgr, 8, 60, 111), "publication must be ours");
                let granted = handle.join().expect("waiter thread");
                assert_eq!(
                    granted, 8,
                    "the waiting gate must grant once install + publication complete"
                );
            }
        });
    }

    /// Fail-closed half of the pre-install window: when the admission aborts
    /// before the connection is ever installed, the waiter must wake and deny —
    /// the wait never fabricates authority for a connection that was never
    /// admitted, installed, or published.
    #[test]
    fn reserved_claim_before_install_wait_denies_after_abort() {
        with_reserved_claim(9, 70, 122, false, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                let waiter = SendConnMgr(mgr);
                let handle = std::thread::spawn(move || {
                    let waiter = waiter;
                    // SAFETY: manager outlives the join below.
                    wait_authenticated_peer_node_id_for_conn(&*waiter.0, 70, 122)
                });
                std::thread::sleep(Duration::from_millis(50));
                abort_claim(&*mgr, 9, 70, 122, None);
                let granted = handle.join().expect("waiter thread");
                assert_eq!(
                    granted, 0,
                    "an admission aborted pre-install must deny the waiting gate"
                );
            }
        });
    }

    /// Generation separation on a REUSED transport `conn_id`: the TCP transport
    /// recycles slot indices (`store_conn` hands out the first free slot), so a
    /// stale reader — one still processing an already-read frame after its
    /// connection was removed — can observe a SUCCESSOR admission's claim under
    /// the same `conn_id`. The stale reader's gate carries its own (older)
    /// publication token and must deny immediately: it never waits on, and
    /// never adopts, the successor's authority, whether the successor's claim
    /// is still Reserved or already Published.
    #[test]
    fn stale_token_on_reused_conn_id_denied_without_wait() {
        with_reserved_strict_conn(11, 80, 200, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                // Successor mid-admission (claim Reserved, token 200): the stale
                // reader (token 150) must deny without consuming the wait budget.
                let started = std::time::Instant::now();
                assert_eq!(
                    wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 150),
                    0,
                    "a stale reader must not wait on a successor's Reserved claim"
                );
                assert!(
                    started.elapsed() < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                    "the stale-token denial must not consume the wait budget"
                );

                // Successor fully admitted (claim Published, token 200): the
                // stale reader still denies; the successor itself grants.
                assert!(
                    publish_claim(&*mgr, 11, 80, 200),
                    "publication must be ours"
                );
                assert_eq!(
                    wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 150),
                    0,
                    "a stale reader must not adopt a successor's published authority"
                );
                assert_eq!(
                    wait_authenticated_peer_node_id_for_conn(&*mgr, 80, 200),
                    11,
                    "the successor's own gate must grant after its publication"
                );
            }
        });
    }

    /// Removal racing publication must resolve the admission wait PROMPTLY —
    /// pinned with a REAL joined reader: the installed actor's `reader_handle`
    /// IS a thread parked in `wait_authenticated_peer_node_id_for_conn`, and
    /// `hew_connmgr_remove` JOINS it (via the actor drop). Without the
    /// pre-join claim abort the join blocks for the full
    /// `CLAIM_RESERVE_WAIT_MS` backstop and the wall-clock assertion fails;
    /// with it the abort wakes the parked reader to a fail-closed deny and
    /// remove returns promptly. Also pins the cleanup: the aborted
    /// reservation leaves no dangling claim to wedge a reconnect's
    /// `reserve_claim`.
    #[test]
    fn remove_during_admission_wait_resolves_promptly_and_cleans_claim() {
        with_reserved_claim(12, 90, 300, false, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                let (started_tx, started_rx) = std::sync::mpsc::channel();
                let (result_tx, result_rx) = std::sync::mpsc::channel();
                let waiter = SendConnMgr(mgr);
                let reader = std::thread::spawn(move || {
                    let waiter = waiter;
                    started_tx.send(()).expect("reader start signal");
                    // SAFETY: the manager outlives the join hew_connmgr_remove
                    // performs on this thread.
                    let granted = wait_authenticated_peer_node_id_for_conn(&*waiter.0, 90, 300);
                    result_tx.send(granted).expect("reader result");
                });

                // Install the actor with the parked thread as its REAL reader
                // handle, exactly what hew_connmgr_remove must join.
                let mut actor = ConnectionActor::new(90);
                actor.peer_node_id = 12;
                actor.peer_identity = Some(test_node_identity(12));
                actor.peer_session_incarnation = 1;
                actor.publication_token = 300;
                actor.posture = crate::peer_binding::Posture::Strict;
                actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
                actor.reader_handle = Some(reader);
                (*mgr).connections.access(|conns| conns.push(actor));

                started_rx.recv().expect("reader thread started");
                // Scheduling grace so the reader reaches the condvar wait; the
                // verdict is interleaving-independent (an un-parked reader
                // sees the aborted claim and denies just the same).
                std::thread::sleep(Duration::from_millis(50));

                let remove_started = std::time::Instant::now();
                assert_eq!(hew_connmgr_remove(mgr, 90), 0, "remove must succeed");
                let elapsed = remove_started.elapsed();
                assert!(
                    elapsed < Duration::from_millis(CLAIM_RESERVE_WAIT_MS / 2),
                    "remove (which joins the parked reader) must resolve at the \
                     abort, not the {CLAIM_RESERVE_WAIT_MS} ms backstop; took {elapsed:?}"
                );
                let granted = result_rx.recv().expect("reader result delivered");
                assert_eq!(
                    granted, 0,
                    "removal aborting the reservation must deny the waiting gate"
                );
                let (lock, _condvar) = &(*mgr).claims;
                assert!(
                    lock.lock_or_recover()
                        .get(&test_node_identity(12))
                        .is_none(),
                    "the aborted reservation must leave no dangling claim"
                );
            }
        });
    }

    /// The remove-side abort restores the same-credential claim this admission
    /// superseded (D3): when a reconnect's admission is removed before its
    /// publication, the PREVIOUS owner's `Published` claim returns to the map
    /// — authority falls back to the still-live prior connection instead of
    /// evaporating.
    #[test]
    fn remove_before_publication_restores_superseded_claim() {
        with_reserved_claim(13, 95, 400, true, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                // The original connection 95 is fully admitted (Published).
                assert!(publish_claim(&*mgr, 13, 95, 400), "original must publish");

                // A same-credential reconnect (conn 96) reserves, superseding
                // the published claim, and installs — but is removed before
                // `publish_connection_established` runs.
                let superseded = match reserve_claim(&*mgr, 13, None, 96, 500) {
                    ClaimReservation::Reserved { superseded } => {
                        superseded.expect("reconnect must supersede the published claim")
                    }
                    ClaimReservation::Rejected(detail) => {
                        panic!("same-credential reconnect must reserve: {detail}")
                    }
                };
                install_strict_conn(&*mgr, 13, 96, 500);
                (*mgr).connections.access(|conns| {
                    let conn = conns
                        .iter_mut()
                        .find(|c| c.conn_id == 96)
                        .expect("reconnect actor installed");
                    *conn.superseded_claim.lock_or_recover() = Some(superseded);
                });

                assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

                let (lock, _condvar) = &(*mgr).claims;
                let guard = lock.lock_or_recover();
                let restored = guard
                    .get(&test_node_identity(13))
                    .expect("superseded claim must be restored");
                assert_eq!(
                    restored.conn_id, 95,
                    "restored claim owner is the original conn"
                );
                assert_eq!(restored.publication_token, 400);
                assert_eq!(restored.state, ClaimState::Published);
            }
        });
    }

    /// Issue #2655: a connection's monitor/link retirement fan-out must honour
    /// the publication token. `retire_connection_publication` fires
    /// `fan_out_monitor_lost_for_node` (arming `MonitorLost` for every pending
    /// cross-node watcher of the peer) ONLY when the retiring connection is the
    /// current publication owner. A superseded connection — one whose claim was
    /// overwritten by a same-credential reconnect while the peer stays live under
    /// the newer claim — retires nothing and must NOT drive the loss fan-out: the
    /// peer is still reachable through its healthy replacement, so a `MonitorLost`
    /// there is a false loss.
    ///
    /// This drives `retire_connection_publication` directly against a registered
    /// watcher — the observation point is connection-level (a superseded vs owning
    /// retire of ONE peer's claim), which the §14 `sim_transport_property` harness
    /// cannot express because it is transport-seam-only and brings up no `HewNode`.
    /// A non-owner retire leaves the watcher pending; the subsequent owner retire
    /// arms it, proving the gate discriminates on ownership, not merely that the
    /// fan-out can fire.
    #[test]
    fn retire_fans_out_monitor_lost_only_for_the_owning_connection() {
        let _rt_guard = crate::runtime_test_guard();
        let rt = crate::runtime::rt_current_opt().expect("test guard installs a runtime");
        let peer: u16 = 42;
        let target = test_location(peer, 7);
        let _ref_id = rt
            .monitors
            .register_remote_monitor(target, 999)
            .expect("test monitor id");

        with_reserved_claim(peer, 95, 400, true, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                assert!(
                    publish_claim(&*mgr, peer, 95, 400),
                    "the owning connection's claim must publish"
                );
                let publication_sync = Arc::new(Mutex::new(()));

                // A superseded (non-owner) retire: `retire_claim` denies, so the
                // fan-out never runs and the watcher stays pending.
                retire_connection_publication(&*mgr, peer, 96, 500, &publication_sync);
                assert_eq!(
                    rt.monitors.pending_observation_count(),
                    1,
                    "a superseded (non-owner) connection's retire must NOT fan out \
                     MonitorLost while the peer is live under a newer claim"
                );

                // The owning connection's retire: `retire_claim` grants, so the
                // fan-out arms the pending watcher with MonitorLost.
                retire_connection_publication(&*mgr, peer, 95, 400, &publication_sync);
                assert_eq!(
                    rt.monitors.pending_observation_count(),
                    0,
                    "the owning connection's retire must fan out MonitorLost"
                );
            }
        });
    }

    /// Shared state for the failed-flush regression test: fails the first
    /// `fail_remaining` sends, records the rest.
    struct FailingOnceSends {
        fail_remaining: AtomicUsize,
        sends: Mutex<Vec<(c_int, Vec<u8>)>>,
    }

    unsafe extern "C" fn fail_once_then_record_send(
        impl_ptr: *mut std::ffi::c_void,
        conn_id: c_int,
        data: *const std::ffi::c_void,
        len: usize,
    ) -> c_int {
        // SAFETY: test installs a FailingOnceSends as the transport impl payload.
        let state = unsafe { &*(impl_ptr.cast::<FailingOnceSends>()) };
        if state
            .fail_remaining
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |n| n.checked_sub(1))
            .is_ok()
        {
            return -1;
        }
        // SAFETY: send_preencoded_on_manager passes an encoded frame valid for len bytes.
        let bytes = unsafe { std::slice::from_raw_parts(data.cast::<u8>(), len) }.to_vec();
        state
            .sends
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push((conn_id, bytes));
        #[expect(
            clippy::cast_possible_truncation,
            clippy::cast_possible_wrap,
            reason = "test payload lengths fit c_int"
        )]
        {
            len as c_int
        }
    }

    /// A transiently failed connection-establish gossip flush must not lose the
    /// cluster's registered names (the third lookup-unresolved mechanism): the
    /// failed frames are PARKED, a stale admission token cannot consume them,
    /// and the connection's next inbound-frame retry delivers them.
    #[test]
    fn failed_gossip_flush_parks_frames_and_retry_delivers() {
        let state = Box::into_raw(Box::new(FailingOnceSends {
            fail_remaining: AtomicUsize::new(1),
            sends: Mutex::new(Vec::new()),
        }));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(fail_once_then_record_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: state.cast(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            let mut peer = ConnectionActor::new(10);
            peer.peer_node_id = 2;
            peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            peer.posture = crate::peer_binding::Posture::Strict;
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));
            let token = test_publish_claim(&*mgr, 2, 10);

            (&*cluster).emit_registry_add("svc", test_location(1, 0x77));

            // Initial flush: the send fails; the frame must be parked, not lost.
            flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
            assert!(
                (*state)
                    .sends
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .is_empty(),
                "the failed initial send must record nothing"
            );
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                1,
                "the failed flush must park its frames"
            );

            // A stale admission token (a reused conn_id's old reader) must not
            // consume the parked frames.
            retry_pending_registry_flush(&*mgr, 10, token + 1);
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                1,
                "a stale token must not consume the parked flush"
            );

            // The connection's own retry (next inbound frame) delivers.
            retry_pending_registry_flush(&*mgr, 10, token);
            {
                let sends = (*state)
                    .sends
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(sends.len(), 1, "the retry must deliver the parked frame");
                assert_eq!(sends[0].0, 10);
                let WireFrame::Control(ctrl) =
                    decode_wire_frame(&sends[0].1).expect("delivered frame decodes")
                else {
                    panic!("delivered frame must be a control frame");
                };
                assert_eq!(ctrl.ctrl_kind, CTRL_REGISTRY_GOSSIP);
                let payload =
                    decode_registry_gossip_payload(&ctrl.payload).expect("gossip payload");
                assert_eq!(payload.name, "svc");
                assert_eq!(payload.op, REGISTRY_GOSSIP_OP_ADD);
            }
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                0,
                "a delivered retry must clear the parked entry"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(state));
        }
        drop(ops);
    }

    /// Remove racing a JUST-PUBLISHED claim whose publication our removal
    /// suppressed: the stashed predecessor claim must be restored after our
    /// claim's retirement — the prior same-credential connection regains
    /// authority instead of being orphaned claimless. (The publish path clears
    /// the stash when publication completes, so this restore can never
    /// resurrect a closed connection's claim.)
    #[test]
    fn remove_after_suppressed_publication_restores_superseded_claim() {
        with_reserved_claim(15, 95, 700, true, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                assert!(publish_claim(&*mgr, 15, 95, 700), "original must publish");

                // Same-credential reconnect: conn 96 reserves (superseding),
                // installs, and its claim reaches Published — but the cluster
                // publication is then SUPPRESSED by removal, so the stash is
                // never cleared.
                let superseded = match reserve_claim(&*mgr, 15, None, 96, 800) {
                    ClaimReservation::Reserved { superseded } => {
                        superseded.expect("reconnect must supersede the published claim")
                    }
                    ClaimReservation::Rejected(detail) => {
                        panic!("same-credential reconnect must reserve: {detail}")
                    }
                };
                install_strict_conn(&*mgr, 15, 96, 800);
                (*mgr).connections.access(|conns| {
                    let conn = conns
                        .iter_mut()
                        .find(|c| c.conn_id == 96)
                        .expect("reconnect actor installed");
                    *conn.superseded_claim.lock_or_recover() = Some(superseded);
                });
                assert!(
                    publish_claim(&*mgr, 15, 96, 800),
                    "reconnect claim publishes"
                );

                assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

                let (lock, _condvar) = &(*mgr).claims;
                let guard = lock.lock_or_recover();
                let restored = guard
                    .get(&test_node_identity(15))
                    .expect("predecessor claim must be restored");
                assert_eq!(restored.conn_id, 95);
                assert_eq!(restored.publication_token, 700);
                assert_eq!(restored.state, ClaimState::Published);
            }
        });
    }

    /// The counter-half: when the publication COMPLETES, the publish path
    /// clears the stash (and closes the superseded connection), so a later
    /// remove retains only the successor's retired replay fence — no resurrected
    /// authority for a closed predecessor.
    #[test]
    fn remove_after_completed_publication_restores_nothing() {
        with_reserved_claim(16, 95, 900, true, |mgr| {
            // SAFETY: mgr is live for the whole closure.
            unsafe {
                assert!(publish_claim(&*mgr, 16, 95, 900), "original must publish");
                let superseded = match reserve_claim(&*mgr, 16, None, 96, 1000) {
                    ClaimReservation::Reserved { superseded } => {
                        superseded.expect("reconnect must supersede the published claim")
                    }
                    ClaimReservation::Rejected(detail) => {
                        panic!("same-credential reconnect must reserve: {detail}")
                    }
                };
                let mut actor = ConnectionActor::new(96);
                actor.peer_node_id = 16;
                actor.publication_token = 1000;
                actor.posture = crate::peer_binding::Posture::Strict;
                actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
                *actor.superseded_claim.lock_or_recover() = Some(superseded.clone());
                let Ok(publication) = install_connection_actor(&*mgr, actor) else {
                    panic!("install must succeed");
                };
                // Full publication (not suppressed): clears the stash.
                publish_connection_established(
                    &*mgr,
                    16,
                    96,
                    0,
                    publication.token,
                    &publication.sync,
                    &publication.removed,
                    Some(superseded),
                );

                assert_eq!(hew_connmgr_remove(mgr, 96), 0, "remove must succeed");

                let (lock, _condvar) = &(*mgr).claims;
                let guard = lock.lock_or_recover();
                let retired = guard
                    .get(&test_node_identity(16))
                    .expect("completed publication must retain a replay fence");
                assert_eq!(retired.conn_id, 96);
                assert_eq!(retired.state, ClaimState::Retired);
            }
        });
    }

    /// Ordering safety across a failed flush (the stale-replay hazard): a
    /// broadcast REMOVE for a name whose ADD is still parked must queue BEHIND
    /// the parked ADD, and the retry must deliver both in original order — the
    /// receiver's final state is the REMOVE, never a replayed stale ADD.
    #[test]
    fn broadcast_parks_behind_undelivered_flush_preserving_order() {
        let state = Box::into_raw(Box::new(FailingOnceSends {
            fail_remaining: AtomicUsize::new(1),
            sends: Mutex::new(Vec::new()),
        }));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(fail_once_then_record_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: state.cast(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            let mut peer = ConnectionActor::new(10);
            peer.peer_node_id = 2;
            peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            peer.posture = crate::peer_binding::Posture::Strict;
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));
            let token = test_publish_claim(&*mgr, 2, 10);

            let location = test_location(1, 0x88);
            (&*cluster).emit_registry_add("svc", location);
            // The ADD flush fails and parks.
            flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                1,
                "the failed ADD must park"
            );

            // A later REMOVE broadcast must park BEHIND it, not overtake it.
            assert_eq!(
                hew_connmgr_broadcast_registry_gossip(mgr, "svc", location, false),
                0,
                "the REMOVE must not report a direct send while the ADD is parked"
            );
            assert!(
                (*state)
                    .sends
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .is_empty(),
                "nothing may reach the wire before the retry drains in order"
            );

            // The retry drains BOTH, in original order: ADD then REMOVE.
            retry_pending_registry_flush(&*mgr, 10, token);
            {
                let sends = (*state)
                    .sends
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                let ops_seen: Vec<u8> = sends
                    .iter()
                    .map(|(_, bytes)| {
                        let WireFrame::Control(ctrl) =
                            decode_wire_frame(bytes).expect("frame decodes")
                        else {
                            panic!("expected control frame");
                        };
                        decode_registry_gossip_payload(&ctrl.payload)
                            .expect("gossip payload")
                            .op
                    })
                    .collect();
                assert_eq!(
                    ops_seen,
                    vec![REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE],
                    "the drain must preserve ADD-before-REMOVE order"
                );
            }
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                0,
                "a full drain clears the parked entry"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(state));
        }
        drop(ops);
    }

    /// The retry budget is BOUNDED: after `MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS`
    /// failed drains the parked frames are dropped fail-closed (with a
    /// diagnostic), never retried forever.
    #[test]
    fn retry_attempts_ceiling_drops_parked_frames() {
        let state = Box::into_raw(Box::new(FailingOnceSends {
            fail_remaining: AtomicUsize::new(usize::MAX),
            sends: Mutex::new(Vec::new()),
        }));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(fail_once_then_record_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: state.cast(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            let mut peer = ConnectionActor::new(10);
            peer.peer_node_id = 2;
            peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            peer.posture = crate::peer_binding::Posture::Strict;
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));
            let token = test_publish_claim(&*mgr, 2, 10);

            (&*cluster).emit_registry_add("svc", test_location(1, 0x99));
            flush_registry_gossip_to_connection(&*mgr, 10, token, HEW_FEATURE_SUPPORTS_GOSSIP);
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                1
            );

            // Every retry fails; the ceiling must eventually drop the entry.
            for _ in 0..MAX_REGISTRY_FLUSH_RETRY_ATTEMPTS {
                retry_pending_registry_flush(&*mgr, 10, token);
                assert_eq!(
                    (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                    1,
                    "within budget, a failed drain re-parks"
                );
            }
            let _ = take_hew_last_error();
            retry_pending_registry_flush(&*mgr, 10, token);
            assert_eq!(
                (*mgr).pending_registry_flush_count.load(Ordering::Acquire),
                0,
                "the attempt over budget must drop the parked entry"
            );
            let diag = take_hew_last_error().expect("budget exhaustion must leave a diagnostic");
            assert!(
                diag.contains("retry budget exhausted"),
                "diagnostic must name the budget exhaustion, got: {diag}"
            );
            assert!(
                (*state)
                    .sends
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .is_empty(),
                "no frame ever reached the wire"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(state));
        }
        drop(ops);
    }

    /// Recorder for the real-order admission test: collects every registry
    /// event the cluster applies.
    extern "C" fn record_registry_apply(
        name: *const std::ffi::c_char,
        location: *const HewLocation,
        is_add: bool,
        user_data: *mut std::ffi::c_void,
    ) {
        // SAFETY: test installs a Mutex<Vec<_>> as the callback user data.
        let applied = unsafe { &*(user_data.cast::<Mutex<Vec<(String, Location, bool)>>>()) };
        // SAFETY: the cluster passes a valid NUL-terminated name.
        let name = unsafe { std::ffi::CStr::from_ptr(name) }
            .to_string_lossy()
            .into_owned();
        // SAFETY: cluster callback supplies a valid location pointer.
        let location = Location::try_from(unsafe { *location }).unwrap();
        applied
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push((name, location, is_add));
    }

    /// Real admission ordering through the production pieces (the full
    /// `hew_connmgr_add` path additionally requires a live Noise handshake to
    /// reach `Strict` posture, which only the two-process e2e suite drives;
    /// this test pins the same ordering seam with the real reserve / install /
    /// publish / gate / decode functions):
    /// `reserve_claim` → a registry-gossip control frame processed by
    /// `handle_control_frame` (real decode + waiting gate) → REAL
    /// `install_connection_actor` → REAL `publish_connection_established` →
    /// the cluster registry callback applies the name. The frame processing
    /// begins strictly BEFORE the install (channel-sequenced, plus a
    /// scheduling grace biasing it into the pre-install window), modelling
    /// the reader thread racing `hew_connmgr_add`; the gate must hold the
    /// frame through install + publication and then apply it — an instant
    /// deny loses the peer's one-shot flush and the name never resolves.
    #[test]
    fn control_frame_before_install_applies_registry_event_after_real_publish() {
        let applied = Box::into_raw(Box::new(Mutex::new(Vec::<(String, Location, bool)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());
        // SAFETY: cluster is live; the recorder's user data outlives it.
        unsafe {
            crate::cluster::hew_cluster_set_registry_callback(
                cluster,
                record_registry_apply,
                applied.cast(),
            );
        }

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Step 1 (real): reserve the claim, as hew_connmgr_add does before
            // spawning the reader.
            let token = next_publication_token(&*mgr);
            let ClaimReservation::Reserved { superseded: None } =
                reserve_claim(&*mgr, 5, None, 30, token)
            else {
                panic!("fresh reservation must succeed without supersession");
            };

            // Step 2 (real decode + gate): a reader surrogate processes the
            // peer's one-shot registry-gossip frame. It signals right before
            // handing the frame to the production dispatch.
            let location = test_location(5, 0x99);
            let payload = RegistryGossipPayload {
                op: crate::cluster::GOSSIP_REGISTRY_ADD,
                name: "svc".to_owned(),
                location: test_location(5, 0x99),
            };
            let frame_bytes = encode_control_frame(&ControlFrame {
                version: WIRE_VERSION,
                ctrl_kind: CTRL_REGISTRY_GOSSIP,
                payload: encode_registry_gossip_payload(&payload).expect("gossip payload encodes"),
            })
            .expect("gossip frame encodes");
            let WireFrame::Control(control) =
                decode_wire_frame(&frame_bytes).expect("frame decodes")
            else {
                panic!("expected a control frame");
            };
            let (started_tx, started_rx) = std::sync::mpsc::channel();
            let reader = SendConnMgr(mgr);
            let reader_handle = std::thread::spawn(move || {
                let reader = reader;
                started_tx.send(()).expect("reader start signal");
                // SAFETY: manager outlives the join below.
                handle_control_frame(reader.0, HEW_FEATURE_SUPPORTS_GOSSIP, 30, token, &control);
                crate::stream_error::take_last_error()
            });
            started_rx.recv().expect("reader surrogate started");
            // Scheduling grace biasing the frame into the PRE-INSTALL window;
            // the assertion is interleaving-independent (the gate is correct
            // for frames arriving anywhere in the admission window).
            std::thread::sleep(Duration::from_millis(50));

            // Steps 3+4 (real): install, then publish, exactly as
            // hew_connmgr_add does after spawning the reader.
            let mut actor = ConnectionActor::new(30);
            actor.peer_node_id = 5;
            actor.publication_token = token;
            actor.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            actor.posture = crate::peer_binding::Posture::Strict;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            let Ok(publication) = install_connection_actor(&*mgr, actor) else {
                panic!("install must succeed");
            };
            publish_connection_established(
                &*mgr,
                5,
                30,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication.token,
                &publication.sync,
                &publication.removed,
                None,
            );

            let reader_error = reader_handle.join().expect("reader surrogate");
            {
                let applied = (*applied)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert_eq!(
                    applied.as_slice(),
                    &[("svc".to_owned(), location, true)],
                    "the pre-install frame must apply exactly once after publication: {reader_error:?}",
                );
            }

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(applied));
        }
        drop(ops);
    }

    /// Issue #2652 (item 1, outbound authority): the predicate the outbound ask
    /// gate in `setup_remote_ask` applies — `authenticated_peer_node_id_for_conn(
    /// mgr, conn) == target_node_id` — must hold ONLY for the exact authenticated
    /// owner of `target_node_id`. This is the outbound symmetric of the inbound
    /// `inbound_ask_denied_unverified` gate: an `Unverified` (delivery-only) or a
    /// **superseded** connection routed to the target both resolve to `0`
    /// (`!= target_node_id`), so an outbound ask over either fails CLOSED with
    /// `AskError::Unauthorized` before serialization; only the exact owner clears
    /// the gate. This test pins the exact tri-state `setup_remote_ask` branches on
    /// at the authority it consults, so a regression in either the authority or
    /// the outbound wiring is caught here.
    #[test]
    fn outbound_ask_gate_authorizes_only_exact_owner() {
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // The predicate the outbound gate applies to a routed target connection.
        let clears_gate = |mgr: &HewConnMgr, conn_id: c_int, target: u16| -> bool {
            authenticated_peer_node_id_for_conn(mgr, conn_id) == target
        };

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // (a) Unverified target (node 2 on conn 10): the gate rejects — an
            // outbound ask here fails closed with Unauthorized.
            let mut unverified = ConnectionActor::new(10);
            unverified.peer_node_id = 2;
            unverified.posture = crate::peer_binding::Posture::Unverified;
            unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(unverified));
            test_publish_claim(&*mgr, 2, 10);
            assert!(
                !clears_gate(&*mgr, 10, 2),
                "an outbound ask to an Unverified target must NOT clear the gate"
            );

            // (b) Exact owner (node 3 on conn 20): the gate authorizes.
            let mut owner = ConnectionActor::new(20);
            owner.peer_node_id = 3;
            owner.posture = crate::peer_binding::Posture::Strict;
            owner.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(owner));
            test_publish_claim(&*mgr, 3, 20);
            assert!(
                clears_gate(&*mgr, 20, 3),
                "an outbound ask to the exact authenticated owner must clear the gate"
            );

            // (c) Supersede node 3's claim onto a new conn 21: the old owner
            // (conn 20) loses authority the instant the claim map is overwritten,
            // so an outbound ask still routed to conn 20 fails closed — while the
            // new exact owner (conn 21) clears the gate.
            let mut owner2 = ConnectionActor::new(21);
            owner2.peer_node_id = 3;
            owner2.posture = crate::peer_binding::Posture::Strict;
            owner2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(owner2));
            test_publish_claim(&*mgr, 3, 21);
            assert!(
                !clears_gate(&*mgr, 20, 3),
                "an outbound ask over a superseded connection must NOT clear the gate"
            );
            assert!(
                clears_gate(&*mgr, 21, 3),
                "the new exact owner must clear the outbound ask gate"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
        }
        drop(ops);
    }

    /// Read and clear the current thread's `hew_last_error` (the C-ABI sink that
    /// `crate::set_last_error` writes to — distinct from `stream_error`'s TLS).
    fn take_hew_last_error() -> Option<String> {
        let ptr = crate::hew_last_error();
        let out = if ptr.is_null() {
            None
        } else {
            // SAFETY: hew_last_error returns null or a valid NUL-terminated
            // C string owned by this thread's LAST_ERROR.
            Some(
                unsafe { std::ffi::CStr::from_ptr(ptr) }
                    .to_str()
                    .expect("hew_last_error must be valid UTF-8")
                    .to_owned(),
            )
        };
        crate::hew_clear_error();
        out
    }

    /// Issue #2652 (item 1): a `CTRL_LINK_DOWN` frame is authorised by the exact
    /// handshake-authenticated owner of the peer's published claim, NOT the
    /// posture-agnostic self-declared delivery id. An `Unverified`
    /// (delivery-only) peer and a **superseded** owner both resolve to `0`
    /// authenticated identity, so their link-DOWN is dropped with a diagnostic
    /// BEFORE any cross-node link-exit cascade can crash a locally-linked actor.
    ///
    /// Regression guard: the pre-fix handler read `peer_node_id_for_conn` (which
    /// returns the nonzero self-declared id even for an `Unverified`/superseded
    /// connection), so `deliver_link_down_to_ref`'s `== 0` guard never fired and
    /// such a peer could crash an actor linked to the genuine owner. If the gate
    /// is removed, the diagnostic below changes (the handler falls through to
    /// `handle_inbound_link_down`, which sets "no runtime installed").
    #[test]
    fn link_down_from_unverified_or_superseded_peer_is_gated() {
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        let link_down_frame = |ref_id: u64, route_slot: u16| {
            let payload = crate::envelope::MonitorDownPayload {
                ref_id,
                target: test_location(route_slot, 1),
                reason: 2,
                crash_kind: 0,
            };
            let bytes =
                crate::envelope::encode_link_down_payload(&payload).expect("link down encodes");
            ControlFrame {
                version: WIRE_VERSION,
                ctrl_kind: CTRL_LINK_DOWN,
                payload: bytes,
            }
        };

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Unverified peer node 2 on conn 10: ACTIVE + a published *delivery*
            // claim (Unverified peers still route fire-and-forget), but posture
            // is Unverified so it carries no control-plane authority.
            let mut unverified = ConnectionActor::new(10);
            unverified.peer_node_id = 2;
            unverified.posture = crate::peer_binding::Posture::Unverified;
            unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(unverified));
            let conn10_token = test_publish_claim(&*mgr, 2, 10);

            let _ = take_hew_last_error();
            assert_eq!(
                authenticated_peer_node_id_for_conn(&*mgr, 10),
                0,
                "precondition: Unverified conn 10 must have no authenticated identity"
            );
            handle_link_down_frame(mgr, 10, conn10_token, &link_down_frame(101, 2));
            let diag = take_hew_last_error().expect("gated link down must leave a diagnostic");
            assert!(
                diag.contains("link down") && diag.contains("missing authenticated peer"),
                "an Unverified peer's CTRL_LINK_DOWN must be gated by the exact-owner \
                 check, got: {diag}"
            );

            // Superseded owner: Strict conn 20 for node 3, then supersede its
            // claim onto conn 21. Conn 20 keeps Strict posture but is no longer
            // the claim owner, so its link-DOWN is refused too.
            let mut strict = ConnectionActor::new(20);
            strict.peer_node_id = 3;
            strict.posture = crate::peer_binding::Posture::Strict;
            strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(strict));
            let conn20_token = test_publish_claim(&*mgr, 3, 20);
            let mut strict2 = ConnectionActor::new(21);
            strict2.peer_node_id = 3;
            strict2.posture = crate::peer_binding::Posture::Strict;
            strict2.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(strict2));
            test_publish_claim(&*mgr, 3, 21);

            let _ = take_hew_last_error();
            handle_link_down_frame(mgr, 20, conn20_token, &link_down_frame(102, 3));
            let diag = take_hew_last_error().expect("superseded link down must leave a diagnostic");
            assert!(
                diag.contains("link down") && diag.contains("missing authenticated peer"),
                "a superseded owner's CTRL_LINK_DOWN must be gated, got: {diag}"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
        }
        drop(ops);
    }

    /// Issue #2652 (item 2): outbound registry gossip and SWIM traffic go ONLY
    /// to authenticated (`Strict` + published-owner) connections. An `Unverified`
    /// (delivery-only) peer must not be selected as a gossip recipient, a SWIM
    /// relay, or a direct SWIM-send target — symmetric with the inbound gate —
    /// while fire-and-forget user-message delivery to it stays allowed (D9's one
    /// intentional Unverified capability).
    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "test covers the full Unverified outbound gate across all control frame types"
    )]
    fn unverified_peer_receives_no_outbound_gossip_or_swim() {
        let sends = Box::into_raw(Box::new(Mutex::new(Vec::<(c_int, Vec<u8>)>::new())));
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: Some(record_registry_gossip_send),
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport_ptr = Box::into_raw(Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: sends.cast(),
        }));
        let cfg = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        // SAFETY: cfg valid for the call.
        let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cfg) };
        assert!(!cluster.is_null());

        // SAFETY: test-owned pointers remain valid until the explicit cleanup.
        unsafe {
            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());

            // Unverified gossip-capable peer node 2 on conn 10.
            let mut unverified = ConnectionActor::new(10);
            unverified.peer_node_id = 2;
            unverified.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            unverified.posture = crate::peer_binding::Posture::Unverified;
            unverified.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(unverified));
            test_publish_claim(&*mgr, 2, 10);

            // Authenticated gossip-capable peer node 3 on conn 20.
            let mut strict = ConnectionActor::new(20);
            strict.peer_node_id = 3;
            strict.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            strict.posture = crate::peer_binding::Posture::Strict;
            strict.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(strict));
            test_publish_claim(&*mgr, 3, 20);

            // Gossip recipient selection excludes the Unverified connection.
            assert_eq!(
                active_gossip_connection_ids(&*mgr),
                vec![20],
                "only the authenticated connection may receive registry gossip"
            );
            // SWIM relay selection excludes the Unverified peer.
            assert_eq!(
                hew_connmgr_active_swim_peers(mgr),
                vec![3],
                "only authenticated peers are eligible SWIM relays"
            );
            // Direct SWIM send to the Unverified peer is refused; to the
            // authenticated peer it proceeds.
            assert_eq!(
                hew_connmgr_send_swim(mgr, 2, crate::cluster::SWIM_MSG_PING, 0),
                -1,
                "a direct SWIM send to an Unverified peer must be refused"
            );
            assert_eq!(
                hew_connmgr_send_swim(mgr, 3, crate::cluster::SWIM_MSG_PING, 0),
                0,
                "a direct SWIM send to an authenticated peer proceeds"
            );

            // A gossip broadcast reaches ONLY the authenticated connection.
            {
                let guard = (&*sends)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    guard.iter().all(|(conn, _)| *conn == 20),
                    "no SWIM/gossip traffic may target the Unverified conn 10"
                );
            }
            (&*sends)
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clear();
            let count =
                hew_connmgr_broadcast_registry_gossip(mgr, "svc", test_location(1, 0xdef0), true);
            assert_eq!(
                count, 1,
                "registry gossip broadcast reaches exactly the one authenticated peer"
            );
            {
                let guard = (&*sends)
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                assert!(
                    guard.iter().all(|(conn, _)| *conn == 20),
                    "registry gossip broadcast must never target the Unverified conn 10"
                );
            }

            // Delivery-only capability preserved: fire-and-forget delivery to the
            // Unverified peer stays allowed even as control-plane traffic is
            // suppressed.
            assert!(
                !inbound_ask_denied_unverified(mgr, 10, 0),
                "fire-and-forget delivery to/from the Unverified peer stays allowed"
            );
            assert!(
                inbound_ask_denied_unverified(mgr, 10, 1),
                "an inbound ask from the Unverified peer stays denied"
            );

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(sends));
        }
        drop(ops);
    }

    /// before joining the reader thread so a reader blocked in `recv()` cannot
    /// hang the drop.
    #[test]
    fn conn_actor_drop_closes_transport_before_join() {
        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Sender<c_int> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
            let _ = tx.send(conn_id);
        }

        // close_tx/close_rx simulates the transport's recv becoming unblocked
        // when close_conn fires.  The reader thread blocks on close_rx.recv()
        // (standing in for a blocking transport recv).
        let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
        let (ready_tx, ready_rx) = std::sync::mpsc::channel::<()>();
        let (reader_saw_close_tx, reader_saw_close_rx) = std::sync::mpsc::channel::<c_int>();
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        let mut actor = ConnectionActor::new(99);
        actor.transport = transport_ptr;
        // Spawn a synthetic reader that blocks on close_rx, simulating a
        // reader thread blocked inside transport recv().  When close_conn(99)
        // fires it sends to close_tx, unblocking this recv().
        actor.reader_handle = Some(std::thread::spawn(move || {
            ready_tx.send(()).expect("ready signal");
            // Block here until transport is closed (mirrors a blocking recv).
            let conn_id = close_rx.recv().unwrap_or(-1);
            reader_saw_close_tx.send(conn_id).ok();
        }));

        ready_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("reader should signal ready");

        // Drop the actor.  Defense-in-depth must:
        //  1. Set reader_stop = 1 (expected-stop path)
        //  2. Call close_transport() → signal_close_conn(99) fires → unblocks reader
        //  3. Join reader thread (reader exits because its recv unblocked)
        // The drop must not hang.
        drop(actor);

        // The reader received conn_id=99 from close_conn, proving close happened
        // before (or during) the join.
        assert_eq!(
            reader_saw_close_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("reader must exit after transport close"),
            99,
            "drop must close transport connection 99 to unblock the reader"
        );

        // SAFETY: test-owned raw pointers outlive the actor.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<c_int>>(),
            ));
        }
        drop(ops);
    }

    #[test]
    fn connmgr_remove_releases_connections_lock_before_reader_wake() {
        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, _conn_id: c_int) {
            // SAFETY: test installs a Sender<()> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<()>>()) };
            tx.send(()).expect("close signal send should succeed");
        }

        let (close_tx, close_rx) = std::sync::mpsc::channel::<()>();
        let (lock_result_tx, lock_result_rx) = std::sync::mpsc::channel::<bool>();
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the lifetime of the manager in this test.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        assert!(!mgr.is_null());

        let mut actor = ConnectionActor::new(41);
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        let mgr_send = SendConnMgr(mgr);
        actor.reader_handle = Some(std::thread::spawn(move || {
            let mgr = mgr_send;
            close_rx.recv().expect("reader should observe close");
            // SAFETY: mgr points at the live manager under test until the outer
            // remove call drops and joins this thread.
            let could_lock = unsafe { (&*mgr.0).connections.try_access(|_| ()).is_some() };
            lock_result_tx
                .send(could_lock)
                .expect("reader should report lock availability");
        }));

        // SAFETY: mgr is a live manager allocated by hew_connmgr_new above.
        unsafe { (&*mgr).connections.access(|conns| conns.push(actor)) };

        // SAFETY: mgr is still valid and owns the test connection above.
        assert_eq!(unsafe { hew_connmgr_remove(mgr, 41) }, 0);
        assert!(
            lock_result_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("reader should report whether the lock was released"),
            "hew_connmgr_remove should release the connections lock before closing the transport"
        );
        // SAFETY: mgr remains valid until the free call below.
        assert_eq!(unsafe { hew_connmgr_count(mgr) }, 0);

        // SAFETY: mgr was allocated by hew_connmgr_new and is no longer used after this.
        unsafe { hew_connmgr_free(mgr) };
        // SAFETY: transport_ptr and close_impl were allocated in this test and outlive the manager.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<()>>(),
            ));
        }
        drop(ops);
    }

    #[test]
    fn install_connection_actor_shutdown_releases_lock_before_reader_wake() {
        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Sender<c_int> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
            tx.send(conn_id).expect("close signal send should succeed");
        }

        let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
        let (lock_result_tx, lock_result_rx) = std::sync::mpsc::channel::<(c_int, bool)>();
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the lifetime of the manager in this test.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        assert!(!mgr.is_null());

        // SAFETY: mgr points at a live manager until explicit teardown below.
        unsafe {
            (&*mgr).reconnect_shutdown.store(true, Ordering::Release);
        }

        let mut actor = ConnectionActor::new(52);
        let mgr_send = SendConnMgr(mgr);
        actor.reader_handle = Some(std::thread::spawn(move || {
            let mgr = mgr_send;
            let conn_id = close_rx.recv().expect("reader should observe close");
            // SAFETY: mgr remains live until install_connection_actor returns and teardown runs.
            let could_lock = unsafe { (&*mgr.0).connections.try_access(|_| ()).is_some() };
            lock_result_tx
                .send((conn_id, could_lock))
                .expect("reader should report lock availability");
        }));

        // SAFETY: mgr points at a live manager under test.
        let install = unsafe { install_connection_actor(&*mgr, actor) };
        assert!(matches!(install, Err(ConnectionInstallError::Shutdown)));
        assert_eq!(
            lock_result_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("reader should unblock and finish"),
            (52, true),
            "shutdown rejection should close transport after releasing the connections lock"
        );
        // SAFETY: mgr remains valid until the free call below.
        assert_eq!(unsafe { hew_connmgr_count(mgr) }, 0);

        // SAFETY: test-owned pointers remain valid until this cleanup completes.
        unsafe {
            hew_connmgr_free(mgr);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<c_int>>(),
            ));
        }
        drop(ops);
    }

    /// Regression test: `hew_connmgr_add` called after `hew_connmgr_free` sets the
    /// teardown flag must be rejected and its transport connection closed, so
    /// `hew_connmgr_free` completes without hanging.
    ///
    /// This reproduces the `reconnect_worker_loop` race where the worker passes all
    /// shutdown checks, connects a new transport, then calls `hew_connmgr_add`
    /// after teardown's drain has already completed.
    #[test]
    fn connmgr_free_rejects_concurrent_add_and_closes_transport() {
        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Sender<c_int> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
            let _ = tx.send(conn_id);
        }

        let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the lifetime of the manager in this test.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        assert!(!mgr.is_null());

        // Simulate the race: a reconnect worker observes shutdown=false, finishes
        // connecting (conn_id=77), then races hew_connmgr_add against free.
        //
        // We orchestrate the race by: setting reconnect_shutdown explicitly (as
        // free does), then calling hew_connmgr_add from this thread.  The early
        // shutdown guard in hew_connmgr_add must catch it, close conn 77, and
        // return -1 before free is even entered.  If free has already drained
        // (or is draining), install_connection_actor catches it under the lock.
        //
        // Phase 1: set shutdown flag (mirrors free's first action).
        // SAFETY: mgr is valid, reconnect_shutdown is AtomicBool.
        unsafe {
            (&*mgr).reconnect_shutdown.store(true, Ordering::Release);
        }

        // Phase 2: concurrent add with shutdown already set must be rejected and
        // must close the transport connection so no reader hangs.
        // SAFETY: mgr is valid; conn_id 77 is unused.
        let add_rc = unsafe { hew_connmgr_add(mgr, 77) };
        assert_eq!(
            add_rc, -1,
            "hew_connmgr_add must return -1 when manager is shutting down"
        );
        assert_eq!(
            close_rx
                .recv_timeout(std::time::Duration::from_millis(200))
                .expect("hew_connmgr_add must close the transport when rejecting during teardown"),
            77,
            "close_conn must be called for conn 77 on rejected add"
        );

        // Phase 3: free must complete without hanging even after the late-add attempt.
        // SAFETY: mgr was allocated by hew_connmgr_new and is not used after this.
        unsafe { hew_connmgr_free(mgr) };

        // SAFETY: test-owned pointers remain valid until this cleanup completes.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<c_int>>(),
            ));
        }
        drop(ops);
    }

    #[test]
    fn connmgr_free_waits_for_self_removed_reader_lifecycle() {
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: None,
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: std::ptr::null_mut(),
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid until explicit cleanup below.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        assert!(!mgr.is_null());

        // Model a reader that already removed/dropped its own ConnectionActor:
        // it no longer appears in `connections`, but its thread is still in
        // post-remove cleanup and must keep the manager's owner from freeing
        // routing/cluster state.
        // SAFETY: mgr is live until the free thread returns.
        let reader_guard = unsafe { (&*mgr).reader_lifecycle.register() };

        let (started_tx, started_rx) = std::sync::mpsc::channel();
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let mgr_addr = mgr as usize;
        let free_thread = std::thread::spawn(move || {
            started_tx
                .send(())
                .expect("free thread should announce start");
            // SAFETY: mgr_addr is the live manager pointer handed to this thread;
            // this call owns and frees it.
            unsafe { hew_connmgr_free(mgr_addr as *mut HewConnMgr) };
            done_tx.send(()).expect("free completion send");
        });

        started_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("free thread should start");
        assert!(
            done_rx
                .recv_timeout(std::time::Duration::from_millis(100))
                .is_err(),
            "hew_connmgr_free returned before the self-removed reader finished"
        );

        drop(reader_guard);
        done_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("free should finish once reader lifecycle is idle");
        free_thread.join().expect("free thread panicked");

        // SAFETY: hew_connmgr_free does not own the test transport allocation.
        unsafe {
            drop(Box::from_raw(transport_ptr));
        }
        drop(ops);
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test stages the full remove-vs-replacement ordering in one place"
    )]
    fn connmgr_remove_skips_stale_route_cleanup_after_replacement() {
        extern "C" fn collect_membership_events(
            node_id: u16,
            event: u8,
            user_data: *mut std::ffi::c_void,
        ) {
            // SAFETY: user_data points at the Vec<(u16, u8)> owned by this test.
            let events = unsafe { &mut *user_data.cast::<Vec<(u16, u8)>>() };
            events.push((node_id, event));
        }

        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Sender<c_int> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
            tx.send(conn_id).expect("close signal send should succeed");
        }

        let mut membership_events: Vec<(u16, u8)> = Vec::new();
        let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
        let (reader_release_tx, reader_release_rx) = std::sync::mpsc::channel::<()>();
        let (remove_result_tx, remove_result_rx) = std::sync::mpsc::channel::<c_int>();
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);
        let cluster_config = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };

        // SAFETY: all raw pointers are allocated in this test and remain valid
        // until the matching free calls below.
        unsafe {
            let routing_table = crate::routing::hew_routing_table_new_for_test(1);
            let cluster = crate::cluster::hew_cluster_new(&raw const cluster_config);
            assert!(!routing_table.is_null() && !cluster.is_null());
            crate::cluster::hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut membership_events).cast(),
            );
            assert_eq!(
                crate::cluster::hew_cluster_join(cluster, 2, c"10.0.0.2:9000".as_ptr()),
                0
            );

            let mgr = hew_connmgr_new(transport_ptr, None, routing_table, cluster, 1);
            assert!(!mgr.is_null());

            let mut old_actor = ConnectionActor::new(11);
            let old_token = next_publication_token(&*mgr);
            old_actor.publication_token = old_token;
            old_actor.peer_node_id = 2;
            old_actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            old_actor.reader_handle = Some(std::thread::spawn(move || {
                reader_release_rx
                    .recv()
                    .expect("reader should be released after replacement install");
            }));
            let old_publication_sync = Arc::clone(&old_actor.publication_sync);
            let old_publication_removed = Arc::clone(&old_actor.publication_removed);
            (&*mgr).connections.access(|conns| conns.push(old_actor));
            let old_superseded = test_reserve_unverified(&*mgr, 2, 11, old_token);
            publish_connection_established(
                &*mgr,
                2,
                11,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                old_token,
                &old_publication_sync,
                &old_publication_removed,
                old_superseded,
            );

            let mgr_send = SendConnMgr(mgr);
            let remove_handle = std::thread::spawn(move || {
                let mgr = mgr_send;
                // SAFETY: mgr points at the live manager under test.
                let result = hew_connmgr_remove(mgr.0, 11);
                remove_result_tx
                    .send(result)
                    .expect("remove thread should report its result");
            });

            assert_eq!(
                close_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("remove should close the old transport before cleanup"),
                11
            );

            let mut replacement_actor = Some({
                let mut actor = ConnectionActor::new(22);
                actor.publication_token = next_publication_token(&*mgr);
                actor.peer_node_id = 2;
                actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
                actor
            });
            let replacement_token = replacement_actor
                .as_ref()
                .map(|actor| actor.publication_token)
                .expect("replacement token should be set before install");
            let replacement_publication_sync = replacement_actor
                .as_ref()
                .map(|actor| Arc::clone(&actor.publication_sync))
                .expect("replacement sync should be set before install");
            let replacement_publication_removed = replacement_actor
                .as_ref()
                .map(|actor| Arc::clone(&actor.publication_removed))
                .expect("replacement removed flag should be set before install");
            let replacement_installed = (0..50).any(|_| {
                if (&*mgr)
                    .connections
                    .try_access(|conns| {
                        conns.push(
                            replacement_actor
                                .take()
                                .expect("replacement should install once"),
                        );
                    })
                    .is_some()
                {
                    true
                } else {
                    // Lock is held by the remove path; retry after a brief sleep.
                    std::thread::sleep(std::time::Duration::from_millis(10));
                    false
                }
            });
            assert!(
                replacement_installed,
                "replacement connection should install while old remove waits on reader shutdown"
            );
            let replacement_superseded = test_reserve_unverified(&*mgr, 2, 22, replacement_token);
            publish_connection_established(
                &*mgr,
                2,
                22,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                replacement_token,
                &replacement_publication_sync,
                &replacement_publication_removed,
                replacement_superseded,
            );

            reader_release_tx
                .send(())
                .expect("reader release should succeed");
            assert_eq!(
                remove_result_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("remove should complete once the old reader exits"),
                0
            );
            remove_handle
                .join()
                .expect("remove thread should not panic");

            assert_eq!(
                crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(2, 0)),
                22,
                "stale remove should not delete the replacement route"
            );
            assert_eq!(
                membership_events,
                vec![(2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED)],
                "stale remove should not emit a lost event after the replacement is established"
            );
            assert_eq!(hew_connmgr_count(mgr), 1);

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            crate::routing::hew_routing_table_free(routing_table);
        }

        // SAFETY: transport_ptr and close_impl were allocated above and are no
        // longer referenced after manager teardown.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<c_int>>(),
            ));
        }
        drop(ops);
    }

    // ---- issue #2652 · Slice 4 · NodeId claim state machine ----------------

    /// Build a minimal manager for claim-machine unit tests: a stub transport
    /// with all-`None` ops (its `close_conn` is a no-op) and no routing/cluster.
    /// The claim helpers only touch `mgr.claims`, so this isolates the state
    /// machine from routing/cluster side effects.
    fn claim_test_mgr() -> (*mut HewConnMgr, *mut HewTransport) {
        let transport = Box::into_raw(Box::new(HewTransport {
            ops: std::ptr::null(),
            r#impl: std::ptr::null_mut(),
        }));
        // SAFETY: transport is a freshly-boxed valid pointer; routing/cluster null
        // is accepted by hew_connmgr_new.
        let mgr = unsafe {
            hew_connmgr_new(
                transport,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                1,
            )
        };
        assert!(!mgr.is_null());
        (mgr, transport)
    }

    fn free_claim_test_mgr(mgr: *mut HewConnMgr, transport: *mut HewTransport) {
        // SAFETY: both pointers were allocated in claim_test_mgr and are not
        // referenced after this call.
        unsafe {
            hew_connmgr_free(mgr);
            drop(Box::from_raw(transport));
        }
    }

    fn claim_snapshot(mgr: &HewConnMgr, node_id: u16) -> Option<LiveClaim> {
        mgr.claims
            .0
            .lock_or_recover()
            .get(&test_node_identity(node_id))
            .cloned()
    }

    #[test]
    fn claim_reserve_publish_retire_exact_owner_lifecycle() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr is live until free below.
        let mgr = unsafe { &*mgr_ptr };
        let cred = Some(PeerCredential::NoiseKey([7u8; 32]));

        match reserve_claim(mgr, 42, cred.as_ref(), 100, 1) {
            ClaimReservation::Reserved { superseded } => assert!(superseded.is_none()),
            ClaimReservation::Rejected(d) => panic!("fresh reserve rejected: {d}"),
        }
        let reserved = claim_snapshot(mgr, 42).expect("claim should exist after reserve");
        assert_eq!(reserved.state, ClaimState::Reserved);
        assert_eq!(reserved.conn_id, 100);

        assert!(publish_claim(mgr, 42, 100, 1), "exact owner should publish");
        assert_eq!(
            claim_snapshot(mgr, 42).expect("claim persists").state,
            ClaimState::Published
        );

        assert!(retire_claim(mgr, 42, 100, 1), "exact owner should retire");
        assert_eq!(
            claim_snapshot(mgr, 42)
                .expect("retired claim remains as a replay fence")
                .state,
            ClaimState::Retired
        );
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn retired_identity_claim_rejects_lower_session_and_allows_reconnect() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr is live until free below.
        let mgr = unsafe { &*mgr_ptr };
        let identity = test_node_identity(42);

        assert!(matches!(
            reserve_identity_claim(mgr, identity, 42, 7, None, 100, 1),
            ClaimReservation::Reserved { superseded: None }
        ));
        assert!(publish_identity_claim(mgr, identity, 100, 1));
        assert!(retire_identity_claim(mgr, identity, 100, 1));

        assert!(matches!(
            reserve_identity_claim(mgr, identity, 42, 6, None, 200, 2),
            ClaimReservation::Rejected(_)
        ));

        let equal_superseded = match reserve_identity_claim(mgr, identity, 42, 7, None, 200, 2) {
            ClaimReservation::Reserved {
                superseded: Some(retired),
            } => retired,
            ClaimReservation::Reserved { superseded: None } => {
                panic!("equal-session reconnect must retain the retired fence")
            }
            ClaimReservation::Rejected(detail) => {
                panic!("equal-session reconnect must be admitted: {detail}")
            }
        };
        assert_eq!(equal_superseded.state, ClaimState::Retired);
        abort_identity_claim(mgr, identity, 200, 2, Some(equal_superseded));

        assert!(matches!(
            reserve_identity_claim(mgr, identity, 42, 8, None, 300, 3),
            ClaimReservation::Reserved {
                superseded: Some(LiveClaim {
                    state: ClaimState::Retired,
                    ..
                })
            }
        ));

        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_reserve_rejects_different_credential_over_published() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        let cred_a = Some(PeerCredential::NoiseKey([0xAA; 32]));
        let cred_b = Some(PeerCredential::NoiseKey([0xBB; 32]));

        // A owns 42, Published.
        assert!(matches!(
            reserve_claim(mgr, 42, cred_a.as_ref(), 100, 1),
            ClaimReservation::Reserved { .. }
        ));
        assert!(publish_claim(mgr, 42, 100, 1));

        // B presents a different credential for the same NodeId ⇒ reject.
        match reserve_claim(mgr, 42, cred_b.as_ref(), 200, 2) {
            ClaimReservation::Rejected(detail) => {
                assert!(detail.contains("another credential"), "detail: {detail}");
            }
            ClaimReservation::Reserved { .. } => {
                panic!("different-credential reserve must be rejected fail-closed")
            }
        }
        // A's claim is untouched.
        let still = claim_snapshot(mgr, 42).expect("A's claim persists");
        assert_eq!(still.conn_id, 100);
        assert_eq!(still.publication_token, 1);
        assert_eq!(still.state, ClaimState::Published);
        assert_eq!(still.credential, cred_a);
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_reserve_supersedes_same_credential_and_abort_restores_it() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        let cred = Some(PeerCredential::NoiseKey([0xCC; 32]));

        assert!(matches!(
            reserve_claim(mgr, 42, cred.as_ref(), 100, 1),
            ClaimReservation::Reserved { .. }
        ));
        assert!(publish_claim(mgr, 42, 100, 1));

        // Same-credential reconnect supersedes the live Published claim.
        let superseded = match reserve_claim(mgr, 42, cred.as_ref(), 200, 2) {
            ClaimReservation::Reserved { superseded } => {
                superseded.expect("same-credential reconnect supersedes the old claim")
            }
            ClaimReservation::Rejected(d) => panic!("same-credential reconnect rejected: {d}"),
        };
        assert_eq!(superseded.conn_id, 100);
        assert_eq!(superseded.state, ClaimState::Published);
        // Map now holds the new Reserved claim.
        let mid = claim_snapshot(mgr, 42).expect("new reservation present");
        assert_eq!(mid.conn_id, 200);
        assert_eq!(mid.state, ClaimState::Reserved);

        // Aborting the replacement restores the superseded Published claim —
        // never orphans it.
        abort_claim(mgr, 42, 200, 2, Some(superseded));
        let restored = claim_snapshot(mgr, 42).expect("superseded claim restored");
        assert_eq!(restored.conn_id, 100);
        assert_eq!(restored.state, ClaimState::Published);
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_abort_removes_fresh_reservation() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        assert!(matches!(
            reserve_claim(mgr, 7, None, 300, 5),
            ClaimReservation::Reserved { .. }
        ));
        abort_claim(mgr, 7, 300, 5, None);
        assert!(
            claim_snapshot(mgr, 7).is_none(),
            "aborting a fresh reservation leaves the map empty"
        );
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_retire_non_owner_removes_nothing() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        assert!(matches!(
            reserve_claim(mgr, 9, None, 100, 1),
            ClaimReservation::Reserved { .. }
        ));
        assert!(publish_claim(mgr, 9, 100, 1));
        // A different conn/token retires nothing and reports "not owner".
        assert!(!retire_claim(mgr, 9, 999, 42), "non-owner must not retire");
        assert!(
            claim_snapshot(mgr, 9).is_some(),
            "non-owner retire leaves the claim intact"
        );
        // The real owner retires.
        assert!(retire_claim(mgr, 9, 100, 1));
        assert_eq!(
            claim_snapshot(mgr, 9)
                .expect("retired claim remains as a replay fence")
                .state,
            ClaimState::Retired
        );
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_publish_aborts_when_superseded_in_reserve_window() {
        let (mgr_ptr, transport) = claim_test_mgr();
        // SAFETY: mgr_ptr live until free.
        let mgr = unsafe { &*mgr_ptr };
        let cred = Some(PeerCredential::NoiseKey([0xDD; 32]));

        // A reserves + publishes.
        assert!(matches!(
            reserve_claim(mgr, 42, cred.as_ref(), 100, 1),
            ClaimReservation::Reserved { .. }
        ));
        assert!(publish_claim(mgr, 42, 100, 1));

        // A2 supersedes (same credential) → map owner is now A2's reservation.
        assert!(matches!(
            reserve_claim(mgr, 42, cred.as_ref(), 200, 2),
            ClaimReservation::Reserved { .. }
        ));

        // A's (late) publish must abort — it is no longer the map owner.
        assert!(
            !publish_claim(mgr, 42, 100, 1),
            "a superseded connection's publish must abort"
        );
        // A2 still owns the reservation, still Reserved (its own publish pending).
        let owner = claim_snapshot(mgr, 42).expect("A2 reservation present");
        assert_eq!(owner.conn_id, 200);
        assert_eq!(owner.state, ClaimState::Reserved);
        free_claim_test_mgr(mgr_ptr, transport);
    }

    #[test]
    fn claim_reserve_waits_on_reserved_then_proceeds_after_abort() {
        use std::sync::atomic::{AtomicBool, Ordering};
        let (mgr_ptr, transport) = claim_test_mgr();
        // A reserves node 42 (Reserved), does not yet publish or abort.
        {
            // SAFETY: mgr_ptr live until free.
            let mgr = unsafe { &*mgr_ptr };
            assert!(matches!(
                reserve_claim(mgr, 42, None, 100, 1),
                ClaimReservation::Reserved { .. }
            ));
        }

        let mgr_addr = mgr_ptr as usize;
        let b_reserved = Arc::new(AtomicBool::new(false));
        let b_reserved_thread = Arc::clone(&b_reserved);
        // B arrives for 42, observes Reserved, and must WAIT (not reject).
        let b = std::thread::spawn(move || {
            // SAFETY: mgr stays live until the test joins this thread.
            let mgr = unsafe { &*(mgr_addr as *mut HewConnMgr) };
            let outcome = reserve_claim(mgr, 42, None, 200, 2);
            b_reserved_thread.store(true, Ordering::Release);
            matches!(outcome, ClaimReservation::Reserved { .. })
        });

        // Give B time to reach the wait; it must not have returned yet.
        std::thread::sleep(std::time::Duration::from_millis(100));
        assert!(
            !b_reserved.load(Ordering::Acquire),
            "B must block while A's reservation is in flight"
        );

        // A aborts → signals the condvar → B wakes, sees absent, reserves.
        {
            // SAFETY: mgr_ptr live.
            let mgr = unsafe { &*mgr_ptr };
            abort_claim(mgr, 42, 100, 1, None);
        }
        let b_ok = b.join().expect("B thread should not panic");
        assert!(b_ok, "B should reserve after A aborts");
        // SAFETY: mgr_ptr live.
        let owner = claim_snapshot(unsafe { &*mgr_ptr }, 42).expect("B now owns 42");
        assert_eq!(owner.conn_id, 200);
        free_claim_test_mgr(mgr_ptr, transport);
    }

    /// Concurrent restoration of BLOCK-3: many admissions race for the same
    /// `NodeId`; exactly one may hold a Published claim at a time.
    #[test]
    fn claim_concurrent_reserve_publish_yields_exactly_one_owner() {
        let (mgr_ptr, transport) = claim_test_mgr();
        let mgr_addr = mgr_ptr as usize;
        let winners = Arc::new(std::sync::Mutex::new(Vec::<c_int>::new()));
        let mut handles = Vec::new();
        for i in 0u8..8 {
            let winners = Arc::clone(&winners);
            handles.push(std::thread::spawn(move || {
                // SAFETY: mgr stays live until join below.
                let mgr = unsafe { &*(mgr_addr as *mut HewConnMgr) };
                let conn_id = 1000 + c_int::from(i);
                let token = 1000 + u64::from(i);
                let cred = Some(PeerCredential::NoiseKey([i; 32]));
                match reserve_claim(mgr, 42, cred.as_ref(), conn_id, token) {
                    ClaimReservation::Reserved { .. } => {
                        if publish_claim(mgr, 42, conn_id, token) {
                            winners.lock_or_recover().push(conn_id);
                        }
                    }
                    ClaimReservation::Rejected(_) => {}
                }
            }));
        }
        for h in handles {
            h.join().expect("claim race thread should not panic");
        }
        // At most one Published owner at any time; whoever published is the sole
        // map owner. Distinct credentials mean losers were rejected, not queued.
        let winners = winners.lock_or_recover();
        assert!(
            winners.len() <= 1,
            "at most one connection may publish a NodeId claim, got {winners:?}"
        );
        // SAFETY: mgr_ptr live.
        let final_owner = claim_snapshot(unsafe { &*mgr_ptr }, 42);
        if let Some(owner) = final_owner {
            assert_eq!(owner.state, ClaimState::Published);
            if let Some(&w) = winners.first() {
                assert_eq!(owner.conn_id, w);
            }
        }
        drop(winners);
        free_claim_test_mgr(mgr_ptr, transport);
    }

    /// Regression: `hew_connmgr_free` must set `reader_stop` BEFORE closing the
    /// transport so the reader that unblocks from the socket shutdown sees
    /// `stop_flag` == 1 (expected-stop path) and does not call `hew_connmgr_remove`.
    #[test]
    fn connmgr_free_sets_reader_stop_before_transport_close() {
        use std::sync::atomic::{AtomicI32, Ordering};

        // A close_conn callback that captures the reader_stop value at the moment
        // the transport close fires.
        extern "C" fn capture_stop_on_close(impl_ptr: *mut std::ffi::c_void, _conn_id: c_int) {
            // SAFETY: impl_ptr points at a Box<(Arc<AtomicI32>, Sender<i32>)>
            // allocated in the test body below; the Box outlives this callback.
            let pair =
                unsafe { &*impl_ptr.cast::<(Arc<AtomicI32>, std::sync::mpsc::Sender<i32>)>() };
            let _ = pair.1.send(pair.0.load(Ordering::Acquire));
        }

        let (stop_tx, stop_rx) = std::sync::mpsc::channel::<i32>();
        // We need to share the reader_stop Arc with the callback before the actor is
        // created. Use a placeholder Arc; we'll swap the real one in after actor creation.
        let placeholder: Arc<AtomicI32> = Arc::new(AtomicI32::new(0));
        let pair = Box::new((Arc::clone(&placeholder), stop_tx));
        let close_impl = Box::into_raw(pair).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(capture_stop_on_close),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the lifetime of the manager.
        let mgr = unsafe {
            hew_connmgr_new(
                transport_ptr,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            )
        };
        assert!(!mgr.is_null());

        let actor = ConnectionActor::new(55);
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        // Swap the real reader_stop Arc into the callback pair so it can observe
        // the stop flag at the moment close_conn fires.
        let real_stop = Arc::clone(&actor.reader_stop);
        // SAFETY: close_impl points at the Box<(Arc<AtomicI32>, Sender<i32>)> we created.
        unsafe {
            let pair = &mut *close_impl.cast::<(Arc<AtomicI32>, std::sync::mpsc::Sender<i32>)>();
            pair.0 = real_stop;
        }

        // SAFETY: mgr is live; actor is pushed and stays until free.
        unsafe { (&*mgr).connections.access(|conns| conns.push(actor)) };
        // SAFETY: mgr was allocated by hew_connmgr_new; this call frees it.
        unsafe { hew_connmgr_free(mgr) };

        let stop_seen = stop_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("close_conn should fire during hew_connmgr_free");
        assert_eq!(
            stop_seen, 1,
            "reader_stop must be 1 at the moment close_conn fires in hew_connmgr_free"
        );

        // SAFETY: allocated in this test; no longer referenced.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<(Arc<AtomicI32>, std::sync::mpsc::Sender<i32>)>(),
            ));
        }
        drop(ops);
    }

    /// Regression: `hew_connmgr_remove` must remove the route from the routing
    /// table BEFORE removing the connection from `mgr.connections`. There must be no
    /// window where `hew_routing_lookup` returns a `conn_id` that `hew_connmgr_send`
    /// would reject (route-ok but conn already gone from the list).
    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "single-place staging of the route-gone-before-conn-leaves ordering, now including the issue #2652 claim reservation"
    )]
    fn connmgr_remove_route_gone_before_conn_leaves_list() {
        use std::sync::atomic::Ordering;

        // A close_conn callback that, at the moment the transport close fires,
        // checks whether the route for peer node 2 is still present AND whether
        // the connection is still in the manager's list. After the fix the route
        // must already be absent while the conn may or may not be gone yet.
        extern "C" fn check_route_on_close(impl_ptr: *mut std::ffi::c_void, _conn_id: c_int) {
            // SAFETY: impl_ptr points at a Box<(*mut HewRoutingTable, *mut HewConnMgr, Sender<(i32, usize)>)>.
            let triple = unsafe {
                &*impl_ptr.cast::<(
                    *mut crate::routing::HewRoutingTable,
                    *mut HewConnMgr,
                    std::sync::mpsc::Sender<(i32, usize)>,
                )>()
            };
            let (routing_table, mgr, tx) = (triple.0, triple.1, &triple.2);
            // Route lookup: returns conn_id for the route, or -1.
            // SAFETY: routing_table is a live pointer allocated by hew_routing_table_new
            // in the test body; the manager keeps it alive for the callback's duration.
            let route = unsafe {
                crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(2, 0))
            };
            // SAFETY: mgr is a live pointer allocated by hew_connmgr_new in the test body.
            let raw_count = unsafe { hew_connmgr_count(mgr) };
            // hew_connmgr_count returns >= 0; saturate negatives to 0 to avoid sign-loss warning.
            let count = raw_count.unsigned_abs() as usize;
            let _ = tx.send((route, count));
        }

        let (check_tx, check_rx) = std::sync::mpsc::channel::<(i32, usize)>();
        // SAFETY: hew_routing_table_new allocates a new routing table with no preconditions.
        let routing_table = crate::routing::hew_routing_table_new_for_test(1);
        assert!(!routing_table.is_null());

        // We'll fill in mgr after creation.
        let triple = Box::new((routing_table, std::ptr::null_mut::<HewConnMgr>(), check_tx));
        let close_impl = Box::into_raw(triple).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(check_route_on_close),
            destroy: None,
        });
        let transport = Box::new(HewTransport {
            ops: &raw const *ops,
            r#impl: close_impl,
        });
        let transport_ptr = Box::into_raw(transport);

        // SAFETY: transport_ptr remains valid for the test duration.
        let mgr =
            unsafe { hew_connmgr_new(transport_ptr, None, routing_table, std::ptr::null_mut(), 1) };
        assert!(!mgr.is_null());

        // Patch the mgr pointer into the callback triple.
        // SAFETY: close_impl points at the Box we created above.
        unsafe {
            let triple = &mut *close_impl.cast::<(
                *mut crate::routing::HewRoutingTable,
                *mut HewConnMgr,
                std::sync::mpsc::Sender<(i32, usize)>,
            )>();
            triple.1 = mgr;
        }

        // Build a real-enough actor for conn_id=77, peer_node_id=2.
        let mut actor = ConnectionActor::new(77);
        actor.peer_node_id = 2;
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        // SAFETY: mgr is live and was allocated by hew_connmgr_new above.
        let publication_token = unsafe { next_publication_token(&*mgr) };
        actor.publication_token = publication_token;
        let pub_sync = Arc::clone(&actor.publication_sync);
        let pub_removed = Arc::clone(&actor.publication_removed);
        // SAFETY: mgr is live.
        unsafe { (&*mgr).connections.access(|conns| conns.push(actor)) };

        // Reserve the claim as admission would before publishing.
        // SAFETY: mgr is live.
        let superseded = unsafe { test_reserve_unverified(&*mgr, 2, 77, publication_token) };

        // Publish the route so routing_lookup returns 77 before remove.
        // SAFETY: mgr is live; all arguments come from the actor and publication
        // metadata allocated in this test.
        unsafe {
            publish_connection_established(
                &*mgr,
                2,
                77,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication_token,
                &pub_sync,
                &pub_removed,
                superseded,
            );
        }
        assert_eq!(
            // SAFETY: routing_table is live and allocated by hew_routing_table_new above.
            unsafe {
                crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(2, 0))
            },
            77,
            "route should exist before remove"
        );

        // SAFETY: mgr is live; this calls close_conn (our callback) during teardown.
        let rc = unsafe { hew_connmgr_remove(mgr, 77) };
        assert_eq!(rc, 0);

        let (route_at_close, _count_at_close) = check_rx
            .recv_timeout(std::time::Duration::from_secs(1))
            .expect("close_conn should fire during hew_connmgr_remove");
        assert_eq!(
            route_at_close, -1,
            "route must already be removed when close_conn fires (no TOCTOU window)"
        );

        // After remove the route should be gone.
        assert_eq!(
            // SAFETY: routing_table is live; still held by the test.
            unsafe {
                crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(2, 0))
            },
            -1,
            "route must be absent after remove completes"
        );

        // SAFETY: mgr was allocated by hew_connmgr_new; routing_table by hew_routing_table_new.
        unsafe { hew_connmgr_free(mgr) };
        // SAFETY: routing_table was allocated by hew_routing_table_new above.
        unsafe { crate::routing::hew_routing_table_free(routing_table) };
        // SAFETY: transport and triple allocated in this test.
        unsafe {
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(close_impl.cast::<(
                *mut crate::routing::HewRoutingTable,
                *mut HewConnMgr,
                std::sync::mpsc::Sender<(i32, usize)>,
            )>()));
        }
        drop(ops);
    }

    #[test]
    fn connmgr_remove_notifies_cluster_without_routing_table() {
        extern "C" fn collect_membership_events(
            node_id: u16,
            event: u8,
            user_data: *mut std::ffi::c_void,
        ) {
            // SAFETY: user_data points at the Vec<(u16, u8)> owned by this test.
            let events = unsafe { &mut *user_data.cast::<Vec<(u16, u8)>>() };
            events.push((node_id, event));
        }

        let mut membership_events: Vec<(u16, u8)> = Vec::new();
        let cluster_config = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };

        // SAFETY: test-owned pointers remain valid until the matching free calls below.
        unsafe {
            let cluster = crate::cluster::hew_cluster_new(&raw const cluster_config);
            assert!(!cluster.is_null());
            crate::cluster::hew_cluster_set_membership_callback(
                cluster,
                collect_membership_events,
                (&raw mut membership_events).cast(),
            );
            assert_eq!(
                crate::cluster::hew_cluster_join(cluster, 2, c"10.0.0.2:9000".as_ptr()),
                0
            );

            let mgr = hew_connmgr_new(
                Box::into_raw(Box::new(HewTransport {
                    ops: std::ptr::null(),
                    r#impl: std::ptr::null_mut(),
                })),
                None,
                std::ptr::null_mut(),
                cluster,
                1,
            );
            assert!(!mgr.is_null());

            let mut actor = ConnectionActor::new(31);
            let publication_token = next_publication_token(&*mgr);
            actor.publication_token = publication_token;
            actor.peer_node_id = 2;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            let publication_sync = Arc::clone(&actor.publication_sync);
            let publication_removed = Arc::clone(&actor.publication_removed);
            (&*mgr).connections.access(|conns| conns.push(actor));
            let superseded = test_reserve_unverified(&*mgr, 2, 31, publication_token);
            publish_connection_established(
                &*mgr,
                2,
                31,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication_token,
                &publication_sync,
                &publication_removed,
                superseded,
            );

            assert_eq!(hew_connmgr_remove(mgr, 31), 0);
            assert_eq!(
                membership_events,
                vec![
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "cluster-only managers should still emit connection_lost"
            );

            let transport_ptr = (*mgr).transport;
            hew_connmgr_free(mgr);
            drop(Box::from_raw(transport_ptr));
            crate::cluster::hew_cluster_free(cluster);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "test stages the delayed establish-publish teardown race end-to-end"
    )]
    fn run_connmgr_publish_skips_removed_connection_test(with_routing_table: bool) {
        unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
            // SAFETY: test installs a Sender<c_int> as the transport impl payload.
            let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
            tx.send(conn_id).expect("close signal send should succeed");
        }

        struct BlockingMembershipState {
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            suspect_seen: std::sync::mpsc::Sender<()>,
            release: std::sync::Arc<std::sync::Barrier>,
            blocked_first_suspect: std::sync::atomic::AtomicBool,
        }

        extern "C" fn block_on_suspect(node_id: u16, event: u8, user_data: *mut std::ffi::c_void) {
            // SAFETY: user_data points at the BlockingMembershipState allocated in this test.
            let state = unsafe { &*user_data.cast::<BlockingMembershipState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT
                && !state.blocked_first_suspect.swap(true, Ordering::AcqRel)
            {
                state
                    .suspect_seen
                    .send(())
                    .expect("suspect callback should notify the test");
                state.release.wait();
            }
        }

        struct SendCluster(*mut crate::cluster::HewCluster);
        // SAFETY: the test keeps the cluster alive until both worker threads complete.
        unsafe impl Send for SendCluster {}

        let cluster_config = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
        let (suspect_tx, suspect_rx) = std::sync::mpsc::channel::<()>();
        let release = std::sync::Arc::new(std::sync::Barrier::new(2));
        let callback_state = Box::into_raw(Box::new(BlockingMembershipState {
            events: std::sync::Mutex::new(Vec::new()),
            suspect_seen: suspect_tx,
            release: std::sync::Arc::clone(&release),
            blocked_first_suspect: std::sync::atomic::AtomicBool::new(false),
        }));
        let close_impl = Box::into_raw(Box::new(close_tx)).cast::<std::ffi::c_void>();
        let ops = Box::new(crate::transport::HewTransportOps {
            connect: None,
            listen: None,
            accept: None,
            send: None,
            recv: None,
            close_conn: Some(signal_close_conn),
            destroy: None,
        });

        // SAFETY: test-owned pointers remain valid until the explicit cleanup below.
        unsafe {
            let transport_ptr = Box::into_raw(Box::new(HewTransport {
                ops: &raw const *ops,
                r#impl: close_impl,
            }));
            let routing_table = if with_routing_table {
                crate::routing::hew_routing_table_new_for_test(1)
            } else {
                std::ptr::null_mut()
            };
            let cluster = crate::cluster::hew_cluster_new(&raw const cluster_config);
            assert!(!cluster.is_null());
            crate::cluster::hew_cluster_set_membership_callback(
                cluster,
                block_on_suspect,
                callback_state.cast(),
            );
            assert_eq!(
                crate::cluster::hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()),
                0
            );
            assert_eq!(
                crate::cluster::hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );

            let mgr = hew_connmgr_new(transport_ptr, None, routing_table, cluster, 1);
            assert!(!mgr.is_null());

            let mut actor = ConnectionActor::new(22);
            actor.publication_token = 2;
            actor.peer_node_id = 2;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            let publication_sync = Arc::clone(&actor.publication_sync);
            let publication_removed = Arc::clone(&actor.publication_removed);
            (&*mgr).connections.access(|conns| conns.push(actor));
            // Reserve the claim (token 2) so the raced publish below can transition
            // it Reserved → Published, mirroring admission.
            let _ = test_reserve_unverified(&*mgr, 2, 22, 2);

            let (lost_done_tx, lost_done_rx) = std::sync::mpsc::channel::<()>();
            let lost_cluster = SendCluster(cluster);
            let lost_handle = std::thread::spawn(move || {
                let cluster = lost_cluster;
                let rc =
                    crate::cluster::hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
                assert_eq!(rc, 0);
                lost_done_tx
                    .send(())
                    .expect("lost thread should report completion");
            });

            suspect_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost path should reach the membership callback");

            let (publish_done_tx, publish_done_rx) = std::sync::mpsc::channel::<()>();
            let mgr_send = SendConnMgr(mgr);
            let publish_handle = std::thread::spawn(move || {
                let mgr = mgr_send;
                // SAFETY: mgr stays alive until the worker joins.
                publish_connection_established(
                    &*mgr.0,
                    2,
                    22,
                    HEW_FEATURE_SUPPORTS_GOSSIP,
                    2,
                    &publication_sync,
                    &publication_removed,
                    None,
                );
                publish_done_tx
                    .send(())
                    .expect("publish thread should report completion");
            });

            let (remove_done_tx, remove_done_rx) = std::sync::mpsc::channel::<c_int>();
            let remove_mgr = SendConnMgr(mgr);
            let remove_handle = std::thread::spawn(move || {
                let mgr = remove_mgr;
                // SAFETY: mgr stays alive until the worker joins.
                let rc = hew_connmgr_remove(mgr.0, 22);
                remove_done_tx
                    .send(rc)
                    .expect("remove thread should report completion");
            });
            assert_eq!(
                close_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("remove should close the test connection before publish resumes"),
                22
            );
            release.wait();

            lost_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("lost thread should finish once released");
            publish_done_rx
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("publish thread should finish after the old lost transition");
            assert_eq!(
                remove_done_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("remove should finish after publication cleanup"),
                0
            );

            lost_handle.join().expect("lost thread should not panic");
            publish_handle
                .join()
                .expect("publish thread should not panic");
            remove_handle
                .join()
                .expect("remove thread should not panic");

            let events = (&*callback_state)
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            assert_eq!(
                events,
                vec![
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "a removed connection must not publish a delayed ALIVE/JOINED transition"
            );
            if with_routing_table {
                assert_eq!(
                    crate::routing::hew_routing_lookup(
                        routing_table,
                        crate::pid::hew_pid_make(2, 0),
                    ),
                    -1,
                    "a removed connection must not publish a delayed route"
                );
            }
            assert_eq!(hew_connmgr_count(mgr), 0);

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            if !routing_table.is_null() {
                crate::routing::hew_routing_table_free(routing_table);
            }
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(
                close_impl.cast::<std::sync::mpsc::Sender<c_int>>(),
            ));
            drop(Box::from_raw(callback_state));
        }
        drop(ops);
    }

    #[test]
    fn connmgr_publish_skips_removed_connection_while_establish_blocks() {
        run_connmgr_publish_skips_removed_connection_test(true);
    }

    #[test]
    fn connmgr_publish_skips_removed_connection_without_routing_table() {
        run_connmgr_publish_skips_removed_connection_test(false);
    }

    #[test]
    #[expect(
        clippy::too_many_lines,
        reason = "test stages a full publish-callback-remove cycle in one place"
    )]
    fn connmgr_publish_allows_reentrant_remove_from_membership_callback() {
        struct ReentrantRemoveState {
            mgr: std::sync::atomic::AtomicPtr<HewConnMgr>,
            conn_id: c_int,
            events: std::sync::Mutex<Vec<(u16, u8)>>,
            remove_result_tx: std::sync::mpsc::Sender<c_int>,
        }

        extern "C" fn remove_on_joined(node_id: u16, event: u8, user_data: *mut std::ffi::c_void) {
            // SAFETY: user_data points at the ReentrantRemoveState allocated in this test.
            let state = unsafe { &*user_data.cast::<ReentrantRemoveState>() };
            state
                .events
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((node_id, event));
            if event == crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED {
                let mgr = state.mgr.load(Ordering::Acquire);
                assert!(!mgr.is_null(), "callback manager should be initialized");
                // SAFETY: the manager remains valid until the test explicitly frees it.
                let rc = unsafe { hew_connmgr_remove(mgr, state.conn_id) };
                state
                    .remove_result_tx
                    .send(rc)
                    .expect("reentrant remove should report completion");
            }
        }

        let cluster_config = crate::cluster::ClusterConfig {
            local_node_id: 1,
            ..crate::cluster::ClusterConfig::default()
        };
        let (remove_result_tx, remove_result_rx) = std::sync::mpsc::channel::<c_int>();
        let callback_state = Box::into_raw(Box::new(ReentrantRemoveState {
            mgr: std::sync::atomic::AtomicPtr::new(std::ptr::null_mut()),
            conn_id: 44,
            events: std::sync::Mutex::new(Vec::new()),
            remove_result_tx,
        }));

        // SAFETY: test-owned pointers remain valid until the explicit cleanup below.
        unsafe {
            let transport_ptr = Box::into_raw(Box::new(HewTransport {
                ops: std::ptr::null(),
                r#impl: std::ptr::null_mut(),
            }));
            let cluster = crate::cluster::hew_cluster_new(&raw const cluster_config);
            assert!(!cluster.is_null());
            assert_eq!(
                crate::cluster::hew_cluster_join(cluster, 2, c"10.0.0.1:9000".as_ptr()),
                0
            );
            assert_eq!(
                crate::cluster::hew_cluster_notify_connection_established_for_token(cluster, 2, 1),
                0
            );
            assert_eq!(
                crate::cluster::hew_cluster_notify_connection_lost_if_current(cluster, 2, 1),
                0
            );

            let mgr = hew_connmgr_new(transport_ptr, None, std::ptr::null_mut(), cluster, 1);
            assert!(!mgr.is_null());
            (&*callback_state).mgr.store(mgr, Ordering::Release);
            crate::cluster::hew_cluster_set_membership_callback(
                cluster,
                remove_on_joined,
                callback_state.cast(),
            );

            let mut actor = ConnectionActor::new(44);
            let publication_token = next_publication_token(&*mgr);
            actor.publication_token = publication_token;
            actor.peer_node_id = 2;
            actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            let publication_sync = Arc::clone(&actor.publication_sync);
            let publication_removed = Arc::clone(&actor.publication_removed);
            (&*mgr).connections.access(|conns| conns.push(actor));
            let superseded = test_reserve_unverified(&*mgr, 2, 44, publication_token);

            publish_connection_established(
                &*mgr,
                2,
                44,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication_token,
                &publication_sync,
                &publication_removed,
                superseded,
            );

            assert_eq!(
                remove_result_rx
                    .recv_timeout(std::time::Duration::from_secs(1))
                    .expect("reentrant remove should finish without deadlocking"),
                0
            );
            assert_eq!(
                (&*callback_state)
                    .events
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone(),
                vec![
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_JOINED),
                    (2, crate::cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT),
                ],
                "reentrant remove should observe ordered JOINED/SUSPECT delivery"
            );
            assert_eq!(hew_connmgr_count(mgr), 0);

            hew_connmgr_free(mgr);
            crate::cluster::hew_cluster_free(cluster);
            drop(Box::from_raw(transport_ptr));
            drop(Box::from_raw(callback_state));
        }
    }

    #[test]
    fn null_mgr_safety() {
        // All operations on null manager should return gracefully.
        // SAFETY: testing null safety.
        unsafe {
            let null_mgr: *mut HewConnMgr = std::ptr::null_mut();
            assert_eq!(hew_connmgr_count(null_mgr), -1);
            assert_eq!(hew_connmgr_last_activity(null_mgr, 0), 0);
            assert_eq!(hew_connmgr_conn_state(null_mgr, 0), CONN_STATE_CLOSED);
            hew_connmgr_free(null_mgr); // should not crash
        }
    }

    #[test]
    fn mgr_null_transport_rejected() {
        // SAFETY: testing null transport rejection.
        unsafe {
            let mgr = hew_connmgr_new(
                std::ptr::null_mut(),
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                0,
            );
            assert!(mgr.is_null());
        }
    }

    #[test]
    fn handshake_round_trip() {
        let hs = HewHandshake {
            protocol_version: HEW_PROTOCOL_VERSION,
            schema_hash: 0x1234_5678,
            feature_flags: HEW_FEATURE_SUPPORTS_GOSSIP,
            node_id: test_node_identity(42),
            session_incarnation: 7,
            static_noise_pubkey: [7; NOISE_STATIC_PUBKEY_LEN],
        };
        let encoded = hs.serialize();
        let decoded = HewHandshake::deserialize(&encoded).expect("valid handshake");
        assert_eq!(decoded, hs);
    }

    #[test]
    fn handshake_rejects_invalid_magic() {
        let mut bytes = [0u8; HEW_HANDSHAKE_SIZE];
        bytes.copy_from_slice(
            &local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize(),
        );
        bytes[0] = b'X';
        assert!(HewHandshake::deserialize(&bytes).is_none());
    }

    #[test]
    fn protocol_version_mismatch_rejected() {
        let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.protocol_version = local.protocol_version.wrapping_add(1);
        assert!(!version_compatible(&local, &peer));
    }

    #[test]
    fn handshake_rejects_future_protocol_version() {
        let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
        let peer = HewHandshake {
            protocol_version: 999,
            ..local
        };
        assert!(!version_compatible(&local, &peer));
    }

    #[test]
    fn schema_hash_mismatch_rejected() {
        let local = local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.schema_hash ^= 0x0100_0000;
        assert!(!schema_compatible(&local, &peer));
    }

    #[test]
    fn handshake_rejects_zero_session_and_nonzero_reserved_fields() {
        let encoded =
            local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();

        let mut zero_session = encoded;
        zero_session[32..36].fill(0);
        assert!(HewHandshake::deserialize(&zero_session).is_none());

        let mut reserved_header = encoded;
        reserved_header[6] = 1;
        assert!(HewHandshake::deserialize(&reserved_header).is_none());

        let mut reserved_identity = encoded;
        reserved_identity[36] = 1;
        assert!(HewHandshake::deserialize(&reserved_identity).is_none());
    }

    #[test]
    fn handshake_rejects_v1_length_and_magic() {
        let old = [0_u8; 48];
        assert!(HewHandshake::deserialize(&old).is_none());
        assert!(HewHandshake::deserialize(&[0_u8; 71]).is_none());
        assert!(HewHandshake::deserialize(&[0_u8; 73]).is_none());

        let mut encoded =
            local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();
        encoded[..4].copy_from_slice(b"HEW\x01");
        assert!(HewHandshake::deserialize(&encoded).is_none());
    }

    #[test]
    fn handshake_rejects_zero_node_id() {
        let mut encoded =
            local_handshake(test_node_identity(1), 1, [0; NOISE_STATIC_PUBKEY_LEN]).serialize();
        encoded[16..32].fill(0);
        assert!(HewHandshake::deserialize(&encoded).is_none());
    }

    #[test]
    fn local_schema_hash_covers_acknowledged_setup_controls() {
        assert_eq!(local_schema_hash(), 0x774b_20fa);
    }
}
