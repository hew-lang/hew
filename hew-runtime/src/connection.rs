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
// `encode_envelope_frame_from_raw_parts` is used only by the encryption-gated
// `encode_envelope`; import it conditionally so non-encryption builds don't
// carry an unused import.
#[cfg(feature = "encryption")]
use crate::envelope::encode_envelope_frame_from_raw_parts;
use crate::envelope::{
    decode_link_down_payload, decode_link_req_payload, decode_monitor_down_payload,
    decode_monitor_req_payload, decode_registry_gossip_payload, decode_swim_payload,
    decode_wire_frame, encode_control_frame, encode_registry_gossip_payload, encode_swim_payload,
    ControlFrame, RegistryGossipPayload, SwimControlPayload, SwimGossipEntry, WireFrame,
    CTRL_DEMONITOR, CTRL_LINK_DOWN, CTRL_LINK_REQ, CTRL_MONITOR_DOWN, CTRL_MONITOR_REQ,
    CTRL_REGISTRY_GOSSIP, CTRL_SWIM, CTRL_UNLINK, FRAME_TYPE_CONTROL, FRAME_TYPE_ENVELOPE,
    REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE, WIRE_VERSION,
};
use crate::lifetime::poison_safe::PoisonSafe;
use crate::mailbox_envelope::{validate_cross_node_send_params, MailboxPayloadClass};
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

const HEW_HANDSHAKE_SIZE: usize = 48;
const HEW_HANDSHAKE_MAGIC: [u8; 4] = *b"HEW\x01";
const HEW_PROTOCOL_VERSION: u16 = 1;
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
    node_id: u16,
    schema_hash: u32,
    feature_flags: u32,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
}

impl HewHandshake {
    fn serialize(self) -> [u8; HEW_HANDSHAKE_SIZE] {
        let mut out = [0u8; HEW_HANDSHAKE_SIZE];
        out[0..4].copy_from_slice(&HEW_HANDSHAKE_MAGIC);
        out[4..6].copy_from_slice(&self.protocol_version.to_be_bytes());
        out[6..8].copy_from_slice(&self.node_id.to_be_bytes());
        out[8..12].copy_from_slice(&self.schema_hash.to_be_bytes());
        out[12..16].copy_from_slice(&self.feature_flags.to_be_bytes());
        out[16..48].copy_from_slice(&self.static_noise_pubkey);
        out
    }

    fn deserialize(buf: &[u8]) -> Option<Self> {
        if buf.len() != HEW_HANDSHAKE_SIZE || buf[0..4] != HEW_HANDSHAKE_MAGIC {
            return None;
        }
        let mut static_noise_pubkey = [0u8; NOISE_STATIC_PUBKEY_LEN];
        static_noise_pubkey.copy_from_slice(&buf[16..48]);
        Some(Self {
            protocol_version: u16::from_be_bytes([buf[4], buf[5]]),
            node_id: u16::from_be_bytes([buf[6], buf[7]]),
            schema_hash: u32::from_be_bytes([buf[8], buf[9], buf[10], buf[11]]),
            feature_flags: u32::from_be_bytes([buf[12], buf[13], buf[14], buf[15]]),
            static_noise_pubkey,
        })
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
    /// Remote node identity from handshake.
    peer_node_id: u16,
    /// Remote capability bitfield from handshake.
    peer_feature_flags: u32,
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
    /// Spawn gate for inbound-ask workers, distinct from `reconnect_shutdown`.
    ///
    /// `hew_node_stop` sets this FIRST — before draining in-flight workers —
    /// so the drain terminates (no new workers spawn) while already-running
    /// `handle_inbound_ask` threads still flush their computed replies to the
    /// wire (those threads bail only on `reconnect_shutdown` / `CURRENT_NODE`,
    /// which are set AFTER the drain). Separating the gate from the teardown
    /// guard is what lets a graceful stop deliver in-flight replies instead of
    /// abandoning them as a spurious `ConnectionDropped`.
    ///
    /// This flag and `inbound_ask_active` form a Dekker pair accessed under
    /// `SeqCst` (see `node_inbound_router` and `drain_inbound_ask_workers`): the
    /// store here is ordered with the counter load in the drain such that a
    /// router which passes the gate is always visible to a concurrent drain, so
    /// no worker can spawn after the drain observed a zero counter.
    inbound_spawn_closed: Arc<AtomicBool>,
    /// Count of inbound-ask worker threads currently active for this manager.
    ///
    /// Incremented in `node_inbound_router` before the gate re-check (so a
    /// concurrent drain always sees a spawning worker), decremented by the
    /// worker's `InboundAskGuard` on exit. Used by `hew_node_stop` to drain
    /// workers before freeing node resources. Accessed under `SeqCst` on the
    /// spawn/drain path — see `inbound_spawn_closed`.
    pub(crate) inbound_ask_active: Arc<AtomicUsize>,
    /// Background reconnect worker handles.
    reconnect_workers: PoisonSafe<Vec<JoinHandle<()>>>,
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
            peer_node_id: 0,
            peer_feature_flags: 0,
            state: AtomicI32::new(CONN_STATE_CONNECTING),
            last_activity_ms: Arc::new(AtomicU64::new(0)),
            #[cfg(feature = "encryption")]
            noise_transport: Arc::new(Mutex::new(None)),
            reader_handle: None,
            reader_stop: Arc::new(AtomicI32::new(0)),
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

fn publish_connection_established(
    mgr: &HewConnMgr,
    peer_node_id: u16,
    conn_id: c_int,
    peer_feature_flags: u32,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
    publication_removed: &Arc<AtomicBool>,
) {
    if peer_node_id == 0 {
        return;
    }

    if publication_removed.load(Ordering::Acquire) {
        return;
    }

    let published = if mgr.cluster.is_null() {
        true
    } else {
        // The handshake is the first point where a bare-address dial learns the
        // peer's real node identity. Admit that authenticated identity before
        // publishing the connection so SWIM, routing, and registry gossip all
        // use the same NodeId instead of a caller-side placeholder.
        // SAFETY: cluster is owned by the live manager.
        unsafe { (&*mgr.cluster).admit_handshake_peer(peer_node_id) };
        // SAFETY: pointer validity is checked by the callee.
        unsafe {
            crate::cluster::hew_cluster_notify_connection_established_for_token_if_not_removed(
                mgr.cluster,
                peer_node_id,
                publication_token,
                publication_sync,
                publication_removed,
            ) == 1
        }
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
        unsafe { hew_routing_add_route(mgr.routing_table, peer_node_id, conn_id) };
    }

    flush_registry_gossip_to_connection(mgr, conn_id, peer_feature_flags);
}

fn retire_connection_publication(
    mgr: &HewConnMgr,
    peer_node_id: u16,
    conn_id: c_int,
    publication_token: u64,
    publication_sync: &Arc<Mutex<()>>,
) {
    if peer_node_id == 0 {
        return;
    }

    {
        let _publication = publication_sync.lock_or_recover();
        if !mgr.routing_table.is_null() {
            // SAFETY: pointer validity is checked by the callee.
            let _ = unsafe {
                hew_routing_remove_route_if_conn(mgr.routing_table, peer_node_id, conn_id)
            };
        }
    }
    if !mgr.cluster.is_null() {
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe {
            crate::cluster::hew_cluster_notify_connection_lost_if_current(
                mgr.cluster,
                peer_node_id,
                publication_token,
            )
        };
    }

    // Cross-node monitor connection-drop fan-out: the connection to
    // `peer_node_id` is gone, so every local watcher of an actor on that node
    // gets a MonitorLost DOWN — unless it already received a definitive
    // clean-exit / crash DOWN (only Pending slots are armed; exactly-once).
    crate::hew_node::fan_out_monitor_lost_for_node(peer_node_id);
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

        let Ok(target_addr) = std::ffi::CString::new(plan.target_addr.as_str()) else {
            set_last_error(format!(
                "hew_connmgr_reconnect: invalid reconnect address for dropped conn {dropped_conn_id}"
            ));
            return;
        };
        // SAFETY: mgr_ptr was checked non-null and remains valid for the connection lifetime.
        let connect_result = unsafe { connect_addr(mgr_ptr, &target_addr) };
        match connect_result {
            Ok(new_conn_id) => {
                // SAFETY: manager pointer is valid until shutdown and join in free.
                if unsafe { hew_connmgr_add(mgr_ptr, new_conn_id) } == 0 {
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
                    return;
                }
                // hew_connmgr_add owns conn_id cleanup on failure (closes the
                // transport connection on all failure paths), so no close here.
                set_last_error(format!(
                    "hew_connmgr_reconnect: failed to install reconnected conn on attempt {attempt}/{}, addr={}",
                    plan.max_retries, plan.target_addr
                ));
            }
            Err(err) => {
                set_last_error(format!(
                    "hew_connmgr_reconnect: attempt {attempt}/{} failed for dropped conn {dropped_conn_id}, addr={}: {err}",
                    plan.max_retries, plan.target_addr
                ));
            }
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
    fnv1a32_update(hash, &[1, 2, 3, 4, 5, 6, 7, 8])
}

fn local_handshake(
    local_node_id: u16,
    static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN],
) -> HewHandshake {
    HewHandshake {
        protocol_version: HEW_PROTOCOL_VERSION,
        node_id: local_node_id,
        schema_hash: local_schema_hash(),
        feature_flags: local_feature_flags(),
        static_noise_pubkey,
    }
}

fn version_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.protocol_version == peer.protocol_version
}

fn schema_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.schema_hash == peer.schema_hash
}

fn peer_identity_compatible(local_node_id: u16, peer_node_id: u16, expected: Option<u16>) -> bool {
    peer_node_id != 0
        && peer_node_id != local_node_id
        && expected.is_none_or(|expected| expected == peer_node_id)
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

// Sole caller is the encryption-gated send path in `hew_connmgr_send`.
#[cfg(feature = "encryption")]
unsafe fn encode_envelope(
    target_actor_id: u64,
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
            target_actor_id,
            0,
            msg_type,
            payload.cast_const(),
            payload_len,
            0,
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

fn encode_registry_gossip_control(name: &str, actor_id: u64, is_add: bool) -> Option<Vec<u8>> {
    let op = if is_add {
        crate::cluster::GOSSIP_REGISTRY_ADD
    } else {
        crate::cluster::GOSSIP_REGISTRY_REMOVE
    };
    let payload = RegistryGossipPayload {
        op,
        name: name.to_owned(),
        actor_id,
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
    control: &ControlFrame,
) {
    match control.ctrl_kind {
        CTRL_REGISTRY_GOSSIP => {}
        CTRL_SWIM => {
            handle_swim_control_frame(mgr, peer_feature_flags, conn_id, control);
            return;
        }
        CTRL_MONITOR_REQ => {
            handle_monitor_req_frame(mgr, conn_id, control);
            return;
        }
        CTRL_DEMONITOR => {
            handle_demonitor_frame(mgr, conn_id, control);
            return;
        }
        CTRL_MONITOR_DOWN => {
            handle_monitor_down_frame(mgr, conn_id, control);
            return;
        }
        CTRL_LINK_REQ => {
            handle_link_req_frame(mgr, conn_id, control);
            return;
        }
        CTRL_UNLINK => {
            handle_unlink_frame(mgr, conn_id, control);
            return;
        }
        CTRL_LINK_DOWN => {
            handle_link_down_frame(mgr, conn_id, control);
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
    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running.
    unsafe {
        (&*mgr_ref.cluster).apply_registry_event(&payload.name, payload.actor_id, is_add);
    }
}

fn authenticated_peer_node_id(mgr: *mut HewConnMgr, conn_id: c_int, context: &str) -> Option<u16> {
    if mgr.is_null() {
        set_last_error(format!("connection reader {context}: missing manager"));
        return None;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    let authenticated = peer_node_id_for_conn(mgr_ref, conn_id);
    if authenticated == 0 {
        set_last_error(format!(
            "connection reader {context}: missing authenticated peer for conn {conn_id}"
        ));
        return None;
    }
    Some(authenticated)
}

/// Handle an inbound `CTRL_MONITOR_REQ`: a remote node is monitoring
/// one of our local actors. Record a target-side remote-watcher entry so the
/// terminal sweep can fan out a `CTRL_MONITOR_DOWN` when that actor dies.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a watcher — no fabricated state from untrusted bytes.
fn handle_monitor_req_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some(authenticated) = authenticated_peer_node_id(mgr, conn_id, "monitor req") else {
        return;
    };
    if payload.watcher_node_id != authenticated {
        set_last_error(format!(
            "connection reader monitor req watcher_node_id {} does not match authenticated peer {authenticated}",
            payload.watcher_node_id
        ));
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("connection reader monitor req: no runtime installed");
        return;
    };
    rt.dist_monitors.register_remote_watcher(
        payload.target_serial,
        payload.watcher_node_id,
        payload.ref_id,
    );
}

/// Handle an inbound `CTRL_DEMONITOR`: a remote node retracted its
/// monitor of one of our local actors. Remove the target-side remote-watcher
/// entry. Idempotent / fail-closed on malformed input.
fn handle_demonitor_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_monitor_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader demonitor payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some(authenticated) = authenticated_peer_node_id(mgr, conn_id, "demonitor") else {
        return;
    };
    if payload.watcher_node_id != authenticated {
        set_last_error(format!(
            "connection reader demonitor watcher_node_id {} does not match authenticated peer {authenticated}",
            payload.watcher_node_id
        ));
        return;
    }
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return;
    };
    rt.dist_monitors.remove_remote_watcher(
        payload.target_serial,
        payload.watcher_node_id,
        payload.ref_id,
    );
}

/// Handle an inbound `CTRL_MONITOR_DOWN`: the node owning an actor we
/// monitor reports that actor reached a terminal state. Arm the watcher slot for
/// `ref_id` with the carried reason so the blocked `hew_node_monitor_recv` wakes.
///
/// Fail-closed on malformed input. Arming the slot removes it from the
/// connection-drop / SWIM-DEAD fan-out's reach (the slot is no longer
/// `Pending`), which is the exactly-once disambiguation: a definitive DOWN beats
/// a later partition signal for the same registration.
fn handle_monitor_down_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_monitor_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader monitor down payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some(authenticated) = authenticated_peer_node_id(mgr, conn_id, "monitor down") else {
        return;
    };
    let Some(rt) = crate::runtime::rt_current_opt() else {
        set_last_error("connection reader monitor down: no runtime installed");
        return;
    };
    rt.dist_monitors
        .deliver_to_ref(payload.ref_id, authenticated, payload.reason);
}

/// Handle an inbound `CTRL_LINK_REQ`: a remote node is linking one of our local
/// actors. Establish the bidirectional cross-node link.
///
/// Fail-closed: a malformed / oversized payload is dropped with `set_last_error`
/// and never registers a link — no fabricated state from untrusted bytes. The
/// decode bar is HIGHER than monitor because a registered link can later crash a
/// real actor.
fn handle_link_req_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link req payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some(authenticated) = authenticated_peer_node_id(mgr, conn_id, "link req") else {
        return;
    };
    if payload.linker_node_id != authenticated {
        set_last_error(format!(
            "connection reader link req linker_node_id {} does not match authenticated peer {authenticated}",
            payload.linker_node_id
        ));
        return;
    }
    crate::hew_node::handle_inbound_link_req(&payload, authenticated);
}

/// Handle an inbound `CTRL_UNLINK`: a remote node retracted a prior
/// link of one of our local actors. Idempotent / fail-closed on malformed input.
fn handle_unlink_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_link_req_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader unlink payload decode failure: {err}"
            ));
            return;
        }
    };
    let Some(authenticated) = authenticated_peer_node_id(mgr, conn_id, "unlink") else {
        return;
    };
    if payload.linker_node_id != authenticated {
        set_last_error(format!(
            "connection reader unlink linker_node_id {} does not match authenticated peer {authenticated}",
            payload.linker_node_id
        ));
        return;
    }
    crate::hew_node::handle_inbound_unlink(&payload, authenticated);
}

/// Handle an inbound `CTRL_LINK_DOWN`: the node owning an actor we LINK
/// reports it reached a terminal state. Fire the cross-node link cascade —
/// synthesize a `SYS_MSG_EXIT` into the LOCAL linked actor's mailbox and crash it
/// (for `CrashLinked`). THE divergence from a monitor DOWN (which arms a recv
/// slot): a link DOWN crashes the linked actor through its mailbox. Fail-closed
/// on malformed input; the EXIT fires exactly once and ONLY for a link entry
/// this node registered AND ONLY when the handshake-authenticated sender of this
/// connection is the same peer that entry is linked to — otherwise neither a
/// forged `ref_id` this node never linked NOR a different, genuinely-connected
/// peer that merely guessed/learned a pending link `ref_id` can crash an actor
/// linked to another, still-alive peer.
fn handle_link_down_frame(mgr: *mut HewConnMgr, conn_id: c_int, control: &ControlFrame) {
    let payload = match decode_link_down_payload(&control.payload) {
        Ok(payload) => payload,
        Err(err) => {
            set_last_error(format!(
                "connection reader link down payload decode failure: {err}"
            ));
            return;
        }
    };
    if mgr.is_null() {
        set_last_error("connection reader link down frame missing manager");
        return;
    }
    // SAFETY: reader_loop owns a live manager pointer for this connection.
    let mgr_ref = unsafe { &*mgr };
    // `deliver_link_down_to_ref` rejects `authenticated_peer == 0` explicitly
    // before comparing it with the stored link peer.
    let authenticated = peer_node_id_for_conn(mgr_ref, conn_id);
    crate::hew_node::handle_inbound_link_down(payload.ref_id, authenticated, payload.reason);
}

fn active_gossip_connection_ids(mgr: &HewConnMgr) -> Vec<c_int> {
    mgr.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| {
                c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && supports_gossip(c.peer_feature_flags)
            })
            .map(|c| c.conn_id)
            .collect()
    })
}

fn flush_registry_gossip_to_connection(mgr: &HewConnMgr, conn_id: c_int, peer_feature_flags: u32) {
    if !supports_gossip(peer_feature_flags) || mgr.cluster.is_null() {
        return;
    }

    // SAFETY: cluster pointer belongs to this live connection manager.
    let events = unsafe { (&*mgr.cluster).take_registry_gossip(MAX_REGISTRY_GOSSIP_FLUSH_EVENTS) };
    for event in events {
        let Some(bytes) = encode_registry_gossip_control(&event.name, event.actor_id, event.is_add)
        else {
            continue;
        };
        // SAFETY: mgr is live and `bytes` is a complete encoded control frame.
        if unsafe { send_preencoded_on_manager(mgr, conn_id, bytes.as_ptr(), bytes.len()) } != 0 {
            set_last_error(format!(
                "registry gossip flush send failed for conn {conn_id}"
            ));
        }
    }
}

// ── SWIM failure-detection transport ────────────────────────────────────

/// Resolve the authenticated peer node ID for an active connection.
///
/// Returns `0` if the connection is not active or is not registered. SWIM
/// uses this to cross-check a frame's claimed `from_node` against the identity
/// established during the handshake.
pub(crate) fn peer_node_id_for_conn(mgr: &HewConnMgr, conn_id: c_int) -> u16 {
    mgr.connections.access(|conns| {
        conns
            .iter()
            .find(|c| c.conn_id == conn_id && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map_or(0, |c| c.peer_node_id)
    })
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
fn collect_swim_gossip(cluster: &HewCluster) -> Vec<SwimGossipEntry> {
    cluster
        .take_swim_gossip(cluster.max_gossip_per_msg())
        .into_iter()
        .map(|(node_id, state, incarnation)| SwimGossipEntry {
            node_id,
            state,
            incarnation,
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
        from_node: mgr.local_node_id,
        incarnation: cluster.local_incarnation(),
        target_node,
        gossip: collect_swim_gossip(cluster),
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

    // SAFETY: cluster pointer is owned by the live manager.
    let cluster = unsafe { &*mgr_ref.cluster };
    let Some(bytes) = build_swim_frame(mgr_ref, cluster, msg_type, target_node) else {
        return -1;
    };
    // SAFETY: manager is live, bytes is a complete encoded control frame.
    if unsafe { send_preencoded_on_manager(mgr_ref, conn_id, bytes.as_ptr(), bytes.len()) } == 0 {
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
    mgr_ref.connections.access(|conns| {
        conns
            .iter()
            .filter(|c| {
                c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && supports_gossip(c.peer_feature_flags)
                    && c.peer_node_id != 0
            })
            .map(|c| c.peer_node_id)
            .collect()
    })
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

    // Cross-attribution defence: the frame's claimed sender must equal the
    // authenticated handshake identity of the connection it arrived on.
    let authenticated = peer_node_id_for_conn(mgr_ref, conn_id);
    if authenticated == 0 || authenticated != payload.from_node {
        set_last_error(format!(
            "connection reader SWIM frame from_node {} does not match authenticated peer {authenticated}",
            payload.from_node
        ));
        return;
    }

    let gossip: Vec<(u16, i32, u64)> = payload
        .gossip
        .iter()
        .map(|e| (e.node_id, e.state, e.incarnation))
        .collect();

    // SAFETY: cluster pointer is owned by the live node/manager and remains
    // valid while the reader thread is running. All driven methods
    // (apply_swim_gossip / refute_if_suspected / process_message) take &self
    // and synchronize internally via Mutex/Atomic, so a shared reference is
    // sound even with the SWIM driver thread concurrently calling tick().
    let cluster = unsafe { &*mgr_ref.cluster };

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
        payload.from_node,
        payload.incarnation,
        authenticated,
    );

    // Respond per message type.
    match payload.msg_type {
        crate::cluster::SWIM_MSG_PING => {
            // Direct probe: ACK the sender.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, payload.from_node, crate::cluster::SWIM_MSG_ACK, 0)
            };
        }
        crate::cluster::SWIM_MSG_PING_REQ
            if payload.target_node != 0 && payload.target_node != mgr_ref.local_node_id =>
        {
            // C4 indirect probing: forward a real PING to the probe target so
            // the relay actually exercises the target's liveness, then the
            // target's ACK propagates membership back via gossip. A no-op if
            // we have no active connection to the target.
            // SAFETY: manager is live for this call.
            let _ = unsafe {
                hew_connmgr_send_swim(mgr, payload.target_node, crate::cluster::SWIM_MSG_PING, 0)
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
        return Some(local.node_id < peer.node_id);
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
fn reader_loop(
    mgr: SendConnMgr,
    transport: SendTransport,
    conn_id: c_int,
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
                handle_control_frame(mgr, peer_feature_flags, conn_id, &control);
            }
            WireFrame::Envelope(mut envelope) => {
                let Some(router_fn) = router else {
                    continue;
                };

                // Reply envelopes (request_id > 0, source_node_id == 0) are
                // deposited directly into the reply routing table, bypassing
                // the normal inbound router.
                if envelope.request_id > 0 && envelope.source_node_id == 0 {
                    if is_ask_rejection_reply(envelope.msg_type, peer_feature_flags) {
                        // Rejection reply: the remote node hit its inbound
                        // ask path and sent an encoded AskError reason.
                        // Mark the pending ask as failed so the originating
                        // caller gets the precise remote rejection reason.
                        //
                        // The `supports_ask_rejection` guard ensures that
                        // old nodes (which never send this sentinel) cannot
                        // accidentally trigger this path even if they happen
                        // to send a message with msg_type = 65535.
                        crate::hew_node::fail_remote_reply(
                            envelope.request_id,
                            envelope.payload.as_slice(),
                        );
                    } else {
                        crate::hew_node::complete_remote_reply(
                            envelope.request_id,
                            envelope.payload.as_slice(),
                        );
                    }
                } else {
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
                            envelope.target_actor_id,
                            envelope.msg_type,
                            payload_ptr,
                            envelope.payload.len(),
                            envelope.request_id,
                            envelope.source_node_id,
                            mgr,
                        );
                    }
                    if let Some(saved) = saved_ctx {
                        crate::tracing::io_recv_span_end(saved);
                    }
                }
            }
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
    cabi_guard!(transport.is_null(), std::ptr::null_mut());
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
        reader_lifecycle: Arc::new(ReaderLifecycle::default()),
        next_publication_token: AtomicU64::new(1),
        local_node_id,
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

        // Drain the connection list and close each transport connection.
        // The closure scope releases the lock before the actors are dropped,
        // so Drop does not race the explicit close below.
        let drained: Vec<ConnectionActor> = mgr.connections.access(std::mem::take);

        for conn in drained {
            // Signal expected stop BEFORE closing the transport so the reader
            // that unblocks from the socket shutdown observes stop_flag == 1
            // (expected-stop path) and does not call hew_connmgr_remove.
            // The Release ordering pairs with the Acquire in reader_cleanup.
            conn.reader_stop.store(1, Ordering::Release);
            // Mark closed before the explicit close so Drop does not
            // double-close when the actor is subsequently dropped.
            conn.transport_closed.store(true, Ordering::Release);
            // Close transport so a reader blocked in recv() unblocks.
            // SAFETY: transport is valid per manager contract.
            unsafe {
                let t = &*transport;
                if let Some(ops) = t.ops.as_ref() {
                    if let Some(close_fn) = ops.close_conn {
                        close_fn(t.r#impl, conn.conn_id);
                    }
                }
            }
            // ConnectionActor::drop joins the reader thread (idempotently
            // sets reader_stop again and guards against double-close via
            // transport_closed, which was already set above).
        }
        mgr.reader_lifecycle.wait_for_idle();
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

/// Return a clone of the inbound-ask spawn-gate flag for `node_inbound_router`
/// to consult before spawning a worker.
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

/// Return a clone of the per-manager inbound-ask active counter.
///
/// Used by `node_inbound_router` to track workers for this specific manager
/// and by `hew_node_stop` to drain them before freeing node resources.
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
        let Ok(pattern) = NOISE_PATTERN.parse() else {
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error("hew_connmgr_add: invalid noise pattern");
            return -1;
        };
        let builder = snow::Builder::new(pattern);
        let Ok(keypair) = builder.generate_keypair() else {
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error("hew_connmgr_add: failed to generate noise keypair");
            return -1;
        };
        local_noise_pubkey.copy_from_slice(&keypair.public);
        Zeroizing::new(keypair.private)
    };

    let local_hs = local_handshake(mgr.local_node_id, local_noise_pubkey);
    // SAFETY: mgr.transport and conn_id are valid per caller contract; local_hs is stack-local.
    let Some(peer_hs) = (unsafe { handshake_exchange(mgr.transport, conn_id, local_hs) }) else {
        // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        return -1;
    };
    if !peer_identity_compatible(mgr.local_node_id, peer_hs.node_id, expected_peer_node_id) {
        // SAFETY: mgr.transport and conn_id are valid per caller contract.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        if peer_hs.node_id == 0 {
            set_last_error(format!(
                "hew_connmgr_add: peer handshake used reserved node id 0 for conn {conn_id}"
            ));
        } else if peer_hs.node_id == mgr.local_node_id {
            set_last_error(format!(
                "hew_connmgr_add: peer node id {} collides with local node id for conn {conn_id}",
                peer_hs.node_id
            ));
        } else {
            set_last_error(format!(
                "hew_connmgr_add: peer node id {} does not match expected node id {} for conn {conn_id}",
                peer_hs.node_id,
                expected_peer_node_id.unwrap_or(0)
            ));
        }
        return -1;
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
        if !crate::encryption::hew_allowlist_check_active_peer(&peer_static_pubkey) {
            // SAFETY: mgr.transport and conn_id are valid per caller contract of hew_connmgr_add.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: peer key not allowlisted for conn {conn_id}"
            ));
            return -1;
        }
        Some(noise)
    } else {
        None
    };

    let mut actor = ConnectionActor::new(conn_id);
    actor.transport = mgr.transport;
    actor.publication_token = next_publication_token(mgr);
    actor.peer_node_id = peer_hs.node_id;
    actor.peer_feature_flags = peer_hs.feature_flags;
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
            set_last_error(format!(
                "hew_connmgr_add: manager shutdown won install race for conn {conn_id}"
            ));
            return -1;
        }
        Err(ConnectionInstallError::Duplicate) => {
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} became duplicate during install"
            ));
            return -1;
        }
    };

    publish_connection_established(
        mgr,
        peer_hs.node_id,
        conn_id,
        peer_hs.feature_flags,
        publication_token,
        &publication_sync,
        &publication_removed,
    );

    0
}

/// Remove a connection from the manager and close it.
///
/// Returns 0 on success, -1 if not found.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
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
            conn.publication_token,
            Arc::clone(&conn.publication_sync),
            Arc::clone(&conn.publication_removed),
        ))
    });
    let Some((peer_node_id, publication_token, publication_sync, publication_removed)) =
        publication_data
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
        if !mgr.routing_table.is_null() && peer_node_id != 0 {
            // SAFETY: routing_table is valid per manager contract.
            let _ = unsafe {
                hew_routing_remove_route_if_conn(mgr.routing_table, peer_node_id, conn_id)
            };
        }
    }

    // Step 2b: remove the connection from the list. The connections lock is
    // released before waking the reader — the reader-cleanup path can re-enter
    // hew_connmgr_remove on an unexpected drop; releasing here prevents deadlock.
    // A second concurrent call (reader re-entry) will find conn_id gone and
    // return 0 harmlessly.
    let conn = mgr.connections.access(|conns| {
        let idx = conns.iter().position(|c| c.conn_id == conn_id)?;
        let conn = conns.swap_remove(idx);
        conn.state.store(CONN_STATE_CLOSED, Ordering::Release);
        Some(conn)
    });
    let Some(conn) = conn else {
        // Already removed by a concurrent hew_connmgr_remove call (reader re-entry).
        return 0;
    };

    // Mark the reader as explicitly stopped before closing the transport so an
    // awakened reader does not treat this teardown as an unexpected drop.
    conn.reader_stop.store(1, Ordering::Release);
    // Mark closed before the explicit close so Drop does not double-close.
    conn.transport_closed.store(true, Ordering::Release);
    // Close the transport connection so a blocking recv unblocks.
    // SAFETY: transport is valid per manager contract.
    unsafe { close_transport_conn(mgr.transport, conn_id) };
    // Drop joins the reader thread after transport close.
    drop(conn);
    // Full publication retirement: re-removes route (idempotent) and fires the
    // cluster connection-lost notification. Deferred until after drop(conn) so
    // that a racing replacement connection can establish and publish first —
    // its higher publication token then suppresses this notification.
    retire_connection_publication(
        mgr,
        peer_node_id,
        conn_id,
        publication_token,
        &publication_sync,
    );

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
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_send(
    mgr: *mut HewConnMgr,
    conn_id: c_int,
    target_actor_id: u64,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null(), -1);
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Verify connection exists, is active, and targets the correct peer node.
    // The peer_node_id check prevents sends to recycled conn_ids that now
    // belong to a different node (conn_id reuse safety).
    let target_node_id = crate::pid::hew_pid_node(target_actor_id);
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let ok = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id
                    && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && (target_node_id == 0 || c.peer_node_id == target_node_id)
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
    if let Some(noise_transport) = maybe_noise {
        // SAFETY: data is valid for size bytes per caller contract of hew_connmgr_send.
        let Some(encoded) = (unsafe {
            encode_envelope(
                target_actor_id,
                msg_type,
                data,
                size,
                MailboxPayloadClass::SerializedCrossNode as u8,
                0,
            )
        }) else {
            return -1;
        };
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

    // Fix 5: guard the plaintext send path with the same validator that the
    // encrypted branch runs inside `encode_envelope`.  Callers of
    // `hew_connmgr_send` must already have serialised the payload (enforced at
    // the `hew_node_send` level), so the class is truthfully SerializedCrossNode.
    if validate_cross_node_send_params(
        MailboxPayloadClass::SerializedCrossNode as u8,
        crate::mailbox_envelope::CANCEL_TOKEN_NONE,
    )
    .is_none()
    {
        return -1;
    }
    // SAFETY: mgr_ref.transport is valid per caller contract; conn_id verified active above;
    //         data is valid for size bytes per caller contract.
    let rc = unsafe {
        crate::transport::wire_send_envelope(
            mgr_ref.transport,
            conn_id,
            target_actor_id,
            0,
            msg_type,
            data,
            size,
        )
    };
    if rc != 0 {
        -1
    } else {
        0
    }
}

unsafe fn send_preencoded_on_manager(
    mgr_ref: &HewConnMgr,
    conn_id: c_int,
    data: *const u8,
    len: usize,
) -> c_int {
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        #[cfg(feature = "encryption")]
        let mut noise_out = None::<Arc<Mutex<Option<snow::TransportState>>>>;
        let ok = mgr_ref.connections.access(|conns| {
            let active = conns.iter().find(|c| {
                c.conn_id == conn_id && c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
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
    unsafe { send_preencoded_on_manager(mgr_ref, conn_id, data, len) }
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
    actor_id: u64,
    is_add: bool,
) -> c_int {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees manager pointer validity.
    let mgr_ref = unsafe { &*mgr };
    let Some(bytes) = encode_registry_gossip_control(name, actor_id, is_add) else {
        return 0;
    };

    let conn_ids = active_gossip_connection_ids(mgr_ref);
    let mut success_count: c_int = 0;
    for conn_id in conn_ids {
        // SAFETY: manager is live and bytes is a complete encoded control frame.
        if unsafe { send_preencoded_on_manager(mgr_ref, conn_id, bytes.as_ptr(), bytes.len()) } == 0
        {
            success_count += 1;
        } else {
            set_last_error(format!(
                "registry gossip broadcast send failed for conn {conn_id}"
            ));
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
    target_actor_id: u64,
    msg_type: i32,
    data: *mut u8,
    size: usize,
) -> c_int {
    cabi_guard!(mgr.is_null(), 0);
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
        let rc = unsafe { hew_connmgr_send(mgr, cid, target_actor_id, msg_type, data, size) };
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
            reader_lifecycle: Arc::new(ReaderLifecycle::default()),
            next_publication_token: AtomicU64::new(1),
            local_node_id: 0,
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
    fn peer_identity_validation_rejects_reserved_colliding_and_unexpected_ids() {
        assert!(peer_identity_compatible(10, 20, None));
        assert!(peer_identity_compatible(10, 20, Some(20)));
        assert!(!peer_identity_compatible(10, 0, None));
        assert!(!peer_identity_compatible(10, 10, None));
        assert!(!peer_identity_compatible(10, 20, Some(21)));
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
            reader_lifecycle: Arc::new(ReaderLifecycle::default()),
            next_publication_token: AtomicU64::new(1),
            local_node_id: 1,
        };
        let mgr_ptr = std::ptr::from_ref(&mgr).cast_mut();
        let payload = crate::envelope::LinkReqPayload {
            linker_node_id: 9,
            ref_id: 123,
            target_serial: 77,
            linker_serial: 88,
            policy_tag: 1,
            reciprocate: 0,
        };

        let link_req = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_LINK_REQ,
            payload: crate::envelope::encode_link_req_payload(&payload)
                .expect("link request payload must encode"),
        };
        handle_control_frame(mgr_ptr, 0, 10, &link_req);
        assert!(
            crate::runtime::rt_current()
                .dist_monitors
                .take_remote_watchers(payload.target_serial)
                .is_empty(),
            "a link request claiming another node must not register a watcher"
        );

        crate::runtime::rt_current()
            .dist_monitors
            .register_remote_link_watcher(payload.target_serial, 2, payload.ref_id);
        let unlink = ControlFrame {
            version: WIRE_VERSION,
            ctrl_kind: CTRL_UNLINK,
            payload: crate::envelope::encode_link_req_payload(&payload)
                .expect("unlink payload must encode"),
        };
        handle_control_frame(mgr_ptr, 0, 10, &unlink);

        let watchers = crate::runtime::rt_current()
            .dist_monitors
            .take_remote_watchers(payload.target_serial);
        assert_eq!(
            watchers.len(),
            1,
            "mismatched unlink must not remove watcher"
        );
        assert_eq!(watchers[0].watcher_node_id, 2);
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
            gossip_peer
                .state
                .store(CONN_STATE_ACTIVE, Ordering::Release);

            let mut old_peer = ConnectionActor::new(11);
            old_peer.peer_node_id = 3;
            old_peer.peer_feature_flags = 0;
            old_peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);

            let mut draining_peer = ConnectionActor::new(12);
            draining_peer.peer_node_id = 4;
            draining_peer.peer_feature_flags = HEW_FEATURE_SUPPORTS_GOSSIP;
            draining_peer
                .state
                .store(CONN_STATE_DRAINING, Ordering::Release);

            (&*mgr).connections.access(|conns| {
                conns.push(gossip_peer);
                conns.push(old_peer);
                conns.push(draining_peer);
            });

            let actor_id = crate::pid::hew_pid_make(2, 0x42);
            assert_eq!(
                hew_connmgr_broadcast_registry_gossip(mgr, "worker", actor_id, true),
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
                assert_eq!(payload.actor_id, actor_id);
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
    /// guard rejects a frame whose `from_node` does not match the handshake
    /// identity of the connection it arrived on.
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
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));

            // Honest PING from node 2 (matches the conn's authenticated id).
            let ping = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_PING,
                from_node: 2,
                incarnation: 1,
                target_node: 0,
                gossip: vec![],
            };
            let frame = decode_wire_frame(&swim_control_frame_bytes(&ping)).expect("frame");
            let WireFrame::Control(control) = frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, &control);

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
                assert_eq!(ack.from_node, 1, "ACK is stamped with our local node id");
            }

            // Spoofed PING claiming to be node 9 on node 2's connection is
            // rejected — no further send.
            let spoof = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_PING,
                from_node: 9,
                incarnation: 1,
                target_node: 0,
                gossip: vec![],
            };
            let spoof_frame = decode_wire_frame(&swim_control_frame_bytes(&spoof)).expect("frame");
            let WireFrame::Control(spoof_control) = spoof_frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, &spoof_control);
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
            peer.state.store(CONN_STATE_ACTIVE, Ordering::Release);
            (&*mgr).connections.access(|conns| conns.push(peer));

            // ACK from node 2 carrying DEAD-about-node-3 gossip.
            let ack = SwimControlPayload {
                msg_type: crate::cluster::SWIM_MSG_ACK,
                from_node: 2,
                incarnation: 1,
                target_node: 0,
                gossip: vec![SwimGossipEntry {
                    node_id: 3,
                    state: crate::cluster::MEMBER_DEAD,
                    incarnation: 5,
                }],
            };
            let frame = decode_wire_frame(&swim_control_frame_bytes(&ack)).expect("frame");
            let WireFrame::Control(control) = frame else {
                panic!("expected control frame");
            };
            handle_swim_control_frame(mgr, HEW_FEATURE_SUPPORTS_GOSSIP, 10, &control);

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

    /// Defense-in-depth: `ConnectionActor::drop` must close the transport
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
            let routing_table = crate::routing::hew_routing_table_new(1);
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
            publish_connection_established(
                &*mgr,
                2,
                11,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                old_token,
                &old_publication_sync,
                &old_publication_removed,
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
            publish_connection_established(
                &*mgr,
                2,
                22,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                replacement_token,
                &replacement_publication_sync,
                &replacement_publication_removed,
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
        let routing_table = unsafe { crate::routing::hew_routing_table_new(1) };
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
            publish_connection_established(
                &*mgr,
                2,
                31,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication_token,
                &publication_sync,
                &publication_removed,
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
                crate::routing::hew_routing_table_new(1)
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

            publish_connection_established(
                &*mgr,
                2,
                44,
                HEW_FEATURE_SUPPORTS_GOSSIP,
                publication_token,
                &publication_sync,
                &publication_removed,
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
            node_id: 42,
            schema_hash: 0x1234_5678,
            feature_flags: HEW_FEATURE_SUPPORTS_GOSSIP,
            static_noise_pubkey: [7; NOISE_STATIC_PUBKEY_LEN],
        };
        let encoded = hs.serialize();
        let decoded = HewHandshake::deserialize(&encoded).expect("valid handshake");
        assert_eq!(decoded, hs);
    }

    #[test]
    fn handshake_rejects_invalid_magic() {
        let mut bytes = [0u8; HEW_HANDSHAKE_SIZE];
        bytes.copy_from_slice(&local_handshake(0, [0; NOISE_STATIC_PUBKEY_LEN]).serialize());
        bytes[0] = b'X';
        assert!(HewHandshake::deserialize(&bytes).is_none());
    }

    #[test]
    fn protocol_version_mismatch_rejected() {
        let local = local_handshake(0, [0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.protocol_version = local.protocol_version.wrapping_add(1);
        assert!(!version_compatible(&local, &peer));
    }

    #[test]
    fn handshake_rejects_future_protocol_version() {
        let local = local_handshake(0, [0; NOISE_STATIC_PUBKEY_LEN]);
        let peer = HewHandshake {
            protocol_version: 999,
            ..local
        };
        assert!(!version_compatible(&local, &peer));
    }

    #[test]
    fn schema_hash_mismatch_rejected() {
        let local = local_handshake(0, [0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.schema_hash ^= 0x0100_0000;
        assert!(!schema_compatible(&local, &peer));
    }

    #[test]
    fn local_schema_hash_is_not_placeholder() {
        assert_ne!(local_schema_hash(), FNV1A32_OFFSET_BASIS);
    }
}
