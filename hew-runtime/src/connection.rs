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

use std::ffi::{c_char, c_int, CStr};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::cluster::HewCluster;
use crate::routing::{hew_routing_add_route, hew_routing_remove_route_if_conn, HewRoutingTable};
use crate::set_last_error;
use crate::transport::{HewTransport, HEW_CONN_INVALID};
use crate::util::MutexExt;
use crate::wire::{
    hew_wire_buf_free, hew_wire_buf_init, hew_wire_buf_init_read, hew_wire_decode_envelope,
    hew_wire_encode_envelope, HewWireBuf, HewWireEnvelope, HBF_FLAG_COMPRESSED, HBF_MAGIC,
    HBF_VERSION, HEW_WIRE_FIXED32, HEW_WIRE_LENGTH_DELIMITED, HEW_WIRE_VARINT,
};

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
const HEW_FEATURE_SUPPORTS_ENCRYPTION: u32 = 1 << 0;
const HEW_FEATURE_SUPPORTS_GOSSIP: u32 = 1 << 1;
// Bit 2 (HEW_FEATURE_SUPPORTS_REMOTE_SPAWN) is reserved; not advertised until a
// bootstrap-based remote-spawn path is implemented.
/// Indicates that this node understands `HEW_REPLY_REJECT_MSG_TYPE = 65535` in
/// reply envelopes.  A node MUST only send the rejection sentinel to peers that
/// advertise this flag; old nodes would misinterpret it as a void-success reply.
pub(crate) const HEW_FEATURE_SUPPORTS_ASK_REJECTION: u32 = 1 << 3;
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
    /// Active connections (protected by mutex for concurrent add/remove).
    connections: Mutex<Vec<ConnectionActor>>,
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
    /// Count of inbound-ask worker threads currently active for this manager.
    ///
    /// Incremented in `node_inbound_router` before spawning; decremented by
    /// the worker's `InboundAskGuard` on exit.  Used by `hew_node_stop` to
    /// drain workers before freeing node resources.
    pub(crate) inbound_ask_active: Arc<AtomicUsize>,
    /// Background reconnect worker handles.
    reconnect_workers: Mutex<Vec<JoinHandle<()>>>,
    /// Monotonic token generator for connection-lifecycle publications.
    next_publication_token: AtomicU64,
    /// The node ID advertised in the handshake for this manager's node.
    /// Stored explicitly so multi-node tests (two nodes in one process) get the
    /// correct ID in their outgoing handshake even when `LOCAL_NODE_ID` refers
    /// to a different (`CURRENT_NODE`) node.
    pub(crate) local_node_id: u16,
}

#[derive(Clone, Debug)]
struct ReconnectSettings {
    target_addr: String,
    max_retries: u32,
}

#[derive(Clone, Debug)]
struct ReconnectPlan {
    target_addr: String,
    max_retries: u32,
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
    MutexPoisoned,
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
    let mut workers = mgr.reconnect_workers.lock_or_recover();
    let mut idx = 0usize;
    while idx < workers.len() {
        if workers[idx].is_finished() {
            let handle = workers.swap_remove(idx);
            let _ = handle.join();
        } else {
            idx += 1;
        }
    }
}

fn next_publication_token(mgr: &HewConnMgr) -> u64 {
    mgr.next_publication_token.fetch_add(1, Ordering::Relaxed)
}

fn publish_connection_established(
    mgr: &HewConnMgr,
    peer_node_id: u16,
    conn_id: c_int,
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
}

fn reconnect_plan(mgr: &HewConnMgr, conn_id: c_int) -> Option<ReconnectPlan> {
    if !mgr.reconnect_enabled.load(Ordering::Acquire)
        || mgr.reconnect_shutdown.load(Ordering::Acquire)
    {
        return None;
    }
    let conns = mgr.connections.lock_or_recover();
    let conn = conns.iter().find(|c| c.conn_id == conn_id)?;
    let reconnect = conn.reconnect.as_ref()?;
    Some(ReconnectPlan {
        target_addr: reconnect.target_addr.clone(),
        max_retries: reconnect.max_retries.max(1),
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
            let mut workers = mgr_ref.reconnect_workers.lock_or_recover();
            workers.push(worker);
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
    hash = fnv1a32_update(hash, &HBF_MAGIC);
    hash = fnv1a32_update(hash, &[HBF_VERSION]);
    hash = fnv1a32_update(hash, &[HBF_FLAG_COMPRESSED]);
    hash = fnv1a32_update(hash, &HEW_WIRE_VARINT.to_le_bytes());
    hash = fnv1a32_update(hash, &HEW_WIRE_LENGTH_DELIMITED.to_le_bytes());
    fnv1a32_update(hash, &HEW_WIRE_FIXED32.to_le_bytes())
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
    let install = match mgr.connections.lock() {
        Ok(mut conns) => {
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
        }
        Err(_) => Err(ConnectionInstallError::MutexPoisoned),
    };
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

unsafe fn encode_envelope(
    target_actor_id: u64,
    msg_type: i32,
    payload: *mut u8,
    payload_len: usize,
) -> Option<Vec<u8>> {
    #[expect(clippy::cast_possible_truncation, reason = "payload bounded by caller")]
    let env = HewWireEnvelope {
        target_actor_id,
        source_actor_id: 0,
        msg_type,
        payload_size: payload_len as u32,
        payload,
        request_id: 0,
        source_node_id: 0,
    };
    // SAFETY: zeroed is valid for HewWireBuf.
    let mut wire_buf: HewWireBuf = unsafe { std::mem::zeroed() };
    // SAFETY: wire_buf is a valid stack allocation.
    unsafe { hew_wire_buf_init(&raw mut wire_buf) };
    // SAFETY: pointers are valid for the duration of the call.
    if unsafe { hew_wire_encode_envelope(&raw mut wire_buf, &raw const env) } != 0 {
        // SAFETY: wire_buf was initialised above.
        unsafe { hew_wire_buf_free(&raw mut wire_buf) };
        return None;
    }
    // SAFETY: wire_buf.data points to wire_buf.len readable bytes until free.
    let bytes = unsafe { std::slice::from_raw_parts(wire_buf.data, wire_buf.len) }.to_vec();
    // SAFETY: wire_buf was initialised above.
    unsafe { hew_wire_buf_free(&raw mut wire_buf) };
    Some(bytes)
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
#[expect(
    clippy::too_many_arguments,
    reason = "reader_loop captures all per-connection state; splitting into a struct \
              would require unsafe Send impls for the contained raw pointers"
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

        let mut payload_ptr = buf.as_mut_ptr();
        let mut payload_len = read_len;
        #[cfg(feature = "encryption")]
        {
            let mut decrypted = vec![0u8; read_len];
            let mut guard = noise_transport.lock_or_recover();
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.read_message(&buf[..read_len], &mut decrypted) else {
                    set_last_error("connection decrypt failure".to_string());
                    reader_cleanup(mgr, conn_id, &stop_flag);
                    break;
                };
                payload_len = n;
                buf[..payload_len].copy_from_slice(&decrypted[..payload_len]);
                payload_ptr = buf.as_mut_ptr();
            }
        }

        // Update heartbeat.
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { crate::io_time::hew_now_ms() };
        last_activity.store(now, Ordering::Release);

        // Decode envelope and route.
        if let Some(router_fn) = router {
            // SAFETY: buf contains `read_len` valid bytes from recv.
            unsafe {
                let mut wire_buf: HewWireBuf = std::mem::zeroed();
                hew_wire_buf_init_read(&raw mut wire_buf, payload_ptr.cast(), payload_len);

                let mut envelope: HewWireEnvelope = std::mem::zeroed();
                let rc = hew_wire_decode_envelope(&raw mut wire_buf, &raw mut envelope);
                if rc == 0 {
                    // Reply envelopes (request_id > 0, source_node_id == 0) are
                    // deposited directly into the reply routing table, bypassing
                    // the normal inbound router.
                    if envelope.request_id > 0 && envelope.source_node_id == 0 {
                        if is_ask_rejection_reply(envelope.msg_type, peer_feature_flags) {
                            let reason_payload =
                                if envelope.payload_size > 0 && !envelope.payload.is_null() {
                                    std::slice::from_raw_parts(
                                        envelope.payload,
                                        envelope.payload_size as usize,
                                    )
                                } else {
                                    &[]
                                };
                            // Rejection reply: the remote node hit its inbound
                            // ask path and sent an encoded AskError reason.
                            // Mark the pending ask as failed so the originating
                            // caller gets the precise remote rejection reason.
                            //
                            // The `supports_ask_rejection` guard ensures that
                            // old nodes (which never send this sentinel) cannot
                            // accidentally trigger this path even if they happen
                            // to send a message with msg_type = 65535.
                            crate::hew_node::fail_remote_reply(envelope.request_id, reason_payload);
                        } else {
                            let reply_payload =
                                if envelope.payload_size > 0 && !envelope.payload.is_null() {
                                    std::slice::from_raw_parts(
                                        envelope.payload,
                                        envelope.payload_size as usize,
                                    )
                                } else {
                                    &[]
                                };
                            crate::hew_node::complete_remote_reply(
                                envelope.request_id,
                                reply_payload,
                            );
                        }
                    } else {
                        router_fn(
                            envelope.target_actor_id,
                            envelope.msg_type,
                            envelope.payload,
                            envelope.payload_size as usize,
                            envelope.request_id,
                            envelope.source_node_id,
                            mgr,
                        );
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
        connections: Mutex::new(Vec::with_capacity(16)),
        transport,
        inbound_router: router,
        routing_table,
        cluster,
        reconnect_enabled: AtomicBool::new(false),
        reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
        reconnect_shutdown: Arc::new(AtomicBool::new(false)),
        inbound_ask_active: Arc::new(AtomicUsize::new(0)),
        reconnect_workers: Mutex::new(Vec::new()),
        next_publication_token: AtomicU64::new(1),
        local_node_id,
    });
    Box::into_raw(mgr)
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

        // Close all connections via transport. We need to drain the
        // connections while the mutex guard is live, then explicitly
        // drop the drained items and guard before the Box drops.
        let drained: Vec<ConnectionActor> = {
            let Ok(mut conns) = mgr.connections.lock() else {
                // Policy: per-connection-manager state (C-ABI) — poisoned mutex
                // means connection registry is corrupted; report error and bail.
                set_last_error("hew_connmgr_free: connections mutex poisoned (a thread panicked)");
                return;
            };
            conns.drain(..).collect()
        };

        for conn in drained {
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
            // ConnectionActor::drop signals reader thread to stop and joins.
        }
        let workers = {
            let Ok(mut guard) = mgr.reconnect_workers.lock() else {
                // Policy: per-connection-manager state (C-ABI) — poisoned mutex
                // means reconnect registry is corrupted; report error and bail.
                set_last_error(
                    "hew_connmgr_free: reconnect_workers mutex poisoned (a thread panicked)",
                );
                return;
            };
            guard.drain(..).collect::<Vec<_>>()
        };
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
) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_configure_reconnect: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let Ok(mut conns) = mgr.connections.lock() else {
        // Policy: per-connection-manager state (C-ABI) — poisoned mutex
        // means connection registry is corrupted; report error and bail.
        set_last_error("hew_connmgr_configure_reconnect: mutex poisoned (a thread panicked)");
        return -1;
    };
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
    // SAFETY: caller guarantees target_addr is a valid C string (or null).
    let Some(target) =
        (unsafe { crate::util::cstr_to_str(target_addr, "hew_connmgr_configure_reconnect") })
    else {
        return -1;
    };
    if target.is_empty() {
        set_last_error("hew_connmgr_configure_reconnect: target_addr is empty");
        return -1;
    }
    let retries = if max_retries > 0 {
        normalize_max_retries(max_retries)
    } else {
        mgr.reconnect_max_retries.load(Ordering::Acquire).max(1)
    };
    conn.reconnect = Some(ReconnectSettings {
        target_addr: target.to_owned(),
        max_retries: retries,
    });
    0
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

    if mgr.reconnect_shutdown.load(Ordering::Acquire) {
        // SAFETY: conn_id came from the transport and is not yet tracked by the manager.
        unsafe { close_transport_conn(mgr.transport, conn_id) };
        set_last_error("hew_connmgr_add: manager is shutting down");
        return -1;
    }

    {
        let Ok(conns) = mgr.connections.lock() else {
            // Policy: per-connection-manager state (C-ABI) — poisoned mutex
            // means connection registry is corrupted; report error and bail.
            // SAFETY: conn_id is a valid transport handle; hew_connmgr_add
            // owns cleanup for all failure paths after entry.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error("hew_connmgr_add: mutex poisoned (a thread panicked)");
            return -1;
        };
        if conns.iter().any(|c| c.conn_id == conn_id) {
            // SAFETY: conn_id is a new transport handle that cannot be installed;
            // close it so the caller does not need to clean up on failure.
            unsafe { close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: connection {conn_id} already exists"
            ));
            return -1;
        }
    }

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

    #[cfg(feature = "encryption")]
    let skip_noise = {
        #[cfg(feature = "quic")]
        {
            // QUIC provides TLS 1.3 encryption — skip Noise when using QUIC transport.
            // SAFETY: mgr.transport is valid while the connection manager is alive.
            unsafe { crate::quic_transport::hew_transport_is_quic(mgr.transport) }
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

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = SendTransport(mgr.transport);
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);
    let mgr_send = SendConnMgr(mgr_ptr);
    let peer_feature_flags = actor.peer_feature_flags;
    #[cfg(feature = "encryption")]
    let noise_transport = Arc::clone(&actor.noise_transport);

    let handle = thread::Builder::new()
        .name(format!("hew-conn-{conn_id}"))
        .spawn(move || {
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
        Err(ConnectionInstallError::MutexPoisoned) => {
            // Policy: per-connection-manager state (C-ABI) — poisoned mutex
            // means connection registry is corrupted; report error and bail.
            set_last_error("hew_connmgr_add: mutex poisoned (a thread panicked)");
            return -1;
        }
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

    let (conn, peer_node_id, publication_token, publication_sync, publication_removed) = {
        let Ok(mut conns) = mgr.connections.lock() else {
            // Policy: per-connection-manager state (C-ABI) — poisoned mutex
            // means connection registry is corrupted; report error and bail.
            set_last_error("hew_connmgr_remove: mutex poisoned (a thread panicked)");
            return -1;
        };

        let idx = conns.iter().position(|c| c.conn_id == conn_id);
        let Some(idx) = idx else {
            set_last_error(format!(
                "hew_connmgr_remove: connection {conn_id} not found"
            ));
            return -1;
        };

        let conn = conns.swap_remove(idx);
        let peer_node_id = conn.peer_node_id;
        let publication_token = conn.publication_token;
        let publication_sync = Arc::clone(&conn.publication_sync);
        let publication_removed = Arc::clone(&conn.publication_removed);
        conn.state.store(CONN_STATE_CLOSED, Ordering::Release);
        (
            conn,
            peer_node_id,
            publication_token,
            publication_sync,
            publication_removed,
        )
    };

    // Release the registry lock before waking the reader. The reader cleanup
    // path can re-enter hew_connmgr_remove on an unexpected drop.
    //
    {
        let _publication = publication_sync.lock_or_recover();
        publication_removed.store(true, Ordering::Release);
    }
    // Mark the reader as explicitly stopped before closing the transport so an
    // awakened reader does not treat this teardown as an unexpected drop.
    conn.reader_stop.store(1, Ordering::Release);
    // Mark closed before the explicit close so Drop does not double-close.
    conn.transport_closed.store(true, Ordering::Release);
    //
    // Close the transport connection so a blocking recv unblocks.
    // SAFETY: transport is valid per manager contract.
    unsafe { close_transport_conn(mgr.transport, conn_id) };
    // Now drop/join the reader thread after transport close.
    drop(conn);
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
    set_last_error("hew_connmgr_set_outbound_capacity: outbound queue support was removed; sends are synchronous");
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
    let target_node_id = (target_actor_id >> 48) as u16;
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        let Ok(conns) = mgr_ref.connections.lock() else {
            // Policy: per-connection-manager state (C-ABI) — poisoned mutex
            // means connection registry is corrupted; report error and bail.
            set_last_error("hew_connmgr_send: mutex poisoned (a thread panicked)");
            return -1;
        };
        let conn = conns.iter().find(|c| c.conn_id == conn_id);
        match conn {
            Some(c)
                if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE
                    && (target_node_id == 0 || c.peer_node_id == target_node_id) =>
            {
                #[cfg(feature = "encryption")]
                {
                    maybe_noise = Some(Arc::clone(&c.noise_transport));
                }
            }
            _ => return -1,
        }
    }

    #[cfg(feature = "encryption")]
    if let Some(noise_transport) = maybe_noise {
        // SAFETY: data is valid for size bytes per caller contract of hew_connmgr_send.
        let Some(encoded) = (unsafe { encode_envelope(target_actor_id, msg_type, data, size) })
        else {
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
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        let Ok(conns) = mgr_ref.connections.lock() else {
            return -1;
        };
        match conns.iter().find(|c| c.conn_id == conn_id) {
            Some(c) if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE => {
                #[cfg(feature = "encryption")]
                {
                    maybe_noise = Some(Arc::clone(&c.noise_transport));
                }
            }
            _ => return -1,
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
    let Ok(conns) = mgr_ref.connections.lock() else {
        return -1;
    };
    for c in conns.iter() {
        if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
            return c.conn_id;
        }
    }
    -1
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
    let Ok(conns) = mgr_ref.connections.lock() else {
        return 0;
    };
    for c in conns.iter() {
        if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
            return c.peer_feature_flags;
        }
    }
    0
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
    let Ok(mut conns) = mgr_ref.connections.lock() else {
        return;
    };
    for c in conns.iter_mut() {
        if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE && c.peer_node_id == node_id {
            c.peer_feature_flags = flags;
        }
    }
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
    let Ok(conns) = mgr.connections.lock() else {
        // Policy: per-connection-manager state (C-ABI) — poisoned mutex
        // means connection registry is corrupted; report error and bail.
        set_last_error("hew_connmgr_count: mutex poisoned (a thread panicked)");
        return -1;
    };
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_possible_wrap,
        reason = "connection count will not exceed c_int range in practice"
    )]
    {
        conns.len() as c_int
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
    let conn_ids: Vec<c_int> = {
        let Ok(conns) = mgr_ref.connections.lock() else {
            // Policy: per-connection-manager state (C-ABI) — poisoned mutex
            // means connection registry is corrupted; report error and bail.
            set_last_error("hew_connmgr_broadcast: mutex poisoned (a thread panicked)");
            return 0;
        };
        conns
            .iter()
            .filter(|c| c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE)
            .map(|c| c.conn_id)
            .collect()
    };

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
    let Ok(conns) = mgr.connections.lock() else {
        // Policy: per-connection-manager state (C-ABI) — poisoned mutex
        // means connection registry is corrupted; report error and bail.
        set_last_error("hew_connmgr_last_activity: mutex poisoned (a thread panicked)");
        return 0;
    };
    conns
        .iter()
        .find(|c| c.conn_id == conn_id)
        .map_or(0, |c| c.last_activity_ms.load(Ordering::Acquire))
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
    let Ok(conns) = mgr.connections.lock() else {
        // Policy: per-connection-manager state (C-ABI) — poisoned mutex
        // means connection registry is corrupted; report error and bail.
        set_last_error("hew_connmgr_conn_state: mutex poisoned (a thread panicked)");
        return CONN_STATE_CLOSED;
    };
    conns
        .iter()
        .find(|c| c.conn_id == conn_id)
        .map_or(CONN_STATE_CLOSED, |c| c.state.load(Ordering::Acquire))
}

// ── Profiler snapshot ───────────────────────────────────────────────────

/// Build a JSON array of active connections for the profiler HTTP API.
///
/// Each element: `{"conn_id":N,"peer_node_id":N,"state":"S","last_activity_ms":N}`
#[cfg(feature = "profiler")]
pub fn snapshot_connections_json(mgr: &HewConnMgr) -> String {
    use std::fmt::Write as _;

    let connections = mgr.connections.lock_or_recover();

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
            connections: Mutex::new(vec![active, draining]),
            transport: std::ptr::null_mut(),
            inbound_router: None,
            routing_table: std::ptr::null_mut(),
            cluster: std::ptr::null_mut(),
            reconnect_enabled: AtomicBool::new(false),
            reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
            reconnect_shutdown: Arc::new(AtomicBool::new(false)),
            inbound_ask_active: Arc::new(AtomicUsize::new(0)),
            reconnect_workers: Mutex::new(Vec::new()),
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
            let could_lock = unsafe { (&*mgr.0).connections.try_lock().is_ok() };
            lock_result_tx
                .send(could_lock)
                .expect("reader should report lock availability");
        }));

        // SAFETY: mgr is a live manager allocated by hew_connmgr_new above.
        unsafe {
            let mut conns = (&*mgr)
                .connections
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            conns.push(actor);
        }

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
            let could_lock = unsafe { (&*mgr.0).connections.try_lock().is_ok() };
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
            (&*mgr)
                .connections
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(old_actor);
            publish_connection_established(
                &*mgr,
                2,
                11,
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
            let replacement_installed = (0..50).any(|_| match (&*mgr).connections.try_lock() {
                Ok(mut conns) => {
                    conns.push(
                        replacement_actor
                            .take()
                            .expect("replacement should install once"),
                    );
                    true
                }
                Err(std::sync::TryLockError::WouldBlock) => {
                    std::thread::sleep(std::time::Duration::from_millis(10));
                    false
                }
                Err(std::sync::TryLockError::Poisoned(err)) => {
                    err.into_inner().push(
                        replacement_actor
                            .take()
                            .expect("replacement should install once"),
                    );
                    true
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
            (&*mgr)
                .connections
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(actor);
            publish_connection_established(
                &*mgr,
                2,
                31,
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
            (&*mgr)
                .connections
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(actor);

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
            (&*mgr)
                .connections
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(actor);

            publish_connection_established(
                &*mgr,
                2,
                44,
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
