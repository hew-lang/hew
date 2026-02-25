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
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::cluster::{
    hew_cluster_notify_connection_established, hew_cluster_notify_connection_lost, HewCluster,
};
use crate::routing::{hew_routing_add_route, hew_routing_remove_route, HewRoutingTable};
use crate::set_last_error;
use crate::transport::{HewTransport, HEW_CONN_INVALID};
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
/// Connection is draining (no new sends, waiting for in-flight).
/// TODO: reserve for future graceful shutdown semantics once conn draining is implemented.
pub const CONN_STATE_DRAINING: i32 = 2;
/// Connection is closed.
pub const CONN_STATE_CLOSED: i32 = 3;

const HEW_HANDSHAKE_SIZE: usize = 48;
const HEW_HANDSHAKE_MAGIC: [u8; 4] = *b"HEW\x01";
const HEW_PROTOCOL_VERSION: u16 = 1;
const HEW_FEATURE_SUPPORTS_ENCRYPTION: u32 = 1 << 0;
const HEW_FEATURE_SUPPORTS_GOSSIP: u32 = 1 << 1;
const HEW_FEATURE_SUPPORTS_REMOTE_SPAWN: u32 = 1 << 2;
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
    transport: *mut HewTransport,
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
    /// Global shutdown signal shared with reconnect workers.
    reconnect_shutdown: Arc<AtomicBool>,
    /// Background reconnect worker handles.
    reconnect_workers: Mutex<Vec<JoinHandle<()>>>,
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
type InboundRouter = unsafe extern "C" fn(u64, i32, *mut u8, usize);

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
            peer_node_id: 0,
            peer_feature_flags: 0,
            state: AtomicI32::new(CONN_STATE_CONNECTING),
            last_activity_ms: Arc::new(AtomicU64::new(0)),
            #[cfg(feature = "encryption")]
            noise_transport: Arc::new(Mutex::new(None)),
            reader_handle: None,
            reader_stop: Arc::new(AtomicI32::new(0)),
            reconnect: None,
        }
    }
}

impl Drop for ConnectionActor {
    fn drop(&mut self) {
        // Signal reader thread to stop.
        self.reader_stop.store(1, Ordering::Release);
        // Wait for it (best-effort).
        if let Some(handle) = self.reader_handle.take() {
            if handle.thread().id() != thread::current().id() {
                let _ = handle.join();
            }
        }
    }
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

fn hew_connmgr_normalize_max_retries(max_retries: c_int) -> u32 {
    if max_retries <= 0 {
        RECONNECT_DEFAULT_MAX_RETRIES
    } else {
        u32::try_from(max_retries).unwrap_or(RECONNECT_DEFAULT_MAX_RETRIES)
    }
}

fn hew_connmgr_jittered_backoff_ms(base_ms: u64) -> u64 {
    let mut rng = rng();
    let jitter_pct = rng.random_range(RECONNECT_JITTER_MIN_PERCENT..=RECONNECT_JITTER_MAX_PERCENT);
    let jittered = base_ms.saturating_mul(jitter_pct) / 100;
    jittered.max(1)
}

fn hew_connmgr_sleep_until_retry(shutdown: &AtomicBool, delay_ms: u64) -> bool {
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

fn hew_connmgr_collect_finished_reconnect_workers(mgr: &HewConnMgr) {
    let mut workers = match mgr.reconnect_workers.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
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

fn hew_connmgr_reconnect_plan(mgr: &HewConnMgr, conn_id: c_int) -> Option<ReconnectPlan> {
    if !mgr.reconnect_enabled.load(Ordering::Acquire)
        || mgr.reconnect_shutdown.load(Ordering::Acquire)
    {
        return None;
    }
    let conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    let conn = conns.iter().find(|c| c.conn_id == conn_id)?;
    let reconnect = conn.reconnect.as_ref()?;
    Some(ReconnectPlan {
        target_addr: reconnect.target_addr.clone(),
        max_retries: reconnect.max_retries.max(1),
    })
}

unsafe fn hew_connmgr_connect_addr(
    mgr: *mut HewConnMgr,
    target_addr: &CStr,
) -> Result<c_int, String> {
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

fn hew_connmgr_spawn_reconnect_worker(mgr: *mut HewConnMgr, conn_id: c_int, plan: ReconnectPlan) {
    if mgr.is_null() {
        return;
    }
    // SAFETY: caller guarantees `mgr` is valid when scheduling workers.
    let mgr_ref = unsafe { &*mgr };
    if mgr_ref.reconnect_shutdown.load(Ordering::Acquire) {
        return;
    }
    hew_connmgr_collect_finished_reconnect_workers(mgr_ref);
    let mgr_send = SendConnMgr(mgr);
    let shutdown = Arc::clone(&mgr_ref.reconnect_shutdown);
    let thread_name = format!("hew-reconnect-{conn_id}");
    let handle = thread::Builder::new().name(thread_name).spawn(move || {
        hew_connmgr_reconnect_worker_loop(mgr_send, shutdown, conn_id, plan);
    });
    match handle {
        Ok(worker) => {
            let mut workers = match mgr_ref.reconnect_workers.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            workers.push(worker);
        }
        Err(_) => {
            set_last_error(format!(
                "hew_connmgr_reconnect: failed to spawn worker for dropped conn {conn_id}"
            ));
        }
    }
}

fn hew_connmgr_reconnect_worker_loop(
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
        let delay_ms = hew_connmgr_jittered_backoff_ms(base_backoff_ms);
        if !hew_connmgr_sleep_until_retry(&shutdown, delay_ms) {
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
        let connect_result = unsafe { hew_connmgr_connect_addr(mgr_ptr, &target_addr) };
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
                // SAFETY: connection belongs to this transport and was not installed.
                unsafe {
                    let mgr_ref = &*mgr_ptr;
                    hew_conn_close_transport_conn(mgr_ref.transport, new_conn_id);
                }
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

fn hew_conn_local_feature_flags() -> u32 {
    let mut flags = HEW_FEATURE_SUPPORTS_GOSSIP | HEW_FEATURE_SUPPORTS_REMOTE_SPAWN;
    #[cfg(feature = "encryption")]
    {
        flags |= HEW_FEATURE_SUPPORTS_ENCRYPTION;
    }
    flags
}

fn hew_conn_local_schema_hash() -> u32 {
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

fn hew_conn_local_handshake(static_noise_pubkey: [u8; NOISE_STATIC_PUBKEY_LEN]) -> HewHandshake {
    HewHandshake {
        protocol_version: HEW_PROTOCOL_VERSION,
        node_id: crate::pid::hew_pid_local_node(),
        schema_hash: hew_conn_local_schema_hash(),
        feature_flags: hew_conn_local_feature_flags(),
        static_noise_pubkey,
    }
}

fn hew_conn_version_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.protocol_version == peer.protocol_version
}

fn hew_conn_schema_compatible(local: &HewHandshake, peer: &HewHandshake) -> bool {
    local.schema_hash == peer.schema_hash
}

unsafe fn hew_conn_send_frame(
    transport: *mut HewTransport,
    conn_id: c_int,
    payload: &[u8],
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
    let Some(send_fn) = ops.send else {
        return false;
    };
    let Ok(expected) = c_int::try_from(payload.len()) else {
        return false;
    };
    // SAFETY: payload pointer is valid for payload.len() bytes.
    unsafe { send_fn(t.r#impl, conn_id, payload.as_ptr().cast(), payload.len()) == expected }
}

unsafe fn hew_conn_recv_frame_exact(
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

unsafe fn hew_conn_handshake_send(
    transport: *mut HewTransport,
    conn_id: c_int,
    handshake: HewHandshake,
) -> c_int {
    // SAFETY: transport and conn_id are validated by caller; serialize returns a fixed-size buffer.
    c_int::from(!unsafe { hew_conn_send_frame(transport, conn_id, &handshake.serialize()) }) * -1
}

unsafe fn hew_conn_handshake_recv(
    transport: *mut HewTransport,
    conn_id: c_int,
) -> Option<HewHandshake> {
    let mut buf = [0u8; HEW_HANDSHAKE_SIZE];
    // SAFETY: transport and conn_id are validated by caller; buf is stack-allocated with correct size.
    if !unsafe { hew_conn_recv_frame_exact(transport, conn_id, &mut buf) } {
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

unsafe fn hew_conn_handshake_exchange(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: HewHandshake,
) -> Option<HewHandshake> {
    // SAFETY: transport and conn_id validated by caller contract.
    if unsafe { hew_conn_handshake_send(transport, conn_id, local) } != 0 {
        set_last_error(format!(
            "hew_connmgr_add: failed to send handshake for conn {conn_id}"
        ));
        return None;
    }
    // SAFETY: same contract — transport remains valid through handshake sequence.
    let peer = unsafe { hew_conn_handshake_recv(transport, conn_id) }?;
    if !hew_conn_version_compatible(&local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake protocol mismatch for conn {conn_id} (local={}, peer={})",
            local.protocol_version, peer.protocol_version
        ));
        return None;
    }
    if !hew_conn_schema_compatible(&local, &peer) {
        set_last_error(format!(
            "hew_connmgr_add: handshake schema hash mismatch for conn {conn_id} (local={:#010x}, peer={:#010x})",
            local.schema_hash, peer.schema_hash
        ));
        return None;
    }
    Some(peer)
}

unsafe fn hew_conn_close_transport_conn(transport: *mut HewTransport, conn_id: c_int) {
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

unsafe fn hew_conn_encode_envelope(
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
fn hew_conn_supports_encryption(flags: u32) -> bool {
    flags & HEW_FEATURE_SUPPORTS_ENCRYPTION != 0
}

#[cfg(feature = "encryption")]
fn hew_conn_noise_is_initiator(local: &HewHandshake, peer: &HewHandshake) -> Option<bool> {
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
unsafe fn hew_conn_upgrade_noise(
    transport: *mut HewTransport,
    conn_id: c_int,
    local: &HewHandshake,
    peer: &HewHandshake,
    local_private_key: &[u8],
) -> Option<(snow::TransportState, [u8; NOISE_STATIC_PUBKEY_LEN])> {
    let initiator = hew_conn_noise_is_initiator(local, peer)?;
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

    let mut msg = vec![0u8; NOISE_MAX_MSG_SIZE];
    let mut payload = vec![0u8; NOISE_MAX_MSG_SIZE];

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
#[expect(
    clippy::needless_pass_by_value,
    reason = "SendTransport and Arc values are moved into this thread from spawn closure"
)]
fn reader_loop(
    mgr: SendConnMgr,
    transport: SendTransport,
    conn_id: c_int,
    stop_flag: Arc<AtomicI32>,
    last_activity: Arc<AtomicU64>,
    router: Option<InboundRouter>,
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
            // Expected shutdown paths set `stop_flag` before closing transport.
            let unexpected_drop = stop_flag.load(Ordering::Acquire) == 0;
            if unexpected_drop {
                // SAFETY: `mgr` and `conn_id` originate from a live connection manager.
                let reconnect_plan = unsafe {
                    if mgr.is_null() {
                        None
                    } else {
                        hew_connmgr_reconnect_plan(&*mgr, conn_id)
                    }
                };
                // SAFETY: manager and conn_id come from active reader state.
                let _ = unsafe { hew_connmgr_remove(mgr, conn_id) };
                if let Some(plan) = reconnect_plan {
                    hew_connmgr_spawn_reconnect_worker(mgr, conn_id, plan);
                }
            }
            // Connection closed or error — stop reading.
            break;
        }

        #[expect(clippy::cast_sign_loss, reason = "bytes_read > 0 checked above")]
        let read_len = bytes_read as usize;

        let mut payload_ptr = buf.as_mut_ptr();
        let mut payload_len = read_len;
        #[cfg(feature = "encryption")]
        {
            let mut decrypted = vec![0u8; read_len];
            let mut guard = match noise_transport.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            if let Some(noise) = guard.as_mut() {
                let Ok(n) = noise.read_message(&buf[..read_len], &mut decrypted) else {
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
        last_activity.store(now, Ordering::Relaxed);

        // Decode envelope and route.
        if let Some(router_fn) = router {
            // SAFETY: buf contains `read_len` valid bytes from recv.
            unsafe {
                let mut wire_buf: HewWireBuf = std::mem::zeroed();
                hew_wire_buf_init_read(&raw mut wire_buf, payload_ptr.cast(), payload_len);

                let mut envelope: HewWireEnvelope = std::mem::zeroed();
                let rc = hew_wire_decode_envelope(&raw mut wire_buf, &raw mut envelope);
                if rc == 0 {
                    router_fn(
                        envelope.target_actor_id,
                        envelope.msg_type,
                        envelope.payload,
                        envelope.payload_size as usize,
                    );
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
) -> *mut HewConnMgr {
    if transport.is_null() {
        return std::ptr::null_mut();
    }
    let mgr = Box::new(HewConnMgr {
        connections: Mutex::new(Vec::with_capacity(16)),
        transport,
        inbound_router: router,
        routing_table,
        cluster,
        reconnect_enabled: AtomicBool::new(false),
        reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
        reconnect_shutdown: Arc::new(AtomicBool::new(false)),
        reconnect_workers: Mutex::new(Vec::new()),
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
            let mut conns = match mgr.connections.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            conns.drain(..).collect()
        };

        for conn in drained {
            // SAFETY: transport is valid per manager contract.
            unsafe {
                let t = &*transport;
                if let Some(ops) = t.ops.as_ref() {
                    if let Some(close_fn) = ops.close_conn {
                        close_fn(t.r#impl, conn.conn_id);
                    }
                }
            }
            // ConnectionActor::drop signals reader thread to stop.
        }
        let workers = {
            let mut guard = match mgr.reconnect_workers.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
            guard.drain(..).collect::<Vec<_>>()
        };
        for worker in workers {
            let _ = worker.join();
        }
        // mgr is dropped here, freeing the HewConnMgr.
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
    mgr.reconnect_max_retries.store(
        hew_connmgr_normalize_max_retries(max_retries),
        Ordering::Release,
    );
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
    let mut conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
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
    if target_addr.is_null() {
        set_last_error("hew_connmgr_configure_reconnect: target_addr is null");
        return -1;
    }
    // SAFETY: caller guarantees target_addr is a valid C string.
    let Ok(target) = unsafe { CStr::from_ptr(target_addr) }.to_str() else {
        set_last_error("hew_connmgr_configure_reconnect: target_addr is not valid UTF-8");
        return -1;
    };
    if target.is_empty() {
        set_last_error("hew_connmgr_configure_reconnect: target_addr is empty");
        return -1;
    }
    let retries = if max_retries > 0 {
        hew_connmgr_normalize_max_retries(max_retries)
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
pub unsafe extern "C" fn hew_connmgr_add(mgr: *mut HewConnMgr, conn_id: c_int) -> c_int {
    if mgr.is_null() {
        set_last_error("hew_connmgr_add: manager is null");
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };

    {
        let conns = match mgr.connections.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        if conns.iter().any(|c| c.conn_id == conn_id) {
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
            unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
            set_last_error("hew_connmgr_add: invalid noise pattern");
            return -1;
        };
        let builder = snow::Builder::new(pattern);
        let Ok(keypair) = builder.generate_keypair() else {
            unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
            set_last_error("hew_connmgr_add: failed to generate noise keypair");
            return -1;
        };
        local_noise_pubkey.copy_from_slice(&keypair.public);
        keypair.private
    };

    let local_hs = hew_conn_local_handshake(local_noise_pubkey);
    let Some(peer_hs) = (unsafe { hew_conn_handshake_exchange(mgr.transport, conn_id, local_hs) })
    else {
        unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
        return -1;
    };

    #[cfg(feature = "encryption")]
    let upgraded_noise = if hew_conn_supports_encryption(local_hs.feature_flags)
        && hew_conn_supports_encryption(peer_hs.feature_flags)
    {
        unsafe {
            hew_conn_upgrade_noise(
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
    let upgraded_noise = if hew_conn_supports_encryption(local_hs.feature_flags)
        && hew_conn_supports_encryption(peer_hs.feature_flags)
    {
        let Some((noise, peer_static_pubkey)) = upgraded_noise else {
            unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: noise upgrade failed for conn {conn_id}"
            ));
            return -1;
        };
        if !crate::encryption::hew_allowlist_check_active_peer(&peer_static_pubkey) {
            unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
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
    actor.peer_node_id = peer_hs.node_id;
    actor.peer_feature_flags = peer_hs.feature_flags;
    #[cfg(feature = "encryption")]
    if let Some(noise) = upgraded_noise {
        let mut guard = match actor.noise_transport.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        *guard = Some(noise);
    }
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);

    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::io_time::hew_now_ms() };
    actor.last_activity_ms.store(now, Ordering::Relaxed);

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = SendTransport(mgr.transport);
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);
    let mgr_send = SendConnMgr(mgr as *const HewConnMgr as *mut HewConnMgr);
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
                #[cfg(feature = "encryption")]
                noise_transport,
            )
        });

    match handle {
        Ok(h) => actor.reader_handle = Some(h),
        Err(_) => {
            unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
            set_last_error(format!(
                "hew_connmgr_add: failed to spawn reader thread for conn {conn_id}"
            ));
            return -1;
        }
    }

    let mut conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    if conns.iter().any(|c| c.conn_id == conn_id) {
        unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
        set_last_error(format!(
            "hew_connmgr_add: connection {conn_id} became duplicate during install"
        ));
        return -1;
    }
    conns.push(actor);
    drop(conns);

    if peer_hs.node_id != 0 {
        // TODO: route entries only store bare conn_id (no generation), so conn_id reuse can
        // cause stale lookups to target a different connection; add generation tokens.
        // SAFETY: pointer validity is checked by the callee.
        unsafe { hew_routing_add_route(mgr.routing_table, peer_hs.node_id, conn_id) };
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe { hew_cluster_notify_connection_established(mgr.cluster, peer_hs.node_id) };
    }

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

    let mut conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
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
    conn.state.store(CONN_STATE_CLOSED, Ordering::Release);

    // Close the transport connection first so a blocking recv unblocks.
    // SAFETY: transport is valid per manager contract.
    unsafe { hew_conn_close_transport_conn(mgr.transport, conn_id) };
    // Now drop/join the reader thread after transport close.
    drop(conn);

    if peer_node_id != 0 {
        // SAFETY: pointer validity is checked by the callee.
        unsafe { hew_routing_remove_route(mgr.routing_table, peer_node_id) };
        // SAFETY: pointer validity is checked by the callee.
        let _ = unsafe { hew_cluster_notify_connection_lost(mgr.cluster, peer_node_id) };
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
    if mgr.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Verify connection exists and is active.
    #[cfg(feature = "encryption")]
    let maybe_noise: Option<Arc<Mutex<Option<snow::TransportState>>>>;
    {
        let conns = match mgr_ref.connections.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        let conn = conns.iter().find(|c| c.conn_id == conn_id);
        match conn {
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
    if let Some(noise_transport) = maybe_noise {
        let encoded =
            match unsafe { hew_conn_encode_envelope(target_actor_id, msg_type, data, size) } {
                Some(b) => b,
                None => return -1,
            };
        let mut maybe_ciphertext = None;
        {
            let mut guard = match noise_transport.lock() {
                Ok(g) => g,
                Err(e) => e.into_inner(),
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
            if unsafe { hew_conn_send_frame(mgr_ref.transport, conn_id, &ciphertext) } {
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

/// Return the number of active connections.
///
/// # Safety
///
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_connmgr_count(mgr: *mut HewConnMgr) -> c_int {
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
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
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr_ref = unsafe { &*mgr };

    // Collect active connection IDs under the lock.
    let conn_ids: Vec<c_int> = {
        let conns = match mgr_ref.connections.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
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
    if mgr.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    conns
        .iter()
        .find(|c| c.conn_id == conn_id)
        .map_or(0, |c| c.last_activity_ms.load(Ordering::Relaxed))
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
    if mgr.is_null() {
        return CONN_STATE_CLOSED;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };
    let conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
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

    let connections = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    let mut json = String::from("[");
    for (i, c) in connections.iter().enumerate() {
        if i > 0 {
            json.push(',');
        }
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
    }
    json.push(']');
    json
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

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
    fn null_mgr_safety() {
        // All operations on null manager should return gracefully.
        // SAFETY: testing null safety.
        unsafe {
            let null_mgr: *mut HewConnMgr = std::ptr::null_mut();
            assert_eq!(hew_connmgr_count(null_mgr), 0);
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
            feature_flags: HEW_FEATURE_SUPPORTS_GOSSIP | HEW_FEATURE_SUPPORTS_REMOTE_SPAWN,
            static_noise_pubkey: [7; NOISE_STATIC_PUBKEY_LEN],
        };
        let encoded = hs.serialize();
        let decoded = HewHandshake::deserialize(&encoded).expect("valid handshake");
        assert_eq!(decoded, hs);
    }

    #[test]
    fn handshake_rejects_invalid_magic() {
        let mut bytes = [0u8; HEW_HANDSHAKE_SIZE];
        bytes.copy_from_slice(&hew_conn_local_handshake([0; NOISE_STATIC_PUBKEY_LEN]).serialize());
        bytes[0] = b'X';
        assert!(HewHandshake::deserialize(&bytes).is_none());
    }

    #[test]
    fn protocol_version_mismatch_rejected() {
        let local = hew_conn_local_handshake([0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.protocol_version = local.protocol_version.wrapping_add(1);
        assert!(!hew_conn_version_compatible(&local, &peer));
    }

    #[test]
    fn handshake_rejects_future_protocol_version() {
        let local = hew_conn_local_handshake([0; NOISE_STATIC_PUBKEY_LEN]);
        let peer = HewHandshake {
            protocol_version: 999,
            ..local
        };
        assert!(!hew_conn_version_compatible(&local, &peer));
    }

    #[test]
    fn schema_hash_mismatch_rejected() {
        let local = hew_conn_local_handshake([0; NOISE_STATIC_PUBKEY_LEN]);
        let mut peer = local;
        peer.schema_hash ^= 0x0100_0000;
        assert!(!hew_conn_schema_compatible(&local, &peer));
    }

    #[test]
    fn local_schema_hash_is_not_placeholder() {
        assert_ne!(hew_conn_local_schema_hash(), FNV1A32_OFFSET_BASIS);
    }
}
