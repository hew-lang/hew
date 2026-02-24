//! Per-connection transport actors for the Hew runtime.
//!
//! Replaces the global-mutex-protected connection array in [`crate::node`]
//! with individual actors per connection. Each connection actor owns a
//! transport connection handle, runs a dedicated reader thread for
//! inbound messages, and processes outbound sends through its mailbox.
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
//! - [`hew_connmgr_count`] — Number of active connections.
//! - [`hew_connmgr_broadcast`] — Send to all connections.

use std::ffi::c_int;
use std::sync::atomic::{AtomicI32, AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use crate::transport::HewTransport;
use crate::wire::{hew_wire_buf_init_read, hew_wire_decode_envelope, HewWireBuf, HewWireEnvelope};

// ── Connection states ──────────────────────────────────────────────────

/// Connection is being established.
pub const CONN_STATE_CONNECTING: i32 = 0;
/// Connection is active and ready for I/O.
pub const CONN_STATE_ACTIVE: i32 = 1;
/// Connection is draining (no new sends, waiting for in-flight).
pub const CONN_STATE_DRAINING: i32 = 2;
/// Connection is closed.
pub const CONN_STATE_CLOSED: i32 = 3;

// ── Connection actor ───────────────────────────────────────────────────

/// Per-connection actor state.
///
/// Each connection actor owns a transport connection handle and tracks
/// connection health via heartbeat timestamps.
#[derive(Debug)]
struct ConnectionActor {
    /// Transport connection ID (index into transport's internal array).
    conn_id: c_int,
    /// Current connection state.
    state: AtomicI32,
    /// Monotonic timestamp (ms) of last successful send or recv.
    last_activity_ms: Arc<AtomicU64>,
    /// Handle to the reader thread (if running).
    reader_handle: Option<JoinHandle<()>>,
    /// Signal to stop the reader thread.
    reader_stop: Arc<AtomicI32>,
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

impl ConnectionActor {
    fn new(conn_id: c_int) -> Self {
        Self {
            conn_id,
            state: AtomicI32::new(CONN_STATE_CONNECTING),
            last_activity_ms: Arc::new(AtomicU64::new(0)),
            reader_handle: None,
            reader_stop: Arc::new(AtomicI32::new(0)),
        }
    }
}

impl Drop for ConnectionActor {
    fn drop(&mut self) {
        // Signal reader thread to stop.
        self.reader_stop.store(1, Ordering::Release);
        // Wait for it (best-effort).
        if let Some(handle) = self.reader_handle.take() {
            let _ = handle.join();
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

// ── Reader thread ──────────────────────────────────────────────────────

/// Reader thread: loops calling transport recv, decodes envelopes,
/// and routes to local actors via the inbound router callback.
#[expect(
    clippy::needless_pass_by_value,
    reason = "SendTransport and Arc values are moved into this thread from spawn closure"
)]
fn reader_loop(
    transport: SendTransport,
    conn_id: c_int,
    stop_flag: Arc<AtomicI32>,
    last_activity: Arc<AtomicU64>,
    router: Option<InboundRouter>,
) {
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
            // Connection closed or error — stop reading.
            break;
        }

        #[expect(clippy::cast_sign_loss, reason = "bytes_read > 0 checked above")]
        let read_len = bytes_read as usize;

        // Update heartbeat.
        // SAFETY: hew_now_ms has no preconditions.
        let now = unsafe { crate::io_time::hew_now_ms() };
        last_activity.store(now, Ordering::Relaxed);

        // Decode envelope and route.
        if let Some(router_fn) = router {
            // SAFETY: buf contains `read_len` valid bytes from recv.
            unsafe {
                let mut wire_buf: HewWireBuf = std::mem::zeroed();
                hew_wire_buf_init_read(&raw mut wire_buf, buf.as_mut_ptr().cast(), read_len);

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
) -> *mut HewConnMgr {
    if transport.is_null() {
        return std::ptr::null_mut();
    }
    let mgr = Box::new(HewConnMgr {
        connections: Mutex::new(Vec::with_capacity(16)),
        transport,
        inbound_router: router,
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
        // mgr is dropped here, freeing the HewConnMgr.
    }
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
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };

    let mut conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    // Check for duplicate.
    if conns.iter().any(|c| c.conn_id == conn_id) {
        return -1;
    }

    let mut actor = ConnectionActor::new(conn_id);
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);

    // SAFETY: hew_now_ms has no preconditions.
    let now = unsafe { crate::io_time::hew_now_ms() };
    actor.last_activity_ms.store(now, Ordering::Relaxed);

    // Spawn reader thread.
    let stop = Arc::clone(&actor.reader_stop);
    let transport_send = SendTransport(mgr.transport);
    let router = mgr.inbound_router;
    let activity_send = Arc::clone(&actor.last_activity_ms);

    let handle = thread::Builder::new()
        .name(format!("hew-conn-{conn_id}"))
        .spawn(move || reader_loop(transport_send, conn_id, stop, activity_send, router));

    match handle {
        Ok(h) => actor.reader_handle = Some(h),
        Err(_) => return -1,
    }

    conns.push(actor);
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
        return -1;
    }
    // SAFETY: caller guarantees `mgr` is valid.
    let mgr = unsafe { &*mgr };

    let mut conns = match mgr.connections.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    let idx = conns.iter().position(|c| c.conn_id == conn_id);
    let Some(idx) = idx else { return -1 };

    let conn = conns.swap_remove(idx);
    // Signal reader thread to stop (happens in Drop).
    drop(conn);

    // Close the transport connection.
    // SAFETY: transport is valid per manager contract.
    unsafe {
        let t = &*mgr.transport;
        if let Some(ops) = t.ops.as_ref() {
            if let Some(close_fn) = ops.close_conn {
                close_fn(t.r#impl, conn_id);
            }
        }
    }

    0
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
    {
        let conns = match mgr_ref.connections.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        let conn = conns.iter().find(|c| c.conn_id == conn_id);
        match conn {
            Some(c) if c.state.load(Ordering::Acquire) == CONN_STATE_ACTIVE => {}
            _ => return -1,
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
            let mgr = hew_connmgr_new(std::ptr::null_mut(), None);
            assert!(mgr.is_null());
        }
    }
}
