//! Connection manager construction, teardown and configuration.

use std::collections::HashMap;
use std::ffi::{c_char, c_int};
use std::sync::atomic::{AtomicBool, AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;

use crate::cluster::HewCluster;
use crate::lifetime::poison_safe::PoisonSafe;
use crate::node_identity::Location;
use crate::peer_binding::PeerAuthSnapshot;
use crate::routing::HewRoutingTable;
use crate::set_last_error;
use crate::transport::{HewTransport, HEW_CONN_INVALID};

use super::handshake::close_transport_conn;
#[cfg(test)]
use super::identity_claim::test_node_identity;
use super::reconnect::normalize_max_retries;
use super::{
    ConnectionActor, HewConnMgr, InboundRouter, ReaderLifecycle, ReconnectSettings,
    CONN_STATE_ACTIVE, RECONNECT_DEFAULT_MAX_RETRIES,
};

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
/// The expectation is consumed by the next [`hew_connmgr_add`](super::admission::hew_connmgr_add) for `conn_id`.
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
/// `mgr` must be a valid pointer returned by [`hew_connmgr_new`]. The caller
/// surrenders exclusive ownership when this call begins: no concurrent call
/// may still be using the manager, and it must not be used after this call.
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
        //
        // Each close is acquired, not announced: a removal or refusal already
        // tearing one of these connections down owns its close, and free must
        // not close the same one-shot handle behind it. Free still waits for
        // that owner's close to land before draining, because the drain drops
        // the actors and each drop joins a reader that only a real close wakes.
        let closing = mgr.connections.access(|connections| {
            connections
                .iter()
                .map(|connection| {
                    connection
                        .publication_removed
                        .store(true, Ordering::Release);
                    connection.reader_stop.store(1, Ordering::Release);
                    (
                        connection.conn_id,
                        Arc::clone(&connection.claimed_send_lifecycle),
                        Arc::clone(&connection.transport_close),
                        connection.transport_close.claim(),
                    )
                })
                .collect::<Vec<_>>()
        });
        for (conn_id, _, transport_close, owns_close) in &closing {
            if *owns_close {
                // SAFETY: transport is valid per manager contract.
                unsafe { close_transport_conn(transport, *conn_id) };
                transport_close.finish();
            }
        }
        for (_, _, transport_close, owns_close) in &closing {
            if !*owns_close {
                transport_close.wait_closed();
            }
        }
        for (_, lifecycle, _, _) in &closing {
            lifecycle.wait_for_idle();
        }
        // The slots are now closed with no claimed sender in flight; draining
        // permits actor Drop to join each reader without any slot-reuse race.
        let drained: Vec<ConnectionActor> = mgr.connections.access(std::mem::take);
        drop(drained);
        mgr.reader_lifecycle.wait_for_idle();
        let workers: Vec<JoinHandle<()>> = mgr.reverse_link_workers.access(std::mem::take);
        for worker in workers {
            crate::util::report_join_panic("connection reverse-link worker", worker.join());
        }
        let workers: Vec<JoinHandle<()>> = mgr.reconnect_workers.access(std::mem::take);
        for worker in workers {
            crate::util::report_join_panic("connection reconnect worker", worker.join());
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
        crate::util::report_join_panic("connection reverse-link worker", worker.join());
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
        crate::util::report_join_panic("connection reverse-link worker", worker.join());
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
