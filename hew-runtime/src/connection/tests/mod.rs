use super::*;
use std::ffi::{c_char, CStr};
use std::time::Duration;

use super::control::{handle_control_frame, handle_link_down_frame};
use super::identity_claim::{
    abort_claim, publish_claim, publish_connection_established, reserve_claim,
    reserve_unverified_identity_claim, retire_claim, retire_connection_publication,
    test_node_identity, test_publish_claim, test_reserve_unverified,
};
use super::identity_claim::{
    abort_identity_claim, clear_superseded_claim_if_current, publish_identity_claim,
    publish_identity_connection_established, refuse_established_publication,
    reserve_identity_claim, retire_identity_claim, ClaimReservation, CLAIM_RESERVE_WAIT_MS,
};
use super::reader::reader_cleanup;
use super::reconnect::{
    collect_finished_reconnect_workers, next_publication_token, reconnect_attempt, reconnect_plan,
    ReconnectAttemptOutcome,
};
use crate::envelope::{
    decode_registry_gossip_payload, decode_swim_payload, decode_wire_frame, encode_control_frame,
    encode_registry_gossip_payload, ControlFrame, RegistryGossipPayload, SwimControlPayload,
    SwimGossipEntry, WireFrame, CTRL_LINK_DOWN, CTRL_LINK_REQ, CTRL_REGISTRY_GOSSIP, CTRL_SWIM,
    CTRL_UNLINK, REGISTRY_GOSSIP_OP_ADD, REGISTRY_GOSSIP_OP_REMOVE, WIRE_VERSION,
};
use crate::node_identity::{HewLocation, Location};
use crate::peer_binding::ClaimState;

mod claim_wait;
mod claims;
mod control;
mod gating;
mod handshake;
mod publication;
mod reconnect;
mod refusal_race;
mod registry_flush;
mod removal;
mod route_refusal;
mod teardown;

fn last_error_string() -> String {
    let error_ptr = crate::hew_last_error();
    assert!(!error_ptr.is_null(), "expected a last-error diagnostic");
    // SAFETY: the caller established that the current thread's last-error is set.
    unsafe {
        CStr::from_ptr(error_ptr)
            .to_str()
            .expect("last error should be utf-8")
            .to_owned()
    }
}

fn test_manager_with_transport() -> (*mut HewConnMgr, *mut HewTransport) {
    let transport = Box::into_raw(Box::new(HewTransport {
        ops: std::ptr::null(),
        r#impl: std::ptr::null_mut(),
    }));
    // SAFETY: transport is a live test-owned allocation and the remaining pointers are null.
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

unsafe fn free_test_manager_and_transport(mgr: *mut HewConnMgr, transport: *mut HewTransport) {
    // SAFETY: the caller owns both allocations and the manager still borrows transport.
    unsafe { hew_connmgr_free(mgr) };
    // SAFETY: the manager has been freed and no connection actor references transport.
    let _ = unsafe { Box::from_raw(transport) };
}

#[test]
fn connection_actor_drop_reports_reader_panic() {
    crate::hew_clear_error();
    let mut actor = ConnectionActor::new(17);
    actor.reader_handle = Some(std::thread::spawn(|| panic!("reader intentional panic")));

    drop(actor);

    let error = last_error_string();
    assert!(
        error.contains("connection reader thread panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("reader intentional panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
}

#[test]
fn collect_finished_reconnect_worker_reports_panic_and_removes_handle() {
    crate::hew_clear_error();
    let (mgr, transport) = test_manager_with_transport();
    let worker = std::thread::spawn(|| panic!("reconnect collector intentional panic"));
    while !worker.is_finished() {
        std::thread::yield_now();
    }
    // SAFETY: mgr is live and exclusively owned by this test.
    unsafe { &*mgr }
        .reconnect_workers
        .access(|workers| workers.push(worker));

    // SAFETY: mgr is live for this call.
    collect_finished_reconnect_workers(unsafe { &*mgr });

    let error = last_error_string();
    assert!(
        error.contains("connection reconnect worker panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("reconnect collector intentional panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
    // SAFETY: mgr is live for this assertion.
    assert!(unsafe { &*mgr }
        .reconnect_workers
        .access(|workers| workers.is_empty()));

    // SAFETY: this test owns mgr and transport.
    unsafe { free_test_manager_and_transport(mgr, transport) };
}

#[test]
fn manager_free_reports_reverse_link_worker_panic() {
    crate::hew_clear_error();
    let (mgr, transport) = test_manager_with_transport();
    let exited = Arc::new(AtomicBool::new(false));
    let worker_exited = Arc::clone(&exited);
    let worker = std::thread::spawn(move || {
        worker_exited.store(true, Ordering::Release);
        panic!("reverse-link free intentional panic");
    });
    // SAFETY: mgr is live and exclusively owned by this test.
    unsafe { &*mgr }
        .reverse_link_workers
        .access(|workers| workers.push(worker));

    // SAFETY: this test owns mgr and transport.
    unsafe { free_test_manager_and_transport(mgr, transport) };

    let error = last_error_string();
    assert!(
        error.contains("connection reverse-link worker panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("reverse-link free intentional panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
    assert!(
        exited.load(Ordering::Acquire),
        "manager free must join the reverse-link worker before returning"
    );
}

#[test]
fn manager_free_reports_reconnect_worker_panic() {
    crate::hew_clear_error();
    let (mgr, transport) = test_manager_with_transport();
    let exited = Arc::new(AtomicBool::new(false));
    let worker_exited = Arc::clone(&exited);
    let worker = std::thread::spawn(move || {
        worker_exited.store(true, Ordering::Release);
        panic!("reconnect free intentional panic");
    });
    // SAFETY: mgr is live and exclusively owned by this test.
    unsafe { &*mgr }
        .reconnect_workers
        .access(|workers| workers.push(worker));

    // SAFETY: this test owns mgr and transport.
    unsafe { free_test_manager_and_transport(mgr, transport) };

    let error = last_error_string();
    assert!(
        error.contains("connection reconnect worker panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("reconnect free intentional panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
    assert!(
        exited.load(Ordering::Acquire),
        "manager free must join the reconnect worker before returning"
    );
}

#[test]
fn reverse_link_tracking_reports_panics_on_immediate_and_reaped_joins() {
    crate::hew_clear_error();
    let immediate = std::thread::spawn(|| panic!("reverse-link null manager panic"));
    // SAFETY: null manager explicitly requests an immediate join.
    unsafe { hew_connmgr_track_reverse_link_worker(std::ptr::null_mut(), immediate) };
    let error = last_error_string();
    assert!(error.contains("reverse-link null manager panic"));

    crate::hew_clear_error();
    let (mgr, transport) = test_manager_with_transport();
    let finished = std::thread::spawn(|| panic!("reverse-link reaped panic"));
    while !finished.is_finished() {
        std::thread::yield_now();
    }
    // SAFETY: mgr is live and exclusively owned by this test.
    unsafe { &*mgr }
        .reverse_link_workers
        .access(|workers| workers.push(finished));
    let following = std::thread::spawn(|| {});
    // SAFETY: mgr remains live through manager teardown below.
    unsafe { hew_connmgr_track_reverse_link_worker(mgr, following) };

    let error = last_error_string();
    assert!(
        error.contains("connection reverse-link worker panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("reverse-link reaped panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
    // SAFETY: mgr remains live and only the non-panicking following worker is tracked.
    let tracked_workers = unsafe { &*mgr }
        .reverse_link_workers
        .access(|workers| workers.len());
    assert_eq!(tracked_workers, 1);

    // SAFETY: this test owns mgr and transport.
    unsafe { free_test_manager_and_transport(mgr, transport) };
}

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

/// Build a SWIM control frame as it would arrive on the wire.
fn swim_control_frame_bytes(payload: &SwimControlPayload) -> Vec<u8> {
    encode_swim_control(payload).expect("swim control frame should encode")
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
