//! Reconnect planning and attempt tests.

use super::*;

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
        let peer_hs = local_handshake(test_node_identity(20), 1, [0u8; NOISE_STATIC_PUBKEY_LEN]);
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
    let (outcome, closed_ids, count, error_message) = run_reconnect_attempt_replay_case(Some(7));
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
