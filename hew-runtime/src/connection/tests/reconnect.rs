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
    let mut pinned = test_actor(30);
    pinned.peer_identity = Some(test_node_identity(7));
    pinned.peer_session_incarnation = 2;
    pinned.publication_token = 45;
    pinned.reconnect = Some(ReconnectSettings {
        target_addr: "127.0.0.1:1".into(),
        max_retries: 3,
        expected_node_id: Some(7),
    });
    let mut bare = test_actor(31);
    bare.peer_identity = Some(test_node_identity(20));
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
        service_exports: Arc::default(),
        routing_table: std::ptr::null_mut(),
        cluster: std::ptr::null_mut(),
        reconnect_enabled: AtomicBool::new(true),
        reconnect_max_retries: AtomicU32::new(RECONNECT_DEFAULT_MAX_RETRIES),
        reconnect_shutdown: Arc::new(AtomicBool::new(false)),
        inbound_spawn_closed: Arc::new(AtomicBool::new(false)),
        inbound_ask_active: Arc::new(AtomicUsize::new(0)),
        inbound_asks: Arc::default(),
        reconnect_workers: PoisonSafe::new(Vec::new()),
        relay: relay::RelayHub::default(),
        registry: std::ptr::null(),
        seed_targets: PoisonSafe::new(Vec::new()),
        seed_identities: PoisonSafe::new(Vec::new()),
        reader_lifecycle: Arc::new(ReaderLifecycle::default()),
        inbound_admissions: Arc::new(ReaderLifecycle::default()),
        next_publication_token: AtomicU64::new(1),
        gossip_in: AtomicU64::new(0),
        gossip_out: Arc::new(AtomicU64::new(0)),
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
    // SAFETY: the manager remains live until the cleanup below.
    let mgr_ref = unsafe { &*mgr };
    reserve_unverified_identity_claim(mgr_ref, test_node_identity(20), 20, 1, 10, 1);
    assert!(retire_identity_claim(
        mgr_ref,
        test_node_identity(20),
        10,
        1
    ));
    let plan = ReconnectPlan {
        predecessor: ReconnectOwner {
            node_id: test_node_identity(20),
            session_incarnation: 1,
            publication_token: 1,
        },
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
    // transport-authenticated credential; over a transport with no channel
    // protection it is refused before any record is exchanged.
    let channel_gate = ChannelRefusal::UnprotectedTransport.to_string();
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
        error_message.contains(&channel_gate),
        "rejection must surface the channel gate, got: {error_message}"
    );

    // A bare-address reconnect is rejected at the same credential gate.
    let (_, _, _, error_message) = run_reconnect_attempt_replay_case(None);
    assert!(
        error_message.contains(&channel_gate),
        "an unpinned reconnect must still require a protected channel, got: {error_message}"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "stage the delayed retry, successor publication and preserved route together"
)]
fn stale_reconnect_cannot_replace_newer_publication() {
    for successor_session in [1, 2] {
        let (mgr_ptr, transport) = test_manager_with_transport();
        // SAFETY: this test retains the manager and routing table through cleanup.
        unsafe {
            (*mgr_ptr).routing_table = crate::routing::hew_routing_table_new_for_test(1);
        }
        // SAFETY: the manager remains live through the assertions below.
        let mgr = unsafe { &*mgr_ptr };
        mgr.reconnect_enabled.store(true, Ordering::Release);
        let identity = test_node_identity(20);
        reserve_unverified_identity_claim(mgr, identity, 20, 1, 10, 100);
        install_strict_conn(mgr, 20, 10, 100);
        mgr.connections.access(|connections| {
            connections[0].reconnect = Some(ReconnectSettings {
                target_addr: "127.0.0.1:1".into(),
                max_retries: 3,
                expected_node_id: None,
            });
        });
        assert!(publish_identity_claim(mgr, identity, 10, 100));
        let plan = reconnect_plan(mgr, 10).expect("admitted peer has reconnect owner");
        assert_eq!(plan.predecessor.node_id, identity);
        assert_eq!(plan.predecessor.session_incarnation, 1);
        assert_eq!(plan.predecessor.publication_token, 100);
        // SAFETY: the manager owns the installed predecessor.
        assert_eq!(unsafe { hew_connmgr_remove(mgr_ptr, 10) }, 0);

        // A failed attempt restores the retired predecessor so retry remains
        // possible; shutdown cancels that otherwise valid retry before connect.
        let ClaimReservation::Reserved { superseded } = reserve_identity_claim(
            mgr,
            identity,
            20,
            1,
            None,
            11,
            101,
            Some(&plan.predecessor),
            TEST_ADMISSION,
            Commit::Unordered,
        ) else {
            panic!("current retired predecessor must permit reconnect");
        };
        assert_eq!(
            reconnect_attempt(mgr_ptr, &plan, 10, 1),
            ReconnectAttemptOutcome::TransportError,
            "a provisional successor must not cancel retry before it resolves"
        );
        abort_identity_claim(mgr, identity, 11, 101, superseded);
        mgr.reconnect_shutdown.store(true, Ordering::Release);
        assert_eq!(
            reconnect_attempt(mgr_ptr, &plan, 10, 1),
            ReconnectAttemptOutcome::Cancelled
        );
        mgr.reconnect_shutdown.store(false, Ordering::Release);
        assert_eq!(
            reconnect_attempt(mgr_ptr, &plan, 10, 1),
            ReconnectAttemptOutcome::TransportError,
            "the restored predecessor permits retry to reach the stub transport"
        );

        // A separately admitted connection wins while the old retry is
        // delayed. Reuse its transport id to exercise the publication token.
        let superseded =
            reserve_unverified_identity_claim(mgr, identity, 20, successor_session, 10, 102);
        let mut actor = test_actor(10);
        actor.peer_node_id = 20;
        actor.peer_identity = Some(identity);
        actor.peer_session_incarnation = successor_session;
        actor.publication_token = 102;
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        let Ok(publication) = install_connection_actor(mgr, actor) else {
            panic!("successor installs");
        };
        assert!(publish_identity_connection_established(
            mgr,
            identity,
            20,
            successor_session,
            10,
            0,
            102,
            &publication.sync,
            &publication.removed,
            superseded,
            None,
        ));
        assert_eq!(
            reconnect_attempt(mgr_ptr, &plan, 10, 2),
            ReconnectAttemptOutcome::Cancelled
        );

        // Also drive the reservation boundary directly: a worker that passed
        // its early check before the successor published must still lose.
        assert!(matches!(
            reserve_identity_claim(
                mgr,
                identity,
                20,
                successor_session,
                None,
                12,
                103,
                Some(&plan.predecessor),
                TEST_ADMISSION,
                Commit::Unordered,
            ),
            ClaimReservation::Rejected(_)
        ));
        let claim = mgr
            .claims
            .0
            .lock_or_recover()
            .get(&identity)
            .cloned()
            .unwrap();
        assert_eq!(claim.state, ClaimState::Published);
        assert_eq!(claim.session_incarnation, successor_session);
        assert_eq!(claim.publication_token, 102);
        // SAFETY: routing table and manager are still owned by this test.
        unsafe {
            assert_eq!(
                crate::routing::hew_routing_lookup(
                    mgr.routing_table,
                    crate::pid::hew_pid_make(20, 0),
                ),
                10
            );
            assert_eq!(hew_connmgr_count(mgr_ptr), 1);
            let routing_table = mgr.routing_table;
            free_test_manager_and_transport(mgr_ptr, transport);
            crate::routing::hew_routing_table_free(routing_table);
        }
    }
}

#[test]
fn configured_seed_retries_absence_once_per_target_and_joins_on_stop() {
    struct ManagerGuard(*mut HewConnMgr);
    impl Drop for ManagerGuard {
        fn drop(&mut self) {
            // SAFETY: this guard drops before the stack-owned transport and sender.
            unsafe { hew_connmgr_free(self.0) };
        }
    }
    unsafe extern "C" fn absent(implementation: *mut std::ffi::c_void, _: *const c_char) -> c_int {
        // SAFETY: the sender remains live until the manager joins its worker.
        let attempts = unsafe { &*implementation.cast::<std::sync::mpsc::Sender<()>>() };
        attempts.send(()).unwrap();
        crate::transport::HEW_CONN_INVALID
    }
    let _clock = crate::deterministic::TEST_LOCK.lock_or_recover();
    crate::deterministic::hew_simtime_enable(1_000);
    let (attempts, observed) = std::sync::mpsc::channel::<()>();
    let ops = crate::transport::HewTransportOps {
        connect: Some(absent),
        listen: None,
        accept: None,
        send: None,
        recv: None,
        close_conn: None,
        destroy: None,
    };
    let mut transport = HewTransport {
        ops: &raw const ops,
        r#impl: (&raw const attempts).cast_mut().cast(),
    };
    // SAFETY: transport and sender outlive manager shutdown and worker join.
    unsafe {
        let mgr = hew_connmgr_new(
            &raw mut transport,
            None,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            1,
        );
        let manager = ManagerGuard(mgr);
        let seed = SeedTarget {
            target_addr: "127.0.0.1:1".into(),
            expected_node_id: Some(7),
        };
        assert!(start_seed(mgr, seed.clone()));
        assert!(start_seed(mgr, seed));
        assert!(!start_seed(
            mgr,
            SeedTarget {
                target_addr: "127.0.0.1:1".into(),
                expected_node_id: Some(8)
            }
        ));
        observed
            .recv_timeout(Duration::from_secs(1))
            .expect("initial absent dial");
        // Advance the existing virtual monotonic clock past the capped
        // backoff. Exceed transient reconnect's retry ceiling without making
        // the test wait for a succession of wall-clock backoffs.
        let watchdog = std::time::Instant::now() + Duration::from_secs(5);
        for _ in 0..8 {
            loop {
                crate::deterministic::hew_simtime_advance_ms(30_000);
                if observed.recv_timeout(Duration::from_millis(100)).is_ok() {
                    break;
                }
                assert!(
                    std::time::Instant::now() < watchdog,
                    "seed stopped retrying"
                );
            }
        }
        assert_eq!((*mgr).reconnect_workers.access(|workers| workers.len()), 1);
        drop(manager);
        // An attempt may have completed immediately before stop, but no
        // sender remains alive after the registered supervisor is joined.
        while observed.try_recv().is_ok() {}
    }
    crate::deterministic::hew_simtime_disable();
}

#[test]
fn configured_seed_follows_successor_but_never_arms_transient_reconnect() {
    // SAFETY: manager is retained until the end of the test; no worker is spawned.
    unsafe {
        let mut transport = HewTransport {
            ops: std::ptr::null(),
            r#impl: std::ptr::null_mut(),
        };
        let mgr = hew_connmgr_new(
            &raw mut transport,
            None,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            1,
        );
        let manager = &*mgr;
        let identity = test_node_identity(7);
        let old = ReconnectOwner {
            node_id: identity,
            session_incarnation: 1,
            publication_token: 40,
        };
        manager.claims.0.lock_or_recover().insert(
            identity,
            crate::peer_binding::LiveClaim {
                credential: None,
                route_slot: 7,
                session_incarnation: 2,
                conn_id: 8,
                publication_token: 42,
                state: ClaimState::Published,
                origin: ConnectionOrigin::Dialled,
                published_at: 0,
                commit_order: 0,
            },
        );
        assert!(super::super::reconnect::seed_predecessor(manager, &old).is_none());
        manager
            .claims
            .0
            .lock_or_recover()
            .get_mut(&identity)
            .unwrap()
            .state = ClaimState::Retired;
        let next = super::super::reconnect::seed_predecessor(manager, &old).unwrap();
        assert_eq!(next.session_incarnation, 2);
        assert_eq!(next.publication_token, 42);
        manager
            .seed_identities
            .access(|identities| identities.push(identity));
        manager.reconnect_enabled.store(true, Ordering::Release);
        let mut actor = test_actor(8);
        actor.peer_identity = Some(identity);
        actor.reconnect = Some(ReconnectSettings {
            target_addr: "other-address".into(),
            max_retries: 5,
            expected_node_id: None,
        });
        manager
            .connections
            .access(|connections| connections.push(actor));
        assert!(reconnect_plan(manager, 8).is_none());
        hew_connmgr_free(mgr);
    }
}

#[test]
fn connection_attempt_budget_uses_simulated_time_and_nested_scope_restores_it() {
    use crate::deterministic::{hew_simtime_advance_ms, hew_simtime_disable, hew_simtime_enable};
    use crate::transport::{connection_attempt_remaining, with_connection_attempt_deadline};
    let _clock = crate::deterministic::TEST_LOCK.lock_or_recover();
    hew_simtime_enable(1_000);
    assert_eq!(connection_attempt_remaining(), None);
    with_connection_attempt_deadline(|| {
        assert_eq!(connection_attempt_remaining(), Some(Duration::from_secs(2)));
        hew_simtime_advance_ms(1_999);
        with_connection_attempt_deadline(|| {
            assert_eq!(
                connection_attempt_remaining(),
                Some(Duration::from_millis(1))
            );
            hew_simtime_advance_ms(1);
            assert_eq!(connection_attempt_remaining(), Some(Duration::ZERO));
        });
        assert_eq!(connection_attempt_remaining(), Some(Duration::ZERO));
    });
    assert_eq!(connection_attempt_remaining(), None);
    hew_simtime_disable();
}
