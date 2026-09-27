//! Publication refusal for reserved route slots and cluster rejection.

use super::*;

/// Post-conditions of a refused publication (see the caller): reported,
/// claim retired, transport closed exactly once through the guarded
/// teardown, connection unlinked, no reconnect armed, no route, no live
/// publication for this admission, no gossip drained. Existing cluster
/// membership at the colliding receiver-local alias is left untouched.
///
/// # Safety
///
/// `mgr`, `cluster`, and `routing_table` must be live.
unsafe fn assert_reserved_slot_refusal(
    mgr: *mut HewConnMgr,
    cluster: *mut crate::cluster::HewCluster,
    routing_table: *mut HewRoutingTable,
    close_rx: &std::sync::mpsc::Receiver<c_int>,
    conn_id: c_int,
    route_slot: u16,
) {
    let reported_ptr = crate::hew_last_error();
    assert!(
        !reported_ptr.is_null(),
        "the refusal must be reported through hew_last_error"
    );
    // SAFETY: non-null `hew_last_error` output is a live NUL-terminated
    // message owned by this thread.
    let reported = unsafe { std::ffi::CStr::from_ptr(reported_ptr) }
        .to_string_lossy()
        .into_owned();
    assert!(
        reported.contains(&format!("route slot {route_slot} is reserved")),
        "refusal must name the reserved slot, got: {reported}"
    );
    assert_eq!(
        close_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the refused connection must be closed"),
        conn_id
    );
    // SAFETY: caller keeps the routing table live.
    let route = unsafe {
        crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(route_slot, 0))
    };
    assert_eq!(
        route, -1,
        "no route may exist for a peer on a reserved slot"
    );
    // SAFETY: caller keeps the manager live.
    let owner = unsafe { authenticated_peer_node_id_for_conn(&*mgr, conn_id) };
    assert_eq!(
        owner, 0,
        "the claim must be retired, leaving no published owner"
    );
    // The refusal must go through the guarded removal path, so the actor is
    // unlinked synchronously. If it were left installed, manager free would
    // close its transport a second time.
    // SAFETY: caller keeps the manager live.
    let installed = unsafe { hew_connmgr_count(mgr) };
    assert_eq!(
        installed, 0,
        "the refused connection must be unlinked by the guarded teardown"
    );
    // A deliberate refusal is not an unexpected drop: `reader_stop` is set
    // before the close, so the woken reader must not arm a retry.
    // SAFETY: caller keeps the manager live.
    let reconnect_workers = unsafe { &*mgr }
        .reconnect_workers
        .access(|workers| workers.len());
    assert_eq!(
        reconnect_workers, 0,
        "a refused peer must never be scheduled for reconnect"
    );
    // Prevalidation ran before this admission installed a cluster token.
    // The already-live cluster entry at the colliding receiver-local alias
    // is not proof that it belongs to this refused identity, so refusing
    // this admission must not demote it by bare route slot.
    // SAFETY: caller keeps the cluster live.
    let member_state = unsafe { crate::cluster::hew_cluster_member_state(cluster, route_slot) };
    assert_eq!(
        member_state,
        crate::cluster::MEMBER_ALIVE,
        "prepublication refusal must not demote existing cluster membership"
    );
    // SAFETY: caller keeps the cluster live.
    let alive = unsafe { crate::cluster::hew_cluster_alive_count(cluster) };
    assert_eq!(
        alive, 1,
        "prepublication refusal must preserve the existing live member"
    );
    // SAFETY: caller keeps the cluster live.
    let pending = unsafe { &*cluster }.registry_gossip_count();
    assert_eq!(
        pending, 1,
        "registry gossip must not be flushed to an unroutable peer"
    );
}

/// `hew_routing_add_route` refuses a reserved route slot (slot `0` or this
/// node's own `local_route_slot`). Publication must fail closed on that
/// refusal instead of continuing: a peer that is published with no
/// routing-table entry turns every later send into an unexplained
/// partition rather than a reported error. Assert the refusal is
/// observable (`set_last_error`), the claim is retired, no registry gossip
/// is drained onto the doomed connection, and — because the refusal runs
/// through the guarded removal path — the transport is closed EXACTLY
/// ONCE, the actor is unlinked, the reader exits via the expected-stop
/// path without arming a reconnect. Because the refusal now precedes
/// cluster publication, an existing live cluster member at the colliding
/// receiver-local alias must not be demoted.
///
/// The connection is staged the way production stages it: reconnect armed,
/// and a reader parked until the transport close wakes it, running the real
/// `reader_cleanup`. A hand-rolled close (one that does not mark
/// `reader_stop` or claim the close first) makes that reader treat the
/// deliberate refusal as an unexpected drop — re-entering removal, closing
/// the transport a second time, and retrying the rejected peer.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "test stages the full production refusal shape — reader, reconnect, cluster — in one place"
)]
fn publication_refuses_peer_whose_route_slot_is_reserved() {
    struct RefusalTransport {
        closes: std::sync::mpsc::Sender<c_int>,
        wake_reader: std::sync::mpsc::Sender<()>,
        /// Cluster visibility sampled at the instant each close is exposed.
        cluster: *mut crate::cluster::HewCluster,
        route_slot: u16,
        seen_at_close: Mutex<Vec<(i32, c_int)>>,
    }

    unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
        // SAFETY: test installs a RefusalTransport as the transport impl payload.
        let signals = unsafe { &*(impl_ptr.cast::<RefusalTransport>()) };
        // Stand in for any other cluster/SWIM observer: what does the peer
        // look like at the moment its close becomes visible?
        // SAFETY: the test keeps the cluster alive past every close.
        let member_state = unsafe {
            crate::cluster::hew_cluster_member_state(signals.cluster, signals.route_slot)
        };
        // SAFETY: as above.
        let alive = unsafe { crate::cluster::hew_cluster_alive_count(signals.cluster) };
        signals
            .seen_at_close
            .lock_or_recover()
            .push((member_state, alive));
        signals
            .closes
            .send(conn_id)
            .expect("close signal send should succeed");
        // Mirror a real transport: closing the connection unblocks the
        // reader parked in recv().
        let _ = signals.wake_reader.send(());
    }

    const LOCAL_ROUTE_SLOT: u16 = 2;
    const CONN_ID: c_int = 33;

    let cluster_config = crate::cluster::ClusterConfig {
        local_node_id: LOCAL_ROUTE_SLOT,
        ..crate::cluster::ClusterConfig::default()
    };
    // SAFETY: `cluster_config` is a live local for the duration of the call.
    let cluster = unsafe { crate::cluster::hew_cluster_new(&raw const cluster_config) };
    assert!(!cluster.is_null());

    let (close_tx, close_rx) = std::sync::mpsc::channel::<c_int>();
    let (wake_tx, wake_rx) = std::sync::mpsc::channel::<()>();
    let (reader_done_tx, reader_done_rx) = std::sync::mpsc::channel::<()>();
    let close_impl = Box::into_raw(Box::new(RefusalTransport {
        closes: close_tx,
        wake_reader: wake_tx,
        cluster,
        route_slot: LOCAL_ROUTE_SLOT,
        seen_at_close: Mutex::new(Vec::new()),
    }))
    .cast::<std::ffi::c_void>();
    let ops = Box::new(crate::transport::HewTransportOps {
        connect: None,
        listen: None,
        accept: None,
        send: None,
        recv: None,
        close_conn: Some(signal_close_conn),
        destroy: None,
    });
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: close_impl,
    }));

    // SAFETY: every raw pointer below is allocated in this test and stays
    // valid until the matching free calls.
    unsafe {
        let routing_table = crate::routing::hew_routing_table_new_for_test(LOCAL_ROUTE_SLOT);
        assert!(!routing_table.is_null());
        assert_eq!(
            crate::cluster::hew_cluster_join(cluster, LOCAL_ROUTE_SLOT, c"10.0.0.2:9000".as_ptr()),
            0
        );
        // A pending gossip event that a successful publication would drain.
        (&*cluster).emit_registry_add(
            "counter",
            crate::node_identity::Location::new(NodeId::from_bytes([7; 16]), 9, 3)
                .expect("test location should be valid"),
        );
        assert_eq!((&*cluster).registry_gossip_count(), 1);

        let mgr = hew_connmgr_new(
            transport_ptr,
            None,
            routing_table,
            cluster,
            LOCAL_ROUTE_SLOT,
        );
        assert!(!mgr.is_null());
        // Arm reconnect so a refusal mistaken for an unexpected drop would
        // schedule a retry of the rejected peer.
        (&*mgr).reconnect_enabled.store(true, Ordering::Release);

        let mut actor = ConnectionActor::new(CONN_ID);
        let token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
        actor.publication_token = token;
        actor.peer_node_id = LOCAL_ROUTE_SLOT;
        actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        actor.reconnect = Some(ReconnectSettings {
            target_addr: "10.0.0.2:9000".to_owned(),
            max_retries: 3,
            expected_node_id: Some(LOCAL_ROUTE_SLOT),
        });
        let reader_stop = Arc::clone(&actor.reader_stop);
        let mgr_send = SendConnMgr(mgr);
        actor.reader_handle = Some(std::thread::spawn(move || {
            let mgr_send = mgr_send;
            // Park like a real reader inside recv() until the close wakes us.
            let _ = wake_rx.recv();
            reader_cleanup(mgr_send.0, CONN_ID, &reader_stop);
            let _ = reader_done_tx.send(());
        }));
        let publication_sync = Arc::clone(&actor.publication_sync);
        let publication_removed = Arc::clone(&actor.publication_removed);
        (&*mgr).connections.access(|conns| conns.push(actor));
        let superseded = test_reserve_unverified(&*mgr, LOCAL_ROUTE_SLOT, CONN_ID, token);

        // This is the manager/cluster's legitimate local member and has no
        // token belonging to the bogus peer admission below. A refusal
        // keyed only by route slot would incorrectly demote the local node.
        assert_eq!(crate::cluster::hew_cluster_alive_count(cluster), 1);
        assert_eq!(
            crate::cluster::hew_cluster_member_state(cluster, LOCAL_ROUTE_SLOT),
            crate::cluster::MEMBER_ALIVE
        );

        publish_connection_established(
            &*mgr,
            LOCAL_ROUTE_SLOT,
            CONN_ID,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            token,
            &publication_sync,
            &publication_removed,
            superseded,
        );

        // The guarded teardown joins the reader before it returns, so this
        // has already happened; waiting makes every post-condition below
        // deterministic for any teardown that does not.
        reader_done_rx
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the woken reader must have run its cleanup");

        assert_reserved_slot_refusal(
            mgr,
            cluster,
            routing_table,
            &close_rx,
            CONN_ID,
            LOCAL_ROUTE_SLOT,
        );
        assert!(
            close_rx.try_recv().is_err(),
            "the woken reader must not treat the refusal as an unexpected drop \
             and close the transport again"
        );

        hew_connmgr_free(mgr);
        // Exactly once: the guarded teardown already closed and unlinked the
        // connection, so manager free has nothing left to close.
        assert!(
            close_rx.try_recv().is_err(),
            "the refused transport must be closed exactly once"
        );

        // The refusal happened before cluster publication. The close
        // observer must still see the pre-existing member alive: demoting
        // it would conflate a receiver-local alias with admission identity.
        // SAFETY: `close_impl` is the live RefusalTransport allocated above.
        let seen_at_close = (*close_impl.cast::<RefusalTransport>())
            .seen_at_close
            .lock_or_recover()
            .clone();
        assert_eq!(
            seen_at_close,
            vec![(crate::cluster::MEMBER_ALIVE, 1)],
            "prepublication refusal must not demote existing cluster membership"
        );

        crate::cluster::hew_cluster_free(cluster);
        crate::routing::hew_routing_table_free(routing_table);
    }

    // SAFETY: transport_ptr and close_impl were allocated above and are no
    // longer referenced after manager teardown.
    unsafe {
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(close_impl.cast::<RefusalTransport>()));
    }
    drop(ops);
}

/// The cluster can refuse an admission outright — a session regression on
/// the peer's route slot, or a member already buried DEAD/LEFT — and that
/// happens BEFORE route registration. Publication must fail closed there
/// too: retiring only the claim would leave the connection installed with
/// an open transport, reading frames as an unroutable, unowned peer. Assert
/// the refused connection is torn down exactly once through the guarded
/// path, and that the token guard keeps the refusal from touching the peer
/// that legitimately holds the slot.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "test stages an established publication and the refused successor in one place"
)]
fn publication_refuses_peer_the_cluster_rejects() {
    unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, conn_id: c_int) {
        // SAFETY: test installs a Sender<c_int> as the transport impl payload.
        let tx = unsafe { &*(impl_ptr.cast::<std::sync::mpsc::Sender<c_int>>()) };
        tx.send(conn_id).expect("close signal send should succeed");
    }

    const ROUTE_SLOT: u16 = 2;
    const ESTABLISHED_CONN: c_int = 41;
    const REFUSED_CONN: c_int = 42;

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
    let transport_ptr = Box::into_raw(Box::new(HewTransport {
        ops: &raw const *ops,
        r#impl: close_impl,
    }));
    let cluster_config = crate::cluster::ClusterConfig {
        local_node_id: 1,
        ..crate::cluster::ClusterConfig::default()
    };

    // SAFETY: every raw pointer below is allocated in this test and stays
    // valid until the matching free calls.
    unsafe {
        let routing_table = crate::routing::hew_routing_table_new_for_test(1);
        let cluster = crate::cluster::hew_cluster_new(&raw const cluster_config);
        assert!(!routing_table.is_null() && !cluster.is_null());
        assert_eq!(
            crate::cluster::hew_cluster_join(cluster, ROUTE_SLOT, c"10.0.0.2:9000".as_ptr()),
            0
        );

        let mgr = hew_connmgr_new(transport_ptr, None, routing_table, cluster, 1);
        assert!(!mgr.is_null());

        // The peer that legitimately owns the slot, at session 5.
        let established_identity = NodeId::from_bytes([5; 16]);
        let mut established = ConnectionActor::new(ESTABLISHED_CONN);
        let established_token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
        established.publication_token = established_token;
        established.peer_node_id = ROUTE_SLOT;
        established.peer_identity = Some(established_identity);
        established.peer_session_incarnation = 5;
        established
            .state
            .store(CONN_STATE_ACTIVE, Ordering::Release);
        let established_sync = Arc::clone(&established.publication_sync);
        let established_removed = Arc::clone(&established.publication_removed);
        (&*mgr).connections.access(|conns| conns.push(established));
        let superseded = reserve_unverified_identity_claim(
            &*mgr,
            established_identity,
            ROUTE_SLOT,
            5,
            ESTABLISHED_CONN,
            established_token,
        );
        publish_identity_connection_established(
            &*mgr,
            established_identity,
            ROUTE_SLOT,
            5,
            ESTABLISHED_CONN,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            established_token,
            &established_sync,
            &established_removed,
            superseded,
        );
        assert_eq!(
            crate::routing::hew_routing_lookup(
                routing_table,
                crate::pid::hew_pid_make(ROUTE_SLOT, 0)
            ),
            ESTABLISHED_CONN,
            "the established peer must own the route"
        );

        // A pending gossip event that a successful publication would drain.
        (&*cluster).emit_registry_add(
            "counter",
            crate::node_identity::Location::new(NodeId::from_bytes([7; 16]), 9, 3)
                .expect("test location should be valid"),
        );
        assert_eq!((&*cluster).registry_gossip_count(), 1);

        // A different NodeId lands on the same route slot with an older
        // session, so the cluster refuses the publication outright.
        let refused_identity = NodeId::from_bytes([6; 16]);
        let mut refused = ConnectionActor::new(REFUSED_CONN);
        let refused_token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
        refused.publication_token = refused_token;
        refused.peer_node_id = ROUTE_SLOT;
        refused.peer_identity = Some(refused_identity);
        refused.peer_session_incarnation = 3;
        refused.state.store(CONN_STATE_ACTIVE, Ordering::Release);
        let refused_sync = Arc::clone(&refused.publication_sync);
        let refused_removed = Arc::clone(&refused.publication_removed);
        (&*mgr).connections.access(|conns| conns.push(refused));
        let refused_superseded = reserve_unverified_identity_claim(
            &*mgr,
            refused_identity,
            ROUTE_SLOT,
            3,
            REFUSED_CONN,
            refused_token,
        );
        publish_identity_connection_established(
            &*mgr,
            refused_identity,
            ROUTE_SLOT,
            3,
            REFUSED_CONN,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            refused_token,
            &refused_sync,
            &refused_removed,
            refused_superseded,
        );

        let reported_ptr = crate::hew_last_error();
        assert!(!reported_ptr.is_null());
        let reported = std::ffi::CStr::from_ptr(reported_ptr)
            .to_string_lossy()
            .into_owned();
        assert!(
            reported.contains("the cluster rejected session 3"),
            "the cluster refusal must be reported, got: {reported}"
        );
        assert_eq!(
            close_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .expect("the refused connection must be closed"),
            REFUSED_CONN
        );
        assert!(
            close_rx.try_recv().is_err(),
            "the refused transport must be closed exactly once"
        );
        assert_eq!(
            hew_connmgr_count(mgr),
            1,
            "only the refused connection may be unlinked"
        );
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, REFUSED_CONN),
            0,
            "the refused claim must be retired"
        );
        assert_eq!((&*cluster).registry_gossip_count(), 1);

        // The token guard keeps the refusal local to the refused
        // admission: the peer that legitimately owns the slot keeps its
        // route, its published owner and its alive membership.
        assert_eq!(
            crate::routing::hew_routing_lookup(
                routing_table,
                crate::pid::hew_pid_make(ROUTE_SLOT, 0)
            ),
            ESTABLISHED_CONN
        );
        assert_eq!(
            authenticated_peer_node_id_for_conn(&*mgr, ESTABLISHED_CONN),
            ROUTE_SLOT
        );
        assert_eq!(
            crate::cluster::hew_cluster_member_state(cluster, ROUTE_SLOT),
            crate::cluster::MEMBER_ALIVE
        );
        assert_eq!(crate::cluster::hew_cluster_alive_count(cluster), 1);

        hew_connmgr_free(mgr);
        assert_eq!(
            close_rx
                .recv_timeout(std::time::Duration::from_secs(5))
                .expect("manager free must close the established connection"),
            ESTABLISHED_CONN
        );
        assert!(close_rx.try_recv().is_err());
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

// ---- teardown is exactly-once under concurrent close paths -------------
