//! Manager free and removal ordering against routes and publication.

use super::*;

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
        let pair = unsafe { &*impl_ptr.cast::<(Arc<AtomicI32>, std::sync::mpsc::Sender<i32>)>() };
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
    let publication_token = unsafe { next_publication_token(&*mgr) }
        .expect("the publication token space is not exhausted");
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
        let publication_token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
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
            let rc = crate::cluster::hew_cluster_notify_connection_lost_if_current(cluster.0, 2, 1);
            assert_eq!(rc, 0);
            lost_done_tx
                .send(())
                .expect("lost thread should report completion");
        });

        suspect_rx
            .recv()
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
                .recv()
                .expect("remove should close the test connection before publish resumes"),
            22
        );
        release.wait();

        lost_done_rx
            .recv()
            .expect("lost thread should finish once released");
        publish_done_rx
            .recv()
            .expect("publish thread should finish after the old lost transition");
        assert_eq!(
            remove_done_rx
                .recv()
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
                crate::routing::hew_routing_lookup(routing_table, crate::pid::hew_pid_make(2, 0),),
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
        let publication_token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
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
                .recv()
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
