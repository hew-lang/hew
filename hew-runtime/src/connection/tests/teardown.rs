//! Lock ordering and transport close during connection teardown.

use super::*;

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

    ready_rx.recv().expect("reader should signal ready");

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
            .recv()
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
    struct CloseState {
        close_tx: std::sync::mpsc::Sender<()>,
        mgr: std::sync::atomic::AtomicPtr<HewConnMgr>,
        lock_result_tx: std::sync::mpsc::Sender<bool>,
    }

    unsafe extern "C" fn signal_close_conn(impl_ptr: *mut std::ffi::c_void, _conn_id: c_int) {
        // SAFETY: test installs a CloseState as the transport impl payload.
        let state = unsafe { &*(impl_ptr.cast::<CloseState>()) };
        let mgr = state.mgr.load(Ordering::Acquire);
        let could_lock = !mgr.is_null()
            // SAFETY: the manager remains live until removal and teardown complete.
            && unsafe { (&*mgr).connections.try_access(|_| ()).is_some() };
        state
            .lock_result_tx
            .send(could_lock)
            .expect("close callback should report lock availability");
        state
            .close_tx
            .send(())
            .expect("close signal send should succeed");
    }

    let (close_tx, close_rx) = std::sync::mpsc::channel::<()>();
    let (lock_result_tx, lock_result_rx) = std::sync::mpsc::channel::<bool>();
    let close_impl = Box::into_raw(Box::new(CloseState {
        close_tx,
        mgr: std::sync::atomic::AtomicPtr::new(std::ptr::null_mut()),
        lock_result_tx,
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
    // SAFETY: close_impl points at the live CloseState allocated above.
    unsafe {
        (&*close_impl.cast::<CloseState>())
            .mgr
            .store(mgr, Ordering::Release);
    }

    let mut actor = ConnectionActor::new(41);
    actor.state.store(CONN_STATE_ACTIVE, Ordering::Release);
    actor.reader_handle = Some(std::thread::spawn(move || {
        close_rx.recv().expect("reader should observe close");
    }));

    // SAFETY: mgr is a live manager allocated by hew_connmgr_new above.
    unsafe { (&*mgr).connections.access(|conns| conns.push(actor)) };

    // SAFETY: mgr is still valid and owns the test connection above.
    assert_eq!(unsafe { hew_connmgr_remove(mgr, 41) }, 0);
    assert!(
        lock_result_rx
            .recv()
            .expect("close callback should report whether the lock was released"),
        "hew_connmgr_remove should release the connections lock before closing the transport"
    );
    // SAFETY: mgr remains valid until the free call below.
    assert_eq!(unsafe { hew_connmgr_count(mgr) }, 0);

    // SAFETY: mgr was allocated by hew_connmgr_new and is no longer used after this.
    unsafe { hew_connmgr_free(mgr) };
    // SAFETY: transport_ptr and close_impl were allocated in this test and outlive the manager.
    unsafe {
        drop(Box::from_raw(transport_ptr));
        drop(Box::from_raw(close_impl.cast::<CloseState>()));
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
            .recv()
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

    started_rx.recv().expect("free thread should start");
    // Deliberate negative-timing assertion: free must remain blocked on the reader lifecycle.
    assert!(
        done_rx
            .recv_timeout(std::time::Duration::from_millis(100))
            .is_err(),
        "hew_connmgr_free returned before the self-removed reader finished"
    );

    drop(reader_guard);
    done_rx
        .recv()
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
        let routing_table = crate::routing::hew_routing_table_new_for_test(1);
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
        let old_token =
            next_publication_token(&*mgr).expect("the publication token space is not exhausted");
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
        let old_superseded = test_reserve_unverified(&*mgr, 2, 11, old_token);
        publish_connection_established(
            &*mgr,
            2,
            11,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            old_token,
            &old_publication_sync,
            &old_publication_removed,
            old_superseded,
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
                .recv()
                .expect("remove should close the old transport before cleanup"),
            11
        );

        let mut replacement_actor = Some({
            let mut actor = ConnectionActor::new(22);
            actor.publication_token = next_publication_token(&*mgr)
                .expect("the publication token space is not exhausted");
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
        let replacement_superseded = test_reserve_unverified(&*mgr, 2, 22, replacement_token);
        publish_connection_established(
            &*mgr,
            2,
            22,
            HEW_FEATURE_SUPPORTS_GOSSIP,
            replacement_token,
            &replacement_publication_sync,
            &replacement_publication_removed,
            replacement_superseded,
        );

        reader_release_tx
            .send(())
            .expect("reader release should succeed");
        assert_eq!(
            remove_result_rx
                .recv()
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
