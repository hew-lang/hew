//! Inbound ask admission and node-stop drain tests.

use super::*;
use crate::hew_node::inbound::{
    handle_inbound_ask, send_reply_envelope, INBOUND_ASK_ERROR_REPLY_HOOK,
    INBOUND_ASK_REPLY_ADMISSION_HOOK, INBOUND_ROUTER_AFTER_INCREMENT_HOOK,
};

#[test]
fn secondary_node_stop_does_not_fail_current_node_pending_asks() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let node1_bind = CString::new("127.0.0.1:0").unwrap();

    // SAFETY: bind addresses are valid C strings for the duration of this test.
    let node1 = unsafe { TestNode::new(318, &node1_bind) };
    assert!(!node1.as_ptr().is_null());

    // SAFETY: node1 comes from TestNode::new and is valid for start-up here.
    unsafe {
        assert_eq!(hew_node_start(node1.as_ptr()), 0);
    }
    let (node2, _node2_port) = start_tcp_test_listener_node(319);

    let (request_id, pending) = reply_table().register(ConnectionKey {
        conn_mgr: 1,
        conn_id: 0,
    });

    // SAFETY: node2 remains valid here and stopping it is the behavior under test.
    unsafe {
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        guard.is_none(),
        "secondary node stop must not fail pending asks owned by CURRENT_NODE"
    );
    drop(guard);
    reply_table().remove(request_id);

    // SAFETY: node1 remains valid until the end of the test.
    unsafe {
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[test]
fn send_reply_envelope_bails_before_touching_stopping_connmgr() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").unwrap();
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node = unsafe { TestNode::new(317, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    // SAFETY: node pointer is valid for start/stop in this scope.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);
    }

    let shutdown_started = Arc::new(AtomicBool::new(true));
    let dangling_mgr = std::ptr::NonNull::<connection::HewConnMgr>::dangling().as_ptr();
    send_reply_envelope(999, 123, 0, &[], dangling_mgr, shutdown_started.as_ref());

    // SAFETY: node pointer remains valid until drop.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

// ── Regression: race-window fixes ────────────────────────────────────

/// After `hew_connmgr_mark_stopping` is called, `node_inbound_router`
/// must not spawn a new inbound-ask worker (the spawn-window fix).
///
/// This test marks the connection manager as stopping, then calls the
/// inbound router directly with an ask-shaped message and asserts that
/// the per-manager counter stays at zero: no request was admitted.
#[test]
fn inbound_router_no_spawn_after_shutdown_started() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let (node, _port) = start_tcp_test_listener_node(351);

    // Capture per-manager counter before marking stopping.
    // SAFETY: node was just started and conn_mgr is valid here.
    let per_mgr_active =
        unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
            .expect("conn_mgr must have inbound_ask_active");

    // Mark the connection manager as stopping — this is what hew_node_stop
    // does inside its CURRENT_NODE write lock.
    // SAFETY: node was just started and conn_mgr is valid here.
    unsafe { connection::hew_connmgr_mark_stopping((*node.as_ptr()).conn_mgr) };

    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        0,
        "precondition: per-manager counter must be zero before the test call"
    );

    // Call the inbound router with an ask-shaped message (request_id > 0,
    // source_node_id > 0).  The router should bail without spawning.
    // SAFETY: conn_mgr is valid; null data with size 0 is the empty-payload contract.
    unsafe {
        node_inbound_router(
            0,
            c"tests.Counter.handle".as_ptr(),
            ptr::null_mut(),
            0,
            1,
            1,
            (*node.as_ptr()).conn_mgr,
            crate::node_identity::HewNodeId::default(),
            0,
            0,
            -1,
            crate::request_state::RequestKind::CallReject as u8,
        );
    }

    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        0,
        "per-manager counter must not increase after shutdown has started"
    );

    // SAFETY: node is valid and owned by the TestNode guard.
    unsafe { assert_eq!(hew_node_stop(node.as_ptr()), 0) };
    crate::registry::hew_registry_clear();
}

/// `hew_node_stop` must return only after the per-manager inbound-ask
/// active counter reaches zero (the drain postcondition).
///
/// This test reserves one per-manager drain reference, lets a background
/// thread release it once stop is under way, and then asserts
/// that stop waited for the decrement (counter is zero on return).
#[test]
fn node_stop_drains_inbound_ask_active() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let (node, _port) = start_tcp_test_listener_node(352);

    // Capture the per-manager counter Arc while conn_mgr is still live.
    // SAFETY: node was just started and conn_mgr is valid here.
    let per_mgr_active =
        unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
            .expect("conn_mgr must have inbound_ask_active");

    per_mgr_active.fetch_add(1, Ordering::AcqRel);
    let per_mgr_clone = Arc::clone(&per_mgr_active);
    let (stopping_tx, stopping_rx) = std::sync::mpsc::channel::<()>();
    let decrement_handle = thread::spawn(move || {
        stopping_rx.recv().expect("stop start signal");
        per_mgr_clone.fetch_sub(1, Ordering::AcqRel);
    });

    let node_ptr = node.as_ptr() as usize;
    let stop_handle = thread::spawn(move || {
        // SAFETY: node stays owned by TestNode until this thread joins.
        unsafe { hew_node_stop(node_ptr as *mut HewNode) }
    });
    stopping_tx.send(()).expect("decrement thread live");
    assert_eq!(stop_handle.join().expect("stop thread panicked"), 0);
    decrement_handle.join().expect("decrement thread panicked");

    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        0,
        "per-manager counter must be zero after node_stop"
    );

    crate::registry::hew_registry_clear();
}

#[test]
fn node_stop_waits_to_free_connmgr_until_inbound_ask_error_worker_drains() {
    struct HookResetGuard;

    impl Drop for HookResetGuard {
        fn drop(&mut self) {
            INBOUND_ASK_ERROR_REPLY_HOOK.release();
            INBOUND_ASK_ERROR_REPLY_HOOK.reset();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.release();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.reset();
        }
    }

    let _guard = crate::runtime_test_guard();
    let _real_sched;
    let _hook_reset = HookResetGuard;
    crate::registry::hew_registry_clear();

    INBOUND_ASK_ERROR_REPLY_HOOK.arm(true);
    NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.arm(false);

    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(353, 354);

    _real_sched = init_real_scheduler();

    crate::pid::hew_pid_set_local_node(354);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(353);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 354);

    let connect_addr = CString::new(format!("354@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: node2 was started above and its conn_mgr is live here.
    let per_mgr_active =
        unsafe { connection::hew_connmgr_inbound_ask_active((*node2.as_ptr()).conn_mgr) }
            .expect("conn_mgr must expose inbound_ask_active");

    // SAFETY: actor was spawned above and remains valid while stopped here.
    unsafe { crate::actor::hew_actor_stop(actor) };

    // Run the router on its reader thread while the rejection publication is held.
    // SAFETY: node2 retains the manager until both test threads have joined.
    let manager = unsafe { (*node2.as_ptr()).conn_mgr } as usize;
    let router = thread::spawn(move || {
        // SAFETY: the manager and peer are live; the empty payload is intentional.
        unsafe {
            node_inbound_router(
                actor_id,
                c"tests.Counter.handle".as_ptr(),
                ptr::null_mut(),
                0,
                1,
                353,
                manager as *mut connection::HewConnMgr,
                crate::node_identity::HewNodeId::default(),
                0,
                0,
                -1,
                crate::request_state::RequestKind::CallReject as u8,
            );
        }
    });

    assert!(
        INBOUND_ASK_ERROR_REPLY_HOOK.wait_until_entered(),
        "inbound ask error path never reached the feature-flags lookup hook"
    );
    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        1,
        "worker must remain counted while the error path is blocked"
    );

    let (stop_tx, stop_rx) = std::sync::mpsc::channel();
    let node2_ptr = node2.as_ptr() as usize;
    let stop_handle = thread::spawn(move || {
        // SAFETY: node2_ptr comes from the live TestNode allocation above
        // and remains valid until this stop thread joins.
        let rc = unsafe { hew_node_stop(node2_ptr as *mut HewNode) };
        stop_tx.send(rc).expect("stop result receiver dropped");
    });

    assert!(
        !NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_millis(100)),
        "hew_node_stop must not reach conn_mgr free while an inbound-ask error worker still holds the manager"
    );

    INBOUND_ASK_ERROR_REPLY_HOOK.release();
    router.join().expect("router completed");

    assert!(
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_until_entered(),
        "hew_node_stop should reach conn_mgr free after the worker drains"
    );
    assert_eq!(
        stop_rx
            .recv()
            .expect("hew_node_stop did not finish after the worker drained"),
        0,
        "hew_node_stop should succeed once the inbound-ask worker drains"
    );
    stop_handle.join().expect("stop thread panicked");

    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        0,
        "per-manager worker count must be zero after node_stop returns"
    );

    // SAFETY: actor and node1 remain valid until teardown here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Pins the ATOMIC spawn-gate + counter protocol in `node_inbound_router`:
/// a worker that has incremented the per-manager counter (passed the gate's
/// increment edge) is ALWAYS observed by a concurrent `hew_node_stop` drain,
/// so `conn_mgr` is never freed (Phase 2) while that worker is live.
///
/// Without the increment-then-recheck protocol the router checked the gate
/// BEFORE incrementing, so a router could pass the check, have the drain
/// close the gate and observe a zero counter, then increment + spawn AFTER
/// the drain finished — reaching teardown and abandoning its reply / touching
/// freed memory. This test wedges a router in the exact Dekker window
/// (counter incremented, gate not yet re-read) and proves the drain blocks
/// for it.
#[test]
fn node_stop_drain_waits_for_router_wedged_after_counter_increment() {
    struct HookResetGuard;
    impl Drop for HookResetGuard {
        fn drop(&mut self) {
            INBOUND_ROUTER_AFTER_INCREMENT_HOOK.release();
            INBOUND_ROUTER_AFTER_INCREMENT_HOOK.reset();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.release();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.reset();
        }
    }

    let _guard = crate::runtime_test_guard();
    let _hook_reset = HookResetGuard;
    crate::registry::hew_registry_clear();

    // Block a router that reaches the post-increment point; observe (do not
    // block) when stop reaches the conn_mgr free.
    INBOUND_ROUTER_AFTER_INCREMENT_HOOK.arm(true);
    NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.arm(false);

    let (node, _port) = start_tcp_test_listener_node(361);

    // SAFETY: node just started; conn_mgr is live here.
    let per_mgr_active =
        unsafe { connection::hew_connmgr_inbound_ask_active((*node.as_ptr()).conn_mgr) }
            .expect("conn_mgr must expose inbound_ask_active");

    // Drive a router on a background thread with an ask-shaped message. It
    // increments the per-manager counter, then blocks at the post-increment
    // hook BEFORE the gate re-check — the precise Dekker window.
    // SAFETY: node is live here; we read its conn_mgr pointer as an integer
    // to ferry it across the thread boundary (it stays valid until join).
    let conn_mgr_addr = unsafe { (*node.as_ptr()).conn_mgr } as usize;
    let router_handle = thread::spawn(move || {
        // SAFETY: conn_mgr stays live until this test frees it after join.
        unsafe {
            node_inbound_router(
                0,
                c"tests.Counter.handle".as_ptr(),
                ptr::null_mut(),
                0,
                1,
                1,
                conn_mgr_addr as *mut connection::HewConnMgr,
                crate::node_identity::HewNodeId::default(),
                0,
                0,
                -1,
                crate::request_state::RequestKind::CallReject as u8,
            );
        }
    });

    assert!(
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.wait_until_entered(),
        "router never reached the post-increment hook"
    );
    assert_eq!(
        per_mgr_active.load(Ordering::SeqCst),
        1,
        "router must have incremented the per-manager counter before the gate re-check"
    );

    // Start stop on another thread. It closes the spawn gate (SeqCst) and
    // drains; the drain MUST block on the wedged router's increment.
    let node_ptr = node.as_ptr() as usize;
    let (stop_tx, stop_rx) = std::sync::mpsc::channel();
    let stop_handle = thread::spawn(move || {
        // SAFETY: node stays valid until this stop thread joins below.
        let rc = unsafe { hew_node_stop(node_ptr as *mut HewNode) };
        stop_tx.send(rc).expect("stop result receiver dropped");
    });

    // The drain is wedged: stop must NOT reach the conn_mgr free while the
    // counted router is still parked at the hook.
    assert!(
        !NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_millis(200)),
        "hew_node_stop reached conn_mgr free while a router counted by the drain was still wedged \
         — the spawn-gate/counter protocol failed to keep the worker visible to the drain"
    );
    assert!(
        stop_rx.recv_timeout(Duration::from_millis(50)).is_err(),
        "hew_node_stop returned before the wedged router drained"
    );

    // Release the router. It re-reads the now-closed gate, decrements, and
    // bails WITHOUT spawning — the counter returns to zero, unblocking the
    // drain. (The gate was closed by stop before the drain, so the re-check
    // sees it closed: this is the Dekker `router observes gate closed` leg.)
    INBOUND_ROUTER_AFTER_INCREMENT_HOOK.release();
    router_handle.join().expect("router thread panicked");

    assert!(
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_until_entered(),
        "hew_node_stop never reached conn_mgr free after the wedged router drained"
    );
    assert_eq!(
        stop_rx
            .recv()
            .expect("hew_node_stop did not finish after the router drained"),
        0,
        "hew_node_stop should succeed once the wedged router drained"
    );
    stop_handle.join().expect("stop thread panicked");

    assert_eq!(
        per_mgr_active.load(Ordering::SeqCst),
        0,
        "per-manager counter must be zero after the wedged router bailed and stop drained"
    );

    crate::registry::hew_registry_clear();
}

/// Pins the SECONDARY-node safety of the up-front feature-flags capture in
/// `handle_inbound_ask`.
///
/// The capture's barrier must be keyed on the SPECIFIC manager's lifetime,
/// not the global `CURRENT_NODE`. In a multi-node runtime `CURRENT_NODE`
/// points at the FIRST node started (node1 here) and stays there; stopping a
/// secondary node (node2) frees node2's `conn_mgr` WITHOUT zeroing
/// `CURRENT_NODE`. A `*guard == 0` check therefore passes during a secondary
/// teardown, so the OLD capture dereferenced node2's already-freed
/// `conn_mgr` (use-after-free; the deeper freed-`conn_mgr` path surfaced
/// under `AddressSanitizer`). The fix also consults THIS manager's
/// `shutdown_started` (`reconnect_shutdown`) flag, which `hew_node_stop` sets
/// for the stopping node under the `CURRENT_NODE` write lock before it frees
/// the manager.
///
/// This test reproduces the straggler exactly: it drives `handle_inbound_ask`
/// for node2 DIRECTLY (uncounted by the per-manager guard, i.e. a worker the
/// drain no longer waits on), wedges it at the capture hook, lets
/// `hew_node_stop(node2)` run to completion (freeing node2's `conn_mgr` while
/// `CURRENT_NODE` still points at node1), then releases the worker. With the
/// fix the worker observes node2's `shutdown_started` and returns `None`
/// WITHOUT touching the freed manager; under `AddressSanitizer` the run is
/// clean. Against the old `*guard == 0`-only capture the release reads freed
/// memory.
#[test]
fn handle_inbound_ask_capture_safe_for_secondary_node_freed_connmgr() {
    struct HookResetGuard;
    impl Drop for HookResetGuard {
        fn drop(&mut self) {
            INBOUND_ASK_REPLY_ADMISSION_HOOK.release();
            INBOUND_ASK_REPLY_ADMISSION_HOOK.reset();
        }
    }

    let _guard = crate::runtime_test_guard();
    let _real_sched;
    let _hook_reset = HookResetGuard;
    crate::registry::hew_registry_clear();

    // Wedge any worker that reaches the top-of-handler capture hook.
    INBOUND_ASK_REPLY_ADMISSION_HOOK.arm(true);

    // node1 starts FIRST and keeps CURRENT_NODE; node2 remains the secondary.
    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(363, 364);

    _real_sched = init_real_scheduler();

    // Spawn (and immediately stop) a node2-PID actor so the inbound ask hits
    // the actor-error path — exercising the full post-capture handler on a
    // freed-conn_mgr straggler. The error path's reply send is itself
    // `shutdown_started`-guarded, so no path may touch the freed manager.
    crate::pid::hew_pid_set_local_node(364);
    let actor = spawn_remote_test_actor(noop_dispatch);
    crate::pid::hew_pid_set_local_node(363);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 364);
    // SAFETY: actor was spawned above and remains valid while stopped here.
    unsafe { crate::actor::hew_actor_stop(actor) };

    // Establish the node1→node2 connection so node2 has an ACTIVE connection
    // back to node1 — the entry `feature_flags_for_node` would walk if the
    // capture dereferenced the (soon-to-be-freed) manager.
    let connect_addr = CString::new(format!("364@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until teardown.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Capture node2's manager pointer (as an integer to ferry across the
    // thread) and a clone of its `shutdown_started` (`reconnect_shutdown`)
    // flag — exactly what `node_inbound_router` passes into the worker.
    // SAFETY: node2 is live here; conn_mgr is valid until we free it below.
    let node2_conn_mgr = unsafe { (*node2.as_ptr()).conn_mgr };
    assert!(!node2_conn_mgr.is_null());
    // SAFETY: node2's conn_mgr is live here.
    let shutdown_started = unsafe { connection::hew_connmgr_shutdown_flag(node2_conn_mgr) }
        .expect("conn_mgr must expose its shutdown flag");
    let conn_mgr_addr = node2_conn_mgr as usize;

    // Drive `handle_inbound_ask` for node2 DIRECTLY — uncounted by the
    // per-manager guard, modelling a straggler the drain has already given
    // up on. It parks at the capture hook before touching conn_mgr.
    let worker_handle = thread::spawn(move || {
        handle_inbound_ask(
            actor_id,
            "tests.Counter.handle",
            &[],
            1,
            363,
            SendConnMgr(conn_mgr_addr as *mut connection::HewConnMgr),
            shutdown_started,
            crate::actor_caller::HewCallerStamp::default(),
            0,
            0,
            crate::request_deadline::RequestDeadline::default(),
            crate::request_state::RequestKind::CallReject,
        );
    });

    assert!(
        INBOUND_ASK_REPLY_ADMISSION_HOOK.wait_until_entered(),
        "straggler worker never reached the feature-flags capture hook"
    );

    // Stop the SECONDARY node to completion. Its drain sees no counted
    // workers and proceeds: it sets node2's `reconnect_shutdown` (under the
    // CURRENT_NODE write lock) and frees node2's conn_mgr — while
    // CURRENT_NODE still points at node1 (so `*guard != 0`).
    // SAFETY: node2 is a live allocation; stop is the documented teardown.
    let stop_rc = unsafe { hew_node_stop(node2.as_ptr()) };
    assert_eq!(stop_rc, 0, "hew_node_stop(node2) should succeed");

    // node2's conn_mgr is now FREED. Release the wedged worker: with the fix
    // it observes node2's `shutdown_started == true` and returns `None`
    // WITHOUT dereferencing the freed manager. (ASan would fire here on the
    // old `*guard == 0`-only capture.)
    INBOUND_ASK_REPLY_ADMISSION_HOOK.release();
    worker_handle
        .join()
        .expect("straggler worker panicked after secondary-node teardown");

    crate::registry::hew_registry_clear();
}

#[test]
fn queued_withdrawal_requires_the_exact_request_and_connection_publication() {
    let _guard = crate::runtime_test_guard();
    let (node, port, peer, _) = start_authorized_tcp_pair(365, 366);
    crate::pid::hew_pid_set_local_node(365);
    let actor = spawn_remote_test_actor(noop_dispatch);
    inbound_test_codec(noop_dispatch, 0);
    // SAFETY: this test retains the node and actor through every admission.
    let (actor_id, manager) = unsafe { ((*actor).id, &*(*node.as_ptr()).conn_mgr) };
    // SAFETY: the test retains both authenticated nodes through every admission.
    let peer_id = unsafe { (*peer.as_ptr()).auth.node_identity().unwrap() };
    let address = CString::new(format!("365@127.0.0.1:{port}")).unwrap();
    for _ in 0..2 {
        // SAFETY: each connect publishes a fresh authenticated connection.
        unsafe {
            connect_with_retry(peer.as_ptr(), &address);
            wait_for_handshake(peer.as_ptr(), node.as_ptr());
        }
        let claim = connection::claim_send_by_identity(manager, peer_id, None).unwrap();
        let publication = claim.publication_token;
        let location = connection::local_location_for_actor(manager, actor_id).unwrap();
        // SAFETY: the test retains this actor throughout both publication checks.
        let target = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(actor) };
        let (_export, offer) = manager
            .service_exports
            .export(
                claim.peer_session(),
                42,
                target,
                location,
                vec!["tests.Counter.handle".into()],
            )
            .unwrap();
        handle_inbound_ask(
            actor_id,
            "tests.Counter.handle",
            &[7],
            42,
            366,
            SendConnMgr(ptr::from_ref(manager).cast_mut()),
            Arc::new(AtomicBool::new(false)),
            crate::actor_caller::HewCallerStamp::peer(peer_id.into()),
            publication,
            offer.grant_id,
            crate::request_deadline::RequestDeadline::default(),
            crate::request_state::RequestKind::CallReject,
        );
        assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 1);
        for (target, connection, request) in [
            (actor_id + 1, publication, 42),
            (actor_id, publication - 1, 42),
            (actor_id, publication, 43),
        ] {
            manager.inbound_asks.withdraw(target, connection, request);
            assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 1);
        }
        manager.inbound_asks.withdraw(actor_id, publication, 42);
        assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 0);
        manager.inbound_asks.withdraw(actor_id, publication, 42);
        assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 0);
    }
    // SAFETY: the queued requests have released their ownership above.
    unsafe {
        crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one admitted request fixture verifies grant revocation at dispatch"
)]
fn service_revocation_refuses_an_already_queued_dispatch_claim() {
    unsafe extern "C" fn drop_unit(_payload: *mut c_void) {}
    let _guard = crate::runtime_test_guard();
    let (node, port, peer, _) = start_authorized_tcp_pair(367, 368);
    crate::pid::hew_pid_set_local_node(367);
    let actor = spawn_remote_test_actor(noop_dispatch);
    inbound_test_codec(noop_dispatch, 0);
    let address = CString::new(format!("367@127.0.0.1:{port}")).unwrap();
    // SAFETY: this test retains both nodes and the actor until all requests retire.
    let (actor_id, incarnation, manager, peer_id) = unsafe {
        connect_with_retry(peer.as_ptr(), &address);
        wait_for_handshake(peer.as_ptr(), node.as_ptr());
        (
            (*actor).id,
            crate::lifetime::live_actors::ActorIncarnation::of(actor),
            &*(*node.as_ptr()).conn_mgr,
            (*peer.as_ptr()).auth.node_identity().unwrap(),
        )
    };
    let claim = connection::claim_send_by_identity(manager, peer_id, None).unwrap();
    let location = connection::local_location_for_actor(manager, actor_id).unwrap();
    // An authenticated peer cannot call even a never-exported actor without a grant.
    assert!(matches!(
        super::super::service::RemoteAdmission::new(
            manager,
            incarnation,
            peer_id,
            claim.publication_token,
            0,
            "tests.Counter.handle",
        ),
        Err(AskError::Unauthorized)
    ));
    let (export, offer) = manager
        .service_exports
        .export(
            claim.peer_session(),
            99,
            incarnation,
            location,
            vec!["tests.Counter.handle".into()],
        )
        .unwrap();
    assert!(matches!(
        super::super::service::RemoteAdmission::new(
            manager,
            incarnation,
            peer_id,
            claim.publication_token,
            offer.grant_id,
            "tests.Counter.other",
        ),
        Err(AskError::Unauthorized)
    ));
    let publication_removed = AtomicBool::new(false);
    let mut wrong_session = claim.peer_session();
    wrong_session.session_incarnation += 1;
    assert!(manager
        .service_exports
        .claim_observation(
            incarnation,
            wrong_session,
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            crate::lifetime::live_actors::ActorIncarnation::NONE,
            claim.peer_session(),
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            incarnation,
            claim.peer_session(),
            0,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            incarnation,
            claim.peer_session(),
            offer.grant_id,
            &AtomicBool::new(true),
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            incarnation,
            claim.peer_session(),
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_ok());
    let admission = super::super::service::RemoteAdmission::new(
        manager,
        incarnation,
        peer_id,
        claim.publication_token,
        offer.grant_id,
        "tests.Counter.handle",
    )
    .unwrap();
    let (_readiness, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: the test transfers one request with a live readiness owner.
    let operation = unsafe {
        crate::actor_call_native::actor_call_new(
            (*actor).local_pid_id,
            1,
            crate::mem::buf_alloc(1),
            1,
            drop_unit,
            0,
            None,
            waker.descriptor(),
            -1,
            crate::request_state::RequestKind::CallWait,
            None,
            None,
            crate::actor_caller::HewCallerStamp::peer(peer_id.into()),
            ptr::null(),
            Some(admission),
        )
    };
    drop(export);
    drop(claim);
    // SAFETY: the test is the mailbox's sole consumer; no scheduler runs this actor.
    unsafe {
        let message = crate::mailbox::hew_mailbox_try_recv((*actor).mailbox.cast());
        assert!(!message.is_null(), "the request must already be queued");
        assert!(!crate::request_state::claim_message(
            (*message).envelope,
            (*message).reply_channel.cast()
        ));
        crate::mailbox::hew_msg_node_free(message);
        assert_eq!(
            crate::actor_call_native::hew_actor_call_poll(operation),
            AskError::Unauthorized as i32
        );
        crate::actor_call_native::hew_actor_call_free(operation);
    }
    // SAFETY: revocation and withdrawal released every queued request owner.
    unsafe {
        crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one admission scenario follows authority through queued dispatch"
)]
fn mailbox_admission_precedes_dispatch_and_preserves_queued_authority() {
    unsafe extern "C" fn drop_unit(_payload: *mut c_void) {}
    let _guard = crate::runtime_test_guard();
    for kind in [
        crate::request_state::RequestKind::SendWait,
        crate::request_state::RequestKind::SendReject,
        crate::request_state::RequestKind::SendDropNewest,
    ] {
        for withdraw in [false, true] {
            let (node, port, peer, _) = start_authorized_tcp_pair(367, 368);
            crate::pid::hew_pid_set_local_node(367);
            let actor = spawn_remote_test_actor(noop_dispatch);
            inbound_test_codec(noop_dispatch, 0);
            let address = CString::new(format!("367@127.0.0.1:{port}")).unwrap();
            // SAFETY: this test retains both nodes and the actor until all requests retire.
            let (actor_id, incarnation, manager, peer_id) = unsafe {
                connect_with_retry(peer.as_ptr(), &address);
                wait_for_handshake(peer.as_ptr(), node.as_ptr());
                (
                    (*actor).id,
                    crate::lifetime::live_actors::ActorIncarnation::of(actor),
                    &*(*node.as_ptr()).conn_mgr,
                    (*peer.as_ptr()).auth.node_identity().unwrap(),
                )
            };
            let claim = connection::claim_send_by_identity(manager, peer_id, None).unwrap();
            let location = connection::local_location_for_actor(manager, actor_id).unwrap();
            let (export, offer) = manager
                .service_exports
                .export(
                    claim.peer_session(),
                    99,
                    incarnation,
                    location,
                    vec!["tests.Counter.handle".into()],
                )
                .unwrap();
            assert!(matches!(
                super::super::service::RemoteAdmission::new(
                    manager,
                    incarnation,
                    peer_id,
                    claim.publication_token,
                    0,
                    "tests.Counter.handle",
                ),
                Err(AskError::Unauthorized)
            ));
            assert!(matches!(
                super::super::service::RemoteAdmission::new(
                    manager,
                    incarnation,
                    peer_id,
                    claim.publication_token,
                    offer.grant_id,
                    "tests.Counter.other",
                ),
                Err(AskError::Unauthorized)
            ));
            let admission = super::super::service::RemoteAdmission::new(
                manager,
                incarnation,
                peer_id,
                claim.publication_token,
                offer.grant_id,
                "tests.Counter.handle",
            )
            .unwrap();
            let (readiness, waker) = crate::wake::blocking::Readiness::new();
            // SAFETY: the test transfers one request with a live readiness owner.
            let operation = unsafe {
                crate::actor_call_native::actor_call_new(
                    (*actor).local_pid_id,
                    1,
                    crate::mem::buf_alloc(1),
                    1,
                    drop_unit,
                    0,
                    None,
                    waker.descriptor(),
                    -1,
                    kind,
                    None,
                    None,
                    crate::actor_caller::HewCallerStamp::peer(peer_id.into()),
                    ptr::null(),
                    Some(admission),
                )
            };
            // SAFETY: admission is ready while no actor worker has claimed the message.
            let (request, timer) = unsafe {
                assert_eq!(crate::actor_call_native::hew_actor_call_poll(operation), 0);
                assert!(matches!(
                    (*operation).take_outcome(ptr::null_mut()),
                    crate::request_state::RequestOutcome::Submitted { discarded: false }
                ));
                let queued = (*operation).take_queued();
                crate::actor_call_native::hew_actor_call_free(operation);
                queued
            };
            assert!(timer.is_null());
            assert!(
                !request.terminal(),
                "taking admission must leave dispatch possible"
            );
            if withdraw {
                request.withdraw();
            }
            drop(export);
            drop(claim);
            // SAFETY: this fixture is the sole mailbox consumer; revocation and
            // explicit withdrawal must both prevent the already accepted dispatch.
            unsafe {
                let message = crate::mailbox::hew_mailbox_try_recv((*actor).mailbox.cast());
                assert!(!message.is_null());
                assert!(
                    (*message).reply_channel.is_null(),
                    "mailbox sends owe no completion"
                );
                assert!(!crate::request_state::claim_message(
                    (*message).envelope,
                    ptr::null_mut()
                ));
                assert!(request.terminal());
                assert!(
                    readiness.take_ready(),
                    "withdrawal wakes the queued publication owner"
                );
                crate::mailbox::hew_msg_node_free(message);
            }
            drop(request);
            drop(waker);
            assert_eq!(
                Arc::strong_count(&readiness),
                1,
                "retirement releases its observer"
            );
            // SAFETY: revocation and withdrawal released every queued request owner.
            unsafe {
                crate::actor::hew_actor_free(actor);
                assert_eq!(hew_node_stop(node.as_ptr()), 0);
            }
        }
    }
}

fn await_observation_setup(operation: *mut monitor_link::HewNativeActorObservation) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    // SAFETY: each fixture retains its unique operation until this helper returns.
    while unsafe { monitor_link::hew_native_actor_observe_poll(operation) } == -1 {
        assert!(
            std::time::Instant::now() < deadline,
            "peer setup acknowledgement timed out"
        );
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "one peer fixture owns both nodes, actor allocations, grants and every observer"
)]
fn admitted_lifecycle_observes_peer_cleanup_and_revokes_only_future_setup() {
    use crate::actor_native::{hew_actor_wait_free, hew_actor_wait_new, hew_actor_wait_poll};
    use crate::lifetime::local_handles::{
        hew_actor_handle_identical, hew_actor_handle_release, register_current_imported_actor,
    };
    let _guard = crate::runtime_test_guard();
    let _scheduler = init_real_scheduler();
    let (client, _, server, port) = start_authorized_tcp_pair(371, 372);
    crate::pid::hew_pid_set_local_node(372);
    let target = spawn_remote_test_actor(noop_dispatch);
    inbound_test_codec(noop_dispatch, 0);
    crate::pid::hew_pid_set_local_node(371);
    let watcher = spawn_remote_test_actor(noop_dispatch);
    let address = CString::new(format!("372@127.0.0.1:{port}")).unwrap();
    // SAFETY: both nodes and actor allocations remain live through all observers.
    unsafe {
        connect_with_retry(client.as_ptr(), &address);
        wait_for_handshake(client.as_ptr(), server.as_ptr());
        let client_manager = &*(*client.as_ptr()).conn_mgr;
        let server_manager = &*(*server.as_ptr()).conn_mgr;
        let client_identity = (*client.as_ptr()).auth.node_identity().unwrap();
        let claim =
            connection::claim_send_by_identity(server_manager, client_identity, None).unwrap();
        let location = connection::local_location_for_actor(server_manager, (*target).id).unwrap();
        let (export, offer) = server_manager
            .service_exports
            .export(
                claim.peer_session(),
                1,
                crate::lifetime::live_actors::ActorIncarnation::of(target),
                location,
                vec!["tests.Counter.handle".into()],
            )
            .unwrap();
        drop(claim);
        let descriptor = service_api::ImportedService::from_offer(
            Arc::downgrade(&client_manager.service_exports),
            offer.clone(),
        );
        let token = register_current_imported_actor(descriptor).unwrap();
        let mut same_offer = offer.clone();
        same_offer.grant_id += 100;
        let same = register_current_imported_actor(service_api::ImportedService::from_offer(
            Arc::downgrade(&client_manager.service_exports),
            same_offer,
        ))
        .unwrap();
        let mut other_offer = offer.clone();
        other_offer.target =
            Location::new(location.node(), location.slot() + 1, location.incarnation()).unwrap();
        let other = register_current_imported_actor(service_api::ImportedService::from_offer(
            Arc::downgrade(&client_manager.service_exports),
            other_offer,
        ))
        .unwrap();
        assert_eq!(hew_actor_handle_identical(token, same), 1);
        assert_eq!(hew_actor_handle_identical(token, other), 0);

        let (readiness, waker) = crate::wake::blocking::Readiness::new();
        // stopped is valid outside an actor context and must remain pending while alive.
        let stopped = hew_actor_wait_new(token, waker.descriptor());
        let (detached_ready, detached_waker) = crate::wake::blocking::Readiness::new();
        let detached = hew_actor_wait_new(token, detached_waker.descriptor());
        assert_eq!(hew_actor_wait_poll(stopped), 0);
        assert_eq!(hew_actor_wait_poll(detached), 0);
        hew_actor_wait_free(detached);
        let _ = detached_ready.take_ready();

        let mut context = crate::execution_context::HewExecutionContext {
            actor: watcher,
            actor_id: (*watcher).id,
            ..Default::default()
        };
        let previous = crate::execution_context::set_current_context(&raw mut context);
        let monitor = monitor_link::hew_native_actor_observe_new(token, 0, waker.descriptor());
        await_observation_setup(monitor);
        let mut monitor_id = 0;
        assert_eq!(
            monitor_link::hew_native_actor_observe_take(monitor, &raw mut monitor_id),
            0
        );
        assert_ne!(monitor_id, 0);
        monitor_link::hew_native_actor_observe_free(monitor);

        // A counterfeit grant cannot subscribe even though it has the exact Location.
        let refused = monitor_link::hew_native_actor_observe_new(same, 0, waker.descriptor());
        await_observation_setup(refused);
        assert!(monitor_link::hew_native_actor_observe_take(refused, &raw mut monitor_id) < 0);
        monitor_link::hew_native_actor_observe_free(refused);

        // The reciprocal half completes before the original link is acknowledged.
        let link = monitor_link::hew_native_actor_observe_new(token, 1, waker.descriptor());
        await_observation_setup(link);
        assert_eq!(
            monitor_link::hew_native_actor_observe_take(link, &raw mut monitor_id),
            0
        );
        monitor_link::hew_native_actor_observe_free(link);
        crate::link::hew_native_actor_unlink(token);
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while crate::runtime::rt_current()
            .monitors
            .pending_observation_count()
            > 2
        {
            assert!(
                std::time::Instant::now() < deadline,
                "unlink retained a reciprocal observation"
            );
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        drop(export);
        assert_eq!(
            hew_actor_handle_identical(token, same),
            1,
            "identity survives grant revocation"
        );
        let revoked = monitor_link::hew_native_actor_observe_new(token, 1, waker.descriptor());
        await_observation_setup(revoked);
        assert!(monitor_link::hew_native_actor_observe_take(revoked, &raw mut monitor_id) < 0);
        monitor_link::hew_native_actor_observe_free(revoked);
        let _ = crate::execution_context::set_current_context(previous);
        assert_eq!(hew_actor_wait_poll(stopped), 0);

        // The target's terminal owner publishes only after state cleanup.
        with_current_node(|current| *current = server.as_ptr() as usize);
        (*target).actor_state.store(
            crate::internal::types::HewActorState::Stopped as i32,
            Ordering::Release,
        );
        (*target).terminate_finished.store(true, Ordering::Release);
        crate::actor_native::finish_native_terminal(&*target);
        with_current_node(|current| *current = client.as_ptr() as usize);
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while hew_actor_wait_poll(stopped) == 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "stopped missed the peer terminal notice"
            );
            let _ = readiness.take_ready();
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        assert_eq!(hew_actor_wait_poll(stopped), 1);
        assert!(
            !detached_ready.take_ready(),
            "cancelled stopped observer retained a callback target"
        );
        hew_actor_wait_free(stopped);
        crate::monitor::hew_actor_demonitor(monitor_id);
        hew_actor_handle_release(token);
        hew_actor_handle_release(same);
        hew_actor_handle_release(other);
        crate::actor::hew_actor_free(watcher);
        crate::actor::hew_actor_free(target);
        assert_eq!(hew_node_stop(server.as_ptr()), 0);
        assert_eq!(hew_node_stop(client.as_ptr()), 0);
    }
}
