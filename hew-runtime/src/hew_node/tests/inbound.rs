//! Inbound ask admission and node-stop drain tests.

use super::*;
use crate::hew_node::inbound::{
    handle_inbound_ask, send_reply_envelope, INBOUND_ASK_ERROR_REPLY_HOOK,
    INBOUND_ASK_REPLY_ADMISSION_HOOK, INBOUND_ROUTER_AFTER_INCREMENT_HOOK,
};
use crate::peer_binding::{ClaimState, ConnectionOrigin};
use crate::util::CondvarExt;
use std::sync::LazyLock;

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

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "keep the paused overlap and real reply outcomes together"
)]
fn replies_select_the_request_publication_during_connection_overlap() {
    let _guard = crate::runtime_test_guard();
    let (client, _, server, port) = start_authorized_tcp_pair(355, 356);
    let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
    // SAFETY: both test nodes retain their managers and routing tables.
    let (client_manager, server_manager, client_routes, server_routes) = unsafe {
        connect_with_retry(client.as_ptr(), &address);
        wait_for_handshake(client.as_ptr(), server.as_ptr());
        (
            (*client.as_ptr()).conn_mgr,
            (*server.as_ptr()).conn_mgr,
            &*(*client.as_ptr()).routing_table,
            &*(*server.as_ptr()).routing_table,
        )
    };
    // SAFETY: the test nodes retain their authenticated identity snapshots.
    let (client_identity, server_identity) = unsafe {
        (
            (*client.as_ptr()).auth.node_identity().unwrap(),
            (*server.as_ptr()).auth.node_identity().unwrap(),
        )
    };
    // SAFETY: server remains live for this complete test.
    let old_publication =
        connection::claim_send_by_identity(unsafe { &*server_manager }, client_identity, None)
            .unwrap()
            .publication_token;
    let (client_entered, client_wait) = std::sync::mpsc::channel();
    let (client_release, client_resume) = std::sync::mpsc::channel();
    let (server_entered, server_wait) = std::sync::mpsc::channel();
    let (server_release, server_resume) = std::sync::mpsc::channel();
    client_routes.set_add_route_probe(Some(Arc::new(crate::routing::RouteAddProbe {
        entered: client_entered,
        release: Mutex::new(client_resume),
    })));
    server_routes.set_add_route_probe(Some(Arc::new(crate::routing::RouteAddProbe {
        entered: server_entered,
        release: Mutex::new(server_resume),
    })));
    let client_pointer = client.as_ptr() as usize;
    let reconnect = thread::spawn(move || {
        // SAFETY: the test retains the client until this thread has joined.
        unsafe { hew_node_connect(client_pointer as *mut HewNode, address.as_ptr()) }
    });
    let observed = (|| -> Result<_, &'static str> {
        client_wait
            .recv_timeout(Duration::from_secs(5))
            .map_err(|_| "client publication")?;
        server_wait
            .recv_timeout(Duration::from_secs(5))
            .map_err(|_| "server publication")?;
        // Both current claims are published while both predecessors remain active.
        // SAFETY: the test nodes retain both managers until reconnect has joined.
        let client_claim =
            connection::claim_send_by_identity(unsafe { &*client_manager }, server_identity, None)
                .ok_or("client send claim")?;
        // SAFETY: the server node remains live throughout the paused publication.
        let server_claim =
            connection::claim_send_by_identity(unsafe { &*server_manager }, client_identity, None)
                .ok_or("server send claim")?;
        let shutdown = AtomicBool::new(false);
        let mut pending = Vec::new();
        for kind in 0..3 {
            let (request, reply) = reply_table().register_with_waker(
                ConnectionKey::new(client_manager, client_claim.connection_id()),
                None,
                PendingReplyKind::Ask(client_claim.publication_token),
            );
            // An old request must never migrate onto the successor connection.
            send_reply_envelope(
                355,
                request,
                old_publication,
                b"obsolete",
                server_manager,
                &shutdown,
            );
            match kind {
                0 => crate::hew_node::inbound::send_refusal_reply(
                    355,
                    request,
                    server_claim.publication_token,
                    AskError::Unauthorized,
                    server_manager,
                    &shutdown,
                ),
                1 => crate::hew_node::inbound::send_rejection_reply(
                    355,
                    request,
                    server_claim.publication_token,
                    AskError::WorkerAtCapacity,
                    server_manager,
                    &shutdown,
                ),
                _ => send_reply_envelope(
                    355,
                    request,
                    server_claim.publication_token,
                    b"current",
                    server_manager,
                    &shutdown,
                ),
            }
            pending.push(reply);
        }
        let last = pending.last().unwrap();
        let (last_outcome, _) = last
            .cond
            .wait_timeout_while(
                last.outcome.lock().unwrap(),
                Duration::from_secs(1),
                |outcome| outcome.is_none(),
            )
            .unwrap();
        drop(last_outcome);
        Ok(pending
            .into_iter()
            .map(|reply| {
                reply_table().remove(reply.request_id);
                reply
                    .outcome
                    .lock()
                    .unwrap()
                    .take()
                    .map(|outcome| (outcome.status, outcome.ask_error, outcome.data))
            })
            .collect::<Vec<_>>())
    })();
    // Release both publication threads before assertions or node teardown.
    let _ = client_release.send(());
    let _ = server_release.send(());
    client_routes.set_add_route_probe(None);
    server_routes.set_add_route_probe(None);
    let connected = reconnect.join().expect("reconnect thread");
    assert_eq!(connected, 0);
    assert_eq!(
        observed.expect("paused connection overlap"),
        vec![
            Some((ReplyStatus::Refused, AskError::Unauthorized, vec![])),
            Some((ReplyStatus::Failed, AskError::WorkerAtCapacity, vec![])),
            Some((ReplyStatus::Success, AskError::None, b"current".to_vec())),
        ]
    );
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
            test_location(1, 1).into(),
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

    let connect_addr = CString::new(format!("127.0.0.1:{node2_port}")).unwrap();
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
                test_location(1, crate::pid::hew_pid_serial(actor_id)).into(),
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
                test_location(1, 1).into(),
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
    let connect_addr = CString::new(format!("127.0.0.1:{node2_port}")).unwrap();
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
            test_location(1, crate::pid::hew_pid_serial(actor_id)),
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
    let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
    let mut previous_publication = None;
    for iteration in 0..2 {
        // SAFETY: both authenticated nodes remain live through the reconnect.
        unsafe { connect_with_retry(peer.as_ptr(), &address) };
        // The old connection can remain authenticated while its replacement is
        // being published. Wait for the receiver's new claim before exporting.
        let claim = loop {
            if let Some(claim) = connection::claim_send_by_identity(manager, peer_id, None) {
                if Some(claim.publication_token) != previous_publication {
                    break claim;
                }
            }
            thread::yield_now();
        };
        let publication = claim.publication_token;
        let location = connection::local_location_for_actor(manager, actor_id).unwrap();
        let (_export, offer) = manager
            .service_exports
            .export(
                manager.local_node_id,
                claim.peer_session(),
                42,
                location,
                vec!["tests.Counter.handle".into()],
            )
            .unwrap();
        handle_inbound_ask(
            location,
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
        assert_eq!(
            manager.inbound_ask_active.load(Ordering::Acquire),
            1,
            "admission at iteration {iteration}, publication {publication}, previous {previous_publication:?}"
        );
        for (target, connection, request) in [
            (actor_id + 1, publication, 42),
            (actor_id, previous_publication.unwrap_or(0), 42),
            (actor_id, publication, 43),
        ] {
            manager.inbound_asks.withdraw(
                Location::direct(
                    location.node(),
                    crate::pid::hew_pid_serial(target),
                    location.incarnation(),
                )
                .unwrap(),
                connection,
                request,
            );
            assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 1);
        }
        manager.inbound_asks.withdraw(location, publication, 42);
        assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 0);
        manager.inbound_asks.withdraw(location, publication, 42);
        assert_eq!(manager.inbound_ask_active.load(Ordering::Acquire), 0);
        previous_publication = Some(publication);
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
    let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
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
            location,
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
            manager.local_node_id,
            claim.peer_session(),
            99,
            location,
            vec!["tests.Counter.handle".into()],
        )
        .unwrap();
    assert!(matches!(
        super::super::service::RemoteAdmission::new(
            manager,
            location,
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
            location,
            wrong_session,
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            Location::direct(
                location.node(),
                location.direct_serial().unwrap() + 1,
                location.incarnation()
            )
            .unwrap(),
            claim.peer_session(),
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            location,
            claim.peer_session(),
            0,
            &publication_removed,
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            location,
            claim.peer_session(),
            offer.grant_id,
            &AtomicBool::new(true),
            || ()
        )
        .is_err());
    assert!(manager
        .service_exports
        .claim_observation(
            location,
            claim.peer_session(),
            offer.grant_id,
            &publication_removed,
            || ()
        )
        .is_ok());
    let admission = super::super::service::RemoteAdmission::new(
        manager,
        location,
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
            core::ptr::null(),
            core::ptr::null(),
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
            let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
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
                    manager.local_node_id,
                    claim.peer_session(),
                    99,
                    location,
                    vec!["tests.Counter.handle".into()],
                )
                .unwrap();
            assert!(matches!(
                super::super::service::RemoteAdmission::new(
                    manager,
                    location,
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
                    location,
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
                location,
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
                    core::ptr::null(),
                    core::ptr::null(),
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

fn await_observation_setup(operation: *mut crate::actor_observe::HewNativeActorObservation) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    // SAFETY: each fixture retains its unique operation until this helper returns.
    while unsafe { crate::actor_observe::hew_native_actor_observe_poll(operation) } == -1 {
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
    let address = CString::new(format!("127.0.0.1:{port}")).unwrap();
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
                server_manager.local_node_id,
                claim.peer_session(),
                1,
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
        other_offer.target = Location::direct(
            location.node(),
            location.direct_serial().expect("direct actor fixture") + 1,
            location.incarnation(),
        )
        .unwrap();
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
        let monitor =
            crate::actor_observe::hew_native_actor_observe_new(token, 0, waker.descriptor());
        await_observation_setup(monitor);
        let mut monitor_id = 0;
        assert_eq!(
            crate::actor_observe::hew_native_actor_observe_take(monitor, &raw mut monitor_id),
            0
        );
        assert_ne!(monitor_id, 0);
        crate::actor_observe::hew_native_actor_observe_free(monitor);

        // A counterfeit grant cannot subscribe even though it has the exact Location.
        let refused =
            crate::actor_observe::hew_native_actor_observe_new(same, 0, waker.descriptor());
        await_observation_setup(refused);
        assert!(
            crate::actor_observe::hew_native_actor_observe_take(refused, &raw mut monitor_id) < 0
        );
        crate::actor_observe::hew_native_actor_observe_free(refused);

        // The reciprocal half completes before the original link is acknowledged.
        let link = crate::actor_observe::hew_native_actor_observe_new(token, 1, waker.descriptor());
        await_observation_setup(link);
        assert_eq!(
            crate::actor_observe::hew_native_actor_observe_take(link, &raw mut monitor_id),
            0
        );
        crate::actor_observe::hew_native_actor_observe_free(link);
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
        let revoked =
            crate::actor_observe::hew_native_actor_observe_new(token, 1, waker.descriptor());
        await_observation_setup(revoked);
        assert!(
            crate::actor_observe::hew_native_actor_observe_take(revoked, &raw mut monitor_id) < 0
        );
        crate::actor_observe::hew_native_actor_observe_free(revoked);
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

/// Read one length-prefixed frame, proving the node accepted the connection
/// and entered its handshake.
fn read_handshake_frame(stream: &mut std::net::TcpStream) {
    use std::io::Read;
    let mut header = [0_u8; 4];
    stream
        .read_exact(&mut header)
        .expect("node sends its handshake to an accepted connection");
    let mut frame = vec![0_u8; u32::from_le_bytes(header) as usize];
    stream
        .read_exact(&mut frame)
        .expect("complete handshake frame");
}

#[test]
fn silent_inbound_handshake_does_not_stall_other_admissions() {
    use std::io::Read;
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let budget = crate::transport::CONNECTION_ATTEMPT_TIMEOUT;

    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(331, 332);

    // A dialer whose path died mid-handshake: connected, then silent.
    let mut silent =
        std::net::TcpStream::connect(("127.0.0.1", node2_port)).expect("connect silent peer");
    silent
        .set_read_timeout(Some(budget * 3))
        .expect("set silent peer read timeout");
    read_handshake_frame(&mut silent);
    let silent_since = std::time::Instant::now();

    // One attempt, no retry: it must be admitted while the silent one waits.
    let target = CString::new(format!("127.0.0.1:{node2_port}")).expect("valid connect addr");
    // SAFETY: node1 and the address remain valid for this call.
    let rc = unsafe { hew_node_connect(node1.as_ptr(), target.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "a redial is admitted beside a silent handshake: {:?}",
        {
            let error = crate::hew_last_error();
            // SAFETY: a non-null `hew_last_error` is a live NUL-terminated string.
            (!error.is_null()).then(|| unsafe { CStr::from_ptr(error) }.to_string_lossy())
        }
    );

    // The silent handshake is closed once its attempt budget runs out.
    let mut byte = [0_u8; 1];
    let closed = silent.read(&mut byte);
    assert!(
        matches!(closed, Ok(0))
            || closed
                .as_ref()
                .is_err_and(|error| error.kind() == std::io::ErrorKind::ConnectionReset),
        "silent handshake is closed, not left open: {closed:?}"
    );
    assert!(
        silent_since.elapsed() < budget * 2,
        "silent handshake closed within its budget: {:?}",
        silent_since.elapsed()
    );

    // Stopping a node joins an admission still waiting on a silent peer
    // within the same budget.
    let mut pending = std::net::TcpStream::connect(("127.0.0.1", node2_port))
        .expect("connect second silent peer");
    pending
        .set_read_timeout(Some(budget * 3))
        .expect("set second silent peer read timeout");
    read_handshake_frame(&mut pending);
    let stopping = std::time::Instant::now();
    // SAFETY: both nodes remain owned by this test.
    unsafe {
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
    }
    assert!(
        stopping.elapsed() < budget * 2,
        "node stop is not held by a silent admission: {:?}",
        stopping.elapsed()
    );
    crate::registry::hew_registry_clear();
}

/// How `node` sees `peer`, by authenticated identity.
///
/// # Safety
/// Both nodes are started and live.
unsafe fn peer_view(node: &TestNode, peer: &TestNode) -> connection::PeerConnectionView {
    // SAFETY: the caller keeps both nodes started.
    unsafe {
        connection::peer_connection_view(
            &*(*node.as_ptr()).conn_mgr,
            (*peer.as_ptr())
                .auth
                .node_identity()
                .expect("peer identity"),
        )
    }
}

/// Wait until `node` holds exactly one installed connection to `peer` and
/// that connection, opened from `origin`, owns the published claim.
///
/// # Safety
/// Both nodes are started and live.
unsafe fn await_kept_connection(node: &TestNode, peer: &TestNode, origin: ConnectionOrigin) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        // SAFETY: the caller keeps both nodes started.
        let view = unsafe { peer_view(node, peer) };
        if view.installed == 1 && view.owner == Some((ClaimState::Published, origin)) {
            return;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "never kept only the {origin:?} connection: {view:?}"
        );
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

/// The origin of the connection both ends keep, seen from `node`.
///
/// # Safety
/// Both nodes are started and live.
unsafe fn kept_origin(node: &TestNode, peer: &TestNode) -> ConnectionOrigin {
    // SAFETY: the caller keeps both nodes started.
    let (local, remote) = unsafe {
        (
            (*node.as_ptr())
                .auth
                .node_identity()
                .expect("local identity"),
            (*peer.as_ptr())
                .auth
                .node_identity()
                .expect("peer identity"),
        )
    };
    if local.to_bytes() < remote.to_bytes() {
        ConnectionOrigin::Dialled
    } else {
        ConnectionOrigin::Accepted
    }
}

/// `NODE_SUSPECT` events each node's cluster raised, by node address.
static SUSPECTS: LazyLock<Mutex<Vec<usize>>> = LazyLock::new(Mutex::default);

/// Record a suspicion, then run the node's own membership callback.
extern "C" fn recording_membership_callback(
    route_slot: u16,
    event: u8,
    user_data: *mut std::ffi::c_void,
) {
    if event == cluster::HEW_MEMBERSHIP_EVENT_NODE_SUSPECT {
        SUSPECTS.lock_or_recover().push(user_data as usize);
    }
    crate::hew_node::node_membership_callback(route_slot, event, user_data);
}

/// Count the suspicions `node`'s cluster raises from here on.
///
/// # Safety
/// The node is started and live.
unsafe fn record_suspects(node: &TestNode) {
    SUSPECTS
        .lock_or_recover()
        .retain(|&recorded| recorded != node.as_ptr() as usize);
    // SAFETY: the started node owns its cluster; the callback forwards to the
    // node's own with the node as its user data, as start installed it.
    unsafe {
        cluster::hew_cluster_set_membership_callback(
            (*node.as_ptr()).cluster,
            recording_membership_callback,
            node.as_ptr().cast(),
        );
    }
}

fn suspects(node: &TestNode) -> usize {
    SUSPECTS
        .lock_or_recover()
        .iter()
        .filter(|&&recorded| recorded == node.as_ptr() as usize)
        .count()
}

/// Dial `port` from `node` on a thread released by `barrier`; the thread
/// reports the result and sets `settled` once the dial returns.
fn spawn_dial(
    node: &TestNode,
    port: u16,
    barrier: &Arc<std::sync::Barrier>,
    settled: Option<Arc<(Mutex<CrossingProgress>, std::sync::Condvar)>>,
) -> std::thread::JoinHandle<(c_int, Option<String>)> {
    let node = node.as_ptr() as usize;
    let barrier = Arc::clone(barrier);
    std::thread::spawn(move || {
        let target = CString::new(format!("127.0.0.1:{port}")).expect("valid address");
        barrier.wait();
        // SAFETY: the test joins this dial before stopping the node.
        let rc = unsafe { hew_node_connect(node as *mut HewNode, target.as_ptr()) };
        let error = crate::hew_last_error();
        let error = (!error.is_null()).then(|| {
            // SAFETY: a non-null `hew_last_error` is a live NUL-terminated string.
            unsafe { CStr::from_ptr(error) }
                .to_string_lossy()
                .into_owned()
        });
        if let Some(progress) = settled {
            progress.0.lock_or_recover().higher_dial_settled = true;
            progress.1.notify_all();
        }
        (rc, error)
    })
}

/// How far a scripted crossing has got.
#[derive(Debug, Default)]
struct CrossingProgress {
    /// The higher node's own dial has returned.
    higher_dial_settled: bool,
    /// The lower node's dial is waiting for the higher node's commit.
    lower_awaiting_commit: bool,
    /// A scripted wait gave up instead of seeing its condition.
    timed_out: bool,
}

type Progress = Arc<(Mutex<CrossingProgress>, std::sync::Condvar)>;

/// Block until `ready` holds, re-checking it as the progress changes and
/// every millisecond for conditions read off a node; records a timeout.
fn await_progress(progress: &Progress, ready: impl Fn(&CrossingProgress) -> bool) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let (lock, condvar) = &**progress;
    let mut state = lock.lock_or_recover();
    while !ready(&state) {
        let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) else {
            state.timed_out = true;
            return;
        };
        state = condvar
            .wait_timeout_or_recover(state, left.min(std::time::Duration::from_millis(1)))
            .0;
    }
}

/// What one scripted crossing observed.
#[derive(Debug)]
struct Crossing {
    lower_dial: (c_int, Option<String>),
    higher_dial: (c_int, Option<String>),
    progress: CrossingProgress,
    lower_suspects: usize,
    higher_suspects: usize,
}

/// Two nodes dial each other at once, in the order that lost the higher
/// node's member on the old admission. The higher node's dial publishes
/// first; the lower node's dial authenticates only after that; the higher
/// node's accept of the lower dial reserves only once the lower node is
/// waiting for its commit, or, between ends that settle on their own, once
/// the lower node has closed the higher node's dial. Neither wait relies on
/// the scheduler.
fn scripted_crossing(id_a: u16, id_b: u16) -> Crossing {
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(id_a, id_b);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let ((lower, lower_port), (higher, higher_port)) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                ((&a, port_a), (&b, port_b))
            } else {
                ((&b, port_b), (&a, port_a))
            };
        let lower_mgr = (*lower.as_ptr()).conn_mgr.cast_const();
        let higher_mgr = (*higher.as_ptr()).conn_mgr.cast_const();
        record_suspects(lower);
        record_suspects(higher);

        let progress: Progress = Arc::default();
        let lower_hook: connection::admission_script::Hook = {
            let progress = Arc::clone(&progress);
            Arc::new(move |point, origin| {
                use connection::admission_script::Point;
                match (point, origin) {
                    (Point::Authenticated, ConnectionOrigin::Dialled) => {
                        await_progress(&progress, |state| state.higher_dial_settled);
                    }
                    (Point::AwaitingCommit, ConnectionOrigin::Dialled) => {
                        progress.0.lock_or_recover().lower_awaiting_commit = true;
                        progress.1.notify_all();
                    }
                    _ => {}
                }
            })
        };
        let higher_hook: connection::admission_script::Hook = {
            let progress = Arc::clone(&progress);
            let (higher_node, lower_node) = (higher.as_ptr() as usize, lower.as_ptr() as usize);
            Arc::new(move |point, origin| {
                if point == connection::admission_script::Point::Authenticated
                    && origin == ConnectionOrigin::Accepted
                {
                    await_progress(&progress, |state| {
                        state.lower_awaiting_commit
                            || (state.higher_dial_settled
                                // The nodes outlive their admissions.
                                && {
                                    connection::peer_connection_view(
                                        &*(*(higher_node as *mut HewNode)).conn_mgr,
                                        (*(lower_node as *mut HewNode))
                                            .auth
                                            .node_identity()
                                            .expect("lower identity"),
                                    )
                                }
                                .installed
                                    == 0)
                    });
                }
            })
        };
        let script = connection::admission_script::install(&[
            (lower_mgr, lower_hook),
            (higher_mgr, higher_hook),
        ]);

        let barrier = Arc::new(std::sync::Barrier::new(2));
        let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
        let higher_dial = spawn_dial(higher, lower_port, &barrier, Some(Arc::clone(&progress)));
        let lower_dial = lower_dial.join().expect("lower dial");
        let higher_dial = higher_dial.join().expect("higher dial");
        await_kept_connection(lower, higher, ConnectionOrigin::Dialled);
        await_kept_connection(higher, lower, ConnectionOrigin::Accepted);
        drop(script);
        let crossing = Crossing {
            lower_dial,
            higher_dial,
            progress: std::mem::take(&mut *progress.0.lock_or_recover()),
            lower_suspects: suspects(lower),
            higher_suspects: suspects(higher),
        };
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
        crossing
    }
}

/// Two nodes that dial each other at once switch to the lower node's dial in
/// one order: the higher node commits it before the lower node closes the
/// higher node's dial. Neither node loses the other's member for a moment,
/// and both dials report the peer connected.
#[test]
fn crossing_dials_switch_to_the_winner_without_suspicion() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    for round in 0..4 {
        let crossing = scripted_crossing(343, 344);
        assert!(
            crossing.progress.lower_awaiting_commit && !crossing.progress.timed_out,
            "round {round}: the script did not run: {crossing:?}"
        );
        assert_eq!(
            (crossing.lower_dial.0, crossing.higher_dial.0),
            (0, 0),
            "round {round}: both dials connect: {crossing:?}"
        );
        assert_eq!(
            (crossing.lower_suspects, crossing.higher_suspects),
            (0, 0),
            "round {round}: no member is suspected: {crossing:?}"
        );
    }
    crate::registry::hew_registry_clear();
}

/// A node that advertises the responder-commit step still pairs with one that
/// does not, whichever of the two is the lower node: crossing dials whose paths
/// differ in latency leave both ends sharing one connection. The node with the
/// step follows the other's choice. A dial that outlasts its attempt budget is
/// not covered: a node without the step can publish it and close the pair's
/// other connection after the dialler has given it up.
#[test]
fn crossing_dials_converge_with_a_node_without_responder_commit() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    for withhold_lower in [true, false] {
        for delay_ms in [0, 300] {
            for slow_higher in [true, false] {
                skewed_crossing(347, 348, delay_ms, false, slow_higher, Some(withhold_lower));
            }
        }
    }
    crate::registry::hew_registry_clear();
}

/// The case that divides a pair keeping the newer connection: each node
/// admits its own dial before the peer's, so each end's newer connection is
/// the one the other end superseded. The node with the responder-commit step
/// follows the other's choice, so the pair still ends sharing one connection.
#[test]
fn opposite_admission_orders_converge_with_a_node_without_responder_commit() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let fences = fence_timeouts();
    for lower_lacks in [true, false] {
        let (a, port_a, b, port_b) = start_authorized_tcp_pair(359, 360);
        // SAFETY: both nodes stay started until the stops below.
        unsafe {
            let ((lower, lower_port), (higher, higher_port)) =
                if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                    ((&a, port_a), (&b, port_b))
                } else {
                    ((&b, port_b), (&a, port_a))
                };
            let lacking = if lower_lacks { lower } else { higher };
            let _withheld =
                connection::withheld_commit::withhold((*lacking.as_ptr()).conn_mgr.cast_const());
            let script = connection::admission_script::install(&[
                (
                    (*lower.as_ptr()).conn_mgr.cast_const(),
                    own_dial_first(lower, higher),
                ),
                (
                    (*higher.as_ptr()).conn_mgr.cast_const(),
                    own_dial_first(higher, lower),
                ),
            ]);
            let barrier = Arc::new(std::sync::Barrier::new(2));
            let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
            let higher_dial = spawn_dial(higher, lower_port, &barrier, None);
            let dials = (
                lower_dial.join().expect("lower dial"),
                higher_dial.join().expect("higher dial"),
            );
            drop(script);
            assert_eq!(fence_timeouts(), fences, "a script fence timed out");
            await_one_shared_connection(
                lower,
                higher,
                &format!("lower lacks the step: {lower_lacks}; dials {dials:?}"),
            );
            assert_eq!(hew_node_stop(b.as_ptr()), 0);
            assert_eq!(hew_node_stop(a.as_ptr()), 0);
        }
    }
    crate::registry::hew_registry_clear();
}

/// Each node admits the peer's dial only once its own dial has published.
fn own_dial_first(node: &TestNode, peer: &TestNode) -> connection::admission_script::Hook {
    let (node, peer) = (node.as_ptr() as usize, peer.as_ptr() as usize);
    Arc::new(move |point, origin| {
        if point == connection::admission_script::Point::Authenticated
            && origin == ConnectionOrigin::Accepted
        {
            // SAFETY: the nodes outlive their admissions.
            unsafe { await_owner(node, peer, ConnectionOrigin::Dialled) };
        }
    })
}

/// Script fences that gave up waiting, so a test can tell a script that did
/// not run from the behaviour it checks.
static FENCE_TIMEOUTS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

fn fence_timeouts() -> usize {
    FENCE_TIMEOUTS.load(std::sync::atomic::Ordering::Acquire)
}

/// Wait until `node` holds `peer`'s published claim on a connection of
/// `origin`; a wait that gives up is counted in [`FENCE_TIMEOUTS`].
///
/// # Safety
/// Both pointers name started nodes.
unsafe fn await_owner(node: usize, peer: usize, origin: ConnectionOrigin) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    // SAFETY: the caller keeps both nodes started.
    while unsafe {
        connection::peer_connection_view(
            &*(*(node as *mut HewNode)).conn_mgr,
            (*(peer as *mut HewNode))
                .auth
                .node_identity()
                .expect("peer identity"),
        )
    }
    .owner
        != Some((ClaimState::Published, origin))
    {
        if std::time::Instant::now() >= deadline {
            FENCE_TIMEOUTS.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}

/// A node without the responder-commit step can send on a connection before
/// it supersedes that one. The follower does not take such a frame for the
/// peer's choice: the pair still ends sharing one connection.
#[test]
fn follower_waits_for_a_close_before_settling_a_crossing() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let fences = fence_timeouts();
    for lower_lacks in [true, false] {
        let (a, port_a, b, port_b) = start_authorized_tcp_pair(361, 362);
        // SAFETY: both nodes stay started until the stops below.
        unsafe {
            let ((lower, lower_port), (higher, higher_port)) =
                if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                    ((&a, port_a), (&b, port_b))
                } else {
                    ((&b, port_b), (&a, port_a))
                };
            let (old, follower) = if lower_lacks {
                (lower, higher)
            } else {
                (higher, lower)
            };
            let _withheld =
                connection::withheld_commit::withhold((*old.as_ptr()).conn_mgr.cast_const());
            // The old node publishes its own dial, waits until the follower has
            // superseded its own dial with it, sends on it, and only then
            // admits the follower's dial.
            let sent = Arc::new(Mutex::new(None));
            let old_hook: connection::admission_script::Hook = {
                let (old, follower) = (old.as_ptr() as usize, follower.as_ptr() as usize);
                let sent = Arc::clone(&sent);
                Arc::new(move |point, origin| {
                    if point != connection::admission_script::Point::Authenticated
                        || origin != ConnectionOrigin::Accepted
                        || sent.lock_or_recover().is_some()
                    {
                        return;
                    }
                    // The nodes outlive their admissions.
                    await_owner(old, follower, ConnectionOrigin::Dialled);
                    await_owner(follower, old, ConnectionOrigin::Accepted);
                    let follower_identity = (*(follower as *mut HewNode))
                        .auth
                        .node_identity()
                        .expect("follower identity");
                    let claim = connection::claim_send_by_identity(
                        &*(*(old as *mut HewNode)).conn_mgr,
                        follower_identity,
                        None,
                    );
                    *sent.lock_or_recover() = Some(claim.map(|claim| claim.send(b"x")));
                })
            };
            let script = connection::admission_script::install(&[
                ((*old.as_ptr()).conn_mgr.cast_const(), old_hook),
                (
                    (*follower.as_ptr()).conn_mgr.cast_const(),
                    own_dial_first(follower, old),
                ),
            ]);
            let barrier = Arc::new(std::sync::Barrier::new(2));
            let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
            let higher_dial = spawn_dial(higher, lower_port, &barrier, None);
            let dials = (
                lower_dial.join().expect("lower dial"),
                higher_dial.join().expect("higher dial"),
            );
            // The follower's dial returns once the follower publishes it; the
            // old node admits it afterwards, once it has sent.
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while sent.lock_or_recover().is_none() && std::time::Instant::now() < deadline {
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            drop(script);
            assert_eq!(fence_timeouts(), fences, "a script fence timed out");
            let sent = *sent.lock_or_recover();
            assert_eq!(
                sent,
                Some(Some(0)),
                "lower lacks the step: {lower_lacks}: the old node sent on its dial; \
                 dials {dials:?}"
            );
            await_one_shared_connection(
                lower,
                higher,
                &format!("lower lacks the step: {lower_lacks}; dials {dials:?}"),
            );
            assert_eq!(hew_node_stop(b.as_ptr()), 0);
            assert_eq!(hew_node_stop(a.as_ptr()), 0);
        }
    }
    crate::registry::hew_registry_clear();
}

/// A held connection that closes while it is being published again withdraws
/// its reservation, so the peer can connect afresh.
#[test]
fn held_connection_closed_during_its_restore_leaves_the_peer_admissible() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let fences = fence_timeouts();
    for lower_lacks in [true, false] {
        let (a, port_a, b, port_b) = start_authorized_tcp_pair(365, 366);
        // SAFETY: both nodes stay started until the stops below.
        unsafe {
            let ((lower, lower_port), (higher, higher_port)) =
                if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                    ((&a, port_a), (&b, port_b))
                } else {
                    ((&b, port_b), (&a, port_a))
                };
            let (old, old_port, follower) = if lower_lacks {
                (lower, lower_port, higher)
            } else {
                (higher, higher_port, lower)
            };
            let _withheld =
                connection::withheld_commit::withhold((*old.as_ptr()).conn_mgr.cast_const());
            let restoring = Arc::new(Mutex::new(None));
            let follower_hook: connection::admission_script::Hook = {
                let own_first = own_dial_first(follower, old);
                let restoring = Arc::clone(&restoring);
                let mgr = (*follower.as_ptr()).conn_mgr as usize;
                Arc::new(move |point, origin| {
                    if let connection::admission_script::Point::RestoringLoser(loser) = point {
                        // The peer drops the held connection as well.
                        // SAFETY: the manager outlives its admissions.
                        let removed = connection::hew_connmgr_remove(
                            mgr as *mut connection::HewConnMgr,
                            loser,
                        );
                        *restoring.lock_or_recover() = Some(removed);
                    } else {
                        own_first(point, origin);
                    }
                })
            };
            let script = connection::admission_script::install(&[
                (
                    (*old.as_ptr()).conn_mgr.cast_const(),
                    own_dial_first(old, follower),
                ),
                ((*follower.as_ptr()).conn_mgr.cast_const(), follower_hook),
            ]);
            let barrier = Arc::new(std::sync::Barrier::new(2));
            let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
            let higher_dial = spawn_dial(higher, lower_port, &barrier, None);
            let dials = (
                lower_dial.join().expect("lower dial"),
                higher_dial.join().expect("higher dial"),
            );
            // Both connections close at both ends; the follower ends with no
            // live claim, and its member is not left alive on a connection that
            // no longer exists: the holder's publication was reported lost.
            let old_slot: u16 = if std::ptr::eq(old.as_ptr(), a.as_ptr()) {
                365
            } else {
                366
            };
            let cluster = &*(*follower.as_ptr()).cluster;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while !(restoring.lock_or_recover().is_some()
                && peer_view(follower, old).installed == 0
                && peer_view(old, follower).installed == 0
                && peer_view(follower, old)
                    .owner
                    .is_none_or(|(state, _)| state == ClaimState::Retired)
                && cluster.member_owner(old_slot).1.is_none()
                && cluster.member_state(old_slot) != crate::cluster::MEMBER_ALIVE)
            {
                assert!(
                    std::time::Instant::now() < deadline,
                    "lower lacks the step: {lower_lacks}; dials {dials:?}: the restore \
                     did not settle: removal {:?}, follower {:?}, old {:?}, member owner \
                     {:?}, state {}",
                    *restoring.lock_or_recover(),
                    peer_view(follower, old),
                    peer_view(old, follower),
                    cluster.member_owner(old_slot),
                    cluster.member_state(old_slot)
                );
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            drop(script);
            assert_eq!(fence_timeouts(), fences, "a script fence timed out");
            let redial = spawn_dial(
                follower,
                old_port,
                &Arc::new(std::sync::Barrier::new(1)),
                None,
            )
            .join()
            .expect("fresh dial");
            assert_eq!(
                redial.0,
                0,
                "lower lacks the step: {lower_lacks}: the peer is admissible again: {redial:?}; \
                 follower {:?}, old {:?}",
                peer_view(follower, old),
                peer_view(old, follower)
            );
            await_one_shared_connection(lower, higher, "after the fresh dial");
            assert_eq!(hew_node_stop(b.as_ptr()), 0);
            assert_eq!(hew_node_stop(a.as_ptr()), 0);
        }
    }
    crate::registry::hew_registry_clear();
}

/// Disconnecting the peer locally closes the connection the follower holds
/// too: a held connection never outlives its holder, nor is it published
/// again by a close the peer did not make.
#[test]
fn local_disconnect_closes_the_held_connection() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let fences = fence_timeouts();
    for lower_lacks in [true, false] {
        let (a, port_a, b, port_b) = start_authorized_tcp_pair(367, 368);
        // SAFETY: both nodes stay started until the stops below.
        unsafe {
            let ((lower, lower_port), (higher, higher_port)) =
                if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                    ((&a, port_a), (&b, port_b))
                } else {
                    ((&b, port_b), (&a, port_a))
                };
            let (old, follower) = if lower_lacks {
                (lower, higher)
            } else {
                (higher, lower)
            };
            let _withheld =
                connection::withheld_commit::withhold((*old.as_ptr()).conn_mgr.cast_const());
            // The old node admits the follower's dial only once released, so
            // the follower holds its own dial meanwhile.
            let (release, released) = std::sync::mpsc::channel::<()>();
            let released = Mutex::new(released);
            let old_hook: connection::admission_script::Hook = Arc::new(move |point, origin| {
                if point == connection::admission_script::Point::Authenticated
                    && origin == ConnectionOrigin::Accepted
                {
                    let _ = released
                        .lock_or_recover()
                        .recv_timeout(std::time::Duration::from_secs(5));
                }
            });
            let script = connection::admission_script::install(&[
                ((*old.as_ptr()).conn_mgr.cast_const(), old_hook),
                (
                    (*follower.as_ptr()).conn_mgr.cast_const(),
                    own_dial_first(follower, old),
                ),
            ]);
            let barrier = Arc::new(std::sync::Barrier::new(2));
            let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
            let higher_dial = spawn_dial(higher, lower_port, &barrier, None);
            let old_identity = (*old.as_ptr()).auth.node_identity().expect("old identity");
            let follower_mgr = (*follower.as_ptr()).conn_mgr;
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            while connection::held_loser_for_test(&*follower_mgr, old_identity).is_none() {
                assert!(
                    std::time::Instant::now() < deadline,
                    "lower lacks the step: {lower_lacks}: the follower never held its dial"
                );
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            let owner = connection::claim_send_by_identity(&*follower_mgr, old_identity, None)
                .expect("the follower's claim is published")
                .connection_id();
            assert_eq!(connection::hew_connmgr_remove(follower_mgr, owner), 0);
            let _ = release.send(());
            let dials = (
                lower_dial.join().expect("lower dial"),
                higher_dial.join().expect("higher dial"),
            );
            drop(script);
            assert_eq!(fence_timeouts(), fences, "a script fence timed out");
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            loop {
                let view = peer_view(follower, old);
                if view.installed == 0 {
                    assert_ne!(
                        view.owner.map(|(state, _)| state),
                        Some(ClaimState::Published),
                        "lower lacks the step: {lower_lacks}; dials {dials:?}: the held \
                         connection was published again"
                    );
                    break;
                }
                assert!(
                    std::time::Instant::now() < deadline,
                    "lower lacks the step: {lower_lacks}; dials {dials:?}: the held \
                     connection outlived its holder: {view:?}"
                );
                std::thread::sleep(std::time::Duration::from_millis(1));
            }
            assert_eq!(hew_node_stop(b.as_ptr()), 0);
            assert_eq!(hew_node_stop(a.as_ptr()), 0);
        }
    }
    crate::registry::hew_registry_clear();
}

/// A dial that yields to the peer's crossing dial is closed, and its transport
/// id can name another connection before reconnect is armed. Reconnect goes to
/// the connection the dial yielded to, never to the id's next holder.
#[test]
fn yielded_dial_arms_reconnect_on_the_kept_connection() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(349, 350);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let ((lower, lower_port), (higher, higher_port)) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                ((&a, port_a), (&b, port_b))
            } else {
                ((&b, port_b), (&a, port_a))
            };
        let higher_mgr = (*higher.as_ptr()).conn_mgr;
        let lower_identity = (*lower.as_ptr())
            .auth
            .node_identity()
            .expect("lower identity");
        let reissued = Arc::new(Mutex::new(None));
        let higher_hook: connection::admission_script::Hook = {
            let reissued = Arc::clone(&reissued);
            let mgr = higher_mgr as usize;
            Arc::new(move |point, origin| {
                use connection::admission_script::Point;
                // The manager outlives its admissions.
                let mgr = &*(mgr as *const connection::HewConnMgr);
                match (point, origin) {
                    // Hold the higher node's own dial until the lower node's
                    // dial has published here, so the higher dial yields.
                    (Point::Authenticated, ConnectionOrigin::Dialled) => {
                        let deadline =
                            std::time::Instant::now() + std::time::Duration::from_secs(5);
                        while connection::peer_connection_view(mgr, lower_identity).owner
                            != Some((ClaimState::Published, ConnectionOrigin::Accepted))
                            && std::time::Instant::now() < deadline
                        {
                            std::thread::sleep(std::time::Duration::from_millis(1));
                        }
                    }
                    (Point::Admitted(conn_id), ConnectionOrigin::Dialled) => {
                        connection::install_stand_in_for_test(mgr, conn_id);
                        *reissued.lock_or_recover() = Some(conn_id);
                    }
                    _ => {}
                }
            })
        };
        let script =
            connection::admission_script::install(&[(higher_mgr.cast_const(), higher_hook)]);
        let barrier = Arc::new(std::sync::Barrier::new(2));
        let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
        let higher_dial = spawn_dial(higher, lower_port, &barrier, None);
        assert_eq!(lower_dial.join().expect("lower dial").0, 0);
        assert_eq!(higher_dial.join().expect("higher dial").0, 0);
        drop(script);
        let reissued = reissued
            .lock_or_recover()
            .expect("the higher dial was admitted");
        let lower_address = format!("127.0.0.1:{lower_port}");
        let targets = connection::reconnect_targets_for_test(&*higher_mgr);
        let stand_in = targets
            .iter()
            .find(|(conn_id, token, _)| *conn_id == reissued && *token == 0)
            .expect("stand-in installed");
        assert_eq!(
            stand_in.2, None,
            "the reissued id is not supervised: {targets:?}"
        );
        let kept = connection::peer_connection_view(&*higher_mgr, lower_identity);
        assert_eq!(
            kept.owner,
            Some((ClaimState::Published, ConnectionOrigin::Accepted)),
            "the higher dial yielded to the lower one"
        );
        assert!(
            targets.iter().any(
                |(conn_id, token, target)| (*conn_id, *token) != (reissued, 0)
                    && target.as_deref() == Some(lower_address.as_str())
            ),
            "the kept connection is supervised: {targets:?}"
        );
        assert_eq!(connection::hew_connmgr_remove(higher_mgr, reissued), 0);
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// The tie-break covers only dials in flight together. A same-session redial
/// that begins after the lower node's connection published still replaces
/// it, as it must replace that connection if it were half-open, and neither
/// node suspects the other while it does.
#[test]
fn later_redial_replaces_the_lower_node_dial() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(345, 346);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let (lower, lower_port, higher, higher_port) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                (&a, port_a, &b, port_b)
            } else {
                (&b, port_b, &a, port_a)
            };
        let higher_address = CString::new(format!("127.0.0.1:{higher_port}")).unwrap();
        assert_eq!(hew_node_connect(lower.as_ptr(), higher_address.as_ptr()), 0);
        await_kept_connection(lower, higher, ConnectionOrigin::Dialled);
        await_kept_connection(higher, lower, ConnectionOrigin::Accepted);
        record_suspects(lower);
        record_suspects(higher);

        let lower_address = CString::new(format!("127.0.0.1:{lower_port}")).unwrap();
        assert_eq!(hew_node_connect(higher.as_ptr(), lower_address.as_ptr()), 0);
        await_kept_connection(lower, higher, ConnectionOrigin::Accepted);
        await_kept_connection(higher, lower, ConnectionOrigin::Dialled);
        assert_eq!(
            (suspects(lower), suspects(higher)),
            (0, 0),
            "the redial replaces the connection without suspicion"
        );
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Forward one accepted connection to `upstream_port` after `delay`.
fn delayed_proxy(upstream_port: u16, delay: std::time::Duration) -> u16 {
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("proxy listener");
    let port = listener.local_addr().expect("proxy address").port();
    std::thread::spawn(move || {
        let Ok((client, _)) = listener.accept() else {
            return;
        };
        std::thread::sleep(delay);
        let Ok(upstream) = std::net::TcpStream::connect(("127.0.0.1", upstream_port)) else {
            return;
        };
        let pipe = |mut from: std::net::TcpStream, mut to: std::net::TcpStream| {
            std::thread::spawn(move || {
                let _ = std::io::copy(&mut from, &mut to);
                let _ = to.shutdown(std::net::Shutdown::Both);
                let _ = from.shutdown(std::net::Shutdown::Both);
            });
        };
        let (client_copy, upstream_copy) = (
            client.try_clone().expect("clone client"),
            upstream.try_clone().expect("clone upstream"),
        );
        pipe(client, upstream);
        pipe(upstream_copy, client_copy);
    });
    port
}

/// Crossing dials whose paths differ in latency still settle on one
/// connection at both ends, with the same commit order. One node's dial passes
/// through a proxy that holds it for the given delay; the other node dials
/// directly 100 ms later, and with `redial` the slow node also dials directly
/// 50 ms after that. Each node sees the admissions overlap differently on its
/// own clock, so only one end may decide. A dial whose path outlasts its
/// attempt budget may report failure; the pair must still keep a connection.
#[test]
fn crossing_dials_with_skewed_latency_keep_one_connection() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    for (delay_ms, redial) in [
        (0, false),
        (300, false),
        (600, false),
        (1500, false),
        (1500, true),
    ] {
        for slow_higher in [true, false] {
            skewed_crossing(351, 352, delay_ms, redial, slow_higher, None);
        }
    }
    crate::registry::hew_registry_clear();
}

/// One skewed crossing: the slow node's dial passes through a proxy that holds
/// it for `delay_ms`; the other node dials directly 100 ms later, and with
/// `redial` the slow node also dials directly 50 ms after that. With
/// `withhold_lower`, the lower (`Some(true)`) or higher (`Some(false)`) node
/// does not advertise the responder-commit step. Asserts the pair ends sharing
/// one connection.
fn skewed_crossing(
    id_a: u16,
    id_b: u16,
    delay_ms: u64,
    redial: bool,
    slow_higher: bool,
    withhold_lower: Option<bool>,
) {
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(id_a, id_b);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let ((lower, lower_port), (higher, higher_port)) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                ((&a, port_a), (&b, port_b))
            } else {
                ((&b, port_b), (&a, port_a))
            };
        let _withheld = withhold_lower.map(|lower_lacks| {
            let node = if lower_lacks { lower } else { higher };
            connection::withheld_commit::withhold((*node.as_ptr()).conn_mgr.cast_const())
        });
        let delay = std::time::Duration::from_millis(delay_ms);
        let (slow, slow_target, slow_direct, fast, fast_target) = if slow_higher {
            (
                higher,
                delayed_proxy(lower_port, delay),
                lower_port,
                lower,
                higher_port,
            )
        } else {
            (
                lower,
                delayed_proxy(higher_port, delay),
                higher_port,
                higher,
                lower_port,
            )
        };
        let start = Arc::new(std::sync::Barrier::new(1));
        let slow_dial = spawn_dial(slow, slow_target, &start, None);
        std::thread::sleep(std::time::Duration::from_millis(100));
        let fast_dial = spawn_dial(fast, fast_target, &start, None);
        let redial = redial.then(|| {
            std::thread::sleep(std::time::Duration::from_millis(50));
            spawn_dial(slow, slow_direct, &start, None)
        });
        let dials = (
            slow_dial.join().expect("slow dial"),
            fast_dial.join().expect("fast dial"),
            redial.map(|dial| dial.join().expect("redial")),
        );
        let case = format!(
            "delay {delay_ms} ms on the {} dial, {withhold_lower:?} lacks the step: dials {dials:?}",
            if slow_higher { "higher" } else { "lower" }
        );
        await_one_shared_connection(lower, higher, &case);
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
}

/// The responder touches nothing until the initiator echoes its commit. A
/// redial whose initiator gives up after the commit leaves both ends on the
/// connection they already shared.
#[test]
fn initiator_that_gives_up_after_the_commit_keeps_the_shared_connection() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(357, 358);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let (lower, higher, higher_port) = if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
            (&a, &b, port_b)
        } else {
            (&b, &a, port_a)
        };
        let higher_address = CString::new(format!("127.0.0.1:{higher_port}")).unwrap();
        assert_eq!(hew_node_connect(lower.as_ptr(), higher_address.as_ptr()), 0);
        await_one_shared_connection(lower, higher, "first dial");
        let order = shared_commit_order(lower, higher);
        // Hold the redial past its attempt budget once it is waiting for the
        // commit, so it gives up after the responder has committed.
        let lower_hook: connection::admission_script::Hook = Arc::new(|point, origin| {
            if point == connection::admission_script::Point::AwaitingCommit
                && origin == ConnectionOrigin::Dialled
            {
                std::thread::sleep(
                    crate::transport::CONNECTION_ATTEMPT_TIMEOUT
                        + std::time::Duration::from_millis(100),
                );
            }
        });
        let script = connection::admission_script::install(&[(
            (*lower.as_ptr()).conn_mgr.cast_const(),
            lower_hook,
        )]);
        assert_ne!(
            hew_node_connect(lower.as_ptr(), higher_address.as_ptr()),
            0,
            "the redial gave up"
        );
        drop(script);
        await_one_shared_connection(lower, higher, "after the abandoned redial");
        assert_eq!(
            shared_commit_order(lower, higher),
            order,
            "the pair kept the connection it shared"
        );
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// An echo that reaches the responder after its attempt budget finds the
/// redial closed. The initiator, which had already moved to the redial, holds
/// the connection the pair shared and returns to it.
#[test]
fn late_echo_returns_both_ends_to_the_shared_connection() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(355, 356);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let (lower, higher, higher_port) = if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
            (&a, &b, port_b)
        } else {
            (&b, &a, port_a)
        };
        let higher_address = CString::new(format!("127.0.0.1:{higher_port}")).unwrap();
        assert_eq!(hew_node_connect(lower.as_ptr(), higher_address.as_ptr()), 0);
        await_one_shared_connection(lower, higher, "first dial");
        let order = shared_commit_order(lower, higher);
        let lower_hook: connection::admission_script::Hook = Arc::new(|point, origin| {
            if point == connection::admission_script::Point::Echoing
                && origin == ConnectionOrigin::Dialled
            {
                std::thread::sleep(
                    crate::transport::CONNECTION_ATTEMPT_TIMEOUT
                        + std::time::Duration::from_millis(100),
                );
            }
        });
        let script = connection::admission_script::install(&[(
            (*lower.as_ptr()).conn_mgr.cast_const(),
            lower_hook,
        )]);
        let rc = hew_node_connect(lower.as_ptr(), higher_address.as_ptr());
        drop(script);
        await_one_shared_connection(lower, higher, &format!("late echo, redial rc {rc}"));
        assert_eq!(
            shared_commit_order(lower, higher),
            order,
            "the pair returned to the connection it shared"
        );
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// While a redial settles, the connection the pair shares stays usable at
/// both ends: the responder can still send on it, and the initiator, which
/// has already moved to the redial, serves the peer on it and replies there.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "keep the held window and the reply that crosses it together"
)]
fn held_connection_serves_the_peer_until_the_switch() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(363, 364);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let (lower, higher, higher_port) = if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
            (&a, &b, port_b)
        } else {
            (&b, &a, port_a)
        };
        let higher_address = CString::new(format!("127.0.0.1:{higher_port}")).unwrap();
        assert_eq!(hew_node_connect(lower.as_ptr(), higher_address.as_ptr()), 0);
        await_one_shared_connection(lower, higher, "first dial");
        let (lower_mgr, higher_mgr) = ((*lower.as_ptr()).conn_mgr, (*higher.as_ptr()).conn_mgr);
        let lower_identity = (*lower.as_ptr())
            .auth
            .node_identity()
            .expect("lower identity");
        let higher_identity = (*higher.as_ptr())
            .auth
            .node_identity()
            .expect("higher identity");

        // Hold the responder between the echo and its reservation.
        let (echoed, at_echo) = std::sync::mpsc::channel();
        let (release, released) = std::sync::mpsc::channel::<()>();
        let released = Mutex::new(released);
        let higher_hook: connection::admission_script::Hook = Arc::new(move |point, _| {
            if point == connection::admission_script::Point::Echoed {
                let _ = echoed.send(());
                let _ = released
                    .lock_or_recover()
                    .recv_timeout(std::time::Duration::from_secs(5));
            }
        });
        let script =
            connection::admission_script::install(&[(higher_mgr.cast_const(), higher_hook)]);
        let lower_pointer = lower.as_ptr() as usize;
        let redial = thread::spawn(move || {
            // The test keeps the lower node started until this joins.
            hew_node_connect(lower_pointer as *mut HewNode, higher_address.as_ptr())
        });
        let observed = (|| -> Result<_, String> {
            at_echo
                .recv_timeout(std::time::Duration::from_secs(5))
                .map_err(|_| "the responder never got the echo")?;
            // The initiator publishes the redial and holds the shared connection.
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            let held = loop {
                if let Some(held) = connection::held_loser_for_test(&*lower_mgr, higher_identity) {
                    break held;
                }
                if std::time::Instant::now() >= deadline {
                    return Err("the initiator never held the shared connection".into());
                }
                std::thread::sleep(std::time::Duration::from_millis(1));
            };
            let shared = connection::claim_send_by_identity(&*higher_mgr, lower_identity, None)
                .ok_or("the responder cannot send on the shared connection")?;
            let route_slot =
                connection::wait_authenticated_peer_node_id_for_conn(&*lower_mgr, held.0, held.1);
            if route_slot == 0 {
                return Err("the held connection lost the peer's authority".into());
            }
            let (request, reply) = reply_table().register_with_waker(
                ConnectionKey::new(higher_mgr, shared.connection_id()),
                None,
                PendingReplyKind::Ask(shared.publication_token),
            );
            drop(shared);
            send_reply_envelope(
                route_slot,
                request,
                held.1,
                b"held",
                lower_mgr,
                &AtomicBool::new(false),
            );
            let (outcome, _) = reply
                .cond
                .wait_timeout_while(
                    reply.outcome.lock().unwrap(),
                    Duration::from_secs(5),
                    |outcome| outcome.is_none(),
                )
                .unwrap();
            let outcome = outcome
                .as_ref()
                .map(|outcome| (outcome.status, outcome.data.clone()));
            reply_table().remove(request);
            Ok(outcome)
        })();
        let _ = release.send(());
        let rc = redial.join().expect("redial");
        drop(script);
        assert_eq!(
            observed,
            Ok(Some((ReplyStatus::Success, b"held".to_vec()))),
            "the reply returned on the held connection"
        );
        assert_eq!(rc, 0, "the redial connected");
        await_one_shared_connection(lower, higher, "after the redial");
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// A crossing in which the initiator moves to the responder's dial, which the
/// responder then yields to the initiator's by the crossing rule once the
/// echo arrives: the initiator returns to its own dial without losing the
/// peer's member for a moment, or the peer's names, which the peer does not
/// announce again on a connection it kept throughout. Counterfactual: the
/// initiator retires the yielded dial before restoring its own, and suspects
/// the peer; or forgets the names when it moves to the yielded dial.
#[test]
fn initiator_returns_from_a_dial_the_responder_yields_without_suspicion() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let fences = fence_timeouts();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(371, 372);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let ((lower, lower_port), (higher, higher_port)) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                ((&a, port_a), (&b, port_b))
            } else {
                ((&b, port_b), (&a, port_a))
            };
        record_suspects(lower);
        record_suspects(higher);
        // The higher node's dial waits, authenticated, until the lower node's
        // dial has published at the higher node; it then commits later.
        let (waiting, higher_waiting) = std::sync::mpsc::channel::<()>();
        let waiting = Mutex::new(waiting);
        let (higher_node, lower_node) = (higher.as_ptr() as usize, lower.as_ptr() as usize);
        let higher_hook: connection::admission_script::Hook = Arc::new(move |point, origin| {
            if point == connection::admission_script::Point::Authenticated
                && origin == ConnectionOrigin::Dialled
            {
                let _ = waiting.lock_or_recover().send(());
                // The nodes outlive their admissions.
                await_owner(higher_node, lower_node, ConnectionOrigin::Accepted);
                await_owner(lower_node, higher_node, ConnectionOrigin::Dialled);
                // The lower node has learnt one of the higher node's names.
                let (lower, higher) = (
                    &*(lower_node as *mut HewNode),
                    &*(higher_node as *mut HewNode),
                );
                let name = Location::direct(
                    higher.auth.node_identity().expect("higher identity"),
                    7,
                    higher.auth.session_incarnation().expect("higher session"),
                )
                .expect("name location");
                (*lower.registry)
                    .remote_names
                    .lock_or_recover()
                    .insert("crossing".into(), name);
            }
        });
        let script = connection::admission_script::install(&[(
            (*higher.as_ptr()).conn_mgr.cast_const(),
            higher_hook,
        )]);
        let start = Arc::new(std::sync::Barrier::new(1));
        let higher_dial = spawn_dial(higher, lower_port, &start, None);
        higher_waiting
            .recv_timeout(std::time::Duration::from_secs(5))
            .expect("the higher node's dial authenticated");
        let lower_dial = spawn_dial(lower, higher_port, &start, None);
        let dials = (
            lower_dial.join().expect("lower dial"),
            higher_dial.join().expect("higher dial"),
        );
        drop(script);
        assert_eq!(fence_timeouts(), fences, "a script fence timed out");
        await_kept_connection(lower, higher, ConnectionOrigin::Dialled);
        await_kept_connection(higher, lower, ConnectionOrigin::Accepted);
        assert_eq!(
            (suspects(lower), suspects(higher)),
            (0, 0),
            "no member is suspected: dials {dials:?}"
        );
        assert!(
            (*(*lower.as_ptr()).registry)
                .remote_names
                .lock_or_recover()
                .contains_key("crossing"),
            "the lower node keeps the higher node's names"
        );
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// When the responder settles a crossing on the connection the initiator moved
/// to, the initiator learns the responder's names afresh from what the
/// responder announces there: a name the responder withdrew on the released
/// connection, unread when that one closed, does not survive the switch, and
/// the names it holds do. The withdrawn name is planted at the initiator.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "keep the scripted crossing and the names it settles together"
)]
fn initiator_relearns_the_responder_names_when_it_settles_a_switch() {
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();
    let (a, port_a, b, port_b) = start_authorized_tcp_pair(391, 392);
    // SAFETY: both nodes stay started until the stops below.
    unsafe {
        let ((lower, lower_port), (higher, higher_port)) =
            if kept_origin(&a, &b) == ConnectionOrigin::Dialled {
                ((&a, port_a), (&b, port_b))
            } else {
                ((&b, port_b), (&a, port_a))
            };
        let lower_mgr = (*lower.as_ptr()).conn_mgr.cast_const();
        let higher_mgr = (*higher.as_ptr()).conn_mgr.cast_const();
        let higher_identity = (*higher.as_ptr())
            .auth
            .node_identity()
            .expect("higher identity");
        let higher_session = (*higher.as_ptr())
            .auth
            .session_incarnation()
            .expect("higher session");
        // A name the responder holds, announced on each connection it publishes.
        assert!((*(*higher.as_ptr()).registry).apply_origin_name(
            "present",
            Location::direct(higher_identity, 9, higher_session).expect("present location"),
            true
        ));
        let progress: Progress = Arc::default();
        let (higher_node, lower_node) = (higher.as_ptr() as usize, lower.as_ptr() as usize);
        let lower_hook: connection::admission_script::Hook = {
            let progress = Arc::clone(&progress);
            Arc::new(move |point, origin| {
                use connection::admission_script::Point;
                match (point, origin) {
                    (Point::Authenticated, ConnectionOrigin::Dialled) => {
                        await_progress(&progress, |state| state.higher_dial_settled);
                        // The nodes outlive their admissions.
                        let (lower, higher) = (
                            &*(lower_node as *mut HewNode),
                            &*(higher_node as *mut HewNode),
                        );
                        let name = Location::direct(
                            higher.auth.node_identity().expect("higher identity"),
                            7,
                            higher.auth.session_incarnation().expect("higher session"),
                        )
                        .expect("name location");
                        (*lower.registry)
                            .remote_names
                            .lock_or_recover()
                            .insert("ghost".into(), name);
                    }
                    (Point::AwaitingCommit, ConnectionOrigin::Dialled) => {
                        progress.0.lock_or_recover().lower_awaiting_commit = true;
                        progress.1.notify_all();
                    }
                    _ => {}
                }
            })
        };
        let higher_hook: connection::admission_script::Hook = {
            let progress = Arc::clone(&progress);
            Arc::new(move |point, origin| {
                if point == connection::admission_script::Point::Authenticated
                    && origin == ConnectionOrigin::Accepted
                {
                    await_progress(&progress, |state| {
                        state.lower_awaiting_commit
                            || (state.higher_dial_settled && {
                                connection::peer_connection_view(
                                    &*(*(higher_node as *mut HewNode)).conn_mgr,
                                    (*(lower_node as *mut HewNode))
                                        .auth
                                        .node_identity()
                                        .expect("lower identity"),
                                )
                            }
                            .installed
                                == 0)
                    });
                }
            })
        };
        let script = connection::admission_script::install(&[
            (lower_mgr, lower_hook),
            (higher_mgr, higher_hook),
        ]);
        let barrier = Arc::new(std::sync::Barrier::new(2));
        let lower_dial = spawn_dial(lower, higher_port, &barrier, None);
        let higher_dial = spawn_dial(higher, lower_port, &barrier, Some(Arc::clone(&progress)));
        let dials = (
            lower_dial.join().expect("lower dial"),
            higher_dial.join().expect("higher dial"),
        );
        await_kept_connection(lower, higher, ConnectionOrigin::Dialled);
        await_kept_connection(higher, lower, ConnectionOrigin::Accepted);
        drop(script);
        let progress = std::mem::take(&mut *progress.0.lock_or_recover());
        assert!(
            progress.lower_awaiting_commit && !progress.timed_out,
            "the script did not run: {progress:?}"
        );
        let names = &(*(*lower.as_ptr()).registry).remote_names;
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        while {
            let names = names.lock_or_recover();
            names.contains_key("ghost") || !names.contains_key("present")
        } {
            assert!(
                std::time::Instant::now() < deadline,
                "dials {dials:?}: the initiator's names for the responder: {:?}",
                names.lock_or_recover().keys().collect::<Vec<_>>()
            );
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
        assert_eq!(hew_node_stop(b.as_ptr()), 0);
        assert_eq!(hew_node_stop(a.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// The commit order of the connection that holds `peer`'s claim at `node`,
/// asserted equal at both ends.
///
/// # Safety
/// Both nodes are started and live.
unsafe fn shared_commit_order(lower: &TestNode, higher: &TestNode) -> Option<u64> {
    // SAFETY: the caller keeps both nodes started.
    unsafe {
        let lower_order = connection::commit_order_for_test(
            &*(*lower.as_ptr()).conn_mgr,
            (*higher.as_ptr())
                .auth
                .node_identity()
                .expect("higher identity"),
        );
        let higher_order = connection::commit_order_for_test(
            &*(*higher.as_ptr()).conn_mgr,
            (*lower.as_ptr())
                .auth
                .node_identity()
                .expect("lower identity"),
        );
        assert_eq!(
            lower_order, higher_order,
            "both ends keep the same connection"
        );
        lower_order
    }
}

/// Wait until each node holds exactly one published connection to the other,
/// and it is the same connection: dialled by one end, accepted by the other,
/// with the same commit order at both ends.
///
/// # Safety
/// Both nodes are started and live.
unsafe fn await_one_shared_connection(lower: &TestNode, higher: &TestNode, case: &str) {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        // SAFETY: the caller keeps both nodes started.
        let (lower_view, higher_view) =
            unsafe { (peer_view(lower, higher), peer_view(higher, lower)) };
        let shared = match (lower_view.owner, higher_view.owner) {
            (
                Some((ClaimState::Published, lower_origin)),
                Some((ClaimState::Published, higher_origin)),
            ) => lower_origin != higher_origin,
            _ => false,
        };
        // SAFETY: the caller keeps both nodes started.
        let orders = unsafe {
            (
                connection::commit_order_for_test(
                    &*(*lower.as_ptr()).conn_mgr,
                    (*higher.as_ptr())
                        .auth
                        .node_identity()
                        .expect("higher identity"),
                ),
                connection::commit_order_for_test(
                    &*(*higher.as_ptr()).conn_mgr,
                    (*lower.as_ptr())
                        .auth
                        .node_identity()
                        .expect("lower identity"),
                ),
            )
        };
        if shared && lower_view.installed == 1 && higher_view.installed == 1 && orders.0 == orders.1
        {
            return;
        }
        assert!(
            std::time::Instant::now() < deadline,
            "{case}: no shared connection: lower {lower_view:?}, higher {higher_view:?}"
        );
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
}
