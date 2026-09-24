//! Inbound ask worker bound and node-stop drain tests.

use super::*;

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
    thread::sleep(Duration::from_millis(50));
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
    send_reply_envelope(999, 123, &[], dangling_mgr, shutdown_started.as_ref());

    // SAFETY: node pointer remains valid until drop.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

// ── Inbound ask worker bound tests ─────────────────────────────────────
//
// These tests verify the backpressure mechanism added to node_inbound_router.
// The mechanism prevents a remote peer from exhausting OS thread count or
// virtual memory by flooding inbound asks.
//
// All of these tests acquire the shared runtime test lock so they serialize with any
// other test that runs a node or manipulates INBOUND_ASK_ACTIVE.

/// `InboundAskGuard` decrements `INBOUND_ASK_ACTIVE` and the per-manager
/// counter exactly once on drop, including when the enclosing scope exits
/// via panic.
#[test]
fn inbound_ask_guard_decrements_on_drop() {
    let _lock = crate::runtime_test_guard();
    // Reset to a known value; restore on exit.
    let saved = INBOUND_ASK_ACTIVE.swap(1, Ordering::AcqRel);
    let per_mgr = Arc::new(AtomicUsize::new(1));
    {
        let _guard = InboundAskGuard(Arc::clone(&per_mgr));
        assert_eq!(
            INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
            1,
            "global counter must be 1 while guard is live"
        );
        assert_eq!(
            per_mgr.load(Ordering::Acquire),
            1,
            "per-mgr counter must be 1 while guard is live"
        );
    }
    // Guard dropped — both counters must be 0.
    let after_global = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
    let after_per_mgr = per_mgr.load(Ordering::Acquire);
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
    assert_eq!(
        after_global, 0,
        "InboundAskGuard must decrement INBOUND_ASK_ACTIVE on drop"
    );
    assert_eq!(
        after_per_mgr, 0,
        "InboundAskGuard must decrement per-manager counter on drop"
    );
}

/// Two guards decrement independently (one per spawned thread).
#[test]
fn inbound_ask_guard_pair_decrements_twice() {
    let _lock = crate::runtime_test_guard();
    let saved = INBOUND_ASK_ACTIVE.swap(2, Ordering::AcqRel);
    let per_mgr1 = Arc::new(AtomicUsize::new(1));
    let per_mgr2 = Arc::new(AtomicUsize::new(1));
    let g1 = InboundAskGuard(Arc::clone(&per_mgr1));
    let g2 = InboundAskGuard(Arc::clone(&per_mgr2));
    drop(g1);
    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        1,
        "first guard must decrement global by 1"
    );
    assert_eq!(
        per_mgr1.load(Ordering::Acquire),
        0,
        "first guard must decrement its per-mgr counter to 0"
    );
    drop(g2);
    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        0,
        "second guard must decrement global back to zero"
    );
    assert_eq!(
        per_mgr2.load(Ordering::Acquire),
        0,
        "second guard must decrement its per-mgr counter to 0"
    );
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
}

/// When `INBOUND_ASK_ACTIVE` is saturated to `INBOUND_ASK_WORKER_LIMIT` the
/// optimistic-increment + revert logic correctly prevents over-commitment.
///
/// This test directly exercises the counter-check branch used in
/// `node_inbound_router` without needing a live transport.
#[test]
fn inbound_ask_worker_limit_rejects_at_capacity() {
    let _lock = crate::runtime_test_guard();
    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

    // Simulate what node_inbound_router does for an inbound ask.
    let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
    let at_limit = prev >= INBOUND_ASK_WORKER_LIMIT;
    if at_limit {
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
    }

    // Must have detected the limit and reverted.
    assert!(
        at_limit,
        "should detect limit when counter == INBOUND_ASK_WORKER_LIMIT"
    );
    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        INBOUND_ASK_WORKER_LIMIT,
        "counter must be reverted to the limit after rejection"
    );

    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
}

/// When `INBOUND_ASK_ACTIVE` is one below the limit, a new ask is accepted.
#[test]
fn inbound_ask_worker_limit_accepts_below_capacity() {
    let _lock = crate::runtime_test_guard();
    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT - 1, Ordering::AcqRel);

    let prev = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
    let at_limit = prev >= INBOUND_ASK_WORKER_LIMIT;
    if at_limit {
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
    }

    assert!(!at_limit, "ask just below limit must be accepted");
    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        INBOUND_ASK_WORKER_LIMIT,
        "accepted ask increments counter to limit"
    );

    // Release the slot we acquired (simulating the InboundAskGuard).
    INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);
}

/// End-to-end: inbound ask counter is bounded during a real two-node ask
/// round-trip. After the ask completes the worker slot is released and the
/// counter returns to its pre-ask value.
#[cfg(feature = "quic")]
#[test]
fn inbound_ask_active_counter_returns_to_baseline_after_round_trip() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();
    register_test_u32_codec(test_dispatch(), 1);

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(320, 321);

    _real_sched = init_real_scheduler();

    // Spawn a u32-echo actor on node2.
    crate::pid::hew_pid_set_local_node(321);
    let echo_actor = spawn_remote_test_actor(ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(320);
    assert!(!echo_actor.is_null(), "actor spawn failed");
    // SAFETY: the actor was just spawned successfully and remains valid here.
    let actor_id = unsafe { (*echo_actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 321);

    let connect_addr = CString::new(format!("321@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and the connect address are valid for this connection attempt.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Record the counter before the ask so we can verify it returns to baseline.
    let baseline = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);

    let payload: u32 = 0xDEAD_BEEF;
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: payload is a valid u32 on the stack; its address is valid for this call.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };
    assert!(status == AskError::None as i32, "remote ask must succeed");

    // After the ask completes the handler thread exits, dropping InboundAskGuard.
    // Give it a brief moment to drain.
    let settled = (0..50).any(|_| {
        let v = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
        if v == baseline {
            true
        } else {
            thread::sleep(Duration::from_millis(10));
            false
        }
    });
    assert!(
        settled,
        "INBOUND_ASK_ACTIVE did not return to baseline after ask completed (got {}; expected {})",
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        baseline,
    );
    assert!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire) <= INBOUND_ASK_WORKER_LIMIT,
        "active worker count must never exceed INBOUND_ASK_WORKER_LIMIT"
    );

    // SAFETY: actor and nodes were allocated in this test and are still valid here.
    unsafe {
        let _ = crate::actor::hew_actor_free(echo_actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

// ── Over-limit rejection tests ────────────────────────────────────────
//
// These tests verify the fail-closed semantics of the worker-limit
// rejection path for both void and non-void remote asks.
//
// When INBOUND_ASK_ACTIVE is artificially saturated to
// INBOUND_ASK_WORKER_LIMIT the inbound router sends a rejection reply
// (HEW_REPLY_REJECT_MSG_TYPE).  The receiving node's connection reader
// calls `fail_remote_reply`, which sets ReplyStatus::Failed with reason
// WorkerAtCapacity so that `hew_node_api_ask` returns the precise
// discriminant — not a false-success void sentinel, and not the generic
// ConnectionDropped that indicates a wire-level failure.

/// Over-limit rejection of a **void** remote ask returns null +
/// `WorkerAtCapacity` (not the void-success sentinel, not `ConnectionDropped`).
#[cfg(feature = "quic")]
#[test]
fn over_limit_void_ask_fails_closed_with_worker_at_capacity() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(322, 323);
    _real_sched = init_real_scheduler();

    // Spawn a void-reply actor on node2.
    crate::pid::hew_pid_set_local_node(323);
    let actor = spawn_remote_test_actor(void_ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(322);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 323);

    let connect_addr = CString::new(format!("323@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Saturate the worker counter on node2 (the answering node) so the
    // next inbound ask is rejected.
    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

    // Void ask: reply_size == 0. Before the fix this would have returned
    // the void-success sentinel because the rejection sent an empty payload
    // which remote_reply_data_to_ptr mistook for a void success.
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: null payload / size-0 are valid; this is a void ask.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            ptr::null_mut(),
            0,
            TEST_REMOTE_ASK_TIMEOUT_MS,
            0,
        )
    };
    let err = status;

    // Restore before any assertions so the teardown path is clean.
    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

    assert!(
        status != AskError::None as i32,
        "over-limit void ask must return null (got non-null = false success)"
    );
    assert_eq!(
        err,
        AskError::WorkerAtCapacity as i32,
        "over-limit void ask must report WorkerAtCapacity, not ConnectionDropped"
    );

    // SAFETY: actor and nodes remain valid until teardown here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

/// Over-limit rejection of a **non-void** remote ask returns null +
/// `WorkerAtCapacity` (not `PayloadSizeMismatch` from the old empty-
/// payload path, not `ConnectionDropped`, and not a spurious success).
#[cfg(feature = "quic")]
#[test]
fn over_limit_nonvoid_ask_fails_closed_with_worker_at_capacity() {
    let _guard = crate::runtime_test_guard();
    let _real_sched;
    crate::registry::hew_registry_clear();
    register_test_u32_codec(test_dispatch(), 1);

    // Cross-bound SPKI→NodeId credentials → Strict authorized connection.
    let (node1, _node1_port, node2, node2_port) = start_authorized_quic_mesh_pair(324, 325);
    _real_sched = init_real_scheduler();

    // Spawn a u32-echo actor on node2.
    crate::pid::hew_pid_set_local_node(325);
    let actor = spawn_remote_test_actor(ask_probe_dispatch);
    crate::pid::hew_pid_set_local_node(324);
    assert!(!actor.is_null(), "actor spawn failed");
    // SAFETY: actor was just spawned and is valid here.
    let actor_id = unsafe { (*actor).id };
    assert_eq!(crate::pid::hew_pid_node(actor_id), 325);

    let connect_addr = CString::new(format!("325@127.0.0.1:{node2_port}")).unwrap();
    // SAFETY: node1 and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };
    // SAFETY: both node pointers remain valid until the end of the test.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // Saturate the worker counter on node2.
    let saved = INBOUND_ASK_ACTIVE.swap(INBOUND_ASK_WORKER_LIMIT, Ordering::AcqRel);

    let payload: u32 = 42;
    let target = remote_pid_for_node(&node2, actor_id);
    // SAFETY: payload is a valid u32; its address is valid for this call.
    let (_, status) = unsafe {
        ask_for_test(
            &raw const target,
            test_dispatch(),
            1,
            std::ptr::from_ref(&payload).cast_mut().cast::<c_void>(),
            std::mem::size_of::<u32>(),
            TEST_REMOTE_ASK_TIMEOUT_MS,
            std::mem::size_of::<u32>(),
        )
    };
    let err = status;

    INBOUND_ASK_ACTIVE.store(saved, Ordering::Release);

    assert!(
        status != AskError::None as i32,
        "over-limit non-void ask must return null"
    );
    assert_eq!(
        err,
        AskError::WorkerAtCapacity as i32,
        "over-limit non-void ask must report WorkerAtCapacity (not PayloadSizeMismatch)"
    );

    // SAFETY: actor and nodes remain valid until teardown here.
    unsafe {
        let _ = crate::actor::hew_actor_free(actor);
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

// ── Regression: race-window fixes ────────────────────────────────────

/// After `hew_connmgr_mark_stopping` is called, `node_inbound_router`
/// must not spawn a new inbound-ask worker (the spawn-window fix).
///
/// This test marks the connection manager as stopping, then calls the
/// inbound router directly with an ask-shaped message and asserts that
/// neither `INBOUND_ASK_ACTIVE` nor the per-manager counter increases —
/// no worker was spawned.
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

    // The per-manager counter is fresh for this node and must be zero.
    // The global counter may be non-zero due to other tests sharing the
    // process — record it and verify it is UNCHANGED after the router call.
    let global_before = INBOUND_ASK_ACTIVE.load(Ordering::Acquire);
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
            1,
            ptr::null_mut(),
            0,
            /*request_id=*/ 1,
            /*source_node_id=*/ 1,
            (*node.as_ptr()).conn_mgr,
        );
    }

    // Give any mistakenly-spawned thread time to increment the counters.
    thread::sleep(Duration::from_millis(20));

    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        global_before,
        "global counter must not change after shutdown has started"
    );
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
/// This test artificially inflates both `INBOUND_ASK_ACTIVE` and the
/// per-conn_mgr counter by one to simulate an in-flight worker, schedules
/// a background thread to decrement them after a short delay, and then
/// asserts that stop waited for the decrement (counter is zero on return).
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

    // Simulate one active inbound-ask worker: increment both counters as
    // the real spawn path does.  A background thread decrements them after
    // 60 ms — long enough to make the drain observable without being slow.
    let global_saved = INBOUND_ASK_ACTIVE.fetch_add(1, Ordering::AcqRel);
    per_mgr_active.fetch_add(1, Ordering::AcqRel);
    let per_mgr_clone = Arc::clone(&per_mgr_active);
    let decrement_handle = thread::spawn(move || {
        thread::sleep(Duration::from_millis(60));
        INBOUND_ASK_ACTIVE.fetch_sub(1, Ordering::AcqRel);
        per_mgr_clone.fetch_sub(1, Ordering::AcqRel);
    });

    let stop_start = std::time::Instant::now();
    // SAFETY: node is valid and owned by TestNode.
    unsafe { assert_eq!(hew_node_stop(node.as_ptr()), 0) };
    let elapsed = stop_start.elapsed();

    // Join the decrement thread (it should already be done by now).
    decrement_handle.join().expect("decrement thread panicked");

    assert_eq!(
        per_mgr_active.load(Ordering::Acquire),
        0,
        "per-manager counter must be zero after node_stop"
    );
    assert_eq!(
        INBOUND_ASK_ACTIVE.load(Ordering::Acquire),
        global_saved,
        "global INBOUND_ASK_ACTIVE must be back to its pre-test value"
    );

    // stop must have waited at least ~60 ms for the drain — but at most
    // a generous 4 s to avoid flakiness on heavily loaded CI machines.
    assert!(
        elapsed >= Duration::from_millis(40),
        "node_stop returned too quickly ({elapsed:?}); drain did not wait"
    );
    assert!(
        elapsed < Duration::from_secs(4),
        "node_stop took too long ({elapsed:?}); drain may have stalled"
    );

    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn node_stop_waits_to_free_connmgr_until_inbound_ask_error_worker_drains() {
    struct HookResetGuard;

    impl Drop for HookResetGuard {
        fn drop(&mut self) {
            INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.release();
            INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.reset();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.release();
            NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.reset();
        }
    }

    let _guard = crate::runtime_test_guard();
    let _real_sched;
    let _hook_reset = HookResetGuard;
    crate::registry::hew_registry_clear();

    INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.arm(true);
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

    // SAFETY: conn_mgr is live and source_node_id identifies the connected peer.
    unsafe {
        node_inbound_router(
            actor_id,
            1,
            ptr::null_mut(),
            0,
            /*request_id=*/ 1,
            /*source_node_id=*/ 353,
            (*node2.as_ptr()).conn_mgr,
        );
    }

    assert!(
        INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.wait_for_enter(Duration::from_secs(1)),
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

    INBOUND_ASK_ERROR_FEATURE_FLAGS_HOOK.release();

    assert!(
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_secs(1)),
        "hew_node_stop should reach conn_mgr free after the worker drains"
    );
    assert_eq!(
        stop_rx
            .recv_timeout(Duration::from_secs(2))
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
                1,
                ptr::null_mut(),
                0,
                /*request_id=*/ 1,
                /*source_node_id=*/ 1,
                conn_mgr_addr as *mut connection::HewConnMgr,
            );
        }
    });

    assert!(
        INBOUND_ROUTER_AFTER_INCREMENT_HOOK.wait_for_enter(Duration::from_secs(1)),
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
        NODE_STOP_BEFORE_CONNMGR_FREE_HOOK.wait_for_enter(Duration::from_secs(1)),
        "hew_node_stop never reached conn_mgr free after the wedged router drained"
    );
    assert_eq!(
        stop_rx
            .recv_timeout(Duration::from_secs(2))
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
#[cfg(feature = "encryption")]
#[test]
fn handle_inbound_ask_capture_safe_for_secondary_node_freed_connmgr() {
    struct HookResetGuard;
    impl Drop for HookResetGuard {
        fn drop(&mut self) {
            INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.release();
            INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.reset();
        }
    }

    let _guard = crate::runtime_test_guard();
    let _real_sched;
    let _hook_reset = HookResetGuard;
    crate::registry::hew_registry_clear();

    // Wedge any worker that reaches the top-of-handler capture hook.
    INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.arm(true);

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
            /*msg_type=*/ 1,
            /*payload=*/ &[],
            /*request_id=*/ 1,
            /*source_node_id=*/ 363,
            // SAFETY: pointer ferried as usize; valid until freed by the
            // stop below, after which the fix guarantees it is never read.
            SendConnMgr(conn_mgr_addr as *mut connection::HewConnMgr),
            shutdown_started,
        );
    });

    assert!(
        INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.wait_for_enter(Duration::from_secs(2)),
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
    INBOUND_ASK_FEATURE_FLAGS_CAPTURE_HOOK.release();
    worker_handle
        .join()
        .expect("straggler worker panicked after secondary-node teardown");

    crate::registry::hew_registry_clear();
}
