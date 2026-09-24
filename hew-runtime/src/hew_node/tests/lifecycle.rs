//! Node lifecycle, quarantine and name-registry tests.

use super::*;

#[test]
fn accept_thread_stop_reports_panic_and_consumes_handle() {
    let _runtime_guard = crate::runtime_test_guard();
    crate::hew_clear_error();
    let bind = CString::new("127.0.0.1:0").expect("valid bind address");
    // SAFETY: bind is a valid C string for the duration of the call.
    let node_ptr = unsafe { hew_node_new(991, bind.as_ptr()) };
    assert!(!node_ptr.is_null());
    // SAFETY: this test exclusively owns the live node allocation.
    let node = unsafe { &mut *node_ptr };
    *node.accept_thread.lock_or_recover() =
        Some(std::thread::spawn(|| panic!("accept intentional panic")));

    stop_node_accept_thread(node);

    let error_ptr = crate::hew_last_error();
    assert!(
        !error_ptr.is_null(),
        "joining a panicked accept thread must record a diagnostic"
    );
    // SAFETY: stop_node_accept_thread populated this thread's last-error slot.
    let error = unsafe {
        CStr::from_ptr(error_ptr)
            .to_str()
            .expect("last error should be utf-8")
    };
    assert!(
        error.contains("hew node accept thread panicked during teardown"),
        "unexpected last error: {error}"
    );
    assert!(
        error.contains("accept intentional panic"),
        "panic payload must be included in the diagnostic: {error}"
    );
    assert!(node.accept_stop.load(Ordering::Acquire));
    assert!(node.accept_thread.lock_or_recover().is_none());

    // SAFETY: the test owns node_ptr and its accept thread has been joined.
    unsafe { hew_node_free(node_ptr) };
}

#[test]
fn monitor_and_link_setup_statuses_match_hew_error_discriminants() {
    assert_eq!(MONITOR_ERR_NODE_NOT_RUNNING, 1);
    assert_eq!(MONITOR_ERR_LOCAL_SHUTDOWN, 6);
    assert_eq!(MONITOR_ERR_RESOURCE_EXHAUSTED, 11);
    assert_eq!(LINK_ERR_DEAD, 1);
    assert_eq!(LINK_ERR_PARTITION, 2);
    assert_eq!(LINK_ERR_NO_CURRENT_ACTOR, 3);
}

#[test]
fn quarantine_insert_blocks_then_evict_clears() {
    let _guard = crate::runtime_test_guard();
    // Insert quarantines the peer at the dead incarnation; a same-or-lower
    // live incarnation is blocked, a higher live incarnation is not.
    quarantine_insert(7, 5);
    assert!(
        quarantine_is_blocked(7, 5),
        "equal live incarnation is blocked"
    );
    assert!(
        quarantine_is_blocked(7, 4),
        "lower live incarnation is blocked"
    );
    assert!(
        !quarantine_is_blocked(7, 6),
        "a live incarnation past the quarantined one is not blocked"
    );
    assert!(
        !quarantine_is_blocked(8, 5),
        "an unrelated node is not blocked"
    );
    // Eviction clears the entry.
    quarantine_evict(7);
    assert!(
        !quarantine_is_blocked(7, 5),
        "evicted node is sendable again"
    );
}

#[test]
fn registry_repoint_changes_future_lookup_without_rewriting_old_location() {
    let registry = HewRegistry::default();
    let old = Location::new(crate::node_identity::NodeId::from_bytes([1; 16]), 100, 3).unwrap();
    let new = Location::new(crate::node_identity::NodeId::from_bytes([1; 16]), 200, 3).unwrap();
    registry
        .remote_names
        .lock_or_recover()
        .insert("kv".to_owned(), old);
    let captured = registry
        .remote_names
        .lock_or_recover()
        .get("kv")
        .copied()
        .unwrap();
    registry
        .remote_names
        .lock_or_recover()
        .insert("kv".to_owned(), new);
    assert_eq!(captured, old);
    assert_eq!(
        registry.remote_names.lock_or_recover().get("kv").copied(),
        Some(new)
    );
}

#[test]
fn quarantine_insert_is_monotonic() {
    let _guard = crate::runtime_test_guard();
    quarantine_insert(9, 3);
    // A higher-incarnation death overwrites.
    quarantine_insert(9, 7);
    assert!(
        quarantine_is_blocked(9, 7),
        "blocked at the higher incarnation"
    );
    assert!(
        quarantine_is_blocked(9, 6),
        "blocked below the higher incarnation"
    );
    // A lower-incarnation death never regresses the recorded incarnation.
    quarantine_insert(9, 2);
    assert!(
        quarantine_is_blocked(9, 7),
        "a lower re-insert must not regress the quarantined incarnation"
    );
}

#[test]
fn quarantine_evict_absent_node_is_noop() {
    let _guard = crate::runtime_test_guard();
    // No panic, no effect.
    quarantine_evict(123);
    assert!(!quarantine_is_blocked(123, 1));
}

/// Proving gate B: a `Quarantine`-policy send to a buried peer fails closed at
/// the consult, then resolves normally once the quarantine entry is evicted.
/// The consult is exercised through `quarantine_blocks_send` (the exact code
/// the send/ask paths call) with a real node + cluster so the live-incarnation
/// lookup is genuine.
#[test]
fn quarantine_blocks_send_under_policy_then_clears_on_evict() {
    const PEER: u16 = 602;
    let _guard = crate::runtime_test_guard();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node_handle = unsafe { TestNode::new(601, &bind_addr) };
    assert!(!node_handle.as_ptr().is_null());
    // SAFETY: pointer came from TestNode::new and is valid until drop.
    unsafe { assert_eq!(hew_node_start(node_handle.as_ptr()), 0) };
    // SAFETY: started node has a non-null cluster.
    let node = unsafe { &*node_handle.as_ptr() };
    assert!(!node.cluster.is_null());
    // SAFETY: cluster is valid while the node is running.
    let cluster = unsafe { &*node.cluster };

    // The peer is known to the cluster, buried (DEAD) at incarnation 5. Seed
    // the membership table so the live-incarnation lookup resolves, and record
    // the quarantine at the incarnation it died at.
    cluster.seed_member_for_test(PEER, crate::cluster::MEMBER_DEAD, 5);
    quarantine_insert(PEER, 5);

    // Under a Quarantine policy the buried peer (live incarnation 5 <=
    // quarantined 5) is blocked.
    {
        let mut ctx = crate::execution_context::HewExecutionContext {
            partition_policy: crate::execution_context::PartitionPolicy::Quarantine.to_slot(),
            ..crate::execution_context::HewExecutionContext::default()
        };
        let _ctx_guard =
            crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
        assert!(
            quarantine_blocks_send(node, PEER),
            "a Quarantine-policy send to a buried peer must fail closed"
        );
    }

    // A FailFast policy never consults the set: the same peer is NOT blocked.
    {
        let mut ctx = crate::execution_context::HewExecutionContext {
            partition_policy: crate::execution_context::PartitionPolicy::FailFast.to_slot(),
            ..crate::execution_context::HewExecutionContext::default()
        };
        let _ctx_guard =
            crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
        assert!(
            !quarantine_blocks_send(node, PEER),
            "a FailFast-policy send must not consult the quarantine set"
        );
    }

    // Evict (the readmission edge) clears the block under Quarantine too.
    quarantine_evict(PEER);
    {
        let mut ctx = crate::execution_context::HewExecutionContext {
            partition_policy: crate::execution_context::PartitionPolicy::Quarantine.to_slot(),
            ..crate::execution_context::HewExecutionContext::default()
        };
        let _ctx_guard =
            crate::execution_context::TestExecutionContext::install(std::mem::take(&mut ctx));
        assert!(
            !quarantine_blocks_send(node, PEER),
            "after eviction the peer is sendable again"
        );
    }

    // SAFETY: stop the node before drop.
    unsafe { assert_eq!(hew_node_stop(node_handle.as_ptr()), 0) };
}

/// The `hew_set_partition_policy` C-ABI symbol — the exact export the Hew
/// surface (`std/link_monitor.hew`: `set_partition_policy`) lowers to —
/// flips the real send gate. This is the policy-takes-effect leg: driving
/// the setter (rather than pre-baking the slot as the test above does)
/// proves the FFI entry point installed on the dispatch context is what the
/// quarantine consult reads.
#[test]
fn set_partition_policy_symbol_drives_the_quarantine_gate() {
    use crate::execution_context::{
        hew_set_partition_policy, HewExecutionContext, PartitionPolicy, TestExecutionContext,
    };
    const PEER: u16 = 612;

    // Position IS the ABI: the C-ABI tag is the discriminant, and the two
    // Hew-side declarations that mirror it (`std/link_monitor.hew`'s
    // `PartitionPolicy` and the `MONITOR_REF_HEW` prelude enum in
    // `hew-types` `check::registration`) must keep this exact order. A drift
    // here would silently misroute a policy tag across the FFI boundary
    // (`builtin-enum-variant-mirror-discipline`).
    assert_eq!(PartitionPolicy::FailFast as i64, 0);
    assert_eq!(PartitionPolicy::Deadline as i64, 1);
    assert_eq!(PartitionPolicy::MonitorLost as i64, 2);
    assert_eq!(PartitionPolicy::CrashLinked as i64, 3);
    assert_eq!(PartitionPolicy::Quarantine as i64, 4);

    let _guard = crate::runtime_test_guard();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node_handle = unsafe { TestNode::new(611, &bind_addr) };
    assert!(!node_handle.as_ptr().is_null());
    // SAFETY: pointer came from TestNode::new and is valid until drop.
    unsafe { assert_eq!(hew_node_start(node_handle.as_ptr()), 0) };
    // SAFETY: started node has a non-null cluster.
    let node = unsafe { &*node_handle.as_ptr() };
    assert!(!node.cluster.is_null());
    // SAFETY: cluster is valid while the node is running.
    let cluster = unsafe { &*node.cluster };
    cluster.seed_member_for_test(PEER, crate::cluster::MEMBER_DEAD, 5);
    quarantine_insert(PEER, 5);

    // Install a dispatch context with the DEFAULT (null) policy slot, then
    // drive the policy purely through the C-ABI setter.
    let _ctx_guard = TestExecutionContext::install(HewExecutionContext::default());

    // Quarantine (tag 4) installed via the setter blocks the buried peer.
    assert!(hew_set_partition_policy(4));
    assert!(
        quarantine_blocks_send(node, PEER),
        "Quarantine set via hew_set_partition_policy must block the buried peer"
    );

    // Overwriting with FailFast (tag 0) via the same setter clears the block:
    // the slot is writable repeatedly within one dispatch, not one-shot.
    assert!(hew_set_partition_policy(0));
    assert!(
        !quarantine_blocks_send(node, PEER),
        "FailFast set via hew_set_partition_policy must not consult the set"
    );

    // SAFETY: stop the node before drop.
    unsafe { assert_eq!(hew_node_stop(node_handle.as_ptr()), 0) };
}

#[test]
fn node_read_and_sweep_paths_treat_missing_runtime_as_empty_state() {
    let _lock = crate::scheduler::SchedTestLock::acquire();
    assert!(
        crate::runtime::rt_default().is_none(),
        "test requires the runtime slot to be empty"
    );

    with_current_node_read(|current| assert_eq!(*current, 0));
    assert!(!complete_remote_reply(std::ptr::null(), 0, 1, &[1, 2, 3]));
    assert!(!fail_remote_reply(std::ptr::null(), 0, 1, &[]));
    fail_remote_replies_for_connection(std::ptr::null(), 0);
    // SAFETY: with no runtime there are no known-node registries to sweep.
    unsafe { unregister_actor_names(crate::pid::hew_pid_make(1, 1)) };
}

#[test]
fn real_scheduler_guard_drop_restores_empty_runtime_slot() {
    let _lock = crate::scheduler::SchedTestLock::acquire();
    assert!(
        crate::runtime::rt_default().is_none(),
        "test requires the runtime slot to start empty"
    );

    {
        let _real_sched = init_real_scheduler();
        assert!(
            crate::runtime::rt_default().is_some(),
            "real scheduler init must install a runtime"
        );
    }

    assert!(
        crate::runtime::rt_default().is_none(),
        "dropping the real scheduler guard must restore the empty runtime slot"
    );
}

/// A low-level node whose explicit route slot contradicts its installed
/// snapshot route slot is rejected before the listener binds.
#[test]
fn node_start_rejects_conflicting_snapshot_route_slot_before_listen() {
    use crate::peer_binding::{PeerAuthConfig, PeerCredential};
    let _guard = crate::runtime_test_guard();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // Explicit low-level route slot 100 conflicts with snapshot route slot 42.
    // SAFETY: bind_addr is valid for the duration of this test.
    let node = unsafe { TestNode::new(100, &bind_addr) };
    assert!(!node.as_ptr().is_null());
    let mut config = PeerAuthConfig::default();
    config.local_route_slot = std::num::NonZeroU16::new(42);
    config
        .pin_peer(43, PeerCredential::NoiseKey([0xAB; 32]))
        .expect("distinct peer pin");
    let snapshot = config.snapshot();
    // SAFETY: node is STOPPED; installing a snapshot is valid.
    let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
    assert_eq!(set_rc, 0);
    // SAFETY: node is valid; start must reject the conflicting route slot.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(rc, -1, "conflicting snapshot route slot must be rejected");
    // SAFETY: node valid; assert fail-closed: STOPPED, no manager, no transport.
    unsafe {
        let n = &*node.as_ptr();
        assert_eq!(n.state.load(Ordering::Acquire), NODE_STATE_STOPPED);
        assert!(n.conn_mgr.is_null(), "no manager on rejected start");
        assert!(n.transport.is_null(), "no listener bound on rejected start");
    }
    // SAFETY: reads the thread-local last-error C string set by the start
    // path; the pointer is valid until the next error is set on this thread.
    let err = unsafe {
        let p = crate::hew_last_error();
        if p.is_null() {
            String::new()
        } else {
            std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned()
        }
    };
    assert!(
        err.contains("conflicts with the frozen local route slot"),
        "diagnostic should name the conflict; got: {err}"
    );
}

/// A low-level node created with route slot zero adopts the frozen snapshot
/// route slot at start.
#[test]
fn node_start_adopts_snapshot_route_slot_when_created_with_zero() {
    use crate::peer_binding::{PeerAuthConfig, PeerCredential};
    let _guard = crate::runtime_test_guard();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr valid for the test.
    let node = unsafe { TestNode::new(0, &bind_addr) };
    assert!(!node.as_ptr().is_null());
    let mut config = PeerAuthConfig::default();
    config.local_route_slot = std::num::NonZeroU16::new(4242);
    config
        .pin_peer(4243, PeerCredential::NoiseKey([0xCD; 32]))
        .expect("distinct peer pin");
    let snapshot = config.snapshot();
    // SAFETY: node STOPPED.
    let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
    assert_eq!(set_rc, 0);
    // SAFETY: node valid; start should adopt 4242 and succeed.
    let rc = unsafe { hew_node_start(node.as_ptr()) };
    assert_eq!(
        rc,
        0,
        "start should adopt snapshot route slot: {:?}",
        crate::stream_error::take_last_error()
    );
    // SAFETY: node valid & running.
    unsafe {
        assert_eq!((*node.as_ptr()).route_slot, 4242);
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
}

/// Defence-in-depth: a self-inconsistent snapshot (unverified opt-out WITH
/// bindings) fails the node's own start before listen.
#[test]
fn node_start_rejects_malformed_snapshot() {
    use crate::peer_binding::{PeerAuthConfig, PeerCredential};
    let _guard = crate::runtime_test_guard();
    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr valid.
    let node = unsafe { TestNode::new(7, &bind_addr) };
    assert!(!node.as_ptr().is_null());
    let mut config = PeerAuthConfig::default();
    config.local_route_slot = std::num::NonZeroU16::new(7);
    config.unverified_optout = true;
    config
        .pin_peer(42, PeerCredential::NoiseKey([0x11; 32]))
        .expect("distinct peer pin");
    let snapshot = config.snapshot();
    // SAFETY: node STOPPED.
    let set_rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), snapshot) };
    assert_eq!(set_rc, 0);
    // SAFETY: node valid; start must reject the malformed snapshot.
    assert_eq!(unsafe { hew_node_start(node.as_ptr()) }, -1);
    // SAFETY: node valid.
    unsafe {
        assert!((*node.as_ptr()).transport.is_null());
    }
}

/// `hew_node_set_auth_snapshot` is rejected once the node is not STOPPED.
#[test]
fn set_auth_snapshot_rejected_when_running() {
    let _guard = crate::runtime_test_guard();
    let (node, _port) = start_tcp_test_listener_node(55);
    // SAFETY: node is RUNNING; the setter must refuse.
    let rc = unsafe { hew_node_set_auth_snapshot(node.as_ptr(), PeerAuthSnapshot::unconfigured()) };
    assert_eq!(rc, -1, "installing a snapshot on a running node must fail");
    // SAFETY: node valid.
    unsafe {
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
}

#[test]
fn node_lifecycle_start_stop() {
    let _guard = crate::runtime_test_guard();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node = unsafe { TestNode::new(101, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    // SAFETY: node pointer is created in this test and valid until drop.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);
        assert_eq!(
            (&*node.as_ptr()).state.load(Ordering::Acquire),
            NODE_STATE_RUNNING
        );
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
        assert_eq!(
            (&*node.as_ptr()).state.load(Ordering::Acquire),
            NODE_STATE_STOPPED
        );
    }
}

#[test]
fn local_registry_register_and_lookup() {
    let _guard = crate::runtime_test_guard();

    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node = unsafe { TestNode::new(102, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    let actor_name = CString::new("hew-node-local-registry").expect("valid actor name");
    let missing_name = CString::new("hew-node-missing-registry").expect("valid actor name");
    let actor_id = (u64::from(102u16) << 48) | 0x1234;

    // SAFETY: node and C string pointers are valid for each call.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);
        assert_eq!(
            hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
            0
        );
        assert_eq!(
            lookup_exact(node.as_ptr(), actor_name.as_ptr()),
            local_actor_location(&*node.as_ptr(), actor_id)
        );
        assert_eq!(lookup_exact(node.as_ptr(), missing_name.as_ptr()), None);
        assert_eq!(
            crate::registry::hew_registry_unregister(actor_name.as_ptr()),
            0
        );
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }

    crate::registry::hew_registry_clear();
}

#[test]
fn actor_free_unregisters_named_actor_and_emits_gossip_remove() {
    let _guard = crate::runtime_test_guard();

    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node = unsafe { TestNode::new(103, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    let actor_name = CString::new("hew-node-actor-free-cleanup").expect("valid actor name");

    // SAFETY: pointers are valid for this scope.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);

        let actor = spawn_remote_test_actor(noop_dispatch);
        assert!(!actor.is_null());
        let actor_id = (*actor).id;
        assert_eq!(crate::pid::hew_pid_node(actor_id), 103);

        assert_eq!(
            hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
            0
        );
        assert_eq!(
            lookup_exact(node.as_ptr(), actor_name.as_ptr()),
            local_actor_location(&*node.as_ptr(), actor_id)
        );

        let n = &*node.as_ptr();
        assert!(!n.cluster.is_null());
        let cluster = &*n.cluster;
        let _ = cluster.take_registry_gossip(10);

        assert_eq!(crate::actor::hew_actor_free(actor), 0);
        assert_eq!(lookup_exact(node.as_ptr(), actor_name.as_ptr()), None);
        assert!(crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

        let events = cluster.take_registry_gossip(10);
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].name, "hew-node-actor-free-cleanup");
        assert!(!events[0].is_add);

        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }

    crate::registry::hew_registry_clear();
}

#[test]
fn node_stop_unregisters_local_names() {
    let _guard = crate::runtime_test_guard();

    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string for the duration of this test.
    let node = unsafe { TestNode::new(104, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    let actor_name = CString::new("hew-node-stop-cleanup").expect("valid actor name");

    // SAFETY: pointers are valid for this scope.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);

        let actor = spawn_remote_test_actor(noop_dispatch);
        assert!(!actor.is_null());
        let actor_id = (*actor).id;
        assert_eq!(crate::pid::hew_pid_node(actor_id), 104);

        assert_eq!(
            hew_node_register(node.as_ptr(), actor_name.as_ptr(), actor_id),
            0
        );
        assert_eq!(
            lookup_exact(node.as_ptr(), actor_name.as_ptr()),
            local_actor_location(&*node.as_ptr(), actor_id)
        );
        assert!(!crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

        assert_eq!(hew_node_stop(node.as_ptr()), 0);
        assert_eq!(lookup_exact(node.as_ptr(), actor_name.as_ptr()), None);
        assert!(crate::registry::hew_registry_lookup(actor_name.as_ptr()).is_null());

        assert_eq!(crate::actor::hew_actor_free(actor), 0);
    }

    crate::registry::hew_registry_clear();
}

#[cfg(feature = "encryption")]
#[test]
fn two_node_connect_and_handshake() {
    let _guard = crate::runtime_test_guard();

    crate::registry::hew_registry_clear();

    let (node1, _node1_port, node2, node2_port) = start_authorized_tcp_pair(201, 202);

    let connect_addr =
        CString::new(format!("202@127.0.0.1:{node2_port}")).expect("valid connect addr");
    // SAFETY: node pointer and connect_addr are valid for this call.
    unsafe { connect_with_retry(node1.as_ptr(), &connect_addr) };

    let actor_name = CString::new("hew-node-remote-actor").expect("valid actor name");
    let actor_id = (u64::from(202u16) << 48) | 0x63;
    // SAFETY: pointers are valid in this scope.
    unsafe {
        assert_eq!(
            hew_node_register(node2.as_ptr(), actor_name.as_ptr(), actor_id),
            0
        );
        assert_eq!(
            lookup_exact(node2.as_ptr(), actor_name.as_ptr()),
            local_actor_location(&*node2.as_ptr(), actor_id)
        );
    }

    // SAFETY: node pointers are valid while the test owns both nodes.
    unsafe { wait_for_handshake(node1.as_ptr(), node2.as_ptr()) };

    // SAFETY: pointers remain valid until dropped.
    unsafe {
        let _ = crate::registry::hew_registry_unregister(actor_name.as_ptr());
        assert_eq!(hew_node_stop(node1.as_ptr()), 0);
        assert_eq!(hew_node_stop(node2.as_ptr()), 0);
    }

    crate::registry::hew_registry_clear();
}

#[test]
fn exact_identity_prunes_remote_names_for_departed_node() {
    let _guard = crate::runtime_test_guard();

    let registry = HewRegistry::default();
    {
        let mut map = registry.remote_names.lock_or_recover();
        map.insert("remote-a".to_owned(), test_location(202, 0x10));
        map.insert("remote-b".to_owned(), test_location(202, 0x11));
        map.insert("other-node".to_owned(), test_location(303, 0x20));
    }

    let _ = take_registry_names_if(&registry, |location| location.node() == test_node_id(202));

    let map = registry.remote_names.lock_or_recover();
    assert!(!map.contains_key("remote-a"));
    assert!(!map.contains_key("remote-b"));
    assert_eq!(
        map.get("other-node").copied(),
        Some(test_location(303, 0x20))
    );
}

#[test]
fn test_node_unregister() {
    // `hew_node_new` records the node in the runtime-owned node slot, which
    // resolves through `rt_current()` and fails closed when no runtime is
    // installed; the guard installs the worker-less default runtime.
    let _guard = crate::runtime_test_guard();

    // SAFETY: bind_addr is a valid NUL-terminated C string literal.
    let node = unsafe { hew_node_new(50, c"127.0.0.1:0".as_ptr()) };
    assert!(!node.is_null());
    // SAFETY: test owns this stopped node.
    unsafe { install_test_auth(node, 50) };
    let name = c"test_unreg_actor";

    // SAFETY: node is a valid pointer; name is a valid C string literal.
    unsafe {
        assert_eq!(hew_node_register(node, name.as_ptr(), 999), 0);
        let mut found = HewRemotePid::default();
        assert_eq!(
            hew_node_lookup_location(node, name.as_ptr(), &raw mut found),
            0
        );
        assert_eq!(
            Location::try_from(found).unwrap(),
            local_actor_location(&*node, 999).unwrap()
        );
    }

    // SAFETY: node is a valid pointer; name is a valid C string literal.
    unsafe {
        assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
        assert_eq!(lookup_exact(node, name.as_ptr()), None);
    }

    // Idempotent
    // SAFETY: node is a valid pointer; name is a valid C string literal.
    unsafe {
        assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
    }

    // Null safety
    // SAFETY: Testing null pointer handling; function returns -1.
    unsafe {
        assert_eq!(hew_node_unregister(std::ptr::null_mut(), name.as_ptr()), -1);
        assert_eq!(hew_node_unregister(node, std::ptr::null()), -1);
    }

    // SAFETY: node was allocated by hew_node_new above.
    unsafe { hew_node_free(node) };
}

#[test]
fn register_same_name_repoints_future_lookup() {
    // `hew_node_new` records the node in the runtime-owned node slot, which
    // resolves through `rt_current()` and fails closed when no runtime is
    // installed; the guard installs the worker-less default runtime.
    let _guard = crate::runtime_test_guard();

    // SAFETY: bind_addr is a valid NUL-terminated C string literal.
    let node = unsafe { hew_node_new(51, c"127.0.0.1:0".as_ptr()) };
    assert!(!node.is_null());
    // SAFETY: test owns this stopped node.
    unsafe { install_test_auth(node, 51) };
    let name = c"l18_primary";

    // SAFETY: node is a valid pointer; name is a valid C string literal.
    unsafe {
        assert_eq!(hew_node_register(node, name.as_ptr(), 7001), 0);
        assert_eq!(
            hew_node_register(node, name.as_ptr(), 7001),
            0,
            "registering the same location is idempotent"
        );
        assert_eq!(hew_node_register(node, name.as_ptr(), 7002), 0);
        let mut found = HewRemotePid::default();
        assert_eq!(
            hew_node_lookup_location(node, name.as_ptr(), &raw mut found),
            0
        );
        assert_eq!(
            Location::try_from(found).unwrap(),
            local_actor_location(&*node, 7002).unwrap()
        );
    }

    // Distinct string still registers fine.
    let other = c"l18_secondary";
    // SAFETY: node is a valid pointer; name is a valid C string literal.
    unsafe {
        assert_eq!(hew_node_register(node, other.as_ptr(), 7002), 0);
        let _ = crate::registry::hew_registry_unregister(name.as_ptr());
        let _ = crate::registry::hew_registry_unregister(other.as_ptr());
        hew_node_free(node);
    }
}

#[test]
fn lookup_location_writes_output_only_on_success() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: bind address is a valid C string for the duration of the test.
    let node = unsafe { hew_node_new(52, c"127.0.0.1:0".as_ptr()) };
    assert!(!node.is_null());
    // SAFETY: test owns this stopped node.
    unsafe { install_test_auth(node, 52) };
    let missing = c"missing_exact_location";
    let sentinel = HewRemotePid::from(test_location(99, 777));
    let mut found = sentinel;

    // SAFETY: node, name, and output pointers are valid.
    unsafe {
        assert_eq!(
            hew_node_lookup_location(node, missing.as_ptr(), &raw mut found),
            -1
        );
        assert_eq!(found, sentinel);
        hew_node_free(node);
    }
}

#[test]
fn unregister_keeps_issued_location_live_until_actor_death() {
    let _guard = crate::runtime_test_guard();

    // SAFETY: bind address is a valid C string for the duration of the test.
    let node = unsafe { hew_node_new(53, c"127.0.0.1:0".as_ptr()) };
    assert!(!node.is_null());
    // SAFETY: test owns this stopped node.
    unsafe {
        install_test_auth(node, 53);
        assert_eq!(hew_node_start(node), 0);
        let actor = spawn_remote_test_actor(noop_dispatch);
        assert!(!actor.is_null());
        let name = c"issued_location_lifetime";
        assert_eq!(hew_node_register(node, name.as_ptr(), (*actor).id), 0);
        let mut target = HewRemotePid::default();
        assert_eq!(
            hew_node_lookup_location(node, name.as_ptr(), &raw mut target),
            0
        );
        assert_eq!(hew_node_unregister(node, name.as_ptr()), 0);
        let exact = Location::try_from(target).unwrap();
        assert_eq!(
            routing::hew_routing_lookup_location((*node).routing_table, exact),
            routing::LocationRoute::Local {
                actor_id: (*actor).id
            }
        );
        assert_eq!(
            crate::lifetime::live_actors::get_actor_ptr_by_id((*actor).id),
            Some(actor)
        );
        crate::actor::hew_actor_close(actor);
        assert_eq!(crate::actor::hew_actor_free(actor), 0);
        assert_eq!(
            hew_node_send_location(node, &raw const target, test_dispatch(), 1, ptr::null(), 0,),
            HEW_ERR_STALE_REF
        );
        assert_eq!(hew_node_stop(node), 0);
        hew_node_free(node);
    }
}

#[test]
fn gossip_repoint_updates_future_lookup_and_old_remove_does_not_erase_it() {
    let _guard = crate::runtime_test_guard();

    let registry = HewRegistry::default();
    let old = HewLocation::from(test_location(90, 9001));
    let new = HewLocation::from(test_location(90, 9002));
    registry
        .remote_names
        .lock_or_recover()
        .insert("l18_gossip".to_owned(), Location::try_from(old).unwrap());
    let user_data = (&raw const registry).cast_mut().cast::<c_void>();
    let name = c"l18_gossip";

    node_registry_gossip_callback(name.as_ptr(), &raw const new, true, user_data);
    let map = registry.remote_names.lock_or_recover();
    assert_eq!(
        map.get("l18_gossip").copied(),
        Some(Location::try_from(new).unwrap()),
        "new registration must re-point future lookup"
    );
    drop(map);
    node_registry_gossip_callback(name.as_ptr(), &raw const old, false, user_data);
    assert_eq!(
        registry
            .remote_names
            .lock_or_recover()
            .get("l18_gossip")
            .copied(),
        Some(Location::try_from(new).unwrap())
    );
}

#[test]
fn remote_lookup_via_registry_gossip() {
    // Verify that a registry gossip callback populates remote_names
    // and that hew_node_lookup falls through to it.
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string.
    let node = unsafe { TestNode::new(110, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    // SAFETY: pointer is valid for each call in this scope.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);

        // Simulate a remote registry gossip event arriving.
        let n = &*node.as_ptr();
        let remote_name = c"remote_counter";
        let remote_location = HewLocation::from(test_location(200, 0x99));

        // Invoke the callback directly (as the cluster would).
        node_registry_gossip_callback(
            remote_name.as_ptr(),
            &raw const remote_location,
            true,
            n.registry.cast::<c_void>(),
        );

        // Local lookup should not find it (not registered locally).
        assert!(crate::registry::hew_registry_lookup(remote_name.as_ptr()).is_null());

        // Node lookup should find it via remote_names.
        let mut found = HewRemotePid::default();
        assert_eq!(
            hew_node_lookup_location(node.as_ptr(), remote_name.as_ptr(), &raw mut found,),
            0
        );
        assert_eq!(
            Location::try_from(found).unwrap(),
            Location::try_from(remote_location).unwrap()
        );

        // Simulate removal.
        node_registry_gossip_callback(
            remote_name.as_ptr(),
            &raw const remote_location,
            false,
            n.registry.cast::<c_void>(),
        );
        assert_eq!(lookup_exact(node.as_ptr(), remote_name.as_ptr()), None);

        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}

#[test]
fn register_emits_gossip_event() {
    // Verify that hew_node_register queues a gossip event in the cluster.
    let _guard = crate::runtime_test_guard();
    crate::registry::hew_registry_clear();

    let bind_addr = CString::new("127.0.0.1:0").expect("valid bind addr");
    // SAFETY: bind_addr is a valid C string.
    let node = unsafe { TestNode::new(111, &bind_addr) };
    assert!(!node.as_ptr().is_null());

    // SAFETY: pointer is valid for each call in this scope.
    unsafe {
        assert_eq!(hew_node_start(node.as_ptr()), 0);

        let name = c"gossip_actor";
        let pid: u64 = (u64::from(111u16) << 48) | 0x2A;
        assert_eq!(hew_node_register(node.as_ptr(), name.as_ptr(), pid), 0);

        // The cluster should have a pending registry gossip event.
        let n = &*node.as_ptr();
        assert!(!n.cluster.is_null());
        assert!(cluster::hew_cluster_registry_gossip_count(n.cluster) > 0);

        let _ = crate::registry::hew_registry_unregister(name.as_ptr());
        assert_eq!(hew_node_stop(node.as_ptr()), 0);
    }
    crate::registry::hew_registry_clear();
}
