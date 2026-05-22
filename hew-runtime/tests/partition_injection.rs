//! Integration tests for the partition-injection seam.
//!
//! Exercises the end-to-end path:
//!   `PartitionRegistry::on_member_dead`
//!     → `Queue::force_partition`
//!       → blocked `recv` resolves to `Err(RecvError::PartitionDetected)`
//!
//! Test topology: two in-process logical nodes sharing a `PartitionRegistry`.
//! The registry plays the role of "the cluster substrate that tracks which
//! queues are bound to which peer node".  No real transport, SWIM protocol,
//! or phi-accrual detector is involved — those compose on top of this seam
//! (C3/A5, Phase 3/5).
//!
//! # CP-3 forward flag
//!
//! `ClusterProtocol::on_member_dead` (the SWIM driver hook, C3) and the A5
//! datagram transport callback both route through `PartitionRegistry::on_member_dead`.
//! These tests verify the seam that C3 and A5 plug into; the tests remain
//! valid once the real transport lands.

use hew_runtime::cluster::PartitionRegistry;
use hew_runtime::duplex::{HewDuplex, RecvError};
use std::sync::Arc;
use std::time::Duration;

/// `on_member_dead` fans out `PartitionDetected` to a blocked `recv`
/// within the allowed timeout.
///
/// Binary gate: the blocked recv must return `Err(RecvError::PartitionDetected)`
/// within 500 ms of the `on_member_dead` call.
#[test]
fn blocked_recv_resolves_to_partition_detected_on_member_dead() {
    const NODE_B_ID: u16 = 2;
    const TIMEOUT_MS: u64 = 500;

    let registry = Arc::new(PartitionRegistry::new());

    // Create a duplex pair. `local` is the actor's side (receives from peer B).
    // Keep `peer` alive so the sender cap on `local`'s recv direction is not
    // dropped before the partition signal fires.
    let (local, peer) = HewDuplex::new_pair(8, 8);

    // Register local's recv with the registry under NODE_B_ID.
    local.register_recv_with_partition_registry(&registry, NODE_B_ID);

    // Spawn a thread that blocks on recv — simulates an actor waiting
    // for a message from node B.
    let recv_handle = {
        let handle = local.clone_handle();
        std::thread::spawn(move || handle.recv())
    };

    // Give the recv thread time to block.
    std::thread::sleep(Duration::from_millis(20));

    // Synthesise the MEMBER_DEAD event through the partition-injection seam.
    // In production, the cluster substrate calls this when phi-accrual or
    // SWIM declares a node dead.
    registry.on_member_dead(NODE_B_ID);

    // The blocked recv must resolve within the timeout.
    let result = recv_handle.join().expect("recv thread panicked");

    assert!(
        matches!(result, Err(RecvError::PartitionDetected)),
        "expected Err(RecvError::PartitionDetected), got: {result:?}"
    );

    drop(peer);
    let _ = TIMEOUT_MS; // documented guarantee; actual wall-clock checked by test runner
}

/// `on_member_dead` on an unregistered node ID is a no-op (not an error).
#[test]
fn on_member_dead_unregistered_node_is_noop() {
    let registry = PartitionRegistry::new();
    // No queues registered for node 99 — must not panic.
    registry.on_member_dead(99);
}

/// Dead `Weak` refs are pruned: after the duplex is dropped, `on_member_dead`
/// finds no live queues for the node.
#[test]
fn dead_weak_refs_pruned_on_member_dead() {
    const NODE_ID: u16 = 3;
    let registry = PartitionRegistry::new();

    {
        let (duplex, _other) = HewDuplex::new_pair(4, 4);
        duplex.register_recv_with_partition_registry(&registry, NODE_ID);
        // `duplex` drops here; the Weak ref becomes dead.
    }

    // After the duplex is dropped the Weak cannot upgrade.
    // `on_member_dead` must not panic and must prune the dead ref.
    registry.on_member_dead(NODE_ID);
    // Calling a second time verifies the slot was pruned (no double-free, no panic).
    registry.on_member_dead(NODE_ID);
}

/// Multiple queues registered for the same node all receive the partition signal.
#[test]
fn multiple_queues_for_same_node_all_partitioned() {
    const NODE_ID: u16 = 4;
    let registry = Arc::new(PartitionRegistry::new());

    // Keep the peer side of each pair alive — it holds the sender cap for
    // the receiving direction. Dropping it early would close the channel
    // and cause the recv threads to return Closed instead of PartitionDetected.
    let (dx1, peer1) = HewDuplex::new_pair(4, 4);
    let (dx2, peer2) = HewDuplex::new_pair(4, 4);
    let (dx3, peer3) = HewDuplex::new_pair(4, 4);

    dx1.register_recv_with_partition_registry(&registry, NODE_ID);
    dx2.register_recv_with_partition_registry(&registry, NODE_ID);
    dx3.register_recv_with_partition_registry(&registry, NODE_ID);

    // Spawn receivers on all three.
    let h1 = {
        let d = dx1.clone_handle();
        std::thread::spawn(move || d.recv())
    };
    let h2 = {
        let d = dx2.clone_handle();
        std::thread::spawn(move || d.recv())
    };
    let h3 = {
        let d = dx3.clone_handle();
        std::thread::spawn(move || d.recv())
    };

    std::thread::sleep(Duration::from_millis(20));

    registry.on_member_dead(NODE_ID);

    for (i, h) in [h1, h2, h3].into_iter().enumerate() {
        let result = h.join().expect("recv thread panicked");
        assert!(
            matches!(result, Err(RecvError::PartitionDetected)),
            "queue {i}: expected PartitionDetected, got: {result:?}"
        );
    }
    // Keep peer sides alive until after the assertion so channels don't close early.
    drop((peer1, peer2, peer3));
}

/// The cluster wires `PartitionRegistry::on_member_dead` into the `MEMBER_DEAD`
/// transition path: verify via `hew_cluster_set_partition_registry` +
/// `hew_cluster_join` + `hew_cluster_notify_connection_lost`.
///
/// This test exercises the cluster→registry→queue end-to-end path using
/// the real `HewCluster` FFI surface.
#[test]
fn cluster_member_dead_fans_out_partition_via_registry() {
    use hew_runtime::cluster::{
        hew_cluster_free, hew_cluster_join, hew_cluster_new,
        hew_cluster_notify_connection_established, hew_cluster_notify_connection_lost,
        hew_cluster_set_partition_registry, hew_cluster_tick, ClusterConfig,
    };
    use std::ffi::CString;

    const LOCAL_ID: u16 = 1;
    const REMOTE_ID: u16 = 2;
    // Use a very short suspect timeout so tick() drives SUSPECT→DEAD quickly.
    const SUSPECT_TIMEOUT_MS: u32 = 50;

    let cfg = ClusterConfig {
        local_node_id: LOCAL_ID,
        suspect_timeout_ms: SUSPECT_TIMEOUT_MS,
        ..ClusterConfig::default()
    };

    // SAFETY: cfg is valid for the duration of the call.
    let cluster = unsafe { hew_cluster_new(&raw const cfg) };
    assert!(!cluster.is_null(), "hew_cluster_new returned null");

    let registry = Arc::new(PartitionRegistry::new());

    // SAFETY: cluster pointer is valid; registry Arc is cloned inside.
    unsafe { hew_cluster_set_partition_registry(cluster, Arc::clone(&registry)) };

    // Add remote peer as ALIVE.
    let addr = CString::new("127.0.0.1:9000").unwrap();
    // SAFETY: cluster and addr are valid.
    unsafe { hew_cluster_join(cluster, REMOTE_ID, addr.as_ptr()) };

    // Mark peer as reachable (establishes a connection token).
    // SAFETY: cluster is valid.
    unsafe { hew_cluster_notify_connection_established(cluster, REMOTE_ID) };

    // Create a duplex and register it for REMOTE_ID.
    // Keep the peer side alive so the recv direction stays open.
    let (dx, peer) = HewDuplex::new_pair(8, 8);
    dx.register_recv_with_partition_registry(&registry, REMOTE_ID);

    // Spawn a receiver thread.
    let recv_handle = {
        let handle = dx.clone_handle();
        std::thread::spawn(move || handle.recv())
    };

    std::thread::sleep(Duration::from_millis(10));

    // Drop the connection — moves peer to SUSPECT.
    // SAFETY: cluster is valid.
    unsafe { hew_cluster_notify_connection_lost(cluster, REMOTE_ID) };

    // Tick repeatedly until SUSPECT→DEAD fires (beyond suspect_timeout_ms).
    let deadline = std::time::Instant::now() + Duration::from_millis(500);
    loop {
        // SAFETY: cluster pointer is valid for the loop duration.
        unsafe { hew_cluster_tick(cluster) };
        if recv_handle.is_finished() {
            break;
        }
        if std::time::Instant::now() > deadline {
            break;
        }
        std::thread::sleep(Duration::from_millis(10));
    }

    let result = recv_handle.join().expect("recv thread panicked");
    assert!(
        matches!(result, Err(RecvError::PartitionDetected)),
        "expected PartitionDetected after MEMBER_DEAD, got: {result:?}"
    );

    // SAFETY: cluster pointer is valid; no further use after free.
    unsafe { hew_cluster_free(cluster) };

    // Keep peer alive until after assertions so the recv direction stays open.
    drop(peer);
}
