//! Distributed partition probe tests.

use super::*;

/// `dist_partition_drain_pending_remote_asks` — the drain seam the gated
/// `hew_dist_partition_pending_remote_asks` probe wraps and the two-process
/// partition fixture's `PartitionInjector` drives — fails EVERY pending
/// remote ask closed with `Partition` through the production reply-table
/// fan-out and returns the count. This is the in-process proof that a stuck
/// pending ask resolves to a typed `Partition` verdict on demand, without
/// waiting on the socket-teardown detector or the SWIM failure detector (the
/// host-load-sensitive timing the fixture used to race).
#[test]
fn dist_partition_probe_fails_pending_remote_asks_with_partition() {
    let _guard = crate::runtime_test_guard();

    // Nothing pending yet: the drain reports 0 so the injector keeps polling
    // rather than declaring victory before the ask registers.
    assert_eq!(
        dist_partition_drain_pending_remote_asks(),
        0,
        "drain must report 0 when no ask is pending"
    );

    let key = ConnectionKey {
        conn_mgr: 91,
        conn_id: 17,
    };
    let (id, pending) = reply_table().register(key);

    // The instant an ask is pending, one call fails it closed and reports it.
    assert_eq!(
        dist_partition_drain_pending_remote_asks(),
        1,
        "drain must fail exactly the one pending ask"
    );

    let guard = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let outcome = guard
        .as_ref()
        .expect("outcome should be set after the partition probe");
    assert_eq!(outcome.status, ReplyStatus::Failed);
    assert_eq!(
        outcome.ask_error,
        AskError::Partition,
        "probe must carry the Partition cause (14), not ConnectionDropped or Timeout"
    );
    drop(guard);

    // The entry is drained, so a racing reply finds nothing and a repeat
    // drain is a no-op — exactly-once, matching the SWIM-DEAD fan-out.
    let map = reply_table()
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(!map.contains_key(&id), "drain must remove the entry");
    drop(map);
    assert_eq!(
        dist_partition_drain_pending_remote_asks(),
        0,
        "a repeat drain with nothing pending is a no-op"
    );
}

/// The exported FFI probe is INERT without the test-capability signal. With
/// `HEW_DIST_TEST_PROBE` unset (the production default) and one remote ask
/// pending, a call to `hew_dist_partition_pending_remote_asks` returns the
/// inert `-1` and leaves the ask pending. This is the guard that a shipped
/// libhew exposes no usable "drain all pending remote asks" capability: the
/// raw symbol resolves to a no-op unless the harness arms it on the process
/// it spawns.
#[test]
fn dist_partition_probe_is_inert_without_test_signal() {
    let _guard = crate::runtime_test_guard();

    // The harness arms the probe per-process via `HEW_DIST_TEST_PROBE=1`; the
    // unit-test process never sets it, so the gate must read disarmed.
    assert!(
        !dist_test_probe_enabled(),
        "HEW_DIST_TEST_PROBE must be unset in the unit-test process"
    );

    let key = ConnectionKey {
        conn_mgr: 73,
        conn_id: 5,
    };
    let (id, pending) = reply_table().register(key);

    // Disarmed: the exported symbol is a no-op that drains nothing.
    assert_eq!(
        hew_dist_partition_pending_remote_asks(),
        -1,
        "the probe must be inert (-1) without the test signal"
    );

    // The pending ask is untouched — no Partition forced on a healthy slot.
    let outcome_unset = pending
        .outcome
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .is_none();
    assert!(
        outcome_unset,
        "an inert probe must not resolve the pending ask"
    );
    let map = reply_table()
        .pending
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    assert!(
        map.contains_key(&id),
        "an inert probe must leave the pending entry registered"
    );
}
