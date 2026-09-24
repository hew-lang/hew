//! Distributed test and observability probes.

use super::{
    reply_table_opt, with_current_node_read, AskError, HewNode, PendingReply, ReplyRoutingTable,
};
use std::sync::Arc;

/// Return the total number of `RemoteWatcher` entries across all serial slots
/// in the target-side monitor table.
///
/// Test-introspection probe used by the two-process watcher-node-death fixture
/// to assert the target-side table is empty after a watcher node dies — proving
/// the prune path fires and the table is bounded. Not user-callable; the
/// compiler does not emit calls to this symbol. Callable from `.hew` via
/// `extern "C" { fn hew_dist_monitor_remote_watcher_count() -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_dist_monitor_remote_watcher_count() -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return -1;
    };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "remote_watcher_count is bounded by the number of live monitors, which is \
                  far below i64::MAX in any realistic deployment; the sentinel -1 covers \
                  the no-runtime case"
    )]
    {
        rt.monitors.remote_watcher_count() as i64
    }
}

/// Return the cumulative number of `RemoteWatcher` registrations ever accepted
/// into the target-side monitor table — monotonic, process-local, never reset.
///
/// Test-introspection probe used by the two-process monitor fixtures to derive
/// "a registration happened" from the event counter instead of sampling the
/// transient live count. On hosts with a coarse timer tick (default Windows
/// ~15.6 ms) a sleep-poll sampler can miss every short-lived `count == 1`
/// window; the monotonic total cannot be missed. Not user-callable; the
/// compiler does not emit calls to this symbol. Callable from `.hew` via
/// `extern "C" { fn hew_dist_monitor_remote_watcher_registered_total() -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_dist_monitor_remote_watcher_registered_total() -> i64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return -1;
    };
    #[expect(
        clippy::cast_possible_wrap,
        reason = "the cumulative registration count grows by one per accepted remote \
                  monitor/link registration and stays far below i64::MAX in any realistic \
                  process lifetime; the sentinel -1 covers the no-runtime case"
    )]
    {
        rt.monitors.remote_watchers_registered_total() as i64
    }
}

/// Return the sole remote monitor id owned by the current actor, or zero when
/// the actor has none or more than one.
///
/// Test-introspection probe for compiled lifecycle fixtures. The value is the
/// same authoritative id stored in `MonitorRef` and delivered in
/// `DownNotification.monitor`; the compiler does not emit calls to this symbol.
#[no_mangle]
pub extern "C" fn hew_dist_monitor_current_actor_ref_id() -> u64 {
    let Some(rt) = crate::runtime::rt_current_opt() else {
        return 0;
    };
    let self_actor = crate::actor::hew_actor_self();
    if self_actor.is_null() {
        return 0;
    }
    // SAFETY: `hew_actor_self` returned the live current actor.
    let watcher_actor_id = unsafe { (*self_actor).id };
    rt.monitors
        .sole_remote_monitor_for_watcher(watcher_actor_id)
        .unwrap_or(0)
}

/// Return the watcher-side observation table size for lifecycle leak fixtures.
///
/// Test-introspection only; the compiler does not emit calls to this symbol.
#[no_mangle]
pub extern "C" fn hew_dist_monitor_pending_observation_count() -> u64 {
    crate::runtime::rt_current_opt().map_or(0, |rt| rt.monitors.pending_observation_count() as u64)
}

/// Drive a single piggybacked SWIM gossip entry into the current node's cluster,
/// exactly as a received gossip frame would.
///
/// Test-introspection probe: lets a two-process rejoin fixture deterministically
/// drive a DEAD verdict, a stale (`<=`-incarnation) ALIVE replay, and a
/// strictly-higher-incarnation rejoin into the membership admission gate without
/// racing the failure detector. Returns 0 on success, -1 when no node is
/// installed. Not user-callable; the compiler does not emit calls to this symbol.
/// Callable from `.hew` via
/// `extern "C" { fn hew_dist_inject_swim_gossip(node_id: u16, state: i32, incarnation: u64) -> i64; }`.
#[no_mangle]
pub extern "C" fn hew_dist_inject_swim_gossip(node_id: u16, state: i32, incarnation: u64) -> i64 {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return -1;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.cluster.is_null() {
            return -1;
        }
        // SAFETY: cluster is valid while the node is installed.
        let cluster = unsafe { &*node.cluster };
        let session = cluster.member_session(node_id).unwrap_or(1);
        cluster.apply_swim_gossip(&[(node_id, session, state, incarnation)]);
        0
    })
}

/// Return the membership state the current node's cluster records for `node_id`
/// (the `MEMBER_*` constants), or -1 if the node is unknown / no node installed.
///
/// Test-introspection probe for the rejoin fixture: asserts a buried node stays
/// DEAD under a stale-ALIVE replay and flips to ALIVE only after a strictly-higher
/// rejoin. Not user-callable.
#[no_mangle]
pub extern "C" fn hew_dist_member_state(node_id: u16) -> i64 {
    with_current_node_read(|guard| {
        let node_ptr = *guard as *const HewNode;
        if node_ptr.is_null() {
            return -1;
        }
        // SAFETY: the read lock pins CURRENT_NODE for the duration of this call.
        let node = unsafe { &*node_ptr };
        if node.cluster.is_null() {
            return -1;
        }
        // SAFETY: cluster is valid while the node is installed.
        let cluster = unsafe { &*node.cluster };
        i64::from(cluster.member_state(node_id))
    })
}

/// Return 1 if `node_id` is currently in the quarantine set, 0 if not, -1 when no
/// node is installed.
///
/// Test-introspection probe for the rejoin fixture: the resurrection-guard teeth.
/// A buried node must remain quarantined under a stale-ALIVE replay and be evicted
/// after a strictly-higher rejoin. Not user-callable.
#[no_mangle]
pub extern "C" fn hew_dist_quarantine_contains(node_id: u16) -> i64 {
    match crate::runtime::rt_current_opt() {
        Some(rt) => rt
            .node
            .quarantine
            .access(|set| i64::from(set.contains_key(&node_id))),
        None => -1,
    }
}

/// Environment signal that arms [`hew_dist_partition_pending_remote_asks`].
///
/// The probe is a test-only capability: it stays inert (a `-1` no-op that drains
/// nothing) unless this variable is present with value `1`. The two-process
/// partition fixture's harness sets it on the client it spawns; a shipped libhew
/// runs with it unset.
const DIST_TEST_PROBE_ENV: &str = "HEW_DIST_TEST_PROBE";

/// Whether the test-only partition probe is armed for this process.
///
/// True only when `HEW_DIST_TEST_PROBE=1` is present — the value the e2e harness
/// sets on the partition-scenario client. Any other value, or an unset variable
/// (the production default), leaves the probe inert.
pub(super) fn dist_test_probe_enabled() -> bool {
    std::env::var_os(DIST_TEST_PROBE_ENV).as_deref() == Some(std::ffi::OsStr::new("1"))
}

/// Fail every currently-pending remote ask CLOSED with [`AskError::Partition`],
/// returning the number of asks it resolved (`0` when none are pending yet, `-1`
/// when no runtime is installed).
///
/// The drain seam behind the test-only partition probe. Drives the in-flight
/// pending-ask fail-closed path on demand through the SAME reply-table seam the
/// SWIM-DEAD fan-out ([`fail_remote_asks_for_node`]) reaches via
/// `fail_connection_with_reason(.., Partition)` — instead of waiting on the OS
/// socket-teardown detector and the SWIM failure detector, whose latency is
/// unbounded under host load. It mirrors how the in-process test
/// `swim_dead_wakes_pending_remote_ask_with_partition` calls the fan-out directly
/// once the ask registers, letting the two-process partition fixture exercise the
/// typed fail-closed verdict deterministically rather than racing real time.
///
/// Draining under the table lock is atomic against a racing reply: an entry this
/// drain removes is guaranteed to wake with `Partition` (a late reply finds no
/// slot and is dropped), so the returned count is exactly the number of asks
/// failed closed. Reads and resolves reply-table slots only; it does NOT alter
/// the production partition/StaleRef decision path.
pub(super) fn dist_partition_drain_pending_remote_asks() -> i64 {
    let Some(table) = reply_table_opt() else {
        return -1;
    };
    // Drain every pending reply slot under the table lock — every entry here is a
    // cross-node remote ask — then fail each closed with Partition OUTSIDE the
    // lock, mirroring `fail_all`/`fail_connection_with_reason`, which never hold
    // the map lock across a waiter wake.
    let drained: Vec<Arc<PendingReply>> = {
        let mut map = table
            .pending
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        map.drain().map(|(_, pending)| pending).collect()
    };
    let failed = {
        #[expect(
            clippy::cast_possible_wrap,
            reason = "the count of pending remote asks is bounded by the live in-flight \
                      asks on this node, far below i64::MAX; the sentinel -1 covers the \
                      no-runtime case"
        )]
        let count = drained.len() as i64;
        count
    };
    for pending in drained {
        ReplyRoutingTable::fail_pending_with_reason(&pending, AskError::Partition);
    }
    failed
}

/// Test-introspection probe (FFI) for the two-process partition fixture: when
/// armed, fails every pending remote ask closed with [`AskError::Partition`] via
/// [`dist_partition_drain_pending_remote_asks`] and returns the count.
///
/// Gated as a test-only capability and INERT by default. Without
/// `HEW_DIST_TEST_PROBE=1` ([`dist_test_probe_enabled`]) it returns `-1` and
/// drains nothing. A shipped libhew exports this symbol but runs with the signal
/// unset, so a Hew program that declares `extern "C" fn
/// hew_dist_partition_pending_remote_asks()` and calls it gets the inert `-1` —
/// never a drain of healthy in-flight asks. The harness arms it only on the
/// partition-scenario client it spawns; the compiler never emits calls to it.
#[no_mangle]
pub extern "C" fn hew_dist_partition_pending_remote_asks() -> i64 {
    if !dist_test_probe_enabled() {
        return -1;
    }
    dist_partition_drain_pending_remote_asks()
}
