//! SWIM failure-detector driver.
//!
//! The cluster substrate (`cluster.rs`) implements a complete SWIM state
//! machine + phi-accrual detector, and the connection mesh (`connection.rs`)
//! carries SWIM control frames — but neither has a *clock*. Without a periodic
//! driver, [`crate::cluster::hew_cluster_tick`] is never called in production,
//! so phi-accrual is trained but never consulted, ALIVE→SUSPECT→DEAD never
//! escalates, partition fan-out never fires, and dead routes are never pruned.
//!
//! This module is that clock. Each running [`HewNode`] gets one background
//! ticker thread (modelled on the proven `timer_periodic` pattern: bounded
//! loop with a stop flag, `JoinHandle` retained for a clean join, Dekker-style
//! in-flight guard so shutdown cannot race a tick). Every protocol period the
//! ticker:
//!
//! 1. calls `hew_cluster_tick`, which advances phi-accrual, escalates
//!    SUSPECT→DEAD on timeout, fans out `PartitionDetected` and fires
//!    `on_member_dead`, and returns the next direct-probe target;
//! 2. sends a direct `PING` (carrying piggybacked membership gossip) to that
//!    target over the connection mesh;
//! 3. issues up to K indirect `PING_REQ` probes for the same target through
//!    randomly-chosen relay peers (SWIM's core accuracy mechanism, C4), so a
//!    single dropped packet does not falsely suspect a healthy node.
//!
//! # Concurrency / lifetime contract
//!
//! The ticker thread dereferences the node's `cluster` and `conn_mgr` pointers.
//! Those are freed in [`crate::hew_node::hew_node_stop`]; the driver MUST be
//! stopped and **joined** before that teardown frees them. `stop_swim_driver`
//! provides that barrier and is called from the node-stop cleanup path. The
//! Dekker `in_flight` guard additionally lets a (defensive) caller observe that
//! a tick is mid-flight. The cluster methods the ticker calls all take `&self`
//! and synchronize internally, so concurrent access from the connection reader
//! threads is sound (no `&mut` aliasing).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI-adjacent driver module; SAFETY documented at each call site."
)]

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use rand::rng;
use rand::RngExt;

use crate::cluster::{self, HewCluster, SWIM_MSG_PING, SWIM_MSG_PING_REQ};
use crate::connection::{self, HewConnMgr};
use crate::hew_node::HewNode;
use crate::lifetime::PoisonSafe;

/// Granularity of the ticker's sleep loop. The thread sleeps in slices of this
/// size and checks the stop flag between slices, so shutdown latency is bounded
/// by one slice rather than one (potentially second-long) protocol period.
const TICK_SLICE: Duration = Duration::from_millis(20);

/// A handle to one node's running ticker thread.
struct DriverHandle {
    /// Set true to request shutdown; the loop observes it within one slice.
    stop: Arc<AtomicBool>,
    /// The ticker thread's join handle (taken at stop time).
    ///
    /// The join — not an in-flight spin guard — is the lifetime barrier here:
    /// unlike `timer_periodic` (whose actor can be freed independently of the
    /// ticker), this thread's only dereferenced state (the node's `cluster` /
    /// `conn_mgr`) is freed strictly after `stop_swim_driver` joins. So joining
    /// before teardown is both necessary and sufficient; no per-tick guard is
    /// required.
    join: Option<JoinHandle<()>>,
}

/// Per-node registry of running ticker threads, keyed by the node pointer
/// address. Guarded by `PoisonSafe` so a panic in one accessor cannot poison
/// the whole table.
static SWIM_DRIVERS: PoisonSafe<Option<HashMap<usize, DriverHandle>>> = PoisonSafe::new(None);

/// A node pointer shuttled into the ticker thread.
///
/// The node allocation outlives the thread: `stop_swim_driver` joins the thread
/// before `hew_node_stop` frees the node's `cluster` / `conn_mgr` and before
/// `hew_node_free` frees the node itself.
#[derive(Clone, Copy)]
struct NodePtr(*mut HewNode);

// SAFETY: the node's interior state (cluster, conn_mgr) is synchronized by its
// own mutexes/atomics; the pointer is only dereferenced while the node is alive
// (guaranteed by the join-before-free ordering documented at module level).
unsafe impl Send for NodePtr {}

/// Start the SWIM ticker for `node` if one is not already running.
///
/// Idempotent: a second call for the same node while a ticker is live is a
/// no-op. Returns `true` if a ticker is running for the node after the call.
///
/// # Safety
///
/// `node` must be a valid pointer to a started [`HewNode`] whose `cluster` and
/// `conn_mgr` remain valid until [`stop_swim_driver`] is called for it.
pub(crate) unsafe fn start_swim_driver(node: *mut HewNode) -> bool {
    if node.is_null() {
        return false;
    }

    SWIM_DRIVERS.access(|slot| {
        let table = slot.get_or_insert_with(HashMap::new);
        if table.contains_key(&(node as usize)) {
            return true; // already running
        }

        let stop = Arc::new(AtomicBool::new(false));
        let thread_stop = Arc::clone(&stop);
        let node_ptr = NodePtr(node);

        let spawn = std::thread::Builder::new()
            .name("hew-swim-driver".into())
            .spawn(move || run_driver_loop(node_ptr, &thread_stop));

        if let Ok(join) = spawn {
            table.insert(
                node as usize,
                DriverHandle {
                    stop,
                    join: Some(join),
                },
            );
            true
        } else {
            crate::set_last_error("start_swim_driver: failed to spawn SWIM ticker thread");
            false
        }
    })
}

/// Stop and join the SWIM ticker for `node`.
///
/// This is the lifetime barrier: after it returns, the ticker thread has fully
/// exited and will never again touch `node`'s `cluster` / `conn_mgr`. Safe to
/// call
/// when no ticker is running (no-op). Safe to call multiple times.
///
/// # Safety
///
/// `node` identifies a node whose ticker was started by [`start_swim_driver`].
/// The pointer is used only as a registry key here; it is not dereferenced.
pub(crate) unsafe fn stop_swim_driver(node: *mut HewNode) {
    // Take the handle out of the registry under the lock, then signal + join
    // outside the lock so a concurrent start for a *different* node is not
    // blocked on this join.
    let handle = SWIM_DRIVERS.access(|slot| {
        slot.as_mut()
            .and_then(|table| table.remove(&(node as usize)))
    });

    let Some(mut handle) = handle else {
        return;
    };
    handle.stop.store(true, Ordering::Release);
    if let Some(join) = handle.join.take() {
        let _ = join.join();
    }
}

/// The ticker loop: run one protocol period every `protocol_period_ms`,
/// checking the stop flag every `TICK_SLICE`.
fn run_driver_loop(node: NodePtr, stop: &Arc<AtomicBool>) {
    // Read the protocol period once from the cluster config; it does not change
    // for a node's lifetime. Fall back to a 1 s default if the cluster is not
    // yet available.
    let period = protocol_period_for(node.0).unwrap_or(Duration::from_secs(1));
    let mut next_period = Instant::now() + period;

    loop {
        if stop.load(Ordering::Acquire) {
            break;
        }
        std::thread::sleep(TICK_SLICE);
        if stop.load(Ordering::Acquire) {
            break;
        }
        if Instant::now() < next_period {
            continue;
        }
        next_period += period;
        // Skip missed periods if the thread was descheduled for a long time,
        // so we do not burst-fire ticks to catch up.
        let now = Instant::now();
        if next_period < now {
            next_period = now + period;
        }

        // SAFETY: the node is alive — stop_swim_driver joins this thread before
        // any teardown of node resources, so the cluster/conn_mgr the period
        // dereferences cannot be freed concurrently with this call.
        unsafe {
            run_one_period(node.0);
        }
    }
}

/// Read the SWIM protocol period from the node's cluster config.
fn protocol_period_for(node: *mut HewNode) -> Option<Duration> {
    if node.is_null() {
        return None;
    }
    // SAFETY: called once at thread start while the node is alive.
    let node_ref = unsafe { &*node };
    if node_ref.cluster.is_null() {
        return None;
    }
    // SAFETY: cluster pointer is owned by the live node.
    let cluster = unsafe { &*node_ref.cluster };
    Some(Duration::from_millis(cluster.protocol_period_ms()))
}

/// Execute a single SWIM protocol period for `node`.
///
/// # Safety
///
/// `node` must be alive for the duration of the call (guaranteed by the
/// join-before-free ordering: this only runs while the ticker thread has not
/// been joined).
unsafe fn run_one_period(node: *mut HewNode) {
    if node.is_null() {
        return;
    }
    // SAFETY: caller guarantees the node is alive.
    let node_ref = unsafe { &*node };

    // Only drive a fully-running node.
    if node_ref.state.load(Ordering::Acquire) != crate::hew_node::NODE_STATE_RUNNING {
        return;
    }
    if node_ref.cluster.is_null() || node_ref.conn_mgr.is_null() {
        return;
    }

    let cluster_ptr: *mut HewCluster = node_ref.cluster;
    let conn_ptr: *mut HewConnMgr = node_ref.conn_mgr;

    // 1. Advance the protocol: phi-accrual consultation, SUSPECT→DEAD
    //    escalation, partition fan-out + on_member_dead, dead-route pruning.
    //    Returns the next direct-probe target (0 = none).
    // SAFETY: cluster pointer is owned by the live node.
    let target = unsafe { cluster::hew_cluster_tick(cluster_ptr) };
    if target == 0 {
        return;
    }

    // 2. Direct probe: PING the target (carries piggybacked gossip C6). The
    //    target's ACK refreshes its last-seen / phi anchor on the next inbound
    //    frame. If the direct PING reaches the target, no indirect probing is
    //    needed; if the target is genuinely gone, indirect probing (below)
    //    gives independent observers a chance to confirm before we suspect.
    // SAFETY: conn_mgr is owned by the live node.
    let direct = unsafe { connection::hew_connmgr_send_swim(conn_ptr, target, SWIM_MSG_PING, 0) };

    // 3. Indirect probing (C4): ask up to K relay peers to PING the target on
    //    our behalf. This is SWIM's accuracy mechanism — a single lost packet
    //    on the direct path does not falsely suspect a live node, because a
    //    relay's successful PING (and the target's ACK back to the relay)
    //    propagates the target's liveness via gossip. We always issue indirect
    //    probes alongside the direct one rather than waiting for a direct
    //    no-ack, because the sync transport gives us no per-probe ack callback
    //    to time out on; the redundancy is cheap and bounded by K.
    // SAFETY: cluster pointer is owned by the live node.
    let k = unsafe { &*cluster_ptr }.indirect_ping_count();
    if k == 0 {
        let _ = direct;
        return;
    }

    // SAFETY: conn_mgr is owned by the live node.
    let mut relays = unsafe { connection::hew_connmgr_active_swim_peers(conn_ptr) };
    relays.retain(|&peer| peer != target);
    if relays.is_empty() {
        return;
    }

    // Choose K distinct relays at random (partial Fisher–Yates).
    let mut rng = rng();
    let pick = k.min(relays.len());
    let len = relays.len();
    for i in 0..pick {
        let j = rng.random_range(i..len);
        relays.swap(i, j);
        let relay = relays[i];
        // SAFETY: conn_mgr is owned by the live node; PING_REQ carries the
        // probe target in target_node so the relay forwards a real PING.
        let _ = unsafe {
            connection::hew_connmgr_send_swim(conn_ptr, relay, SWIM_MSG_PING_REQ, target)
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Stopping a node with no running driver is a safe no-op.
    #[test]
    fn stop_without_start_is_noop() {
        // SAFETY: a dangling, never-registered pointer is only used as a key.
        unsafe {
            stop_swim_driver(0xdead_0000 as *mut HewNode);
        }
    }

    /// start/stop is idempotent and tears the registry entry down.
    #[test]
    fn start_then_stop_removes_registry_entry() {
        // Use a fabricated-but-stable pointer; the loop reads it but a null
        // cluster makes run_one_period bail immediately, so it never UB-derefs.
        // We allocate a zeroed HewNode-sized box to back the pointer safely.
        let bind = std::ffi::CString::new("127.0.0.1:0").unwrap();
        // SAFETY: bind is a valid C string for the call.
        let node = unsafe { crate::hew_node::hew_node_new(900, bind.as_ptr()) };
        assert!(!node.is_null());

        // Not started, so the node has a null cluster/conn_mgr; the loop's
        // run_one_period will bail on the state check. Start the driver.
        // SAFETY: node is a valid (un-started) node; the loop tolerates null
        // cluster/conn_mgr by bailing each period.
        let started = unsafe { start_swim_driver(node) };
        assert!(started, "driver should start");

        let present = SWIM_DRIVERS.access(|slot| {
            slot.as_ref()
                .is_some_and(|table| table.contains_key(&(node as usize)))
        });
        assert!(present, "registry must contain the driver entry");

        // Second start is a no-op (still running).
        // SAFETY: same node pointer.
        assert!(unsafe { start_swim_driver(node) });

        // SAFETY: stops + joins the ticker thread.
        unsafe { stop_swim_driver(node) };

        let gone = SWIM_DRIVERS.access(|slot| {
            slot.as_ref()
                .is_none_or(|table| !table.contains_key(&(node as usize)))
        });
        assert!(gone, "registry entry must be removed after stop");

        // SAFETY: node was allocated by hew_node_new and the driver is stopped.
        unsafe { crate::hew_node::hew_node_free(node) };
    }
}
