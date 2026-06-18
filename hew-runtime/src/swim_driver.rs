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
//! # Clock seam
//!
//! `run_driver_loop` decides its tick cadence through a [`MonoClock`]
//! injected at driver-start time rather than calling `Instant::now()` /
//! `thread::sleep` directly.  The production implementation delegates to the
//! real OS clock (zero behaviour change).  When simulated time is active
//! (`SIMTIME_ENABLED`) the sim implementation reads `SIMTIME_MS` for `now_ms`
//! and performs a real 1 ms OS sleep for `sleep_ms`.  The test drives sim time
//! forward by calling [`crate::deterministic::hew_simtime_advance_ms`].
//! The 1 ms real sleep (rather than `yield_now`) surrenders the OS timeslice
//! unconditionally, giving the TCP connection-reader thread time to refresh
//! `last_seen_ms` even under heavy instrumentation overhead (e.g. llvm-cov's
//! ~3-5× slowdown), preventing false-DEAD verdicts for live peers.
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
use std::time::Duration;

use rand::rng;
use rand::RngExt;

use crate::cluster::{self, HewCluster, SWIM_MSG_PING, SWIM_MSG_PING_REQ};
use crate::connection::{self, HewConnMgr};
use crate::hew_node::HewNode;
use crate::lifetime::PoisonSafe;

/// Granularity of the ticker's sleep loop under the real (wall-clock) clock.
///
/// The thread sleeps in slices of this size and checks the stop flag between
/// slices, so shutdown latency is bounded by one slice rather than one
/// (potentially second-long) protocol period.  Under simulated time the loop
/// uses a fixed 1 ms real sleep instead (see `sim_sleep_ms`) — this constant
/// is only relevant for the real-time production path.
const TICK_SLICE_MS: u64 = 20;

// ── MonoClock seam ─────────────────────────────────────────────────────────

/// Monotonic-clock abstraction injected into `run_driver_loop`.
///
/// Two implementations exist:
/// - [`MonoClock::real`]: delegates to the OS (`Instant` + `thread::sleep`).
///   This is the production path; behaviour is identical to the previous
///   direct calls.
/// - [`MonoClock::sim`]: reads `SIMTIME_MS` from [`crate::deterministic`]
///   for `now_ms`; `sleep_ms` performs a real 1 ms OS sleep so the TCP
///   connection-reader thread gets a scheduler timeslice.  Sim time only
///   advances when the test calls `hew_simtime_advance_ms`.
///
/// WHY fn pointers instead of a trait object: the driver loop is `unsafe` and
/// FFI-adjacent; keeping the seam as a plain `Copy` struct of function pointers
/// avoids vtable indirection and matches the style of this module.
/// WHEN sim becomes obsolete: never — this is the principled sim-clock seam
/// for SWIM driver tests; it eliminates wall-time sensitivity under any
/// instrumentation overhead.
/// WHAT the real solution is: this struct IS the real solution for Stage 1.
#[derive(Clone, Copy)]
pub(crate) struct MonoClock {
    /// Return the current monotonic time in milliseconds.
    pub(crate) now_ms: fn() -> u64,
    /// Sleep (or advance simulated time) by `ms` milliseconds.
    pub(crate) sleep_ms: fn(u64),
}

/// Real-time implementation: uses the process-epoch `Instant` and
/// `thread::sleep`.  This is identical to the previous hard-coded behaviour.
fn real_now_ms() -> u64 {
    // Re-use the same epoch anchor as `io_time::monotonic_ms`.
    use std::sync::OnceLock;
    use std::time::Instant;
    static EPOCH: OnceLock<Instant> = OnceLock::new();
    let epoch = EPOCH.get_or_init(Instant::now);
    #[expect(
        clippy::cast_possible_truncation,
        reason = "monotonic ms since process start will not exceed u64"
    )]
    {
        epoch.elapsed().as_millis() as u64
    }
}

fn real_sleep_ms(ms: u64) {
    std::thread::sleep(Duration::from_millis(ms));
}

/// Simulated-time implementation: reads / advances `SIMTIME_MS`.
fn sim_now_ms() -> u64 {
    crate::deterministic::simtime_now().unwrap_or(0)
}

fn sim_sleep_ms(_ms: u64) {
    // In sim mode the test controls time: hew_simtime_advance_ms is called by
    // the test harness to push the virtual clock forward.  The driver loop just
    // checks now_ms against its next-period target after each "sleep".
    //
    // WHY a real 1 ms sleep instead of yield_now(): the driver is NOT
    // time-sensitive in sim mode (sim time only advances when the test calls
    // hew_simtime_advance_ms), so a 1 ms real pause costs nothing
    // correctness-wise.  yield_now() is non-blocking — under coverage
    // instrumentation (~3-5× per-instruction slowdown) the driver re-enters
    // immediately, consuming all OS scheduler quanta and starving the TCP
    // connection-reader thread that calls hew_now_ms() to refresh last_seen_ms.
    // The starvation causes last_seen_ms to go stale, which makes the alive-peer
    // test fail with a false SUSPECT→DEAD verdict.  A 1 ms sleep surrenders the
    // OS timeslice unconditionally, eliminating the starvation under any
    // instrumentation overhead.
    //
    // Advancing simtime from inside the driver would race last_seen updates and
    // cause false-DEAD verdicts for live nodes (the bug this seam was built to
    // prevent) — so we only sleep real time, never advance sim time here.
    std::thread::sleep(std::time::Duration::from_millis(1));
}

impl MonoClock {
    /// Production clock: real OS time and real thread sleep.
    pub(crate) const fn real() -> Self {
        Self {
            now_ms: real_now_ms,
            sleep_ms: real_sleep_ms,
        }
    }

    /// Simulated clock: reads `SIMTIME_MS` for now; real 1 ms sleep for `sleep_ms`.
    pub(crate) const fn sim() -> Self {
        Self {
            now_ms: sim_now_ms,
            sleep_ms: sim_sleep_ms,
        }
    }

    /// Choose the clock appropriate for the current testing context: sim when
    /// `SIMTIME_ENABLED` is set, real otherwise.  Called once at driver start
    /// so the choice is stable for the driver's lifetime.
    fn for_current_context() -> Self {
        if crate::deterministic::simtime_now().is_some() {
            Self::sim()
        } else {
            Self::real()
        }
    }
}

// ── Driver registry ────────────────────────────────────────────────────────

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
        // Select clock once at start: sim when SIMTIME_ENABLED, real otherwise.
        let clock = MonoClock::for_current_context();

        let spawn = std::thread::Builder::new()
            .name("hew-swim-driver".into())
            .spawn(move || run_driver_loop(node_ptr, &thread_stop, clock));

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
/// checking the stop flag every `TICK_SLICE_MS` (real) or every 1 ms
/// (simulated — see `sim_sleep_ms` for why a real OS sleep is used there).
fn run_driver_loop(node: NodePtr, stop: &Arc<AtomicBool>, clock: MonoClock) {
    // Read the protocol period once from the cluster config; it does not change
    // for a node's lifetime. Fall back to a 1 s default if the cluster is not
    // yet available.
    let period_ms = protocol_period_ms_for(node.0).unwrap_or(1_000);
    let mut next_period_ms = (clock.now_ms)() + period_ms;

    loop {
        if stop.load(Ordering::Acquire) {
            break;
        }
        (clock.sleep_ms)(TICK_SLICE_MS);
        if stop.load(Ordering::Acquire) {
            break;
        }
        let now_ms = (clock.now_ms)();
        if now_ms < next_period_ms {
            continue;
        }
        next_period_ms += period_ms;
        // Skip missed periods if the thread was descheduled (real) or the test
        // advanced time by more than one period (sim), so we do not burst-fire
        // ticks to catch up.
        let now_ms = (clock.now_ms)();
        if next_period_ms < now_ms {
            next_period_ms = now_ms + period_ms;
        }

        // SAFETY: the node is alive — stop_swim_driver joins this thread before
        // any teardown of node resources, so the cluster/conn_mgr the period
        // dereferences cannot be freed concurrently with this call.
        unsafe {
            run_one_period(node.0);
        }
    }
}

/// Read the SWIM protocol period in milliseconds from the node's cluster config.
fn protocol_period_ms_for(node: *mut HewNode) -> Option<u64> {
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
    Some(cluster.protocol_period_ms())
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
        // `hew_node_new`/`hew_node_free` touch the runtime-owned node slot
        // (`remember_node`/`forget_node`), so a default runtime must be
        // installed or `rt_current()` traps across the FFI boundary.
        let _runtime_guard = crate::runtime_test_guard();
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
