//! Distributed (cross-node) actor monitor table (DIST-6).
//!
//! The in-process monitor table (`crate::monitor`) keys on a local `actor_id`
//! and stores a raw `*mut HewActor` watcher pointer, so it is fundamentally
//! process-local: a watcher on another node has no pointer in this address
//! space. This module is the cross-node analogue — a separate table keyed by
//! node identity that records the *local* observation slot for a remote
//! monitor, so a remote actor's death is observable here.
//!
//! # Two halves
//!
//! A cross-node monitor spans two nodes, so the table has two faces:
//!
//! - **Watcher side** (the node that called `monitor(RemotePid)`): keyed by the
//!   local `ref_id` (the `MonitorRef` value). The entry records which remote
//!   `(node_id, serial)` is watched and a one-shot terminal slot that
//!   `hew_node_monitor_recv` blocks on. The three delivery causes
//!   (`CTRL_MONITOR_DOWN` receipt, connection-drop, SWIM-DEAD) all arm this slot.
//!
//! - **Target side** (the node that owns the monitored actor): keyed by the
//!   target actor's `serial`. Each entry records the remote watchers
//!   `(watcher_node_id, ref_id)` that asked to be told when this actor dies, so
//!   the local terminal sweep can fan out a `CTRL_MONITOR_DOWN` to each.
//!
//! # Exactly-once delivery (R1 / R2)
//!
//! The watcher slot is a three-state machine: `Pending → Delivered(reason) →
//! Consumed`. Arming only transitions `Pending → Delivered`; a second arm (a
//! connection-drop racing a clean exit's DOWN frame) finds the slot already
//! `Delivered`/`Consumed` and is a no-op. `recv_down` returns the reason exactly
//! once (`Delivered → Consumed`) and then times out on any further call. This is
//! the cross-node mirror of the local table's `terminal_reasons` de-dup.
//!
//! # What crosses the wire
//!
//! Only `(node_id, serial, ref_id, reason)` — never a raw `usize` actor pointer
//! or a dispatch code address (`dispatch-pointer-never-crosses-wire`). The
//! watcher slot stores no remote pointer; the target side stores only the
//! watcher's node id and ref id.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Condvar, Mutex, PoisonError};
use std::time::Duration;

/// Reason sentinel delivered when a monitored remote actor becomes unreachable
/// because its connection dropped or its node was declared SWIM-DEAD, rather
/// than because it exited or crashed.
///
/// Negative so it can never collide with a `HewActorState` terminal value
/// (`Crashed = 5`, `Stopped = 6`), which are non-negative; the watcher uses the
/// sign to distinguish a partition from a definitive actor exit (R4). Maps to
/// the `MonitorError::MonitorLost` / `PartitionPolicy::MonitorLost` taxonomy.
pub const MONITOR_REASON_LOST: i32 = -1;

/// Reason sentinel returned by `hew_node_monitor_recv` when the deadline elapsed
/// before any terminal signal arrived (or after the single signal was already
/// observed). Distinct from `MONITOR_REASON_LOST` so a caller can tell "no
/// signal yet" from "the peer is gone".
pub const MONITOR_REASON_TIMEOUT: i32 = -2;

/// One-shot terminal state for a watcher-side monitor registration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TerminalSlot {
    /// No terminal signal yet.
    Pending,
    /// A terminal signal arrived carrying this reason; not yet observed.
    Delivered(i32),
    /// The terminal signal was observed by a `recv_down` call.
    Consumed,
}

/// Watcher-side entry: a local registration watching a remote actor.
#[derive(Debug)]
struct WatcherEntry {
    /// Node owning the monitored actor.
    remote_node_id: u16,
    /// Monitored actor's serial on the owning node.
    target_serial: u64,
    /// One-shot terminal slot armed by the delivery causes.
    slot: TerminalSlot,
}

/// A remote watcher recorded on the target side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RemoteWatcher {
    /// Node that issued the monitor (the DOWN destination).
    watcher_node_id: u16,
    /// Watcher's monitor-registration id (echoed in the DOWN).
    ref_id: u64,
}

/// Per-runtime distributed monitor state.
///
/// Owned by `RuntimeInner.dist_monitors` (mirroring `RuntimeInner.monitors`),
/// so two runtimes in one process keep independent cross-node monitor tables.
pub(crate) struct DistMonitorState {
    /// Watcher side: `ref_id` -> the local observation slot.
    watchers: Mutex<HashMap<u64, WatcherEntry>>,
    /// Condvar paired with `watchers` for blocking `recv_down`.
    delivered: Condvar,
    /// Target side: monitored `serial` -> the remote watchers of that actor.
    targets: Mutex<HashMap<u64, Vec<RemoteWatcher>>>,
    /// Monotonic `ref_id` allocator for watcher registrations.
    ref_counter: AtomicU64,
}

impl DistMonitorState {
    /// Construct an empty distributed monitor table for a fresh runtime.
    pub(crate) fn new() -> Self {
        Self {
            watchers: Mutex::new(HashMap::new()),
            delivered: Condvar::new(),
            targets: Mutex::new(HashMap::new()),
            ref_counter: AtomicU64::new(1),
        }
    }

    /// Allocate a fresh, non-zero `ref_id` for a new watcher registration.
    fn next_ref_id(&self) -> u64 {
        self.ref_counter.fetch_add(1, Ordering::Relaxed)
    }

    // ── Watcher side ──────────────────────────────────────────────────────

    /// Register a watcher entry and return its `ref_id`.
    pub(crate) fn register_watcher(&self, remote_node_id: u16, target_serial: u64) -> u64 {
        let ref_id = self.next_ref_id();
        let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
        watchers.insert(
            ref_id,
            WatcherEntry {
                remote_node_id,
                target_serial,
                slot: TerminalSlot::Pending,
            },
        );
        ref_id
    }

    /// Arm the terminal slot for a single watcher registration with `reason`.
    ///
    /// Only transitions `Pending → Delivered`; a no-op if the slot is already
    /// `Delivered` or `Consumed` (exactly-once). Wakes any blocked `recv_down`.
    /// Returns true if this call armed the slot.
    pub(crate) fn deliver_to_ref(&self, ref_id: u64, reason: i32) -> bool {
        let armed = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            match watchers.get_mut(&ref_id) {
                Some(entry) if entry.slot == TerminalSlot::Pending => {
                    entry.slot = TerminalSlot::Delivered(reason);
                    true
                }
                _ => false,
            }
        };
        if armed {
            self.delivered.notify_all();
        }
        armed
    }

    /// Arm every watcher registration targeting `remote_node_id` with `reason`,
    /// for the connection-drop / SWIM-DEAD fan-out.
    ///
    /// Only `Pending` slots are armed, so a clean-exit DOWN that already armed a
    /// slot (and may already be consumed) is not overwritten — the drop fan-out
    /// fires only for registrations with no prior definitive signal (R1/R2).
    /// Returns the number of slots newly armed.
    pub(crate) fn deliver_to_node(&self, remote_node_id: u16, reason: i32) -> usize {
        let armed = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            let mut count = 0;
            for entry in watchers.values_mut() {
                if entry.remote_node_id == remote_node_id && entry.slot == TerminalSlot::Pending {
                    entry.slot = TerminalSlot::Delivered(reason);
                    count += 1;
                }
            }
            count
        };
        if armed > 0 {
            self.delivered.notify_all();
        }
        armed
    }

    /// Block until the watcher registration `ref_id` has a delivered terminal
    /// signal, or until `timeout` elapses, returning the carried reason.
    ///
    /// Returns the reason exactly once (`Delivered → Consumed`). A second call
    /// after the signal was observed (slot `Consumed`), an unknown `ref_id`, or a
    /// deadline that elapses while still `Pending` all return
    /// [`MONITOR_REASON_TIMEOUT`].
    pub(crate) fn recv_down(&self, ref_id: u64, timeout: Duration) -> i32 {
        let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
        let deadline = std::time::Instant::now() + timeout;
        loop {
            match watchers.get(&ref_id).map(|e| e.slot) {
                Some(TerminalSlot::Delivered(reason)) => {
                    // Consume the one-shot signal so a second recv times out.
                    if let Some(entry) = watchers.get_mut(&ref_id) {
                        entry.slot = TerminalSlot::Consumed;
                    }
                    return reason;
                }
                // Already observed, unknown ref, or no signal yet within the
                // deadline: fall through to the wait / timeout below.
                Some(TerminalSlot::Consumed) | None => return MONITOR_REASON_TIMEOUT,
                Some(TerminalSlot::Pending) => {}
            }
            let now = std::time::Instant::now();
            if now >= deadline {
                return MONITOR_REASON_TIMEOUT;
            }
            let remaining = deadline - now;
            let (guard, wait) = self
                .delivered
                .wait_timeout(watchers, remaining)
                .unwrap_or_else(PoisonError::into_inner);
            watchers = guard;
            if wait.timed_out() {
                // Re-check once after the timeout: a signal may have raced in
                // between the predicate check and the wait.
                match watchers.get_mut(&ref_id).map(|e| {
                    let slot = e.slot;
                    if let TerminalSlot::Delivered(_) = slot {
                        e.slot = TerminalSlot::Consumed;
                    }
                    slot
                }) {
                    Some(TerminalSlot::Delivered(reason)) => return reason,
                    _ => return MONITOR_REASON_TIMEOUT,
                }
            }
        }
    }

    /// Remove a watcher registration (demonitor). Returns the watched
    /// `(remote_node_id, target_serial)` if the entry existed, so the caller can
    /// send a `CTRL_DEMONITOR` to the peer. Idempotent: a missing `ref_id`
    /// returns `None`.
    ///
    /// Wakes any thread blocked in [`recv_down`] for this `ref_id`. The woken
    /// thread sees no entry and returns [`MONITOR_REASON_TIMEOUT`] immediately —
    /// the correct response to an explicit demonitor. Without the wake the thread
    /// would wait out its full timeout before observing the missing entry.
    pub(crate) fn remove_watcher(&self, ref_id: u64) -> Option<(u16, u64)> {
        let result = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            watchers
                .remove(&ref_id)
                .map(|e| (e.remote_node_id, e.target_serial))
        };
        // Wake any thread parked in recv_down on this ref so it observes the
        // removal immediately rather than waiting out its full timeout.
        self.delivered.notify_all();
        result
    }

    // ── Target side ───────────────────────────────────────────────────────

    /// Record a remote watcher of a locally-owned actor (`CTRL_MONITOR_REQ`
    /// receipt). Deduplicates on `(watcher_node_id, ref_id)` so a retried
    /// request does not double-register.
    pub(crate) fn register_remote_watcher(
        &self,
        target_serial: u64,
        watcher_node_id: u16,
        ref_id: u64,
    ) {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        let watchers = targets.entry(target_serial).or_default();
        let watcher = RemoteWatcher {
            watcher_node_id,
            ref_id,
        };
        if !watchers.contains(&watcher) {
            watchers.push(watcher);
        }
    }

    /// Remove a remote watcher of a locally-owned actor (`CTRL_DEMONITOR`
    /// receipt). Idempotent.
    pub(crate) fn remove_remote_watcher(
        &self,
        target_serial: u64,
        watcher_node_id: u16,
        ref_id: u64,
    ) {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        if let Some(watchers) = targets.get_mut(&target_serial) {
            watchers.retain(|w| !(w.watcher_node_id == watcher_node_id && w.ref_id == ref_id));
            if watchers.is_empty() {
                targets.remove(&target_serial);
            }
        }
    }

    /// Take all remote watchers of a locally-dying actor (terminal sweep),
    /// removing the target entry so a later sweep finds nothing. Returns the
    /// `(watcher_node_id, ref_id)` pairs to which a `CTRL_MONITOR_DOWN` must be
    /// fanned out.
    pub(crate) fn take_remote_watchers(&self, target_serial: u64) -> Vec<(u16, u64)> {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        targets
            .remove(&target_serial)
            .map(|ws| {
                ws.into_iter()
                    .map(|w| (w.watcher_node_id, w.ref_id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Whether any remote watcher is registered for `target_serial`. Lets the
    /// terminal sweep skip the frame-send path when no node watches the actor.
    pub(crate) fn has_remote_watchers(&self, target_serial: u64) -> bool {
        let targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        targets.get(&target_serial).is_some_and(|ws| !ws.is_empty())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn register_and_recv_delivers_reason_exactly_once() {
        let state = DistMonitorState::new();
        let ref_id = state.register_watcher(7, 99);
        assert_ne!(ref_id, 0);

        // Arm with the clean-exit reason (HewActorState::Stopped == 6).
        assert!(state.deliver_to_ref(ref_id, 6), "first arm must succeed");
        // A second arm is a no-op (exactly-once arming).
        assert!(
            !state.deliver_to_ref(ref_id, 5),
            "second arm must be a no-op"
        );

        // First recv observes the reason.
        assert_eq!(state.recv_down(ref_id, Duration::from_secs(1)), 6);
        // Second recv times out — the one-shot signal was consumed.
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
    }

    #[test]
    fn deliver_to_node_arms_only_pending_slots() {
        let state = DistMonitorState::new();
        let a = state.register_watcher(7, 1);
        let b = state.register_watcher(7, 2);
        let other = state.register_watcher(9, 3);

        // A clean-exit DOWN already armed `a` before the connection dropped.
        assert!(state.deliver_to_ref(a, 6));

        // Connection drop to node 7 fans out MonitorLost: it must arm only `b`
        // (still pending), not `a` (already delivered) and not `other` (node 9).
        let armed = state.deliver_to_node(7, MONITOR_REASON_LOST);
        assert_eq!(armed, 1);

        // `a` keeps its clean-exit reason; `b` gets MonitorLost; `other` is
        // untouched (still pending → recv times out).
        assert_eq!(state.recv_down(a, Duration::from_millis(50)), 6);
        assert_eq!(
            state.recv_down(b, Duration::from_millis(50)),
            MONITOR_REASON_LOST
        );
        assert_eq!(
            state.recv_down(other, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
    }

    #[test]
    fn drop_after_clean_exit_does_not_redeliver() {
        // R2: a clean exit delivers + is consumed, then the node drops; the
        // drop fan-out must NOT re-arm the slot (no second DOWN).
        let state = DistMonitorState::new();
        let ref_id = state.register_watcher(7, 1);
        assert!(state.deliver_to_ref(ref_id, 6));
        assert_eq!(state.recv_down(ref_id, Duration::from_millis(50)), 6);

        // Node drops afterwards: slot is Consumed, so no re-arm.
        assert_eq!(state.deliver_to_node(7, MONITOR_REASON_LOST), 0);
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
    }

    #[test]
    fn recv_unknown_ref_times_out() {
        let state = DistMonitorState::new();
        assert_eq!(
            state.recv_down(424_242, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
    }

    #[test]
    fn recv_pending_times_out_then_delivers_after_arm() {
        let state = DistMonitorState::new();
        let ref_id = state.register_watcher(7, 1);
        // No signal yet: a short recv times out without consuming anything.
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
        // Now arm and recv: the still-Pending slot delivers.
        assert!(state.deliver_to_ref(ref_id, 5));
        assert_eq!(state.recv_down(ref_id, Duration::from_millis(50)), 5);
    }

    #[test]
    fn remove_watcher_returns_target_and_is_idempotent() {
        let state = DistMonitorState::new();
        let ref_id = state.register_watcher(7, 99);
        assert_eq!(state.remove_watcher(ref_id), Some((7, 99)));
        // Idempotent: a second remove finds nothing.
        assert_eq!(state.remove_watcher(ref_id), None);
        // recv on the removed ref times out (no entry).
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
    }

    #[test]
    fn target_side_register_dedupes_and_take_sweeps() {
        let state = DistMonitorState::new();
        state.register_remote_watcher(50, 7, 100);
        state.register_remote_watcher(50, 7, 100); // duplicate: ignored
        state.register_remote_watcher(50, 9, 200);
        assert!(state.has_remote_watchers(50));

        let mut taken = state.take_remote_watchers(50);
        taken.sort_unstable();
        assert_eq!(taken, vec![(7, 100), (9, 200)]);
        // The sweep removed the entry.
        assert!(!state.has_remote_watchers(50));
        assert!(state.take_remote_watchers(50).is_empty());
    }

    #[test]
    fn target_side_remove_remote_watcher_is_idempotent() {
        let state = DistMonitorState::new();
        state.register_remote_watcher(50, 7, 100);
        state.remove_remote_watcher(50, 7, 100);
        assert!(!state.has_remote_watchers(50));
        // Idempotent: removing again does not panic.
        state.remove_remote_watcher(50, 7, 100);
        state.remove_remote_watcher(999, 1, 1);
    }

    #[test]
    fn recv_blocks_until_concurrent_arm() {
        use std::sync::Arc;
        let state = Arc::new(DistMonitorState::new());
        let ref_id = state.register_watcher(7, 1);

        let state_recv = Arc::clone(&state);
        let handle = std::thread::spawn(move || {
            // Generous timeout; the arm below should wake this well before it.
            state_recv.recv_down(ref_id, Duration::from_secs(5))
        });

        // Give the recv thread time to park on the condvar, then arm.
        std::thread::sleep(Duration::from_millis(50));
        assert!(state.deliver_to_ref(ref_id, 6));

        let reason = handle.join().expect("recv thread panicked");
        assert_eq!(reason, 6, "blocked recv must wake with the armed reason");
    }

    /// F3: `remove_watcher` wakes a blocked `recv_down` promptly.
    ///
    /// A thread parked on `recv_down(ref, 5 s)` must return well before the
    /// 5-second timeout when the registration is removed by `remove_watcher`.
    /// Without the `notify_all` the thread would sleep for the full timeout and
    /// only then observe the absent entry.
    #[test]
    fn remove_watcher_wakes_blocked_recv_promptly() {
        use std::sync::Arc;
        use std::time::Instant;
        let state = Arc::new(DistMonitorState::new());
        let ref_id = state.register_watcher(7, 1);

        // A sibling ref on another node — must NOT be disturbed by this remove.
        let sibling_ref = state.register_watcher(9, 2);

        let state_recv = Arc::clone(&state);
        let started = Instant::now();
        let handle = std::thread::spawn(move || {
            // 5-second ceiling; a missing wake would blow this.
            state_recv.recv_down(ref_id, Duration::from_secs(5))
        });

        // Park the recv thread, then remove the registration.
        std::thread::sleep(Duration::from_millis(50));
        let removed = state.remove_watcher(ref_id);
        assert_eq!(
            removed,
            Some((7, 1)),
            "remove_watcher must return the target"
        );

        let reason = handle.join().expect("recv thread panicked");
        let elapsed = started.elapsed();

        // Correct return value: no entry → MONITOR_REASON_TIMEOUT.
        assert_eq!(
            reason, MONITOR_REASON_TIMEOUT,
            "removed ref must yield MONITOR_REASON_TIMEOUT"
        );
        // Bounded-time: the wake must arrive well before the 5 s timeout.
        // Allow 2 s for scheduling jitter on a slow CI host; a missing notify_all
        // would take the full 5 s.
        assert!(
            elapsed < Duration::from_secs(2),
            "recv_down after remove_watcher took {elapsed:?}; expected well under 2 s \
             (notify_all missing?)"
        );
        // Negative guard: the sibling ref on node 9 must not have been woken with
        // a spurious signal — it is still Pending, so its recv times out normally.
        assert_eq!(
            state.recv_down(sibling_ref, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT,
            "notify_all on remove must not spuriously deliver to a sibling ref"
        );
    }
}
