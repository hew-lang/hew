//! Distributed (cross-node) actor monitor table.
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

use crate::node_identity::{Location, NodeId};

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
    /// The terminal signal was observed by a `recv_down` call (monitor) or the
    /// link EXIT cascade was fired (link).
    Consumed,
}

/// What a watcher-side entry does when its remote target reaches a terminal
/// state.
///
/// A single table backs both cross-node monitors and cross-node links so the
/// register / deliver / terminal-sweep / connection-drop / SWIM-DEAD machinery
/// serves both (per `feedback_unify_repeated_compiler_patterns` — no third
/// special-case table). The action is the per-entry divergence: a monitor arms a
/// recv slot a program polls; a link fires a `SYS_MSG_EXIT` into the LOCAL linked
/// actor's mailbox (the OTP crash cascade), per its `PartitionPolicy`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherAction {
    /// Cross-node monitor: arm the recv slot; `recv_down` observes it.
    Observe,
    /// Cross-node link: on the target's death, fire a link-down into
    /// the LOCAL linked actor `local_actor_id` per `policy_tag` (the
    /// `PartitionPolicy` discriminant; `CrashLinked` == 3 crashes it).
    Link { local_actor_id: u64, policy_tag: u8 },
}

/// The cross-node link cascade a delivery cause produces: fire a link-down into
/// `local_actor_id` carrying `reason`, governed by `policy_tag`. Returned to the
/// caller (the runtime layer) which performs the mailbox-EXIT synthesis — the
/// table stays pure data and does not reach into actor code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LinkDown {
    /// The LOCAL linked actor to fire the EXIT into.
    pub local_actor_id: u64,
    /// The dead remote peer's serial (carried into the `ExitMessage` for the
    /// typed `#[on(exit)]` consumer; informational — the crash is the action).
    pub remote_target_serial: u64,
    /// The terminal reason (`HewActorState` value, or a partition sentinel).
    pub reason: i32,
    /// The `PartitionPolicy` discriminant governing the link.
    pub policy_tag: u8,
}

/// Watcher-side entry: a local registration watching a remote actor (monitor or
/// link).
#[derive(Debug)]
struct WatcherEntry {
    /// Exact monitored/linked actor identity.
    target: Location,
    /// One-shot terminal slot armed by the delivery causes.
    slot: TerminalSlot,
    /// Monitor (arm recv slot) vs link (fire mailbox EXIT cascade).
    action: WatcherAction,
}

/// A remote watcher recorded on the target side.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RemoteWatcher {
    /// Exact watcher/linker actor identity (the DOWN destination).
    watcher: Location,
    /// Watcher's monitor/link-registration id (echoed in the DOWN).
    ref_id: u64,
    /// Whether this watcher is a cross-node LINK (true → the terminal sweep
    /// sends `CTRL_LINK_DOWN`; false → `CTRL_MONITOR_DOWN`). The wire frame kind
    /// and the receiver's handling (mailbox-EXIT crash vs recv slot) diverge on
    /// this bit.
    is_link: bool,
}

/// A target-side remote watcher's destination + kind, returned by the terminal
/// sweep so the caller routes the right DOWN frame kind to the right node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RemoteWatcherTarget {
    /// Exact watcher/linker destination.
    pub watcher: Location,
    /// The watcher's registration id (echoed in the DOWN).
    pub ref_id: u64,
    /// LINK (`CTRL_LINK_DOWN` + mailbox-EXIT) vs MONITOR (`CTRL_MONITOR_DOWN`).
    pub is_link: bool,
}

/// Base for the cross-node `ref_id` allocator.
///
/// Cross-node `ref_id`s are namespaced into a high range disjoint from the local
/// monitor counter (`crate::monitor`, which starts at 1) so the locality split
/// in `hew_actor_demonitor` can route by `ref_id` without misrouting: a local ref
/// is always `< DIST_REF_ID_BASE` and a cross-node ref always `>=` it, so a
/// local-table miss for a value in the high range unambiguously routes to the
/// cross-node teardown. 2^48 is far above any realistic local
/// monitor count and below the pid serial mask, so the two spaces never collide.
pub(crate) const DIST_REF_ID_BASE: u64 = 1 << 48;

/// Whether `ref_id` belongs to the cross-node namespace (so `hew_actor_demonitor`
/// routes its teardown to the dist table rather than the local monitor table).
#[must_use]
pub(crate) fn is_cross_node_ref(ref_id: u64) -> bool {
    ref_id >= DIST_REF_ID_BASE
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
    /// Monotonic `ref_id` allocator for watcher registrations, based at
    /// [`DIST_REF_ID_BASE`] so cross-node refs never collide with local ones.
    ref_counter: AtomicU64,
}

impl DistMonitorState {
    /// Construct an empty distributed monitor table for a fresh runtime.
    pub(crate) fn new() -> Self {
        Self {
            watchers: Mutex::new(HashMap::new()),
            delivered: Condvar::new(),
            targets: Mutex::new(HashMap::new()),
            // Base in the high cross-node namespace (see DIST_REF_ID_BASE).
            ref_counter: AtomicU64::new(DIST_REF_ID_BASE + 1),
        }
    }

    /// Allocate a fresh, non-zero `ref_id` for a new watcher registration.
    fn next_ref_id(&self) -> u64 {
        self.ref_counter.fetch_add(1, Ordering::Relaxed)
    }

    // ── Watcher side ──────────────────────────────────────────────────────

    /// Register a cross-node MONITOR watcher entry and return its `ref_id`.
    pub(crate) fn register_watcher(&self, target: Location) -> u64 {
        self.register_entry(target, WatcherAction::Observe)
    }

    /// Register a cross-node LINK watcher entry and return its `ref_id`. On the
    /// remote target's death, the entry fires a link-down into the LOCAL linked
    /// actor `local_actor_id` per `policy_tag`.
    pub(crate) fn register_link_watcher(
        &self,
        target: Location,
        local_actor_id: u64,
        policy_tag: u8,
    ) -> u64 {
        self.register_entry(
            target,
            WatcherAction::Link {
                local_actor_id,
                policy_tag,
            },
        )
    }

    /// Register a watcher entry with the given action and return its `ref_id`.
    fn register_entry(&self, target: Location, action: WatcherAction) -> u64 {
        let ref_id = self.next_ref_id();
        let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
        watchers.insert(
            ref_id,
            WatcherEntry {
                target,
                slot: TerminalSlot::Pending,
                action,
            },
        );
        ref_id
    }

    /// Arm the terminal slot for a single watcher registration with `reason`.
    ///
    /// The `remote_node_id` must match the node this monitor registration was
    /// originally opened against. This binds `CTRL_MONITOR_DOWN` delivery to the
    /// handshake-authenticated peer that sent the frame instead of trusting the
    /// ref id alone.
    ///
    /// Only transitions `Pending → Delivered`; a no-op if the slot is already
    /// `Delivered` or `Consumed` (exactly-once). Wakes any blocked `recv_down`.
    /// Returns true if this call armed the slot.
    pub(crate) fn deliver_to_ref(&self, ref_id: u64, target: Location, reason: i32) -> bool {
        let armed = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            match watchers.get_mut(&ref_id) {
                // Only MONITOR (Observe) entries arm a recv slot. A link entry is
                // delivered through `deliver_link_down_to_ref`; a CTRL_MONITOR_DOWN
                // must never arm a link entry's slot (it has no recv consumer).
                Some(entry)
                    if entry.slot == TerminalSlot::Pending
                        && entry.target == target
                        && matches!(entry.action, WatcherAction::Observe) =>
                {
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

    /// Fire a cross-node LINK down for a single link registration (`CTRL_LINK_DOWN`
    /// receipt). Transitions the entry `Pending → Consumed` exactly once and
    /// returns the cascade action (the LOCAL linked actor + reason + policy) so the
    /// caller can synthesize the mailbox EXIT. A no-op (returns `None`) if
    /// `authenticated_peer` is 0 (unauthenticated sender), the entry is unknown,
    /// already fired, is a monitor (not a link), or `authenticated_peer` does not
    /// match the entry's own `remote_node_id` — the exactly-once + fail-closed +
    /// peer-binding guard. The EXIT is fired ONLY for an entry THIS node
    /// registered AND ONLY when the frame's authenticated sender is the same peer
    /// this node linked to, so neither a forged `ref_id` this node never linked
    /// NOR a genuinely-authenticated but UNRELATED peer (one that merely
    /// guessed/learned a pending cross-node link `ref_id`) can crash a linked
    /// actor before its real remote peer has died.
    ///
    /// `authenticated_peer == 0` is rejected explicitly before it is compared to
    /// `entry.remote_node_id`, including when no ACTIVE connection matches the
    /// frame's `conn_id`.
    pub(crate) fn deliver_link_down_to_ref(
        &self,
        ref_id: u64,
        target: Location,
        reason: i32,
    ) -> Option<LinkDown> {
        let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
        match watchers.get_mut(&ref_id) {
            Some(entry) if entry.slot == TerminalSlot::Pending && entry.target == target => {
                if let WatcherAction::Link {
                    local_actor_id,
                    policy_tag,
                } = entry.action
                {
                    let remote_target_serial = entry.target.slot();
                    // Fire-once: a link has no recv step, so go straight to
                    // Consumed (a later partition fan-out finds it non-Pending).
                    entry.slot = TerminalSlot::Consumed;
                    Some(LinkDown {
                        local_actor_id,
                        remote_target_serial,
                        reason,
                        policy_tag,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Arm every watcher registration targeting `remote_node_id` with `reason`,
    /// for the connection-drop / SWIM-DEAD fan-out.
    ///
    /// Only `Pending` slots are armed, so a clean-exit DOWN that already armed a
    /// slot (and may already be consumed) is not overwritten — the drop fan-out
    /// fires only for registrations with no prior definitive signal (R1/R2).
    /// Returns the number of slots newly armed.
    pub(crate) fn deliver_to_node(&self, remote_node: NodeId, reason: i32) -> usize {
        self.deliver_to_matching(|target| target.node() == remote_node, reason)
    }

    /// Arm pending monitor registrations from one superseded node session.
    pub(crate) fn deliver_to_session(
        &self,
        remote_node: NodeId,
        session_incarnation: u32,
        reason: i32,
    ) -> usize {
        self.deliver_to_matching(
            |target| target.node() == remote_node && target.incarnation() == session_incarnation,
            reason,
        )
    }

    fn deliver_to_matching(&self, matches: impl Fn(Location) -> bool, reason: i32) -> usize {
        let armed = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            let mut count = 0;
            for entry in watchers.values_mut() {
                // Only MONITOR (Observe) entries arm a recv slot here; link
                // entries fire through `take_link_downs_for_node` (the mailbox
                // EXIT cascade has no recv consumer).
                if matches(entry.target)
                    && entry.slot == TerminalSlot::Pending
                    && matches!(entry.action, WatcherAction::Observe)
                {
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

    /// Fire the cross-node LINK cascade for every still-`Pending` LINK entry
    /// targeting `remote_node_id` (connection-drop / SWIM-DEAD fan-out). Each is
    /// transitioned `Pending → Consumed` exactly once and returned as a `LinkDown`
    /// the caller fires into the local linked actor's mailbox per its policy.
    ///
    /// Only `Pending` link entries fire, so a link that already received a
    /// definitive `CTRL_LINK_DOWN` (clean exit / crash) is untouched — the
    /// exactly-once disambiguation: a definitive remote-exit beats a later
    /// partition signal for the same registration.
    pub(crate) fn take_link_downs_for_node(
        &self,
        remote_node: NodeId,
        reason: i32,
    ) -> Vec<LinkDown> {
        self.take_link_downs_matching(|target| target.node() == remote_node, reason)
    }

    /// Consume pending links from one superseded node session.
    pub(crate) fn take_link_downs_for_session(
        &self,
        remote_node: NodeId,
        session_incarnation: u32,
        reason: i32,
    ) -> Vec<LinkDown> {
        self.take_link_downs_matching(
            |target| target.node() == remote_node && target.incarnation() == session_incarnation,
            reason,
        )
    }

    fn take_link_downs_matching(
        &self,
        matches: impl Fn(Location) -> bool,
        reason: i32,
    ) -> Vec<LinkDown> {
        let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
        let mut downs = Vec::new();
        for entry in watchers.values_mut() {
            if matches(entry.target) && entry.slot == TerminalSlot::Pending {
                if let WatcherAction::Link {
                    local_actor_id,
                    policy_tag,
                } = entry.action
                {
                    let remote_target_serial = entry.target.slot();
                    entry.slot = TerminalSlot::Consumed;
                    downs.push(LinkDown {
                        local_actor_id,
                        remote_target_serial,
                        reason,
                        policy_tag,
                    });
                }
            }
        }
        downs
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
    pub(crate) fn remove_watcher(&self, ref_id: u64) -> Option<Location> {
        let result = {
            let mut watchers = self.watchers.lock().unwrap_or_else(PoisonError::into_inner);
            watchers.remove(&ref_id).map(|e| e.target)
        };
        // Wake any thread parked in recv_down on this ref so it observes the
        // removal immediately rather than waiting out its full timeout.
        self.delivered.notify_all();
        result
    }

    // ── Target side ───────────────────────────────────────────────────────

    /// Record a remote MONITOR watcher of a locally-owned actor
    /// (`CTRL_MONITOR_REQ` receipt). Deduplicates on `(watcher_node_id, ref_id)`.
    pub(crate) fn register_remote_watcher(
        &self,
        target_serial: u64,
        watcher: Location,
        ref_id: u64,
    ) {
        self.register_remote_watcher_kind(target_serial, watcher, ref_id, false);
    }

    /// Record a remote LINK watcher of a locally-owned actor (`CTRL_LINK_REQ`
    /// receipt): when the local target dies, the terminal sweep fans a
    /// `CTRL_LINK_DOWN` (not a monitor DOWN) to the linker node.
    pub(crate) fn register_remote_link_watcher(
        &self,
        target_serial: u64,
        watcher: Location,
        ref_id: u64,
    ) {
        self.register_remote_watcher_kind(target_serial, watcher, ref_id, true);
    }

    /// Record a remote watcher (monitor or link) of a locally-owned actor.
    /// Deduplicates on `(watcher_node_id, ref_id)` so a retried request does not
    /// double-register.
    fn register_remote_watcher_kind(
        &self,
        target_serial: u64,
        watcher: Location,
        ref_id: u64,
        is_link: bool,
    ) {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        let watchers = targets.entry(target_serial).or_default();
        // Dedup on the identifying (node, ref) pair regardless of kind so a
        // retried REQ does not double-register.
        if !watchers
            .iter()
            .any(|w| w.watcher == watcher && w.ref_id == ref_id)
        {
            watchers.push(RemoteWatcher {
                watcher,
                ref_id,
                is_link,
            });
        }
    }

    /// Remove a remote watcher of a locally-owned actor (`CTRL_DEMONITOR`
    /// receipt). Idempotent.
    pub(crate) fn remove_remote_watcher(&self, target_serial: u64, watcher: Location, ref_id: u64) {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        if let Some(watchers) = targets.get_mut(&target_serial) {
            watchers.retain(|w| !(w.watcher == watcher && w.ref_id == ref_id));
            if watchers.is_empty() {
                targets.remove(&target_serial);
            }
        }
    }

    /// Take all remote watchers of a locally-dying actor (terminal sweep),
    /// removing the target entry so a later sweep finds nothing. Returns each
    /// watcher's destination + kind so the caller fans the right DOWN frame:
    /// `CTRL_MONITOR_DOWN` for a monitor watcher, `CTRL_LINK_DOWN` for a link
    /// watcher (whose receiver crashes its local linked actor).
    pub(crate) fn take_remote_watchers(&self, target_serial: u64) -> Vec<RemoteWatcherTarget> {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        targets
            .remove(&target_serial)
            .map(|ws| {
                ws.into_iter()
                    .map(|w| RemoteWatcherTarget {
                        watcher: w.watcher,
                        ref_id: w.ref_id,
                        is_link: w.is_link,
                    })
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

    /// Prune every `RemoteWatcher` entry on the target side whose `watcher_node_id`
    /// matches `dead_watcher_node_id`, removing now-empty serial slots.
    ///
    /// Called when a watcher node dies (connection-drop or SWIM-DEAD) so its
    /// pending watch registrations do not accumulate indefinitely on a long-lived
    /// target actor. Returns the number of entries pruned.
    ///
    /// Takes only the `targets` lock; no interaction with the `watchers` mutex or
    /// `delivered` condvar, so lock ordering is preserved. Idempotent: pruning a
    /// node with no registered watchers is a no-op returning 0.
    pub(crate) fn prune_remote_watchers_for_node(&self, dead_watcher: NodeId) -> usize {
        self.prune_remote_watchers_matching(|watcher| watcher.node() == dead_watcher)
    }

    /// Prune target-side registrations owned by one superseded watcher session.
    pub(crate) fn prune_remote_watchers_for_session(
        &self,
        dead_watcher: NodeId,
        session_incarnation: u32,
    ) -> usize {
        self.prune_remote_watchers_matching(|watcher| {
            watcher.node() == dead_watcher && watcher.incarnation() == session_incarnation
        })
    }

    fn prune_remote_watchers_matching(&self, matches: impl Fn(Location) -> bool) -> usize {
        let mut targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        let mut pruned = 0usize;
        targets.retain(|_serial, watchers| {
            let before = watchers.len();
            watchers.retain(|w| !matches(w.watcher));
            pruned += before - watchers.len();
            // Drop the serial slot when no watchers remain (mirrors
            // remove_remote_watcher's empty-slot cleanup).
            !watchers.is_empty()
        });
        pruned
    }

    /// Count of remote watcher entries across all serials.
    ///
    /// Used by `hew_dist_monitor_remote_watcher_count` (the test-introspection
    /// probe callable from `.hew` fixtures) and by the in-process unit tests.
    pub(crate) fn remote_watcher_count(&self) -> usize {
        let targets = self.targets.lock().unwrap_or_else(PoisonError::into_inner);
        targets.values().map(Vec::len).sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::link::POLICY_TAG_CRASH_LINKED;

    fn location(node: u8, slot: u64) -> Location {
        Location::new(NodeId::from_bytes([node; 16]), slot, 1).unwrap()
    }

    fn location_with_session(node: u8, slot: u64, session: u32) -> Location {
        Location::new(NodeId::from_bytes([node; 16]), slot, session).unwrap()
    }

    #[test]
    fn register_and_recv_delivers_reason_exactly_once() {
        let state = DistMonitorState::new();
        let target = location(7, 99);
        let ref_id = state.register_watcher(target);
        assert_ne!(ref_id, 0);

        // Arm with the clean-exit reason (HewActorState::Stopped == 6).
        assert!(
            state.deliver_to_ref(ref_id, target, 6),
            "first arm must succeed"
        );
        // A second arm is a no-op (exactly-once arming).
        assert!(
            !state.deliver_to_ref(ref_id, target, 5),
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
    fn monitor_down_from_wrong_authenticated_peer_is_rejected() {
        let state = DistMonitorState::new();
        let target = location(7, 99);
        let ref_id = state.register_watcher(target);

        assert!(
            !state.deliver_to_ref(ref_id, location(9, 99), 5),
            "DOWN from a different authenticated peer must not arm the monitor"
        );
        assert!(
            !state.deliver_to_ref(ref_id, location_with_session(7, 99, 2), 5),
            "DOWN from a stale session must not arm the monitor"
        );
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT,
            "forged DOWN must leave the slot pending"
        );

        assert!(
            state.deliver_to_ref(ref_id, target, 6),
            "the owning authenticated peer can still deliver"
        );
        assert_eq!(state.recv_down(ref_id, Duration::from_millis(50)), 6);
    }

    #[test]
    fn deliver_to_node_arms_only_pending_slots() {
        let state = DistMonitorState::new();
        let a_target = location(7, 1);
        let a = state.register_watcher(a_target);
        let b = state.register_watcher(location(7, 2));
        let other = state.register_watcher(location(9, 3));

        // A clean-exit DOWN already armed `a` before the connection dropped.
        assert!(state.deliver_to_ref(a, a_target, 6));

        // Connection drop to node 7 fans out MonitorLost: it must arm only `b`
        // (still pending), not `a` (already delivered) and not `other` (node 9).
        let armed = state.deliver_to_node(NodeId::from_bytes([7; 16]), MONITOR_REASON_LOST);
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
    fn superseded_session_fan_out_leaves_fresh_session_entries_pending() {
        let state = DistMonitorState::new();
        let old_target = location_with_session(7, 1, 1);
        let fresh_target = location_with_session(7, 1, 2);
        let old_monitor = state.register_watcher(old_target);
        let fresh_monitor = state.register_watcher(fresh_target);
        let old_link = state.register_link_watcher(old_target, 100, POLICY_TAG_CRASH_LINKED);
        let fresh_link = state.register_link_watcher(fresh_target, 200, POLICY_TAG_CRASH_LINKED);

        assert_eq!(
            state.deliver_to_session(NodeId::from_bytes([7; 16]), 1, MONITOR_REASON_LOST),
            1
        );
        assert_eq!(
            state.recv_down(old_monitor, Duration::from_millis(50)),
            MONITOR_REASON_LOST
        );
        assert_eq!(
            state.recv_down(fresh_monitor, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );

        let downs =
            state.take_link_downs_for_session(NodeId::from_bytes([7; 16]), 1, MONITOR_REASON_LOST);
        assert_eq!(downs.len(), 1);
        assert_eq!(downs[0].local_actor_id, 100);
        assert!(state
            .deliver_link_down_to_ref(fresh_link, fresh_target, 6)
            .is_some());
        assert!(state
            .deliver_link_down_to_ref(old_link, old_target, 6)
            .is_none());
    }

    #[test]
    fn drop_after_clean_exit_does_not_redeliver() {
        // R2: a clean exit delivers + is consumed, then the node drops; the
        // drop fan-out must NOT re-arm the slot (no second DOWN).
        let state = DistMonitorState::new();
        let target = location(7, 1);
        let ref_id = state.register_watcher(target);
        assert!(state.deliver_to_ref(ref_id, target, 6));
        assert_eq!(state.recv_down(ref_id, Duration::from_millis(50)), 6);

        // Node drops afterwards: slot is Consumed, so no re-arm.
        assert_eq!(
            state.deliver_to_node(NodeId::from_bytes([7; 16]), MONITOR_REASON_LOST),
            0
        );
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
        let target = location(7, 1);
        let ref_id = state.register_watcher(target);
        // No signal yet: a short recv times out without consuming anything.
        assert_eq!(
            state.recv_down(ref_id, Duration::from_millis(20)),
            MONITOR_REASON_TIMEOUT
        );
        // Now arm and recv: the still-Pending slot delivers.
        assert!(state.deliver_to_ref(ref_id, target, 5));
        assert_eq!(state.recv_down(ref_id, Duration::from_millis(50)), 5);
    }

    #[test]
    fn remove_watcher_returns_target_and_is_idempotent() {
        let state = DistMonitorState::new();
        let target = location(7, 99);
        let ref_id = state.register_watcher(target);
        assert_eq!(state.remove_watcher(ref_id), Some(target));
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
        state.register_remote_watcher(50, location(7, 1), 100); // monitor
        state.register_remote_watcher(50, location(7, 1), 100); // duplicate: ignored
        state.register_remote_link_watcher(50, location(9, 2), 200); // link
        assert!(state.has_remote_watchers(50));

        let taken: Vec<(NodeId, u64, bool)> = state
            .take_remote_watchers(50)
            .into_iter()
            .map(|w| (w.watcher.node(), w.ref_id, w.is_link))
            .collect();
        // The monitor watcher carries is_link=false; the link watcher is_link=true
        // so the terminal sweep routes CTRL_LINK_DOWN vs CTRL_MONITOR_DOWN.
        assert_eq!(
            taken,
            vec![
                (NodeId::from_bytes([7; 16]), 100, false),
                (NodeId::from_bytes([9; 16]), 200, true)
            ]
        );
        // The sweep removed the entry.
        assert!(!state.has_remote_watchers(50));
        assert!(state.take_remote_watchers(50).is_empty());
    }

    #[test]
    fn target_side_remove_remote_watcher_is_idempotent() {
        let state = DistMonitorState::new();
        let watcher = location(7, 1);
        state.register_remote_watcher(50, watcher, 100);
        state.remove_remote_watcher(50, watcher, 100);
        assert!(!state.has_remote_watchers(50));
        // Idempotent: removing again does not panic.
        state.remove_remote_watcher(50, watcher, 100);
        state.remove_remote_watcher(999, location(1, 1), 1);
    }

    #[test]
    fn target_side_session_prune_preserves_fresh_watcher_session() {
        let state = DistMonitorState::new();
        state.register_remote_watcher(50, location_with_session(7, 1, 1), 100);
        state.register_remote_watcher(50, location_with_session(7, 1, 2), 101);

        assert_eq!(
            state.prune_remote_watchers_for_session(NodeId::from_bytes([7; 16]), 1),
            1
        );
        let remaining = state.take_remote_watchers(50);
        assert_eq!(remaining.len(), 1);
        assert_eq!(remaining[0].watcher, location_with_session(7, 1, 2));
    }

    /// Pruning a dead watcher node removes exactly its entries and leaves
    /// surviving nodes intact. Also verifies empty serial slots are dropped.
    #[test]
    fn prune_remote_watchers_for_node_removes_dead_node_entries() {
        let state = DistMonitorState::new();
        // serial 50: watched by node 7 (two refs) and node 9 (one ref).
        state.register_remote_watcher(50, location(7, 1), 100);
        state.register_remote_watcher(50, location(7, 2), 101);
        state.register_remote_watcher(50, location(9, 3), 200);
        // serial 51: watched only by node 7.
        state.register_remote_watcher(51, location(7, 4), 102);
        // serial 52: watched only by node 9.
        state.register_remote_watcher(52, location(9, 5), 201);

        // Node 7 dies — prune its entries from the target side.
        let pruned = state.prune_remote_watchers_for_node(NodeId::from_bytes([7; 16]));
        // Three entries for node 7 across serials 50 and 51.
        assert_eq!(pruned, 3, "must prune exactly the three node-7 entries");

        // serial 50: node-9 entry survives.
        assert!(
            state.has_remote_watchers(50),
            "serial 50 must still have node-9 watcher"
        );
        // serial 51: was node-7 only → empty, slot must be dropped.
        assert!(
            !state.has_remote_watchers(51),
            "serial 51 must be gone (empty after pruning node 7)"
        );
        // serial 52: node-9 only — untouched.
        assert!(
            state.has_remote_watchers(52),
            "serial 52 must still have node-9 watcher"
        );

        // Exact count: 1 (serial 50 / node 9) + 1 (serial 52 / node 9) = 2.
        assert_eq!(
            state.remote_watcher_count(),
            2,
            "exactly 2 entries must remain"
        );

        // Negative: pruning again is a no-op.
        assert_eq!(
            state.prune_remote_watchers_for_node(NodeId::from_bytes([7; 16])),
            0,
            "second prune must be a no-op"
        );

        // Negative: pruning a node with no entries is a no-op.
        assert_eq!(
            state.prune_remote_watchers_for_node(NodeId::from_bytes([42; 16])),
            0,
            "pruning a node with no entries must return 0"
        );
    }

    /// A cross-node LINK entry fires its cascade exactly once via
    /// `deliver_link_down_to_ref`, carrying the local actor id + policy, and a
    /// second delivery is a no-op (the exactly-once link-down guard).
    #[test]
    fn link_entry_fires_cascade_exactly_once() {
        let state = DistMonitorState::new();
        let target = location(7, 99);
        let ref_id = state.register_link_watcher(target, 12_345, POLICY_TAG_CRASH_LINKED);
        assert_ne!(ref_id, 0);

        // First link-down, authenticated as node 7 (the real linked-to peer),
        // fires the cascade carrying the local actor + policy.
        let down = state
            .deliver_link_down_to_ref(ref_id, target, 5)
            .expect("first link-down must fire");
        assert_eq!(down.local_actor_id, 12_345);
        assert_eq!(down.reason, 5);
        assert_eq!(down.policy_tag, POLICY_TAG_CRASH_LINKED);

        // Second delivery is a no-op (fire-once): the entry is Consumed.
        assert!(
            state.deliver_link_down_to_ref(ref_id, target, 6).is_none(),
            "second link-down must be a no-op"
        );
    }

    /// A `CTRL_LINK_DOWN` whose handshake-authenticated sender does NOT match
    /// the entry's own `remote_node_id` must be rejected — a real link to node 7
    /// must not be crashed by a different, genuinely-connected peer (node 99)
    /// that merely guessed/learned the pending `ref_id`. This is the exact
    /// upstream defect this test locks in: the entry must survive intact and
    /// remain deliverable by its real peer afterward.
    #[test]
    fn link_down_from_wrong_authenticated_peer_is_rejected() {
        let state = DistMonitorState::new();
        let target = location(7, 99);
        let ref_id = state.register_link_watcher(target, 777, POLICY_TAG_CRASH_LINKED);

        // Node 99 is connected and authenticated, but it is NOT the peer this
        // link was registered against (node 7) — the forged/misattributed case.
        assert!(
            state
                .deliver_link_down_to_ref(ref_id, location(99, 99), 4321)
                .is_none(),
            "a link-down from an unrelated authenticated peer must not fire the cascade"
        );

        // The entry must still be Pending: the real peer (node 7) can still
        // legitimately fire it later.
        let down = state
            .deliver_link_down_to_ref(ref_id, target, 4321)
            .expect("the real linked-to peer must still be able to fire the cascade");
        assert_eq!(down.local_actor_id, 777);
        assert_eq!(down.reason, 4321);
    }

    /// A `CTRL_MONITOR_DOWN`-style `deliver_to_ref` must NOT fire a LINK entry,
    /// and a `CTRL_LINK_DOWN`-style `deliver_link_down_to_ref` must NOT fire a
    /// MONITOR entry — the two delivery paths never cross (a link has no recv
    /// slot; a monitor has no mailbox-EXIT cascade).
    #[test]
    fn link_and_monitor_delivery_paths_do_not_cross() {
        let state = DistMonitorState::new();
        let monitor_target = location(7, 1);
        let link_target = location(7, 2);
        let monitor_ref = state.register_watcher(monitor_target);
        let link_ref = state.register_link_watcher(link_target, 500, POLICY_TAG_CRASH_LINKED);

        // A monitor DOWN must not fire the link entry.
        assert!(
            state
                .deliver_link_down_to_ref(monitor_ref, monitor_target, 5)
                .is_none(),
            "link delivery must not fire a monitor entry"
        );
        // A link DOWN must not arm the monitor's recv slot.
        assert!(
            !state.deliver_to_ref(link_ref, link_target, 5),
            "monitor delivery must not arm a link entry"
        );

        // Each fires only through its own path.
        assert!(
            state.deliver_to_ref(monitor_ref, monitor_target, 6),
            "monitor arms"
        );
        assert!(
            state
                .deliver_link_down_to_ref(link_ref, link_target, 5)
                .is_some(),
            "link fires"
        );
    }

    /// The node fan-out fires only still-Pending LINK entries on the dead node;
    /// a link that already received a definitive `CTRL_LINK_DOWN` is untouched
    /// Exactly-once: a definitive remote-exit beats a later partition.
    #[test]
    fn take_link_downs_for_node_fires_pending_links_only() {
        let state = DistMonitorState::new();
        let a_target = location(7, 1);
        let a = state.register_link_watcher(a_target, 100, POLICY_TAG_CRASH_LINKED);
        let _b = state.register_link_watcher(location(7, 2), 200, POLICY_TAG_CRASH_LINKED);
        let other_target = location(9, 3);
        let other = state.register_link_watcher(other_target, 300, POLICY_TAG_CRASH_LINKED);
        // A monitor on the same node must NOT appear in the link fan-out.
        let _m = state.register_watcher(location(7, 4));

        // `a` already received a definitive crash DOWN, authenticated as its own
        // registered peer (node 7).
        assert!(state.deliver_link_down_to_ref(a, a_target, 5).is_some());

        // Connection drop to node 7: only the still-Pending link `b` fires
        // (MonitorLost == -1); `a` is Consumed, `other` is on node 9, the
        // monitor is not a link.
        let downs =
            state.take_link_downs_for_node(NodeId::from_bytes([7; 16]), MONITOR_REASON_LOST);
        assert_eq!(downs.len(), 1, "only the pending link on node 7 fires");
        assert_eq!(downs[0].local_actor_id, 200);
        assert_eq!(downs[0].reason, MONITOR_REASON_LOST);

        // `other` (node 9) untouched; still fires on its own node's drop,
        // authenticated as its own registered peer (node 9).
        assert!(state
            .deliver_link_down_to_ref(other, other_target, 5)
            .is_some());
    }

    /// A non-`CrashLinked` policy is carried through the table verbatim so the
    /// runtime layer can decide (non-fatal); the table itself does not interpret.
    #[test]
    fn link_entry_carries_policy_tag_verbatim() {
        let state = DistMonitorState::new();
        // MonitorLost policy (tag 2) — non-fatal.
        let target = location(7, 1);
        let ref_id = state.register_link_watcher(target, 42, 2);
        let down = state
            .deliver_link_down_to_ref(ref_id, target, 6)
            .expect("link-down fires regardless of policy; the runtime decides fatality");
        assert_eq!(down.policy_tag, 2, "policy tag round-trips verbatim");
        assert_eq!(down.local_actor_id, 42);
    }

    #[test]
    fn recv_blocks_until_concurrent_arm() {
        use std::sync::Arc;
        let state = Arc::new(DistMonitorState::new());
        let target = location(7, 1);
        let ref_id = state.register_watcher(target);

        let state_recv = Arc::clone(&state);
        let handle = std::thread::spawn(move || {
            // Generous timeout; the arm below should wake this well before it.
            state_recv.recv_down(ref_id, Duration::from_secs(5))
        });

        // Give the recv thread time to park on the condvar, then arm.
        std::thread::sleep(Duration::from_millis(50));
        assert!(state.deliver_to_ref(ref_id, target, 6));

        let reason = handle.join().expect("recv thread panicked");
        assert_eq!(reason, 6, "blocked recv must wake with the armed reason");
    }

    /// `remove_watcher` wakes a blocked `recv_down` promptly.
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
        let target = location(7, 1);
        let ref_id = state.register_watcher(target);

        // A sibling ref on another node — must NOT be disturbed by this remove.
        let sibling_ref = state.register_watcher(location(9, 2));

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
            Some(target),
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
