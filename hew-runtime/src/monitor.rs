//! Actor monitors implementation for unidirectional death notifications.
//!
//! In Erlang-style actor systems, monitors are unidirectional: when actor A
//! monitors actor B, if B dies, A receives a DOWN message but does NOT crash.
//! This module implements the monitor table and death notification logic.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
#[cfg(test)]
use std::sync::LazyLock;
#[cfg(test)]
use std::sync::{Arc, Barrier};
use std::sync::{Mutex, PoisonError};

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::lifetime::poison_safe::PoisonSafeRw;
use crate::mailbox;
use crate::node_identity::{Location, NodeId};
use crate::supervisor::SYS_MSG_DOWN;

/// Number of shards for monitor table to reduce contention.
const MONITOR_SHARDS: usize = 16;

/// Entry in the monitor table.
#[derive(Debug, Clone)]
struct MonitorEntry {
    /// Unique reference ID for this monitor (for demonitor).
    ref_id: u64,
}

/// Shard in the monitor table.
#[derive(Debug)]
struct MonitorShard {
    /// Maps `monitored_actor_id` -> Vec of monitors watching that actor.
    monitors: HashMap<u64, Vec<MonitorEntry>>,
    /// Maps `ref_id` -> monitored actor id for O(1) local-index cleanup.
    ref_to_monitor: HashMap<u64, u64>,
    /// Tracks actors whose terminal monitor sweep has already completed.
    terminal_reasons: HashMap<u64, TerminalMonitorReason>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TerminalMonitorReason {
    state: i32,
    crash_kind: u32,
}

/// Per-runtime actor-monitor state: the sharded monitor table plus the
/// reference-id counter for entries in that table.
///
/// Was the process-global `MONITOR_TABLE` plus `MONITOR_REF_COUNTER`. Each
/// runtime now owns its own shards as `RuntimeInner.monitors`, resolved through
/// [`monitor_state`], so two runtimes can monitor identical actor ids without
/// sharing entries or DOWN delivery.
pub(crate) struct MonitorState {
    table: [PoisonSafeRw<MonitorShard>; MONITOR_SHARDS],
    /// Sole watcher-side observation authority for local/remote monitors and links.
    observations: Mutex<HashMap<u64, ObservationEntry>>,
    /// Target-side remote registrations used only to fan terminal frames to peers.
    remote_targets: Mutex<HashMap<u64, Vec<RemoteWatcher>>>,
    ref_counter: AtomicU64,
    ref_exhausted: AtomicBool,
}

/// Reason sentinel used when a remote target becomes unreachable.
pub const MONITOR_REASON_LOST: i32 = -1;

/// What one watcher-side observation does when its target terminates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherAction {
    Monitor { watcher_actor_id: u64 },
    Link { local_actor_id: u64, policy_tag: u8 },
}

/// Target identity for one observation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObservationTarget {
    Local(u64),
    Remote(Location),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ObservationEntry {
    target: ObservationTarget,
    action: WatcherAction,
}

/// A claimed remote monitor terminal action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MonitorDown {
    pub monitor_id: u64,
    pub watcher_actor_id: u64,
    pub target: Location,
}

/// A claimed cross-node link terminal action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LinkDown {
    pub local_actor_id: u64,
    pub remote_target_serial: u64,
    pub reason: i32,
    pub policy_tag: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RemoteWatcher {
    watcher: Location,
    ref_id: u64,
    is_link: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct RemoteWatcherTarget {
    pub watcher: Location,
    pub ref_id: u64,
    pub is_link: bool,
}

impl MonitorState {
    /// Construct an empty monitor table for a fresh runtime.
    pub(crate) fn new() -> Self {
        Self {
            table: std::array::from_fn(|_| {
                PoisonSafeRw::new(MonitorShard {
                    monitors: HashMap::new(),
                    ref_to_monitor: HashMap::new(),
                    terminal_reasons: HashMap::new(),
                })
            }),
            observations: Mutex::new(HashMap::new()),
            remote_targets: Mutex::new(HashMap::new()),
            ref_counter: AtomicU64::new(1),
            ref_exhausted: AtomicBool::new(false),
        }
    }

    /// Allocate one non-zero observation id without wrap or reuse.
    pub(crate) fn next_observation_id(&self) -> Option<u64> {
        loop {
            if self.ref_exhausted.load(Ordering::Acquire) {
                return None;
            }
            let current = self.ref_counter.load(Ordering::Relaxed);
            if current == u64::MAX {
                return self
                    .ref_exhausted
                    .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                    .ok()
                    .map(|_| current);
            }
            if self
                .ref_counter
                .compare_exchange_weak(current, current + 1, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                return Some(current);
            }
        }
    }

    pub(crate) fn register_remote_monitor(
        &self,
        target: Location,
        watcher_actor_id: u64,
    ) -> Option<u64> {
        self.register_remote_observation(target, WatcherAction::Monitor { watcher_actor_id })
    }

    pub(crate) fn register_link_watcher(
        &self,
        target: Location,
        local_actor_id: u64,
        policy_tag: u8,
    ) -> Option<u64> {
        self.register_remote_observation(
            target,
            WatcherAction::Link {
                local_actor_id,
                policy_tag,
            },
        )
    }

    fn register_remote_observation(&self, target: Location, action: WatcherAction) -> Option<u64> {
        let monitor_id = self.next_observation_id()?;
        self.observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(
                monitor_id,
                ObservationEntry {
                    target: ObservationTarget::Remote(target),
                    action,
                },
            );
        Some(monitor_id)
    }

    /// Remove one setup-time remote observation without sending a demonitor.
    pub(crate) fn remove_remote_observation(&self, monitor_id: u64) -> Option<Location> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).copied()?;
        let ObservationTarget::Remote(target) = entry.target else {
            return None;
        };
        observations.remove(&monitor_id);
        Some(target)
    }

    pub(crate) fn sole_remote_monitor_for_watcher(&self, watcher_actor_id: u64) -> Option<u64> {
        let observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let mut matches = observations.iter().filter_map(|(monitor_id, entry)| {
            matches!(
                entry.action,
                WatcherAction::Monitor {
                    watcher_actor_id: entry_watcher
                } if entry_watcher == watcher_actor_id
            )
            .then_some(*monitor_id)
        });
        let monitor_id = matches.next()?;
        matches.next().is_none().then_some(monitor_id)
    }

    pub(crate) fn deliver_monitor_to_ref(
        &self,
        monitor_id: u64,
        target: Location,
    ) -> Option<MonitorDown> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).copied()?;
        match (entry.target, entry.action) {
            (
                ObservationTarget::Remote(entry_target),
                WatcherAction::Monitor { watcher_actor_id },
            ) if entry_target == target => {
                observations.remove(&monitor_id);
                Some(MonitorDown {
                    monitor_id,
                    watcher_actor_id,
                    target,
                })
            }
            _ => None,
        }
    }

    pub(crate) fn deliver_link_down_to_ref(
        &self,
        monitor_id: u64,
        target: Location,
        reason: i32,
    ) -> Option<LinkDown> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).copied()?;
        match (entry.target, entry.action) {
            (
                ObservationTarget::Remote(entry_target),
                WatcherAction::Link {
                    local_actor_id,
                    policy_tag,
                },
            ) if entry_target == target => {
                observations.remove(&monitor_id);
                Some(LinkDown {
                    local_actor_id,
                    remote_target_serial: target.slot(),
                    reason,
                    policy_tag,
                })
            }
            _ => None,
        }
    }

    pub(crate) fn take_monitor_downs_for_node(&self, remote_node: NodeId) -> Vec<MonitorDown> {
        self.take_monitor_downs_matching(|target| target.node() == remote_node)
    }

    pub(crate) fn take_monitor_downs_for_session(
        &self,
        remote_node: NodeId,
        session_incarnation: u32,
    ) -> Vec<MonitorDown> {
        self.take_monitor_downs_matching(|target| {
            target.node() == remote_node && target.incarnation() == session_incarnation
        })
    }

    pub(crate) fn drain_remote_observations_for_shutdown(&self) {
        for down in self.take_monitor_downs_matching(|_| true) {
            self.enqueue_shutdown(down);
        }
        for down in self.take_link_downs_matching(|_| true, MONITOR_REASON_LOST) {
            crate::link::deliver_cross_node_link_exit(
                down.local_actor_id,
                down.remote_target_serial,
                down.reason,
                down.policy_tag,
            );
        }
    }

    fn take_monitor_downs_matching(&self, matches: impl Fn(Location) -> bool) -> Vec<MonitorDown> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let ids: Vec<_> = observations
            .iter()
            .filter_map(|(&monitor_id, entry)| match (entry.target, entry.action) {
                (
                    ObservationTarget::Remote(target),
                    WatcherAction::Monitor { watcher_actor_id },
                ) if matches(target) => Some((monitor_id, watcher_actor_id, target)),
                _ => None,
            })
            .collect();
        for (monitor_id, _, _) in &ids {
            observations.remove(monitor_id);
        }
        ids.into_iter()
            .map(|(monitor_id, watcher_actor_id, target)| MonitorDown {
                monitor_id,
                watcher_actor_id,
                target,
            })
            .collect()
    }

    pub(crate) fn take_link_downs_for_node(
        &self,
        remote_node: NodeId,
        reason: i32,
    ) -> Vec<LinkDown> {
        self.take_link_downs_matching(|target| target.node() == remote_node, reason)
    }

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
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let ids: Vec<_> = observations
            .iter()
            .filter_map(|(&monitor_id, entry)| match (entry.target, entry.action) {
                (
                    ObservationTarget::Remote(target),
                    WatcherAction::Link {
                        local_actor_id,
                        policy_tag,
                    },
                ) if matches(target) => Some((
                    monitor_id,
                    LinkDown {
                        local_actor_id,
                        remote_target_serial: target.slot(),
                        reason,
                        policy_tag,
                    },
                )),
                _ => None,
            })
            .collect();
        for (monitor_id, _) in &ids {
            observations.remove(monitor_id);
        }
        ids.into_iter().map(|(_, down)| down).collect()
    }

    pub(crate) fn register_remote_watcher(
        &self,
        target_serial: u64,
        watcher: Location,
        monitor_id: u64,
    ) {
        self.register_remote_watcher_kind(target_serial, watcher, monitor_id, false);
    }

    pub(crate) fn register_remote_link_watcher(
        &self,
        target_serial: u64,
        watcher: Location,
        monitor_id: u64,
    ) {
        self.register_remote_watcher_kind(target_serial, watcher, monitor_id, true);
    }

    fn register_remote_watcher_kind(
        &self,
        target_serial: u64,
        watcher: Location,
        monitor_id: u64,
        is_link: bool,
    ) {
        let mut targets = self
            .remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let watchers = targets.entry(target_serial).or_default();
        if !watchers
            .iter()
            .any(|entry| entry.watcher == watcher && entry.ref_id == monitor_id)
        {
            watchers.push(RemoteWatcher {
                watcher,
                ref_id: monitor_id,
                is_link,
            });
        }
    }

    pub(crate) fn remove_remote_watcher(
        &self,
        target_serial: u64,
        watcher: Location,
        monitor_id: u64,
    ) {
        let mut targets = self
            .remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        if let Some(watchers) = targets.get_mut(&target_serial) {
            watchers.retain(|entry| !(entry.watcher == watcher && entry.ref_id == monitor_id));
            if watchers.is_empty() {
                targets.remove(&target_serial);
            }
        }
    }

    pub(crate) fn take_remote_watchers(&self, target_serial: u64) -> Vec<RemoteWatcherTarget> {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .remove(&target_serial)
            .map(|watchers| {
                watchers
                    .into_iter()
                    .map(|entry| RemoteWatcherTarget {
                        watcher: entry.watcher,
                        ref_id: entry.ref_id,
                        is_link: entry.is_link,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    pub(crate) fn has_remote_watchers(&self, target_serial: u64) -> bool {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .get(&target_serial)
            .is_some_and(|watchers| !watchers.is_empty())
    }

    pub(crate) fn prune_remote_watchers_for_node(&self, dead_watcher: NodeId) -> usize {
        self.prune_remote_watchers_matching(|watcher| watcher.node() == dead_watcher)
    }

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
        let mut targets = self
            .remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let mut pruned = 0;
        targets.retain(|_, watchers| {
            let before = watchers.len();
            watchers.retain(|entry| !matches(entry.watcher));
            pruned += before - watchers.len();
            !watchers.is_empty()
        });
        pruned
    }

    pub(crate) fn remote_watcher_count(&self) -> usize {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .values()
            .map(Vec::len)
            .sum()
    }

    pub(crate) fn pending_observation_count(&self) -> usize {
        self.observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .len()
    }

    #[expect(
        clippy::unused_self,
        reason = "mailbox delivery remains namespaced under the sole MonitorState authority"
    )]
    pub(crate) fn enqueue_down(
        &self,
        watcher_actor_id: u64,
        monitor_id: u64,
        target: Location,
        reason: i32,
        crash_kind: u32,
    ) {
        deliver_down_message(
            watcher_actor_id,
            HewDownMessage::remote(monitor_id, target, reason, crash_kind),
        );
    }

    #[expect(
        clippy::unused_self,
        reason = "mailbox delivery remains namespaced under the sole MonitorState authority"
    )]
    pub(crate) fn enqueue_lost(&self, down: MonitorDown) {
        deliver_down_message(
            down.watcher_actor_id,
            HewDownMessage::remote_lost(down.monitor_id, down.target),
        );
    }

    #[expect(
        clippy::unused_self,
        reason = "mailbox delivery remains namespaced under the sole MonitorState authority"
    )]
    pub(crate) fn enqueue_shutdown(&self, down: MonitorDown) {
        deliver_down_message(
            down.watcher_actor_id,
            HewDownMessage::remote_shutdown(down.monitor_id, down.target),
        );
    }
}

#[inline]
fn monitor_state() -> &'static MonitorState {
    &crate::runtime::rt_current().monitors
}

/// Resolve the per-runtime monitor state, tolerating a missing runtime.
///
/// The death and cleanup reads (`notify_monitors_on_death`,
/// `remove_all_monitors_for_actor`, `has_monitors_for_actor`) are reachable from
/// `hew_actor_trap` and the actor-free teardown, which run with OR without an
/// installed runtime — the supervisor-cascade unit tests trap a
/// manually-constructed actor that never installed a runtime guard. When the
/// monitor table lived in a process-global `LazyLock` these paths always found
/// an (empty) table and silently did nothing; moving it behind the fail-closed
/// [`monitor_state`] would instead abort "no runtime installed".
///
/// This resolver restores that tolerance: with no runtime there is no per-runtime
/// table, so there are no monitors to notify, remove, or count, and the caller
/// no-ops — exactly the old empty-global-table behaviour. The registration
/// surface (`hew_actor_monitor` / `hew_actor_demonitor`) keeps the fail-closed
/// [`monitor_state`], since recording a monitor requires its owning runtime
/// (mirroring the metrics register/mutate vs read split on
/// [`crate::runtime::rt_current_opt`]).
#[inline]
fn monitor_state_opt() -> Option<&'static MonitorState> {
    crate::runtime::rt_current_opt().map(|rt| &rt.monitors)
}

#[cfg(test)]
#[inline]
fn monitor_table_for_test() -> &'static [PoisonSafeRw<MonitorShard>; MONITOR_SHARDS] {
    &monitor_state().table
}

#[cfg(test)]
fn monitor_shard_for_runtime_test(
    rt: &crate::runtime::RuntimeInner,
    actor_id: u64,
) -> &PoisonSafeRw<MonitorShard> {
    &rt.monitors.table[get_shard_index(actor_id)]
}

/// Get shard index for an actor ID.
fn get_shard_index(actor_id: u64) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "shard index is bounded by MONITOR_SHARDS (16)"
    )]
    {
        (actor_id as usize) % MONITOR_SHARDS
    }
}

fn terminal_monitor_reason(actor_state: i32) -> Option<i32> {
    if actor_state == HewActorState::Stopped as i32 || actor_state == HewActorState::Crashed as i32
    {
        Some(actor_state)
    } else {
        None
    }
}

const DOWN_TARGET_LOCAL: u32 = 0;
const DOWN_TARGET_REMOTE: u32 = 1;
const DOWN_REASON_EXITED: u32 = 0;
const DOWN_REASON_CRASHED: u32 = 1;
const DOWN_REASON_MONITOR_LOST: u32 = 2;
const DOWN_REASON_LOCAL_SHUTDOWN: u32 = 3;

/// Fixed actor-mailbox DOWN payload.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewDownMessage {
    pub monitor_id: u64,
    pub target_kind: u32,
    pub reason_kind: u32,
    pub node_hi: u64,
    pub node_lo: u64,
    pub slot: u64,
    pub session_incarnation: u32,
    pub crash_kind: u32,
}

impl HewDownMessage {
    fn local(monitor_id: u64, actor_id: u64, terminal: i32, crash_kind: u32) -> Self {
        let reason_kind = if terminal == HewActorState::Stopped as i32 {
            DOWN_REASON_EXITED
        } else {
            DOWN_REASON_CRASHED
        };
        Self {
            monitor_id,
            target_kind: DOWN_TARGET_LOCAL,
            reason_kind,
            node_hi: 0,
            node_lo: 0,
            slot: crate::pid::hew_pid_serial(actor_id),
            session_incarnation: 0,
            crash_kind: if reason_kind == DOWN_REASON_CRASHED {
                crash_kind
            } else {
                0
            },
        }
    }

    pub(crate) fn remote(
        monitor_id: u64,
        target: Location,
        terminal: i32,
        crash_kind: u32,
    ) -> Self {
        let reason_kind = if terminal == HewActorState::Stopped as i32 {
            DOWN_REASON_EXITED
        } else {
            DOWN_REASON_CRASHED
        };
        Self {
            monitor_id,
            target_kind: DOWN_TARGET_REMOTE,
            reason_kind,
            node_hi: target.node().hi(),
            node_lo: target.node().lo(),
            slot: target.slot(),
            session_incarnation: target.incarnation(),
            crash_kind: if reason_kind == DOWN_REASON_CRASHED {
                crash_kind
            } else {
                0
            },
        }
    }

    pub(crate) fn remote_lost(monitor_id: u64, target: Location) -> Self {
        Self {
            monitor_id,
            target_kind: DOWN_TARGET_REMOTE,
            reason_kind: DOWN_REASON_MONITOR_LOST,
            node_hi: target.node().hi(),
            node_lo: target.node().lo(),
            slot: target.slot(),
            session_incarnation: target.incarnation(),
            crash_kind: 0,
        }
    }

    pub(crate) fn remote_shutdown(monitor_id: u64, target: Location) -> Self {
        Self {
            reason_kind: DOWN_REASON_LOCAL_SHUTDOWN,
            ..Self::remote_lost(monitor_id, target)
        }
    }
}

const _: () = {
    assert!(std::mem::size_of::<HewDownMessage>() == 48);
    assert!(std::mem::offset_of!(HewDownMessage, monitor_id) == 0);
    assert!(std::mem::offset_of!(HewDownMessage, target_kind) == 8);
    assert!(std::mem::offset_of!(HewDownMessage, reason_kind) == 12);
    assert!(std::mem::offset_of!(HewDownMessage, node_hi) == 16);
    assert!(std::mem::offset_of!(HewDownMessage, node_lo) == 24);
    assert!(std::mem::offset_of!(HewDownMessage, slot) == 32);
    assert!(std::mem::offset_of!(HewDownMessage, session_incarnation) == 40);
    assert!(std::mem::offset_of!(HewDownMessage, crash_kind) == 44);
};

pub(crate) fn deliver_down_message(watcher_actor_id: u64, down_data: HewDownMessage) {
    let Some(monitoring_actor) =
        crate::lifetime::live_actors::get_actor_ptr_by_id(watcher_actor_id)
    else {
        return;
    };
    let mut wake_actor = false;
    let mut enqueue_failed = false;
    crate::actor::with_live_actor_by_id(
        watcher_actor_id,
        monitoring_actor,
        |monitoring_actor_ref| {
            let mailbox = monitoring_actor_ref.mailbox.cast::<mailbox::HewMailbox>();

            if !mailbox.is_null() {
                let data_ptr = (&raw const down_data).cast::<c_void>();
                let data_size = std::mem::size_of::<HewDownMessage>();

                // SAFETY: LIVE_ACTORS keeps the monitoring actor and mailbox live.
                let sent = unsafe {
                    mailbox::hew_mailbox_send_sys_checked(
                        mailbox,
                        SYS_MSG_DOWN,
                        data_ptr.cast_mut(),
                        data_size,
                    )
                };
                if !sent {
                    enqueue_failed = true;
                    return;
                }

                if monitoring_actor_ref
                    .actor_state
                    .compare_exchange(
                        HewActorState::Idle as i32,
                        HewActorState::Runnable as i32,
                        std::sync::atomic::Ordering::AcqRel,
                        std::sync::atomic::Ordering::Acquire,
                    )
                    .is_ok()
                {
                    monitoring_actor_ref
                        .idle_count
                        .store(0, std::sync::atomic::Ordering::Relaxed);
                    monitoring_actor_ref
                        .hibernating
                        .store(0, std::sync::atomic::Ordering::Relaxed);
                    wake_actor = true;
                }
            }

            #[cfg(test)]
            run_notify_monitors_hook();
        },
    );
    if wake_actor {
        crate::scheduler::sched_enqueue(monitoring_actor);
    }
    if enqueue_failed {
        crate::actor::with_live_actor_by_id(
            watcher_actor_id,
            monitoring_actor,
            |_monitoring_actor_ref| {
                // SAFETY: the live-actor authority validated the pointer.
                unsafe {
                    crate::actor::hew_actor_trap(
                        monitoring_actor,
                        crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED,
                    );
                }
            },
        );
    }
}

/// Create a monitor: watcher monitors target.
///
/// Returns zero and writes a unique monitor id on success. Non-zero returns are
/// one-based `MonitorError` discriminants.
///
/// # Safety
///
/// `watcher`, `target`, and `out_monitor_id` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_monitor(
    watcher: *mut HewActor,
    target: *mut HewActor,
    out_monitor_id: *mut u64,
) -> i32 {
    const INVALID_TARGET: i32 = 2;
    const RESOURCE_EXHAUSTED: i32 = 11;

    if watcher.is_null() || target.is_null() || out_monitor_id.is_null() {
        return INVALID_TARGET;
    }

    // SAFETY: Caller guarantees both pointers are valid.
    let watcher_ref = unsafe { &*watcher };
    // SAFETY: Caller guarantees both pointers are valid.
    let target_ref = unsafe { &*target };

    let target_id = target_ref.id;

    // Generate one shared local/remote observation ID.
    let state = monitor_state();
    let Some(ref_id) = state.next_observation_id() else {
        crate::set_last_error("hew_actor_monitor: monitor id space exhausted");
        return RESOURCE_EXHAUSTED;
    };

    let monitor_entry = MonitorEntry { ref_id };
    let observation = ObservationEntry {
        target: ObservationTarget::Local(target_id),
        action: WatcherAction::Monitor {
            watcher_actor_id: watcher_ref.id,
        },
    };

    let shard_index = get_shard_index(target_id);
    let terminal_reason = state.table[shard_index].access(|shard| {
        if let Some(&reason) = shard.terminal_reasons.get(&target_id) {
            Some(reason)
        } else if let Some(reason) =
            terminal_monitor_reason(target_ref.actor_state.load(Ordering::Acquire))
        {
            Some(TerminalMonitorReason {
                state: reason,
                crash_kind: if reason == HewActorState::Crashed as i32 {
                    crate::internal::types::CrashKind::tag_from_error_code(
                        target_ref.error_code.load(Ordering::Acquire),
                    )
                    .cast_unsigned()
                } else {
                    0
                },
            })
        } else {
            shard
                .monitors
                .entry(target_id)
                .or_default()
                .push(monitor_entry.clone());

            shard.ref_to_monitor.insert(ref_id, target_id);
            state
                .observations
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .insert(ref_id, observation);
            None
        }
    });

    if let Some(reason) = terminal_reason {
        let down = HewDownMessage::local(ref_id, target_id, reason.state, reason.crash_kind);
        deliver_down_message(watcher_ref.id, down);
    }

    // SAFETY: caller provided a writable out pointer; write only on success.
    unsafe { *out_monitor_id = ref_id };
    0
}

/// Rust convenience wrapper around the explicit status/out-id monitor ABI.
///
/// # Safety
///
/// `watcher` and `target` must satisfy [`hew_actor_monitor`]'s pointer contract.
///
/// # Errors
///
/// Returns the non-zero monitor setup status when registration fails.
pub unsafe fn register_actor_monitor(
    watcher: *mut HewActor,
    target: *mut HewActor,
) -> Result<u64, i32> {
    let mut monitor_id = 0;
    // SAFETY: inherited from the caller; the local out pointer is writable.
    let status = unsafe { hew_actor_monitor(watcher, target, &raw mut monitor_id) };
    if status == 0 {
        Ok(monitor_id)
    } else {
        Err(status)
    }
}

/// Remove a monitor by its reference ID.
///
/// **Silent-Ok contract** — this function is idempotent and never panics:
///
/// - `ref_id == 0`: early-returns immediately (no-op).
/// - Unknown `ref_id` (already removed, or never registered): scans all shards,
///   finds nothing, and returns silently.
/// - Valid live `ref_id`: removes the entry from both `ref_to_monitor` and the
///   target's `monitors` list, then returns.
///
/// This invariant will be relied upon by the upcoming `MonitorRef::close()` and
/// `MonitorRef`'s `Drop` implementation (landing in M.1): calling
/// `hew_actor_demonitor` twice with the same `ref_id` (explicit `close()`
/// followed by scope-exit drop) will always be safe.  The second call finds no
/// entry and returns silently — no double-free, no crash.
///
/// When a target actor is freed, `remove_all_monitors_for_actor` sweeps its
/// `ref_to_monitor` entries first. A subsequent `hew_actor_demonitor` call for
/// any of those stale `ref_id` values falls through the unknown-id path above
/// and returns silently.
#[no_mangle]
pub extern "C" fn hew_actor_demonitor(ref_id: u64) {
    if ref_id == 0 {
        return;
    }

    let Some(state) = monitor_state_opt() else {
        return;
    };
    let entry = state
        .observations
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .remove(&ref_id);
    let Some(entry) = entry else {
        return;
    };
    match entry.target {
        ObservationTarget::Local(target_id) => {
            let shard_index = get_shard_index(target_id);
            state.table[shard_index].access(|shard| {
                shard.ref_to_monitor.remove(&ref_id);
                if let Some(monitor_list) = shard.monitors.get_mut(&target_id) {
                    monitor_list.retain(|entry| entry.ref_id != ref_id);
                    if monitor_list.is_empty() {
                        shard.monitors.remove(&target_id);
                    }
                }
            });
        }
        ObservationTarget::Remote(target) => {
            if let WatcherAction::Monitor { watcher_actor_id } = entry.action {
                crate::hew_node::send_remote_demonitor(ref_id, target, watcher_actor_id);
            }
        }
    }
}

/// Send DOWN notifications to all actors monitoring the dead actor.
///
/// This function is called from `hew_actor_trap` after the actor has
/// transitioned to a terminal state. It removes all monitors for the
/// dead actor and sends DOWN messages to all monitoring actors.
pub(crate) fn notify_monitors_on_death(actor_id: u64, reason: i32, crash_kind: u32) {
    let shard_index = get_shard_index(actor_id);
    // No runtime → no per-runtime monitor table → no monitors to notify. This
    // path is reachable from `hew_actor_trap` without an installed runtime
    // (supervisor-cascade unit tests), and the old process-global table simply
    // had no entries to sweep there. Tolerate it as before.
    let Some(state) = monitor_state_opt() else {
        return;
    };

    // Take all monitors for this actor ID.
    let monitors = state.table[shard_index].access(|shard| {
        let monitors = shard.monitors.remove(&actor_id).unwrap_or_default();
        shard.terminal_reasons.insert(
            actor_id,
            TerminalMonitorReason {
                state: reason,
                crash_kind,
            },
        );

        // Also remove from ref_to_monitor mapping
        for monitor in &monitors {
            shard.ref_to_monitor.remove(&monitor.ref_id);
        }

        monitors
    });

    let claimed: Vec<_> = {
        let mut observations = state
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        monitors
            .into_iter()
            .filter_map(|monitor| {
                let entry = observations.remove(&monitor.ref_id)?;
                match (entry.target, entry.action) {
                    (
                        ObservationTarget::Local(target),
                        WatcherAction::Monitor { watcher_actor_id },
                    ) if target == actor_id => Some((monitor.ref_id, watcher_actor_id)),
                    _ => None,
                }
            })
            .collect()
    };

    for (monitor_id, watcher_actor_id) in claimed {
        deliver_down_message(
            watcher_actor_id,
            HewDownMessage::local(monitor_id, actor_id, reason, crash_kind),
        );
    }

    // Cross-node terminal sweep: if any REMOTE node monitors this
    // locally-dying actor, fan out a CTRL_MONITOR_DOWN carrying the same
    // terminal reason. The distributed table keys on the actor's serial; the
    // fan-out is fail-closed and no-ops when no remote watcher exists or no
    // node/conn-mgr is installed (R7), so the local sweep above is unaffected.
    crate::hew_node::fan_out_remote_monitor_down(
        crate::pid::hew_pid_serial(actor_id),
        reason,
        crash_kind,
    );
}

#[cfg(test)]
type NotifyMonitorsHook = Option<(Arc<Barrier>, Arc<Barrier>)>;

#[cfg(test)]
static NOTIFY_MONITORS_HOOK: LazyLock<Mutex<NotifyMonitorsHook>> =
    LazyLock::new(|| Mutex::new(None));

#[cfg(test)]
fn run_notify_monitors_hook() {
    let hook = {
        let guard = NOTIFY_MONITORS_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        guard.clone()
    };

    if let Some((entered, release)) = hook {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
struct NotifyMonitorsHookGuard;

#[cfg(test)]
impl NotifyMonitorsHookGuard {
    fn install(entered: Arc<Barrier>, release: Arc<Barrier>) -> Self {
        let mut guard = NOTIFY_MONITORS_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = Some((entered, release));
        Self
    }
}

#[cfg(test)]
impl Drop for NotifyMonitorsHookGuard {
    fn drop(&mut self) {
        let mut guard = NOTIFY_MONITORS_HOOK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *guard = None;
    }
}

/// Remove all monitor entries for a dying actor — both as a monitored target
/// and as a monitoring watcher — from all shards.
///
/// Called from `hew_actor_free` before deallocation to prevent
/// `notify_monitors_on_death` from dereferencing freed memory.
pub(crate) fn remove_all_monitors_for_actor(actor_id: u64, _actor_addr: *mut HewActor) {
    // Like `notify_monitors_on_death`, this teardown read runs from the actor
    // free path with OR without an installed runtime. No runtime → no table →
    // no entries to scrub, matching the old empty-global-table no-op.
    let Some(state) = monitor_state_opt() else {
        return;
    };

    // Remove any monitors-on-this-actor entry from its shard.
    let own_shard = get_shard_index(actor_id);
    let target_refs = state.table[own_shard].access(|shard| {
        shard.terminal_reasons.remove(&actor_id);
        let monitors = shard.monitors.remove(&actor_id).unwrap_or_default();
        for m in &monitors {
            shard.ref_to_monitor.remove(&m.ref_id);
        }
        monitors
            .into_iter()
            .map(|monitor| monitor.ref_id)
            .collect::<Vec<_>>()
    });
    let watcher_local_refs: Vec<(u64, u64)> = {
        let mut observations = state
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        for ref_id in target_refs {
            observations.remove(&ref_id);
        }
        let refs: Vec<_> = observations
            .iter()
            .filter_map(|(&ref_id, entry)| {
                let owns = matches!(
                    entry.action,
                    WatcherAction::Monitor { watcher_actor_id } if watcher_actor_id == actor_id
                ) || matches!(
                    entry.action,
                    WatcherAction::Link { local_actor_id, .. } if local_actor_id == actor_id
                );
                if !owns {
                    return None;
                }
                match entry.target {
                    ObservationTarget::Local(target) => Some((ref_id, target)),
                    ObservationTarget::Remote(_) => Some((ref_id, 0)),
                }
            })
            .collect();
        for (ref_id, _) in &refs {
            observations.remove(ref_id);
        }
        refs
    };

    for (ref_id, target_id) in watcher_local_refs {
        if target_id != 0 {
            state.table[get_shard_index(target_id)].access(|shard| {
                shard.ref_to_monitor.remove(&ref_id);
                if let Some(monitors) = shard.monitors.get_mut(&target_id) {
                    monitors.retain(|monitor| monitor.ref_id != ref_id);
                    if monitors.is_empty() {
                        shard.monitors.remove(&target_id);
                    }
                }
            });
        }
    }
}

/// Returns true if any monitor entries reference the given actor (as monitored
/// target by ID or as watcher by address).
#[cfg(test)]
pub(crate) fn has_monitors_for_actor(actor_id: u64, _actor_addr: *mut HewActor) -> bool {
    let state = monitor_state();
    state
        .observations
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .values()
        .any(|entry| {
            matches!(entry.target, ObservationTarget::Local(target) if target == actor_id)
                || matches!(
                    entry.action,
                    WatcherAction::Monitor { watcher_actor_id } if watcher_actor_id == actor_id
                )
                || matches!(
                    entry.action,
                    WatcherAction::Link { local_actor_id, .. } if local_actor_id == actor_id
                )
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, AtomicPtr};

    unsafe extern "C-unwind" fn noop_dispatch(
        _ctx: *mut crate::execution_context::HewExecutionContext,
        _state: *mut c_void,
        _msg_type: i32,
        _data: *mut c_void,
        _size: usize,
        _borrow_mode: i32,
    ) -> *mut c_void {
        std::ptr::null_mut()
    }

    unsafe fn monitor_for_test(watcher: *mut HewActor, target: *mut HewActor) -> u64 {
        let mut monitor_id = 0;
        // SAFETY: caller supplies the actor pointers; the out pointer is writable.
        let status = unsafe { hew_actor_monitor(watcher, target, &raw mut monitor_id) };
        assert_eq!(status, 0);
        monitor_id
    }

    fn create_test_actor(id: u64) -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
            id,
            state: std::ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: std::ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Idle as i32),
            budget: AtomicI32::new(0),
            init_state: std::ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
            terminate_fn: None,
            state_drop_fn: None,
            state_clone_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
            dispatch_active: AtomicBool::new(false),
            error_code: AtomicI32::new(0),
            supervisor: std::ptr::null_mut(),
            supervisor_child_index: 0,
            priority: AtomicI32::new(1),
            reductions: AtomicI32::new(0),
            idle_count: AtomicI32::new(0),
            hibernation_threshold: AtomicI32::new(0),
            hibernating: AtomicI32::new(0),
            prof_messages_processed: AtomicU64::new(0),
            prof_processing_time_ns: AtomicU64::new(0),
            arena: std::ptr::null_mut(),
            suspended_cont: AtomicPtr::new(std::ptr::null_mut()),
            cont_tag: AtomicI32::new(crate::internal::types::ContTag::Empty as i32),
            pending_wake: AtomicBool::new(false),
            suspended_reply_channel: AtomicPtr::new(std::ptr::null_mut()),
            suspended_cancel_token: AtomicPtr::new(std::ptr::null_mut()),
            runtime_id: crate::runtime_id::RuntimeId::DEFAULT,
            runtime: std::ptr::null(),
            send_pin_count: std::sync::atomic::AtomicU32::new(0),
            gen_sink: AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    /// Tiny Rust-side model of the Hew `MonitorRef` wrapper.
    ///
    /// `close(self)` performs an explicit demonitor, then `Drop` runs on the
    /// consumed value at scope-exit from `close()`, producing the second call
    /// that the Hew lowering relies on being idempotent.
    struct TestMonitorRef {
        ref_id: u64,
    }

    impl TestMonitorRef {
        fn new(ref_id: u64) -> Self {
            Self { ref_id }
        }

        fn close(self) {
            hew_actor_demonitor(self.ref_id);
        }
    }

    impl Drop for TestMonitorRef {
        fn drop(&mut self) {
            hew_actor_demonitor(self.ref_id);
        }
    }

    struct MonitorTestGuard {
        _enter: crate::runtime::EnterGuard,
        _rt: Box<crate::runtime::RuntimeInner>,
    }

    fn guard() -> MonitorTestGuard {
        let rt = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));
        // SAFETY: the guard owns `rt` and drops `_enter` before it, so the
        // entered runtime outlives the thread-local selection that names it.
        let enter = unsafe { crate::runtime::enter(&rt) };
        MonitorTestGuard {
            _enter: enter,
            _rt: rt,
        }
    }

    #[test]
    fn test_monitor_creation_and_demonitor() {
        let _g = guard();
        // Use unique IDs to avoid collisions with parallel tests.
        let watcher_id = 10_100;
        let target_id = 10_200;
        let mut watcher = create_test_actor(watcher_id);
        let mut target = create_test_actor(target_id);

        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // Create monitor
        // SAFETY: Both pointers are valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };

        assert_ne!(ref_id, 0);

        // Verify monitor exists
        let shard_index = get_shard_index(target_id);
        monitor_table_for_test()[shard_index].read_access(|shard| {
            let monitors = shard
                .monitors
                .get(&target_id)
                .expect("monitor should exist");
            // Find our specific monitor entry.
            let our_monitor = monitors.iter().find(|m| m.ref_id == ref_id);
            assert!(our_monitor.is_some(), "our monitor entry should exist");
            assert!(shard.ref_to_monitor.contains_key(&ref_id));
        });
        let observation = monitor_state()
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .get(&ref_id)
            .copied();
        assert_eq!(
            observation,
            Some(ObservationEntry {
                target: ObservationTarget::Local(target_id),
                action: WatcherAction::Monitor {
                    watcher_actor_id: watcher_id,
                },
            })
        );

        // Remove monitor
        hew_actor_demonitor(ref_id);

        // Verify monitor is removed
        monitor_table_for_test()[shard_index].read_access(|shard| {
            assert!(
                !shard.monitors.contains_key(&target_id)
                    || shard
                        .monitors
                        .get(&target_id)
                        .is_none_or(std::vec::Vec::is_empty)
            );
            assert!(!shard.ref_to_monitor.contains_key(&ref_id));
        });
    }

    #[test]
    fn test_multiple_monitors_same_target() {
        let _g = guard();
        // Use unique IDs to avoid collisions with parallel tests.
        let mut watcher1 = create_test_actor(20_100);
        let mut watcher2 = create_test_actor(20_110);
        let mut target = create_test_actor(20_200);

        let watcher1_ptr = &raw mut watcher1;
        let watcher2_ptr = &raw mut watcher2;
        let target_ptr = &raw mut target;

        // Create two monitors for same target
        // SAFETY: Both pointers are valid stack-allocated test actors.
        let ref_id1 = unsafe { monitor_for_test(watcher1_ptr, target_ptr) };
        // SAFETY: Both pointers are valid stack-allocated test actors.
        let ref_id2 = unsafe { monitor_for_test(watcher2_ptr, target_ptr) };

        assert_ne!(ref_id1, ref_id2);

        // Verify both monitors exist
        let shard_index = get_shard_index(20_200);
        monitor_table_for_test()[shard_index].read_access(|shard| {
            let monitors = shard.monitors.get(&20_200).expect("monitors should exist");
            assert_eq!(monitors.len(), 2);
        });

        // Remove first monitor
        hew_actor_demonitor(ref_id1);

        // Verify only second monitor remains
        monitor_table_for_test()[shard_index].read_access(|shard| {
            let monitors = shard
                .monitors
                .get(&20_200)
                .expect("one monitor should remain");
            assert_eq!(monitors.len(), 1);
            assert_eq!(monitors[0].ref_id, ref_id2);
        });

        // Remove second monitor
        hew_actor_demonitor(ref_id2);

        // Verify all monitors removed
        monitor_table_for_test()[shard_index].read_access(|shard| {
            assert!(
                !shard.monitors.contains_key(&20_200)
                    || shard
                        .monitors
                        .get(&20_200)
                        .is_none_or(std::vec::Vec::is_empty)
            );
        });
    }

    #[test]
    fn test_null_actor_handling() {
        let _g = guard();
        let mut actor = create_test_actor(300);
        let actor_ptr = &raw mut actor;

        // These should not panic and should return 0
        // SAFETY: Testing null pointer handling; function returns 0 for null.
        unsafe {
            let mut monitor_id = 99;
            assert_eq!(
                hew_actor_monitor(std::ptr::null_mut(), actor_ptr, &raw mut monitor_id),
                2
            );
            assert_eq!(
                hew_actor_monitor(actor_ptr, std::ptr::null_mut(), &raw mut monitor_id),
                2
            );
            assert_eq!(
                hew_actor_monitor(
                    std::ptr::null_mut(),
                    std::ptr::null_mut(),
                    &raw mut monitor_id,
                ),
                2
            );
            assert_eq!(monitor_id, 99, "failure must not write the out id");
        }

        // Demonitor with invalid ref_id should not panic
        hew_actor_demonitor(0);
        hew_actor_demonitor(99999);
    }

    #[test]
    fn remove_all_monitors_clears_target_and_watcher_entries() {
        let _g = guard();
        // Use unique IDs (40xxx) to avoid collisions.
        let mut watcher = create_test_actor(40_100);
        let mut target = create_test_actor(40_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0);

        // Precondition: target has monitors, watcher appears as a watcher.
        assert!(has_monitors_for_actor(40_200, target_ptr));

        // Act: remove all monitor entries for the target.
        remove_all_monitors_for_actor(40_200, target_ptr);

        // Target's own monitored-entry should be gone.
        assert!(
            !has_monitors_for_actor(40_200, target_ptr),
            "target should have no monitor entries after cleanup"
        );
    }

    #[test]
    fn remove_all_monitors_as_watcher_clears_watcher_entries() {
        let _g = guard();
        // Unique IDs (41xxx).
        let mut watcher = create_test_actor(41_100);
        let mut target = create_test_actor(41_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0);

        // Act: remove all entries where watcher appears (as if freeing watcher).
        remove_all_monitors_for_actor(41_100, watcher_ptr);

        // Watcher's address should be purged from target's monitor list.
        let shard = get_shard_index(41_200);
        monitor_table_for_test()[shard].read_access(|table| {
            let remaining = table.monitors.get(&41_200);
            assert!(
                remaining.is_none() || remaining.unwrap().is_empty(),
                "target's monitor list should no longer contain the freed watcher"
            );
        });
    }

    #[test]
    fn remove_all_monitors_no_monitors_does_not_panic() {
        let _g = guard();
        let actor = create_test_actor(40_300);
        let ptr = (&raw const actor).cast_mut();
        remove_all_monitors_for_actor(40_300, ptr);
        assert!(!has_monitors_for_actor(40_300, ptr));
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn notify_monitors_keeps_actor_live_through_sys_send() {
        let _guard = crate::runtime_test_guard();
        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let _hook = NotifyMonitorsHookGuard::install(Arc::clone(&entered), Arc::clone(&release));

        // SAFETY: spawned actors are owned by this test and freed before return.
        unsafe {
            let watcher =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            let target =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!watcher.is_null());
            assert!(!target.is_null());

            (*watcher).actor_state.store(
                HewActorState::Runnable as i32,
                std::sync::atomic::Ordering::Release,
            );

            let ref_id = monitor_for_test(watcher, target);
            assert_ne!(ref_id, 0);

            let target_id = (*target).id;
            let notify = std::thread::spawn(move || notify_monitors_on_death(target_id, 77, 0));
            entered.wait();

            // The DOWN message has now been delivered: `notify` is parked in
            // the hook still holding the LIVE_ACTORS lock. Verify delivery
            // from the main thread *before* spawning the free thread, so the
            // thread-spawn happens-before edge orders this read + node free
            // ahead of the free thread's mailbox teardown (rather than racing
            // its drain through the TSan-invisible std `Barrier`).
            let mailbox = (*watcher).mailbox.cast::<mailbox::HewMailbox>();
            let node = mailbox::hew_mailbox_try_recv_sys(mailbox);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, SYS_MSG_DOWN);
            let payload = &*((*node).data.cast::<HewDownMessage>());
            assert_eq!(payload.monitor_id, ref_id);
            assert_eq!(payload.target_kind, DOWN_TARGET_LOCAL);
            assert_eq!(payload.slot, crate::pid::hew_pid_serial(target_id));
            assert_eq!(payload.reason_kind, DOWN_REASON_CRASHED);
            mailbox::hew_msg_node_free(node);

            (*watcher).actor_state.store(
                HewActorState::Idle as i32,
                std::sync::atomic::Ordering::Release,
            );

            let free_started = Arc::new(std::sync::atomic::AtomicBool::new(false));
            let free_done = Arc::new(std::sync::atomic::AtomicBool::new(false));
            let watcher_addr = watcher as usize;
            let free_started_thread = Arc::clone(&free_started);
            let free_done_thread = Arc::clone(&free_done);
            let free_handle = std::thread::spawn(move || {
                free_started_thread.store(true, std::sync::atomic::Ordering::Release);
                let rc = crate::actor::hew_actor_free(watcher_addr as *mut HewActor);
                assert_eq!(rc, 0);
                free_done_thread.store(true, std::sync::atomic::Ordering::Release);
            });

            while !free_started.load(std::sync::atomic::Ordering::Acquire) {
                std::thread::yield_now();
            }

            std::thread::sleep(std::time::Duration::from_millis(50));
            assert!(
                !free_done.load(std::sync::atomic::Ordering::Acquire),
                "hew_actor_free must wait until notify_monitors_on_death releases LIVE_ACTORS"
            );

            release.wait();
            notify.join().unwrap();
            free_handle.join().unwrap();

            assert_eq!(crate::actor::hew_actor_free(target), 0);
        }
    }

    // --- hew_actor_demonitor silent-Ok contract discriminators ---
    //
    // These four tests pin the idempotence invariant that MonitorRef::close()
    // and its Drop impl rely on.  See the doc-comment on hew_actor_demonitor.

    /// (a) Let a MonitorRef-like wrapper leave scope without an explicit close.
    ///     Drop must clear the table entry.
    #[test]
    fn monitor_ref_drop_clears_table_entry() {
        let _g = guard();
        let mut watcher = create_test_actor(50_100);
        let mut target = create_test_actor(50_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0, "monitor creation must succeed");

        {
            let _monitor_ref = TestMonitorRef::new(ref_id);
        }

        // Verify: entry is gone from both maps.
        let shard = get_shard_index(50_200);
        monitor_table_for_test()[shard].read_access(|table| {
            assert!(
                !table.ref_to_monitor.contains_key(&ref_id),
                "ref_to_monitor must not contain the removed ref_id"
            );
            let monitors = table.monitors.get(&50_200);
            assert!(
                monitors.is_none_or(std::vec::Vec::is_empty),
                "target's monitor list must be empty after demonitor"
            );
        });
    }

    /// (b) Explicit close followed by the consumed value's Drop must be safe.
    #[test]
    fn monitor_ref_close_then_drop_is_safe() {
        let _g = guard();
        let mut watcher = create_test_actor(51_100);
        let mut target = create_test_actor(51_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0);

        TestMonitorRef::new(ref_id).close();

        // Confirm idempotence: the table is clean after both calls.
        let shard = get_shard_index(51_200);
        monitor_table_for_test()[shard].read_access(|table| {
            assert!(
                !table.ref_to_monitor.contains_key(&ref_id),
                "ref_to_monitor must not contain the ref_id after double-demonitor"
            );
            let monitors = table.monitors.get(&51_200);
            assert!(
                monitors.is_none_or(std::vec::Vec::is_empty),
                "target's monitor list must be empty after double-demonitor"
            );
        });
    }

    /// (c) Close after the target actor's monitor entries were swept
    ///     (simulating actor-free path) — still resolves silently.
    #[test]
    fn monitor_ref_close_after_target_freed_ok() {
        let _g = guard();
        let mut watcher = create_test_actor(52_100);
        let mut target = create_test_actor(52_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { monitor_for_test(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0);

        // Simulate actor-free: sweep all monitor entries for the target.
        remove_all_monitors_for_actor(52_200, target_ptr);

        // The ref_id is now stale — explicit close plus the consumed value's
        // Drop must both remain silent no-ops.
        TestMonitorRef::new(ref_id).close();

        let shard = get_shard_index(52_200);
        monitor_table_for_test()[shard].read_access(|table| {
            assert!(
                !table.ref_to_monitor.contains_key(&ref_id),
                "stale ref_id must remain absent after actor-free close"
            );
            let monitors = table.monitors.get(&52_200);
            assert!(
                monitors.is_none_or(std::vec::Vec::is_empty),
                "target's monitor list must stay empty after actor-free close"
            );
        });
    }

    /// (d) Demonitor with zero or arbitrary invalid `ref_id` values — must be silent no-ops.
    #[test]
    fn demonitor_invalid_ref_id_is_silent_noop() {
        let _g = guard();
        // ref_id == 0: early-return guard.
        hew_actor_demonitor(0);

        // ref_id that was never registered: unknown-id scan path.
        hew_actor_demonitor(u64::MAX);
        hew_actor_demonitor(99_999_999);
    }

    #[test]
    fn independent_runtimes_keep_separate_monitor_tables() {
        let rt_a = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));
        let rt_b = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));

        let mut watcher_a = create_test_actor(60_100);
        let mut target_a = create_test_actor(60_200);
        let mut watcher_b = create_test_actor(60_100);
        let mut target_b = create_test_actor(60_200);

        {
            // SAFETY: rt_a lives for the whole entered scope.
            let _enter = unsafe { crate::runtime::enter(&rt_a) };
            // SAFETY: watcher_a and target_a are valid test actors for the call.
            let ref_a = unsafe { monitor_for_test(&raw mut watcher_a, &raw mut target_a) };
            assert_eq!(ref_a, 1);
        }

        {
            // SAFETY: rt_b lives for the whole entered scope.
            let _enter = unsafe { crate::runtime::enter(&rt_b) };
            // SAFETY: watcher_b and target_b are valid test actors for the call.
            let ref_b = unsafe { monitor_for_test(&raw mut watcher_b, &raw mut target_b) };
            assert_eq!(
                ref_b, 1,
                "each runtime owns an independent monitor reference counter"
            );
        }

        {
            // SAFETY: rt_a lives for the whole entered scope.
            let _enter = unsafe { crate::runtime::enter(&rt_a) };
            notify_monitors_on_death(60_200, HewActorState::Stopped as i32, 0);
        }

        monitor_shard_for_runtime_test(&rt_a, 60_200).read_access(|shard| {
            assert!(
                !shard.monitors.contains_key(&60_200),
                "runtime A notification removes only runtime A's monitor"
            );
            assert!(
                shard.terminal_reasons.contains_key(&60_200),
                "runtime A records only its own terminal sweep"
            );
        });
        monitor_shard_for_runtime_test(&rt_b, 60_200).read_access(|shard| {
            assert_eq!(
                shard.monitors.get(&60_200).map(Vec::len),
                Some(1),
                "runtime B's monitor with the same actor id must not be cross-notified"
            );
            assert!(
                !shard.terminal_reasons.contains_key(&60_200),
                "runtime B must not inherit runtime A's terminal reason"
            );
        });
    }

    /// Death-path monitor reads must tolerate a missing runtime, exactly as the
    /// old process-global table did. `hew_actor_trap` reaches
    /// `notify_monitors_on_death` (and the free path reaches
    /// `remove_all_monitors_for_actor`) without an installed runtime in the
    /// supervisor-cascade unit tests; the fail-closed `monitor_state` resolver
    /// would abort there. With no runtime there is no table and thus no monitors
    /// to sweep, so both calls must be silent no-ops rather than panicking.
    #[test]
    fn death_path_tolerates_no_runtime() {
        // No runtime guard installed on this thread. Before the fix the
        // fail-closed `monitor_state` resolver aborted "no runtime installed"
        // here; both calls must now return silently. (A global default runtime
        // installed by a concurrent scheduler test would also be tolerated —
        // it resolves an empty table and no-ops just the same.)
        notify_monitors_on_death(70_200, HewActorState::Crashed as i32, 0);

        let actor = create_test_actor(70_200);
        let ptr = (&raw const actor).cast_mut();
        remove_all_monitors_for_actor(70_200, ptr);
    }

    /// With a runtime installed the death path resolves the per-runtime table
    /// and records the terminal sweep there — confirming the deglobalization is
    /// intact and the no-runtime tolerance did not flatten it back to a no-op.
    #[test]
    fn death_path_with_runtime_records_terminal_reason() {
        let rt = Box::new(crate::runtime::RuntimeInner::new(
            crate::scheduler::worker_less_scheduler(),
        ));
        // SAFETY: rt lives for the whole entered scope.
        let _enter = unsafe { crate::runtime::enter(&rt) };

        notify_monitors_on_death(71_200, HewActorState::Crashed as i32, 0);

        monitor_shard_for_runtime_test(&rt, 71_200).read_access(|shard| {
            assert_eq!(
                shard.terminal_reasons.get(&71_200).copied(),
                Some(TerminalMonitorReason {
                    state: HewActorState::Crashed as i32,
                    crash_kind: 0,
                }),
                "the installed runtime's monitor table must record the terminal sweep"
            );
        });
    }

    fn remote_location(byte: u8, slot: u64, incarnation: u32) -> Location {
        Location::new(NodeId::from_bytes([byte; 16]), slot, incarnation).unwrap()
    }

    #[test]
    fn observation_id_allocator_issues_max_then_exhausts_without_table_mutation() {
        let state = MonitorState::new();
        state.ref_counter.store(u64::MAX - 1, Ordering::Release);
        let first = state
            .register_remote_monitor(remote_location(1, 10, 1), 20)
            .expect("u64::MAX - 1 remains a valid nonzero id");
        let second = state
            .register_link_watcher(remote_location(1, 11, 1), 21, 3)
            .expect("u64::MAX remains a valid nonzero id");
        assert_eq!(first, u64::MAX - 1);
        assert_eq!(second, u64::MAX);
        assert_ne!(first, 0);
        assert_ne!(second, 0);
        assert_eq!(state.pending_observation_count(), 2);

        assert_eq!(
            state.register_remote_monitor(remote_location(1, 12, 1), 22),
            None
        );
        assert_eq!(state.ref_counter.load(Ordering::Acquire), u64::MAX);
        assert!(state.ref_exhausted.load(Ordering::Acquire));
        assert_eq!(state.pending_observation_count(), 2);
    }

    #[test]
    fn remote_monitor_claim_is_exactly_once_and_rejects_stale_incarnation() {
        let state = MonitorState::new();
        let target = remote_location(2, 10, 7);
        let stale_target = remote_location(2, 10, 6);
        let monitor_id = state
            .register_remote_monitor(target, 20)
            .expect("monitor id");

        assert_eq!(state.deliver_monitor_to_ref(monitor_id, stale_target), None);
        assert_eq!(state.pending_observation_count(), 1);

        let down = state
            .deliver_monitor_to_ref(monitor_id, target)
            .expect("matching terminal claim");
        assert_eq!(down.monitor_id, monitor_id);
        assert_eq!(down.watcher_actor_id, 20);
        assert_eq!(down.target, target);
        assert_eq!(state.deliver_monitor_to_ref(monitor_id, target), None);
        assert_eq!(state.pending_observation_count(), 0);
    }

    #[test]
    fn close_wins_over_later_remote_terminal_delivery() {
        let state = MonitorState::new();
        let target = remote_location(3, 11, 8);
        let monitor_id = state
            .register_remote_monitor(target, 21)
            .expect("monitor id");

        assert_eq!(state.remove_remote_observation(monitor_id), Some(target));
        assert_eq!(state.deliver_monitor_to_ref(monitor_id, target), None);
    }

    #[test]
    fn session_loss_claims_only_the_matching_incarnation() {
        let state = MonitorState::new();
        let old = remote_location(4, 12, 1);
        let current = remote_location(4, 12, 2);
        let old_id = state
            .register_remote_monitor(old, 22)
            .expect("old monitor id");
        let current_id = state
            .register_remote_monitor(current, 23)
            .expect("current monitor id");

        let downs = state.take_monitor_downs_for_session(old.node(), old.incarnation());
        assert_eq!(downs.len(), 1);
        assert_eq!(downs[0].monitor_id, old_id);
        assert_eq!(downs[0].target, old);
        assert_eq!(state.pending_observation_count(), 1);
        assert!(state.deliver_monitor_to_ref(current_id, current).is_some());
    }

    #[test]
    fn remote_link_terminal_claim_is_exactly_once() {
        let state = MonitorState::new();
        let target = remote_location(5, 13, 3);
        let link_id = state
            .register_link_watcher(target, 24, 3)
            .expect("link observation id");

        let down = state
            .deliver_link_down_to_ref(link_id, target, HewActorState::Crashed as i32)
            .expect("link terminal claim");
        assert_eq!(down.local_actor_id, 24);
        assert_eq!(down.remote_target_serial, target.slot());
        assert_eq!(down.policy_tag, 3);
        assert_eq!(
            state.deliver_link_down_to_ref(link_id, target, HewActorState::Crashed as i32),
            None
        );
    }

    #[test]
    fn shutdown_claims_all_remote_monitor_and_link_observations() {
        let state = MonitorState::new();
        state
            .register_remote_monitor(remote_location(6, 60, 1), 70)
            .expect("monitor observation");
        state
            .register_link_watcher(remote_location(7, 80, 1), 90, 2)
            .expect("link observation");

        state.drain_remote_observations_for_shutdown();
        state.drain_remote_observations_for_shutdown();

        assert_eq!(state.pending_observation_count(), 0);
    }
}
