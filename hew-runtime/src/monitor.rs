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
#[cfg(any(test, not(target_arch = "wasm32")))]
use std::sync::Arc;
#[cfg(test)]
use std::sync::Barrier;
#[cfg(test)]
use std::sync::LazyLock;
use std::sync::{Mutex, PoisonError};

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::lifetime::live_actors::pin_actor_by_id;
use crate::lifetime::local_handles::{resolve_current_actor, HewLocalPidId};
use crate::lifetime::poison_safe::PoisonSafeRw;
#[cfg(test)]
use crate::mailbox;
use crate::mailbox_header::HewSysMsg;
#[cfg(not(target_arch = "wasm32"))]
use crate::node_identity::{Location, NodeId};

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
pub(crate) struct TerminalMonitorReason {
    pub(crate) state: i32,
    pub(crate) crash_kind: u32,
    pub(crate) end: crate::internal::types::ActorEndReason,
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
    #[cfg(not(target_arch = "wasm32"))]
    remote_targets: Mutex<HashMap<u64, Vec<RemoteWatcher>>>,
    /// Cumulative count of target-side remote-watcher registrations ever
    /// accepted — monotonic, never reset. Test-introspection authority: an
    /// event counter cannot be missed by a sampler the way the transient live
    /// count can.
    #[cfg(not(target_arch = "wasm32"))]
    remote_watchers_registered_total: AtomicU64,
    ref_counter: AtomicU64,
    ref_exhausted: AtomicBool,
}

/// Reason sentinel used when a remote target becomes unreachable.
pub const MONITOR_REASON_LOST: i32 = -1;

/// What one watcher-side observation does when its target terminates.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WatcherAction {
    Monitor {
        watcher_actor_id: u64,
    },
    #[cfg(not(target_arch = "wasm32"))]
    Link {
        local_actor_id: u64,
        policy_tag: u8,
    },
}

impl WatcherAction {
    fn belongs_to(self, actor_id: u64) -> bool {
        match self {
            Self::Monitor { watcher_actor_id } => watcher_actor_id == actor_id,
            #[cfg(not(target_arch = "wasm32"))]
            Self::Link { local_actor_id, .. } => local_actor_id == actor_id,
        }
    }
}

/// Target identity for one observation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ObservationTarget {
    Local(u64),
    #[cfg(not(target_arch = "wasm32"))]
    Remote(Location),
}

#[derive(Debug, Clone)]
struct ObservationEntry {
    #[cfg(not(target_arch = "wasm32"))]
    authority: Option<ObservationAuthority>,
    #[cfg(not(target_arch = "wasm32"))]
    completion: Option<Arc<crate::actor_native::NativeActorCompletion>>,
    target: ObservationTarget,
    action: WatcherAction,
}

#[derive(Debug, Clone, Copy)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct ObservationAuthority {
    pub(crate) grant: u64,
    pub(crate) publication: u64,
    pub(crate) watcher: Location,
}

/// A claimed remote monitor terminal action.
#[derive(Debug, Clone)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct MonitorDown {
    pub(crate) completion: Option<Arc<crate::actor_native::NativeActorCompletion>>,
    pub monitor_id: u64,
    pub watcher_actor_id: u64,
    pub target: Location,
}

/// A claimed cross-node link terminal action.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct LinkDown {
    pub local_actor_id: u64,
    pub target: Location,
    pub reason: i32,
    pub crash_kind: i32,
    pub policy_tag: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(not(target_arch = "wasm32"))]
struct RemoteWatcher {
    destination: Location,
    publication: Option<u64>,
    reciprocal_ref: u64,
    watcher: Location,
    ref_id: u64,
    is_link: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct RemoteWatcherTarget {
    pub(crate) destination: Location,
    pub(crate) publication: Option<u64>,
    pub watcher: Location,
    pub ref_id: u64,
    pub is_link: bool,
}

/// Snapshot for one late remote subscription. Diagnostic ownership stays with
/// the pinned actor completion, never the scalar terminal-history table.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) struct RemoteTerminal {
    pub(crate) reason: TerminalMonitorReason,
    pub(crate) close_fault: Option<(i32, String)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg(not(target_arch = "wasm32"))]
pub(crate) enum RemoteWatcherSetup {
    Registered,
    Terminal(RemoteTerminal),
    Refused,
    TargetGone,
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
            #[cfg(not(target_arch = "wasm32"))]
            remote_targets: Mutex::new(HashMap::new()),
            #[cfg(not(target_arch = "wasm32"))]
            remote_watchers_registered_total: AtomicU64::new(0),
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

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_remote_monitor(
        &self,
        target: Location,
        watcher_actor_id: u64,
    ) -> Option<u64> {
        self.register_remote_observation(target, WatcherAction::Monitor { watcher_actor_id })
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_link_watcher(
        &self,
        target: Location,
        local_actor_id: u64,
        policy_tag: u8,
    ) -> Option<u64> {
        let monitor_id = self.next_observation_id()?;
        self.register_link_watcher_with_id(monitor_id, target, local_actor_id, policy_tag);
        Some(monitor_id)
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_link_watcher_with_id(
        &self,
        monitor_id: u64,
        target: Location,
        local_actor_id: u64,
        policy_tag: u8,
    ) {
        self.insert_remote_observation(
            monitor_id,
            target,
            WatcherAction::Link {
                local_actor_id,
                policy_tag,
            },
        );
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    fn register_remote_observation(&self, target: Location, action: WatcherAction) -> Option<u64> {
        let monitor_id = self.next_observation_id()?;
        self.insert_remote_observation(monitor_id, target, action);
        Some(monitor_id)
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    fn insert_remote_observation(&self, monitor_id: u64, target: Location, action: WatcherAction) {
        self.observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(
                monitor_id,
                ObservationEntry {
                    authority: None,
                    completion: None,
                    target: ObservationTarget::Remote(target),
                    action,
                },
            );
    }

    /// Remove one setup-time remote observation without sending a demonitor.
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remove_remote_observation(&self, monitor_id: u64) -> Option<Location> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).cloned()?;
        let ObservationTarget::Remote(target) = entry.target else {
            return None;
        };
        observations.remove(&monitor_id);
        drop(observations);
        if let (WatcherAction::Link { local_actor_id, .. }, Some(authority)) =
            (entry.action, entry.authority)
        {
            self.remove_reciprocal_target(
                local_actor_id,
                target,
                monitor_id,
                authority.publication,
            );
        }
        Some(target)
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn remove_reciprocal_target(&self, actor: u64, target: Location, id: u64, publication: u64) {
        let mut targets = crate::util::MutexExt::lock_or_recover(&self.remote_targets);
        let actor = crate::pid::hew_pid_serial(actor);
        if let Some(watchers) = targets.get_mut(&actor) {
            watchers.retain(|watcher| {
                !(watcher.watcher == target
                    && watcher.reciprocal_ref == id
                    && watcher.publication == Some(publication))
            });
            if watchers.is_empty() {
                targets.remove(&actor);
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn insert_admitted_observation(
        &self,
        id: u64,
        target: Location,
        action: WatcherAction,
        authority: ObservationAuthority,
        completion: Option<Arc<crate::actor_native::NativeActorCompletion>>,
    ) {
        crate::util::MutexExt::lock_or_recover(&self.observations).insert(
            id,
            ObservationEntry {
                target: ObservationTarget::Remote(target),
                action,
                authority: Some(authority),
                completion,
            },
        );
    }

    /// A reciprocal request can only complete the exact link this caller began.
    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn permits_reciprocal(
        &self,
        payload: &crate::envelope::LinkReqPayload,
        publication: u64,
    ) -> bool {
        let entries = crate::util::MutexExt::lock_or_recover(&self.observations);
        Self::reciprocal_matches(&entries, payload, publication)
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn reciprocal_matches(
        entries: &HashMap<u64, ObservationEntry>,
        payload: &crate::envelope::LinkReqPayload,
        publication: u64,
    ) -> bool {
        entries.get(&payload.reciprocal_ref).is_some_and(|entry| {
            matches!(entry.action, WatcherAction::Link { policy_tag, .. } if policy_tag == payload.policy_tag)
                && entry.target == ObservationTarget::Remote(payload.linker)
                && entry.authority.is_some_and(|auth| auth.grant == payload.grant_id
                    && auth.publication == publication && auth.watcher == payload.target)
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remove_imported_links(
        &self,
        actor: u64,
        target: Location,
    ) -> Vec<(u64, ObservationAuthority)> {
        let mut entries = crate::util::MutexExt::lock_or_recover(&self.observations);
        let selected: Vec<_> = entries.iter().filter_map(|(&id, entry)| {
            (entry.target == ObservationTarget::Remote(target)
                && matches!(entry.action, WatcherAction::Link { local_actor_id, .. } if local_actor_id == actor))
                .then(|| entry.authority.map(|auth| (id, auth))).flatten()
        }).collect();
        for (id, _) in &selected {
            entries.remove(id);
        }
        drop(entries);
        for (id, authority) in &selected {
            self.remove_reciprocal_target(actor, target, *id, authority.publication);
        }
        selected
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn enqueue_terminal(
        &self,
        down: MonitorDown,
        crash_kind: u32,
        end_reason: u8,
        close_fault: Option<(i32, String)>,
    ) {
        if let Some(completion) = down.completion {
            // The original crash stays on its owning node. Only a distinct
            // cleanup fault transfers to this completion observer.
            completion.finish_remote(close_fault);
        } else {
            self.enqueue_down(
                down.watcher_actor_id,
                down.monitor_id,
                down.target,
                crash_kind,
                end_reason,
            );
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
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

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn deliver_monitor_to_ref(
        &self,
        monitor_id: u64,
        target: Location,
    ) -> Option<MonitorDown> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).cloned()?;
        match (entry.target, entry.action) {
            (
                ObservationTarget::Remote(entry_target),
                WatcherAction::Monitor { watcher_actor_id },
            ) if entry_target == target => {
                observations.remove(&monitor_id);
                Some(MonitorDown {
                    completion: entry.completion,
                    monitor_id,
                    watcher_actor_id,
                    target,
                })
            }
            _ => None,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn deliver_link_down_to_ref(
        &self,
        monitor_id: u64,
        target: Location,
        reason: i32,
        crash_kind: i32,
    ) -> Option<LinkDown> {
        let mut observations = self
            .observations
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let entry = observations.get(&monitor_id).cloned()?;
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
                    target,
                    reason,
                    crash_kind,
                    policy_tag,
                })
            }
            _ => None,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn take_monitor_downs_for_owner(
        &self,
        remote_node: NodeId,
        session_incarnation: u32,
        publication: u64,
    ) -> Vec<MonitorDown> {
        self.take_monitor_downs_matching(|target, authority| {
            target.node() == remote_node
                && target.incarnation() == session_incarnation
                && authority.is_some_and(|authority| authority.publication == publication)
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn drain_remote_observations_for_shutdown(&self) {
        for down in self.take_monitor_downs_matching(|_, _| true) {
            self.enqueue_shutdown(down);
        }
        for down in self.take_link_downs_matching(|_, _| true) {
            crate::link::deliver_cross_node_link_exit(
                down.local_actor_id,
                down.target,
                down.reason,
                down.policy_tag,
                down.crash_kind,
            );
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn take_monitor_downs_matching(
        &self,
        matches: impl Fn(Location, Option<ObservationAuthority>) -> bool,
    ) -> Vec<MonitorDown> {
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
                ) if matches(target, entry.authority) => Some((
                    monitor_id,
                    watcher_actor_id,
                    target,
                    entry.completion.clone(),
                )),
                _ => None,
            })
            .collect();
        for (monitor_id, _, _, _) in &ids {
            observations.remove(monitor_id);
        }
        ids.into_iter()
            .map(
                |(monitor_id, watcher_actor_id, target, completion)| MonitorDown {
                    completion,
                    monitor_id,
                    watcher_actor_id,
                    target,
                },
            )
            .collect()
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn take_link_downs_for_owner(
        &self,
        remote_node: NodeId,
        session_incarnation: u32,
        publication: u64,
    ) -> Vec<LinkDown> {
        self.take_link_downs_matching(|target, authority| {
            target.node() == remote_node
                && target.incarnation() == session_incarnation
                && authority.is_some_and(|authority| authority.publication == publication)
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn take_link_downs_matching(
        &self,
        matches: impl Fn(Location, Option<ObservationAuthority>) -> bool,
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
                ) if matches(target, entry.authority) => Some((
                    monitor_id,
                    LinkDown {
                        local_actor_id,
                        target,
                        reason: MONITOR_REASON_LOST,
                        crash_kind: crate::internal::types::CrashKind::PartitionDetected.tag(),
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

    #[cfg(not(target_arch = "wasm32"))]
    #[allow(
        clippy::too_many_arguments,
        reason = "registration binds selected worker, semantic destination and authenticated publication"
    )]
    pub(crate) fn register_admitted_watcher(
        &self,
        actor: crate::lifetime::live_actors::ActorIncarnation,
        destination: Location,
        watcher: Location,
        id: u64,
        link: bool,
        publication: u64,
        reciprocal_ref: u64,
    ) -> RemoteWatcherSetup {
        self.register_remote_watcher_kind(
            actor,
            RemoteWatcher {
                destination,
                watcher,
                ref_id: id,
                is_link: link,
                publication: Some(publication),
                reciprocal_ref,
            },
            None,
        )
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_reciprocal_watcher(
        &self,
        actor: crate::lifetime::live_actors::ActorIncarnation,
        payload: &crate::envelope::LinkReqPayload,
        publication: u64,
    ) -> RemoteWatcherSetup {
        self.register_remote_watcher_kind(
            actor,
            RemoteWatcher {
                destination: payload.target,
                watcher: payload.linker,
                ref_id: payload.ref_id,
                is_link: true,
                publication: Some(publication),
                reciprocal_ref: payload.reciprocal_ref,
            },
            Some(payload),
        )
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn retract_admitted_watcher(
        &self,
        destination: Location,
        watcher: Location,
        id: u64,
        publication: u64,
    ) {
        let reverse = {
            let mut targets = crate::util::MutexExt::lock_or_recover(&self.remote_targets);
            let mut reverse = Vec::new();
            targets.retain(|_, entries| {
                entries.retain(|entry| {
                    let matches = entry.destination == destination
                        && entry.watcher == watcher
                        && entry.ref_id == id
                        && entry.publication == Some(publication);
                    if matches && entry.reciprocal_ref != 0 {
                        reverse.push(entry.reciprocal_ref);
                    }
                    !matches
                });
                !entries.is_empty()
            });
            reverse
        };
        for reference in reverse {
            self.remove_remote_observation(reference);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn permits_terminal(&self, id: u64, target: Location, publication: u64) -> bool {
        crate::util::MutexExt::lock_or_recover(&self.observations)
            .get(&id)
            .is_some_and(|entry| {
                entry.target == ObservationTarget::Remote(target)
                    && entry
                        .authority
                        .is_none_or(|authority| authority.publication == publication)
            })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn register_remote_watcher_kind(
        &self,
        selected: crate::lifetime::live_actors::ActorIncarnation,
        registration: RemoteWatcher,
        reciprocal: Option<&crate::envelope::LinkReqPayload>,
    ) -> RemoteWatcherSetup {
        let target_actor_id = selected.actor_id();
        let RemoteWatcher {
            destination: _,
            watcher,
            ref_id: monitor_id,
            is_link: _,
            publication,
            reciprocal_ref: _,
        } = registration;
        let shard_index = get_shard_index(target_actor_id);
        let register = |actor: &HewActor| {
            self.table[shard_index].access(|shard| {
                // Match and install under the same observation claim. Cancellation
                // cannot retire the caller between reciprocal authorization and insertion.
                let caller =
                    reciprocal.map(|_| crate::util::MutexExt::lock_or_recover(&self.observations));
                if reciprocal
                    .zip(caller.as_ref())
                    .is_some_and(|(payload, entries)| {
                        !Self::reciprocal_matches(
                            entries,
                            payload,
                            publication.expect("authenticated reciprocal publication"),
                        )
                    })
                {
                    return RemoteWatcherSetup::Refused;
                }
                let terminal = shard
                    .terminal_reasons
                    .get(&target_actor_id)
                    .copied()
                    .or_else(|| {
                        let state = actor.actor_state.load(Ordering::Acquire);
                        (terminal_monitor_reason(state).is_some()
                            && actor
                                .native_completion
                                .as_ref()
                                .is_none_or(|completion| completion.is_finished()))
                        .then(|| TerminalMonitorReason {
                            state,
                            crash_kind: crate::internal::types::CrashKind::tag_from_error_code(
                                actor.error_code.load(Ordering::Acquire),
                            )
                            .cast_unsigned(),
                            // SAFETY: the send pin retains this actor and mailbox.
                            end: crate::internal::types::ActorEndReason::from_terminal(
                                state,
                                unsafe {
                                    crate::mailbox::mailbox_terminate_requested(
                                        actor.mailbox.cast(),
                                    )
                                },
                            ),
                        })
                    });
                if let Some(reason) = terminal {
                    return RemoteWatcherSetup::Terminal(RemoteTerminal {
                        reason,
                        close_fault: actor
                            .native_completion
                            .as_ref()
                            .and_then(|completion| completion.close_fault_payload()),
                    });
                }
                let target_serial = crate::pid::hew_pid_serial(target_actor_id);
                let mut targets = self
                    .remote_targets
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner);
                let watchers = targets.entry(target_serial).or_default();
                if !watchers.iter().any(|entry| {
                    entry.destination == registration.destination
                        && entry.watcher == watcher
                        && entry.ref_id == monitor_id
                        && entry.publication == publication
                }) {
                    watchers.push(registration);
                    self.remote_watchers_registered_total
                        .fetch_add(1, Ordering::Relaxed);
                }
                RemoteWatcherSetup::Registered
            })
        };

        if let Some(result) = crate::lifetime::live_actors::with_live_incarnation(selected, |pin| {
            register(pin.actor())
        }) {
            return result;
        }

        RemoteWatcherSetup::TargetGone
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn register_remote_link_watcher_for_test(
        &self,
        target_serial: u64,
        watcher: Location,
        monitor_id: u64,
    ) {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .entry(target_serial)
            .or_default()
            .push(RemoteWatcher {
                destination: Location::direct(watcher.node(), target_serial, watcher.incarnation())
                    .expect("test destination"),
                publication: None,
                reciprocal_ref: 0,
                watcher,
                ref_id: monitor_id,
                is_link: true,
            });
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remove_remote_watcher(
        &self,
        destination: Location,
        watcher: Location,
        monitor_id: u64,
    ) {
        let mut targets = crate::util::MutexExt::lock_or_recover(&self.remote_targets);
        targets.retain(|_, watchers| {
            watchers.retain(|entry| {
                !(entry.destination == destination
                    && entry.watcher == watcher
                    && entry.ref_id == monitor_id)
            });
            !watchers.is_empty()
        });
    }

    #[cfg(test)]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn take_remote_watchers(&self, target_serial: u64) -> Vec<RemoteWatcherTarget> {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .remove(&target_serial)
            .map(|watchers| {
                watchers
                    .into_iter()
                    .map(|entry| RemoteWatcherTarget {
                        destination: entry.destination,
                        publication: entry.publication,
                        watcher: entry.watcher,
                        ref_id: entry.ref_id,
                        is_link: entry.is_link,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn prune_remote_watchers_for_owner(
        &self,
        dead_watcher: NodeId,
        session_incarnation: u32,
        publication: u64,
    ) -> usize {
        self.prune_remote_watchers_matching(|entry| {
            entry.watcher.node() == dead_watcher
                && entry.watcher.incarnation() == session_incarnation
                && entry.publication == Some(publication)
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn prune_remote_watchers_matching(&self, matches: impl Fn(RemoteWatcher) -> bool) -> usize {
        let mut targets = self
            .remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let mut pruned = 0;
        targets.retain(|_, watchers| {
            let before = watchers.len();
            watchers.retain(|entry| !matches(*entry));
            pruned += before - watchers.len();
            !watchers.is_empty()
        });
        pruned
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote_watchers_registered_total(&self) -> u64 {
        self.remote_watchers_registered_total
            .load(Ordering::Relaxed)
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote_watcher_count(&self) -> usize {
        self.remote_targets
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .values()
            .map(Vec::len)
            .sum()
    }

    #[cfg(not(target_arch = "wasm32"))]
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
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn enqueue_down(
        &self,
        watcher_actor_id: u64,
        monitor_id: u64,
        target: Location,
        crash_kind: u32,
        end_reason: u8,
    ) {
        deliver_down_message(
            watcher_actor_id,
            HewDownMessage::remote(monitor_id, target, crash_kind, end_reason),
        );
    }

    #[expect(
        clippy::unused_self,
        reason = "mailbox delivery remains namespaced under the sole MonitorState authority"
    )]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn enqueue_lost(&self, down: MonitorDown) {
        if let Some(completion) = down.completion {
            completion.finish(crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
            return;
        }
        deliver_down_message(
            down.watcher_actor_id,
            HewDownMessage::remote_lost(down.monitor_id, down.target),
        );
    }

    #[expect(
        clippy::unused_self,
        reason = "mailbox delivery remains namespaced under the sole MonitorState authority"
    )]
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn enqueue_shutdown(&self, down: MonitorDown) {
        if let Some(completion) = down.completion {
            completion.finish(crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED);
            return;
        }
        deliver_down_message(
            down.watcher_actor_id,
            HewDownMessage::remote_shutdown(down.monitor_id, down.target),
        );
    }
}

#[inline]
fn monitor_state() -> &'static MonitorState {
    #[cfg(not(target_arch = "wasm32"))]
    {
        &crate::runtime::rt_current().monitors
    }
    #[cfg(target_arch = "wasm32")]
    {
        static STATE: std::sync::OnceLock<MonitorState> = std::sync::OnceLock::new();
        STATE.get_or_init(MonitorState::new)
    }
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
    #[cfg(not(target_arch = "wasm32"))]
    {
        crate::runtime::rt_current_opt().map(|rt| &rt.monitors)
    }
    #[cfg(target_arch = "wasm32")]
    {
        Some(monitor_state())
    }
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
#[cfg(not(target_arch = "wasm32"))]
const DOWN_TARGET_REMOTE: u32 = 1;
#[cfg(not(target_arch = "wasm32"))]
/// Native notification tag for a remote stable-role Location.
pub const DOWN_TARGET_REMOTE_ROLE: u32 = 3;
pub(crate) const DOWN_TARGET_ROLE: u32 = 2;
const DOWN_REASON_STOPPED: u32 = 0;
const DOWN_REASON_TERMINATED: u32 = 1;
const DOWN_REASON_CRASHED: u32 = 2;
#[cfg(not(target_arch = "wasm32"))]
const DOWN_REASON_MONITOR_LOST: u32 = 3;
#[cfg(not(target_arch = "wasm32"))]
const DOWN_REASON_LOCAL_SHUTDOWN: u32 = 4;

/// Shared physical target payload for monitor and link notifications.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewNotificationTarget {
    pub kind: u32,
    pub session_incarnation: u32,
    pub node_hi: u64,
    pub node_lo: u64,
    pub slot: u64,
}

impl HewNotificationTarget {
    pub(crate) fn local(actor_id: u64) -> Self {
        Self {
            kind: DOWN_TARGET_LOCAL,
            session_incarnation: 0,
            node_hi: 0,
            node_lo: 0,
            slot: crate::pid::hew_pid_serial(actor_id),
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote(target: Location) -> Self {
        Self {
            kind: match target.target() {
                crate::node_identity::Target::Direct(_) => DOWN_TARGET_REMOTE,
                crate::node_identity::Target::Role(_) => DOWN_TARGET_REMOTE_ROLE,
            },
            session_incarnation: target.incarnation(),
            node_hi: target.node().hi(),
            node_lo: target.node().lo(),
            slot: target.target().id(),
        }
    }

    pub(crate) fn role(token: HewLocalPidId) -> Self {
        Self {
            kind: DOWN_TARGET_ROLE,
            session_incarnation: 0,
            node_hi: 0,
            node_lo: 0,
            // Stable local role tokens are semantic identities, not packed
            // actor PIDs. Keep the complete token in the shared slot word.
            slot: u64::try_from(token.as_usize()).expect("local role token fits notification slot"),
        }
    }
}

/// Fixed actor-mailbox DOWN payload.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HewDownMessage {
    pub monitor_id: u64,
    pub target: HewNotificationTarget,
    pub reason_kind: u32,
    pub crash_kind: u32,
}

impl HewDownMessage {
    fn local(
        monitor_id: u64,
        actor_id: u64,
        end: crate::internal::types::ActorEndReason,
        crash_kind: u32,
    ) -> Self {
        let reason_kind = match end {
            crate::internal::types::ActorEndReason::Stopped => DOWN_REASON_STOPPED,
            crate::internal::types::ActorEndReason::Terminated => DOWN_REASON_TERMINATED,
            crate::internal::types::ActorEndReason::Crashed => DOWN_REASON_CRASHED,
        };
        Self {
            monitor_id,
            target: HewNotificationTarget::local(actor_id),
            reason_kind,
            crash_kind: if reason_kind == DOWN_REASON_CRASHED {
                crash_kind
            } else {
                0
            },
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote(
        monitor_id: u64,
        target: Location,
        crash_kind: u32,
        end_reason: u8,
    ) -> Self {
        let reason_kind = match end_reason {
            0 => DOWN_REASON_STOPPED,
            1 => DOWN_REASON_TERMINATED,
            2 => DOWN_REASON_CRASHED,
            4 => DOWN_REASON_LOCAL_SHUTDOWN,
            // The codec admits 3 (monitor lost); any invalid internal caller
            // also fails closed as lost rather than inventing a terminal cause.
            _ => DOWN_REASON_MONITOR_LOST,
        };
        Self {
            monitor_id,
            target: HewNotificationTarget::remote(target),
            reason_kind,
            crash_kind: if reason_kind == DOWN_REASON_CRASHED {
                crash_kind
            } else {
                0
            },
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote_lost(monitor_id: u64, target: Location) -> Self {
        Self {
            monitor_id,
            target: HewNotificationTarget::remote(target),
            reason_kind: DOWN_REASON_MONITOR_LOST,
            crash_kind: 0,
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn remote_shutdown(monitor_id: u64, target: Location) -> Self {
        Self {
            reason_kind: DOWN_REASON_LOCAL_SHUTDOWN,
            ..Self::remote_lost(monitor_id, target)
        }
    }

    pub(crate) fn role_stopped(monitor_id: u64, token: HewLocalPidId) -> Self {
        Self {
            monitor_id,
            target: HewNotificationTarget::role(token),
            reason_kind: DOWN_REASON_STOPPED,
            crash_kind: 0,
        }
    }
}

const _: () = {
    assert!(std::mem::size_of::<HewNotificationTarget>() == 32);
    assert!(std::mem::size_of::<HewDownMessage>() == 48);
    assert!(std::mem::offset_of!(HewDownMessage, monitor_id) == 0);
    assert!(std::mem::offset_of!(HewDownMessage, target) == 8);
    assert!(std::mem::offset_of!(HewDownMessage, reason_kind) == 40);
    assert!(std::mem::offset_of!(HewDownMessage, crash_kind) == 44);
};

pub(crate) fn deliver_down_message(watcher_actor_id: u64, down_data: HewDownMessage) {
    let Some(monitoring_actor_pin) = pin_actor_by_id(watcher_actor_id) else {
        return;
    };
    let monitoring_actor = monitoring_actor_pin.as_ptr();
    let data_ptr = (&raw const down_data).cast::<c_void>();
    let data_size = std::mem::size_of::<HewDownMessage>();
    // SAFETY: ActorPin keeps the actor and mailbox live through enqueue,
    // terminal handoff, and the controlled-failure trap.
    let sent = unsafe {
        crate::actor::send_system_message(
            monitoring_actor,
            HewSysMsg::Down,
            data_ptr.cast_mut(),
            data_size,
        )
    };

    #[cfg(test)]
    run_notify_monitors_hook();

    if !sent {
        // Crucially this runs without LIVE_ACTORS held. Trap notification may
        // recurse through monitor/link lookup, while the ActorPin alone keeps
        // this allocation live.
        // SAFETY: ActorPin validates and retains `monitoring_actor`.
        unsafe {
            crate::actor::hew_actor_trap(
                monitoring_actor,
                crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED,
            );
        }
    }
}

/// The one refusal `link` and `monitor` report: no current actor can receive
/// the exit or `DOWN`. Every other outcome is `Ok`; a dead, retired or
/// unreachable target answers with an immediate exit or `DOWN` instead.
pub const HEW_OBSERVATION_NO_CONTEXT: i32 = 1;

/// Abort when the 64-bit observation id space is spent: no id can name the
/// subscription, and reusing one would deliver to the wrong watcher.
pub(crate) fn observation_ids_exhausted() -> ! {
    crate::set_last_error("observation id space exhausted");
    eprintln!("hew: observation id space exhausted");
    std::process::abort()
}

/// Create a monitor: watcher monitors target.
///
/// Returns zero and writes a unique monitor id; a target that already exited
/// receives its `DOWN` at once. A null watcher is
/// [`HEW_OBSERVATION_NO_CONTEXT`]; a null target or out pointer breaks the
/// caller's contract and aborts.
///
/// # Safety
///
/// `target` and `out_monitor_id` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_monitor(
    watcher: *mut HewActor,
    target: *mut HewActor,
    out_monitor_id: *mut u64,
) -> i32 {
    if watcher.is_null() {
        return HEW_OBSERVATION_NO_CONTEXT;
    }
    if target.is_null() || out_monitor_id.is_null() {
        crate::set_last_error("hew_actor_monitor: null target or out pointer");
        std::process::abort();
    }

    // SAFETY: Caller guarantees both pointers are valid.
    let watcher_ref = unsafe { &*watcher };
    // SAFETY: Caller guarantees both pointers are valid.
    let target_ref = unsafe { &*target };

    // SAFETY: both borrowed actor allocations and the out pointer are valid.
    unsafe {
        register_local_monitor(
            watcher_ref.id,
            target_ref.id,
            Some(target_ref),
            out_monitor_id,
        )
    }
}

/// Register from the incarnation identity; a borrowed target is optional once
/// retirement has published terminal state and reclaimed its allocation.
unsafe fn register_local_monitor(
    watcher_id: u64,
    target_id: u64,
    target: Option<&HewActor>,
    out_monitor_id: *mut u64,
) -> i32 {
    // Generate one shared local/remote observation ID.
    let state = monitor_state();
    let ref_id = state
        .next_observation_id()
        .unwrap_or_else(|| observation_ids_exhausted());

    let monitor_entry = MonitorEntry { ref_id };
    let observation = ObservationEntry {
        #[cfg(not(target_arch = "wasm32"))]
        authority: None,
        #[cfg(not(target_arch = "wasm32"))]
        completion: None,
        target: ObservationTarget::Local(target_id),
        action: WatcherAction::Monitor {
            watcher_actor_id: watcher_id,
        },
    };

    let shard_index = get_shard_index(target_id);
    let terminal_reason = state.table[shard_index].access(|shard| {
        if let Some(&reason) = shard.terminal_reasons.get(&target_id) {
            Some(reason)
        } else if let Some((target_ref, reason)) = target.and_then(|target| {
            terminal_monitor_reason(target.actor_state.load(Ordering::Acquire))
                .map(|reason| (target, reason))
        }) {
            Some(TerminalMonitorReason {
                state: reason,
                end: crate::internal::types::ActorEndReason::from_terminal(
                    reason,
                    crate::mailbox::mailbox_terminate_requested(target_ref.mailbox.cast()),
                ),
                crash_kind: if reason == HewActorState::Crashed as i32 {
                    crate::internal::types::CrashKind::tag_from_error_code(
                        target_ref.error_code.load(Ordering::Acquire),
                    )
                    .cast_unsigned()
                } else {
                    0
                },
            })
        } else if target.is_none() {
            // A clean free can retire an incarnation without an earlier terminal
            // sweep. Its identity still produces an immediate normal exit.
            Some(TerminalMonitorReason {
                state: HewActorState::Stopped as i32,
                crash_kind: 0,
                end: crate::internal::types::ActorEndReason::Stopped,
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
        let down = HewDownMessage::local(ref_id, target_id, reason.end, reason.crash_kind);
        deliver_down_message(watcher_id, down);
    }

    // SAFETY: caller provided a writable out pointer; write only on success.
    unsafe { *out_monitor_id = ref_id };
    0
}

/// Monitor the current actor's local target, including a retired incarnation.
/// Status zero initializes the owned monitor ID; [`HEW_OBSERVATION_NO_CONTEXT`]
/// means no current actor.
/// Negative statuses are logical fault codes, never `LinkError` discriminants.
///
/// # Safety
/// `out_monitor_id` must be writable.
#[no_mangle]
pub unsafe extern "C" fn hew_native_actor_monitor(
    target: HewLocalPidId,
    out_monitor_id: *mut u64,
) -> i32 {
    let watcher = crate::actor::hew_actor_self();
    if watcher.is_null() {
        return HEW_OBSERVATION_NO_CONTEXT;
    }
    let Some(target) = crate::lifetime::local_handles::resolve_current_observed_target(target)
    else {
        return -crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED;
    };
    // SAFETY: the current activation retains this non-null watcher actor.
    let watcher_id = unsafe { (*watcher).id };
    match target {
        crate::lifetime::local_handles::ObservedActorTarget::Actor(target_id) => {
            let target = pin_actor_by_id(target_id);
            // SAFETY: the activation owns watcher; the optional pin protects
            // target; the caller provides a writable output slot.
            let status = unsafe {
                register_local_monitor(
                    watcher_id,
                    target_id,
                    target
                        .as_ref()
                        .map(crate::lifetime::live_actors::ActorPin::actor),
                    out_monitor_id,
                )
            };
            if status == 0 {
                0
            } else {
                -crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
            }
        }
        crate::lifetime::local_handles::ObservedActorTarget::Role { token, .. } => {
            for _ in 0..2 {
                let Some((target_id, target_serial)) =
                    crate::supervisor::current_actor_role_identity(token)
                else {
                    let state = monitor_state();
                    let ref_id = state
                        .next_observation_id()
                        .unwrap_or_else(|| observation_ids_exhausted());
                    // This role has no current incarnation. Report the stable
                    // role identity once with a normal terminal reason; a
                    // later replacement is a separate observation.
                    deliver_down_message(watcher_id, HewDownMessage::role_stopped(ref_id, token));
                    // SAFETY: inherited from this function's writable output
                    // pointer contract.
                    unsafe { *out_monitor_id = ref_id };
                    return 0;
                };
                // Pin the exact incarnation selected under the supervisor's
                // roster lock. A packed actor ID alone could alias another
                // incarnation before the monitor is registered.
                let status = crate::lifetime::live_actors::with_actor_send_by_identity(
                    target_id,
                    target_serial,
                    |pin| {
                        // SAFETY: the activation owns watcher; `pin` retains
                        // the selected target through registration.
                        unsafe {
                            register_local_monitor(
                                watcher_id,
                                target_id,
                                Some(pin.actor()),
                                out_monitor_id,
                            )
                        }
                    },
                );
                if let Some(status) = status {
                    return if status == 0 {
                        0
                    } else {
                        -crate::internal::types::HEW_TRAP_HEAP_EXCEEDED
                    };
                }
            }
            -crate::internal::types::HEW_TRAP_ACTOR_SEND_FAILED
        }
    }
}

/// Create a monitor between two stable local actor identities.
///
/// Returns the same one-based monitor status vocabulary as
/// [`hew_actor_monitor`]. Missing, foreign-runtime, or retired handles report
/// `INVALID_TARGET` (`2`).
///
/// # Safety
/// `out_monitor_id` must be writable when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_local_pid_monitor(
    watcher: HewLocalPidId,
    target: HewLocalPidId,
    out_monitor_id: *mut u64,
) -> i32 {
    const INVALID_TARGET: i32 = 2;

    let (Some(watcher_id), Some(target_id)) = (
        resolve_current_actor(watcher),
        resolve_current_actor(target),
    ) else {
        return INVALID_TARGET;
    };
    let Some(watcher_pin) = pin_actor_by_id(watcher_id) else {
        return INVALID_TARGET;
    };
    if watcher_id == target_id {
        #[cfg(test)]
        run_local_pid_monitor_registration_hook();
        // SAFETY: the one allocation is pinned and the out pointer follows
        // this function's contract.
        return unsafe {
            hew_actor_monitor(watcher_pin.as_ptr(), watcher_pin.as_ptr(), out_monitor_id)
        };
    }
    let Some(target_pin) = pin_actor_by_id(target_id) else {
        return INVALID_TARGET;
    };
    #[cfg(test)]
    run_local_pid_monitor_registration_hook();
    // SAFETY: both allocations stay pinned for the complete registration.
    unsafe { hew_actor_monitor(watcher_pin.as_ptr(), target_pin.as_ptr(), out_monitor_id) }
}

#[cfg(test)]
type LocalPidMonitorRegistrationHook = Option<(Arc<Barrier>, Arc<Barrier>)>;

#[cfg(test)]
static LOCAL_PID_MONITOR_REGISTRATION_HOOK: LazyLock<Mutex<LocalPidMonitorRegistrationHook>> =
    LazyLock::new(|| Mutex::new(None));

#[cfg(test)]
fn run_local_pid_monitor_registration_hook() {
    let hook = LOCAL_PID_MONITOR_REGISTRATION_HOOK
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .clone();
    if let Some((entered, release)) = hook {
        entered.wait();
        release.wait();
    }
}

#[cfg(test)]
struct LocalPidMonitorRegistrationHookGuard;

#[cfg(test)]
impl LocalPidMonitorRegistrationHookGuard {
    fn install(entered: Arc<Barrier>, release: Arc<Barrier>) -> Self {
        *LOCAL_PID_MONITOR_REGISTRATION_HOOK
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = Some((entered, release));
        Self
    }
}

#[cfg(test)]
impl Drop for LocalPidMonitorRegistrationHookGuard {
    fn drop(&mut self) {
        *LOCAL_PID_MONITOR_REGISTRATION_HOOK
            .lock()
            .unwrap_or_else(PoisonError::into_inner) = None;
    }
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
        #[cfg(not(target_arch = "wasm32"))]
        ObservationTarget::Remote(target) => {
            if let Some(authority) = entry.authority {
                crate::hew_node::monitor_link::send_admitted_cleanup(
                    target, authority, ref_id, false,
                );
            } else if let WatcherAction::Monitor { watcher_actor_id } = entry.action {
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
pub(crate) fn notify_monitors_on_end(
    actor_id: u64,
    reason: i32,
    crash_kind: u32,
    end: crate::internal::types::ActorEndReason,
    close_fault: Option<&(i32, String)>,
) {
    let shard_index = get_shard_index(actor_id);
    // No runtime → no per-runtime monitor table → no monitors to notify. This
    // path is reachable from `hew_actor_trap` without an installed runtime
    // (supervisor-cascade unit tests), and the old process-global table simply
    // had no entries to sweep there. Tolerate it as before.
    let Some(state) = monitor_state_opt() else {
        return;
    };

    // Take all monitors for this actor ID.
    let snapshot = state.table[shard_index].access(|shard| {
        let monitors = shard.monitors.remove(&actor_id).unwrap_or_default();
        shard.terminal_reasons.insert(
            actor_id,
            TerminalMonitorReason {
                state: reason,
                crash_kind,
                end,
            },
        );

        // Also remove from ref_to_monitor mapping
        for monitor in &monitors {
            shard.ref_to_monitor.remove(&monitor.ref_id);
        }

        #[cfg(not(target_arch = "wasm32"))]
        {
            let target_serial = crate::pid::hew_pid_serial(actor_id);
            let remote_watchers: Vec<_> = state
                .remote_targets
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .remove(&target_serial)
                .unwrap_or_default()
                .into_iter()
                .map(|entry| RemoteWatcherTarget {
                    destination: entry.destination,
                    publication: entry.publication,
                    watcher: entry.watcher,
                    ref_id: entry.ref_id,
                    is_link: entry.is_link,
                })
                .collect();

            (monitors, remote_watchers)
        }
        #[cfg(target_arch = "wasm32")]
        {
            monitors
        }
    });

    #[cfg(not(target_arch = "wasm32"))]
    let (monitors, remote_watchers) = snapshot;
    #[cfg(target_arch = "wasm32")]
    let monitors = snapshot;

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
            HewDownMessage::local(monitor_id, actor_id, end, crash_kind),
        );
    }

    // Cross-node terminal sweep: if any REMOTE node monitors this
    // locally-dying actor, fan out a CTRL_MONITOR_DOWN carrying the same
    // terminal reason. The distributed table keys on the actor's serial; the
    // fan-out is fail-closed and no-ops when no remote watcher exists or no
    // node/conn-mgr is installed (R7), so the local sweep above is unaffected.
    #[cfg(not(target_arch = "wasm32"))]
    crate::hew_node::fan_out_remote_monitor_down(
        remote_watchers,
        reason,
        crash_kind,
        end,
        close_fault,
    );
    #[cfg(target_arch = "wasm32")]
    let _ = close_fault;
}

#[cfg(test)]
pub(crate) fn notify_monitors_on_death(actor_id: u64, reason: i32, crash_kind: u32) {
    notify_monitors_on_end(
        actor_id,
        reason,
        crash_kind,
        crate::internal::types::ActorEndReason::from_terminal(reason, false),
        None,
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
        // Incarnation identities never reuse within a runtime. Preserve the
        // terminal cause for monitors created through a retired actor token.
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
                let owns = entry.action.belongs_to(actor_id);
                if !owns {
                    return None;
                }
                match entry.target {
                    ObservationTarget::Local(target) => Some((ref_id, target)),
                    #[cfg(not(target_arch = "wasm32"))]
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
                || entry.action.belongs_to(actor_id)
        })
}

/// Returns true if any monitor index still contains `ref_id`.
#[cfg(test)]
fn has_monitor_ref_in_any_index(ref_id: u64) -> bool {
    let state = monitor_state();
    if state
        .observations
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .contains_key(&ref_id)
    {
        return true;
    }
    state.table.iter().any(|shard| {
        shard.read_access(|shard| {
            shard.ref_to_monitor.contains_key(&ref_id)
                || shard
                    .monitors
                    .values()
                    .any(|entries| entries.iter().any(|entry| entry.ref_id == ref_id))
        })
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

    #[test]
    fn local_pid_monitor_resolves_both_actor_identities_and_rejects_stale_target() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: empty state and no-op dispatch form valid actor spawns.
        let watcher =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: same contract as watcher.
        let target =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!watcher.is_null() && !target.is_null());
        // SAFETY: both actors remain live through monitor registration.
        let (watcher_id, watcher_token, target_id, target_token) = unsafe {
            (
                (*watcher).id,
                (*watcher).local_pid_id,
                (*target).id,
                (*target).local_pid_id,
            )
        };

        let mut monitor_id = 0;
        assert_eq!(
            unsafe {
                // SAFETY: out pointer is writable and both tokens resolve live actors.
                hew_local_pid_monitor(watcher_token, target_token, &raw mut monitor_id)
            },
            0
        );
        assert_ne!(monitor_id, 0);
        assert!(has_monitors_for_actor(watcher_id, watcher));
        assert!(has_monitors_for_actor(target_id, target));
        hew_actor_demonitor(monitor_id);
        assert!(!has_monitors_for_actor(watcher_id, watcher));
        assert!(!has_monitors_for_actor(target_id, target));

        let mut self_monitor_id = 0;
        assert_eq!(
            unsafe {
                // SAFETY: out pointer is writable; one live token supplies both roles.
                hew_local_pid_monitor(watcher_token, watcher_token, &raw mut self_monitor_id)
            },
            0
        );
        hew_actor_demonitor(self_monitor_id);
        assert!(!has_monitors_for_actor(watcher_id, watcher));

        // SAFETY: target is idle and no monitor remains.
        assert_eq!(unsafe { crate::actor::hew_actor_free(target) }, 0);
        let mut unchanged = u64::MAX;
        assert_eq!(
            unsafe {
                // SAFETY: out pointer is writable; stale target fails before write.
                hew_local_pid_monitor(watcher_token, target_token, &raw mut unchanged)
            },
            2
        );
        assert_eq!(unchanged, u64::MAX);

        // SAFETY: watcher remains idle and owned by this test.
        assert_eq!(unsafe { crate::actor::hew_actor_free(watcher) }, 0);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn native_monitor_reclaimed_target_preserves_terminal_cause_and_context() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: empty actors are test-owned; no scheduler workers execute them.
        unsafe {
            let watcher =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            let target =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!watcher.is_null() && !target.is_null());
            let target_id = (*target).id;
            let token = (*target).local_pid_id;
            let mut id = u64::MAX;
            let previous = crate::execution_context::set_current_context(std::ptr::null_mut());
            assert_eq!(
                hew_native_actor_monitor(token, &raw mut id),
                HEW_OBSERVATION_NO_CONTEXT
            );
            assert_eq!(id, u64::MAX, "NoContext must not initialize an owner");
            assert_eq!(
                crate::link::hew_native_actor_link(token),
                HEW_OBSERVATION_NO_CONTEXT
            );

            let mut context = crate::execution_context::HewExecutionContext {
                actor: watcher,
                actor_id: (*watcher).id,
                ..Default::default()
            };
            let _ = crate::execution_context::set_current_context(&raw mut context);
            assert_eq!(hew_native_actor_monitor(token, &raw mut id), 0);
            assert!(has_monitor_ref_in_any_index(id));
            hew_actor_demonitor(id);
            hew_actor_demonitor(id);
            assert!(!has_monitor_ref_in_any_index(id));
            let (_, wake) = crate::wake::blocking::Readiness::new();
            let observation =
                crate::actor_observe::hew_native_actor_observe_new(token, 1, wake.descriptor());
            assert_eq!(
                crate::actor_observe::hew_native_actor_observe_poll(observation),
                0
            );
            assert!(crate::link::has_links_for_actor((*watcher).id, watcher));
            crate::actor_observe::hew_native_actor_observe_free(observation);
            assert!(!crate::link::has_links_for_actor((*watcher).id, watcher));
            assert_eq!(crate::link::hew_native_actor_link(token), 0);
            let duplicate =
                crate::actor_observe::hew_native_actor_observe_new(token, 1, wake.descriptor());
            crate::actor_observe::hew_native_actor_observe_free(duplicate);
            assert!(
                crate::link::has_links_for_actor((*watcher).id, watcher),
                "cancelling a duplicate setup removed an existing link"
            );
            crate::link::hew_native_actor_unlink(token);

            let crash = crate::internal::types::HEW_TRAP_DIVIDE_BY_ZERO;
            let crash_kind =
                crate::internal::types::CrashKind::tag_from_error_code(crash).cast_unsigned();
            (*target)
                .actor_state
                .store(HewActorState::Crashed as i32, Ordering::Release);
            (*target).error_code.store(crash, Ordering::Release);
            notify_monitors_on_death(target_id, HewActorState::Crashed as i32, crash_kind);
            assert_eq!(crate::actor::hew_actor_free(target), 0);
            assert!(pin_actor_by_id(target_id).is_none());
            (*watcher)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);
            // Linking a retired incarnation succeeds and delivers its exit.
            assert_eq!(crate::link::hew_native_actor_link(token), 0);
            let mailbox = (*watcher).mailbox.cast::<mailbox::HewMailbox>();
            let exit = mailbox::hew_mailbox_try_recv_sys(mailbox);
            assert!(!exit.is_null(), "retired link target must deliver its exit");
            assert_eq!((*exit).msg_type, HewSysMsg::Exit.as_i32());
            mailbox::hew_msg_node_free(exit);
            for _ in 0..2 {
                let previous_id = id;
                assert_eq!(hew_native_actor_monitor(token, &raw mut id), 0);
                assert_ne!(id, previous_id);
                let node = mailbox::hew_mailbox_try_recv_sys(mailbox);
                assert!(
                    !node.is_null(),
                    "retired monitor must immediately deliver DOWN"
                );
                let down = &*((*node).data.cast::<HewDownMessage>());
                assert_eq!((*node).msg_type, HewSysMsg::Down.as_i32());
                assert_eq!(down.monitor_id, id);
                assert_eq!(down.target.kind, DOWN_TARGET_LOCAL);
                assert_eq!(down.target.slot, crate::pid::hew_pid_serial(target_id));
                assert_eq!(down.reason_kind, DOWN_REASON_CRASHED);
                assert_eq!(down.crash_kind, crash_kind);
                mailbox::hew_msg_node_free(node);
                hew_actor_demonitor(id);
                assert!(!has_monitor_ref_in_any_index(id));
            }
            assert!(mailbox::hew_mailbox_try_recv_sys(mailbox).is_null());
            let _ = crate::execution_context::set_current_context(previous);
            (*watcher)
                .actor_state
                .store(HewActorState::Idle as i32, Ordering::Release);
            assert_eq!(crate::actor::hew_actor_free(watcher), 0);
        }
    }

    fn local_pid_monitor_registration_race(free_watcher: bool) {
        let _guard = crate::runtime_test_guard();
        // SAFETY: empty state and no-op dispatch form valid actor spawns.
        let watcher =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: same contract as watcher.
        let target =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        assert!(!watcher.is_null() && !target.is_null());
        // SAFETY: both fresh actors are live.
        let (watcher_id, watcher_token, target_id, target_token) = unsafe {
            (
                (*watcher).id,
                (*watcher).local_pid_id,
                (*target).id,
                (*target).local_pid_id,
            )
        };

        let registration_entered = Arc::new(Barrier::new(2));
        let registration_release = Arc::new(Barrier::new(2));
        let _registration_hook = LocalPidMonitorRegistrationHookGuard::install(
            registration_entered.clone(),
            registration_release.clone(),
        );
        let free_entered = Arc::new(Barrier::new(2));
        let free_release = Arc::new(Barrier::new(2));
        let _free_hook = if free_watcher {
            crate::actor::install_free_post_retire_registration_hook_for_test(
                watcher_id,
                free_entered.clone(),
                free_release.clone(),
            )
        } else {
            crate::actor::install_free_pre_latch_registration_hook_for_test(
                target_id,
                free_entered.clone(),
                free_release.clone(),
            )
        };
        let registration = std::thread::spawn(move || {
            let mut monitor_id = 0;
            // SAFETY: the out pointer is thread-local and writable.
            let status =
                unsafe { hew_local_pid_monitor(watcher_token, target_token, &raw mut monitor_id) };
            (status, monitor_id)
        });
        registration_entered.wait();

        let retired_ptr = if free_watcher { watcher } else { target };
        let retired_id = if free_watcher { watcher_id } else { target_id };
        let retired_addr = retired_ptr as usize;
        let (done_tx, done_rx) = std::sync::mpsc::channel();
        let free = std::thread::spawn(move || {
            // SAFETY: the spawned actor remains allocated until its pins drain.
            let rc = unsafe { crate::actor::hew_actor_free(retired_addr as *mut HewActor) };
            done_tx.send(rc).expect("free completion receiver");
        });
        free_entered.wait();
        assert_eq!(
            crate::lifetime::live_actors::is_actor_live_with_id(retired_id, retired_ptr),
            !free_watcher,
            "watcher test pauses after retirement; target test pauses before latch"
        );
        assert!(
            matches!(
                done_rx.try_recv(),
                Err(std::sync::mpsc::TryRecvError::Empty)
            ),
            "free must await the pinned registration before its final scrub"
        );

        registration_release.wait();
        let (status, monitor_id) = registration.join().expect("monitor registration thread");
        assert_eq!(status, 0);
        assert_ne!(monitor_id, 0);
        assert!(has_monitor_ref_in_any_index(monitor_id));
        assert!(has_monitors_for_actor(watcher_id, watcher));
        assert!(has_monitors_for_actor(target_id, target));

        free_release.wait();
        free.join().expect("free thread");
        assert_eq!(done_rx.recv().expect("free result"), 0);

        assert!(!has_monitors_for_actor(watcher_id, std::ptr::null_mut()));
        assert!(!has_monitors_for_actor(target_id, std::ptr::null_mut()));
        assert!(!has_monitor_ref_in_any_index(monitor_id));
        hew_actor_demonitor(monitor_id);

        let survivor = if free_watcher { target } else { watcher };
        // SAFETY: survivor is still allocated, live, and quiescent/terminal.
        assert_eq!(unsafe { crate::actor::hew_actor_free(survivor) }, 0);
    }

    #[test]
    fn target_free_waits_for_pinned_local_monitor_then_scrubs_registry() {
        local_pid_monitor_registration_race(false);
    }

    #[test]
    fn watcher_free_waits_for_pinned_local_monitor_then_scrubs_registry() {
        local_pid_monitor_registration_race(true);
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
            coalesce_match_fn: None,
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
            local_pid_id: crate::lifetime::local_handles::HewLocalPidId::INVALID,
            spawn_serial: id,
            sys_dispatch: None,
            state_drop_consumed: AtomicBool::new(false),
            state_drop_borrowed: AtomicBool::new(false),
            parked_ask_channel: AtomicPtr::new(std::ptr::null_mut()),
            checked_invocation: AtomicPtr::new(std::ptr::null_mut()),
            #[cfg(not(target_arch = "wasm32"))]
            pending_external_trap_code: AtomicI32::new(0),
            #[cfg(not(target_arch = "wasm32"))]
            native_completion: None,
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
            .cloned();
        assert!(matches!(observation, Some(ObservationEntry {
            target: ObservationTarget::Local(target),
            action: WatcherAction::Monitor { watcher_actor_id }, ..
        }) if target == target_id && watcher_actor_id == watcher_id));

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

        // No current actor is the one refusal; it must not write the out id.
        // SAFETY: a null watcher is checked before any other pointer is read.
        unsafe {
            let mut monitor_id = 99;
            assert_eq!(
                hew_actor_monitor(std::ptr::null_mut(), actor_ptr, &raw mut monitor_id),
                HEW_OBSERVATION_NO_CONTEXT
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
            let notify = std::thread::spawn(move || {
                notify_monitors_on_death(target_id, HewActorState::Crashed as i32, 0);
            });
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
            assert_eq!((*node).msg_type, HewSysMsg::Down.as_i32());
            let payload = &*((*node).data.cast::<HewDownMessage>());
            assert_eq!(payload.monitor_id, ref_id);
            assert_eq!(payload.target.kind, DOWN_TARGET_LOCAL);
            assert_eq!(payload.target.slot, crate::pid::hew_pid_serial(target_id));
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
                    end: crate::internal::types::ActorEndReason::Crashed,
                }),
                "the installed runtime's monitor table must record the terminal sweep"
            );
        });
    }

    fn remote_location(byte: u8, slot: u64, incarnation: u32) -> Location {
        Location::direct(NodeId::from_bytes([byte; 16]), slot, incarnation).unwrap()
    }

    #[test]
    fn remote_down_retains_terminated_reason_across_wire() {
        let target = remote_location(7, 13, 2);
        let payload = crate::envelope::MonitorDownPayload {
            ref_id: 41,
            target,
            reason: HewActorState::Stopped as i32,
            end_reason: crate::internal::types::ActorEndReason::Terminated as u8,
            crash_kind: 0,
            close_fault: None,
        };
        let bytes = crate::envelope::encode_monitor_down_payload(&payload).unwrap();
        let decoded = crate::envelope::decode_monitor_down_payload(&bytes).unwrap();
        let down = HewDownMessage::remote(
            decoded.ref_id,
            decoded.target,
            decoded.crash_kind.cast_unsigned(),
            decoded.end_reason,
        );
        assert_eq!(down.reason_kind, DOWN_REASON_TERMINATED);
        assert_eq!(down.crash_kind, 0);
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

        assert!(state
            .deliver_monitor_to_ref(monitor_id, stale_target)
            .is_none());
        assert_eq!(state.pending_observation_count(), 1);

        let down = state
            .deliver_monitor_to_ref(monitor_id, target)
            .expect("matching terminal claim");
        assert_eq!(down.monitor_id, monitor_id);
        assert_eq!(down.watcher_actor_id, 20);
        assert_eq!(down.target, target);
        assert!(state.deliver_monitor_to_ref(monitor_id, target).is_none());
        assert_eq!(state.pending_observation_count(), 0);
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn member_dead_claims_only_monitor_observations_for_its_publication() {
        let state = MonitorState::new();
        let node = NodeId::from_bytes([7; 16]);
        let old_target = Location::direct(node, 13, 1).unwrap();
        let new_target = Location::direct(node, 13, 1).unwrap();
        for (id, target, publication) in [(41, old_target, 10), (42, new_target, 20)] {
            state.insert_admitted_observation(
                id,
                target,
                WatcherAction::Monitor {
                    watcher_actor_id: 99,
                },
                ObservationAuthority {
                    grant: 1,
                    publication,
                    watcher: remote_location(8, 99, 1),
                },
                None,
            );
        }

        let retired = state.take_monitor_downs_for_owner(node, 1, 10);
        assert_eq!(retired.len(), 1);
        assert_eq!(retired[0].monitor_id, 41);
        assert_eq!(retired[0].target, old_target);
        assert_eq!(state.pending_observation_count(), 1);

        let successor = state.take_monitor_downs_for_owner(node, 1, 20);
        assert_eq!(successor.len(), 1);
        assert_eq!(successor[0].monitor_id, 42);
        assert_eq!(successor[0].target, new_target);
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
        assert!(state.deliver_monitor_to_ref(monitor_id, target).is_none());
    }

    #[test]
    fn remote_link_terminal_claim_is_exactly_once() {
        let state = MonitorState::new();
        let target = remote_location(5, 13, 3);
        let link_id = state
            .register_link_watcher(target, 24, 3)
            .expect("link observation id");

        let down = state
            .deliver_link_down_to_ref(link_id, target, HewActorState::Crashed as i32, 1)
            .expect("link terminal claim");
        assert_eq!(down.local_actor_id, 24);
        assert_eq!(down.target, target);
        assert_eq!(down.crash_kind, 1);
        assert_eq!(down.policy_tag, 3);
        assert_eq!(
            state.deliver_link_down_to_ref(link_id, target, HewActorState::Crashed as i32, 1),
            None
        );
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn remote_link_wire_notice_preserves_target_and_class_once() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test owns an empty actor without scheduler workers.
        unsafe {
            let watcher =
                crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch));
            assert!(!watcher.is_null());
            (*watcher)
                .actor_state
                .store(HewActorState::Runnable as i32, Ordering::Release);
            let target = remote_location(9, 73, 6);
            let id = monitor_state()
                .register_link_watcher(target, (*watcher).id, 3)
                .unwrap();
            let payload = crate::envelope::MonitorDownPayload {
                ref_id: id,
                target,
                reason: HewActorState::Crashed as i32,
                end_reason: crate::internal::types::ActorEndReason::Crashed as u8,
                crash_kind: crate::internal::types::CrashKind::HeapExceeded.tag(),
                close_fault: None,
            };
            let bytes = crate::envelope::encode_monitor_down_payload(&payload).unwrap();
            let decoded = crate::envelope::decode_monitor_down_payload(&bytes).unwrap();
            let mailbox = (*watcher).mailbox.cast::<mailbox::HewMailbox>();
            crate::hew_node::handle_inbound_link_down(
                id,
                remote_location(9, 73, 5),
                decoded.reason,
                decoded.crash_kind,
            );
            assert!(mailbox::hew_mailbox_try_recv_sys(mailbox).is_null());
            for _ in 0..2 {
                crate::hew_node::handle_inbound_link_down(
                    decoded.ref_id,
                    decoded.target,
                    decoded.reason,
                    decoded.crash_kind,
                );
            }
            let node = mailbox::hew_mailbox_try_recv_sys(mailbox);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, HewSysMsg::Exit.as_i32());
            let exit = &*((*node).data.cast::<crate::link::ExitMessage>());
            assert_eq!(exit.target, HewNotificationTarget::remote(target));
            assert_eq!(
                exit.crash_kind,
                crate::internal::types::CrashKind::HeapExceeded.tag()
            );
            mailbox::hew_msg_node_free(node);
            assert!(mailbox::hew_mailbox_try_recv_sys(mailbox).is_null());
            (*watcher)
                .actor_state
                .store(HewActorState::Idle as i32, Ordering::Release);
            assert_eq!(crate::actor::hew_actor_free(watcher), 0);
        }
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
    #[test]
    fn reciprocal_setup_requires_the_exact_live_publication_and_cancels_in_either_order() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: the test owns this actor and sends it no executable messages.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: the actor remains live throughout its observation registrations.
        let actor_id = unsafe { (*actor).id };
        // SAFETY: this fixture retains the same actor until all registrations retire.
        let selected = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(actor) };
        let local = remote_location(91, crate::pid::hew_pid_serial(actor_id), 1);
        let remote = remote_location(92, 11, 3);
        let state = monitor_state();
        for cancel_first in [false, true] {
            let id = state.next_observation_id().unwrap();
            state.insert_admitted_observation(
                id,
                remote,
                WatcherAction::Link {
                    local_actor_id: actor_id,
                    policy_tag: 3,
                },
                ObservationAuthority {
                    grant: 7,
                    publication: 17,
                    watcher: local,
                },
                None,
            );
            let reciprocal = crate::envelope::LinkReqPayload {
                grant_id: 7,
                reciprocal_ref: id,
                linker: remote,
                ref_id: 99,
                target: local,
                policy_tag: 3,
                reciprocate: 0,
                setup_id: 101,
            };
            assert!(state.permits_terminal(id, remote, 17));
            assert!(!state.permits_terminal(id, remote, 18));
            assert!(state.permits_reciprocal(&reciprocal, 17));
            assert!(!state.permits_reciprocal(&reciprocal, 18));
            for malformed in [
                crate::envelope::LinkReqPayload {
                    grant_id: 8,
                    ..reciprocal.clone()
                },
                crate::envelope::LinkReqPayload {
                    reciprocal_ref: id + 1,
                    ..reciprocal.clone()
                },
                crate::envelope::LinkReqPayload {
                    policy_tag: 0,
                    ..reciprocal.clone()
                },
                crate::envelope::LinkReqPayload {
                    target: remote_location(91, 999, 1),
                    ..reciprocal.clone()
                },
                crate::envelope::LinkReqPayload {
                    linker: remote_location(92, 12, 3),
                    ..reciprocal.clone()
                },
            ] {
                assert_eq!(
                    state.register_reciprocal_watcher(selected, &malformed, 17),
                    RemoteWatcherSetup::Refused
                );
            }
            if cancel_first {
                state.remove_remote_observation(id);
            }
            assert_eq!(
                state.register_reciprocal_watcher(selected, &reciprocal, 17),
                if cancel_first {
                    RemoteWatcherSetup::Refused
                } else {
                    RemoteWatcherSetup::Registered
                }
            );
            if !cancel_first {
                state.remove_remote_observation(id);
            }
            assert!(state
                .take_remote_watchers(crate::pid::hew_pid_serial(actor_id))
                .is_empty());
        }
        // SAFETY: both cancellation orders retired every observation of this actor.
        unsafe { crate::actor::hew_actor_free(actor) };
    }

    #[test]
    fn remote_role_registration_retracts_by_semantic_destination_and_publication() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: the test exclusively owns this message-free actor.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        // SAFETY: the allocation remains live until both registrations retire.
        let selected = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(actor) };
        let watcher = remote_location(93, 19, 1);
        let role = Location::role(watcher.node(), (1_u64 << 60) + 7, 1).unwrap();
        let direct = Location::direct(role.node(), role.target().id(), 1).unwrap();
        let state = monitor_state();
        for destination in [role, direct] {
            assert_eq!(
                state.register_admitted_watcher(selected, destination, watcher, 7, false, 17, 0),
                RemoteWatcherSetup::Registered,
            );
        }
        state.retract_admitted_watcher(role, watcher, 7, 18);
        let retained = state.take_remote_watchers(crate::pid::hew_pid_serial(selected.actor_id()));
        assert_eq!(retained.len(), 2);
        assert!(retained.iter().any(|entry| entry.destination == role));
        for destination in [role, direct] {
            assert_eq!(
                state.register_admitted_watcher(selected, destination, watcher, 7, false, 17, 0),
                RemoteWatcherSetup::Registered,
            );
        }
        state.retract_admitted_watcher(role, watcher, 7, 17);
        let remaining = state.take_remote_watchers(crate::pid::hew_pid_serial(selected.actor_id()));
        assert_eq!(remaining.len(), 1);
        assert_eq!(remaining[0].destination, direct);
        let target = HewNotificationTarget::remote(role);
        assert_eq!(target.kind, DOWN_TARGET_REMOTE_ROLE);
        assert_eq!(target.slot, (1_u64 << 60) + 7);
        assert_eq!(target.session_incarnation, role.incarnation());
        // SAFETY: both selected registrations have retired and no messages were sent.
        unsafe { crate::actor::hew_actor_free(actor) };
    }

    #[test]
    fn remote_registration_waits_for_native_cleanup_and_preserves_late_terminal_reason() {
        let _guard = crate::runtime_test_guard();
        // SAFETY: this test exclusively owns a message-free actor allocation.
        let actor =
            unsafe { crate::actor::hew_actor_spawn(std::ptr::null_mut(), 0, Some(noop_dispatch)) };
        let completion = Arc::new(crate::actor_native::NativeActorCompletion::default());
        // SAFETY: no scheduler activation borrows this fixture's actor.
        unsafe {
            (*actor).native_completion = Some(completion.clone());
            (*actor)
                .actor_state
                .store(HewActorState::Stopped as i32, Ordering::Release);
            let state = monitor_state();
            let watcher = remote_location(93, 19, 1);
            let destination = remote_location(94, crate::pid::hew_pid_serial((*actor).id), 1);
            assert_eq!(
                state.register_admitted_watcher(
                    crate::lifetime::live_actors::ActorIncarnation::of(actor),
                    destination,
                    watcher,
                    1,
                    false,
                    17,
                    0
                ),
                RemoteWatcherSetup::Registered
            );
            state.remove_remote_watcher(destination, watcher, 1);
            completion.record_close_fault(crate::fault::HewFault::with_message(
                212,
                "late cleanup failure".into(),
            ));
            completion.finish(212);
            let expected = RemoteTerminal {
                reason: TerminalMonitorReason {
                    state: HewActorState::Stopped as i32,
                    crash_kind: 0,
                    end: crate::internal::types::ActorEndReason::Stopped,
                },
                close_fault: Some((212, "late cleanup failure".into())),
            };
            assert_eq!(
                state.register_admitted_watcher(
                    crate::lifetime::live_actors::ActorIncarnation::of(actor),
                    destination,
                    watcher,
                    2,
                    false,
                    17,
                    0
                ),
                RemoteWatcherSetup::Terminal(expected.clone()),
            );
            notify_monitors_on_end(
                (*actor).id,
                expected.reason.state,
                expected.reason.crash_kind,
                expected.reason.end,
                expected.close_fault.as_ref(),
            );
            assert_eq!(
                state.register_admitted_watcher(
                    crate::lifetime::live_actors::ActorIncarnation::of(actor),
                    destination,
                    watcher,
                    3,
                    false,
                    17,
                    0
                ),
                RemoteWatcherSetup::Terminal(expected),
            );
            crate::actor::hew_actor_free(actor);
        }
    }
}
