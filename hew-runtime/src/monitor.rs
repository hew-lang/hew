//! Actor monitors implementation for unidirectional death notifications.
//!
//! In Erlang-style actor systems, monitors are unidirectional: when actor A
//! monitors actor B, if B dies, A receives a DOWN message but does NOT crash.
//! This module implements the monitor table and death notification logic.

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{LazyLock, RwLock};

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::mailbox;
use crate::supervisor::SYS_MSG_DOWN;

/// Number of shards for monitor table to reduce contention.
const MONITOR_SHARDS: usize = 16;

/// Unique reference ID generator for monitors.
static MONITOR_REF_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Entry in the monitor table.
#[derive(Debug, Clone)]
struct MonitorEntry {
    /// Actor that is monitoring (will receive DOWN message).
    /// Using usize instead of *mut `HewActor` for thread safety.
    monitoring_actor: usize,
    /// Unique reference ID for this monitor (for demonitor).
    ref_id: u64,
}

/// Shard in the monitor table.
#[derive(Debug)]
struct MonitorShard {
    /// Maps `monitored_actor_id` -> Vec of monitors watching that actor.
    monitors: HashMap<u64, Vec<MonitorEntry>>,
    /// Maps `ref_id` -> (`monitored_actor_id`, `monitoring_actor`) for demonitor.
    ref_to_monitor: HashMap<u64, (u64, usize)>,
}

/// Global sharded monitor table.
/// We use usize to store actor pointers to make it Send+Sync safe.
static MONITOR_TABLE: LazyLock<[RwLock<MonitorShard>; MONITOR_SHARDS]> = LazyLock::new(|| {
    std::array::from_fn(|_| {
        RwLock::new(MonitorShard {
            monitors: HashMap::new(),
            ref_to_monitor: HashMap::new(),
        })
    })
});

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

/// Create a monitor: watcher monitors target.
/// Returns a unique reference ID for this monitor.
///
/// # Panics
///
/// Panics if the monitor table's `RwLock` is poisoned.
///
/// # Safety
///
/// Both `watcher` and `target` must be valid pointers to [`HewActor`] structs.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_monitor(watcher: *mut HewActor, target: *mut HewActor) -> u64 {
    if watcher.is_null() || target.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees both pointers are valid.
    let _watcher_ref = unsafe { &*watcher };
    // SAFETY: Caller guarantees both pointers are valid.
    let target_ref = unsafe { &*target };

    let target_id = target_ref.id;

    // Generate unique reference ID.
    let ref_id = MONITOR_REF_COUNTER.fetch_add(1, Ordering::Relaxed);

    let monitor_entry = MonitorEntry {
        monitoring_actor: watcher as usize,
        ref_id,
    };

    let shard_index = get_shard_index(target_id);
    let mut shard = MONITOR_TABLE[shard_index].write().unwrap();

    // Add to monitors: target_id -> [monitoring actors]
    shard
        .monitors
        .entry(target_id)
        .or_default()
        .push(monitor_entry);

    // Add to ref lookup: ref_id -> (target_id, watcher)
    shard
        .ref_to_monitor
        .insert(ref_id, (target_id, watcher as usize));

    ref_id
}

/// Remove a monitor by its reference ID.
///
/// # Panics
///
/// Panics if any monitor table's `RwLock` is poisoned.
#[no_mangle]
pub extern "C" fn hew_actor_demonitor(ref_id: u64) {
    if ref_id == 0 {
        return;
    }

    // Find which shard contains this ref_id by checking all shards.
    // This is not optimal but monitors are typically rare operations.
    for shard_index in 0..MONITOR_SHARDS {
        let mut shard = MONITOR_TABLE[shard_index].write().unwrap();

        if let Some((target_id, _watcher_addr)) = shard.ref_to_monitor.remove(&ref_id) {
            // Remove from monitors list
            if let Some(monitor_list) = shard.monitors.get_mut(&target_id) {
                monitor_list.retain(|entry| entry.ref_id != ref_id);
                if monitor_list.is_empty() {
                    shard.monitors.remove(&target_id);
                }
            }
            return;
        }
    }
}

/// Send DOWN notifications to all actors monitoring the dead actor.
///
/// This function is called from `hew_actor_trap` after the actor has
/// transitioned to a terminal state. It removes all monitors for the
/// dead actor and sends DOWN messages to all monitoring actors.
pub(crate) fn notify_monitors_on_death(actor_id: u64, reason: i32) {
    let shard_index = get_shard_index(actor_id);

    // Take all monitors for this actor ID.
    let monitors = {
        let mut shard = MONITOR_TABLE[shard_index].write().unwrap();
        let monitors = shard.monitors.remove(&actor_id).unwrap_or_default();

        // Also remove from ref_to_monitor mapping
        for monitor in &monitors {
            shard.ref_to_monitor.remove(&monitor.ref_id);
        }

        monitors
    };

    // Send DOWN messages to all monitoring actors.
    for monitor in monitors {
        let monitoring_actor_addr = monitor.monitoring_actor;
        if monitoring_actor_addr == 0 {
            continue;
        }

        let monitoring_actor = monitoring_actor_addr as *mut HewActor;
        // SAFETY: monitoring_actor was stored from a valid HewActor pointer.
        // The actor might have been freed, but we handle null mailbox gracefully.
        let monitoring_actor_ref = unsafe { &*monitoring_actor };
        let mailbox = monitoring_actor_ref.mailbox.cast::<mailbox::HewMailbox>();

        if !mailbox.is_null() {
            // Prepare DOWN message data: { monitored_actor_id: u64, ref_id: u64, reason: i32 }
            let down_data = DownMessage {
                monitored_actor_id: actor_id,
                ref_id: monitor.ref_id,
                reason,
            };

            let data_ptr = (&raw const down_data).cast::<c_void>();
            let data_size = std::mem::size_of::<DownMessage>();

            // SAFETY: mailbox is valid for the actor's lifetime, down_data is valid.
            unsafe {
                mailbox::hew_mailbox_send_sys(
                    mailbox,
                    SYS_MSG_DOWN,
                    data_ptr.cast_mut(),
                    data_size,
                );
            }

            // Wake the monitoring actor so it processes the DOWN message.
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
                crate::scheduler::sched_enqueue(monitoring_actor);
            }
        }
    }
}

/// Message data for DOWN system messages.
#[repr(C)]
#[derive(Debug)]
struct DownMessage {
    /// ID of the monitored actor that died.
    monitored_actor_id: u64,
    /// Reference ID of the monitor (from `hew_actor_monitor`).
    ref_id: u64,
    /// Reason code (`error_code` from `hew_actor_trap`).
    reason: i32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, AtomicPtr};

    fn create_test_actor(id: u64) -> HewActor {
        HewActor {
            sched_link_next: AtomicPtr::new(std::ptr::null_mut()),
            id,
            pid: id,
            state: std::ptr::null_mut(),
            state_size: 0,
            dispatch: None,
            mailbox: std::ptr::null_mut(),
            actor_state: AtomicI32::new(HewActorState::Idle as i32),
            budget: AtomicI32::new(0),
            init_state: std::ptr::null_mut(),
            init_state_size: 0,
            coalesce_key_fn: None,
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
        }
    }

    #[test]
    fn test_monitor_creation_and_demonitor() {
        // Use unique IDs to avoid collisions with parallel tests.
        let watcher_id = 10_100;
        let target_id = 10_200;
        let mut watcher = create_test_actor(watcher_id);
        let mut target = create_test_actor(target_id);

        let watcher_ptr = &mut watcher as *mut HewActor;
        let target_ptr = &mut target as *mut HewActor;

        // Create monitor
        let ref_id = unsafe { hew_actor_monitor(watcher_ptr, target_ptr) };

        assert_ne!(ref_id, 0);

        // Verify monitor exists
        let shard_index = get_shard_index(target_id);
        {
            let shard = MONITOR_TABLE[shard_index].read().unwrap();
            let monitors = shard
                .monitors
                .get(&target_id)
                .expect("monitor should exist");
            // Find our specific monitor entry.
            let our_monitor = monitors.iter().find(|m| m.ref_id == ref_id);
            assert!(our_monitor.is_some(), "our monitor entry should exist");
            assert_eq!(our_monitor.unwrap().monitoring_actor, watcher_ptr as usize);

            assert!(shard.ref_to_monitor.contains_key(&ref_id));
        }

        // Remove monitor
        hew_actor_demonitor(ref_id);

        // Verify monitor is removed
        {
            let shard = MONITOR_TABLE[shard_index].read().unwrap();
            assert!(
                !shard.monitors.contains_key(&target_id)
                    || shard
                        .monitors
                        .get(&target_id)
                        .map_or(true, |v| v.is_empty())
            );
            assert!(!shard.ref_to_monitor.contains_key(&ref_id));
        }
    }

    #[test]
    fn test_multiple_monitors_same_target() {
        // Use unique IDs to avoid collisions with parallel tests.
        let mut watcher1 = create_test_actor(20_100);
        let mut watcher2 = create_test_actor(20_110);
        let mut target = create_test_actor(20_200);

        let watcher1_ptr = &mut watcher1 as *mut HewActor;
        let watcher2_ptr = &mut watcher2 as *mut HewActor;
        let target_ptr = &mut target as *mut HewActor;

        // Create two monitors for same target
        let ref_id1 = unsafe { hew_actor_monitor(watcher1_ptr, target_ptr) };
        let ref_id2 = unsafe { hew_actor_monitor(watcher2_ptr, target_ptr) };

        assert_ne!(ref_id1, ref_id2);

        // Verify both monitors exist
        let shard_index = get_shard_index(20_200);
        {
            let shard = MONITOR_TABLE[shard_index].read().unwrap();
            let monitors = shard.monitors.get(&20_200).expect("monitors should exist");
            assert_eq!(monitors.len(), 2);
        }

        // Remove first monitor
        hew_actor_demonitor(ref_id1);

        // Verify only second monitor remains
        {
            let shard = MONITOR_TABLE[shard_index].read().unwrap();
            let monitors = shard
                .monitors
                .get(&20_200)
                .expect("one monitor should remain");
            assert_eq!(monitors.len(), 1);
            assert_eq!(monitors[0].ref_id, ref_id2);
        }

        // Remove second monitor
        hew_actor_demonitor(ref_id2);

        // Verify all monitors removed
        {
            let shard = MONITOR_TABLE[shard_index].read().unwrap();
            assert!(
                !shard.monitors.contains_key(&20_200)
                    || shard.monitors.get(&20_200).map_or(true, |v| v.is_empty())
            );
        }
    }

    #[test]
    fn test_null_actor_handling() {
        let mut actor = create_test_actor(300);
        let actor_ptr = &mut actor as *mut HewActor;

        // These should not panic and should return 0
        unsafe {
            assert_eq!(hew_actor_monitor(std::ptr::null_mut(), actor_ptr), 0);
            assert_eq!(hew_actor_monitor(actor_ptr, std::ptr::null_mut()), 0);
            assert_eq!(
                hew_actor_monitor(std::ptr::null_mut(), std::ptr::null_mut()),
                0
            );
        }

        // Demonitor with invalid ref_id should not panic
        hew_actor_demonitor(0);
        hew_actor_demonitor(99999);
    }
}
