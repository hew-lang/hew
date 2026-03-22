//! Actor monitors implementation for unidirectional death notifications.
//!
//! In Erlang-style actor systems, monitors are unidirectional: when actor A
//! monitors actor B, if B dies, A receives a DOWN message but does NOT crash.
//! This module implements the monitor table and death notification logic.

use crate::util::RwLockExt;
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
    let mut shard = MONITOR_TABLE[shard_index].write_or_recover();
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
#[no_mangle]
pub extern "C" fn hew_actor_demonitor(ref_id: u64) {
    if ref_id == 0 {
        return;
    }

    // Find which shard contains this ref_id by checking all shards.
    // This is not optimal but monitors are typically rare operations.
    for shard_index in 0..MONITOR_SHARDS {
        let mut shard = MONITOR_TABLE[shard_index].write_or_recover();

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
        let mut shard = MONITOR_TABLE[shard_index].write_or_recover();
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

        // Guard: skip actors that have already been freed.
        if !crate::actor::is_actor_live(monitoring_actor) {
            continue;
        }

        // SAFETY: monitoring_actor is live (checked above).
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

/// Remove all monitor entries for a dying actor — both as a monitored target
/// and as a monitoring watcher — from all shards.
///
/// Called from `hew_actor_free` before deallocation to prevent
/// `notify_monitors_on_death` from dereferencing freed memory.
pub(crate) fn remove_all_monitors_for_actor(actor_id: u64, actor_addr: *mut HewActor) {
    let actor_usize = actor_addr as usize;

    // Remove any monitors-on-this-actor entry from its shard.
    let own_shard = get_shard_index(actor_id);
    {
        let mut shard = MONITOR_TABLE[own_shard].write_or_recover();
        if let Some(monitors) = shard.monitors.remove(&actor_id) {
            for m in &monitors {
                shard.ref_to_monitor.remove(&m.ref_id);
            }
        }
    }

    // Scan all shards and remove this actor's address from other actors'
    // monitor watcher lists (where this actor was the watcher).
    for shard_rw in MONITOR_TABLE.iter() {
        let mut shard = shard_rw.write_or_recover();
        let mut refs_to_remove = Vec::new();
        for monitor_list in shard.monitors.values_mut() {
            monitor_list.retain(|entry| {
                if entry.monitoring_actor == actor_usize {
                    refs_to_remove.push(entry.ref_id);
                    false
                } else {
                    true
                }
            });
        }
        // Clean up empty entries and ref lookups.
        shard.monitors.retain(|_id, list| !list.is_empty());
        for ref_id in refs_to_remove {
            shard.ref_to_monitor.remove(&ref_id);
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

/// Returns true if any monitor entries reference the given actor (as monitored
/// target by ID or as watcher by address).
#[cfg(test)]
pub(crate) fn has_monitors_for_actor(actor_id: u64, actor_addr: *mut HewActor) -> bool {
    let actor_usize = actor_addr as usize;
    let own_shard = get_shard_index(actor_id);
    {
        let shard = MONITOR_TABLE[own_shard].read().unwrap();
        if shard.monitors.contains_key(&actor_id) {
            return true;
        }
    }
    // Check if this actor appears as a watcher in any shard.
    for shard_rw in MONITOR_TABLE.iter() {
        let shard = shard_rw.read().unwrap();
        for monitors in shard.monitors.values() {
            if monitors.iter().any(|m| m.monitoring_actor == actor_usize) {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr};

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
            terminate_fn: None,
            terminate_called: AtomicBool::new(false),
            terminate_finished: AtomicBool::new(false),
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

        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // Create monitor
        // SAFETY: Both pointers are valid stack-allocated test actors.
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
                        .is_none_or(std::vec::Vec::is_empty)
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

        let watcher1_ptr = &raw mut watcher1;
        let watcher2_ptr = &raw mut watcher2;
        let target_ptr = &raw mut target;

        // Create two monitors for same target
        // SAFETY: Both pointers are valid stack-allocated test actors.
        let ref_id1 = unsafe { hew_actor_monitor(watcher1_ptr, target_ptr) };
        // SAFETY: Both pointers are valid stack-allocated test actors.
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
                    || shard
                        .monitors
                        .get(&20_200)
                        .is_none_or(std::vec::Vec::is_empty)
            );
        }
    }

    #[test]
    fn test_null_actor_handling() {
        let mut actor = create_test_actor(300);
        let actor_ptr = &raw mut actor;

        // These should not panic and should return 0
        // SAFETY: Testing null pointer handling; function returns 0 for null.
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

    #[test]
    fn remove_all_monitors_clears_target_and_watcher_entries() {
        // Use unique IDs (40xxx) to avoid collisions.
        let mut watcher = create_test_actor(40_100);
        let mut target = create_test_actor(40_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { hew_actor_monitor(watcher_ptr, target_ptr) };
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
        // Unique IDs (41xxx).
        let mut watcher = create_test_actor(41_100);
        let mut target = create_test_actor(41_200);
        let watcher_ptr = &raw mut watcher;
        let target_ptr = &raw mut target;

        // SAFETY: Valid stack-allocated test actors.
        let ref_id = unsafe { hew_actor_monitor(watcher_ptr, target_ptr) };
        assert_ne!(ref_id, 0);

        // Act: remove all entries where watcher appears (as if freeing watcher).
        remove_all_monitors_for_actor(41_100, watcher_ptr);

        // Watcher's address should be purged from target's monitor list.
        let shard = get_shard_index(41_200);
        let table = MONITOR_TABLE[shard].read().unwrap();
        let remaining = table.monitors.get(&41_200);
        assert!(
            remaining.is_none() || remaining.unwrap().is_empty(),
            "target's monitor list should no longer contain the freed watcher"
        );
    }

    #[test]
    fn remove_all_monitors_no_monitors_does_not_panic() {
        let actor = create_test_actor(40_300);
        let ptr = (&raw const actor).cast_mut();
        remove_all_monitors_for_actor(40_300, ptr);
        assert!(!has_monitors_for_actor(40_300, ptr));
    }
}
