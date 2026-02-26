//! Actor links implementation for fail-together semantics.
//!
//! In Erlang-style actor systems, links are bidirectional: when one linked
//! actor crashes, all linked actors also crash (unless they are trapping exits).
//! This module implements the link table and crash propagation logic.

use std::collections::HashMap;
use std::ffi::c_void;
use std::sync::{LazyLock, RwLock};

use crate::actor::HewActor;
use crate::internal::types::HewActorState;
use crate::mailbox;
use crate::supervisor::SYS_MSG_EXIT;

/// Number of shards for link table to reduce contention.
const LINK_SHARDS: usize = 16;

/// Entry in the link table mapping `actor_id` -> linked actors.
#[derive(Debug)]
struct LinkShard {
    /// Maps `actor_id` to Vec of actors linked to that actor.
    /// Using usize instead of *mut `HewActor` for thread safety.
    links: HashMap<u64, Vec<usize>>,
}

/// Global sharded link table.
/// We use usize to store actor pointers to make it Send+Sync safe.
/// The runtime guarantees actors remain valid while linked.
static LINK_TABLE: LazyLock<[RwLock<LinkShard>; LINK_SHARDS]> = LazyLock::new(|| {
    std::array::from_fn(|_| {
        RwLock::new(LinkShard {
            links: HashMap::new(),
        })
    })
});

/// Get shard index for an actor ID.
fn get_shard_index(actor_id: u64) -> usize {
    #[expect(
        clippy::cast_possible_truncation,
        reason = "shard index is bounded by LINK_SHARDS (16)"
    )]
    {
        (actor_id as usize) % LINK_SHARDS
    }
}

/// Create a bidirectional link between two actors.
///
/// # Safety
///
/// Both `a` and `b` must be valid pointers to [`HewActor`] structs.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_link(a: *mut HewActor, b: *mut HewActor) {
    if a.is_null() || b.is_null() || a == b {
        return;
    }

    // SAFETY: Caller guarantees `a` is a valid pointer.
    let actor_a = unsafe { &*a };
    // SAFETY: Caller guarantees `b` is a valid pointer.
    let actor_b = unsafe { &*b };

    let id_a = actor_a.id;
    let id_b = actor_b.id;

    // Add bidirectional links: A -> B and B -> A
    add_link(id_a, b);
    add_link(id_b, a);
}

/// Remove a bidirectional link between two actors.
///
/// # Safety
///
/// Both `a` and `b` must be valid pointers to [`HewActor`] structs.
#[no_mangle]
pub unsafe extern "C" fn hew_actor_unlink(a: *mut HewActor, b: *mut HewActor) {
    if a.is_null() || b.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `a` is a valid pointer.
    let actor_a = unsafe { &*a };
    // SAFETY: Caller guarantees `b` is a valid pointer.
    let actor_b = unsafe { &*b };

    let id_a = actor_a.id;
    let id_b = actor_b.id;

    // Remove bidirectional links: A -/-> B and B -/-> A
    remove_link(id_a, b);
    remove_link(id_b, a);
}

/// Add a unidirectional link: `from_id` -> `to_actor`.
fn add_link(from_id: u64, to_actor: *mut HewActor) {
    let shard_index = get_shard_index(from_id);
    let mut shard = LINK_TABLE[shard_index].write().unwrap();

    shard
        .links
        .entry(from_id)
        .or_default()
        .push(to_actor as usize);
}

/// Remove a unidirectional link: `from_id` -/-> `to_actor`.
fn remove_link(from_id: u64, to_actor: *mut HewActor) {
    let shard_index = get_shard_index(from_id);
    let mut shard = LINK_TABLE[shard_index].write().unwrap();

    if let Some(linked_actors) = shard.links.get_mut(&from_id) {
        let target_addr = to_actor as usize;
        linked_actors.retain(|&actor_addr| actor_addr != target_addr);
        if linked_actors.is_empty() {
            shard.links.remove(&from_id);
        }
    }
}

/// Propagate exit signal to all linked actors when an actor crashes.
///
/// This function is called from `hew_actor_trap` after the actor has
/// transitioned to a terminal state. It removes all links for the
/// crashing actor to prevent infinite propagation loops, then sends
/// EXIT messages to all linked actors.
pub(crate) fn propagate_exit_to_links(actor_id: u64, reason: i32) {
    let shard_index = get_shard_index(actor_id);

    // Take all linked actors for this actor ID to prevent re-entrancy.
    let linked_actors = {
        let mut shard = LINK_TABLE[shard_index].write().unwrap();
        shard.links.remove(&actor_id).unwrap_or_default()
    };

    // Send EXIT messages to all linked actors.
    for &linked_actor_addr in &linked_actors {
        if linked_actor_addr == 0 {
            continue;
        }

        let linked_actor = linked_actor_addr as *mut HewActor;
        // SAFETY: linked_actor was stored from a valid HewActor pointer.
        // The actor might have been freed, but we handle null mailbox gracefully.
        let linked_actor_ref = unsafe { &*linked_actor };
        let linked_id = linked_actor_ref.id;

        // Remove the reverse link: linked_actor -/-> crashing_actor
        remove_link_by_target(linked_id, actor_id);

        // Send EXIT system message with reason code.
        let mailbox = linked_actor_ref.mailbox.cast::<mailbox::HewMailbox>();
        if !mailbox.is_null() {
            // Prepare EXIT message data: { crashed_actor_id: u64, reason: i32 }
            let exit_data = ExitMessage {
                crashed_actor_id: actor_id,
                reason,
            };

            let data_ptr = (&raw const exit_data).cast::<c_void>();
            let data_size = std::mem::size_of::<ExitMessage>();

            // SAFETY: mailbox is valid for the actor's lifetime, exit_data is valid.
            unsafe {
                mailbox::hew_mailbox_send_sys(
                    mailbox,
                    SYS_MSG_EXIT,
                    data_ptr.cast_mut(),
                    data_size,
                );
            }

            // Wake the linked actor so it processes the EXIT message.
            // Without this, an idle actor would never see the system message.
            if linked_actor_ref
                .actor_state
                .compare_exchange(
                    HewActorState::Idle as i32,
                    HewActorState::Runnable as i32,
                    std::sync::atomic::Ordering::AcqRel,
                    std::sync::atomic::Ordering::Acquire,
                )
                .is_ok()
            {
                linked_actor_ref
                    .idle_count
                    .store(0, std::sync::atomic::Ordering::Relaxed);
                linked_actor_ref
                    .hibernating
                    .store(0, std::sync::atomic::Ordering::Relaxed);
                crate::scheduler::sched_enqueue(linked_actor);
            }
        }
    }
}

/// Remove all links where the target is the specified actor ID.
/// This is used to clean up reverse links when an actor exits.
fn remove_link_by_target(from_id: u64, target_id: u64) {
    let shard_index = get_shard_index(from_id);
    let mut shard = LINK_TABLE[shard_index].write().unwrap();

    if let Some(linked_actors) = shard.links.get_mut(&from_id) {
        linked_actors.retain(|&actor_addr| {
            if actor_addr == 0 {
                return false;
            }
            let actor = actor_addr as *mut HewActor;
            // SAFETY: actor was stored from a valid HewActor pointer.
            let actor_ref = unsafe { &*actor };
            actor_ref.id != target_id
        });

        if linked_actors.is_empty() {
            shard.links.remove(&from_id);
        }
    }
}

/// Message data for EXIT system messages.
#[repr(C)]
#[derive(Debug)]
struct ExitMessage {
    /// ID of the actor that crashed and caused this exit signal.
    crashed_actor_id: u64,
    /// Reason code (`error_code` from `hew_actor_trap`).
    reason: i32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU64};

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
    fn test_link_creation_and_removal() {
        let mut actor_a = create_test_actor(100);
        let mut actor_b = create_test_actor(200);

        let a_ptr = &mut actor_a as *mut HewActor;
        let b_ptr = &mut actor_b as *mut HewActor;

        // Create bidirectional link
        unsafe {
            hew_actor_link(a_ptr, b_ptr);
        }

        // Verify links exist
        let shard_a = get_shard_index(100);
        let shard_b = get_shard_index(200);

        {
            let table_a = LINK_TABLE[shard_a].read().unwrap();
            assert!(table_a
                .links
                .get(&100)
                .map_or(false, |v| v.contains(&(b_ptr as usize))));
        }
        {
            let table_b = LINK_TABLE[shard_b].read().unwrap();
            assert!(table_b
                .links
                .get(&200)
                .map_or(false, |v| v.contains(&(a_ptr as usize))));
        }

        // Remove link
        unsafe {
            hew_actor_unlink(a_ptr, b_ptr);
        }

        // Verify links are removed
        {
            let table_a = LINK_TABLE[shard_a].read().unwrap();
            assert!(!table_a
                .links
                .get(&100)
                .map_or(false, |v| v.contains(&(b_ptr as usize))));
        }
        {
            let table_b = LINK_TABLE[shard_b].read().unwrap();
            assert!(!table_b
                .links
                .get(&200)
                .map_or(false, |v| v.contains(&(a_ptr as usize))));
        }
    }

    #[test]
    fn test_null_actor_handling() {
        let mut actor = create_test_actor(300);
        let actor_ptr = &mut actor as *mut HewActor;

        // These should not panic
        unsafe {
            hew_actor_link(std::ptr::null_mut(), actor_ptr);
            hew_actor_link(actor_ptr, std::ptr::null_mut());
            hew_actor_link(std::ptr::null_mut(), std::ptr::null_mut());

            hew_actor_unlink(std::ptr::null_mut(), actor_ptr);
            hew_actor_unlink(actor_ptr, std::ptr::null_mut());
            hew_actor_unlink(std::ptr::null_mut(), std::ptr::null_mut());
        }

        // Self-linking should be ignored
        unsafe {
            hew_actor_link(actor_ptr, actor_ptr);
        }

        let shard = get_shard_index(300);
        let table = LINK_TABLE[shard].read().unwrap();
        assert!(!table.links.contains_key(&300));
    }
}
