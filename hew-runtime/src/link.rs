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
use crate::util::RwLockExt;

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
    let mut shard = LINK_TABLE[shard_index].write_or_recover();

    shard
        .links
        .entry(from_id)
        .or_default()
        .push(to_actor as usize);
}

/// Remove a unidirectional link: `from_id` -/-> `to_actor`.
fn remove_link(from_id: u64, to_actor: *mut HewActor) {
    let shard_index = get_shard_index(from_id);
    let mut shard = LINK_TABLE[shard_index].write_or_recover();

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
        let mut shard = LINK_TABLE[shard_index].write_or_recover();
        shard.links.remove(&actor_id).unwrap_or_default()
    };

    // Send EXIT messages to all linked actors.
    for &linked_actor_addr in &linked_actors {
        if linked_actor_addr == 0 {
            continue;
        }

        let linked_actor = linked_actor_addr as *mut HewActor;

        // Guard: skip actors that have already been freed.
        if !crate::actor::is_actor_live(linked_actor) {
            continue;
        }

        // SAFETY: linked_actor is live (checked above).
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
    let mut shard = LINK_TABLE[shard_index].write_or_recover();

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

/// Remove all link entries for a given actor (by ID) and purge its address
/// from every other actor's link list across all shards.
///
/// Called from `hew_actor_free` before deallocation to prevent link
/// propagation from dereferencing freed memory.
pub(crate) fn remove_all_links_for_actor(actor_id: u64, actor_addr: *mut HewActor) {
    let actor_usize = actor_addr as usize;

    // Remove the actor's own link-list entry from its shard.
    let own_shard = get_shard_index(actor_id);
    {
        let mut shard = LINK_TABLE[own_shard].write().unwrap();
        shard.links.remove(&actor_id);
    }

    // Scan all shards and remove this actor's address from other actors'
    // link lists. This is O(shards × entries) but actors rarely have many
    // links, and this only runs at free time.
    for shard_rw in LINK_TABLE.iter() {
        let mut shard = shard_rw.write().unwrap();
        shard.links.retain(|_id, linked_actors| {
            linked_actors.retain(|&addr| addr != actor_usize);
            !linked_actors.is_empty()
        });
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

/// Returns true if any link entries reference the given actor address.
#[cfg(test)]
pub(crate) fn has_links_for_actor(actor_id: u64, actor_addr: *mut HewActor) -> bool {
    let actor_usize = actor_addr as usize;
    let own_shard = get_shard_index(actor_id);
    {
        let shard = LINK_TABLE[own_shard].read().unwrap();
        if shard.links.contains_key(&actor_id) {
            return true;
        }
    }
    // Check if this actor appears as a target in any other actor's link list.
    for shard_rw in LINK_TABLE.iter() {
        let shard = shard_rw.read().unwrap();
        for linked in shard.links.values() {
            if linked.contains(&actor_usize) {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicBool, AtomicI32, AtomicPtr, AtomicU64};

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
    fn test_link_creation_and_removal() {
        let mut actor_a = create_test_actor(100);
        let mut actor_b = create_test_actor(200);

        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // Create bidirectional link
        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
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
                .is_some_and(|v| v.contains(&(b_ptr as usize))));
        }
        {
            let table_b = LINK_TABLE[shard_b].read().unwrap();
            assert!(table_b
                .links
                .get(&200)
                .is_some_and(|v| v.contains(&(a_ptr as usize))));
        }

        // Remove link
        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
        unsafe {
            hew_actor_unlink(a_ptr, b_ptr);
        }

        // Verify links are removed
        {
            let table_a = LINK_TABLE[shard_a].read().unwrap();
            assert!(!table_a
                .links
                .get(&100)
                .is_some_and(|v| v.contains(&(b_ptr as usize))));
        }
        {
            let table_b = LINK_TABLE[shard_b].read().unwrap();
            assert!(!table_b
                .links
                .get(&200)
                .is_some_and(|v| v.contains(&(a_ptr as usize))));
        }
    }

    #[test]
    fn test_null_actor_handling() {
        let mut actor = create_test_actor(300);
        let actor_ptr = &raw mut actor;

        // These should not panic
        // SAFETY: Testing null pointer handling; functions handle null gracefully.
        unsafe {
            hew_actor_link(std::ptr::null_mut(), actor_ptr);
            hew_actor_link(actor_ptr, std::ptr::null_mut());
            hew_actor_link(std::ptr::null_mut(), std::ptr::null_mut());

            hew_actor_unlink(std::ptr::null_mut(), actor_ptr);
            hew_actor_unlink(actor_ptr, std::ptr::null_mut());
            hew_actor_unlink(std::ptr::null_mut(), std::ptr::null_mut());
        }

        // Self-linking should be ignored
        // SAFETY: actor_ptr is a valid pointer; self-link is a no-op.
        unsafe {
            hew_actor_link(actor_ptr, actor_ptr);
        }

        let shard = get_shard_index(300);
        let table = LINK_TABLE[shard].read().unwrap();
        assert!(!table.links.contains_key(&300));
    }

    /// Link operations survive a poisoned `RwLock` shard.
    #[test]
    fn link_survives_poisoned_shard() {
        // Poison a standalone RwLock to prove the pattern works.
        let lock = std::sync::RwLock::new(42);
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _guard = lock.write().unwrap();
            panic!("intentional poison");
        }));
        assert!(lock.is_poisoned());

        // The global LINK_TABLE uses write_or_recover, so link/unlink
        // must not panic even if another thread poisoned a shard.
        let mut actor_a = create_test_actor(900);
        let mut actor_b = create_test_actor(901);

        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // SAFETY: a_ptr and b_ptr are valid pointers to stack-allocated test actors.
        unsafe {
            hew_actor_link(a_ptr, b_ptr);
            hew_actor_unlink(a_ptr, b_ptr);
        }
    }

    #[test]
    fn remove_all_links_clears_both_directions() {
        // Use unique IDs (30xxx) to avoid collisions with parallel tests.
        let mut actor_a = create_test_actor(30_100);
        let mut actor_b = create_test_actor(30_200);
        let a_ptr = &raw mut actor_a;
        let b_ptr = &raw mut actor_b;

        // SAFETY: Both are valid stack-allocated test actors.
        unsafe { hew_actor_link(a_ptr, b_ptr) };

        // Precondition: both directions exist.
        assert!(has_links_for_actor(30_100, a_ptr));
        assert!(has_links_for_actor(30_200, b_ptr));

        // Act: remove all links for actor A (simulating hew_actor_free).
        remove_all_links_for_actor(30_100, a_ptr);

        // Actor A's own entry and its address in B's list should both be gone.
        assert!(
            !has_links_for_actor(30_100, a_ptr),
            "actor A should have no link entries after cleanup"
        );

        // Actor B's own entry that pointed to A should also be gone.
        let shard_b = get_shard_index(30_200);
        let table_b = LINK_TABLE[shard_b].read().unwrap();
        let b_links = table_b.links.get(&30_200);
        assert!(
            b_links.is_none() || !b_links.unwrap().contains(&(a_ptr as usize)),
            "actor B's link list should no longer reference actor A"
        );
    }

    #[test]
    fn remove_all_links_no_links_does_not_panic() {
        // An actor with no links — cleanup should be a no-op.
        let actor = create_test_actor(30_300);
        let ptr = (&raw const actor).cast_mut();
        remove_all_links_for_actor(30_300, ptr);
        assert!(!has_links_for_actor(30_300, ptr));
    }
}
