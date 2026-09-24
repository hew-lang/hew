//! Supervisor resource cleanup and top-level crash handling.
#[allow(
    clippy::wildcard_imports,
    reason = "submodule split from a single large file; re-exports its parent scope"
)]
use super::*;

/// Free a supervisor struct without stopping actors or spin-waiting.
///
/// Used during post-shutdown cleanup when worker threads are already
/// joined. Nulls the `self_actor`'s state pointer to prevent a double-free
/// in [`crate::actor::cleanup_all_actors`], then drops the Box to free
/// child spec resources via their Drop impls.
///
/// # Safety
///
/// `sup` must be a valid, non-null pointer to a `HewSupervisor`.
/// Worker threads must have been joined before calling.
///
/// Returns `false` when a delayed-restart timer still borrows this tree. The
/// incomplete tree remains registered and its runtime/actors must stay alive
/// rather than being freed unsafely. There is no automatic retry; an embedder
/// may explicitly invoke cleanup again after the borrower drains.
pub(crate) unsafe fn free_supervisor_resources(sup: *mut HewSupervisor) -> bool {
    // ROSTER-EXCLUSIVE: runtime cleanup runs after workers join and closes
    // supervisor access before traversing or dropping roster storage.
    // SAFETY: canonical cleanup still owns a live raw supervisor allocation.
    let Some(access) = (unsafe { close_supervisor_access(sup, SUPERVISOR_PIN_DRAIN_TIMEOUT) })
    else {
        set_last_error("runtime cleanup left a pinned supervisor allocated");
        // SAFETY: canonical cleanup still owns this live top-level root.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        return false;
    };
    // Clone the Arc before cancellation without creating an exclusive borrow:
    // a timer that already reached its deadline may still hold a shared raw
    // access until cancellation acquires the control mutex.
    // SAFETY: caller guarantees sup is valid.
    let restart_timers = Arc::clone(unsafe { &(*sup).restart_timers });
    // SAFETY: the allocation remains live through the bounded drain below.
    publish_supervisor_cancellation(sup);
    let deadline = Instant::now() + SUPERVISOR_CLEANUP_TIMER_DRAIN_TIMEOUT;
    if !wait_for_pending_restart_timers(&restart_timers, deadline) {
        set_last_error("runtime cleanup retained supervisor with pending restart timers");
        // The timer thread still owns a raw borrow. Retain this root; an
        // explicit later cleanup may reclaim it after the borrow drains, while
        // a one-shot teardown leaks it fail-closed. Freeing now would be a
        // use-after-free in the delayed-restart thread.
        // SAFETY: canonical cleanup still owns the live top-level allocation.
        unsafe { crate::shutdown::hew_shutdown_register_supervisor(sup) };
        return false;
    }
    // SAFETY: every timer raw borrow drained above; canonical cleanup now has
    // exclusive access to the supervisor allocation.
    let self_actor = unsafe { (*sup).self_actor };
    if !self_actor.is_null() {
        // Null out state so cleanup_all_actors won't buf_free it
        // (state points to the supervisor Box, not a sized-block allocation).
        // SAFETY: self_actor is non-null (checked above) and valid for the supervisor's lifetime.
        unsafe {
            (*self_actor).state = ptr::null_mut();
            (*self_actor).state_size = 0;
        }
    }

    // Recursively free child supervisors. A child that cannot drain its
    // restart timer becomes an independent root before its parent Box is
    // dropped, so it never retains a dangling parent pointer.
    let mut complete = true;
    for (child_sup, _child_token, _child_spec) in take_nested_supervisor_roster(sup) {
        if !child_sup.is_null() {
            // SAFETY: child_sup is non-null (checked above), workers are joined,
            // and the parent still owns the child supervisor allocation.
            unsafe { (*child_sup).parent = ptr::null_mut() };
            // SAFETY: ownership was detached above; the child either frees now
            // or re-registers itself as an independent retained root.
            complete &= unsafe { free_supervisor_resources(child_sup) };
        }
    }
    // Drop the Box — child spec Drop impls free names + init_state.
    // SAFETY: sup was allocated with Box::into_raw and is valid per caller contract.
    drop(unsafe { Box::from_raw(sup) }); // ALLOCATOR-PAIRING: GlobalAlloc
    finish_supervisor_reclamation(&access);
    complete
}

/// Handle a crashed child actor by applying the supervisor's restart strategy.
///
/// This is a convenience entry point that can be called directly (e.g. from
/// `hew_actor_trap`) instead of going through the system-message path.
///
/// # Safety
///
/// - `sup` must be a valid pointer returned by [`hew_supervisor_new`].
/// - `child` must be a valid pointer to a [`HewActor`] that belongs to `sup`.
#[no_mangle]
pub unsafe extern "C" fn hew_supervisor_handle_crash(
    sup: *mut HewSupervisor,
    child: *mut HewActor,
) {
    cabi_guard!(sup.is_null() || child.is_null());
    let (child_index, child_id, exit_state, crash_code) = {
        // SAFETY: caller keeps `sup` live; the guard prevents remove/restart
        // from reclaiming `child` while its event scalars are copied out.
        let guard = unsafe { &(*sup).roster }.lock_or_recover();
        // SAFETY: roster access is serialized by the guard.
        let s = &*guard;
        // SAFETY: the caller contract guarantees a live child for this call.
        let crashed =
            unsafe { &*child }.actor_state.load(Ordering::Acquire) == HewActorState::Crashed as i32;
        let Some(index) = s.children.iter().position(|candidate| *candidate == child) else {
            // This supervisor does not own the child, so it cannot rule on the
            // crash and no other authority will be told about it.
            if crashed {
                crate::exit_status::record_unrecovered_actor_fault();
            }
            return;
        };
        let Ok(child_index) = u32::try_from(index) else {
            if crashed {
                crate::exit_status::record_unrecovered_actor_fault();
            }
            return;
        };
        // SAFETY: the matching roster slot owns this live child for the
        // duration of the critical section.
        let child_ref = unsafe { &*child };
        (
            child_index,
            child_ref.id,
            child_ref.actor_state.load(Ordering::Acquire),
            child_ref.error_code.load(Ordering::Acquire),
        )
    };

    // Open the crash's record before the notification is queued, exactly as the
    // trap path does. This entry is an alternate route to the SAME event, so it
    // must be an equal citizen of the exit-status authority: a crash reported
    // here without a record would be a fault the process exits `0` over.
    let record = if exit_state == HewActorState::Crashed as c_int {
        let record = crate::exit_status::open_supervised_fault();
        // Attribute it exactly as the trap path does, so a restart barrier on
        // this role - or on any ancestor role - sees the fault as pending.
        // SAFETY: the caller keeps `sup` live through this notification.
        crate::exit_status::attribute_supervised_fault(record, unsafe {
            child_role_chain(sup, child_index)
        });
        record
    } else {
        FaultRecord::NONE
    };

    // Notify the supervisor actor via the event system.
    // SAFETY: sup is valid and child_id / exit_state are read from valid memory.
    let notified = unsafe {
        hew_supervisor_notify_child_actor_event(
            sup,
            child_index,
            child_id,
            exit_state,
            crash_code,
            record.as_raw(),
        )
    };
    if !notified {
        // The event reached no supervisor actor, so the record reached no
        // authority. Settle it here rather than leave it to time out.
        crate::exit_status::settle_supervised_fault(record, FaultRuling::Unrecovered);
    }
}
