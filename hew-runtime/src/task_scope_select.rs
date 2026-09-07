//! Borrowed task observation for source selection. The winning branch performs
//! the ordinary checked await; observing readiness never consumes a result.

use super::{
    checked, hew_checked_task_wait_new, retain, HewCheckedTaskWait, HewTask, PENDING, TAKEN,
};
use crate::coro_sleep::{
    hew_coro_sleep_free, hew_coro_sleep_new, hew_coro_sleep_status, HewCoroSleep,
};
use crate::coro_state::CoroStatus;
use crate::util::MutexExt;
use crate::wake::HewWaker;
use std::ptr;

/// Independently retained observations of the source handles and optional timer.
#[derive(Debug)]
pub struct HewCheckedTaskSelect {
    tasks: Vec<HewCheckedTaskWait>,
    timer: *mut HewCoroSleep,
}

impl Drop for HewCheckedTaskSelect {
    fn drop(&mut self) {
        // SAFETY: the selection uniquely owns this timer and all observations.
        unsafe { hew_coro_sleep_free(self.timer) };
    }
}

/// Observe checked task handles without transferring their source ownership.
/// The timer starts after every task observation has been registered.
///
/// # Safety
/// `tasks` points to `count` live checked handles, or is null when count is zero.
/// Each source handle stays live for this call. The retained waker descriptor
/// obeys its contract. Free the returned selection exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_new(
    tasks: *const *mut HewTask,
    count: usize,
    has_timeout: i32,
    duration_ns: i64,
    waker: *const HewWaker,
) -> *mut HewCheckedTaskSelect {
    let mut observations = Vec::with_capacity(count);
    for index in 0..count {
        // SAFETY: the source array is live; each observation acquires its own
        // task reference before passing that reference to the wait constructor.
        unsafe {
            let task = *tasks.add(index);
            retain(task);
            observations.push(*Box::from_raw(hew_checked_task_wait_new(task, waker)));
        }
    }
    let timer = if has_timeout != 0 {
        // SAFETY: caller provides a retained-target descriptor.
        unsafe { hew_coro_sleep_new(duration_ns, waker) }
    } else {
        ptr::null_mut()
    };
    Box::into_raw(Box::new(HewCheckedTaskSelect {
        tasks: observations,
        timer,
    }))
}

/// Return a ready task's source index, or `count` when the timer wins.
/// Pending is -1; an invalid consumed source or failed timer is -2.
/// When several tasks are ready, source order breaks the tie.
///
/// # Safety
/// `selection` is a live observation owned by the caller. Its tasks have no
/// concurrent consuming observer; checked source ownership establishes this.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_poll(
    selection: *const HewCheckedTaskSelect,
) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    unsafe { poll(selection, false) }
}

/// Select the earliest completion, even when several children are already ready.
/// Source order breaks an equal completion-order tie.
///
/// # Safety
/// The same retained observation contract as `hew_checked_task_select_poll`.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_poll_first(
    selection: *const HewCheckedTaskSelect,
) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    unsafe { poll(selection, true) }
}

unsafe fn poll(selection: *const HewCheckedTaskSelect, first_completion: bool) -> i64 {
    // SAFETY: caller retains the observation throughout this poll.
    let selection = unsafe { &*selection };
    let mut first = None;
    for (index, wait) in selection.tasks.iter().enumerate() {
        // SAFETY: each wait independently retains the checked task storage.
        let state = unsafe { checked(wait.task) }.lock_or_recover();
        let status = state.outcome();
        if status == TAKEN {
            return -2;
        }
        if status != PENDING {
            if !first_completion {
                return i64::try_from(index).unwrap_or(-2);
            }
            let candidate = (state.order, index);
            first = Some(first.map_or(candidate, |previous| std::cmp::min(previous, candidate)));
        }
    }
    if let Some((_, index)) = first {
        return i64::try_from(index).unwrap_or(-2);
    }
    if !selection.timer.is_null() {
        // SAFETY: the selection owns its timer until detached.
        let status = unsafe { hew_coro_sleep_status(selection.timer) };
        if status == CoroStatus::Complete as i32 {
            return i64::try_from(selection.tasks.len()).unwrap_or(-2);
        }
        if status != CoroStatus::Pending as i32 {
            return -2;
        }
    }
    -1
}

/// Detach every observation without consuming or cancelling a source task.
/// Late notifications retain their own readiness target, never frame storage.
///
/// # Safety
/// `selection` is null or the uniquely owned live handle, with no active poll.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_select_free(selection: *mut HewCheckedTaskSelect) {
    if !selection.is_null() {
        // SAFETY: caller transfers the unique selection allocation.
        drop(unsafe { Box::from_raw(selection) });
    }
}
