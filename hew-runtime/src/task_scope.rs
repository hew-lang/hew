//! Owning storage for checked structured tasks and lexical scopes.

use crate::cancel_token::{
    hew_cancel_token_cancel, hew_cancel_token_new_child, hew_cancel_token_release,
    hew_cancel_token_retain, HewCancellationToken,
};
use std::ptr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

#[path = "task_scope_checked.rs"]
pub mod checked;

/// Return the current context's active task scope (null if none).
pub(crate) fn current_task_scope() -> *mut HewTaskScope {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary.
    unsafe { (*ctx).task_scope }
}

/// Set the current task scope lane, returning the previous value.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`checked::hew_checked_scope_new`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_set_current(scope: *mut HewTaskScope) -> *mut HewTaskScope {
    let ctx = crate::execution_context::require_current_context();
    if ctx.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: a non-null canonical context points to a live context slot owned
    // by the current dispatch/scope boundary. Non-null scope is caller-owned and
    // valid per this function's contract.
    unsafe {
        let previous = (*ctx).task_scope;
        (*ctx).task_scope = scope;
        (*ctx).cancel_token = if scope.is_null() {
            ptr::null_mut()
        } else {
            (*scope).cancel_token
        };
        previous
    }
}

/// Scope, source handle and scheduler invocation retain independent references.
pub struct HewTask {
    refs: AtomicUsize,
    checked: Option<Mutex<checked::CheckedTaskState>>,
    scope: *mut HewTaskScope,
    cancel_token: *mut HewCancellationToken,
    next: *mut HewTask,
}

impl std::fmt::Debug for HewTask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewTask").finish_non_exhaustive()
    }
}

unsafe fn hew_task_new() -> *mut HewTask {
    Box::into_raw(Box::new(HewTask {
        refs: AtomicUsize::new(1),
        checked: None,
        scope: ptr::null_mut(),
        cancel_token: ptr::null_mut(),
        next: ptr::null_mut(),
    }))
}

/// Release an owning task handle after its result has been consumed or drained.
/// # Safety
/// `task` is null or one live owning reference; it cannot be used afterwards.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_task_free(task: *mut HewTask) {
    if task.is_null() {
        return;
    }
    // SAFETY: caller transfers one independently retained task reference.
    if unsafe { &*task }.refs.fetch_sub(1, Ordering::AcqRel) != 1 {
        return;
    }
    // SAFETY: the final reference exclusively owns allocation and token.
    let task = unsafe { Box::from_raw(task) };
    // SAFETY: this task owns its cancellation ancestry.
    unsafe { hew_cancel_token_release(task.cancel_token) };
}

unsafe fn hew_task_scope_spawn(scope: *mut HewTaskScope, task: *mut HewTask) {
    // SAFETY: the caller exclusively owns both pointers before publication.
    unsafe {
        (*task).scope = scope;
        (*task).cancel_token = hew_cancel_token_new_child((*scope).cancel_token);
        (*task).next = (*scope).tasks;
        (*scope).tasks = task;
    }
}

unsafe fn free_scope_tasks(scope: &mut HewTaskScope) {
    let mut task = scope.tasks;
    while !task.is_null() {
        // SAFETY: the drained scope owns each linked task reference.
        unsafe {
            let next = (*task).next;
            hew_task_free(task);
            task = next;
        }
    }
}

/// Lexical scope lifetime; the checked drain owns result disposal and faults.
#[derive(Debug)]
pub struct HewTaskScope {
    tasks: *mut HewTask,
    pub(crate) cancel_token: *mut HewCancellationToken,
    checked_deadline: *mut crate::coro_sleep::HewCoroSleep,
}

impl Drop for HewTaskScope {
    fn drop(&mut self) {
        // SAFETY: a completed checked drain leaves only the scope-owned timer/token.
        unsafe {
            crate::coro_sleep::hew_coro_sleep_free(self.checked_deadline);
            hew_cancel_token_release(self.cancel_token);
        }
    }
}

/// Borrow the cancellation token until the scope's completed drain and close.
/// # Safety
/// `scope` is null or a live checked scope.
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_cancel_token(
    scope: *mut HewTaskScope,
) -> *mut HewCancellationToken {
    // SAFETY: the caller retains its optional scope for this borrow.
    unsafe { scope.as_ref() }.map_or(ptr::null_mut(), |scope| scope.cancel_token)
}
