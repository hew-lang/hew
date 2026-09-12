//! Checked structured tasks on the existing native task-thread runtime.
//!
//! The scope, source handle and worker own independent task references. A
//! terminal result is published only after the callable driver has destroyed
//! its frame; task observers retain readiness targets independently of frames.

use super::{
    free_scope_tasks, hew_cancel_token_cancel, hew_cancel_token_new_child,
    hew_task_complete_threaded, hew_task_free, hew_task_new, hew_task_scope_spawn,
    hew_task_spawn_thread, HewCancellationToken, HewTask, HewTaskScope,
};
use crate::callable::{hew_callable_drop, HewCallableValue};
use crate::coro_root::hew_coro_run_callable;
use crate::coro_state::{hew_coro_state_free, hew_coro_state_new, CoroStatus, HewCoroState};
use crate::fault::{hew_fault_combine, hew_fault_drop, HewFault, HEW_FAULT_CANCELLED};
use crate::util::MutexExt;
use crate::value_close::{
    hew_value_close_collect, hew_value_close_finish, hew_value_close_poll, HewValueClose,
};
use crate::wake::{HewWaker, OwnedWaker};
use hew_cabi::value::HewValueLayout;
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Weak;
use std::sync::{Arc, Mutex};

#[path = "task_scope_select.rs"]
mod select;
pub use select::{
    hew_checked_task_select_add_actor, hew_checked_task_select_add_channel,
    hew_checked_task_select_add_task, hew_checked_task_select_arm_timer,
    hew_checked_task_select_fault, hew_checked_task_select_free, hew_checked_task_select_new,
    hew_checked_task_select_poll, hew_checked_task_select_poll_first,
    hew_checked_task_select_set_context, HewCheckedTaskSelect,
};

const PENDING: i32 = 0;
const READY: i32 = 1;
const FAULT: i32 = 2;
const CANCELLED: i32 = 3;
const TAKEN: i32 = 4;
static COMPLETION_ORDER: AtomicU64 = AtomicU64::new(1);

pub(super) struct CheckedTaskState {
    callable: Option<HewCallableValue>,
    layout: *const HewValueLayout,
    allocation: Option<Layout>,
    result: *mut c_void,
    initialized: bool,
    completed: bool,
    taken: bool,
    status: i32,
    fault: *mut HewFault,
    order: u64,
    waiters: Vec<Weak<TaskWake>>,
}

// SAFETY: the compiler establishes Send for captures and the result. A mutex
// protects publication and consumption; immutable generated layouts outlive tasks.
unsafe impl Send for CheckedTaskState {}

impl CheckedTaskState {
    fn outcome(&self) -> i32 {
        if !self.completed {
            PENDING
        } else if self.taken {
            TAKEN
        } else if self.status == 0 {
            READY
        } else if self.status == HEW_FAULT_CANCELLED {
            CANCELLED
        } else {
            FAULT
        }
    }
}

impl Drop for CheckedTaskState {
    fn drop(&mut self) {
        // SAFETY: the final task owner has exclusive access; each remaining
        // capture, result and fault represents one untransferred obligation.
        unsafe {
            if let Some(mut callable) = self.callable.take() {
                hew_callable_drop(&raw mut callable);
            }
            if self.initialized {
                if let Some(drop_fn) = (*self.layout).drop_fn {
                    drop_fn(self.result);
                }
            }
            if let Some(allocation) = self.allocation {
                dealloc(self.result.cast(), allocation);
            }
            hew_fault_drop(self.fault);
        }
    }
}

#[derive(Debug)]
struct TaskWake {
    waker: OwnedWaker,
    armed: AtomicBool,
}

/// One disarmable observation, retaining a source task handle until released.
#[derive(Debug)]
pub struct HewCheckedTaskWait {
    task: *mut HewTask,
    wake: Arc<TaskWake>,
}

impl Drop for HewCheckedTaskWait {
    fn drop(&mut self) {
        self.wake.armed.store(false, Ordering::Release);
        // SAFETY: this operation owns one task reference independently of scope.
        unsafe { hew_task_free(self.task) };
    }
}

/// One scope drain retains every child through cooperative result disposal.
#[derive(Debug)]
pub struct HewCheckedScopeWait {
    scope: *mut HewTaskScope,
    tasks: Vec<HewCheckedTaskWait>,
    cancellation: ScopeCancellation,
    close: Mutex<ScopeResultClose>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeCancellation {
    Ordinary,
    RaceLosers,
}

#[derive(Debug)]
struct ScopeResultClose {
    state: *mut HewCoroState,
    collector: *mut HewValueClose,
    collected: bool,
    complete: bool,
    fault: *mut HewFault,
}

impl Drop for ScopeResultClose {
    fn drop(&mut self) {
        // An active collector borrows task result storage. Its owners cannot
        // be released until the retained readiness target reports quiescence.
        if self.collected && !self.complete {
            std::process::abort();
        }
        // SAFETY: no collector borrows remain; this drain owns state and fault.
        unsafe {
            hew_coro_state_free(self.state);
            hew_fault_drop(self.fault);
        }
    }
}

unsafe fn retain(task: *mut HewTask) {
    // SAFETY: caller retains a live reference throughout this increment.
    let previous = unsafe { &*task }.refs.fetch_add(1, Ordering::Relaxed);
    if previous > isize::MAX as usize {
        std::process::abort();
    }
}

unsafe fn checked<'a>(task: *mut HewTask) -> &'a Mutex<CheckedTaskState> {
    // SAFETY: all callers bound this internal borrow by an owning task reference.
    unsafe { &*task }
        .checked
        .as_ref()
        .expect("checked task contract")
}

unsafe extern "C" fn run(task: *mut HewTask) {
    // SAFETY: the spawn path owns a worker reference until the final statement.
    unsafe {
        let (callable, output) = {
            let mut state = checked(task).lock_or_recover();
            (
                state.callable.take().expect("one task invocation"),
                state.result,
            )
        };
        let context = crate::execution_context::current_context();
        // Use the task's child token, preserving cancellation of one child.
        (*context).cancel_token = (*task).cancel_token;
        let mut fault = ptr::null_mut();
        let status = hew_coro_run_callable(
            (*callable.descriptor).invoke_once,
            callable.environment,
            ptr::null(),
            output,
            &raw mut fault,
        );
        let waiters = {
            let mut state = checked(task).lock_or_recover();
            if status == 0 && state.layout.is_null() {
                // A callable returning ! cannot publish a successful result.
                std::process::abort();
            }
            state.initialized = status == 0;
            state.fault = fault.cast();
            state.status = status;
            state.completed = true;
            state.order = COMPLETION_ORDER.fetch_add(1, Ordering::Relaxed);
            std::mem::take(&mut state.waiters)
        };
        hew_task_complete_threaded(task);
        for waiter in waiters.into_iter().filter_map(|waiter| waiter.upgrade()) {
            if waiter.armed.load(Ordering::Acquire) {
                waiter.waker.wake();
            }
        }
        hew_task_free(task);
    }
}

/// Create a scope with explicit cancellation ancestry.
///
/// # Safety
/// `parent` is null or a live token. Close this scope only after a completed
/// checked drain; never use legacy forced-cancellation APIs for checked tasks.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_new(
    parent: *mut HewCancellationToken,
) -> *mut HewTaskScope {
    // SAFETY: constructor takes a reference to the caller's live parent token.
    let cancel_token = unsafe { hew_cancel_token_new_child(parent) };
    Box::into_raw(Box::new(HewTaskScope {
        tasks: ptr::null_mut(),
        task_count: 0,
        completed_count: 0,
        cancelled: AtomicBool::new(false),
        cancel_token,
        deadlines: ptr::null_mut(),
        checked_deadline: ptr::null_mut(),
        parent: ptr::null_mut(),
    }))
}

unsafe extern "C" fn deadline_wake(token: *mut c_void) {
    // SAFETY: the timer retains this token independently of the scope frame.
    unsafe { hew_cancel_token_cancel(token.cast(), crate::fault::HEW_FAULT_DEADLINE) };
}

unsafe extern "C" fn deadline_retain(token: *mut c_void) {
    // SAFETY: retaining a live token is thread-safe.
    unsafe { super::hew_cancel_token_retain(token.cast()) };
}

unsafe extern "C" fn deadline_release(token: *mut c_void) {
    // SAFETY: callback ownership transfers exactly one retained reference.
    unsafe { super::hew_cancel_token_release(token.cast()) };
}

/// Arm this lexical scope's deadline on the shared timer wheel.
///
/// # Safety
/// Scope is live, exclusively accessed and has no existing checked deadline.
/// Its close path must drain all child frames before releasing the scope.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_deadline(scope: *mut HewTaskScope, duration_ns: i64) {
    // SAFETY: scope owns its token and receives sole ownership of the timer.
    unsafe {
        let scope = &mut *scope;
        if !scope.checked_deadline.is_null() {
            std::process::abort();
        }
        let waker = HewWaker {
            context: scope.cancel_token.cast(),
            wake: deadline_wake,
            retain: deadline_retain,
            release: deadline_release,
        };
        scope.checked_deadline =
            crate::coro_sleep::hew_coro_sleep_new(duration_ns, &raw const waker);
        if crate::coro_sleep::hew_coro_sleep_status(scope.checked_deadline)
            != crate::coro_state::CoroStatus::Pending as i32
        {
            // An already expired deadline or unavailable timer must prevent
            // the body from running without its promised cancellation bound.
            deadline_wake(waker.context);
        }
    }
}

/// Transfer a checked nullary once callable into a scope and return one handle.
///
/// # Safety
/// Scope is live and exclusively accessed. Callable and layout have the exact
/// checked Send input/result contract; the descriptor code outlives the scope.
/// A null layout declares an uninhabited result: the callable must never succeed.
/// This consumes and clears `callable` before starting its worker.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_spawn(
    scope: *mut HewTaskScope,
    callable: *mut HewCallableValue,
    layout: *const HewValueLayout,
) -> *mut HewTask {
    // SAFETY: the compiler supplies an immutable layout, or null for !.
    let allocation = unsafe { layout.as_ref() }.map(|layout| {
        Layout::from_size_align(layout.size.max(1), layout.align)
            .unwrap_or_else(|_| std::process::abort())
    });
    let result = allocation.map_or(ptr::null_mut(), |allocation| {
        // SAFETY: the valid nonzero layout is released by the final task owner.
        let result = unsafe { alloc(allocation) };
        if result.is_null() {
            handle_alloc_error(allocation);
        }
        result
    });
    // SAFETY: caller transfers the owning carrier and exclusively borrows scope.
    unsafe {
        let owner = ptr::read(callable);
        (*callable).environment = ptr::null_mut();
        (*callable).descriptor = ptr::null();
        let task = hew_task_new();
        (*task).checked = Some(Mutex::new(CheckedTaskState {
            callable: Some(owner),
            layout,
            allocation,
            result: result.cast(),
            initialized: false,
            completed: false,
            taken: false,
            status: 0,
            fault: ptr::null_mut(),
            order: 0,
            waiters: Vec::new(),
        }));
        hew_task_scope_spawn(scope, task); // original reference becomes scope-owned
        retain(task); // source handle
        retain(task); // worker
        hew_task_spawn_thread(task, run);
        task
    }
}

/// Observe completion, consuming one source task reference into the wait.
///
/// # Safety
/// `task` is one owning checked task handle. `waker` is a valid retained-target
/// descriptor. Free the wait after taking its result or abandoning observation.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_new(
    task: *mut HewTask,
    waker: *const HewWaker,
) -> *mut HewCheckedTaskWait {
    // SAFETY: caller retains descriptor until registration returns.
    let wake = Arc::new(TaskWake {
        // SAFETY: caller retains the descriptor throughout registration.
        waker: unsafe { OwnedWaker::retain(&*waker) },
        armed: AtomicBool::new(true),
    });
    let complete = {
        // SAFETY: this operation has taken ownership of the task reference.
        let mut state = unsafe { checked(task) }.lock_or_recover();
        if state.completed {
            true
        } else {
            state.waiters.retain(|waiter| waiter.strong_count() != 0);
            state.waiters.push(Arc::downgrade(&wake));
            false
        }
    };
    if complete {
        wake.waker.wake();
    }
    Box::into_raw(Box::new(HewCheckedTaskWait { task, wake }))
}

/// # Safety
/// Wait remains live for the poll. Status: pending 0, value 1, fault 2,
/// cancellation 3, or already consumed 4.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_status(wait: *const HewCheckedTaskWait) -> i32 {
    // SAFETY: the live wait retains its task.
    unsafe { checked((*wait).task) }.lock_or_recover().outcome()
}

/// # Safety
/// Wait retains a live child. Cancellation still requires a completed drain.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_cancel(wait: *const HewCheckedTaskWait) {
    // SAFETY: the wait retains its task and the task retains its token.
    unsafe { hew_cancel_token_cancel((*(*wait).task).cancel_token, 1) };
}

/// # Safety
/// Wait remains live and its task has completed.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_private_status(
    wait: *const HewCheckedTaskWait,
) -> i32 {
    // SAFETY: the wait retains its task while the outcome is read.
    unsafe { checked((*wait).task) }.lock_or_recover().status
}

/// Transfer a completed result or fault, leaving outputs untouched if pending.
///
/// # Safety
/// Wait is live; output has its exact checked result layout and fault is a
/// writable empty slot. Only one consumer may take this task's output.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_take(
    wait: *const HewCheckedTaskWait,
    output: *mut c_void,
    fault: *mut *mut HewFault,
) -> i32 {
    // SAFETY: the wait retains task storage until this operation returns.
    let mut state = unsafe { checked((*wait).task) }.lock_or_recover();
    let outcome = state.outcome();
    if outcome == PENDING || outcome == TAKEN {
        return outcome;
    }
    // SAFETY: completed value/fault has one owner and caller supplies exact slots.
    unsafe {
        if state.initialized {
            let size = (*state.layout).size;
            if size != 0 {
                ptr::copy_nonoverlapping(state.result.cast::<u8>(), output.cast(), size);
            }
            state.initialized = false;
        } else {
            fault.write(std::mem::replace(&mut state.fault, ptr::null_mut()));
        }
    }
    state.taken = true;
    outcome
}

/// # Safety
/// Free one owning wait after no execution can access it. This disarms its
/// registration without cancelling the independently scope-owned child.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_task_wait_free(wait: *mut HewCheckedTaskWait) {
    if !wait.is_null() {
        // SAFETY: caller transfers the unique wait allocation.
        drop(unsafe { Box::from_raw(wait) });
    }
}

/// # Safety
/// Scope is live and has no further parent-side spawns during this drain.
/// Waker obeys the retained readiness contract. Scope outlives the returned wait.
/// Only one drain may own the scope's result cleanup at a time.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_new(
    scope: *mut HewTaskScope,
    waker: *const HewWaker,
) -> *mut HewCheckedScopeWait {
    let mut tasks = Vec::new();
    // SAFETY: the exclusively accessed scope retains every listed task.
    unsafe {
        let mut task = (*scope).tasks;
        while !task.is_null() {
            retain(task);
            tasks.push(*Box::from_raw(hew_checked_task_wait_new(task, waker)));
            task = (*task).next;
        }
    }
    // SAFETY: scope retains the cancellation ancestry and registration retains
    // the caller's readiness target independently of its suspended frame.
    let state = unsafe { hew_coro_state_new(waker, (*scope).cancel_token) };
    Box::into_raw(Box::new(HewCheckedScopeWait {
        scope,
        tasks,
        cancellation: ScopeCancellation::Ordinary,
        close: Mutex::new(ScopeResultClose {
            state,
            collector: ptr::null_mut(),
            collected: false,
            complete: false,
            fault: ptr::null_mut(),
        }),
    }))
}

/// Request cooperative cancellation; user code and operation producers must
/// still complete before a scope drain can report ready.
///
/// # Safety
/// Scope remains live throughout this call.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_cancel(scope: *mut HewTaskScope) {
    // SAFETY: scope owns the cancellation token and tasks retain its ancestry.
    unsafe {
        let token = (*scope).cancel_token;
        let reason = super::cancel_token_reason(token);
        hew_cancel_token_cancel(token, if reason == 0 { 1 } else { reason });
    }
}

/// Request cancellation after the selected race child has completed. The drain
/// removes only its own cancellation marker, preserving every actual failure.
///
/// # Safety
/// The caller exclusively owns this live drain; all children belong to the race.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_cancel_losers(wait: *mut HewCheckedScopeWait) {
    // SAFETY: exclusive access precedes polling and the drain retains its scope.
    unsafe {
        let wait = &mut *wait;
        wait.cancellation = ScopeCancellation::RaceLosers;
        let token = (*wait.scope).cancel_token;
        let reason = super::cancel_token_reason(token);
        let reason = if reason == 0 {
            crate::fault::HEW_FAULT_RACE_LOST
        } else {
            reason
        };
        for task in &wait.tasks {
            // Completed winners can own values that survive this scope. Their
            // cancellation ancestry must not be changed by losing siblings.
            let pending = !checked(task.task).lock_or_recover().completed;
            if pending {
                hew_cancel_token_cancel((*task.task).cancel_token, reason);
            }
        }
    }
}

/// # Safety
/// Wait and scope remain live. Returns 1 only after all child frames have
/// completed cleanup and abandoned results have closed. An observed child fault
/// requests cancellation of siblings. Calls on one wait must be serialized.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_status(wait: *const HewCheckedScopeWait) -> i32 {
    // SAFETY: caller retains the scope drain and its task references.
    let wait = unsafe { &*wait };
    let mut pending = false;
    let mut failed = false;
    for task in &wait.tasks {
        // SAFETY: each wait retains the corresponding task.
        let state = unsafe { checked(task.task) }.lock_or_recover();
        pending |= !state.completed;
        failed |= state.completed && !state.taken && state.status != 0;
    }
    if failed && wait.cancellation == ScopeCancellation::Ordinary {
        // SAFETY: wait borrows its still-live scope until released.
        unsafe { hew_checked_scope_cancel(wait.scope) };
    }
    if pending {
        return PENDING;
    }
    let mut close = wait.close.lock_or_recover();
    if close.complete {
        return READY;
    }
    // SAFETY: all child invocations have stopped. The drain retains every task
    // allocation and claims initialized results before borrowing their children.
    unsafe {
        if !close.collected {
            for task in &wait.tasks {
                let mut state = checked(task.task).lock_or_recover();
                if state.initialized && !state.taken {
                    state.taken = true;
                    hew_value_close_collect(
                        state.result,
                        state.layout,
                        (&raw mut close.collector).cast(),
                    );
                }
            }
            close.collected = true;
        }
        if hew_value_close_poll(close.collector, close.state.cast()) == CoroStatus::Pending as i32 {
            return PENDING;
        }
        let collector = std::mem::replace(&mut close.collector, ptr::null_mut());
        hew_value_close_finish(collector, &raw mut close.fault);
        for task in &wait.tasks {
            let release = {
                let mut state = checked(task.task).lock_or_recover();
                if std::mem::take(&mut state.initialized) {
                    (*state.layout)
                        .drop_fn
                        .map(|drop_fn| (drop_fn, state.result))
                } else {
                    None
                }
            };
            if let Some((drop_fn, result)) = release {
                drop_fn(result);
            }
        }
    }
    close.complete = true;
    READY
}

/// Transfer unobserved child faults in completion order after a complete drain.
/// Result cleanup faults follow child faults without replacing their primary.
///
/// # Safety
/// Wait remains live and fault is an empty, writable owning slot. A pending
/// drain leaves both the slot and every child fault untouched.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_take_fault(
    wait: *const HewCheckedScopeWait,
    fault: *mut *mut HewFault,
) -> i32 {
    // SAFETY: caller retains this drain and its task references.
    let wait = unsafe { &*wait };
    // Completion is monotonic. Check every child before transferring any owner,
    // so a pending child cannot discard faults already removed from siblings.
    // SAFETY: this poll preserves fault ownership until result cleanup completes.
    if unsafe { hew_checked_scope_wait_status(wait) } == PENDING {
        return PENDING;
    }
    let mut failures = Vec::new();
    for task in &wait.tasks {
        // SAFETY: the drain retains the task.
        let mut state = unsafe { checked(task.task) }.lock_or_recover();
        if !state.taken && state.status != 0 {
            state.taken = true;
            let mut owner = std::mem::replace(&mut state.fault, ptr::null_mut());
            if wait.cancellation == ScopeCancellation::RaceLosers {
                // SAFETY: this completed child transfers its optional fault owner.
                owner = unsafe { crate::fault::finish_race_loser(owner) };
            }
            // The cancellation marker may have been removed or a secondary
            // cleanup fault promoted; the remaining fault owns its status.
            // SAFETY: owner is null or the uniquely transferred live fault.
            if let Some(fault) = unsafe { owner.as_ref() } {
                failures.push((state.order, fault.code(), owner));
            }
        }
    }
    failures.sort_by_key(|failure| failure.0);
    let mut status = 0;
    let mut primary = ptr::null_mut();
    for (_, code, owner) in failures {
        if status == 0 {
            status = code;
        }
        // SAFETY: each child transfers a distinct owned fault at most once.
        primary = unsafe { hew_fault_combine(primary, owner) };
    }
    let cleanup = std::mem::replace(&mut wait.close.lock_or_recover().fault, ptr::null_mut());
    // SAFETY: the completed collector transfers one optional cleanup fault.
    unsafe {
        if status == 0 {
            status = cleanup.as_ref().map_or(0, HewFault::code);
        }
        primary = hew_fault_combine(primary, cleanup);
    }
    // SAFETY: caller supplies an empty writable fault slot.
    unsafe { fault.write(primary) };
    status
}

/// # Safety
/// Caller transfers the unique drain handle; no poll may access it afterwards.
/// Started result cleanup must have completed and its fault must be taken.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_wait_free(wait: *mut HewCheckedScopeWait) {
    if !wait.is_null() {
        // SAFETY: the caller transfers the unique allocation.
        drop(unsafe { Box::from_raw(wait) });
    }
}

/// Release the scope's task references after all user frames have stopped.
/// Worker epilogues retain their own references and never access parent data.
///
/// # Safety
/// Scope has completed a checked drain, has no active waits or deadline nodes,
/// and is consumed by this call. Source handles cannot escape their scope.
#[no_mangle]
pub unsafe extern "C" fn hew_checked_scope_close(scope: *mut HewTaskScope) {
    // SAFETY: the caller transfers a drained scope allocation.
    let mut scope = unsafe { Box::from_raw(scope) };
    if !scope.deadlines.is_null() {
        std::process::abort();
    }
    // The checked drain has closed and disposed every abandoned result before
    // enclosing source resources can be released.
    // SAFETY: scope retains every task and the completed frames no longer use results.
    unsafe {
        let mut task = scope.tasks;
        while !task.is_null() {
            let state = checked(task).lock_or_recover();
            if !state.completed || state.initialized {
                std::process::abort();
            }
            task = (*task).next;
        }
        free_scope_tasks(&mut scope);
    }
}

#[cfg(test)]
#[path = "task_scope_checked_tests.rs"]
mod tests;
