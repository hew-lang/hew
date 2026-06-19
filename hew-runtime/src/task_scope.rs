//! Hew runtime: task management for structured concurrency.
//!
//! Tasks are units of concurrent work spawned via `fork name = call(...)`
//! inside a `scope { ... }` block. Each task runs on a separate OS thread
//! (from the runtime's pool), providing true parallelism. Tasks do NOT
//! share mutable state — like actors, they communicate via results, not
//! shared memory.
//!
//! Thread-safe completion notification uses `Mutex` + `Condvar` so that
//! `await` can block until a task finishes.

#[cfg(test)]
use std::cell::Cell;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;
use std::time::Duration;

use crate::internal::types::{HewTaskError, HewTaskState};
use crate::rc::hew_rc_drop;
use crate::util::{CondvarExt, MutexExt};

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
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`], or null.
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

// ── Cancellation tokens ─────────────────────────────────────────────────

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HewCancellationState {
    Active = 0,
    CancelRequested = 1,
    Observed = 2,
    Trapped = 3,
    Completed = 4,
}

impl HewCancellationState {
    fn from_i32(raw: i32) -> Self {
        match raw {
            0 => Self::Active,
            1 => Self::CancelRequested,
            2 => Self::Observed,
            3 => Self::Trapped,
            4 => Self::Completed,
            _ => panic!("HewCancellationToken.state contained an invalid discriminant"),
        }
    }

    fn is_requested(self) -> bool {
        matches!(self, Self::CancelRequested | Self::Observed | Self::Trapped)
    }
}

/// Opaque, ref-counted cancellation token used by scope-owned tasks.
///
/// Tokens form a parent-child tree: cancelling a parent is observed by every
/// descendant through [`hew_cancel_token_is_requested`]. The tree is owned by
/// task scopes and tasks; raw FFI handles returned by scope accessors are
/// borrowed unless the function explicitly says otherwise.
#[derive(Debug)]
pub struct HewCancellationToken {
    refs: AtomicUsize,
    state: AtomicI32,
    reason: AtomicI32,
    parent: *mut HewCancellationToken,
    children_total: AtomicI32,
    #[expect(
        dead_code,
        reason = "diagnostic counters are populated by later cancellation slices"
    )]
    children_terminal: AtomicI32,
    #[expect(
        dead_code,
        reason = "diagnostic counters are populated by later cancellation slices"
    )]
    last_nonterminal_child: AtomicUsize,
}

// SAFETY: all mutable token state is atomic. The parent pointer is retained
// for the token lifetime, and released only when this token's ref-count
// reaches zero.
unsafe impl Send for HewCancellationToken {}
// SAFETY: all shared token state is atomic and parent lifetime is retained by
// token ref-counting.
unsafe impl Sync for HewCancellationToken {}

unsafe fn hew_cancel_token_retain_impl(token: *mut HewCancellationToken) {
    if !token.is_null() {
        // SAFETY: caller guarantees `token` is a live token pointer.
        unsafe { (*token).refs.fetch_add(1, Ordering::Relaxed) };
    }
}

unsafe fn hew_cancel_token_release_impl(token: *mut HewCancellationToken) {
    if token.is_null() {
        return;
    }

    // SAFETY: caller guarantees `token` is a live token pointer.
    if unsafe { (*token).refs.fetch_sub(1, Ordering::Release) } != 1 {
        return;
    }

    std::sync::atomic::fence(Ordering::Acquire);
    // SAFETY: this was the last reference, so reclaim the Box allocation.
    let boxed = unsafe { Box::from_raw(token) };
    if !boxed.parent.is_null() {
        // SAFETY: child construction retained the parent for this token.
        unsafe { hew_cancel_token_release_impl(boxed.parent) };
    }
}

fn token_state(token: &HewCancellationToken) -> HewCancellationState {
    HewCancellationState::from_i32(token.state.load(Ordering::Acquire))
}

/// Increment the reference count on `token`.
///
/// Null is accepted and treated as a no-op.
///
/// # Safety
///
/// If non-null, `token` must be a live pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`]. The caller must guarantee the token is not
/// concurrently freed.
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_retain(token: *mut HewCancellationToken) {
    // SAFETY: delegated to impl; caller upholds the same contract.
    unsafe { hew_cancel_token_retain_impl(token) }
}

/// Decrement the reference count on `token`, freeing it when it reaches zero.
///
/// Null is accepted and treated as a no-op. When the last reference is dropped,
/// the parent's reference count is decremented recursively.
///
/// # Safety
///
/// If non-null, `token` must be a live pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`]. After this call returns the pointer must
/// not be used (it may have been freed).
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_release(token: *mut HewCancellationToken) {
    // SAFETY: delegated to impl; caller upholds the same contract.
    unsafe { hew_cancel_token_release_impl(token) }
}

/// Create a cancellation token derived from `parent`.
///
/// Passing null creates a root token. The returned token is owned by the caller
/// and must be attached to a scope/task that will release it.
///
/// # Safety
///
/// If `parent` is non-null, it must be a valid pointer returned by
/// [`hew_cancel_token_new_child`] or borrowed from
/// [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_new_child(
    parent: *mut HewCancellationToken,
) -> *mut HewCancellationToken {
    if !parent.is_null() {
        // SAFETY: caller guarantees `parent` is valid.
        unsafe {
            hew_cancel_token_retain_impl(parent);
            (*parent).children_total.fetch_add(1, Ordering::Relaxed);
        }
    }

    let token = Box::new(HewCancellationToken {
        refs: AtomicUsize::new(1),
        state: AtomicI32::new(HewCancellationState::Active as i32),
        reason: AtomicI32::new(0),
        parent,
        children_total: AtomicI32::new(0),
        children_terminal: AtomicI32::new(0),
        last_nonterminal_child: AtomicUsize::new(0),
    });
    Box::into_raw(token)
}

/// Request cancellation on `token`.
///
/// The transition from `Active` to `CancelRequested` happens at most once.
/// Descendant tokens observe the request transitively.
///
/// # Safety
///
/// `token` must be a valid pointer returned by [`hew_cancel_token_new_child`]
/// or borrowed from [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_cancel(token: *mut HewCancellationToken, reason: i32) {
    cabi_guard!(token.is_null());
    // SAFETY: caller guarantees `token` is valid.
    let t = unsafe { &*token };
    if t.state
        .compare_exchange(
            HewCancellationState::Active as i32,
            HewCancellationState::CancelRequested as i32,
            Ordering::AcqRel,
            Ordering::Acquire,
        )
        .is_ok()
    {
        t.reason.store(reason, Ordering::Release);
    }
}

unsafe fn cancel_token_is_requested_raw(token: *mut HewCancellationToken) -> bool {
    if token.is_null() {
        return false;
    }

    // SAFETY: caller guarantees `token` is valid.
    let t = unsafe { &*token };
    if token_state(t).is_requested() {
        return true;
    }

    // SAFETY: token construction retained the parent for this child.
    unsafe { cancel_token_is_requested_raw(t.parent) }
}

/// Return whether `token` or any ancestor has requested cancellation.
///
/// Returns `1` when cancellation is requested, otherwise `0`.
///
/// # Safety
///
/// `token` must be a valid pointer returned by [`hew_cancel_token_new_child`]
/// or borrowed from [`hew_task_scope_cancel_token`].
#[no_mangle]
pub unsafe extern "C" fn hew_cancel_token_is_requested(token: *mut HewCancellationToken) -> i32 {
    cabi_guard!(token.is_null(), 0);
    i32::from(
        // SAFETY: caller guarantees `token` is valid.
        unsafe { cancel_token_is_requested_raw(token) },
    )
}

fn cancel_token_is_requested(token: *mut HewCancellationToken) -> bool {
    // SAFETY: callers only pass live token pointers or null.
    unsafe { cancel_token_is_requested_raw(token) }
}

unsafe fn cancel_token_cancel_if_present(token: *mut HewCancellationToken, reason: i32) {
    if !token.is_null() {
        // SAFETY: caller guarantees non-null token is live.
        unsafe { hew_cancel_token_cancel(token, reason) };
    }
}

// ── Task ───────────────────────────────────────────────────────────────

/// A single task representing concurrent work within a scope.
///
/// Opaque, Box-allocated. Linked into its parent scope's task list via
/// the `next` pointer. Thread-safe completion notification via `done_signal`.
pub struct HewTask {
    /// Current lifecycle state (atomic for cross-thread visibility).
    ///
    /// Worker threads store `Done` with `Release` ordering after writing the
    /// result; readers use `Acquire` to observe both the state transition and
    /// any preceding result writes.
    pub state: AtomicI32,
    /// Error code (`None` = success).
    pub error: HewTaskError,
    /// Task result value (set on completion, malloc'd copy).
    pub result: *mut c_void,
    /// Size of `result` in bytes.
    pub result_size: usize,
    /// Parent scope (structured lifetime).
    pub scope: *mut HewTaskScope,
    /// Cancellation token owned by this task.
    pub cancel_token: *mut HewCancellationToken,
    /// Intrusive linked-list pointer within the scope.
    pub next: *mut HewTask,
    /// Thread-safe completion signal for `await` blocking.
    pub done_signal: Option<Arc<TaskDoneSignal>>,
    /// Thread join handle for the spawned worker thread.
    pub thread_handle: Option<std::thread::JoinHandle<()>>,
    /// Set when a cancelled running task must be reclaimed by deferred scope
    /// teardown after the worker thread exits.
    detached_on_cancel: bool,
    /// Captured environment pointer (Rc-allocated) for scope tasks.
    pub env_ptr: *mut c_void,
    /// Compiler-generated cleanup function invoked when a `Ready` or
    /// `Suspended` task is cancelled before its `task_fn` body can run drop
    /// elaboration. Called with `env_ptr` as argument before
    /// `mark_done(Cancelled)`. If null, cancellation skips user-level cleanup
    /// (`env_ptr` deallocation still occurs via `hew_task_free → hew_rc_drop`).
    ///
    /// # Contract
    ///
    /// The callback receives a **borrowed** `env_ptr`; it must NOT release the
    /// Rc allocation itself — that is done by `hew_task_free`. The callback
    /// runs at most once: `run_cancel_cleanup` clears the field before invoking
    /// it to prevent re-entrancy on double-cancel.
    ///
    /// The `"C-unwind"` ABI allows Rust panics originating inside the callback
    /// to propagate through the boundary and be caught by the
    /// `std::panic::catch_unwind` in `run_cancel_cleanup`. If the callback
    /// panics, the panic payload is logged via `set_last_error` (with prefix
    /// `"task_scope cancel_cleanup_fn panicked:"`) and cancellation continues
    /// — the panic is **not** re-raised.
    pub cancel_cleanup_fn: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
}

struct HewTaskScopeDeadline {
    cancelled: Arc<AtomicBool>,
    thread_handle: Option<JoinHandle<()>>,
    next: *mut HewTaskScopeDeadline,
}

type TaskCompletionCallback = unsafe extern "C" fn(*mut c_void);

#[derive(Debug, Clone, Copy)]
struct TaskDoneObserver {
    callback: TaskCompletionCallback,
    ctx: usize,
}

#[derive(Debug)]
struct TaskDoneState {
    done: bool,
    observers: Vec<TaskDoneObserver>,
}

/// Thread-safe signal for task completion notification.
#[derive(Debug)]
pub struct TaskDoneSignal {
    /// Guards completion state and one-shot observers.
    lock: Mutex<TaskDoneState>,
    /// Notified when the task completes.
    cond: Condvar,
}

impl TaskDoneSignal {
    fn new() -> Self {
        Self {
            lock: Mutex::new(TaskDoneState {
                done: false,
                observers: Vec::new(),
            }),
            cond: Condvar::new(),
        }
    }

    fn notify_done(&self) {
        let observers = {
            let mut state = self.lock.lock_or_recover();
            if state.done {
                Vec::new()
            } else {
                state.done = true;
                std::mem::take(&mut state.observers)
            }
        };
        self.cond.notify_all();
        for observer in observers {
            // SAFETY: `hew_task_completion_observe` accepts only non-null C ABI
            // callbacks; callback-specific context validity is part of that
            // callback's contract with its registrant.
            unsafe { (observer.callback)(observer.ctx as *mut c_void) };
        }
    }

    fn notify_waiters(&self) {
        self.cond.notify_all();
    }

    fn observe(&self, callback: TaskCompletionCallback, ctx: *mut c_void) {
        let fire_now = {
            let mut state = self.lock.lock_or_recover();
            if state.done {
                true
            } else {
                state.observers.push(TaskDoneObserver {
                    callback,
                    ctx: ctx as usize,
                });
                false
            }
        };
        if fire_now {
            // SAFETY: see the deferred-callback invocation above.
            unsafe { callback(ctx) };
        }
    }

    fn unobserve(&self, callback: TaskCompletionCallback, ctx: *mut c_void) -> bool {
        let mut state = self.lock.lock_or_recover();
        if state.done {
            return false;
        }
        let Some(pos) = state.observers.iter().position(|observer| {
            observer.callback as usize == callback as usize && observer.ctx == ctx as usize
        }) else {
            return false;
        };
        state.observers.swap_remove(pos);
        true
    }

    fn wait_until_done(&self) {
        let mut state = self.lock.lock_or_recover();
        while !state.done {
            state = self.cond.wait_or_recover(state);
        }
    }

    /// Block until `keep_waiting` returns `false`.
    ///
    /// Unlike [`wait_until_done`], this method re-evaluates `keep_waiting` on
    /// every condvar wake-up — including spurious ones triggered by a
    /// best-effort notify (e.g. from `hew_task_scope_cancel_one` on a Running
    /// task). Use this when the "done" predicate is external to the signal itself.
    fn wait_while<F: FnMut() -> bool>(&self, mut keep_waiting: F) {
        let mut guard = self.lock.lock_or_recover();
        while keep_waiting() {
            guard = self.cond.wait_or_recover(guard);
        }
    }
}

#[expect(
    clippy::missing_fields_in_debug,
    reason = "raw pointers and thread handles are not useful in debug output"
)]
impl std::fmt::Debug for HewTask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewTask")
            .field("state", &self.load_state())
            .field("error", &self.error)
            .field("result_size", &self.result_size)
            .finish()
    }
}

// SAFETY: Tasks are only accessed from the single actor thread that owns
// the enclosing task scope. No cross-thread sharing occurs.
unsafe impl Send for HewTask {}

impl HewTask {
    /// Atomically load the current state with `Acquire` ordering.
    ///
    /// Pairs with the `Release` store in [`hew_task_complete_threaded`] to
    /// ensure result data written by the worker thread is visible.
    fn load_state(&self) -> HewTaskState {
        let raw = self.state.load(Ordering::Acquire);
        HewTaskState::from_i32(raw).expect("HewTask.state contained an invalid discriminant")
    }

    /// Atomically store a new state.
    ///
    /// Use `Release` ordering when the store must publish preceding writes
    /// (e.g. result data) to other threads.
    fn store_state(&self, new_state: HewTaskState, ordering: Ordering) {
        self.state.store(new_state as i32, ordering);
    }

    fn notify_done_signal(&self) {
        if let Some(ref signal) = self.done_signal {
            signal.notify_done();
        }
    }

    fn notify_done_waiters(&self) {
        if let Some(ref signal) = self.done_signal {
            signal.notify_waiters();
        }
    }

    fn mark_done(&mut self, error: HewTaskError) {
        self.error = error;
        self.store_state(HewTaskState::Done, Ordering::Release);
        self.notify_done_signal();
    }

    fn mark_terminal_from_current_token(&mut self, default_error: HewTaskError) {
        let error = if cancel_token_is_requested(self.cancel_token) {
            HewTaskError::Cancelled
        } else {
            default_error
        };
        self.mark_done(error);
    }

    /// Invoke and clear `cancel_cleanup_fn`, if registered and `env_ptr` is non-null.
    ///
    /// Clears the field **before** calling the function to prevent re-entrancy
    /// on double-cancel. Safe to call when `env_ptr` is null — the call is
    /// skipped in that case.
    ///
    /// If the callback panics, the panic payload is extracted and logged via
    /// `set_last_error` (prefix `"task_scope cancel_cleanup_fn panicked:"`).
    /// The panic is caught and **not** re-raised; cancellation continues.
    fn run_cancel_cleanup(&mut self) {
        if let Some(f) = self.cancel_cleanup_fn.take() {
            if !self.env_ptr.is_null() {
                let env_ptr = self.env_ptr;
                // Wrapped in `catch_unwind` so that a panic in `cancel_cleanup_fn`
                // cannot abort the process or propagate through the cancel path.
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    // SAFETY: env_ptr is valid while the task is live; cancel_cleanup_fn
                    // is a valid extern "C-unwind" fn pointer (set at registration,
                    // cleared above); receives a borrowed reference and must not free
                    // the Rc allocation (that happens in hew_task_free → hew_rc_drop).
                    unsafe { f(env_ptr) };
                }));
                if let Err(panic_payload) = result {
                    let msg: String = if let Some(s) = panic_payload.downcast_ref::<&str>() {
                        (*s).to_string()
                    } else if let Some(s) = panic_payload.downcast_ref::<String>() {
                        s.clone()
                    } else {
                        "non-string panic payload".to_string()
                    };
                    let error_msg = format!("task_scope cancel_cleanup_fn panicked: {msg}");
                    crate::set_last_error(error_msg.clone());
                    #[cfg(test)]
                    {
                        *LAST_CANCEL_CLEANUP_PANIC_MSG
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(error_msg);
                    }
                }
            }
        }
    }
}

fn take_detached_task_handles(scope: &mut HewTaskScope) -> Vec<std::thread::JoinHandle<()>> {
    let mut handles = Vec::new();
    let mut cur = scope.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid while the scope is alive.
        let task = unsafe { &mut *cur };
        if task.detached_on_cancel {
            if let Some(handle) = task.thread_handle.take() {
                handles.push(handle);
            }
        }
        cur = task.next;
    }
    handles
}

fn reap_detached_scope_tasks(
    mut scope_box: Box<HewTaskScope>,
    detached_handles: Vec<std::thread::JoinHandle<()>>,
) {
    for handle in detached_handles {
        let _ = handle.join();
    }
    // SAFETY: every detached worker has exited, so no task pointer can be
    // observed again after this point.
    unsafe { free_scope_tasks(&mut scope_box) };
}

type TaskReaperState = (Box<HewTaskScope>, Vec<std::thread::JoinHandle<()>>);

fn take_reaper_state(state: &Mutex<Option<TaskReaperState>>) -> Option<TaskReaperState> {
    state.lock_or_recover().take()
}

/// # Safety
///
/// Callers must ensure no worker thread can access any task in `scope`.
unsafe fn free_scope_tasks(scope: &mut HewTaskScope) {
    let mut cur = scope.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers were Box-allocated by hew_task_new.
        let next = unsafe { (*cur).next };
        // SAFETY: cur was allocated by hew_task_new.
        unsafe { hew_task_free(cur) };
        cur = next;
    }
}

// ── Task lifecycle ─────────────────────────────────────────────────────

/// Allocate a new task.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_task_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_new() -> *mut HewTask {
    let task = Box::new(HewTask {
        state: AtomicI32::new(HewTaskState::Ready as i32),
        error: HewTaskError::None,
        result: ptr::null_mut(),
        result_size: 0,
        scope: ptr::null_mut(),
        cancel_token: ptr::null_mut(),
        next: ptr::null_mut(),
        done_signal: None,
        thread_handle: None,
        detached_on_cancel: false,
        env_ptr: ptr::null_mut(),
        cancel_cleanup_fn: None,
    });
    Box::into_raw(task)
}

/// Free a task and its result buffer.
///
/// # Safety
///
/// `task` must have been returned by [`hew_task_new`] and must not be
/// used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_task_free(task: *mut HewTask) {
    cabi_guard!(task.is_null());
    // SAFETY: Caller guarantees `task` was Box-allocated.
    let t = unsafe { Box::from_raw(task) };
    if !t.result.is_null() {
        // SAFETY: result was malloc'd by hew_task_set_result.
        unsafe { libc::free(t.result) };
    }
    if !t.env_ptr.is_null() {
        // SAFETY: env_ptr was set by hew_task_set_env from a valid Rc allocation.
        unsafe { hew_rc_drop(t.env_ptr.cast()) };
    }
    // SAFETY: cancel_token, when present, is owned by this task.
    unsafe { hew_cancel_token_release_impl(t.cancel_token) };
}

/// Associate an environment pointer with a task.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`]. `env` should
/// either be null or an Rc-allocated pointer returned by `hew_rc_new`.
#[no_mangle]
pub unsafe extern "C" fn hew_task_set_env(task: *mut HewTask, env: *mut c_void) {
    cabi_guard!(task.is_null());
    // SAFETY: caller guarantees `task` is a valid, non-null pointer.
    let t = unsafe { &mut *task };
    t.env_ptr = env;
}

/// Register a cancel-cleanup function for a task.
///
/// When the task is in `Ready` or `Suspended` state and is cancelled (via
/// [`hew_task_scope_cancel`] or [`hew_task_scope_cancel_one`]), the runtime
/// calls `cleanup_fn(env_ptr)` before transitioning the task to `Done`.
/// This lets compiler-generated code run drop elaboration for `@resource` and
/// `@linear` captures that would otherwise only execute inside `task_fn`.
///
/// Pass `null` to clear a previously registered function.
///
/// # Contract
///
/// - The callback receives a **borrowed** `env_ptr`; it must not release the
///   Rc allocation (that is done by `hew_task_free → hew_rc_drop`).
/// - The callback is invoked at most once (the field is cleared before the
///   call) even under double-cancel.
/// - The callback is skipped if `env_ptr` is null at cancellation time.
/// - The `"C-unwind"` ABI is required so that Rust panics from the callback
///   can be caught by the `catch_unwind` inside `run_cancel_cleanup`. If the
///   callback panics, the payload is logged via `set_last_error` (prefix
///   `"task_scope cancel_cleanup_fn panicked:"`) and cancellation continues.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_set_cancel_cleanup_fn(
    task: *mut HewTask,
    cleanup_fn: Option<unsafe extern "C-unwind" fn(*mut c_void)>,
) {
    cabi_guard!(task.is_null());
    // SAFETY: caller guarantees `task` is a valid, non-null pointer.
    unsafe { (*task).cancel_cleanup_fn = cleanup_fn };
}

/// Fetch the environment pointer associated with a task.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_get_env(task: *mut HewTask) -> *mut c_void {
    cabi_guard!(task.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `task` is a valid, non-null pointer.
    unsafe { (*task).env_ptr }
}

/// Get the task's result pointer, or null if not done.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_get_result(task: *mut HewTask) -> *mut c_void {
    cabi_guard!(task.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &*task };
    if t.load_state() != HewTaskState::Done {
        return ptr::null_mut();
    }
    t.result
}

/// Register a one-shot callback fired when `task` reaches `Done`.
///
/// The callback is invoked outside the task completion lock. If the task is
/// already done, the callback fires synchronously before this function returns.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
/// - `task` must be a valid task pointer owned by `scope`.
/// - `cb` must be a valid C ABI function pointer when non-null.
#[no_mangle]
pub unsafe extern "C" fn hew_task_completion_observe(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
    cb: Option<TaskCompletionCallback>,
    ctx: *mut c_void,
) -> i32 {
    if scope.is_null() || task.is_null() {
        crate::set_last_error("C-ABI guard failed: scope.is_null() || task.is_null()");
        return -1;
    }
    let Some(callback) = cb else {
        crate::set_last_error("C-ABI guard failed: cb.is_none()");
        return -1;
    };
    // SAFETY: caller guarantees `task` is a live task owned by `scope`; null was
    // rejected above. The scope pointer is part of the FFI contract and is not
    // dereferenced here beyond the null guard.
    let t = unsafe { &mut *task };
    if t.load_state() == HewTaskState::Done {
        // SAFETY: callback validity was checked above; context validity is the
        // registrant/callback contract.
        unsafe { callback(ctx) };
        return 0;
    }
    let signal = t
        .done_signal
        .get_or_insert_with(|| Arc::new(TaskDoneSignal::new()));
    signal.observe(callback, ctx);
    0
}

/// Remove a previously registered task-completion observer.
///
/// # Safety
///
/// `task` must be a live task allocated by this runtime and owned by `scope`.
/// `cb` and `ctx` must match the observer registration being removed; `ctx` is
/// not dereferenced by this function.
#[no_mangle]
pub unsafe extern "C" fn hew_task_completion_unobserve(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
    cb: Option<TaskCompletionCallback>,
    ctx: *mut c_void,
) -> i32 {
    if scope.is_null() || task.is_null() {
        crate::set_last_error("C-ABI guard failed: scope.is_null() || task.is_null()");
        return -1;
    }
    let Some(callback) = cb else {
        crate::set_last_error("C-ABI guard failed: cb.is_none()");
        return -1;
    };
    // SAFETY: caller guarantees `task` is a live task owned by `scope`; null was
    // rejected above. The scope pointer is part of the FFI contract and is not
    // dereferenced here beyond the null guard.
    let t = unsafe { &mut *task };
    let Some(signal) = t.done_signal.as_ref() else {
        return 0;
    };
    i32::from(signal.unobserve(callback, ctx))
}

// ── Suspending await (cut-task-sleep) ────────────────────────────────────────
//
// These entries flip `await t` over a scope-owned child `Task<T>` from a
// worker-blocking `hew_task_await_blocking` (a condvar) onto the read-slot /
// `enqueue_resume` substrate when the caller carries an execution context. The
// codegen suspend ramp calls `hew_task_await_suspend` to register the parked
// continuation as a task-completion observer, suspends on TASK_AWAIT_SUSPEND,
// and on the resume / immediate-ready edge reads the result through
// `hew_task_get_result`. The task handle is BORROWED across the suspend (the
// scope-join owns its free). The wake discipline mirrors `ChannelCore::wake`:
// deposit a readiness status into the slot (a no-op + no wake if the abandon
// edge cancelled it first) then `enqueue_resume` the parked actor.

/// Codegen ABI: the await parked the continuation; the runtime wakes it via
/// `enqueue_resume`. The caller MUST `coro.suspend`.
pub const TASK_AWAIT_SUSPEND: i32 = 0;
/// Codegen ABI: the task is already `Done`; the caller MUST NOT suspend and
/// binds the result on the immediate edge.
pub const TASK_AWAIT_READY: i32 = 1;

/// One parked task-await waiter: the awaiting actor + its readiness slot. The
/// task's completion observer owns one retained in-flight ref on `slot` and
/// fires `wake` exactly once on `Done`.
struct TaskAwaitWaiter {
    /// The parked-continuation actor, woken via `enqueue_resume`. Raw and
    /// possibly-stale: `enqueue_resume` re-validates liveness, never this code.
    actor: *mut crate::actor::HewActor,
    /// The readiness slot; the observer holds one retained ref while registered.
    slot: *mut crate::read_slot::HewReadSlot,
}

/// Task-completion observer callback: deposit a Data readiness signal into the
/// waiter's slot and wake the parked actor, then release the observer's
/// in-flight slot ref and free the waiter box. Runs OUTSIDE the task lock
/// (`TaskDoneSignal::notify_done` fires observers after releasing its lock).
///
/// # Safety
///
/// `ctx` must be the `*mut TaskAwaitWaiter` handed to `observe` by
/// [`hew_task_await_suspend`]; it is consumed (boxed-freed) exactly once here.
unsafe extern "C" fn task_await_wake(ctx: *mut c_void) {
    if ctx.is_null() {
        return;
    }
    // SAFETY: `ctx` is the waiter box `hew_task_await_suspend` leaked via
    // `Box::into_raw`; the observer fires exactly once, so reclaiming it here is
    // the single free.
    let waiter = unsafe { Box::from_raw(ctx.cast::<TaskAwaitWaiter>()) };
    // SAFETY: the observer holds an in-flight ref on the slot; depositing a
    // terminal status is the documented reactor-deposit contract. A no-op + no
    // wake if the abandon edge cancelled the slot first (the channel-core race
    // guard).
    let do_wake = unsafe {
        crate::read_slot::read_slot_deposit_status(waiter.slot, crate::read_slot::ReadStatus::Data)
    };
    if do_wake {
        // SAFETY: `enqueue_resume` re-validates `waiter.actor` under the registry
        // lock; a freed actor drops the wake with no deref.
        unsafe { crate::scheduler::enqueue_resume(waiter.actor, ptr::null_mut()) };
    }
    // Release the observer's in-flight ref (the single authority for it).
    // SAFETY: the observer owned this ref; nothing else releases it.
    unsafe { crate::read_slot::hew_read_slot_free(waiter.slot) };
}

/// Register a suspending `await t` over a child `Task<T>`.
///
/// Returns [`TASK_AWAIT_READY`] when the task is already `Done` (bind via
/// `hew_task_get_result` on the immediate edge), or [`TASK_AWAIT_SUSPEND`]
/// after parking the awaiting continuation as a task-completion observer. The
/// caller MUST `coro.suspend` on SUSPEND and bind on READY / resume.
///
/// # Safety
///
/// - `scope` must be a valid scope pointer (part of the FFI contract; not
///   dereferenced beyond the null guard).
/// - `task` must be a live task owned by `scope`.
/// - `actor` is the awaiting actor (`hew_actor_self`).
/// - `slot` is a live read slot the caller created and holds the creator ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_task_await_suspend(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
    actor: *mut crate::actor::HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> i32 {
    if scope.is_null() || task.is_null() || slot.is_null() {
        crate::set_last_error("C-ABI guard failed: scope/task/slot null in hew_task_await_suspend");
        // Fail closed: report READY so the caller binds immediately rather than
        // parking forever; `hew_task_get_result` returns null for a non-done
        // task, which the unit await ignores.
        return TASK_AWAIT_READY;
    }
    // SAFETY: caller guarantees `task` is a live task owned by `scope`.
    let t = unsafe { &mut *task };
    if t.load_state() == HewTaskState::Done {
        return TASK_AWAIT_READY;
    }
    // Park: the observer takes an in-flight ref so the wake cannot free the slot
    // out from under the abandon edge.
    // SAFETY: caller holds the creator ref, so the slot is live to retain.
    unsafe { crate::read_slot::read_slot_retain(slot) };
    let waiter = Box::into_raw(Box::new(TaskAwaitWaiter { actor, slot }));
    let signal = t
        .done_signal
        .get_or_insert_with(|| Arc::new(TaskDoneSignal::new()));
    // `observe` fires `task_await_wake` synchronously if the task became Done
    // between our state check and here — which consumes the waiter box + ref and
    // deposits readiness, so the SUSPEND we return still resumes immediately on
    // the coro switch (the slot already carries Data). This is the same
    // already-ready race the channel-recv park tolerates.
    signal.observe(task_await_wake, waiter.cast::<c_void>());
    TASK_AWAIT_SUSPEND
}

/// Detach an abandoned suspending awaiter (the codegen abandon edge). Cancels
/// the read slot so a racing task `Done` observer drops its wake (the
/// channel-core race guard) and releases the awaiter's creator ref on the slot.
///
/// The observer box + its retained in-flight slot ref are reclaimed when the
/// task eventually completes (the scope-join always drives every child to
/// completion or cancellation before the scope exits, so the observer always
/// fires): `task_await_wake` sees the cancelled slot, deposits no wake, frees
/// the box, and releases the in-flight ref.
///
/// # Safety
///
/// `scope`/`task` are part of the FFI contract (not dereferenced beyond null
/// guards). `slot` is the awaiter's read slot; the caller holds the creator ref
/// released here.
#[no_mangle]
pub unsafe extern "C" fn hew_task_detach_await(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    let _ = (scope, task);
    if slot.is_null() {
        return;
    }
    // Cancel so a racing observer wake is suppressed (deposit_status returns
    // false on a cancelled slot), then release the creator ref. The observer's
    // own in-flight ref is released when it fires against the cancelled slot.
    // SAFETY: caller holds the creator ref; cancel + free are the documented
    // abandon sequence.
    unsafe {
        crate::read_slot::hew_read_slot_cancel(slot);
        crate::read_slot::hew_read_slot_free(slot);
    }
}

// ── Suspending scope-deadline join (cut-task-sleep S5) ───────────────────────
//
// `scope { ... } after(d) { body }` with a NON-EMPTY timeout body races the
// scope's child-task join (wait-ALL) against a timer-wheel deadline. The codegen
// `SuspendingScopeDeadline` ramp owns ONE shared `HewAwaitCancel` arbiter (the
// same first-ready one-shot CAS the suspending select uses): the deadline arm is
// armed via `hew_await_cancel_schedule_deadline_ms`, and the JOIN arm is wired
// here — the scope's last child to complete fires `hew_await_cancel_complete` and
// `enqueue_resume`s the parked actor exactly once. Whichever arm wins the CAS
// settles the wait; the loser's finish returns 0 and drops its wake.

/// Codegen ABI: every scope child was already `Done` when the join observer was
/// installed; the arbiter is completed synchronously and the caller MUST NOT
/// suspend — it binds the join-won (scope-complete) edge immediately.
pub const SCOPE_JOIN_READY: i32 = 1;
/// Codegen ABI: at least one child is outstanding; the caller MUST `coro.suspend`
/// and resume on the first-ready wake (join completes or the deadline fires).
pub const SCOPE_JOIN_SUSPEND: i32 = 0;

/// Shared join-arm state across a scope's child-completion observers. One ref is
/// held per registered child observer; the last observer to fire reclaims the
/// box and releases the arbiter ref. `pending` counts the not-yet-fired child
/// observers; reaching zero is the wait-ALL completion that wins the arbiter.
struct ScopeJoinWaiter {
    /// The shared first-ready arbiter (retained once for this waiter). The
    /// winning child observer calls `hew_await_cancel_complete` on it.
    reg: *mut crate::await_cancel::HewAwaitCancel,
    /// The parked-continuation actor, woken via `enqueue_resume` when the join
    /// wins. Raw and possibly-stale: `enqueue_resume` re-validates liveness.
    actor: *mut crate::actor::HewActor,
    /// Outstanding child observers not yet fired. The observer that decrements
    /// this to zero is the join winner.
    pending: AtomicUsize,
    /// Live observer references on this box (one per registered observer). The
    /// observer that drops the last ref reclaims the box + releases `reg`.
    refs: AtomicUsize,
}

/// Release one `ScopeJoinWaiter` box reference; the last release reclaims the box
/// and releases the arbiter ref it retained.
///
/// # Safety
///
/// `waiter` must be a live `ScopeJoinWaiter` box pointer this caller holds a ref
/// to.
unsafe fn scope_join_waiter_release(waiter: *mut ScopeJoinWaiter) {
    if waiter.is_null() {
        return;
    }
    // SAFETY: caller holds one live reference.
    let prev = unsafe { (*waiter).refs.fetch_sub(1, Ordering::AcqRel) };
    debug_assert!(prev > 0, "scope-join waiter release on a released box");
    if prev != 1 {
        return;
    }
    // SAFETY: last ref; reclaim the box and release the arbiter ref it held.
    let boxed = unsafe { Box::from_raw(waiter) };
    // SAFETY: the waiter retained `reg` once at registration.
    unsafe { crate::await_cancel::hew_await_cancel_free(boxed.reg) };
}

/// One scope child completed (or was cancelled). Decrement the wait-ALL count;
/// the last child wins the arbiter and wakes the parked actor. Runs OUTSIDE the
/// task lock (`TaskDoneSignal::notify_done` fires observers after releasing it).
///
/// # Safety
///
/// `ctx` must be a `*mut ScopeJoinWaiter` handed to `observe` by
/// [`hew_task_scope_completion_observe`]; this consumes exactly one box ref.
unsafe extern "C" fn scope_join_child_done(ctx: *mut c_void) {
    if ctx.is_null() {
        return;
    }
    let waiter = ctx.cast::<ScopeJoinWaiter>();
    // SAFETY: the registrant retained one box ref per observer; this observer
    // owns the ref it releases at the end.
    let w = unsafe { &*waiter };
    // The wait-ALL fires only when the LAST child completes. `fetch_sub` returns
    // the prior value, so `prev == 1` means this observer drove pending to zero.
    let prev = w.pending.fetch_sub(1, Ordering::AcqRel);
    if prev == 1 {
        // Join won: settle the shared arbiter. `complete` returns 1 iff this call
        // won the one-shot CAS (the deadline had not already fired); only then do
        // we wake — the deadline-won edge already woke the actor.
        // SAFETY: the waiter holds a retained arbiter ref for the lifetime of the
        // box.
        let won = unsafe { crate::await_cancel::hew_await_cancel_complete(w.reg) != 0 };
        if won {
            let actor = w.actor;
            if !actor.is_null() {
                // SAFETY: `enqueue_resume` re-validates `actor` liveness under the
                // registry lock; a freed actor drops the wake with no deref.
                unsafe { crate::scheduler::enqueue_resume(actor, ptr::null_mut()) };
            }
        }
    }
    // Release this observer's box ref (the last release reclaims the box).
    // SAFETY: this observer held exactly one box ref.
    unsafe { scope_join_waiter_release(waiter) };
}

/// Wire the scope's child-task join (wait-ALL) as the completion arm of the
/// shared `SuspendingScopeDeadline` arbiter `reg`.
///
/// Counts the scope's not-yet-`Done` children. When every child is already
/// `Done`, completes the arbiter synchronously and returns [`SCOPE_JOIN_READY`]
/// (the caller binds the scope-complete edge without suspending). Otherwise
/// registers one counting observer per outstanding child; the last child to
/// complete fires `hew_await_cancel_complete` + `enqueue_resume`, and returns
/// [`SCOPE_JOIN_SUSPEND`] (the caller MUST `coro.suspend`).
///
/// The arbiter is BORROWED: this call retains its own arbiter reference for the
/// join waiter's lifetime and releases it when the last observer fires (or the
/// synchronous-complete path frees it before returning). The deadline arm and
/// the caller's resume/abandon edges hold their own references.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
/// - `reg` must be a live arbiter the caller created via `hew_await_cancel_new`.
/// - `actor` is the awaiting actor (`hew_actor_self`); may be null (no wake).
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_completion_observe(
    scope: *mut HewTaskScope,
    reg: *mut crate::await_cancel::HewAwaitCancel,
    actor: *mut crate::actor::HewActor,
) -> i32 {
    if scope.is_null() || reg.is_null() {
        crate::set_last_error("C-ABI guard failed: scope/reg null in scope_completion_observe");
        // Fail closed: report READY so the caller binds the join-won edge rather
        // than parking forever on an un-wired arbiter.
        return SCOPE_JOIN_READY;
    }
    // SAFETY: caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };

    // First pass: count outstanding (not-yet-Done) children.
    let mut pending: usize = 0;
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: all task pointers in the scope list are valid.
        let t = unsafe { &*cur };
        if t.load_state() != HewTaskState::Done {
            pending += 1;
        }
        cur = t.next;
    }

    if pending == 0 {
        // The whole scope already joined. Win the arbiter synchronously; the
        // caller binds the scope-complete edge on the immediate path.
        // SAFETY: `reg` is a live arbiter.
        unsafe { crate::await_cancel::hew_await_cancel_complete(reg) };
        return SCOPE_JOIN_READY;
    }

    // Park: one box ref + one arbiter ref per outstanding child observer, plus a
    // single registrant ref released after the registration loop so a child that
    // completes mid-loop cannot reclaim the box before every observer is wired.
    // SAFETY: `reg` is live; retain one arbiter ref the box owns for its lifetime.
    unsafe { crate::await_cancel::hew_await_cancel_retain(reg) };
    let waiter = Box::into_raw(Box::new(ScopeJoinWaiter {
        reg,
        actor,
        pending: AtomicUsize::new(pending),
        // refs = one per observer (set below) + one registrant guard ref.
        refs: AtomicUsize::new(pending + 1),
    }));

    // Second pass: register a counting observer on each outstanding child. A
    // child that became Done between the two passes fires `scope_join_child_done`
    // synchronously inside `observe`, which still decrements `pending` correctly.
    cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: all task pointers in the scope list are valid.
        let t = unsafe { &mut *cur };
        let next = t.next;
        if t.load_state() != HewTaskState::Done {
            let signal = t
                .done_signal
                .get_or_insert_with(|| Arc::new(TaskDoneSignal::new()));
            signal.observe(scope_join_child_done, waiter.cast::<c_void>());
        }
        cur = next;
    }

    // Drop the registrant guard ref. If every child already fired during the
    // registration loop (all completed mid-loop), this release reclaims the box.
    // SAFETY: the registrant held exactly one guard ref.
    unsafe { scope_join_waiter_release(waiter) };
    SCOPE_JOIN_SUSPEND
}

/// Detach an abandoned scope-deadline join arm (the codegen abandon edge).
///
/// The per-child observers remain registered; the scope-join driven by the
/// resume/abandon teardown (`hew_task_scope_cancel` + `hew_task_scope_join_all`)
/// drives every child to completion, so each observer still fires exactly once
/// against the now-cancelled arbiter (`hew_await_cancel_complete` returns 0 — the
/// abandon edge already won the one-shot CAS) and releases its box ref. The box
/// is reclaimed when the last observer fires. This entry is the explicit
/// no-resource-leaked acknowledgement that the join arm needs no separate
/// unobserve sweep: cancelling the scope forces every child Done, which fires
/// every observer.
///
/// # Safety
///
/// `scope` is part of the FFI contract (not dereferenced beyond the null guard).
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_detach_completion(scope: *mut HewTaskScope) {
    // The observers self-reclaim when their children complete under the
    // scope-cancel + join that the abandon/resume teardown always runs. Nothing
    // to do here beyond the documented contract; kept as a named ABI seam so the
    // codegen abandon edge has an explicit detach call mirroring the await /
    // select arms.
    let _ = scope;
}

/// Set the task's result by deep-copying `result`.
///
/// # Panics
///
/// Panics if memory allocation for the result copy fails (out of memory).
///
/// # Safety
///
/// - `task` must be a valid pointer returned by [`hew_task_new`].
/// - `result` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_task_set_result(task: *mut HewTask, result: *mut c_void, size: usize) {
    cabi_guard!(task.is_null());
    // SAFETY: Caller guarantees `task` is valid.
    // SAFETY: caller guarantees task is valid.
    let t = unsafe { &mut *task };
    if size > 0 && !result.is_null() {
        // SAFETY: malloc for deep copy.
        let buf = unsafe { libc::malloc(size) };
        assert!(!buf.is_null(), "OOM allocating task result ({size} bytes)");
        // SAFETY: result points to `size` readable bytes.
        unsafe { ptr::copy_nonoverlapping(result.cast::<u8>(), buf.cast::<u8>(), size) };
        t.result = buf;
        t.result_size = size;
    }
}

/// Get the task's error code.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_get_error(task: *mut HewTask) -> i32 {
    cabi_guard!(task.is_null(), HewTaskError::None as i32);
    // SAFETY: Caller guarantees `task` is valid.
    unsafe { (*task).error as i32 }
}

/// Check whether a task was cancelled.
///
/// Returns `1` if cancelled, `0` otherwise.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_is_cancelled(task: *mut HewTask) -> i32 {
    cabi_guard!(task.is_null(), 0);
    // SAFETY: Caller guarantees `task` is valid.
    i32::from(unsafe { (*task).error } == HewTaskError::Cancelled)
}

/// Attach an owned cancellation token to `task`.
///
/// The task takes ownership of `token` and releases any token it previously
/// owned. Passing null clears the task token.
///
/// # Safety
///
/// - `task` must be a valid pointer returned by [`hew_task_new`].
/// - `token` must be null or an owned pointer returned by
///   [`hew_cancel_token_new_child`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_set_cancel_token(
    task: *mut HewTask,
    token: *mut HewCancellationToken,
) {
    cabi_guard!(task.is_null());
    // SAFETY: caller guarantees `task` is valid.
    let t = unsafe { &mut *task };
    let old = std::mem::replace(&mut t.cancel_token, token);
    // SAFETY: old, when present, was owned by this task.
    unsafe { hew_cancel_token_release_impl(old) };
}

// ── Thread-spawned tasks ───────────────────────────────────────────────

/// Task function type: takes task pointer, stores result and marks done.
///
/// The generated code calls `hew_task_set_result` and
/// `hew_task_complete_threaded` from within this function.
pub type TaskFn = unsafe extern "C" fn(*mut HewTask);

/// Context-aware task function type used by spawned closure/fork-child codegen.
///
/// The first parameter is the child thread's installed execution context. The
/// second is the task pointer whose result/completion is owned by the wrapper.
pub type ContextTaskFn =
    unsafe extern "C" fn(*mut crate::execution_context::HewExecutionContext, *mut HewTask);

/// Spawn a task on a new OS thread.
///
/// The runtime calls `task_fn(task)` on a new thread. The task function
/// is responsible for computing the result, calling `hew_task_set_result`,
/// and calling `hew_task_complete_threaded` when done.
///
/// Returns the task pointer (same as input) for convenience.
///
/// # Safety
///
/// - `task` must be a valid pointer returned by [`hew_task_new`].
/// - `task_fn` must be a valid function pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_task_spawn_thread(task: *mut HewTask, task_fn: TaskFn) {
    cabi_guard!(task.is_null());

    // Set up the done signal for cross-thread notification.
    let signal = Arc::new(TaskDoneSignal::new());
    // SAFETY: Caller guarantees `task` is valid. We write before spawning the thread.
    let t = unsafe { &mut *task };
    t.done_signal = Some(Arc::clone(&signal));
    t.store_state(HewTaskState::Running, Ordering::Relaxed);

    // We must pass raw pointers across the thread boundary.
    let task_raw = task as usize;
    let fn_raw = task_fn as usize;

    let handle = std::thread::spawn(move || {
        let task_ptr = task_raw as *mut HewTask;
        // SAFETY: fn_raw is a valid TaskFn function pointer passed to
        // hew_task_spawn_thread via the task_fn parameter.
        let fn_ptr: TaskFn = unsafe { std::mem::transmute(fn_raw) };

        // SAFETY: task_ptr is valid for the lifetime of the thread (scope
        // waits for all tasks before destroying them). The child thread
        // inherits the lexical task scope through its canonical context so
        // cooperate-sites can observe the child token.
        let child_scope = unsafe { (*task_ptr).scope };
        let child_token = if child_scope.is_null() {
            ptr::null_mut()
        } else {
            // SAFETY: child_scope is owned by the parent scope for the child
            // thread lifetime.
            unsafe { (*child_scope).cancel_token }
        };
        let mut execution_context = crate::execution_context::HewExecutionContext {
            cancel_token: child_token,
            task_scope: child_scope,
            prev_context: crate::execution_context::current_context(),
            ..crate::execution_context::HewExecutionContext::default()
        };
        let previous_context = execution_context.prev_context;
        let installed_previous =
            crate::execution_context::set_current_context(&raw mut execution_context);
        debug_assert_eq!(installed_previous, previous_context);
        // SAFETY: fn_ptr is the validated TaskFn supplied to
        // hew_task_spawn_thread, and task_ptr stays live until scope teardown.
        unsafe { fn_ptr(task_ptr) };
        let restored_context = crate::execution_context::set_current_context(previous_context);
        debug_assert_eq!(restored_context, &raw mut execution_context);

        // Signal completion.
        signal.notify_done();
    });

    t.thread_handle = Some(handle);
}

/// Spawn a task on a new OS thread with a child execution context derived from
/// `parent_ctx`.
///
/// The child inherits cancellation lineage, supervisor lineage, and trace
/// context by value. Actor identity, actor-local arena, and lock seat remain
/// empty for the spawned task's own execution context.
///
/// Returns `0` on success and `-1` on fail-closed rejection.
///
/// # Safety
///
/// - `parent_ctx` must be the live execution context installed at the spawn
///   site.
/// - `task` must be a valid pointer returned by [`hew_task_new`].
/// - `task_fn` must be a valid function pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_task_spawn_thread_with_inherited_context(
    parent_ctx: *mut crate::execution_context::HewExecutionContext,
    task: *mut HewTask,
    task_fn: ContextTaskFn,
) -> i32 {
    if parent_ctx.is_null() {
        crate::set_last_error(crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED_AT_SPAWN);
        return -1;
    }
    cabi_guard!(task.is_null(), -1);

    // Snapshot inherited lanes before the parent dispatch frame can move on.
    // SAFETY: caller guarantees parent_ctx is live for this call.
    let (parent_supervisor_raw, supervisor_child_index, trace, parent_cancel_token) = unsafe {
        let parent = &*parent_ctx;
        (
            parent.parent_supervisor as usize,
            parent.supervisor_child_index,
            parent.trace,
            parent.cancel_token,
        )
    };

    // SAFETY: task is valid. If the parent has a cancellation token, make the
    // task own a child token linked to that parent; otherwise keep the token
    // installed by hew_task_scope_spawn.
    let t = unsafe { &mut *task };
    if !parent_cancel_token.is_null() {
        // SAFETY: parent_cancel_token was snapshotted from a live parent context.
        let child_token = unsafe { hew_cancel_token_new_child(parent_cancel_token) };
        // SAFETY: task is valid and takes ownership of child_token.
        unsafe { hew_task_set_cancel_token(task, child_token) };
    }
    let child_scope_raw = t.scope as usize;
    let child_token_raw = t.cancel_token as usize;

    // Set up the done signal for cross-thread notification.
    let signal = Arc::new(TaskDoneSignal::new());
    t.done_signal = Some(Arc::clone(&signal));
    t.store_state(HewTaskState::Running, Ordering::Relaxed);

    let task_raw = task as usize;
    let fn_raw = task_fn as usize;

    let handle = std::thread::spawn(move || {
        let task_ptr = task_raw as *mut HewTask;
        let child_scope = child_scope_raw as *mut HewTaskScope;
        let child_token = child_token_raw as *mut HewCancellationToken;
        let parent_supervisor = parent_supervisor_raw as *mut c_void;
        // SAFETY: fn_raw is a valid ContextTaskFn supplied through the C ABI.
        let fn_ptr: ContextTaskFn = unsafe { std::mem::transmute(fn_raw) };

        let mut execution_context = crate::execution_context::HewExecutionContext {
            actor: ptr::null_mut(),
            actor_id: 0,
            parent_supervisor,
            supervisor_child_index,
            flags: 0,
            cancel_token: child_token,
            task_scope: child_scope,
            arena: ptr::null_mut(),
            trace,
            partition_policy: ptr::null_mut(),
            prev_context: ptr::null_mut(),
            lock_seat: ptr::null_mut(),
            reply_channel: ptr::null_mut(),
        };
        let installed_previous =
            crate::execution_context::set_current_context(&raw mut execution_context);
        debug_assert!(installed_previous.is_null());

        // SAFETY: fn_ptr is the validated ContextTaskFn supplied to this
        // helper, and task_ptr stays live until scope teardown.
        unsafe { fn_ptr(&raw mut execution_context, task_ptr) };

        let restored_context = crate::execution_context::set_current_context(ptr::null_mut());
        debug_assert_eq!(restored_context, &raw mut execution_context);
        signal.notify_done();
    });

    t.thread_handle = Some(handle);
    0
}

/// Block the calling thread until the task completes, then return
/// the result pointer.
///
/// Returns the task's result pointer, or null if the task produced
/// no result (e.g., was cancelled or returned void).
///
/// # Safety
///
/// - `task` must be a valid pointer returned by [`hew_task_new`].
/// - Must not be called from the same thread that is running the task.
#[no_mangle]
pub unsafe extern "C" fn hew_task_await_blocking(task: *mut HewTask) -> *mut c_void {
    cabi_guard!(task.is_null(), ptr::null_mut());

    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &*task };

    // If already done, return immediately (Acquire pairs with the worker's
    // Release store, ensuring the result data is visible).
    if t.load_state() == HewTaskState::Done {
        return t.result;
    }

    // Wait until the task state transitions to Done.
    //
    // `wait_while` re-evaluates the predicate on every condvar wake-up,
    // including best-effort notifies fired by `hew_task_scope_cancel_one`
    // when the task is still Running. Without this loop a spurious notify
    // could unblock the awaiter before the worker has written the result or
    // transitioned the state to Done.
    if let Some(ref signal) = t.done_signal {
        signal.wait_while(|| t.load_state() != HewTaskState::Done);
    }

    // SAFETY: state is Done (Acquire-loaded inside wait_while's loop exit);
    // the worker's Release store guarantees result data is visible here.
    unsafe { &*task }.result
}

/// Mark a threaded task as completed.
///
/// Called from the task's thread after setting the result. Updates the
/// task state and increments the scope's completed count (thread-safe
/// via the done signal, not the scope's internal counter — the scope
/// counter is updated at join time).
///
/// # Safety
///
/// - `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_complete_threaded(task: *mut HewTask) {
    cabi_guard!(task.is_null());
    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &mut *task };
    // Release store: ensures all preceding writes (result data, result_size)
    // are visible to any thread that subsequently Acquire-loads `Done`.
    t.mark_terminal_from_current_token(HewTaskError::None);
}

/// Wait for all tasks in a scope to complete (join all threads).
///
/// This is called at scope exit to ensure structured concurrency:
/// no task outlives its enclosing scope.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_join_all(scope: *mut HewTaskScope) {
    cabi_guard!(scope.is_null());
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let t = unsafe { &mut *cur };

        let detach_cancelled_worker =
            cancel_token_is_requested(s.cancel_token) && t.thread_handle.is_some();

        if detach_cancelled_worker {
            t.detached_on_cancel = true;
        } else if let Some(handle) = t.thread_handle.take() {
            let _ = handle.join();
            t.detached_on_cancel = false;
        } else {
            t.detached_on_cancel = false;
        }

        // Wait on done signal only once there is no outstanding worker handle.
        // After cancellation, the join handle is the liveness authority: a task
        // may have published `Done` and notified the condvar before its worker
        // thread has actually exited.
        if !t.detached_on_cancel && t.load_state() == HewTaskState::Done {
            if let Some(ref signal) = t.done_signal {
                signal.wait_until_done();
            }
        }

        // Update scope count.
        if t.load_state() == HewTaskState::Done && s.completed_count < s.task_count {
            s.completed_count += 1;
        }

        cur = t.next;
    }
}

/// Check if the scope's cancellation flag is set.
///
/// Returns `1` if cancelled, `0` otherwise.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_is_cancelled(scope: *mut HewTaskScope) -> i32 {
    cabi_guard!(scope.is_null(), 0);
    // SAFETY: Caller guarantees `scope` is valid.
    i32::from(unsafe { cancel_token_is_requested((*scope).cancel_token) })
}

// ── Task scope ─────────────────────────────────────────────────────────

/// Intra-actor cooperative task scope.
///
/// Owns a linked list of tasks and tracks completion counts.
/// The cancellation token is the semantic authority; `cancelled` remains a
/// compatibility mirror while older call sites migrate.
#[derive(Debug)]
pub struct HewTaskScope {
    /// Head of the intrusive linked list of child tasks.
    tasks: *mut HewTask,
    /// Total number of spawned tasks.
    task_count: i32,
    /// Number of completed tasks.
    completed_count: i32,
    /// Cooperative cancellation flag (atomic: tasks run on OS threads).
    pub(crate) cancelled: AtomicBool,
    /// Scope-root cancellation token owned by this task scope.
    pub(crate) cancel_token: *mut HewCancellationToken,
    deadlines: *mut HewTaskScopeDeadline,
    /// Parent scope for nesting (reserved for future nested scope support).
    #[expect(dead_code, reason = "reserved for future nested scope tree support")]
    parent: *mut HewTaskScope,
}

// SAFETY: Task scopes are only accessed from the single actor thread.
unsafe impl Send for HewTaskScope {}

impl Drop for HewTaskScope {
    fn drop(&mut self) {
        // SAFETY: cancel_token, when present, is owned by this scope.
        unsafe { hew_cancel_token_release_impl(self.cancel_token) };
    }
}

unsafe fn cancel_scope_deadlines(scope: &mut HewTaskScope) {
    let mut cur = scope.deadlines;
    scope.deadlines = ptr::null_mut();
    while !cur.is_null() {
        // SAFETY: deadline nodes are Box-allocated and owned by this scope list.
        let mut deadline = unsafe { Box::from_raw(cur) };
        cur = deadline.next;
        deadline.cancelled.store(true, Ordering::Release);
        if let Some(handle) = deadline.thread_handle.take() {
            handle.thread().unpark();
            let _ = handle.join();
        }
    }
}

/// Create a new empty task scope.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_task_scope_destroy`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_new() -> *mut HewTaskScope {
    let parent_scope = current_task_scope();
    let parent_token = if parent_scope.is_null() {
        ptr::null_mut()
    } else {
        // SAFETY: the task-scope lane only stores live scope pointers while a
        // scope is active in the current context.
        unsafe { (*parent_scope).cancel_token }
    };
    // SAFETY: null parent creates a root token; non-null parent_token is retained.
    let cancel_token = unsafe { hew_cancel_token_new_child(parent_token) };
    let scope = Box::new(HewTaskScope {
        tasks: ptr::null_mut(),
        task_count: 0,
        completed_count: 0,
        cancelled: AtomicBool::new(false),
        cancel_token,
        deadlines: ptr::null_mut(),
        parent: ptr::null_mut(),
    });
    Box::into_raw(scope)
}

/// Return the borrowed cancellation token owned by `scope`.
///
/// The returned pointer is valid until [`hew_task_scope_destroy`] and must not
/// be released by the caller.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_cancel_token(
    scope: *mut HewTaskScope,
) -> *mut HewCancellationToken {
    cabi_guard!(scope.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `scope` is valid.
    unsafe { (*scope).cancel_token }
}

/// Spawn a task into the scope.
///
/// The task is prepended to the scope's linked list.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
/// - `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_spawn(scope: *mut HewTaskScope, task: *mut HewTask) {
    cabi_guard!(scope.is_null() || task.is_null());
    // SAFETY: Caller guarantees both pointers are valid.
    let s = unsafe { &mut *scope };
    // SAFETY: caller guarantees task is valid.
    let t = unsafe { &mut *task };
    t.scope = scope;
    if t.cancel_token.is_null() {
        // SAFETY: `s.cancel_token` is valid for the scope lifetime.
        let token = unsafe { hew_cancel_token_new_child(s.cancel_token) };
        // SAFETY: `task` is valid and takes ownership of the child token.
        unsafe { hew_task_set_cancel_token(task, token) };
    }
    t.store_state(HewTaskState::Ready, Ordering::Relaxed);
    // Prepend to task list.
    t.next = s.tasks;
    s.tasks = task;
    s.task_count += 1;
}

/// Poll for the next ready task.
///
/// Returns a pointer to a `READY` task, or null if none.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_poll(scope: *mut HewTaskScope) -> *mut HewTask {
    cabi_guard!(scope.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        if unsafe { (*cur).load_state() } == HewTaskState::Ready {
            return cur;
        }
        // SAFETY: cur is valid.
        cur = unsafe { (*cur).next };
    }
    ptr::null_mut()
}

/// Check if all tasks are complete.
///
/// Returns `1` if done, `0` otherwise.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_is_done(scope: *mut HewTaskScope) -> i32 {
    cabi_guard!(scope.is_null(), 1);
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    i32::from(s.completed_count >= s.task_count)
}

/// Cancel all non-terminal tasks in the scope.
///
/// Cancelled tasks transition to `Done` with error `Cancelled`.
/// Suspended waiters are woken so they can observe the cancellation.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_cancel(scope: *mut HewTaskScope) {
    cabi_guard!(scope.is_null());
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };
    s.cancelled.store(true, Ordering::Release);
    // SAFETY: `s.cancel_token` is valid for the scope lifetime.
    unsafe { hew_cancel_token_cancel(s.cancel_token, HewTaskError::Cancelled as i32) };

    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let t = unsafe { &mut *cur };
        let cur_state = t.load_state();
        if cur_state == HewTaskState::Ready || cur_state == HewTaskState::Suspended {
            t.run_cancel_cleanup();
            t.mark_done(HewTaskError::Cancelled);
            s.completed_count += 1;
        }
        cur = t.next;
    }
}

/// Schedule cancellation of `scope` after `duration_ns` nanoseconds.
///
/// The deadline handle is owned by the scope and cancelled/joined during
/// [`hew_task_scope_destroy`], so the timer thread cannot observe freed scope
/// memory if the scope exits before the deadline fires.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_cancel_after_ns(
    scope: *mut HewTaskScope,
    duration_ns: i64,
) {
    cabi_guard!(scope.is_null());
    let duration_ns = duration_ns.max(0).cast_unsigned();
    let cancelled = Arc::new(AtomicBool::new(false));
    let cancelled_for_thread = Arc::clone(&cancelled);
    let scope_addr = scope as usize;
    let duration = Duration::from_nanos(duration_ns);

    let handle = std::thread::spawn(move || {
        std::thread::park_timeout(duration);
        if !cancelled_for_thread.load(Ordering::Acquire) {
            let scope_ptr = scope_addr as *mut HewTaskScope;
            // SAFETY: the owning scope joins this deadline thread before freeing
            // the scope allocation. If cancellation won the race, the flag above
            // prevents dereferencing the pointer.
            unsafe { hew_task_scope_cancel(scope_ptr) };
        }
    });

    let deadline = Box::into_raw(Box::new(HewTaskScopeDeadline {
        cancelled,
        thread_handle: Some(handle),
        // SAFETY: caller guarantees scope is valid.
        next: unsafe { (*scope).deadlines },
    }));
    // SAFETY: caller guarantees scope is valid.
    unsafe {
        (*scope).deadlines = deadline;
    }
}

/// Cancel a single task within a scope.
///
/// # Contract
///
/// - `Ready` / `Suspended`: marks the task `Done(Cancelled)`, notifies the
///   `done_signal`, and bumps `scope.completed_count`. Returns 0.
/// - `Running`: notifies the `done_signal` only. The worker thread holds the
///   task's execution quantum; cooperative finish is required before the task
///   transitions to `Done`. Returns 0.
/// - `Done`: no-op. Returns 0.
/// - Null `scope` or null `task`: returns -1 (`cabi_guard!` sentinel).
///
/// Idempotent: double-cancel and cancel-of-Done both return 0.
///
/// Does NOT set `scope.cancelled` — that flag is scope-wide-cancel semantics
/// (`hew_task_scope_cancel`). Per-task cancel must not flip the scope flag.
///
/// There is no observable cooperative-cancel flag on `HewTask` today; the
/// `Running` branch notifies `done_signal` waiters so any parked waiter is woken
/// to re-check `Done` state.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
/// - `task` must be a valid pointer to a task spawned into that scope.
///
/// WASM-TODO(#1451): `task_scope` is native-only; this function is excluded
/// from the WASM build by the same module-level cfg guard as the rest of
/// `task_scope.rs`.
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_cancel_one(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
) -> i32 {
    cabi_guard!(scope.is_null() || task.is_null(), -1);
    // SAFETY:
    // - Provenance: `scope` was returned by a prior `hew_task_scope_new` FFI call.
    // - Type/tag: caller asserts via FFI contract that the pointer is a `HewTaskScope`.
    // - Lifetime/owner: caller owns the scope until `hew_task_scope_destroy`; it outlives this call.
    // - Aliasing/concurrency: `cabi_guard!` above rejects null; the scope is accessed from the
    //   single actor thread that owns it — no mutable aliases exist concurrently.
    // - Bounds: pointer is non-null (cabi_guard); struct size is known at compile time.
    // - Failure mode: `cabi_guard!` returns -1 on null before this point is reached.
    let s = unsafe { &mut *scope };
    // SAFETY:
    // - Provenance: `task` was returned by a prior `hew_task_new` FFI call and spawned into this scope.
    // - Type/tag: caller asserts via FFI contract that the pointer is a `HewTask`.
    // - Lifetime/owner: the owning scope keeps the task alive until `hew_task_scope_destroy`.
    // - Aliasing/concurrency: `cabi_guard!` above rejects null; the task state machine is
    //   accessed through atomic loads, so concurrent reads from worker threads are safe.
    // - Bounds: pointer is non-null (cabi_guard); struct size is known at compile time.
    // - Failure mode: `cabi_guard!` returns -1 on null before this point is reached.
    let t = unsafe { &mut *task };

    match t.load_state() {
        HewTaskState::Ready | HewTaskState::Suspended => {
            // SAFETY: task token, when present, is valid while the task lives.
            unsafe {
                cancel_token_cancel_if_present(t.cancel_token, HewTaskError::Cancelled as i32);
            };
            t.run_cancel_cleanup();
            t.mark_done(HewTaskError::Cancelled);
            s.completed_count += 1;
        }
        HewTaskState::Running => {
            // SAFETY: task token, when present, is valid while the task lives.
            unsafe {
                cancel_token_cancel_if_present(t.cancel_token, HewTaskError::Cancelled as i32);
            };
            // Best-effort cooperative cancel: wake any parked waiter so it
            // can observe the Done transition once the worker finishes its
            // quantum. Does not force completion.
            t.notify_done_waiters();
        }
        HewTaskState::Done => {
            // Already terminal; no-op.
        }
    }
    0
}

/// Mark a task as completed.
///
/// # Safety
///
/// - `scope` must be a valid pointer returned by [`hew_task_scope_new`].
/// - `task` must be a valid pointer to a task in the scope.
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_complete_task(
    scope: *mut HewTaskScope,
    task: *mut HewTask,
) {
    cabi_guard!(scope.is_null() || task.is_null());
    // SAFETY: Caller guarantees both pointers are valid.
    let s = unsafe { &mut *scope };
    // SAFETY: caller guarantees task is valid.
    let t = unsafe { &mut *task };

    if t.load_state() == HewTaskState::Done {
        return; // Already terminal.
    }

    t.mark_terminal_from_current_token(HewTaskError::None);
    s.completed_count += 1;
}

/// Get the Nth task by index (0-based).
///
/// Walks the linked list. Returns null if out of bounds.
///
/// # Panics
///
/// Panics if `index` is negative or greater than or equal to the scope's task count.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_get_task(
    scope: *mut HewTaskScope,
    index: i32,
) -> *mut HewTask {
    cabi_guard!(scope.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    assert!(
        index >= 0 && index < s.task_count,
        "task index {index} out of bounds (count = {})",
        s.task_count
    );

    let mut cur = s.tasks;
    for _ in 0..index {
        if cur.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: cur is valid.
        cur = unsafe { (*cur).next };
    }
    cur
}

/// Find the first completed task (for `wait_first` / select lowering).
///
/// Returns null if no task is done yet.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_wait_first(scope: *mut HewTaskScope) -> *mut HewTask {
    cabi_guard!(scope.is_null(), ptr::null_mut());
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        if unsafe { (*cur).load_state() } == HewTaskState::Done {
            return cur;
        }
        // SAFETY: cur is valid.
        cur = unsafe { (*cur).next };
    }
    ptr::null_mut()
}

/// Check whether the scope has any active (non-terminal) tasks.
///
/// Returns `1` if active tasks exist, `0` otherwise.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_has_active_tasks(scope: *mut HewTaskScope) -> i32 {
    cabi_guard!(scope.is_null(), 0);
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let state = unsafe { (*cur).load_state() };
        if state == HewTaskState::Ready
            || state == HewTaskState::Running
            || state == HewTaskState::Suspended
        {
            return 1;
        }
        // SAFETY: cur is valid.
        cur = unsafe { (*cur).next };
    }
    0
}

/// Destroy the scope, freeing all tasks.
///
/// # Safety
///
/// `scope` must have been returned by [`hew_task_scope_new`] and must
/// not be used after this call.
///
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_destroy(scope: *mut HewTaskScope) {
    cabi_guard!(scope.is_null());
    // Join all worker threads before freeing tasks to avoid UAF.
    // SAFETY: Caller guarantees `scope` is valid.
    unsafe { hew_task_scope_join_all(scope) };
    // SAFETY: Caller guarantees `scope` was Box-allocated.
    let mut scope_box = unsafe { Box::from_raw(scope) };
    // SAFETY: deadline nodes are owned exclusively by this scope.
    unsafe { cancel_scope_deadlines(&mut scope_box) };
    let detached_handles = take_detached_task_handles(&mut scope_box);
    if detached_handles.is_empty() {
        // SAFETY: join_all already drained every worker that could still touch
        // the tasks in this scope.
        unsafe { free_scope_tasks(&mut scope_box) };
        return;
    }

    // WASM-TODO(#1451): task_scope uses OS threads throughout and has no WASM target;
    // the reaper thread below is likewise native-only.
    #[cfg(test)]
    if should_fail_task_reaper_spawn() {
        crate::set_last_error(
            "hew_task_scope_destroy: failed to spawn hew-task-reaper thread; running cleanup synchronously",
        );
        reap_detached_scope_tasks(scope_box, detached_handles);
        return;
    }

    let reaper_state = Arc::new(Mutex::new(Some((scope_box, detached_handles))));
    let background_state = Arc::clone(&reaper_state);
    if let Ok(reaper) = std::thread::Builder::new()
        .name("hew-task-reaper".into())
        .spawn(move || {
            if let Some((scope_box, detached_handles)) = take_reaper_state(&background_state) {
                reap_detached_scope_tasks(scope_box, detached_handles);
            }
        })
    {
        drop(reaper);
    } else {
        crate::set_last_error(
            "hew_task_scope_destroy: failed to spawn hew-task-reaper thread; running cleanup synchronously",
        );
        if let Some((scope_box, detached_handles)) = take_reaper_state(&reaper_state) {
            reap_detached_scope_tasks(scope_box, detached_handles);
        }
    }
}

#[cfg(test)]
thread_local! {
    static FAIL_TASK_REAPER_SPAWN: Cell<bool> = const { Cell::new(false) };
}

#[cfg(test)]
fn should_fail_task_reaper_spawn() -> bool {
    FAIL_TASK_REAPER_SPAWN.with(Cell::get)
}

// ── Tests ──────────────────────────────────────────────────────────────

/// Test-only: captures the message passed to `set_last_error` when
/// `cancel_cleanup_fn` panics. Written by `run_cancel_cleanup` on panic;
/// read by `cancel_cleanup_fn_panic_does_not_abort_process`.
#[cfg(test)]
static LAST_CANCEL_CLEANUP_PANIC_MSG: std::sync::Mutex<Option<String>> =
    std::sync::Mutex::new(None);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution_context::{HewExecutionContext, TestExecutionContext};
    use std::ffi::CStr;

    #[test]
    fn task_lifecycle() {
        // SAFETY: test owns all task pointers exclusively; all are valid.
        unsafe {
            let t = hew_task_new();
            assert!(!t.is_null());
            assert_eq!((*t).load_state(), HewTaskState::Ready);
            assert_eq!((*t).error, HewTaskError::None);

            let val: i32 = 42;
            hew_task_set_result(t, (&raw const val).cast_mut().cast(), size_of::<i32>());
            (*t).store_state(HewTaskState::Done, Ordering::Release);
            let result = hew_task_get_result(t);
            assert!(!result.is_null());
            assert_eq!(*(result.cast::<i32>()), 42);

            hew_task_free(t);
        }
    }

    #[test]
    fn scope_spawn_and_complete() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            let t2 = hew_task_new();

            hew_task_scope_spawn(scope, t1);
            hew_task_scope_spawn(scope, t2);
            assert_eq!(hew_task_scope_is_done(scope), 0);

            hew_task_scope_complete_task(scope, t1);
            assert_eq!(hew_task_scope_is_done(scope), 0);

            hew_task_scope_complete_task(scope, t2);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_completion_observe_wins_arbiter_on_last_child() {
        use crate::await_cancel::{
            hew_await_cancel_free, hew_await_cancel_new, hew_await_cancel_status, AwaitCancelStatus,
        };
        // The wait-ALL join arm of the SuspendingScopeDeadline arbiter must win
        // the shared one-shot CAS only when the LAST child completes — not the
        // first. `actor` is null so no wake is enqueued; the CAS transition is the
        // observable outcome.
        // SAFETY: the test owns every scope/task/arbiter pointer exclusively.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            let t2 = hew_task_new();
            hew_task_scope_spawn(scope, t1);
            hew_task_scope_spawn(scope, t2);

            let reg = hew_await_cancel_new(ptr::null_mut(), None, ptr::null_mut());
            // Two outstanding children → the join arm parks (SCOPE_JOIN_SUSPEND).
            assert_eq!(
                hew_task_scope_completion_observe(scope, reg, ptr::null_mut()),
                SCOPE_JOIN_SUSPEND
            );
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::Pending as i32,
                "the arbiter must stay Pending while any child is outstanding"
            );

            // First child done → still one outstanding → arbiter Pending.
            hew_task_scope_complete_task(scope, t1);
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::Pending as i32,
                "the join must not win on the first of two children"
            );

            // Last child done → the join wins the one-shot → Completed.
            hew_task_scope_complete_task(scope, t2);
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::Completed as i32,
                "the join must win the arbiter when the last child completes"
            );

            // The caller's creator ref + clean teardown: no double-free / leak.
            hew_await_cancel_free(reg);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_completion_observe_ready_when_all_children_already_done() {
        use crate::await_cancel::{
            hew_await_cancel_free, hew_await_cancel_new, hew_await_cancel_status, AwaitCancelStatus,
        };
        // When every child is already Done at registration time, the join arm
        // completes the arbiter synchronously and reports SCOPE_JOIN_READY so the
        // caller binds the scope-complete edge without suspending.
        // SAFETY: the test owns every scope/task/arbiter pointer exclusively.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            hew_task_scope_spawn(scope, t1);
            hew_task_scope_complete_task(scope, t1);

            let reg = hew_await_cancel_new(ptr::null_mut(), None, ptr::null_mut());
            assert_eq!(
                hew_task_scope_completion_observe(scope, reg, ptr::null_mut()),
                SCOPE_JOIN_READY,
                "an all-done scope must report READY (bind immediately)"
            );
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::Completed as i32,
                "the synchronous-ready path must complete the arbiter"
            );

            hew_await_cancel_free(reg);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_completion_observe_deadline_wins_first_blocks_the_join() {
        use crate::await_cancel::{
            hew_await_cancel_cancel, hew_await_cancel_free, hew_await_cancel_new,
            hew_await_cancel_status, AwaitCancelStatus,
        };
        // The first-ready CAS makes deadline-vs-completion mutually exclusive:
        // when the deadline arm wins first (here forced via an explicit TimedOut
        // cancel), the later child-completion observers must NOT re-win the
        // arbiter — its status stays TimedOut and the join's complete is a no-op.
        // SAFETY: the test owns every scope/task/arbiter pointer exclusively.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            hew_task_scope_spawn(scope, t1);

            let reg = hew_await_cancel_new(ptr::null_mut(), None, ptr::null_mut());
            assert_eq!(
                hew_task_scope_completion_observe(scope, reg, ptr::null_mut()),
                SCOPE_JOIN_SUSPEND
            );

            // The deadline arm wins first (TimedOut, no wake — null actor).
            assert_eq!(
                hew_await_cancel_cancel(reg, AwaitCancelStatus::TimedOut as i32, 0),
                1,
                "the deadline arm must win the one-shot CAS"
            );
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::TimedOut as i32
            );

            // The child completes AFTER the deadline won: the join observer fires
            // but its hew_await_cancel_complete loses the CAS — status unchanged.
            hew_task_scope_complete_task(scope, t1);
            assert_eq!(
                hew_await_cancel_status(reg),
                AwaitCancelStatus::TimedOut as i32,
                "a late child completion must not re-win after the deadline"
            );

            hew_await_cancel_free(reg);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_cancel() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let t = hew_task_new();
            hew_task_scope_spawn(scope, t);

            hew_task_scope_cancel(scope);
            assert_eq!((*t).load_state(), HewTaskState::Done);
            assert_eq!((*t).error, HewTaskError::Cancelled);
            assert_eq!(hew_task_is_cancelled(t), 1);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancellation_token_scope_cancel_marks_ready_and_suspended() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let ready = hew_task_new();
            let suspended = hew_task_new();
            hew_task_scope_spawn(scope, ready);
            hew_task_scope_spawn(scope, suspended);
            (*suspended).store_state(HewTaskState::Suspended, Ordering::Release);

            assert_eq!(
                hew_cancel_token_is_requested(hew_task_scope_cancel_token(scope)),
                0
            );
            assert_eq!(hew_cancel_token_is_requested((*ready).cancel_token), 0);
            assert_eq!(hew_cancel_token_is_requested((*suspended).cancel_token), 0);

            hew_task_scope_cancel(scope);

            assert_eq!(hew_task_scope_is_cancelled(scope), 1);
            assert_eq!(
                hew_cancel_token_is_requested(hew_task_scope_cancel_token(scope)),
                1
            );
            assert_eq!(hew_cancel_token_is_requested((*ready).cancel_token), 1);
            assert_eq!(hew_cancel_token_is_requested((*suspended).cancel_token), 1);
            assert_eq!((*ready).load_state(), HewTaskState::Done);
            assert_eq!((*ready).error, HewTaskError::Cancelled);
            assert_eq!((*suspended).load_state(), HewTaskState::Done);
            assert_eq!((*suspended).error, HewTaskError::Cancelled);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancellation_token_transitive_child_observes_parent() {
        // SAFETY: test owns all token pointers exclusively; all are valid.
        unsafe {
            let parent = hew_cancel_token_new_child(ptr::null_mut());
            let child = hew_cancel_token_new_child(parent);

            assert_eq!(hew_cancel_token_is_requested(parent), 0);
            assert_eq!(hew_cancel_token_is_requested(child), 0);

            hew_cancel_token_cancel(parent, HewTaskError::Cancelled as i32);

            assert_eq!(hew_cancel_token_is_requested(parent), 1);
            assert_eq!(hew_cancel_token_is_requested(child), 1);

            hew_cancel_token_release(child);
            hew_cancel_token_release(parent);
        }
    }

    #[test]
    fn cancellation_token_parent_cancel_reaches_child() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            hew_cancel_token_cancel(
                hew_task_scope_cancel_token(scope),
                HewTaskError::Cancelled as i32,
            );

            assert_eq!(hew_cancel_token_is_requested((*task).cancel_token), 1);
            hew_task_scope_complete_task(scope, task);
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!(
                (*task).error,
                HewTaskError::Cancelled,
                "completion after parent cancellation must not look successful"
            );

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cooperate_observes_current_task_scope_cancel_without_actor() {
        // SAFETY: test owns the scope pointer and restores the context before destroy.
        unsafe {
            let _ctx = TestExecutionContext::install(HewExecutionContext::default());
            let scope = hew_task_scope_new();
            let previous = hew_task_scope_set_current(scope);
            hew_task_scope_cancel(scope);

            assert_eq!(crate::scheduler::hew_actor_cooperate(), 2);

            let _ = hew_task_scope_set_current(previous);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cooperate_without_execution_context_fails_closed() {
        crate::hew_clear_error();
        assert_eq!(crate::scheduler::hew_actor_cooperate(), 0);
        let err = crate::hew_last_error();
        assert!(!err.is_null());
        // SAFETY: hew_last_error returned a non-null C string.
        let err = unsafe { CStr::from_ptr(err).to_str().unwrap() };
        assert_eq!(
            err,
            crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED
        );
        crate::hew_clear_error();
    }

    #[test]
    fn spawned_thread_inherits_scope_and_observes_cancel() {
        use std::sync::atomic::AtomicBool;
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static OBSERVED_CANCEL: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn cooperative_task(task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            while crate::scheduler::hew_actor_cooperate() != 2 {
                std::thread::sleep(Duration::from_millis(1));
            }
            OBSERVED_CANCEL.store(true, Ordering::SeqCst);
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        STARTED.store(false, Ordering::SeqCst);
        OBSERVED_CANCEL.store(false, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, cooperative_task);

            let deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < deadline {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert!(STARTED.load(Ordering::SeqCst), "worker did not start");

            hew_task_scope_cancel(scope);
            let _ = hew_task_await_blocking(task);

            assert!(OBSERVED_CANCEL.load(Ordering::SeqCst));
            assert_eq!((*task).error, HewTaskError::Cancelled);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn spawned_thread_with_inherited_context_observes_parent_cancel() {
        use std::sync::atomic::AtomicBool;
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static OBSERVED_CANCEL: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn cooperative_task(ctx: *mut HewExecutionContext, task: *mut HewTask) {
            assert_eq!(ctx, crate::execution_context::current_context());
            STARTED.store(true, Ordering::SeqCst);
            while crate::scheduler::hew_actor_cooperate() != 2 {
                std::thread::sleep(Duration::from_millis(1));
            }
            OBSERVED_CANCEL.store(true, Ordering::SeqCst);
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        STARTED.store(false, Ordering::SeqCst);
        OBSERVED_CANCEL.store(false, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let _ctx = TestExecutionContext::install(HewExecutionContext::default());
            let scope = hew_task_scope_new();
            let previous = hew_task_scope_set_current(scope);
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            assert_eq!(
                hew_task_spawn_thread_with_inherited_context(
                    crate::execution_context::current_context(),
                    task,
                    cooperative_task,
                ),
                0
            );

            let deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < deadline {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert!(STARTED.load(Ordering::SeqCst), "worker did not start");

            hew_task_scope_cancel(scope);
            let _ = hew_task_await_blocking(task);

            assert!(OBSERVED_CANCEL.load(Ordering::SeqCst));
            assert_eq!((*task).error, HewTaskError::Cancelled);
            let _ = hew_task_scope_set_current(previous);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    #[allow(
        clippy::too_many_lines,
        reason = "The snapshot test pins every inherited and non-inherited context lane together."
    )]
    fn inherited_context_snapshots_supervisor_lineage_and_trace() {
        use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, AtomicUsize};
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static RELEASE: AtomicBool = AtomicBool::new(false);
        static SEEN_PARENT_SUPERVISOR: AtomicUsize = AtomicUsize::new(0);
        static SEEN_CHILD_INDEX: AtomicI32 = AtomicI32::new(0);
        static SEEN_TRACE_ID_HI: AtomicU64 = AtomicU64::new(0);
        static SEEN_TRACE_ID_LO: AtomicU64 = AtomicU64::new(0);
        static SEEN_SPAN_ID: AtomicU64 = AtomicU64::new(0);
        static SEEN_PARENT_SPAN_ID: AtomicU64 = AtomicU64::new(0);
        static SEEN_ACTOR: AtomicUsize = AtomicUsize::new(usize::MAX);
        static SEEN_ACTOR_ID: AtomicU64 = AtomicU64::new(u64::MAX);
        static SEEN_ARENA: AtomicUsize = AtomicUsize::new(usize::MAX);
        static SEEN_LOCK_SEAT: AtomicUsize = AtomicUsize::new(usize::MAX);

        unsafe extern "C" fn record_context(ctx: *mut HewExecutionContext, task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert_eq!(ctx, crate::execution_context::current_context());
            // SAFETY: ctx is the live child context installed by the spawn helper.
            let ctx_ref = unsafe { &*ctx };
            SEEN_PARENT_SUPERVISOR.store(ctx_ref.parent_supervisor as usize, Ordering::SeqCst);
            SEEN_CHILD_INDEX.store(ctx_ref.supervisor_child_index, Ordering::SeqCst);
            SEEN_TRACE_ID_HI.store(ctx_ref.trace.trace_id_hi, Ordering::SeqCst);
            SEEN_TRACE_ID_LO.store(ctx_ref.trace.trace_id_lo, Ordering::SeqCst);
            SEEN_SPAN_ID.store(ctx_ref.trace.span_id, Ordering::SeqCst);
            SEEN_PARENT_SPAN_ID.store(ctx_ref.trace.parent_span_id, Ordering::SeqCst);
            SEEN_ACTOR.store(ctx_ref.actor as usize, Ordering::SeqCst);
            SEEN_ACTOR_ID.store(ctx_ref.actor_id, Ordering::SeqCst);
            SEEN_ARENA.store(ctx_ref.arena as usize, Ordering::SeqCst);
            SEEN_LOCK_SEAT.store(ctx_ref.lock_seat as usize, Ordering::SeqCst);
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        let parent_supervisor = 0x1234usize as *mut c_void;
        let mutated_supervisor = 0x5678usize as *mut c_void;
        let trace = crate::tracing::HewTraceContext {
            trace_id_hi: 11,
            trace_id_lo: 22,
            span_id: 33,
            parent_span_id: 44,
            flags: 1,
        };
        let mutated_trace = crate::tracing::HewTraceContext {
            trace_id_hi: 111,
            trace_id_lo: 222,
            span_id: 333,
            parent_span_id: 444,
            flags: 0,
        };

        STARTED.store(false, Ordering::SeqCst);
        RELEASE.store(false, Ordering::SeqCst);
        SEEN_PARENT_SUPERVISOR.store(0, Ordering::SeqCst);
        SEEN_CHILD_INDEX.store(0, Ordering::SeqCst);
        SEEN_TRACE_ID_HI.store(0, Ordering::SeqCst);
        SEEN_TRACE_ID_LO.store(0, Ordering::SeqCst);
        SEEN_SPAN_ID.store(0, Ordering::SeqCst);
        SEEN_PARENT_SPAN_ID.store(0, Ordering::SeqCst);
        SEEN_ACTOR.store(usize::MAX, Ordering::SeqCst);
        SEEN_ACTOR_ID.store(u64::MAX, Ordering::SeqCst);
        SEEN_ARENA.store(usize::MAX, Ordering::SeqCst);
        SEEN_LOCK_SEAT.store(usize::MAX, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let _ctx = TestExecutionContext::install(HewExecutionContext {
                parent_supervisor,
                supervisor_child_index: 7,
                trace,
                ..HewExecutionContext::default()
            });
            let parent_ctx = crate::execution_context::current_context();
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            assert_eq!(
                hew_task_spawn_thread_with_inherited_context(parent_ctx, task, record_context),
                0
            );

            let deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < deadline {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert!(STARTED.load(Ordering::SeqCst), "worker did not start");

            (*parent_ctx).parent_supervisor = mutated_supervisor;
            (*parent_ctx).supervisor_child_index = 99;
            (*parent_ctx).trace = mutated_trace;
            RELEASE.store(true, Ordering::SeqCst);
            let _ = hew_task_await_blocking(task);

            assert_eq!(
                SEEN_PARENT_SUPERVISOR.load(Ordering::SeqCst),
                parent_supervisor as usize
            );
            assert_eq!(SEEN_CHILD_INDEX.load(Ordering::SeqCst), 7);
            assert_eq!(SEEN_TRACE_ID_HI.load(Ordering::SeqCst), trace.trace_id_hi);
            assert_eq!(SEEN_TRACE_ID_LO.load(Ordering::SeqCst), trace.trace_id_lo);
            assert_eq!(SEEN_SPAN_ID.load(Ordering::SeqCst), trace.span_id);
            assert_eq!(
                SEEN_PARENT_SPAN_ID.load(Ordering::SeqCst),
                trace.parent_span_id
            );
            assert_eq!(SEEN_ACTOR.load(Ordering::SeqCst), 0);
            assert_eq!(SEEN_ACTOR_ID.load(Ordering::SeqCst), 0);
            assert_eq!(SEEN_ARENA.load(Ordering::SeqCst), 0);
            assert_eq!(SEEN_LOCK_SEAT.load(Ordering::SeqCst), 0);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn inherited_context_spawn_with_null_parent_fails_closed() {
        unsafe extern "C" fn unreachable_task(_: *mut HewExecutionContext, _: *mut HewTask) {
            panic!("null parent context must reject before spawning");
        }

        crate::hew_clear_error();
        // SAFETY: test owns the task pointer exclusively.
        unsafe {
            let task = hew_task_new();
            assert_eq!(
                hew_task_spawn_thread_with_inherited_context(
                    ptr::null_mut(),
                    task,
                    unreachable_task,
                ),
                -1
            );
            let err = crate::hew_last_error();
            assert!(!err.is_null());
            // SAFETY: hew_last_error returned a non-null C string.
            let err = CStr::from_ptr(err).to_str().unwrap();
            assert_eq!(
                err,
                crate::execution_context::EXECUTION_CONTEXT_NOT_INSTALLED_AT_SPAWN
            );
            crate::hew_clear_error();
            hew_task_free(task);
        }
    }

    #[test]
    fn deadline_cancels_scope_and_destroy_cancels_pending_deadline() {
        use std::time::{Duration, Instant};

        // SAFETY: test owns the scope pointer exclusively.
        unsafe {
            let scope = hew_task_scope_new();
            hew_task_scope_cancel_after_ns(scope, 1_000_000);
            let deadline = Instant::now() + Duration::from_secs(1);
            while hew_task_scope_is_cancelled(scope) == 0 && Instant::now() < deadline {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert_eq!(hew_task_scope_is_cancelled(scope), 1);
            hew_task_scope_destroy(scope);

            let scope = hew_task_scope_new();
            hew_task_scope_cancel_after_ns(scope, 1_000_000_000);
            let destroy_started = Instant::now();
            hew_task_scope_destroy(scope);
            assert!(
                destroy_started.elapsed() < Duration::from_millis(250),
                "destroy must cancel and unpark pending deadline threads"
            );
        }
    }

    #[test]
    fn scope_cancel_notifies_done_signal_for_non_running_tasks() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            let signal = Arc::new(TaskDoneSignal::new());
            (*task).done_signal = Some(Arc::clone(&signal));
            hew_task_scope_spawn(scope, task);

            hew_task_scope_cancel(scope);

            signal.wait_until_done();
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!((*task).error, HewTaskError::Cancelled);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_cancel_of_ready_task_reclaims_env_ptr_but_runs_no_user_drop() {
        // Pin the edition-2026 substrate behaviour for cancelling a Ready
        // (never-spawned) task. The intra-process expectation is:
        //
        // 1. Cancellation transitions the task to Done with error Cancelled.
        // 2. hew_task_scope_destroy reclaims the task's env_ptr through the
        //    declared Rc drop fn — the captured environment allocation is
        //    not leaked.
        //
        // What this test deliberately does NOT assert: that user-level
        // @resource close() or @linear consume calls run on the cancel
        // path. Those calls are emitted inside the task_fn body that the
        // runtime never invokes for a Ready-cancelled task. Edition 2026
        // therefore rejects @linear captures into a child whose
        // cancel-reachable exits don't pass through a consume site (see
        // LinearCaptureCancellable, HEW-SPEC-2026 §4.3); @resource close
        // semantics rely on the task body reaching its drop elaboration.
        // When cancel_cleanup_fn IS registered (see the companion tests
        // below), cleanup fires at cancel time. When it is NOT registered
        // (this test), only the Rc deallocation fires at destroy time.
        use std::sync::atomic::AtomicUsize;

        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);
        unsafe extern "C" fn count_env_drop(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_scope_spawn(scope, task);

            // Ready-cancel transitions the task to Done with Cancelled. No
            // worker thread was spawned, so no user-level task_fn body ran.
            hew_task_scope_cancel(scope);
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!((*task).error, HewTaskError::Cancelled);
            assert_eq!(
                ENV_DROPS.load(Ordering::SeqCst),
                0,
                "env_ptr drop must not fire while the task pointer is still owned by the scope"
            );

            // hew_task_scope_destroy walks the task list and frees each
            // task; hew_task_free invokes hew_rc_drop on a non-null
            // env_ptr, which calls the registered count_env_drop hook.
            hew_task_scope_destroy(scope);
            assert_eq!(
                ENV_DROPS.load(Ordering::SeqCst),
                1,
                "env_ptr drop must fire exactly once when the scope reclaims the cancelled task"
            );
        }
    }

    /// Regression test: when `cancel_cleanup_fn` is registered, scope-wide
    /// cancel fires it for every Ready and Suspended task before transitioning
    /// them to `Done(Cancelled)`. This exercises the resource-reclamation path
    /// described in the `cleanup-all-exits` LESSONS row.
    ///
    /// Shape: each `cleanup_fn` simulates dropping a Vec by incrementing a static
    /// counter, matching the "assert `Vec::drop` ran" contract in the brief.
    #[test]
    fn scope_cancel_fires_cancel_cleanup_fn_for_ready_and_suspended_tasks() {
        use std::sync::atomic::AtomicUsize;

        static CLEANUPS: AtomicUsize = AtomicUsize::new(0);
        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);

        /// Simulates the compiler-generated cancel-cleanup: drops a
        /// heap-allocated resource (represented here as a counter bump).
        unsafe extern "C-unwind" fn resource_cleanup(_env: *mut c_void) {
            CLEANUPS.fetch_add(1, Ordering::SeqCst);
        }

        unsafe extern "C" fn count_env_drop(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        CLEANUPS.store(0, Ordering::SeqCst);
        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();

            // Task 1: Ready (never dispatched to a thread).
            let t_ready = hew_task_new();
            hew_task_set_env(
                t_ready,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_set_cancel_cleanup_fn(t_ready, Some(resource_cleanup));
            hew_task_scope_spawn(scope, t_ready);

            // Task 2: Suspended (simulates a cooperatively-suspended task).
            let t_suspended = hew_task_new();
            hew_task_set_env(
                t_suspended,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_set_cancel_cleanup_fn(t_suspended, Some(resource_cleanup));
            hew_task_scope_spawn(scope, t_suspended);
            (*t_suspended).store_state(HewTaskState::Suspended, Ordering::Release);

            // Scope cancel must call cleanup before marking tasks Done.
            hew_task_scope_cancel(scope);

            assert_eq!(
                CLEANUPS.load(Ordering::SeqCst),
                2,
                "cancel_cleanup_fn must fire once per Ready/Suspended task on scope cancel"
            );
            assert_eq!((*t_ready).load_state(), HewTaskState::Done);
            assert_eq!((*t_ready).error, HewTaskError::Cancelled);
            assert_eq!((*t_suspended).load_state(), HewTaskState::Done);
            assert_eq!((*t_suspended).error, HewTaskError::Cancelled);

            // env_ptr Rc drop fires at destroy time (not at cancel time).
            assert_eq!(ENV_DROPS.load(Ordering::SeqCst), 0);
            hew_task_scope_destroy(scope);
            assert_eq!(
                ENV_DROPS.load(Ordering::SeqCst),
                2,
                "env_ptr must be reclaimed for both tasks at destroy time"
            );
        }
    }

    /// `cancel_cleanup_fn` fires on per-task cancel via `cancel_one`.
    #[test]
    fn cancel_one_fires_cancel_cleanup_fn_for_ready_task() {
        use std::sync::atomic::AtomicUsize;

        static CLEANUPS: AtomicUsize = AtomicUsize::new(0);
        unsafe extern "C-unwind" fn resource_cleanup_one(_env: *mut c_void) {
            CLEANUPS.fetch_add(1, Ordering::SeqCst);
        }
        unsafe extern "C" fn noop_drop(_: *mut u8) {}

        CLEANUPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_cancel_cleanup_fn(task, Some(resource_cleanup_one));
            // env_ptr with a non-null pointer so cleanup_fn is invoked.
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(noop_drop)).cast(),
            );
            hew_task_scope_spawn(scope, task);

            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(ret, 0);
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!((*task).error, HewTaskError::Cancelled);
            assert_eq!(
                CLEANUPS.load(Ordering::SeqCst),
                1,
                "cancel_cleanup_fn must fire on cancel_one of a Ready task"
            );

            hew_task_scope_destroy(scope);
        }
    }

    /// Double-cancel must not invoke `cancel_cleanup_fn` twice (idempotence).
    #[test]
    fn cancel_cleanup_fn_fires_at_most_once_on_double_cancel() {
        use std::sync::atomic::AtomicUsize;

        static CLEANUPS: AtomicUsize = AtomicUsize::new(0);
        unsafe extern "C-unwind" fn resource_cleanup_double(_env: *mut c_void) {
            CLEANUPS.fetch_add(1, Ordering::SeqCst);
        }
        unsafe extern "C" fn noop_drop2(_: *mut u8) {}

        CLEANUPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_cancel_cleanup_fn(task, Some(resource_cleanup_double));
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(noop_drop2)).cast(),
            );
            hew_task_scope_spawn(scope, task);

            // First cancel: fires cleanup.
            hew_task_scope_cancel(scope);
            assert_eq!(CLEANUPS.load(Ordering::SeqCst), 1);

            // Second cancel: task is already Done — cleanup must not fire again.
            hew_task_scope_cancel(scope);
            assert_eq!(
                CLEANUPS.load(Ordering::SeqCst),
                1,
                "cancel_cleanup_fn must not fire on double-cancel"
            );

            hew_task_scope_destroy(scope);
        }
    }

    /// Regression: a panicking `cancel_cleanup_fn` must not abort the process.
    ///
    /// The `catch_unwind` in `run_cancel_cleanup` must:
    /// - Survive the panic (test completes = process did not abort).
    /// - Log the panic message via `set_last_error` (readable via
    ///   `LAST_CANCEL_CLEANUP_PANIC_MSG`).
    /// - Still transition the task to `Done(Cancelled)` — no state skip.
    #[test]
    fn cancel_cleanup_fn_panic_does_not_abort_process() {
        use std::sync::atomic::AtomicUsize;

        static SCOPE_CANCEL_CALLS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C-unwind" fn panicking_cleanup(_env: *mut c_void) {
            SCOPE_CANCEL_CALLS.fetch_add(1, Ordering::SeqCst);
            panic!("cancel_cleanup intentional panic");
        }
        unsafe extern "C" fn noop_drop_panic(_: *mut u8) {}

        // Reset the shared panic-message capture.
        *LAST_CANCEL_CLEANUP_PANIC_MSG
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = None;
        SCOPE_CANCEL_CALLS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_cancel_cleanup_fn(task, Some(panicking_cleanup));
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(noop_drop_panic)).cast(),
            );
            hew_task_scope_spawn(scope, task);

            // This must not abort the process despite the panicking callback.
            hew_task_scope_cancel(scope);

            // The callback ran (exactly once).
            assert_eq!(
                SCOPE_CANCEL_CALLS.load(Ordering::SeqCst),
                1,
                "panicking cancel_cleanup_fn must still be invoked"
            );

            // Task must have transitioned to Done(Cancelled) — no state skip.
            assert_eq!(
                (*task).load_state(),
                HewTaskState::Done,
                "task must be Done(Cancelled) even when cancel_cleanup_fn panicked"
            );
            assert_eq!((*task).error, HewTaskError::Cancelled);

            hew_task_scope_destroy(scope);
        }

        // Panic message must be captured in the test-only static.
        let captured = LAST_CANCEL_CLEANUP_PANIC_MSG
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let msg = captured.expect("set_last_error must have been called on panic");
        assert!(
            msg.contains("task_scope cancel_cleanup_fn panicked:"),
            "error message must carry the runtime prefix; got: {msg:?}"
        );
        assert!(
            msg.contains("cancel_cleanup intentional panic"),
            "error message must contain the original panic payload; got: {msg:?}"
        );
    }

    #[test]
    fn scope_destroy_after_cancel_stays_bounded_with_live_running_task() {
        use std::sync::atomic::AtomicBool;
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static RELEASE: AtomicBool = AtomicBool::new(false);
        static EXITED: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn blocking_task(task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            EXITED.store(true, Ordering::SeqCst);
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        STARTED.store(false, Ordering::SeqCst);
        RELEASE.store(false, Ordering::SeqCst);
        EXITED.store(false, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, blocking_task);

            let started_deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert!(
                STARTED.load(Ordering::SeqCst),
                "worker thread never started"
            );

            hew_task_scope_cancel(scope);
            assert_eq!((*task).load_state(), HewTaskState::Running);
            assert_eq!(hew_task_scope_has_active_tasks(scope), 1);

            let destroy_started = Instant::now();
            hew_task_scope_destroy(scope);
            assert!(
                destroy_started.elapsed() < Duration::from_millis(250),
                "destroy blocked on a cancelled running task"
            );
        }

        RELEASE.store(true, Ordering::SeqCst);
        let exit_deadline = Instant::now() + Duration::from_secs(1);
        while !EXITED.load(Ordering::SeqCst) && Instant::now() < exit_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert!(
            EXITED.load(Ordering::SeqCst),
            "detached worker never exited"
        );
    }

    #[test]
    fn scope_destroy_after_cancel_reclaims_tasks_once_detached_worker_exits() {
        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static RELEASE: AtomicBool = AtomicBool::new(false);
        static EXITED: AtomicBool = AtomicBool::new(false);
        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn blocking_task(task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            EXITED.store(true, Ordering::SeqCst);
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        unsafe extern "C" fn count_env_drop(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        STARTED.store(false, Ordering::SeqCst);
        RELEASE.store(false, Ordering::SeqCst);
        EXITED.store(false, Ordering::SeqCst);
        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let running_task = hew_task_new();
            let cancelled_task = hew_task_new();

            hew_task_set_env(
                running_task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_set_env(
                cancelled_task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );

            hew_task_scope_spawn(scope, running_task);
            hew_task_scope_spawn(scope, cancelled_task);
            hew_task_spawn_thread(running_task, blocking_task);

            let started_deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert!(
                STARTED.load(Ordering::SeqCst),
                "worker thread never started"
            );

            hew_task_scope_cancel(scope);
            assert_eq!((*running_task).load_state(), HewTaskState::Running);
            assert_eq!((*cancelled_task).load_state(), HewTaskState::Done);

            let destroy_started = Instant::now();
            hew_task_scope_destroy(scope);
            assert!(
                destroy_started.elapsed() < Duration::from_millis(250),
                "destroy blocked on a cancelled running task"
            );
        }

        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            0,
            "destroy reclaimed tasks while the detached worker was still live"
        );

        RELEASE.store(true, Ordering::SeqCst);

        let exit_deadline = Instant::now() + Duration::from_secs(1);
        while !EXITED.load(Ordering::SeqCst) && Instant::now() < exit_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert!(
            EXITED.load(Ordering::SeqCst),
            "detached worker never exited"
        );

        let cleanup_deadline = Instant::now() + Duration::from_secs(1);
        while ENV_DROPS.load(Ordering::SeqCst) != 2 && Instant::now() < cleanup_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            2,
            "task cleanup never ran after the detached worker exited"
        );
    }

    #[test]
    fn scope_destroy_after_cancel_stays_bounded_when_worker_marked_done_but_not_exited() {
        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static COMPLETED: AtomicBool = AtomicBool::new(false);
        static RELEASE_EXIT: AtomicBool = AtomicBool::new(false);
        static EXITED: AtomicBool = AtomicBool::new(false);
        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn done_then_linger(task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            // SAFETY: `task` remains owned by the worker until it exits.
            unsafe { hew_task_complete_threaded(task) };
            COMPLETED.store(true, Ordering::SeqCst);
            while !RELEASE_EXIT.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            EXITED.store(true, Ordering::SeqCst);
        }

        unsafe extern "C" fn count_env_drop(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        STARTED.store(false, Ordering::SeqCst);
        COMPLETED.store(false, Ordering::SeqCst);
        RELEASE_EXIT.store(false, Ordering::SeqCst);
        EXITED.store(false, Ordering::SeqCst);
        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, done_then_linger);

            let started_deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert!(
                STARTED.load(Ordering::SeqCst),
                "worker thread never started"
            );

            let completed_deadline = Instant::now() + Duration::from_secs(1);
            while !COMPLETED.load(Ordering::SeqCst) && Instant::now() < completed_deadline {
                std::thread::yield_now();
            }
            assert!(
                COMPLETED.load(Ordering::SeqCst),
                "worker thread never published completion"
            );
            assert_eq!((*task).load_state(), HewTaskState::Done);

            hew_task_scope_cancel(scope);

            let destroy_started = Instant::now();
            hew_task_scope_destroy(scope);
            assert!(
                destroy_started.elapsed() < Duration::from_millis(250),
                "destroy blocked on a cancelled worker tail after Done"
            );
        }

        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            0,
            "destroy reclaimed the task before the worker actually exited"
        );

        RELEASE_EXIT.store(true, Ordering::SeqCst);

        let exit_deadline = Instant::now() + Duration::from_secs(1);
        while !EXITED.load(Ordering::SeqCst) && Instant::now() < exit_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert!(EXITED.load(Ordering::SeqCst), "worker tail never exited");

        let cleanup_deadline = Instant::now() + Duration::from_secs(1);
        while ENV_DROPS.load(Ordering::SeqCst) != 1 && Instant::now() < cleanup_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            1,
            "task cleanup never ran after the done-but-live worker exited"
        );
    }

    #[test]
    fn scope_poll() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let t = hew_task_new();
            hew_task_scope_spawn(scope, t);

            let polled = hew_task_scope_poll(scope);
            assert_eq!(polled, t);

            hew_task_scope_complete_task(scope, t);
            let polled = hew_task_scope_poll(scope);
            assert!(polled.is_null());

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_wait_first() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            let t2 = hew_task_new();
            hew_task_scope_spawn(scope, t1);
            hew_task_scope_spawn(scope, t2);

            // No tasks done yet.
            assert!(hew_task_scope_wait_first(scope).is_null());

            hew_task_scope_complete_task(scope, t1);
            let first = hew_task_scope_wait_first(scope);
            assert_eq!(first, t1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn threaded_task_spawn_and_await() {
        unsafe extern "C" fn compute_42(task: *mut HewTask) {
            let val: i32 = 42;
            // SAFETY: task is valid, val is on our stack but we deep-copy.
            unsafe {
                hew_task_set_result(task, (&raw const val).cast_mut().cast(), size_of::<i32>());
                hew_task_complete_threaded(task);
            }
        }

        // SAFETY: test owns the scope and task; pointers are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            hew_task_spawn_thread(task, compute_42);

            let result = hew_task_await_blocking(task);
            assert!(!result.is_null());
            assert_eq!(*(result.cast::<i32>()), 42);
            assert_eq!((*task).load_state(), HewTaskState::Done);

            hew_task_scope_join_all(scope);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn threaded_tasks_run_concurrently() {
        use std::sync::atomic::{AtomicI32, Ordering};
        use std::time::Duration;

        static COUNTER: AtomicI32 = AtomicI32::new(0);

        unsafe extern "C" fn increment_task(task: *mut HewTask) {
            // Simulate some work
            std::thread::sleep(Duration::from_millis(10));
            COUNTER.fetch_add(1, Ordering::SeqCst);
            // SAFETY: task is valid for the lifetime of the thread.
            unsafe { hew_task_complete_threaded(task) };
        }

        COUNTER.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let t1 = hew_task_new();
            let t2 = hew_task_new();
            let t3 = hew_task_new();
            hew_task_scope_spawn(scope, t1);
            hew_task_scope_spawn(scope, t2);
            hew_task_scope_spawn(scope, t3);

            hew_task_spawn_thread(t1, increment_task);
            hew_task_spawn_thread(t2, increment_task);
            hew_task_spawn_thread(t3, increment_task);

            // Wait for all
            hew_task_scope_join_all(scope);

            assert_eq!(COUNTER.load(Ordering::SeqCst), 3);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn scope_is_cancelled_returns_flag() {
        // SAFETY: test owns the scope pointer exclusively; it is valid.
        unsafe {
            let scope = hew_task_scope_new();
            assert_eq!(hew_task_scope_is_cancelled(scope), 0);
            hew_task_scope_cancel(scope);
            assert_eq!(hew_task_scope_is_cancelled(scope), 1);
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn task_state_transitions_through_lifecycle() {
        // Verify the expected Ready → Running → Done progression and that
        // each transition is observable via the atomic load.
        // SAFETY: test owns the task pointer exclusively; it is valid.
        unsafe {
            let t = hew_task_new();
            assert_eq!((*t).load_state(), HewTaskState::Ready);

            (*t).store_state(HewTaskState::Running, Ordering::Release);
            assert_eq!((*t).load_state(), HewTaskState::Running);

            (*t).store_state(HewTaskState::Suspended, Ordering::Release);
            assert_eq!((*t).load_state(), HewTaskState::Suspended);

            (*t).store_state(HewTaskState::Done, Ordering::Release);
            assert_eq!((*t).load_state(), HewTaskState::Done);

            hew_task_free(t);
        }
    }

    #[test]
    fn complete_threaded_publishes_result_to_awaiter() {
        // The worker thread sets a result and calls hew_task_complete_threaded
        // (Release store). The main thread's hew_task_await_blocking must see
        // both the Done state and the result data (Acquire load).
        unsafe extern "C" fn produce_result(task: *mut HewTask) {
            let val: i32 = 99;
            // SAFETY: task is valid, val is on our stack but set_result deep-copies.
            unsafe {
                hew_task_set_result(task, (&raw const val).cast_mut().cast(), size_of::<i32>());
                hew_task_complete_threaded(task);
            }
        }

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, produce_result);

            let result = hew_task_await_blocking(task);
            assert!(!result.is_null());
            assert_eq!(*(result.cast::<i32>()), 99);
            assert_eq!((*task).load_state(), HewTaskState::Done);

            hew_task_scope_destroy(scope);
        }
    }

    /// Stress test: spawn many tasks concurrently and verify every one
    /// reaches `Done` with the correct result visible to the joining thread.
    #[test]
    fn concurrent_tasks_all_reach_done() {
        use std::sync::atomic::AtomicUsize;

        const TASK_COUNT: usize = 200;

        /// Global counter: each worker claims a unique index via `fetch_add`.
        static NEXT_INDEX: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn identity_task(task: *mut HewTask) {
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "TASK_COUNT is well under i32::MAX"
            )]
            let idx = NEXT_INDEX.fetch_add(1, Ordering::Relaxed) as i32;
            let boxed = Box::new(idx);
            let ptr = Box::into_raw(boxed);
            // SAFETY: task is valid; ptr points to a valid i32 allocation.
            unsafe {
                hew_task_set_result(task, ptr.cast(), size_of::<i32>());
                // Free the temporary box — set_result deep-copies.
                drop(Box::from_raw(ptr));
                hew_task_complete_threaded(task);
            }
        }

        NEXT_INDEX.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let mut tasks: Vec<*mut HewTask> = Vec::with_capacity(TASK_COUNT);

            for _ in 0..TASK_COUNT {
                let t = hew_task_new();
                hew_task_scope_spawn(scope, t);
                hew_task_spawn_thread(t, identity_task);
                tasks.push(t);
            }

            // Await every task and collect results.
            let mut results = Vec::with_capacity(TASK_COUNT);
            for (i, &t) in tasks.iter().enumerate() {
                let result = hew_task_await_blocking(t);
                assert!(!result.is_null(), "task {i} returned null result");
                results.push(*(result.cast::<i32>()));
            }

            // Every index 0..TASK_COUNT should appear exactly once.
            results.sort_unstable();
            #[expect(
                clippy::cast_possible_truncation,
                clippy::cast_possible_wrap,
                reason = "TASK_COUNT is well under i32::MAX"
            )]
            let expected: Vec<i32> = (0..TASK_COUNT as i32).collect();
            assert_eq!(results, expected, "not all task indices were produced");

            // All tasks should be Done.
            hew_task_scope_join_all(scope);
            for (i, &t) in tasks.iter().enumerate() {
                assert_eq!(
                    (*t).load_state(),
                    HewTaskState::Done,
                    "task {i} not in Done state after join"
                );
            }
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn from_i32_round_trips_all_variants() {
        for (expected, val) in [
            (HewTaskState::Ready, 0),
            (HewTaskState::Running, 1),
            (HewTaskState::Suspended, 2),
            (HewTaskState::Done, 3),
        ] {
            assert_eq!(HewTaskState::from_i32(val), Some(expected));
        }
        assert_eq!(HewTaskState::from_i32(-1), None);
        assert_eq!(HewTaskState::from_i32(4), None);
    }

    /// Exercises the lock-free reader path that was racy before the
    /// `AtomicI32` fix: one thread completes a task while the main thread
    /// spin-polls `hew_task_get_result` (which reads `state` directly,
    /// bypassing the `TaskDoneSignal` condvar).
    #[test]
    fn spin_poll_observes_done_without_condvar() {
        use std::sync::Barrier;

        unsafe extern "C" fn delayed_complete(task: *mut HewTask) {
            let val: i32 = 777;
            // SAFETY: task is valid; val is stack-local but set_result deep-copies.
            unsafe {
                hew_task_set_result(task, (&raw const val).cast_mut().cast(), size_of::<i32>());
                hew_task_complete_threaded(task);
            }
        }

        // Run multiple iterations to increase the chance of hitting the
        // interleaving that exposed the original data race.
        for _ in 0..50 {
            // SAFETY: test owns all scope/task pointers exclusively; all valid.
            unsafe {
                let scope = hew_task_scope_new();
                let task = hew_task_new();
                hew_task_scope_spawn(scope, task);

                // Use a barrier so the worker and the poller start at roughly
                // the same time, maximising the race window.
                let barrier = Arc::new(Barrier::new(2));
                let b2 = Arc::clone(&barrier);

                let task_addr = task as usize;
                let worker = std::thread::spawn(move || {
                    b2.wait();
                    let t = task_addr as *mut HewTask;
                    // SAFETY: t is valid for the lifetime of the scope.
                    delayed_complete(t);
                });

                barrier.wait();

                // Spin-poll hew_task_get_result — this reads state WITHOUT
                // going through the condvar, exercising the Acquire load path.
                loop {
                    let r = hew_task_get_result(task);
                    if !r.is_null() {
                        assert_eq!(*(r.cast::<i32>()), 777);
                        break;
                    }
                    std::hint::spin_loop();
                }

                worker.join().expect("worker panicked");
                hew_task_scope_destroy(scope);
            }
        }
    }

    extern "C" fn increment_observer(ctx: *mut c_void) {
        // SAFETY: tests pass a live `AtomicUsize` pointer as callback context.
        unsafe { (&*(ctx.cast::<AtomicUsize>())).fetch_add(1, Ordering::SeqCst) };
    }

    #[test]
    fn completion_observe_ready_task_fires_on_done() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            let hits = AtomicUsize::new(0);

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                0
            );
            assert_eq!(hits.load(Ordering::SeqCst), 0);
            hew_task_scope_complete_task(scope, task);
            assert_eq!(hits.load(Ordering::SeqCst), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn completion_observe_suspended_task_fires_on_done() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            (*task).store_state(HewTaskState::Suspended, Ordering::Relaxed);
            let hits = AtomicUsize::new(0);

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                0
            );
            hew_task_scope_complete_task(scope, task);
            assert_eq!(hits.load(Ordering::SeqCst), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn completion_observe_running_task_fires_on_done() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            (*task).store_state(HewTaskState::Running, Ordering::Relaxed);
            let hits = AtomicUsize::new(0);

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                0
            );
            hew_task_complete_threaded(task);
            assert_eq!(hits.load(Ordering::SeqCst), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn completion_observe_after_done_fires_synchronously() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            hew_task_scope_complete_task(scope, task);
            let hits = AtomicUsize::new(0);

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                0
            );
            assert_eq!(hits.load(Ordering::SeqCst), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn completion_observe_null_guards_return_minus_one() {
        // SAFETY: null inputs are passed deliberately to verify C ABI sentinels.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            let hits = AtomicUsize::new(0);

            assert_eq!(
                hew_task_completion_observe(
                    ptr::null_mut(),
                    task,
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                -1
            );
            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    ptr::null_mut(),
                    Some(increment_observer),
                    (&raw const hits).cast_mut().cast(),
                ),
                -1
            );
            assert_eq!(
                hew_task_completion_observe(scope, task, None, (&raw const hits).cast_mut().cast()),
                -1
            );
            assert_eq!(hits.load(Ordering::SeqCst), 0);

            hew_task_scope_destroy(scope);
        }
    }

    static COMPLETION_OBSERVE_ORDER: Mutex<Vec<i32>> = Mutex::new(Vec::new());

    extern "C" fn record_observer_one(_: *mut c_void) {
        COMPLETION_OBSERVE_ORDER.lock_or_recover().push(1);
    }

    extern "C" fn record_observer_two(_: *mut c_void) {
        COMPLETION_OBSERVE_ORDER.lock_or_recover().push(2);
    }

    #[test]
    fn completion_observe_two_observers_fire_in_registration_order() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            COMPLETION_OBSERVE_ORDER.lock_or_recover().clear();
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(record_observer_one),
                    ptr::null_mut()
                ),
                0
            );
            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(record_observer_two),
                    ptr::null_mut()
                ),
                0
            );
            hew_task_scope_complete_task(scope, task);
            assert_eq!(*COMPLETION_OBSERVE_ORDER.lock_or_recover(), vec![1, 2]);

            hew_task_scope_destroy(scope);
        }
    }

    struct ReentrantObserveCtx {
        scope: *mut HewTaskScope,
        task: *mut HewTask,
        outer_hits: AtomicUsize,
        inner_hits: AtomicUsize,
        inner_ret: AtomicI32,
    }

    extern "C" fn reentrant_inner_observer(ctx: *mut c_void) {
        // SAFETY: tests pass a live `ReentrantObserveCtx`.
        let ctx = unsafe { &*(ctx.cast::<ReentrantObserveCtx>()) };
        ctx.inner_hits.fetch_add(1, Ordering::SeqCst);
    }

    extern "C" fn reentrant_outer_observer(ctx: *mut c_void) {
        // SAFETY: tests pass a live `ReentrantObserveCtx`; the callback does
        // not outlive the stack frame that owns it.
        let ctx_ref = unsafe { &*(ctx.cast::<ReentrantObserveCtx>()) };
        ctx_ref.outer_hits.fetch_add(1, Ordering::SeqCst);
        // SAFETY: the same task is already complete. This re-enters the
        // observer registration path and would deadlock if callbacks fired
        // while holding the `TaskDoneSignal` mutex.
        let ret = unsafe {
            hew_task_completion_observe(
                ctx_ref.scope,
                ctx_ref.task,
                Some(reentrant_inner_observer),
                ctx,
            )
        };
        ctx_ref.inner_ret.store(ret, Ordering::SeqCst);
    }

    #[test]
    fn completion_observe_reentrant_callback_does_not_deadlock() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            let ctx = ReentrantObserveCtx {
                scope,
                task,
                outer_hits: AtomicUsize::new(0),
                inner_hits: AtomicUsize::new(0),
                inner_ret: AtomicI32::new(i32::MIN),
            };

            assert_eq!(
                hew_task_completion_observe(
                    scope,
                    task,
                    Some(reentrant_outer_observer),
                    (&raw const ctx).cast_mut().cast(),
                ),
                0
            );
            hew_task_scope_complete_task(scope, task);
            assert_eq!(ctx.outer_hits.load(Ordering::SeqCst), 1);
            assert_eq!(ctx.inner_ret.load(Ordering::SeqCst), 0);
            assert_eq!(ctx.inner_hits.load(Ordering::SeqCst), 1);

            hew_task_scope_destroy(scope);
        }
    }

    /// Two or more running tasks are all cancelled-and-detached together.
    ///
    /// This exercises the multi-handle Vec path in `take_detached_task_handles`:
    /// every thread is joined by the reaper and every allocation is freed
    /// exactly once, with no UAF (all joins happen before any free).
    #[test]
    fn destroy_cancel_multiple_running_tasks_all_reclaimed_by_reaper() {
        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::time::{Duration, Instant};

        const N: usize = 3;

        static STARTED: AtomicUsize = AtomicUsize::new(0);
        static RELEASE: AtomicBool = AtomicBool::new(false);
        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn blocking_worker_n(task: *mut HewTask) {
            STARTED.fetch_add(1, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        unsafe extern "C" fn count_env_drop_n(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        STARTED.store(0, Ordering::SeqCst);
        RELEASE.store(false, Ordering::SeqCst);
        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();

            for _ in 0..N {
                let t = hew_task_new();
                // Attach a drop-counting env so we can detect when the reaper
                // has freed each task allocation.
                hew_task_set_env(
                    t,
                    crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop_n)).cast(),
                );
                hew_task_scope_spawn(scope, t);
                hew_task_spawn_thread(t, blocking_worker_n);
            }

            // Wait for all N workers to start blocking.
            let started_deadline = Instant::now() + Duration::from_secs(2);
            while STARTED.load(Ordering::SeqCst) < N && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert_eq!(
                STARTED.load(Ordering::SeqCst),
                N,
                "not all {N} workers started"
            );

            // Cancel + destroy: all N running tasks must be detached, reaper spawned.
            hew_task_scope_cancel(scope);
            let t0 = Instant::now();
            hew_task_scope_destroy(scope);
            assert!(
                t0.elapsed() < Duration::from_millis(250),
                "destroy blocked with {N} running tasks"
            );
        }

        // Allocations must not be freed while workers are still running.
        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            0,
            "reaper freed tasks before workers exited"
        );

        // Release all workers → reaper joins all N threads, then frees all N tasks.
        RELEASE.store(true, Ordering::SeqCst);

        let cleanup_deadline = Instant::now() + Duration::from_secs(2);
        while ENV_DROPS.load(Ordering::SeqCst) < N && Instant::now() < cleanup_deadline {
            std::thread::sleep(Duration::from_millis(1));
        }
        assert_eq!(
            ENV_DROPS.load(Ordering::SeqCst),
            N,
            "reaper did not reclaim all {N} task allocations"
        );
    }

    #[test]
    fn destroy_reaper_spawn_failure_falls_back_to_sync_cleanup() {
        use std::sync::atomic::{AtomicBool, AtomicUsize};
        use std::time::{Duration, Instant};

        static STARTED: AtomicBool = AtomicBool::new(false);
        static RELEASE: AtomicBool = AtomicBool::new(false);
        static ENV_DROPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn blocking_worker(task: *mut HewTask) {
            STARTED.store(true, Ordering::SeqCst);
            while !RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            // SAFETY: `task` is the live task pointer owned by this worker.
            unsafe { hew_task_complete_threaded(task) };
        }

        unsafe extern "C" fn count_env_drop(_: *mut u8) {
            ENV_DROPS.fetch_add(1, Ordering::SeqCst);
        }

        STARTED.store(false, Ordering::SeqCst);
        RELEASE.store(false, Ordering::SeqCst);
        ENV_DROPS.store(0, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            crate::hew_clear_error();
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_set_env(
                task,
                crate::rc::hew_rc_new(ptr::null(), 0, 0, Some(count_env_drop)).cast(),
            );
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, blocking_worker);

            let started_deadline = Instant::now() + Duration::from_secs(1);
            while !STARTED.load(Ordering::SeqCst) && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert!(
                STARTED.load(Ordering::SeqCst),
                "worker thread never started"
            );

            hew_task_scope_cancel(scope);
            FAIL_TASK_REAPER_SPAWN.with(|fail| fail.set(true));
            let release_thread = std::thread::spawn(|| {
                std::thread::sleep(Duration::from_millis(75));
                RELEASE.store(true, Ordering::SeqCst);
            });

            let destroy_started = Instant::now();
            hew_task_scope_destroy(scope);
            let destroy_elapsed = destroy_started.elapsed();

            FAIL_TASK_REAPER_SPAWN.with(|fail| fail.set(false));
            release_thread.join().expect("release thread panicked");

            assert!(
                destroy_elapsed >= Duration::from_millis(50),
                "sync fallback should wait for detached worker cleanup"
            );
            assert_eq!(
                ENV_DROPS.load(Ordering::SeqCst),
                1,
                "sync fallback should reclaim task allocations exactly once"
            );
            let err = CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .expect("last error should be utf-8");
            assert!(
                err.contains("failed to spawn hew-task-reaper thread"),
                "unexpected last error: {err}"
            );
        }
    }

    // ── cancel_one tests ────────────────────────────────────────────────────

    #[test]
    fn cancel_one_ready_marks_done() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            assert_eq!((*task).load_state(), HewTaskState::Ready);
            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(ret, 0, "cancel_one must return 0 on success");
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!((*task).error, HewTaskError::Cancelled);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancel_one_suspended_marks_done() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            (*task).store_state(HewTaskState::Suspended, Ordering::Relaxed);

            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(ret, 0, "cancel_one must return 0 on success");
            assert_eq!((*task).load_state(), HewTaskState::Done);
            assert_eq!((*task).error, HewTaskError::Cancelled);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancel_one_done_is_noop() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            // Put task into Done state directly (simulates a completed task).
            (*task).mark_done(HewTaskError::None);
            (*scope).completed_count += 1;

            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(
                ret, 0,
                "cancel_one of Done task must be a no-op returning 0"
            );
            assert_eq!((*task).load_state(), HewTaskState::Done);
            // Error field must not be overwritten by the no-op cancel.
            assert_eq!((*task).error, HewTaskError::None);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancel_one_double_cancel_idempotent() {
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            let first = hew_task_scope_cancel_one(scope, task);
            let second = hew_task_scope_cancel_one(scope, task);
            assert_eq!(first, 0, "first cancel must return 0");
            assert_eq!(second, 0, "second cancel must return 0 (idempotent)");
            // completed_count must only be bumped once — the first cancel
            // transitions to Done; the second is a Done no-op.
            assert_eq!((*scope).completed_count, 1);

            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancel_one_null_scope_returns_sentinel() {
        // SAFETY: task is valid; scope is intentionally null.
        unsafe {
            let task = hew_task_new();
            let ret = hew_task_scope_cancel_one(ptr::null_mut(), task);
            assert_eq!(ret, -1, "null scope must return -1 sentinel");
            hew_task_free(task);
        }
    }

    #[test]
    fn cancel_one_null_task_returns_sentinel() {
        // SAFETY: scope is valid; task is intentionally null.
        unsafe {
            let scope = hew_task_scope_new();
            let ret = hew_task_scope_cancel_one(scope, ptr::null_mut());
            assert_eq!(ret, -1, "null task must return -1 sentinel");
            hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn cancel_one_running_returns_zero_state_unchanged() {
        // Running-state cancel is best-effort and cooperative. The runtime
        // notifies done_signal (waking any parked waiter) but does NOT force
        // the task into Done — the worker thread must finish its quantum and
        // check for cancellation itself.
        //
        // There is no cooperative-cancel flag on HewTask today; observable
        // effects are limited to waking waiters so they can re-check state.
        //
        // This test verifies the call succeeds and does not alter state.
        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            (*task).store_state(HewTaskState::Running, Ordering::Relaxed);

            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(ret, 0, "cancel_one of Running task must return 0");
            // State must remain Running: best-effort cancel does not force Done.
            assert_eq!((*task).load_state(), HewTaskState::Running);
            // Scope must not be prematurely marked done.
            assert_eq!(hew_task_scope_is_done(scope), 0);

            // Restore to Done so destroy can clean up cleanly.
            (*task).mark_done(HewTaskError::None);
            (*scope).completed_count += 1;
            hew_task_scope_destroy(scope);
        }
    }

    /// `hew_task_await_blocking` must not return until the task is truly Done,
    /// even when `hew_task_scope_cancel_one` fires a best-effort notify while
    /// the task is still Running.
    ///
    /// Without the `wait_while` fix, `cancel_one` sets the `TaskDoneSignal`
    /// boolean to `true`, causing `wait_until_done` to return immediately with
    /// a stale (non-Done, null-result) state.
    ///
    /// Test structure:
    /// 1. Spawn a real OS thread that blocks until `WORKER_RELEASE` is set.
    /// 2. Spawn an awaiter thread calling `hew_task_await_blocking`; the
    ///    awaiter sets `AWAITER_RETURNED` as soon as the call returns.
    /// 3. Call `cancel_one` while the worker is still blocked.
    /// 4. Sleep briefly to give a spurious-unblock time to propagate.
    /// 5. Assert `AWAITER_RETURNED` is still false — the awaiter must not have
    ///    returned yet (the task is still Running).
    /// 6. Release the worker → it calls `hew_task_complete_threaded`.
    /// 7. Assert `AWAITER_RETURNED` becomes true; task state is Done.
    #[test]
    fn cancel_one_running_awaiter_stays_blocked_until_done() {
        use std::sync::atomic::AtomicBool;
        use std::time::{Duration, Instant};

        static WORKER_STARTED: AtomicBool = AtomicBool::new(false);
        static WORKER_RELEASE: AtomicBool = AtomicBool::new(false);
        static AWAITER_RETURNED: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn blocking_worker_cancel_one(task: *mut HewTask) {
            WORKER_STARTED.store(true, Ordering::SeqCst);
            while !WORKER_RELEASE.load(Ordering::SeqCst) {
                std::thread::sleep(Duration::from_millis(1));
            }
            // SAFETY: `task` is valid; called from the task's own thread.
            unsafe { hew_task_complete_threaded(task) };
        }

        WORKER_STARTED.store(false, Ordering::SeqCst);
        WORKER_RELEASE.store(false, Ordering::SeqCst);
        AWAITER_RETURNED.store(false, Ordering::SeqCst);

        // SAFETY: test owns all scope/task pointers exclusively; all are valid.
        // The awaiter thread receives a raw pointer address (usize) and
        // reconstructs it; the scope keeps the task alive until destroy.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);
            hew_task_spawn_thread(task, blocking_worker_cancel_one);

            // Wait until the worker is inside its blocking loop.
            let started_deadline = Instant::now() + Duration::from_secs(2);
            while !WORKER_STARTED.load(Ordering::SeqCst) && Instant::now() < started_deadline {
                std::thread::yield_now();
            }
            assert!(
                WORKER_STARTED.load(Ordering::SeqCst),
                "worker did not start in time"
            );

            // Spawn the awaiter thread before cancel_one so it is parked in
            // wait_while when the spurious notify arrives.
            // `AWAITER_RETURNED` is set immediately after `hew_task_await_blocking`
            // returns, providing a reliable cross-thread signal.
            // Return the pointer as a usize so the closure is Send (*mut c_void is not).
            let task_addr = task as usize;
            let awaiter = std::thread::spawn(move || {
                let t = task_addr as *mut HewTask;
                // SAFETY: scope keeps task alive until destroy; awaiter joins before destroy.
                let ptr = hew_task_await_blocking(t);
                AWAITER_RETURNED.store(true, Ordering::SeqCst);
                ptr as usize
            });

            // Give the awaiter time to enter wait_while and park on the condvar.
            std::thread::sleep(Duration::from_millis(20));

            // Fire cancel_one — this sends a best-effort notify on the done_signal.
            // The task is still Running; without the fix `wait_until_done` returns here
            // because it trusts the boolean, not the task state.
            let ret = hew_task_scope_cancel_one(scope, task);
            assert_eq!(ret, 0, "cancel_one must return 0 for a Running task");

            // Give any spurious unblock 20 ms to propagate through the awaiter thread.
            // If the fix is absent, `AWAITER_RETURNED` will be true by now.
            std::thread::sleep(Duration::from_millis(20));

            assert!(
                !AWAITER_RETURNED.load(Ordering::SeqCst),
                "awaiter unblocked early — cancel_one spurious notify bypassed wait_while"
            );

            // Release the worker so it transitions the task to Done and fires the
            // real notify.
            WORKER_RELEASE.store(true, Ordering::SeqCst);

            // The awaiter must now return within a reasonable timeout.
            let done_deadline = Instant::now() + Duration::from_secs(2);
            while !AWAITER_RETURNED.load(Ordering::SeqCst) && Instant::now() < done_deadline {
                std::thread::sleep(Duration::from_millis(1));
            }
            assert!(
                AWAITER_RETURNED.load(Ordering::SeqCst),
                "awaiter did not unblock after the task genuinely completed"
            );
            awaiter.join().expect("awaiter thread panicked");

            // Task must be Done after the worker completed.
            assert_eq!(
                (*task).load_state(),
                HewTaskState::Done,
                "task must be Done after worker completes"
            );

            (*scope).completed_count += 1;
            hew_task_scope_destroy(scope);
        }
    }

    /// Abandon drop-safety: a parked `await t` whose continuation is dropped
    /// (`hew_task_detach_await`) before the task completes must still reclaim its
    /// `TaskAwaitWaiter` box and the observer's in-flight slot ref exactly once
    /// when the task later reaches `Done` and `task_await_wake` fires against the
    /// cancelled slot.
    ///
    /// The ladder this test pins:
    /// 1. `hew_task_await_suspend` retains the slot (creator ref + observer
    ///    in-flight ref = 2) and registers the waiter as a completion observer.
    /// 2. `hew_task_detach_await` cancels the slot and releases the creator ref
    ///    (refs = 1: only the observer's in-flight ref remains). It does NOT free
    ///    the waiter box — the still-registered observer owns that teardown.
    /// 3. Driving the task to `Done` fires `task_await_wake`, which boxes-frees
    ///    the waiter, sees the cancelled slot (no wake, no enqueue against a torn
    ///    -down continuation), and releases the in-flight ref (refs = 0 → slot
    ///    freed).
    ///
    /// Under `ASan` this proves single-free of both the waiter box and the slot
    /// on the abandon path; the explicit ref-ladder assertions give the test
    /// teeth without a sanitizer (a leaked observer ref would leave refs at 1 and
    /// a double-release would underflow the refcount debug assert).
    #[test]
    fn detach_await_reclaims_waiter_box_and_slot_ref_once_on_later_done() {
        // SAFETY: the test owns every scope/task/slot pointer exclusively and
        // drives the lifecycle in order; all are valid at each call.
        unsafe {
            let scope = hew_task_scope_new();
            let task = hew_task_new();
            hew_task_scope_spawn(scope, task);

            let slot = crate::read_slot::hew_read_slot_new();
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "fresh slot must carry exactly the creator ref"
            );

            // Park the awaiter with a null actor: `enqueue_resume` re-validates
            // liveness and a cancelled slot suppresses the wake anyway, so no
            // actor is needed to exercise the abandon teardown.
            let ret = hew_task_await_suspend(scope, task, ptr::null_mut(), slot);
            assert_eq!(
                ret, TASK_AWAIT_SUSPEND,
                "a not-yet-Done task must park the awaiter"
            );
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                2,
                "suspend must retain the observer's in-flight ref atop the creator ref"
            );

            // Abandon edge: cancel + release the creator ref. The observer's
            // in-flight ref must survive so the later wake has a live slot.
            hew_task_detach_await(scope, task, slot);
            assert_eq!(
                crate::read_slot::read_slot_refs_for_test(slot),
                1,
                "detach must release only the creator ref, leaving the in-flight ref"
            );

            // Drive the task to Done. This fires `task_await_wake`, which frees
            // the waiter box, observes the cancelled slot (no wake), and releases
            // the in-flight ref — dropping the slot to zero and freeing it. ASan
            // flags any double-free of the box or slot here; a missed release
            // would instead leak the slot (caught by leak detection).
            hew_task_complete_threaded(task);
            assert_eq!(
                (*task).load_state(),
                HewTaskState::Done,
                "completing the task must transition it to Done"
            );

            // The slot is freed by now (refs reached 0 in the wake); reading its
            // refcount would be a use-after-free, so we do not touch `slot` again.
            (*scope).completed_count += 1;
            hew_task_scope_destroy(scope);
        }
    }
}
