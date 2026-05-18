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

// ── Thread-local current task scope ────────────────────────────────────

thread_local! {
    /// The task scope active on this thread (set during scope execution).
    static CURRENT_TASK_SCOPE: Cell<*mut HewTaskScope> = const { Cell::new(ptr::null_mut()) };
}

/// Return the current thread's active task scope (null if none).
pub(crate) fn current_task_scope() -> *mut HewTaskScope {
    CURRENT_TASK_SCOPE.with(Cell::get)
}

/// Set the current task scope for this thread, returning the previous value.
///
/// # Safety
///
/// `scope` must be a valid pointer returned by [`hew_task_scope_new`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_set_current(scope: *mut HewTaskScope) -> *mut HewTaskScope {
    CURRENT_TASK_SCOPE.with(|c| c.replace(scope))
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

unsafe fn hew_cancel_token_retain(token: *mut HewCancellationToken) {
    if !token.is_null() {
        // SAFETY: caller guarantees `token` is a live token pointer.
        unsafe { (*token).refs.fetch_add(1, Ordering::Relaxed) };
    }
}

unsafe fn hew_cancel_token_release(token: *mut HewCancellationToken) {
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
        unsafe { hew_cancel_token_release(boxed.parent) };
    }
}

fn token_state(token: &HewCancellationToken) -> HewCancellationState {
    HewCancellationState::from_i32(token.state.load(Ordering::Acquire))
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
            hew_cancel_token_retain(parent);
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
}

struct HewTaskScopeDeadline {
    cancelled: Arc<AtomicBool>,
    thread_handle: Option<JoinHandle<()>>,
    next: *mut HewTaskScopeDeadline,
}

/// Thread-safe signal for task completion notification.
#[derive(Debug)]
pub struct TaskDoneSignal {
    /// Guards the `done` flag.
    lock: Mutex<bool>,
    /// Notified when the task completes.
    cond: Condvar,
}

impl TaskDoneSignal {
    fn new() -> Self {
        Self {
            lock: Mutex::new(false),
            cond: Condvar::new(),
        }
    }

    fn notify_done(&self) {
        let mut done = self.lock.lock_or_recover();
        *done = true;
        self.cond.notify_all();
    }

    fn wait_until_done(&self) {
        let mut done = self.lock.lock_or_recover();
        while !*done {
            done = self.cond.wait_or_recover(done);
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
    unsafe { hew_cancel_token_release(t.cancel_token) };
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
    unsafe { hew_cancel_token_release(old) };
}

// ── Thread-spawned tasks ───────────────────────────────────────────────

/// Task function type: takes task pointer, stores result and marks done.
///
/// The generated code calls `hew_task_set_result` and
/// `hew_task_complete_threaded` from within this function.
pub type TaskFn = unsafe extern "C" fn(*mut HewTask);

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
        // waits for all tasks before destroying them). fn_ptr is a valid
        // function compiled by MLIR/LLVM. The child thread inherits the
        // lexical task scope so cooperate-sites can observe the child token.
        let previous_scope = unsafe { hew_task_scope_set_current((*task_ptr).scope) };
        // SAFETY: fn_ptr is the validated TaskFn supplied to
        // hew_task_spawn_thread, and task_ptr stays live until scope teardown.
        unsafe { fn_ptr(task_ptr) };
        // SAFETY: restore the thread-local scope before the worker exits.
        unsafe {
            let _ = hew_task_scope_set_current(previous_scope);
        }

        // Signal completion.
        signal.notify_done();
    });

    t.thread_handle = Some(handle);
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
        unsafe { hew_cancel_token_release(self.cancel_token) };
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
        // SAFETY: CURRENT_TASK_SCOPE only stores live scope pointers while a
        // scope is active on the current thread.
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
/// `Running` branch notifies `done_signal` so any parked waiter is woken to
/// re-check `Done` state. When the multiplex-await primitive
/// (completion-observer / park / resume) lands, that branch will also set a
/// task-level cancel flag that the worker checks at its next safepoint.
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
            t.notify_done_signal();
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

#[cfg(test)]
mod tests {
    use super::*;
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
        // SAFETY: test owns the scope pointer and restores TLS before destroy.
        unsafe {
            let scope = hew_task_scope_new();
            let previous = hew_task_scope_set_current(scope);
            hew_task_scope_cancel(scope);

            assert_eq!(crate::scheduler::hew_actor_cooperate(), 2);

            let _ = hew_task_scope_set_current(previous);
            hew_task_scope_destroy(scope);
        }
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
        // The deeper fix (running per-task drop dispatch on the cancel
        // path) is tracked alongside the broader cancellation-token
        // vocabulary in HEW-FUTURE §1.2; this regression test pins the
        // current contract so a future change either upgrades it or has
        // to acknowledge it.
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
                crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop)).cast(),
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
                crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop)).cast(),
            );
            hew_task_set_env(
                cancelled_task,
                crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop)).cast(),
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
                crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop)).cast(),
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
                    crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop_n)).cast(),
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
                crate::rc::hew_rc_new(ptr::null(), 0, Some(count_env_drop)).cast(),
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
        // effects are limited to the done_signal notification. When the
        // multiplex-await primitive (completion-observer / park / resume) lands,
        // a task-level cancel flag will be added and this test updated (§3.7).
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
}
