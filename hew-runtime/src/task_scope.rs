//! Hew runtime: task management for structured concurrency.
//!
//! Tasks are units of concurrent work spawned via `s.launch` (inside `scope |s| { ... }`). Each
//! task runs on a separate OS thread (from the runtime's pool), providing
//! true parallelism. Tasks do NOT share mutable state — like actors,
//! they communicate via results, not shared memory.
//!
//! Thread-safe completion notification uses `Mutex` + `Condvar` so that
//! `await` can block until a task finishes.

use std::cell::Cell;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Condvar, Mutex};

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
        // function compiled by MLIR/LLVM.
        unsafe { fn_ptr(task_ptr) };

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

    // Wait on the done signal.
    if let Some(ref signal) = t.done_signal {
        signal.wait_until_done();
    }

    // SAFETY: Task is now Done; result is safe to read.
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
    t.store_state(HewTaskState::Done, Ordering::Release);
    t.notify_done_signal();
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
            s.cancelled.load(Ordering::Acquire) && t.thread_handle.is_some();

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
    i32::from(unsafe { (*scope).cancelled.load(Ordering::Acquire) })
}

// ── Task scope ─────────────────────────────────────────────────────────

/// Intra-actor cooperative task scope.
///
/// Owns a linked list of tasks and tracks completion counts.
/// The `cancelled` flag is atomic (tasks run on OS threads).
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
    /// Parent scope for nesting (reserved for future nested scope support).
    #[expect(dead_code, reason = "reserved for future nested scope tree support")]
    parent: *mut HewTaskScope,
}

// SAFETY: Task scopes are only accessed from the single actor thread.
unsafe impl Send for HewTaskScope {}

/// Create a new empty task scope.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_task_scope_destroy`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_new() -> *mut HewTaskScope {
    let scope = Box::new(HewTaskScope {
        tasks: ptr::null_mut(),
        task_count: 0,
        completed_count: 0,
        cancelled: AtomicBool::new(false),
        parent: ptr::null_mut(),
    });
    Box::into_raw(scope)
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

    t.mark_done(HewTaskError::None);
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
#[no_mangle]
pub unsafe extern "C" fn hew_task_scope_destroy(scope: *mut HewTaskScope) {
    cabi_guard!(scope.is_null());
    // Join all worker threads before freeing tasks to avoid UAF.
    // SAFETY: Caller guarantees `scope` is valid.
    unsafe { hew_task_scope_join_all(scope) };
    // SAFETY: Caller guarantees `scope` was Box-allocated.
    let mut scope_box = unsafe { Box::from_raw(scope) };
    let detached_handles = take_detached_task_handles(&mut scope_box);
    if detached_handles.is_empty() {
        // SAFETY: join_all already drained every worker that could still touch
        // the tasks in this scope.
        unsafe { free_scope_tasks(&mut scope_box) };
        return;
    }

    // WASM-TODO: task_scope uses OS threads throughout and has no WASM target;
    // the reaper thread below is likewise native-only.
    let reaper = std::thread::Builder::new()
        .name("hew-task-reaper".into())
        .spawn(move || {
            for handle in detached_handles {
                let _ = handle.join();
            }
            // SAFETY: every detached worker has exited, so no task pointer can be
            // observed again after this point.
            unsafe { free_scope_tasks(&mut scope_box) };
        })
        .expect("failed to spawn hew-task-reaper thread");
    drop(reaper);
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

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
}
