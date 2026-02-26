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
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::internal::types::{HewTaskError, HewTaskState};

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
    /// Current lifecycle state.
    pub state: HewTaskState,
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
        let mut done = self.lock.lock().unwrap();
        *done = true;
        self.cond.notify_all();
    }

    fn wait_until_done(&self) {
        let mut done = self.lock.lock().unwrap();
        while !*done {
            done = self.cond.wait(done).unwrap();
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
            .field("state", &self.state)
            .field("error", &self.error)
            .field("result_size", &self.result_size)
            .finish()
    }
}

// SAFETY: Tasks are only accessed from the single actor thread that owns
// the enclosing task scope. No cross-thread sharing occurs.
unsafe impl Send for HewTask {}

// ── Task lifecycle ─────────────────────────────────────────────────────

/// Allocate a new task.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_task_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_new() -> *mut HewTask {
    let task = Box::new(HewTask {
        state: HewTaskState::Ready,
        error: HewTaskError::None,
        result: ptr::null_mut(),
        result_size: 0,
        scope: ptr::null_mut(),
        next: ptr::null_mut(),
        done_signal: None,
        thread_handle: None,
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
    if task.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `task` was Box-allocated.
    let t = unsafe { Box::from_raw(task) };
    if !t.result.is_null() {
        // SAFETY: result was malloc'd by hew_task_set_result.
        unsafe { libc::free(t.result) };
    }
}

/// Get the task's result pointer, or null if not done.
///
/// # Safety
///
/// `task` must be a valid pointer returned by [`hew_task_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_task_get_result(task: *mut HewTask) -> *mut c_void {
    if task.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &*task };
    if t.state != HewTaskState::Done {
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
    if task.is_null() {
        return;
    }
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
    if task.is_null() {
        return HewTaskError::None as i32;
    }
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
    if task.is_null() {
        return 0;
    }
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
    if task.is_null() {
        return;
    }

    // Set up the done signal for cross-thread notification.
    let signal = Arc::new(TaskDoneSignal::new());
    // SAFETY: Caller guarantees `task` is valid. We write before spawning the thread.
    let t = unsafe { &mut *task };
    t.done_signal = Some(Arc::clone(&signal));
    t.state = HewTaskState::Running;

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
    if task.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &*task };

    // If already done, return immediately.
    if t.state == HewTaskState::Done {
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
    if task.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `task` is valid.
    let t = unsafe { &mut *task };
    t.state = HewTaskState::Done;
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
    if scope.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let t = unsafe { &mut *cur };

        // Join the thread if it was spawned.
        if let Some(handle) = t.thread_handle.take() {
            let _ = handle.join();
        }

        // Wait on done signal if present.
        if let Some(ref signal) = t.done_signal {
            signal.wait_until_done();
        }

        // Update scope count.
        if t.state == HewTaskState::Done && s.completed_count < s.task_count {
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
    if scope.is_null() {
        return 0;
    }
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
    if scope.is_null() || task.is_null() {
        return;
    }
    // SAFETY: Caller guarantees both pointers are valid.
    let s = unsafe { &mut *scope };
    // SAFETY: caller guarantees task is valid.
    let t = unsafe { &mut *task };
    t.scope = scope;
    t.state = HewTaskState::Ready;
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
    if scope.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        if unsafe { (*cur).state } == HewTaskState::Ready {
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
    if scope.is_null() {
        return 1;
    }
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
    if scope.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &mut *scope };
    s.cancelled.store(true, Ordering::Release);

    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let t = unsafe { &mut *cur };
        if t.state == HewTaskState::Ready || t.state == HewTaskState::Suspended {
            t.state = HewTaskState::Done;
            t.error = HewTaskError::Cancelled;
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
    if scope.is_null() || task.is_null() {
        return;
    }
    // SAFETY: Caller guarantees both pointers are valid.
    let s = unsafe { &mut *scope };
    // SAFETY: caller guarantees task is valid.
    let t = unsafe { &mut *task };

    if t.state == HewTaskState::Done {
        return; // Already terminal.
    }

    t.state = HewTaskState::Done;
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
    if scope.is_null() {
        return ptr::null_mut();
    }
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
    if scope.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        if unsafe { (*cur).state } == HewTaskState::Done {
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
    if scope.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `scope` is valid.
    let s = unsafe { &*scope };
    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers in the list are valid.
        let state = unsafe { (*cur).state };
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
    if scope.is_null() {
        return;
    }
    // Join all worker threads before freeing tasks to avoid UAF.
    // SAFETY: Caller guarantees `scope` is valid.
    unsafe { hew_task_scope_join_all(scope) };
    // SAFETY: Caller guarantees `scope` was Box-allocated.
    let s = unsafe { Box::from_raw(scope) };

    let mut cur = s.tasks;
    while !cur.is_null() {
        // SAFETY: All task pointers were Box-allocated by hew_task_new.
        let next = unsafe { (*cur).next };
        // SAFETY: cur was allocated by hew_task_new.
        unsafe { hew_task_free(cur) };
        cur = next;
    }
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
            assert_eq!((*t).state, HewTaskState::Ready);
            assert_eq!((*t).error, HewTaskError::None);

            let val: i32 = 42;
            hew_task_set_result(t, (&raw const val).cast_mut().cast(), size_of::<i32>());
            (*t).state = HewTaskState::Done;
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
            assert_eq!((*t).state, HewTaskState::Done);
            assert_eq!((*t).error, HewTaskError::Cancelled);
            assert_eq!(hew_task_is_cancelled(t), 1);
            assert_eq!(hew_task_scope_is_done(scope), 1);

            hew_task_scope_destroy(scope);
        }
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
            assert_eq!((*task).state, HewTaskState::Done);

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
}
