//! Elastic thread pool for work with no portable readiness.
//!
//! Jobs run in submission order. A job that finds no idle thread starts a new
//! one, up to [`HEW_BLOCKING_POOL_MAX`]; past that cap it waits in the queue and
//! the saturation counter records it. A thread that stays idle for the idle
//! timeout (`HEW_POOL_IDLE_MS`, default ten seconds) exits, so the pool shrinks
//! back after a burst. The pool is opaque to C callers (Box-allocated).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::collections::VecDeque;
use std::ffi::c_void;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;
use std::time::Duration;

use crate::util::{CondvarExt, MutexExt};

/// Most threads the blocking pool runs at once.
pub const HEW_BLOCKING_POOL_MAX: usize = 64;

/// How long a thread waits for a job before it exits.
const DEFAULT_IDLE: Duration = Duration::from_secs(10);

/// C function pointer for a blocking task.
pub type HewBlockingFn = unsafe extern "C" fn(arg: *mut c_void);

/// A queued task: function pointer + opaque argument.
struct Task {
    func: HewBlockingFn,
    arg: *mut c_void,
}

// SAFETY: The `arg` pointer is passed to the function by the submitter and is
// not dereferenced by the pool itself. The contract is that the submitter
// guarantees the pointer is valid until the function completes.
unsafe impl Send for Task {}

struct Queue {
    tasks: VecDeque<Task>,
    running: bool,
    /// Threads alive, including those running a job.
    threads: usize,
    /// Threads waiting for a job.
    idle: usize,
}

/// Shared state between the pool handle and worker threads.
struct PoolInner {
    queue: Mutex<Queue>,
    work: Condvar,
    max_threads: usize,
    idle_timeout: Duration,
    /// Jobs that found every thread busy at the cap and had to queue.
    saturations: AtomicU64,
}

/// Elastic blocking thread pool.
pub struct HewBlockingPool {
    inner: Arc<PoolInner>,
    workers: Mutex<Vec<JoinHandle<()>>>,
}

impl std::fmt::Debug for HewBlockingPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewBlockingPool")
            .field("threads", &self.inner.queue.lock_or_recover().threads)
            .finish_non_exhaustive()
    }
}

fn idle_timeout_from_env() -> Duration {
    std::env::var("HEW_POOL_IDLE_MS")
        .ok()
        .and_then(|value| value.trim().parse::<u64>().ok())
        .map_or(DEFAULT_IDLE, Duration::from_millis)
}

fn new_pool(max_threads: usize, idle_timeout: Duration) -> *mut HewBlockingPool {
    Box::into_raw(Box::new(HewBlockingPool {
        inner: Arc::new(PoolInner {
            queue: Mutex::new(Queue {
                tasks: VecDeque::new(),
                running: true,
                threads: 0,
                idle: 0,
            }),
            work: Condvar::new(),
            max_threads: max_threads.max(1),
            idle_timeout,
            saturations: AtomicU64::new(0),
        }),
        workers: Mutex::new(Vec::new()),
    }))
}

/// Create a new blocking pool. It starts with no threads and grows on demand.
///
/// Returns a heap-allocated, opaque pool pointer.
///
/// # Safety
///
/// No preconditions. The caller must eventually call [`hew_blocking_pool_stop`]
/// to free the pool.
#[no_mangle]
pub unsafe extern "C" fn hew_blocking_pool_new() -> *mut HewBlockingPool {
    new_pool(HEW_BLOCKING_POOL_MAX, idle_timeout_from_env())
}

impl HewBlockingPool {
    fn spawn_worker(&self) {
        let shared = Arc::clone(&self.inner);
        let spawned = std::thread::Builder::new()
            .name("hew-blocking".into())
            .spawn(move || worker_loop(&shared));
        let mut workers = self.workers.lock_or_recover();
        // Reap threads that exited on idle so the list tracks live threads.
        let (finished, live): (Vec<_>, Vec<_>) =
            workers.drain(..).partition(JoinHandle::is_finished);
        *workers = live;
        match spawned {
            Ok(handle) => workers.push(handle),
            Err(error) => {
                // The job stays queued for an existing thread; undo the count.
                let mut queue = self.inner.queue.lock_or_recover();
                queue.threads -= 1;
                crate::observe::record_pool_threads(queue.threads);
                drop(queue);
                eprintln!("hew: blocking pool could not start a thread: {error}");
            }
        }
        drop(workers);
        for handle in finished {
            let _ = handle.join();
        }
    }

    /// Threads alive now.
    #[cfg(test)]
    pub(crate) fn thread_count(&self) -> usize {
        self.inner.queue.lock_or_recover().threads
    }

    /// Jobs that had to queue behind a full pool.
    #[cfg(test)]
    pub(crate) fn saturation_count(&self) -> u64 {
        self.inner.saturations.load(Ordering::Relaxed)
    }
}

/// Submit a blocking task to the pool.
///
/// Returns 0 on success, -1 if the pool is null or has been stopped.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_blocking_pool_new`].
/// `func` must be a valid function pointer. `arg` must remain valid until
/// `func` completes.
#[no_mangle]
pub unsafe extern "C" fn hew_blocking_pool_submit(
    pool: *mut HewBlockingPool,
    func: HewBlockingFn,
    arg: *mut c_void,
) -> i32 {
    if pool.is_null() {
        return -1;
    }
    // SAFETY: caller guarantees `pool` is valid.
    let p = unsafe { &*pool };
    let mut queue = p.inner.queue.lock_or_recover();
    if !queue.running {
        return -1;
    }
    // The single-thread driver runs offloaded work inline at submission, so
    // its completion enters the ready list in program order.
    if crate::driver::active() {
        drop(queue);
        // SAFETY: the caller keeps `arg` valid until `func` completes, which
        // is before this call returns.
        unsafe { func(arg) };
        return 0;
    }
    queue.tasks.push_back(Task { func, arg });
    // More queued jobs than waiting threads: grow, or record saturation.
    let grow = if queue.tasks.len() > queue.idle {
        if queue.threads < p.inner.max_threads {
            queue.threads += 1;
            crate::observe::record_pool_threads(queue.threads);
            true
        } else {
            p.inner.saturations.fetch_add(1, Ordering::Relaxed);
            crate::observe::record_pool_saturation();
            false
        }
    } else {
        false
    };
    drop(queue);
    p.inner.work.notify_one();
    if grow {
        p.spawn_worker();
    }
    0
}

/// Reasons a [`spawn_blocking_result`] call may fail without producing a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockingPoolError {
    /// The optional deadline elapsed before the worker produced a result. The
    /// task will still run to completion on the pool thread; its result is
    /// discarded when the worker writes it to the now-orphaned slot.
    TimedOut,
    /// The pool was null or has been stopped; the task was never enqueued.
    PoolStopped,
    /// No runtime is installed, so there is no pool to offload onto. Surfaced by
    /// the fail-closed [`shared_blocking_pool_opt`] path when a C-ABI offload
    /// entrypoint is reached before `hew_sched_init` (production) or without a
    /// runtime test guard (tests). The task was never enqueued; the entrypoint
    /// returns a clean error to the C caller instead of aborting.
    NoRuntime,
    /// The worker caught a panic from the submitted closure and reported it to
    /// the waiting caller.
    WorkerPanicked,
}

enum SlotOutcome<T> {
    Pending,
    Ready(T),
    Panicked(String),
}

/// Result slot shared between the caller (waits) and the worker (writes).
struct Slot<T> {
    outcome: Mutex<SlotOutcome<T>>,
    ready: Condvar,
}

/// Heap-allocated context handed to the C trampoline. Owns the `FnOnce`
/// closure (in an `Option` so it can be moved out by value) and a strong
/// reference to the result slot.
struct TaskCtx<T, F> {
    slot: Arc<Slot<T>>,
    f: Option<F>,
}

/// C trampoline: recover the boxed context, run the closure, publish the
/// result, signal the caller. Runs entirely on a pool worker thread.
///
/// # Safety
///
/// `arg` must have been constructed by `Box::into_raw(Box::new(TaskCtx<T, F>))`
/// in the matching `spawn_blocking_result::<T, F>` call. The pool calls this
/// trampoline exactly once per submission; the function reclaims ownership.
unsafe extern "C" fn spawn_blocking_trampoline<T, F>(arg: *mut c_void)
where
    T: Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    // SAFETY: `arg` was constructed by `Box::into_raw(Box::new(TaskCtx<T,F>))`
    // in the matching `spawn_blocking_result::<T, F>` call. The pool calls
    // this trampoline exactly once per submission; we reclaim ownership.
    let ctx = unsafe { Box::from_raw(arg.cast::<TaskCtx<T, F>>()) };
    let TaskCtx { slot, mut f } = *ctx;
    // Take the FnOnce out of the Option so we can call it by value.
    let f = f.take().expect("FnOnce only invoked once");
    let outcome = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(value) => SlotOutcome::Ready(value),
        Err(payload) => {
            let message = crate::util::take_panic_payload_message(payload);
            SlotOutcome::Panicked(message)
        }
    };
    // Publish the outcome. If the caller has already given up (timed out), it
    // is stored and dropped when the last Arc drops.
    let mut guard = slot.outcome.lock_or_recover();
    *guard = outcome;
    slot.ready.notify_one();
}

/// Submit a closure to the blocking pool, park the calling thread on a
/// condvar-guarded result slot, and return the closure's value or an error.
///
/// `deadline` is optional: `None` parks indefinitely; `Some(d)` returns
/// `Err(BlockingPoolError::TimedOut)` if the worker has not produced a result
/// within `d`. On timeout the worker thread continues running the closure to
/// completion — its result is stored into the shared slot and dropped when the
/// last `Arc` reference goes away. There is no resource leak: the pool thread
/// is reclaimed naturally and the heap allocations live exactly until both
/// caller and worker drop their `Arc`.
///
/// This is a Rust-only API (no `#[no_mangle]`). The C ABI of the pool is
/// unchanged — internally the closure is heap-boxed and a stable C trampoline
/// is registered with [`hew_blocking_pool_submit`].
///
/// # Saturation
///
/// The pool has a fixed size ([`HEW_BLOCKING_POOL_SIZE`]). If every worker is
/// occupied with non-cancellable blocking syscalls, additional submissions
/// queue and parked callers will time out at their per-call deadline. The
/// pool is never deadlocked — workers drain the queue as previous tasks
/// complete.
///
/// WASM-TODO(blocking-pool): there is no blocking pool on WASM; until a
/// WASM-compatible deadline primitive exists (web-sys + Promise
/// integration, wasi-threads, or polled futures), transport callers on WASM
/// keep their pre-existing unguarded blocking shape.
///
/// # Errors
///
/// Returns `Err(BlockingPoolError::PoolStopped)` if `pool` is null or the
/// pool has been stopped (so the task could not be enqueued). Returns
/// `Err(BlockingPoolError::TimedOut)` if `deadline` is `Some(d)` and `d`
/// elapses before the worker publishes a result. Returns
/// `Err(BlockingPoolError::WorkerPanicked)` if the submitted closure panics.
///
/// # Panics
///
/// Panics in the trampoline if a `Box` reclaim fails — this should be
/// impossible because the trampoline owns a pointer it produced itself.
///
/// # Safety
///
/// `pool` must either be null or a valid pointer returned by
/// [`hew_blocking_pool_new`] that has not yet been freed with
/// [`hew_blocking_pool_stop`]. Callers must not race a `stop` against a
/// `spawn_blocking_result` on the same pool — the pool pointer itself becomes
/// invalid after stop returns.
pub unsafe fn spawn_blocking_result<T, F>(
    pool: *mut HewBlockingPool,
    f: F,
    deadline: Option<Duration>,
) -> Result<T, BlockingPoolError>
where
    T: Send + 'static,
    F: FnOnce() -> T + Send + 'static,
{
    if pool.is_null() {
        return Err(BlockingPoolError::PoolStopped);
    }

    let slot: Arc<Slot<T>> = Arc::new(Slot {
        outcome: Mutex::new(SlotOutcome::Pending),
        ready: Condvar::new(),
    });

    // Box the closure + a clone of the Arc so the C trampoline can recover
    // both via a single `*mut c_void`.
    let ctx_box: *mut TaskCtx<T, F> = Box::into_raw(Box::new(TaskCtx::<T, F> {
        slot: Arc::clone(&slot),
        f: Some(f),
    }));
    let ctx: *mut c_void = ctx_box.cast();

    // SAFETY: pool non-null is checked above; trampoline + ctx pointer are
    // valid for the life of the task. The pool guarantees the trampoline is
    // called at most once.
    let submit_status =
        unsafe { hew_blocking_pool_submit(pool, spawn_blocking_trampoline::<T, F>, ctx) };
    if submit_status != 0 {
        // Pool was stopped between the null check and submit. Reclaim the
        // Box we leaked above so we don't leak memory; the worker will never
        // run the trampoline, so we own the allocation.
        // SAFETY: `ctx_box` is the exact pointer we leaked; we never handed
        // ownership to a worker because submit failed.
        unsafe {
            let _ = Box::from_raw(ctx_box);
        }
        return Err(BlockingPoolError::PoolStopped);
    }

    // Park on the condvar until the worker publishes a result or the deadline
    // expires. cleanup-all-exits invariant: on the timeout path we explicitly
    // drop the guard and the Arc — the worker still holds a reference and
    // will drop the slot's storage after publishing.
    let mut guard = slot.outcome.lock_or_recover();
    match deadline {
        None => {
            while matches!(*guard, SlotOutcome::Pending) {
                guard = slot.ready.wait_or_recover(guard);
            }
        }
        Some(d) => {
            let start = std::time::Instant::now();
            while matches!(*guard, SlotOutcome::Pending) {
                let elapsed = start.elapsed();
                let Some(remaining) = d.checked_sub(elapsed) else {
                    return Err(BlockingPoolError::TimedOut);
                };
                let (next_guard, wait_result) =
                    slot.ready.wait_timeout_or_recover(guard, remaining);
                guard = next_guard;
                if wait_result.timed_out() && matches!(*guard, SlotOutcome::Pending) {
                    // cleanup-all-exits: dropping `guard` releases the lock
                    // before we return; `slot` (Arc) stays alive on the worker
                    // side and is freed when the worker publishes and drops.
                    return Err(BlockingPoolError::TimedOut);
                }
            }
        }
    }

    match std::mem::replace(&mut *guard, SlotOutcome::Pending) {
        SlotOutcome::Ready(value) => Ok(value),
        SlotOutcome::Panicked(message) => {
            crate::set_last_error(format!("blocking pool worker panicked: {message}"));
            Err(BlockingPoolError::WorkerPanicked)
        }
        SlotOutcome::Pending => unreachable!("worker outcome was observed ready"),
    }
}

/// Per-runtime owner of a `*mut HewBlockingPool`.
///
/// Held in a [`OnceLock`] field on the runtime (`RuntimeInner::blocking_pool`);
/// created lazily on the first blocking offload and dropped with its runtime.
/// `*mut T` is neither `Send` nor `Sync`; this wrapper asserts both based on the
/// pool's internal Mutex/Condvar synchronization. The pointer is valid for the
/// owning runtime's lifetime — it is stopped (queue drained, workers joined)
/// only when this field drops, by which point cleanup has already joined every
/// dispatch/reactor/worker thread that could submit to it.
pub(crate) struct OwnedBlockingPool(*mut HewBlockingPool);

// SAFETY: `HewBlockingPool` is heap-allocated by `hew_blocking_pool_new` and
// internally synchronized via Mutex+Condvar. The handle is owned by exactly one
// runtime and every access goes through the pool's own internal locks, so the
// raw pointer can cross threads (worker/reactor submitters) safely.
unsafe impl Send for OwnedBlockingPool {}
// SAFETY: see Send. All access goes through the pool's own internal locks.
unsafe impl Sync for OwnedBlockingPool {}

impl OwnedBlockingPool {
    /// Spawn a fresh blocking pool (its worker threads) and take ownership.
    pub(crate) fn new() -> Self {
        // SAFETY: `hew_blocking_pool_new` is safe to call (no preconditions); it
        // returns a heap-allocated pool this handle now owns and stops on drop.
        Self(unsafe { hew_blocking_pool_new() })
    }

    /// The raw pool pointer, valid for the owning runtime's lifetime.
    pub(crate) fn as_ptr(&self) -> *mut HewBlockingPool {
        self.0
    }
}

impl Drop for OwnedBlockingPool {
    fn drop(&mut self) {
        if self.0.is_null() {
            return;
        }
        // SAFETY: `self.0` came from `hew_blocking_pool_new` and is owned solely
        // by this field, so it is stopped exactly once — when the runtime that
        // owns it drops. `hew_runtime_cleanup` joins the reactor, ticker, and
        // scheduler workers (the only submitters) BEFORE dropping `RuntimeInner`
        // (`cleanup-all-exits`), so no submitting thread is parked on a pool slot
        // here; `hew_blocking_pool_stop` drains the queue and joins the pool's
        // own worker threads.
        unsafe { hew_blocking_pool_stop(self.0) };
    }
}

/// Return the calling runtime's blocking pool, creating it on first use.
///
/// Resolves the pool through the current runtime (`rt_current().blocking_pool()`)
/// and is therefore an init/mutate site: it spawns worker threads and requires a
/// runtime to be installed (`deglobalize-reads-stay-tolerant` — this is not a
/// tolerant read/sweep caller, so the fail-closed `rt_current()` is correct).
/// The returned pointer is valid for that runtime's lifetime and is shared
/// across its transport callers (DNS, TCP, HTTP, etc.) that offload blocking
/// syscalls with a deadline. Callers MUST NOT pass this pointer to
/// [`hew_blocking_pool_stop`] — the owning runtime stops it on drop.
///
/// Idempotent within a runtime: subsequent calls under the same runtime return
/// the same pointer.
///
/// WASM-TODO(blocking-pool): there is no blocking pool on WASM; transport callers
/// on WASM keep their pre-existing unguarded blocking shape until a
/// WASM-compatible deadline primitive exists.
#[must_use]
pub fn shared_blocking_pool() -> *mut HewBlockingPool {
    crate::runtime::rt_current().blocking_pool()
}

/// Return the calling runtime's blocking pool, or `None` when no runtime is
/// installed.
///
/// This is the fail-closed sibling of [`shared_blocking_pool`]: it resolves
/// through the non-panicking [`crate::runtime::rt_current_opt`] instead of the
/// trapping `rt_current()`. C-ABI entrypoints that offload blocking syscalls
/// (DNS resolve, TCP connect) call this so a request issued before
/// `hew_sched_init` installs a runtime — or after teardown drops it — returns a
/// defined error to the C caller instead of aborting across the ABI boundary.
///
/// Reaching a blocking-pool offload with no runtime installed is a programming
/// error, but it must surface as a clean error return, never a SIGABRT.
#[must_use]
pub fn shared_blocking_pool_opt() -> Option<*mut HewBlockingPool> {
    crate::runtime::rt_current_opt().map(crate::runtime::RuntimeInner::blocking_pool)
}

/// Stop the pool: reject new work, let workers finish the queue, and join them.
///
/// # Safety
///
/// `pool` must be a valid pointer returned by [`hew_blocking_pool_new`] and
/// must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_blocking_pool_stop(pool: *mut HewBlockingPool) {
    if pool.is_null() {
        return;
    }
    // SAFETY: caller guarantees `pool` is valid and surrenders ownership.
    let p = unsafe { *Box::from_raw(pool) };
    p.inner.queue.lock_or_recover().running = false;
    p.inner.work.notify_all();
    let workers = std::mem::take(&mut *p.workers.lock_or_recover());
    for handle in workers {
        if let Err(panic_payload) = handle.join() {
            let message = format!(
                "blocking pool worker panicked during teardown: {}",
                crate::util::take_panic_payload_message(panic_payload)
            );
            crate::set_last_error(message.clone());
            eprintln!("hew: {message}");
        }
    }
}

/// Worker thread main loop: take jobs in submission order; exit after the idle
/// timeout with nothing queued, or once the pool stops and its queue is empty.
fn worker_loop(inner: &PoolInner) {
    loop {
        let task = {
            let mut queue = inner.queue.lock_or_recover();
            loop {
                if let Some(task) = queue.tasks.pop_front() {
                    break Some(task);
                }
                let expired = if queue.running {
                    queue.idle += 1;
                    let (next, waited) = inner
                        .work
                        .wait_timeout_or_recover(queue, inner.idle_timeout);
                    queue = next;
                    queue.idle -= 1;
                    waited.timed_out() && queue.tasks.is_empty()
                } else {
                    true
                };
                if expired {
                    // Leave under the same lock that decided, so a submitter
                    // never counts this thread as able to take its job.
                    queue.threads -= 1;
                    crate::observe::record_pool_threads(queue.threads);
                    break None;
                }
            }
        };
        let Some(task) = task else {
            return;
        };
        crate::observe::record_blocking_start();
        // SAFETY: the submitter guarantees `func` and `arg` are valid.
        unsafe {
            (task.func)(task.arg);
        }
        crate::observe::record_blocking_finish();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CStr;
    use std::sync::atomic::{AtomicU32, AtomicUsize};

    /// A count paired with a Mutex+Condvar "done" latch so the submitting
    /// thread can wait for the worker to actually finish, rather than racing
    /// a fixed sleep against pool scheduling under load.
    struct SignalingCounter {
        count: AtomicU32,
        done: Mutex<bool>,
        ready: Condvar,
    }

    unsafe extern "C" fn increment_and_signal(arg: *mut c_void) {
        // SAFETY: caller passes an Arc::into_raw'd SignalingCounter pointer.
        let c = unsafe { Arc::from_raw(arg as *const SignalingCounter) };
        c.count.fetch_add(1, Ordering::Relaxed);
        let mut done = c.done.lock_or_recover();
        *done = true;
        c.ready.notify_one();
    }

    unsafe extern "C" fn noop(_: *mut c_void) {}

    /// Normal pool round-trip: submit, execute, stop.
    ///
    /// Waits on a completion latch (Mutex+Condvar owned by the test) instead
    /// of sleeping a fixed duration, so the assertion only fires once the
    /// worker has actually run the task. The wait is bounded so a genuinely
    /// deadlocked worker still fails the test loudly instead of hanging.
    #[test]
    fn normal_submit_and_stop() {
        // SAFETY: test-only — we control the pool lifetime and task pointer.
        unsafe {
            let pool = hew_blocking_pool_new();

            let signal = Arc::new(SignalingCounter {
                count: AtomicU32::new(0),
                done: Mutex::new(false),
                ready: Condvar::new(),
            });
            let c = Arc::clone(&signal);
            let c_ptr = Arc::into_raw(c) as *mut c_void;

            assert_eq!(
                hew_blocking_pool_submit(pool, increment_and_signal, c_ptr),
                0
            );

            let wait_budget = Duration::from_secs(5);
            let start = std::time::Instant::now();
            let mut done = signal.done.lock_or_recover();
            while !*done {
                let elapsed = start.elapsed();
                let Some(remaining) = wait_budget.checked_sub(elapsed) else {
                    panic!(
                        "blocking pool worker did not complete the submitted task \
                         within {wait_budget:?}; worker likely deadlocked"
                    );
                };
                let (next_done, wait_result) =
                    signal.ready.wait_timeout_or_recover(done, remaining);
                done = next_done;
                assert!(
                    !wait_result.timed_out() || *done,
                    "blocking pool worker did not complete the submitted task \
                     within {wait_budget:?}; worker likely deadlocked"
                );
            }
            drop(done);

            assert_eq!(signal.count.load(Ordering::Relaxed), 1);

            hew_blocking_pool_stop(pool);
        }
    }

    /// Submit to a null pool returns -1.
    #[test]
    fn submit_null_pool_returns_error() {
        // SAFETY: null pointer is the condition under test.
        unsafe {
            assert_eq!(
                hew_blocking_pool_submit(std::ptr::null_mut(), noop, std::ptr::null_mut()),
                -1
            );
        }
    }

    /// Stop on a null pointer is a no-op (no crash).
    #[test]
    fn stop_null_pool_is_noop() {
        // SAFETY: null pointer is the condition under test.
        unsafe {
            hew_blocking_pool_stop(std::ptr::null_mut());
        }
    }

    /// A gate the test opens to release every blocked job at once.
    struct Gate {
        open: Mutex<bool>,
        opened: Condvar,
        order: Mutex<Vec<usize>>,
        finished: Condvar,
    }

    struct GatedJob {
        gate: Arc<Gate>,
        id: usize,
    }

    unsafe extern "C" fn gated_job(arg: *mut c_void) {
        // SAFETY: each submission transfers one boxed job.
        let job = unsafe { Box::from_raw(arg.cast::<GatedJob>()) };
        let mut open = job.gate.open.lock_or_recover();
        while !*open {
            open = job.gate.opened.wait_or_recover(open);
        }
        drop(open);
        job.gate.order.lock_or_recover().push(job.id);
        job.gate.finished.notify_all();
    }

    fn submit_gated(pool: *mut HewBlockingPool, gate: &Arc<Gate>, id: usize) {
        let job = Box::into_raw(Box::new(GatedJob {
            gate: Arc::clone(gate),
            id,
        }));
        // SAFETY: the pool is live and the job box is transferred.
        assert_eq!(
            unsafe { hew_blocking_pool_submit(pool, gated_job, job.cast()) },
            0
        );
    }

    fn new_gate() -> Arc<Gate> {
        Arc::new(Gate {
            open: Mutex::new(false),
            opened: Condvar::new(),
            order: Mutex::new(Vec::new()),
            finished: Condvar::new(),
        })
    }

    fn wait_for(gate: &Gate, count: usize) {
        let mut order = gate.order.lock_or_recover();
        while order.len() < count {
            let (next, waited) = gate
                .finished
                .wait_timeout_or_recover(order, Duration::from_secs(10));
            order = next;
            assert!(!waited.timed_out() || order.len() >= count, "jobs stalled");
        }
    }

    /// Three times the cap completes: the pool grows to its cap, queues the
    /// rest (counting each as saturation) and runs queued jobs in submission
    /// order. With a short idle timeout every thread then exits.
    #[test]
    fn saturated_pool_runs_fifo_then_shrinks_when_idle() {
        let cap = 4;
        let pool = new_pool(cap, Duration::from_millis(50));
        // SAFETY: the pool lives until stopped below.
        let handle = unsafe { &*pool };
        let gate = new_gate();
        for id in 0..cap * 3 {
            submit_gated(pool, &gate, id);
        }
        assert_eq!(
            handle.thread_count(),
            cap,
            "pool grows to its cap and no further"
        );
        assert_eq!(
            handle.saturation_count(),
            (cap * 2) as u64,
            "every job past the cap queued behind a busy pool"
        );
        *gate.open.lock_or_recover() = true;
        gate.opened.notify_all();
        wait_for(&gate, cap * 3);
        let order = gate.order.lock_or_recover().clone();
        // The first `cap` jobs ran concurrently; the queued rest ran FIFO.
        assert_eq!(&order[cap..], &(cap..cap * 3).collect::<Vec<_>>()[..]);
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while handle.thread_count() != 0 {
            assert!(
                std::time::Instant::now() < deadline,
                "idle threads must exit after the idle timeout"
            );
            std::thread::sleep(Duration::from_millis(5));
        }
        // A job after the pool shrank to nothing still runs.
        submit_gated(pool, &gate, 99);
        wait_for(&gate, cap * 3 + 1);
        // SAFETY: the pool is live and not used after this call.
        unsafe { hew_blocking_pool_stop(pool) };
    }

    /// An idle thread takes the next job instead of a new thread starting.
    #[test]
    fn idle_thread_is_reused_before_growing() {
        let pool = new_pool(8, Duration::from_secs(60));
        // SAFETY: the pool lives until stopped below.
        let handle = unsafe { &*pool };
        let gate = new_gate();
        *gate.open.lock_or_recover() = true;
        submit_gated(pool, &gate, 0);
        wait_for(&gate, 1);
        // Let the worker settle into its idle wait.
        let deadline = std::time::Instant::now() + Duration::from_secs(10);
        while handle.inner.queue.lock_or_recover().idle != 1 {
            assert!(std::time::Instant::now() < deadline, "worker never idled");
            std::thread::sleep(Duration::from_millis(1));
        }
        submit_gated(pool, &gate, 1);
        wait_for(&gate, 2);
        assert_eq!(handle.thread_count(), 1);
        assert_eq!(handle.saturation_count(), 0);
        // SAFETY: the pool is live and not used after this call.
        unsafe { hew_blocking_pool_stop(pool) };
    }

    /// Off-dispatch fallback: a closure run on a blocking-pool worker thread
    /// resolves `rt_current()` through the `DEFAULT_RUNTIME` fallback, NOT a TLS
    /// install.
    ///
    /// The pool worker never calls `enter()` (its `CURRENT_RUNTIME` slot stays
    /// null), so a submitted closure that touches a runtime authority must fall
    /// through to the installed default rather than trap. This is the
    /// blocking-pool arm of the off-dispatch inventory: the only production
    /// closure (`transport::resolve_addr_via_pool`'s `getaddrinfo`) reads no
    /// authority today, but were one to, the fallback must carry it. A TLS-only
    /// `rt_current()` would panic here on the pool thread.
    #[test]
    fn pool_closure_resolves_runtime_via_default_fallback() {
        // Install a worker-less default `RuntimeInner` (serialized on the
        // default-runtime slot) so `rt_current()` has a default to fall back to.
        let _guard = crate::runtime_test_guard();
        let want = crate::runtime::default_runtime_ptr(Ordering::SeqCst);
        assert!(!want.is_null(), "guard must install a default runtime");

        // SAFETY: test owns pool lifetime.
        unsafe {
            let pool = hew_blocking_pool_new();

            // The closure runs on a pool worker thread, off the dispatch path.
            // Resolve `rt_current()` there and report the address it selected
            // (a raw pointer is not `Send`; the address is what we compare).
            let got = spawn_blocking_result(
                pool,
                || std::ptr::from_ref(crate::runtime::rt_current()) as usize,
                None,
            )
            .expect("pool closure must complete");

            assert_eq!(
                got, want as usize,
                "an off-dispatch pool worker must resolve the default runtime via \
                 the rt_current() fallback (null TLS), not trap or pick a different \
                 runtime"
            );

            hew_blocking_pool_stop(pool);
        }
    }

    /// Normal completion: closure returns a value, caller observes it.
    #[test]
    fn spawn_blocking_result_normal_completion() {
        // SAFETY: test owns pool lifetime.
        unsafe {
            let pool = hew_blocking_pool_new();

            let result = spawn_blocking_result(pool, || 42_i32, None);
            assert_eq!(result, Ok(42));

            // Follow-up call on the same pool also succeeds — proves no
            // shared state was wedged.
            let result2 = spawn_blocking_result(pool, || "hello".to_string(), None);
            assert_eq!(result2.as_deref(), Ok("hello"));

            hew_blocking_pool_stop(pool);
        }
    }

    #[test]
    fn worker_panic_reports_and_pool_survives() {
        crate::hew_clear_error();

        // SAFETY: test owns pool lifetime.
        unsafe {
            let pool = hew_blocking_pool_new();

            let result = spawn_blocking_result::<(), _>(
                pool,
                || panic!("blocking worker intentional panic"),
                None,
            );
            assert_eq!(result, Err(BlockingPoolError::WorkerPanicked));

            let error_ptr = crate::hew_last_error();
            assert!(
                !error_ptr.is_null(),
                "a panicking blocking worker must record a diagnostic"
            );
            // SAFETY: spawn_blocking_result populated this thread's last-error slot.
            let error = CStr::from_ptr(error_ptr)
                .to_str()
                .expect("last error should be utf-8");
            assert!(
                error.contains("blocking pool worker panicked"),
                "unexpected last error: {error}"
            );
            assert!(
                error.contains("blocking worker intentional panic"),
                "panic payload must be included in the diagnostic: {error}"
            );

            let following = spawn_blocking_result(pool, || 41_i32 + 1, None);
            assert_eq!(following, Ok(42), "the pool queue must remain serviceable");

            hew_blocking_pool_stop(pool);
        }
    }

    #[test]
    fn hostile_worker_panic_payload_is_quarantined_and_pool_survives() {
        static PAYLOAD_DROPS: AtomicUsize = AtomicUsize::new(0);

        struct HostilePayload;
        impl Drop for HostilePayload {
            fn drop(&mut self) {
                PAYLOAD_DROPS.fetch_add(1, Ordering::SeqCst);
                panic!("hostile blocking-worker payload destructor");
            }
        }

        PAYLOAD_DROPS.store(0, Ordering::SeqCst);
        // SAFETY: test owns the pool and stops it after all submissions finish.
        unsafe {
            let pool = hew_blocking_pool_new();
            let result = spawn_blocking_result::<(), _>(
                pool,
                || std::panic::panic_any(HostilePayload),
                None,
            );
            assert_eq!(result, Err(BlockingPoolError::WorkerPanicked));
            assert_eq!(PAYLOAD_DROPS.load(Ordering::SeqCst), 1);
            assert_eq!(spawn_blocking_result(pool, || 42, None), Ok(42));
            hew_blocking_pool_stop(pool);
        }
    }

    /// Timeout: closure runs longer than deadline; caller returns `TimedOut`
    /// and the pool thread cleanly publishes its discarded result. After the
    /// timeout, a follow-up call on the same pool succeeds (no held lock).
    #[test]
    fn spawn_blocking_result_timeout_discards_result() {
        // SAFETY: test owns pool lifetime.
        unsafe {
            let pool = hew_blocking_pool_new();

            // Use a Barrier so the test does not depend on wall-clock racing.
            // The task waits for the test to release it; the test waits up to
            // 100 ms which deterministically expires before the release.
            let release = Arc::new(std::sync::Barrier::new(2));
            let release_clone = Arc::clone(&release);

            let result = spawn_blocking_result(
                pool,
                move || {
                    release_clone.wait();
                    99_u64
                },
                Some(Duration::from_millis(50)),
            );
            assert_eq!(result, Err(BlockingPoolError::TimedOut));

            // Release the worker so it can publish its discarded result and
            // free the slot. Without this the test thread leaks a barrier
            // refcount but does not deadlock.
            release.wait();

            // Follow-up call composes: pool is not wedged.
            let result2 = spawn_blocking_result(pool, || 7_i32, None);
            assert_eq!(result2, Ok(7));

            hew_blocking_pool_stop(pool);
        }
    }

    /// `shared_blocking_pool` returns the same pointer on every call within one
    /// runtime, and the pool is usable.
    #[test]
    fn shared_blocking_pool_is_idempotent_and_usable() {
        // The pool now resolves through `rt_current()`, so a runtime must be
        // installed; the guard installs a worker-less default and reclaims it
        // (stopping the pool) on drop.
        let _runtime = crate::runtime_test_guard();

        let p1 = shared_blocking_pool();
        let p2 = shared_blocking_pool();
        assert_eq!(p1, p2, "the pool is a per-runtime singleton");
        assert!(!p1.is_null(), "the pool must be allocated");

        // Submit through the pool and observe the result. This proves it is
        // wired through `spawn_blocking_result` correctly.
        // SAFETY: shared_blocking_pool returns a valid pool for the installed
        // runtime; it stays valid until the guard drops it below.
        let result = unsafe { spawn_blocking_result(p1, || 1729_u32, None) };
        assert_eq!(result, Ok(1729));
    }

    /// Two live runtimes own DIFFERENT blocking pools — the deglob's core teeth.
    /// Before CAP-09's residual close, a process-global `SHARED_POOL` singleton
    /// served both; now each `RuntimeInner` lazily owns its own, and both are
    /// independently usable. Completing under the lane timeout is the join proof:
    /// dropping each runtime stops its own pool without hanging.
    #[test]
    fn two_runtimes_do_not_share_the_blocking_pool() {
        let _lock = crate::scheduler::SchedTestLock::acquire();

        let rt_a = crate::runtime::RuntimeInner::new(crate::scheduler::worker_less_scheduler());
        let rt_b = crate::runtime::RuntimeInner::new(crate::scheduler::worker_less_scheduler());

        // Resolve each runtime's pool while it is the entered current runtime.
        let pool_a = {
            // SAFETY: rt_a is a stack local that outlives this enter guard.
            let _enter = unsafe { crate::runtime::enter(&rt_a) };
            shared_blocking_pool()
        };
        let pool_b = {
            // SAFETY: rt_b is a stack local that outlives this enter guard.
            let _enter = unsafe { crate::runtime::enter(&rt_b) };
            shared_blocking_pool()
        };

        assert!(!pool_a.is_null() && !pool_b.is_null());
        assert_ne!(
            pool_a, pool_b,
            "each runtime must own a distinct blocking pool, not share one global"
        );

        // Both pools are live and independently usable.
        // SAFETY: pool_a came from rt_a's live pool; rt_a is still in scope.
        let ran_a = unsafe { spawn_blocking_result(pool_a, || 41_i32 + 1, None) };
        // SAFETY: pool_b came from rt_b's live pool; rt_b is still in scope.
        let ran_b = unsafe { spawn_blocking_result(pool_b, || 20_i32 + 22, None) };
        assert_eq!(ran_a, Ok(42));
        assert_eq!(ran_b, Ok(42));

        // Dropping rt_a then rt_b stops each pool (joins its workers); reaching
        // the end of the test without hanging is the teardown-ordering proof.
    }

    /// Dropping a runtime stops its pool; a subsequent runtime gets a fresh,
    /// usable pool rather than a stale/stopped handle. The drop must not hang
    /// (its pool joins its own workers), and the fresh pool must accept work —
    /// a stopped handle would return `PoolStopped`.
    #[test]
    fn runtime_drop_stops_its_pool_then_fresh_runtime_gets_a_live_pool() {
        let _lock = crate::scheduler::SchedTestLock::acquire();

        {
            let rt = crate::runtime::RuntimeInner::new(crate::scheduler::worker_less_scheduler());
            // SAFETY: rt is a stack local that outlives its enter guard.
            let _enter = unsafe { crate::runtime::enter(&rt) };
            let pool = shared_blocking_pool();
            // SAFETY: pool is this runtime's live pool.
            let ran = unsafe { spawn_blocking_result(pool, || 7_i32, None) };
            assert_eq!(ran, Ok(7));
            // rt drops here: its pool is stopped (workers joined) without hanging.
        }

        let rt2 = crate::runtime::RuntimeInner::new(crate::scheduler::worker_less_scheduler());
        // SAFETY: rt2 is a stack local that outlives its enter guard.
        let _enter = unsafe { crate::runtime::enter(&rt2) };
        let pool2 = shared_blocking_pool();
        assert!(
            !pool2.is_null(),
            "the fresh runtime lazily creates a new pool"
        );
        // A stale/stopped handle would reject work; a live fresh pool accepts it.
        // SAFETY: pool2 is rt2's live pool.
        let ran = unsafe { spawn_blocking_result(pool2, || 99_i32, None) };
        assert_eq!(ran, Ok(99));
    }

    /// `shared_blocking_pool()` is an init/mutate site (it spawns threads), so it
    /// resolves through the fail-closed `rt_current()`. With no runtime installed
    /// it panics rather than fabricating a process-global pool — pinning the
    /// `deglobalize-reads-stay-tolerant` / `no-fail-open-fallback-after-authority`
    /// classification. If this panic is ever seen from a production path, that is
    /// scope-guard condition 1 (a no-runtime caller).
    #[test]
    fn shared_blocking_pool_without_runtime_fails_closed() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the default runtime slot to be empty"
        );

        let panicked = std::panic::catch_unwind(shared_blocking_pool).is_err();
        assert!(
            panicked,
            "shared_blocking_pool with no runtime installed must fail closed (panic)"
        );
    }

    /// `shared_blocking_pool_opt` is the fail-closed sibling C-ABI offload
    /// entrypoints resolve through: `None` with no runtime installed (so the
    /// entrypoint returns a clean error instead of aborting), `Some` live pool
    /// once a runtime is installed.
    #[test]
    fn shared_blocking_pool_opt_reflects_runtime_presence() {
        let _lock = crate::scheduler::SchedTestLock::acquire();
        assert!(
            crate::runtime::rt_default().is_none(),
            "test requires the default runtime slot to be empty"
        );
        assert!(
            shared_blocking_pool_opt().is_none(),
            "no runtime installed must resolve to None, not a fabricated pool"
        );

        let rt = crate::runtime::RuntimeInner::new(crate::scheduler::worker_less_scheduler());
        // SAFETY: rt is a stack local that outlives its enter guard.
        let _enter = unsafe { crate::runtime::enter(&rt) };
        let pool = shared_blocking_pool_opt().expect("a runtime is installed");
        assert!(!pool.is_null(), "an installed runtime yields a live pool");
        // SAFETY: pool is the installed runtime's live pool.
        let ran = unsafe { spawn_blocking_result(pool, || 7_u32, None) };
        assert_eq!(ran, Ok(7));
    }

    /// Null pool: caller gets `PoolStopped` without leaking the boxed task
    /// ctx. Post-stop submit (a stopped-but-not-freed pool) is unreachable
    /// through the safe API: `hew_blocking_pool_stop` frees the Box. The
    /// `PoolStopped` branch in `spawn_blocking_result` is therefore covered
    /// by the null pointer path here.
    #[test]
    fn spawn_blocking_result_null_pool_returns_error() {
        // SAFETY: null pointer is the condition under test.
        let result = unsafe { spawn_blocking_result::<i32, _>(std::ptr::null_mut(), || 1, None) };
        assert_eq!(result, Err(BlockingPoolError::PoolStopped));
    }
}
