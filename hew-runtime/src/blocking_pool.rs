//! Fixed-size thread pool for offloading blocking work.
//!
//! Provides a simple pool of `HEW_BLOCKING_POOL_SIZE` (4) worker threads that
//! drain a shared task queue. The pool is opaque to C callers (Box-allocated).

use std::ffi::c_void;
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::thread::JoinHandle;
use std::time::Duration;

use crate::util::{CondvarExt, MutexExt};

/// Number of worker threads in the blocking pool.
pub const HEW_BLOCKING_POOL_SIZE: usize = 4;

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

/// Shared state between the pool handle and worker threads.
struct PoolInner {
    queue: Mutex<(Vec<Task>, bool)>, // (tasks, running)
    condvar: Condvar,
}

/// Fixed-size blocking thread pool.
pub struct HewBlockingPool {
    inner: std::sync::Arc<PoolInner>,
    workers: Vec<JoinHandle<()>>,
}

impl std::fmt::Debug for HewBlockingPool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewBlockingPool")
            .field("workers", &self.workers.len())
            .finish_non_exhaustive()
    }
}

/// Create a new blocking pool with [`HEW_BLOCKING_POOL_SIZE`] worker threads.
///
/// Returns a heap-allocated, opaque pool pointer.
///
/// # Safety
///
/// No preconditions. The caller must eventually call [`hew_blocking_pool_stop`]
/// to free the pool.
#[no_mangle]
pub unsafe extern "C" fn hew_blocking_pool_new() -> *mut HewBlockingPool {
    let inner = std::sync::Arc::new(PoolInner {
        queue: Mutex::new((Vec::new(), true)),
        condvar: Condvar::new(),
    });

    let mut workers = Vec::with_capacity(HEW_BLOCKING_POOL_SIZE);
    for _ in 0..HEW_BLOCKING_POOL_SIZE {
        let shared = std::sync::Arc::clone(&inner);
        workers.push(std::thread::spawn(move || {
            worker_loop(&shared);
        }));
    }

    Box::into_raw(Box::new(HewBlockingPool { inner, workers }))
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
    let mut guard = p.inner.queue.lock_or_recover();
    let (ref mut queue, running) = *guard;
    if !running {
        return -1;
    }
    queue.push(Task { func, arg });
    p.inner.condvar.notify_one();
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
}

/// Result slot shared between the caller (waits) and the worker (writes).
struct Slot<T> {
    value: Mutex<Option<T>>,
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
    let value = f();
    // Publish the result. If the caller has already given up (timed out),
    // the value is simply stored and dropped when the last Arc drops.
    let mut guard = slot.value.lock_or_recover();
    *guard = Some(value);
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
/// WASM-TODO(#1451): there is no blocking pool on WASM; until a
/// WASM-compatible deadline primitive exists (web-sys + Promise
/// integration, wasi-threads, or polled futures), transport callers on WASM
/// keep their pre-existing unguarded blocking shape.
///
/// # Errors
///
/// Returns `Err(BlockingPoolError::PoolStopped)` if `pool` is null or the
/// pool has been stopped (so the task could not be enqueued). Returns
/// `Err(BlockingPoolError::TimedOut)` if `deadline` is `Some(d)` and `d`
/// elapses before the worker publishes a result.
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
        value: Mutex::new(None),
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
    let mut guard = slot.value.lock_or_recover();
    match deadline {
        None => {
            while guard.is_none() {
                guard = slot.ready.wait_or_recover(guard);
            }
            Ok(guard.take().expect("value was just observed Some"))
        }
        Some(d) => {
            let start = std::time::Instant::now();
            while guard.is_none() {
                let elapsed = start.elapsed();
                let Some(remaining) = d.checked_sub(elapsed) else {
                    return Err(BlockingPoolError::TimedOut);
                };
                let (next_guard, wait_result) =
                    slot.ready.wait_timeout_or_recover(guard, remaining);
                guard = next_guard;
                if wait_result.timed_out() && guard.is_none() {
                    // cleanup-all-exits: dropping `guard` releases the lock
                    // before we return; `slot` (Arc) stays alive on the worker
                    // side and is freed when the worker publishes and drops.
                    return Err(BlockingPoolError::TimedOut);
                }
            }
            Ok(guard.take().expect("value was just observed Some"))
        }
    }
}

/// Process-lifetime singleton wrapper around `*mut HewBlockingPool`.
///
/// `*mut T` is neither `Send` nor `Sync`. This wrapper asserts both based on
/// the pool's internal Mutex/Condvar synchronization. The pointer's allocation
/// lives for the duration of the process — the singleton is never stopped, so
/// the inner pointer never dangles.
struct SharedPoolHandle(*mut HewBlockingPool);

// SAFETY: `HewBlockingPool` is heap-allocated by `hew_blocking_pool_new` and
// internally synchronized via Mutex+Condvar. The singleton lives for the
// lifetime of the process and is never freed via `hew_blocking_pool_stop`,
// so the raw pointer remains valid for any thread that observes it.
unsafe impl Send for SharedPoolHandle {}
// SAFETY: see Send. All access goes through the pool's own internal locks.
unsafe impl Sync for SharedPoolHandle {}

static SHARED_POOL: OnceLock<SharedPoolHandle> = OnceLock::new();

/// Return the process-wide blocking pool, creating it on first call.
///
/// The returned pointer is valid for the lifetime of the process. It is
/// shared across all transport callers (DNS, TCP, HTTP, etc.) that need to
/// offload blocking syscalls with a deadline. Callers MUST NOT pass this
/// pointer to [`hew_blocking_pool_stop`] — the singleton is never stopped.
///
/// Idempotent: subsequent calls return the same pointer.
///
/// WASM-TODO(#1451): there is no blocking pool on WASM; transport callers
/// on WASM keep their pre-existing unguarded blocking shape until a
/// WASM-compatible deadline primitive exists.
pub fn shared_blocking_pool() -> *mut HewBlockingPool {
    SHARED_POOL
        .get_or_init(|| {
            // SAFETY: hew_blocking_pool_new is safe to call (no preconditions);
            // it returns a heap-allocated pool that is owned by the singleton
            // and never freed.
            let raw = unsafe { hew_blocking_pool_new() };
            SharedPoolHandle(raw)
        })
        .0
}

/// Stop the pool: reject new work, wake all workers, and join threads.
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
    let mut p = unsafe { *Box::from_raw(pool) };

    // Signal workers to stop.
    {
        let mut guard = p.inner.queue.lock_or_recover();
        guard.1 = false; // running = false
    }
    p.inner.condvar.notify_all();

    // Join all worker threads.
    for handle in p.workers.drain(..) {
        let _ = handle.join();
    }
}

/// Worker thread main loop.
fn worker_loop(inner: &PoolInner) {
    loop {
        let task = {
            let mut guard = inner.queue.lock_or_recover();
            loop {
                let (ref mut queue, running) = *guard;
                if let Some(t) = queue.pop() {
                    break Some(t);
                }
                if !running {
                    break None;
                }
                guard = inner.condvar.wait_or_recover(guard);
            }
        };
        match task {
            Some(t) => {
                // SAFETY: the submitter guarantees `func` and `arg` are valid.
                unsafe {
                    (t.func)(t.arg);
                }
            }
            None => return,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicU32, Ordering};

    unsafe extern "C" fn increment(arg: *mut c_void) {
        // SAFETY: caller passes an Arc::into_raw'd AtomicU32 pointer.
        let c = unsafe { Arc::from_raw(arg as *const AtomicU32) };
        c.fetch_add(1, Ordering::Relaxed);
    }

    unsafe extern "C" fn noop(_: *mut c_void) {}

    /// Normal pool round-trip: submit, execute, stop.
    #[test]
    fn normal_submit_and_stop() {
        // SAFETY: test-only — we control the pool lifetime and task pointer.
        unsafe {
            let pool = hew_blocking_pool_new();

            let counter = Arc::new(AtomicU32::new(0));
            let c = Arc::clone(&counter);
            let c_ptr = Arc::into_raw(c) as *mut c_void;

            assert_eq!(hew_blocking_pool_submit(pool, increment, c_ptr), 0);
            std::thread::sleep(std::time::Duration::from_millis(50));
            assert_eq!(counter.load(Ordering::Relaxed), 1);

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

    /// A poisoned pool mutex does not cascade via `lock_or_recover`.
    ///
    /// We build a `PoolInner` directly so we can poison its mutex from a
    /// regular Rust thread (extern "C" fns abort on panic, so we can't
    /// poison through the task callback).
    #[test]
    fn poisoned_mutex_does_not_cascade() {
        let inner = Arc::new(PoolInner {
            queue: Mutex::new((Vec::new(), true)),
            condvar: Condvar::new(),
        });

        // Poison the mutex: acquire it in a thread that panics.
        let shared = Arc::clone(&inner);
        let handle = std::thread::spawn(move || {
            let _guard = shared.queue.lock().unwrap();
            panic!("intentional poison");
        });
        let _ = handle.join(); // join collects the panic

        // The mutex is now poisoned. Verify lock_or_recover succeeds.
        let guard = inner.queue.lock_or_recover();
        assert!(guard.1, "running flag should still be true");
        drop(guard);

        // Condvar wait_or_recover also tolerates the poisoned state.
        // (We can't easily test wait without a second thread, but
        // lock_or_recover proves the PoisonError path works.)
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
            let result2 = spawn_blocking_result(pool, || 7_i32, Some(Duration::from_millis(500)));
            assert_eq!(result2, Ok(7));

            hew_blocking_pool_stop(pool);
        }
    }

    /// `shared_blocking_pool` returns the same pointer on every call and
    /// the pool is usable.
    #[test]
    fn shared_blocking_pool_is_idempotent_and_usable() {
        let p1 = shared_blocking_pool();
        let p2 = shared_blocking_pool();
        assert_eq!(p1, p2, "shared pool must be a singleton");
        assert!(!p1.is_null(), "shared pool must be allocated");

        // Submit through the shared pool and observe the result. This proves
        // the singleton is wired through `spawn_blocking_result` correctly.
        // SAFETY: shared_blocking_pool returns a valid, never-stopped pool.
        let result = unsafe { spawn_blocking_result(p1, || 1729_u32, None) };
        assert_eq!(result, Ok(1729));
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
