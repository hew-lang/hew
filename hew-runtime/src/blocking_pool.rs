//! Fixed-size thread pool for offloading blocking work.
//!
//! Provides a simple pool of `HEW_BLOCKING_POOL_SIZE` (4) worker threads that
//! drain a shared task queue. The pool is opaque to C callers (Box-allocated).

use std::ffi::c_void;
use std::sync::{Condvar, Mutex};
use std::thread::JoinHandle;

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
    let mut guard = p.inner.queue.lock().unwrap();
    let (ref mut queue, running) = *guard;
    if !running {
        return -1;
    }
    queue.push(Task { func, arg });
    p.inner.condvar.notify_one();
    0
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
    if let Ok(mut guard) = p.inner.queue.lock() {
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
            let mut guard = inner.queue.lock().unwrap();
            loop {
                let (ref mut queue, running) = *guard;
                if let Some(t) = queue.pop() {
                    break Some(t);
                }
                if !running {
                    break None;
                }
                guard = inner.condvar.wait(guard).unwrap();
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
