//! Hew runtime: counting semaphore for concurrency control.

use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// ── Counting semaphore ──────────────────────────────────────────────────

/// A counting semaphore.
#[derive(Debug)]
pub struct HewSemaphore {
    count: Mutex<i32>,
    condvar: Condvar,
}

// ── Constructor ─────────────────────────────────────────────────────────

/// Create a semaphore with the given initial count.
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_semaphore_free`].
#[no_mangle]
pub extern "C" fn hew_semaphore_new(initial_count: i32) -> *mut HewSemaphore {
    Box::into_raw(Box::new(HewSemaphore {
        count: Mutex::new(initial_count),
        condvar: Condvar::new(),
    }))
}

// ── Acquire ─────────────────────────────────────────────────────────────

/// Decrement the semaphore count, blocking if the count is zero.
///
/// # Safety
///
/// `sem` must be a valid pointer returned by [`hew_semaphore_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire(sem: *mut HewSemaphore) {
    if sem.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `sem` is a valid, properly aligned pointer.
    let sem = unsafe { &*sem };
    let mut guard = match sem.count.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    while *guard <= 0 {
        guard = match sem.condvar.wait(guard) {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
    }
    *guard -= 1;
}

/// Try to decrement the semaphore count without blocking.
///
/// Returns 1 if acquired, 0 if the semaphore would block.
///
/// # Safety
///
/// `sem` must be a valid pointer returned by [`hew_semaphore_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_try_acquire(sem: *mut HewSemaphore) -> i32 {
    if sem.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees `sem` is a valid, properly aligned pointer.
    let sem = unsafe { &*sem };
    let mut guard = match sem.count.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    if *guard > 0 {
        *guard -= 1;
        1
    } else {
        0
    }
}

/// Acquire the semaphore with a timeout.
///
/// Returns 1 if acquired, 0 if the timeout expired.
///
/// # Safety
///
/// `sem` must be a valid pointer returned by [`hew_semaphore_new`].
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "C ABI: timeout_ms validated non-negative via max(0)"
)]
pub unsafe extern "C" fn hew_semaphore_acquire_timeout(
    sem: *mut HewSemaphore,
    timeout_ms: i64,
) -> i32 {
    if sem.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees `sem` is a valid, properly aligned pointer.
    let sem = unsafe { &*sem };
    let deadline = Instant::now() + Duration::from_millis(timeout_ms.max(0) as u64);
    let mut guard = match sem.count.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };

    while *guard <= 0 {
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return 0;
        }
        let (new_guard, wait_result) = match sem.condvar.wait_timeout(guard, remaining) {
            Ok(result) => result,
            Err(e) => e.into_inner(),
        };
        guard = new_guard;
        if wait_result.timed_out() && *guard <= 0 {
            return 0;
        }
    }

    *guard -= 1;
    1
}

// ── Release ─────────────────────────────────────────────────────────────

/// Increment the semaphore count and wake one waiting thread.
///
/// # Safety
///
/// `sem` must be a valid pointer returned by [`hew_semaphore_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_release(sem: *mut HewSemaphore) {
    if sem.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `sem` is a valid, properly aligned pointer.
    let sem = unsafe { &*sem };
    let mut guard = match sem.count.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    };
    *guard += 1;
    drop(guard);
    sem.condvar.notify_one();
}

// ── Query ───────────────────────────────────────────────────────────────

/// Return the current semaphore count (a snapshot; may be stale).
///
/// # Safety
///
/// `sem` must be a valid pointer returned by [`hew_semaphore_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_count(sem: *const HewSemaphore) -> i32 {
    if sem.is_null() {
        return 0;
    }

    // SAFETY: Caller guarantees `sem` is a valid, properly aligned pointer.
    let sem = unsafe { &*sem };
    *match sem.count.lock() {
        Ok(g) => g,
        Err(e) => e.into_inner(),
    }
}

// ── Cleanup ─────────────────────────────────────────────────────────────

/// Free a semaphore.
///
/// # Safety
///
/// `sem` must have been returned by [`hew_semaphore_new`] and must not be
/// used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_free(sem: *mut HewSemaphore) {
    if sem.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `sem` was Box-allocated and is exclusively owned.
    unsafe {
        drop(Box::from_raw(sem));
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn create_and_free() {
        let sem = hew_semaphore_new(5);
        assert!(!sem.is_null());
        // SAFETY: `sem` was just created above.
        unsafe {
            assert_eq!(hew_semaphore_count(sem), 5);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn acquire_release_basic() {
        let sem = hew_semaphore_new(2);
        // SAFETY: `sem` was just created above.
        unsafe {
            hew_semaphore_acquire(sem);
            assert_eq!(hew_semaphore_count(sem), 1);
            hew_semaphore_acquire(sem);
            assert_eq!(hew_semaphore_count(sem), 0);
            hew_semaphore_release(sem);
            assert_eq!(hew_semaphore_count(sem), 1);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn try_acquire_succeeds_and_fails() {
        let sem = hew_semaphore_new(1);
        // SAFETY: `sem` was just created above.
        unsafe {
            assert_eq!(hew_semaphore_try_acquire(sem), 1);
            assert_eq!(hew_semaphore_count(sem), 0);
            assert_eq!(hew_semaphore_try_acquire(sem), 0);
            hew_semaphore_release(sem);
            assert_eq!(hew_semaphore_try_acquire(sem), 1);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn count_snapshot() {
        let sem = hew_semaphore_new(3);
        // SAFETY: `sem` was just created above.
        unsafe {
            assert_eq!(hew_semaphore_count(sem), 3);
            hew_semaphore_acquire(sem);
            assert_eq!(hew_semaphore_count(sem), 2);
            hew_semaphore_release(sem);
            hew_semaphore_release(sem);
            assert_eq!(hew_semaphore_count(sem), 4);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn concurrent_access() {
        let sem = hew_semaphore_new(0);
        // Wrap in Arc via a newtype so we can share across threads.
        let sem_addr = sem as usize;

        let producer = thread::spawn(move || {
            let sem = sem_addr as *mut HewSemaphore;
            for _ in 0..5 {
                // SAFETY: `sem` is valid for the lifetime of this test.
                unsafe {
                    hew_semaphore_release(sem);
                }
            }
        });

        let consumer = thread::spawn(move || {
            let sem = sem_addr as *mut HewSemaphore;
            for _ in 0..5 {
                // SAFETY: `sem` is valid for the lifetime of this test.
                unsafe {
                    hew_semaphore_acquire(sem);
                }
            }
        });

        producer.join().expect("producer panicked");
        consumer.join().expect("consumer panicked");

        // SAFETY: Both threads finished; `sem` is exclusively ours.
        unsafe {
            assert_eq!(hew_semaphore_count(sem), 0);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn acquire_timeout_expires() {
        let sem = hew_semaphore_new(0);
        // SAFETY: `sem` was just created above.
        unsafe {
            let result = hew_semaphore_acquire_timeout(sem, 10);
            assert_eq!(result, 0);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn acquire_timeout_succeeds() {
        let sem = hew_semaphore_new(1);
        // SAFETY: `sem` was just created above.
        unsafe {
            let result = hew_semaphore_acquire_timeout(sem, 100);
            assert_eq!(result, 1);
            assert_eq!(hew_semaphore_count(sem), 0);
            hew_semaphore_free(sem);
        }
    }
}
