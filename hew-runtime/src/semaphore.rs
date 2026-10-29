//! Counting semaphores with owned, cancellable permit acquisition.
//!
//! The count and wait queue share one lock. An acquisition reserves its permit
//! until taken; abandoning that reservation transfers it to another waiter or
//! returns it to the count. The same operation serves coroutine and host waits.

use crate::runtime_sync::{AtomicI32, Mutex};
use crate::timer_wheel::{
    hew_timer_wheel_remove, timer_wheel_schedule_precise_handle, HewTimerHandle, HewTimerWheel,
};
use crate::util::MutexExt;
use crate::wake::{HewWaker, OwnedWaker};
use std::collections::VecDeque;
use std::ffi::c_void;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Weak};

const PENDING: i32 = 0;
const ACQUIRED: i32 = 1;
const TIMED_OUT: i32 = 2;
const CANCELLED: i32 = 3;
const FAULT: i32 = 4;
const TAKEN: i32 = 5;

#[derive(Debug)]
struct State {
    count: i32,
    waiters: VecDeque<Weak<Waiter>>,
}

/// An owning semaphore handle. In-flight acquisitions retain its backing.
#[derive(Debug)]
pub struct HewSemaphore {
    state: Arc<Mutex<State>>,
}

#[derive(Debug)]
struct Waiter {
    state: Arc<Mutex<State>>,
    // Every transition is serialized by `state`; atomic reads allow status
    // inspection without retaining the count lock across compiler operations.
    status: AtomicI32,
    waker: OwnedWaker,
}

/// One acquired-or-queued permit and its optional shared-wheel deadline.
#[derive(Debug)]
pub struct HewSemaphoreAcquire {
    waiter: Arc<Waiter>,
    wheel: *mut HewTimerWheel,
    timer: HewTimerHandle,
}

// SAFETY: the operation's backing and waiter are synchronized. The runtime
// owns the wheel, whose handle removal validates pointer and generation.
unsafe impl Send for HewSemaphoreAcquire {}

/// Transfer one permit under the count lock; notify only after releasing it.
fn release_locked(state: &mut State) -> Option<Arc<Waiter>> {
    while let Some(weak) = state.waiters.pop_front() {
        let Some(waiter) = weak.upgrade() else {
            continue;
        };
        if waiter.status.load(Ordering::Acquire) == PENDING {
            waiter.status.store(ACQUIRED, Ordering::Release);
            return Some(waiter);
        }
    }
    state.count += 1;
    None
}

impl Drop for HewSemaphoreAcquire {
    fn drop(&mut self) {
        let wake = {
            let mut state = self.waiter.state.lock_or_recover();
            let previous = self.waiter.status.swap(CANCELLED, Ordering::AcqRel);
            state
                .waiters
                .retain(|weak| !weak.ptr_eq(&Arc::downgrade(&self.waiter)));
            if previous == ACQUIRED {
                release_locked(&mut state)
            } else {
                None
            }
        };
        if !self.timer.entry.is_null() {
            // SAFETY: this operation owns the handle; removal returns the
            // callback reference only when that callback cannot run.
            let payload = unsafe {
                hew_timer_wheel_remove(self.wheel, self.timer.entry, self.timer.generation)
            };
            if !payload.is_null() {
                // SAFETY: successful removal transfers the callback's Arc.
                drop(unsafe { Arc::from_raw(payload.cast::<Waiter>()) });
            }
        }
        if let Some(waiter) = wake {
            waiter.waker.wake();
        }
    }
}

unsafe extern "C" fn deadline(context: *mut c_void) {
    // SAFETY: scheduling transferred one Arc reference to this callback.
    let waiter = unsafe { Arc::from_raw(context.cast::<Waiter>()) };
    let wake = {
        let mut state = waiter.state.lock_or_recover();
        if waiter.status.load(Ordering::Acquire) == PENDING {
            waiter.status.store(TIMED_OUT, Ordering::Release);
            state
                .waiters
                .retain(|weak| !weak.ptr_eq(&Arc::downgrade(&waiter)));
            true
        } else {
            false
        }
    };
    if wake {
        waiter.waker.wake();
    }
}

/// Create a semaphore. Release the returned handle with `hew_semaphore_free`.
#[no_mangle]
pub extern "C" fn hew_semaphore_new(initial_count: i32) -> *mut HewSemaphore {
    Box::into_raw(Box::new(HewSemaphore {
        state: Arc::new(Mutex::new(State {
            count: initial_count,
            waiters: VecDeque::new(),
        })),
    }))
}

/// Create a separately owned handle to the same permit count and wait queue.
/// Release each handle independently with `hew_semaphore_free`.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_clone(sem: *const HewSemaphore) -> *mut HewSemaphore {
    if sem.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller holds the live handle while its shared backing is cloned.
    let state = unsafe { &*sem }.state.clone();
    Box::into_raw(Box::new(HewSemaphore { state }))
}

/// Begin acquiring a permit without blocking. A negative timeout means no
/// deadline; zero still permits an immediate acquisition.
///
/// # Safety
/// `sem` and `waker` must be live. The caller owns the returned operation and
/// must take its result or free it to abandon the claim.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire_new(
    sem: *const HewSemaphore,
    timeout_ms: i64,
    waker: *const HewWaker,
) -> *mut HewSemaphoreAcquire {
    if sem.is_null() || waker.is_null() {
        return std::ptr::null_mut();
    }
    let expires = crate::timer_wheel::now_for_wheel()
        .checked_add(std::time::Duration::from_millis(
            timeout_ms.max(0).cast_unsigned(),
        ))
        .unwrap_or(crate::timer_deadline::Deadline::from_millis(u64::MAX));
    // SAFETY: the caller holds both live handles throughout registration.
    let backing = unsafe { &*sem }.state.clone();
    let waiter = Arc::new(Waiter {
        state: backing.clone(),
        status: AtomicI32::new(PENDING),
        // SAFETY: caller supplies a live descriptor to retain.
        waker: unsafe { OwnedWaker::retain(&*waker) },
    });
    {
        let mut state = backing.lock_or_recover();
        if state.count > 0 {
            state.count -= 1;
            waiter.status.store(ACQUIRED, Ordering::Release);
        } else if timeout_ms == 0 {
            waiter.status.store(TIMED_OUT, Ordering::Release);
        } else {
            state.waiters.push_back(Arc::downgrade(&waiter));
        }
    }
    let mut operation = HewSemaphoreAcquire {
        waiter,
        wheel: std::ptr::null_mut(),
        timer: HewTimerHandle::null(),
    };
    if timeout_ms > 0 && operation.waiter.status.load(Ordering::Acquire) == PENDING {
        // SAFETY: the runtime owns this wheel through operation cleanup.
        unsafe {
            arm_deadline(
                &mut operation,
                crate::timer_periodic::global_wheel(),
                expires,
            );
        };
    }
    Box::into_raw(Box::new(operation))
}

/// Arm the optional deadline; a missing wheel is a fault, never a timeout.
///
/// # Safety
/// A non-null wheel must remain live through operation cleanup.
unsafe fn arm_deadline(
    operation: &mut HewSemaphoreAcquire,
    wheel: *mut HewTimerWheel,
    expires: crate::timer_deadline::Deadline,
) {
    operation.wheel = wheel;
    let payload = Arc::into_raw(operation.waiter.clone()).cast_mut().cast();
    // SAFETY: the runtime-owned wheel outlives the operation; registration
    // accepts the callback Arc even when it fires before returning.
    operation.timer = if operation.wheel.is_null() {
        HewTimerHandle::null()
    } else {
        // SAFETY: the caller supplies a live wheel and the retained callback
        // payload outlives firing or removal.
        unsafe { timer_wheel_schedule_precise_handle(operation.wheel, expires, deadline, payload) }
    };
    if operation.timer.entry.is_null() {
        // SAFETY: failed registration did not accept the callback payload.
        drop(unsafe { Arc::from_raw(payload.cast::<Waiter>()) });
        let _state = operation.waiter.state.lock_or_recover();
        let _ = operation.waiter.status.compare_exchange(
            PENDING,
            FAULT,
            Ordering::AcqRel,
            Ordering::Acquire,
        );
    }
}

/// Inspect an owned acquisition: pending=0, acquired=1, timed out=2, fault=4.
///
/// # Safety
/// `operation` must be live and owned by the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire_status(
    operation: *const HewSemaphoreAcquire,
) -> i32 {
    if operation.is_null() {
        return FAULT;
    }
    // SAFETY: caller retains the operation.
    unsafe { &*operation }.waiter.status.load(Ordering::Acquire)
}

/// Consume the result, returning one for an acquired permit and zero otherwise.
///
/// # Safety
/// `operation` must be live, complete, and exclusively owned by the caller.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire_take(operation: *const HewSemaphoreAcquire) -> i32 {
    if operation.is_null() {
        return 0;
    }
    // SAFETY: caller owns the live operation.
    let waiter = &unsafe { &*operation }.waiter;
    let _state = waiter.state.lock_or_recover();
    i32::from(waiter.status.swap(TAKEN, Ordering::AcqRel) == ACQUIRED)
}

/// Free an acquisition, returning an untaken permit or detaching a queued wait.
///
/// # Safety
/// `operation` must be null or transferred exactly once by its owner.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire_free(operation: *mut HewSemaphoreAcquire) {
    if !operation.is_null() {
        // SAFETY: caller transfers the unique operation handle.
        drop(unsafe { Box::from_raw(operation) });
    }
}

unsafe fn acquire_blocking(sem: *mut HewSemaphore, timeout_ms: i64) -> i32 {
    if sem.is_null() {
        return 0;
    }
    let (ready, waker) = crate::wake::blocking::Readiness::new();
    // SAFETY: caller holds the semaphore; this adapter owns its descriptor.
    let operation = unsafe { hew_semaphore_acquire_new(sem, timeout_ms, waker.descriptor()) };
    // SAFETY: this adapter exclusively owns the operation until freeing it.
    let result = unsafe { wait_blocking(operation, &ready) };
    drop(waker);
    drop(ready);
    if let Ok(result) = result {
        result
    } else {
        let fault = Box::into_raw(Box::new(crate::fault::HewFault::with_message(
            crate::internal::types::HEW_TRAP_USER_PANIC,
            "semaphore acquisition could not register its deadline".into(),
        )));
        // SAFETY: cleanup released the operation and all readiness owners;
        // this transfers one unique fault through the existing trap bridge.
        unsafe {
            crate::fault::hew_fault_trap(crate::internal::types::HEW_TRAP_USER_PANIC, fault);
        };
        0
    }
}

unsafe fn wait_blocking(
    operation: *mut HewSemaphoreAcquire,
    ready: &crate::wake::blocking::Readiness,
) -> Result<i32, ()> {
    let result = loop {
        // SAFETY: caller uniquely owns the operation throughout this wait.
        match unsafe { hew_semaphore_acquire_status(operation) } {
            PENDING => ready.wait(),
            ACQUIRED | TIMED_OUT => {
                // SAFETY: the operation has a completed result, consumed once.
                break Ok(unsafe { hew_semaphore_acquire_take(operation) });
            }
            _ => break Err(()),
        }
    };
    // SAFETY: caller transfers the sole operation handle, including on fault.
    unsafe { hew_semaphore_acquire_free(operation) };
    result
}

/// Acquire a permit using the shared operation at a synchronous host boundary.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire(sem: *mut HewSemaphore) {
    // SAFETY: caller guarantees a live semaphore.
    let _ = unsafe { acquire_blocking(sem, -1) };
}

/// Acquire a permit within the given timeout at a synchronous host boundary.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_acquire_timeout(
    sem: *mut HewSemaphore,
    timeout_ms: i64,
) -> i32 {
    // SAFETY: caller guarantees a live semaphore; negative public timeouts
    // remain an immediate attempt, as on the existing public surface.
    unsafe { acquire_blocking(sem, timeout_ms.max(0)) }
}

/// Try to claim one permit without waiting.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_try_acquire(sem: *mut HewSemaphore) -> i32 {
    if sem.is_null() {
        return 0;
    }
    // SAFETY: caller holds the live semaphore.
    let mut state = unsafe { &*sem }.state.lock_or_recover();
    if state.count > 0 {
        state.count -= 1;
        1
    } else {
        0
    }
}

/// Release one permit, completing a queued acquisition when present.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_release(sem: *mut HewSemaphore) {
    if sem.is_null() {
        return;
    }
    let wake = {
        // SAFETY: caller holds the live semaphore.
        let mut state = unsafe { &*sem }.state.lock_or_recover();
        release_locked(&mut state)
    };
    if let Some(waiter) = wake {
        waiter.waker.wake();
    }
}

/// Return a snapshot of unclaimed permits.
///
/// # Safety
/// `sem` must be a live semaphore handle.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_count(sem: *const HewSemaphore) -> i32 {
    if sem.is_null() {
        return 0;
    }
    // SAFETY: caller holds the live semaphore.
    unsafe { &*sem }.state.lock_or_recover().count
}

/// Release a semaphore handle. Outstanding acquisitions retain their backing.
///
/// # Safety
/// `sem` must be null or a unique handle transferred exactly once.
#[no_mangle]
pub unsafe extern "C" fn hew_semaphore_free(sem: *mut HewSemaphore) {
    if !sem.is_null() {
        // SAFETY: caller transfers the uniquely owned handle.
        drop(unsafe { Box::from_raw(sem) });
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn cloned_handles_share_permits_and_survive_independent_close() {
        let original = hew_semaphore_new(0);
        // SAFETY: each returned Box is a distinct owner; the operation keeps
        // its backing after the original handle closes.
        unsafe {
            let waiting = hew_semaphore_clone(original);
            let releasing = hew_semaphore_clone(original);
            assert_ne!(waiting, original);
            assert_ne!(releasing, original);
            assert_ne!(waiting, releasing);
            let (ready, waker) = crate::wake::blocking::Readiness::new();
            let operation = hew_semaphore_acquire_new(waiting, -1, waker.descriptor());
            assert_eq!(hew_semaphore_acquire_status(operation), PENDING);
            hew_semaphore_free(original);
            hew_semaphore_free(waiting);
            hew_semaphore_release(releasing);
            assert!(ready.take_ready());
            assert_eq!(hew_semaphore_acquire_take(operation), 1);
            hew_semaphore_acquire_free(operation);
            assert_eq!(hew_semaphore_count(releasing), 0);
            hew_semaphore_free(releasing);
        }
    }

    #[test]
    fn release_reserves_one_permit_and_abandonment_hands_it_on() {
        let sem = hew_semaphore_new(0);
        let (first_ready, first_waker) = crate::wake::blocking::Readiness::new();
        let (second_ready, second_waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: the test owns both operations and the semaphore.
        unsafe {
            let first = hew_semaphore_acquire_new(sem, -1, first_waker.descriptor());
            let second = hew_semaphore_acquire_new(sem, -1, second_waker.descriptor());
            hew_semaphore_release(sem);
            assert!(first_ready.take_ready());
            assert_eq!(hew_semaphore_count(sem), 0);
            assert_eq!(hew_semaphore_try_acquire(sem), 0);
            hew_semaphore_acquire_free(first);
            assert!(second_ready.take_ready());
            assert_eq!(hew_semaphore_acquire_take(second), 1);
            hew_semaphore_acquire_free(second);
            assert_eq!(hew_semaphore_count(sem), 0);
            hew_semaphore_free(sem);
        }
    }

    #[test]
    fn cancellation_racing_release_preserves_exactly_one_permit() {
        for _ in 0..64 {
            let sem = hew_semaphore_new(0);
            let (_, waker) = crate::wake::blocking::Readiness::new();
            // SAFETY: the test keeps the semaphore live until both threads join;
            // only this thread owns and frees the acquisition.
            unsafe {
                let operation = hew_semaphore_acquire_new(sem, -1, waker.descriptor());
                let address = sem as usize;
                let barrier = Arc::new(std::sync::Barrier::new(2));
                let producer_barrier = barrier.clone();
                let producer = thread::spawn(move || {
                    producer_barrier.wait();
                    hew_semaphore_release(address as *mut HewSemaphore);
                });
                barrier.wait();
                hew_semaphore_acquire_free(operation);
                producer.join().expect("release thread");
                assert_eq!(hew_semaphore_count(sem), 1);
                assert_eq!(hew_semaphore_try_acquire(sem), 1);
                assert_eq!(hew_semaphore_try_acquire(sem), 0);
                hew_semaphore_free(sem);
            }
        }
    }

    #[test]
    fn timeout_and_release_have_one_winner() {
        for timeout_first in [false, true] {
            let sem = hew_semaphore_new(0);
            let (ready, waker) = crate::wake::blocking::Readiness::new();
            // SAFETY: the test owns the operation and supplies the timer callback
            // its own retained waiter reference, as the wheel does.
            unsafe {
                let operation = hew_semaphore_acquire_new(sem, -1, waker.descriptor());
                let callback = Arc::into_raw((*operation).waiter.clone()).cast_mut().cast();
                if timeout_first {
                    deadline(callback);
                    hew_semaphore_release(sem);
                } else {
                    hew_semaphore_release(sem);
                    deadline(callback);
                }
                assert!(ready.take_ready());
                assert!(!ready.take_ready());
                assert_eq!(
                    hew_semaphore_acquire_take(operation),
                    i32::from(!timeout_first)
                );
                assert_eq!(hew_semaphore_count(sem), i32::from(timeout_first));
                hew_semaphore_acquire_free(operation);
                hew_semaphore_free(sem);
            }
        }
    }

    #[test]
    fn pending_operation_retains_backing_and_detaches_its_timer() {
        let sem = hew_semaphore_new(0);
        let (_, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: this test uniquely owns both handles; only the timer owns an
        // additional waiter reference, removed by freeing the operation.
        unsafe {
            let operation = hew_semaphore_acquire_new(sem, 60_000, waker.descriptor());
            let backing = Arc::downgrade(&(*sem).state);
            let waiter = Arc::downgrade(&(*operation).waiter);
            hew_semaphore_free(sem);
            assert!(backing.upgrade().is_some());
            hew_semaphore_acquire_free(operation);
            assert!(waiter.upgrade().is_none());
            assert!(backing.upgrade().is_none());
        }
    }

    #[test]
    fn unavailable_deadline_wheel_is_a_fault_and_releases_the_wait() {
        let sem = hew_semaphore_new(0);
        let (ready, waker) = crate::wake::blocking::Readiness::new();
        // SAFETY: the test owns all handles. A null wheel deliberately exercises
        // the production registration failure path without changing global state.
        unsafe {
            let operation = hew_semaphore_acquire_new(sem, -1, waker.descriptor());
            arm_deadline(
                &mut *operation,
                std::ptr::null_mut(),
                crate::timer_deadline::Deadline::from_millis(1),
            );
            assert_eq!(hew_semaphore_acquire_status(operation), FAULT);
            let waiter = Arc::downgrade(&(*operation).waiter);
            assert_eq!(wait_blocking(operation, &ready), Err(()));
            assert!(waiter.upgrade().is_none());
            hew_semaphore_release(sem);
            assert_eq!(hew_semaphore_count(sem), 1);
            assert_eq!(hew_semaphore_try_acquire(sem), 1);
            hew_semaphore_free(sem);
        }
    }

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
