//! Retained readiness notifications shared by native coroutine operations.
//!
//! A waker identifies a readiness target, never storage inside a coroutine
//! frame. Operations retain it while a completion or cancellation can notify
//! that target. Result ownership remains with the operation until it is taken.

#[path = "wake_scheduled.rs"]
pub(crate) mod scheduled;

#[path = "waker_target.rs"]
mod target;
pub(crate) use target::WakeTarget;
pub use target::{HewWaker, OwnedWaker};

/// Registrations are weak so cancelling a sender immediately releases its
/// readiness target. A notification retains each live target outside the lock.
#[derive(Debug, Default)]
pub(crate) struct ReadinessRegistrations(std::sync::Mutex<Vec<std::sync::Weak<dyn WakeTarget>>>);

impl ReadinessRegistrations {
    pub(crate) fn register<T: WakeTarget + 'static>(&self, waker: &std::sync::Arc<T>) {
        let mut waiters = crate::util::MutexExt::lock_or_recover(&self.0);
        waiters.retain(|waiter| waiter.strong_count() != 0);
        let weak = std::sync::Arc::downgrade(waker);
        let weak: std::sync::Weak<dyn WakeTarget> = weak;
        if !waiters.iter().any(|existing| existing.ptr_eq(&weak)) {
            waiters.push(weak);
        }
    }

    pub(crate) fn notify(&self) {
        let ready: Vec<_> = {
            let mut waiters = crate::util::MutexExt::lock_or_recover(&self.0);
            let ready = waiters
                .iter()
                .filter_map(std::sync::Weak::upgrade)
                .collect();
            waiters.retain(|waiter| waiter.strong_count() != 0);
            ready
        };
        for waker in ready {
            waker.wake();
        }
    }
}

/// Wait for a peer to change a condition the caller re-checks under its own
/// lock, and hand back the re-taken guard.
///
/// This is not a [`blocking::Readiness`] wait: the condition lives in the
/// caller's `Mutex`, not in a latch, so the caller loops and re-reads it.
/// Natively the peer's `notify_all` ends the wait early and the timeout bounds
/// a caller that also polls something nobody notifies. On the single-thread
/// driver this thread is the only one, so releasing the lock and stepping the
/// driver is what lets the peer run at all, and the driver fails closed on its
/// own when nothing is left that could.
pub(crate) fn wait_for_peer<'a, T>(
    lock: &'a std::sync::Mutex<T>,
    condition: &std::sync::Condvar,
    guard: std::sync::MutexGuard<'a, T>,
    timeout: std::time::Duration,
) -> std::sync::MutexGuard<'a, T> {
    use crate::util::MutexExt;
    if crate::driver::active() {
        let _ = (condition, timeout);
        drop(guard);
        crate::driver::step();
        return lock.lock_or_recover();
    }
    let _ = lock;
    condition
        .wait_timeout(guard, timeout)
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .0
}

/// A readiness latch for synchronous hosting boundaries. Actor and task
/// continuations use scheduler readiness targets instead of waiting here.
pub mod blocking {
    use super::{HewWaker, OwnedWaker};
    use crate::util::{CondvarExt, MutexExt};
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::{Arc, Condvar, Mutex};

    #[cfg(not(target_arch = "wasm32"))]
    use crate::scheduler::spin::{SpinBudget, DEFAULT_SPIN_LIMIT};

    #[cfg(not(target_arch = "wasm32"))]
    thread_local! {
        /// Adaptive spin budget of the synchronous waiter on this thread.
        static WAITER_SPIN: std::cell::Cell<SpinBudget> =
            const { std::cell::Cell::new(SpinBudget::new(DEFAULT_SPIN_LIMIT)) };
    }

    /// `pending` and `blocked` form a Dekker pair under `SeqCst`: the waiter
    /// publishes `blocked` before its last `pending` check under `lock`, and a
    /// waker publishes `pending` before reading `blocked`, so a waker that sees
    /// no blocked waiter skips the lock and the futex wake, and the waiter then
    /// observes `pending` itself.
    #[derive(Debug, Default)]
    pub struct Readiness {
        pending: AtomicBool,
        /// The waiter is blocking (or about to block) on `changed`.
        blocked: AtomicBool,
        lock: Mutex<()>,
        changed: Condvar,
        /// On the single-thread driver: the set latch is on the ready list and
        /// its waiter resumes only once the driver picks it.
        scheduled: AtomicBool,
    }

    impl Readiness {
        /// Construct a driver-owned latch and a retained operation descriptor.
        /// Wakes before the driver waits remain pending, including wakes during
        /// registration and while a coroutine is running.
        #[must_use]
        pub fn new() -> (Arc<Self>, OwnedWaker) {
            let readiness = Arc::new(Self::default());
            let descriptor = HewWaker {
                context: Arc::as_ptr(&readiness).cast_mut().cast(),
                wake,
                retain,
                release,
            };
            // SAFETY: readiness keeps the Arc allocation live during retain.
            let waker = unsafe { OwnedWaker::retain(&descriptor) };
            (readiness, waker)
        }

        /// Consume one readiness notification, blocking only if none arrived.
        ///
        /// On the single-thread driver the waiting thread is the one that must
        /// make progress: it steps the driver until the driver picks this latch,
        /// so the waiter takes its turn in the schedule like any participant.
        pub fn wait(&self) {
            if crate::driver::active() {
                while self.scheduled.load(Ordering::Acquire) || !self.take_ready() {
                    crate::driver::step();
                }
                return;
            }
            if self.take_ready() {
                return;
            }
            // A reply usually lands within a futex round trip; spin for it
            // first so neither side makes a syscall.
            #[cfg(not(target_arch = "wasm32"))]
            {
                let spun = WAITER_SPIN.with(|budget| {
                    let mut current = budget.get();
                    let spun = crate::scheduler::spin_waiter(&mut current, || {
                        self.pending.load(Ordering::Acquire)
                    });
                    budget.set(current);
                    spun
                });
                if spun && self.take_ready() {
                    return;
                }
            }
            let mut guard = self.lock.lock_or_recover();
            self.blocked.store(true, Ordering::SeqCst);
            while !self.pending.swap(false, Ordering::SeqCst) {
                guard = self.changed.wait_or_recover(guard);
            }
            self.blocked.store(false, Ordering::Relaxed);
        }

        /// Consume one readiness notification like [`Self::wait`], giving up
        /// at `deadline`. Returns whether a notification was consumed.
        #[cfg(test)]
        pub fn wait_until(&self, deadline: std::time::Instant) -> bool {
            if crate::driver::active() {
                while self.scheduled.load(Ordering::Acquire) || !self.take_ready() {
                    if std::time::Instant::now() >= deadline {
                        return false;
                    }
                    crate::driver::step();
                }
                return true;
            }
            let mut guard = self.lock.lock_or_recover();
            self.blocked.store(true, Ordering::SeqCst);
            while !self.pending.swap(false, Ordering::SeqCst) {
                let remaining = deadline.saturating_duration_since(std::time::Instant::now());
                if remaining.is_zero() {
                    self.blocked.store(false, Ordering::Relaxed);
                    return false;
                }
                guard = self.changed.wait_timeout_or_recover(guard, remaining).0;
            }
            self.blocked.store(false, Ordering::Relaxed);
            true
        }

        /// Consume an already pending notification without blocking.
        pub fn take_ready(&self) -> bool {
            self.pending.swap(false, Ordering::AcqRel)
        }

        /// The driver picked this latch's ready-list entry.
        pub(crate) fn picked(&self) {
            self.scheduled.store(false, Ordering::Release);
        }
    }

    unsafe extern "C" fn wake(context: *mut c_void) {
        // SAFETY: descriptor holders retain this Arc allocation.
        let readiness = unsafe { &*context.cast::<Readiness>() };
        let was_pending = readiness.pending.swap(true, Ordering::SeqCst);
        if !crate::driver::active() {
            if readiness.blocked.load(Ordering::SeqCst) {
                // Passing through the lock orders this notify after the
                // waiter's under-lock `pending` check or after it entered the
                // wait. Notify after unlocking so the woken waiter does not
                // contend on the lock this thread still holds.
                drop(readiness.lock.lock_or_recover());
                readiness.changed.notify_one();
            }
            return;
        }
        if was_pending || readiness.scheduled.swap(true, Ordering::AcqRel) {
            return;
        }
        // SAFETY: the descriptor holder's reference keeps the allocation live
        // while the ready list takes its own.
        let entry = unsafe {
            Arc::increment_strong_count(context.cast::<Readiness>());
            Arc::from_raw(context.cast::<Readiness>())
        };
        crate::driver::publish_root(entry);
    }

    unsafe extern "C" fn retain(context: *mut c_void) {
        // SAFETY: the descriptor contract requires a live Arc reference.
        unsafe { Arc::increment_strong_count(context.cast::<Readiness>()) };
    }

    unsafe extern "C" fn release(context: *mut c_void) {
        // SAFETY: consumes exactly one reference acquired by retain.
        unsafe { Arc::decrement_strong_count(context.cast::<Readiness>()) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[test]
    fn timed_readiness_wait_consumes_wakes_and_recovers_after_timeout() {
        let _runtime = crate::runtime_test_guard();
        let (readiness, waker) = blocking::Readiness::new();
        assert!(!readiness.wait_until(std::time::Instant::now()));
        waker.wake();
        assert!(readiness.wait_until(std::time::Instant::now()));
        assert!(
            !readiness.take_ready(),
            "the wait consumes its notification"
        );
        assert!(!readiness.wait_until(std::time::Instant::now()));
        std::thread::spawn(move || waker.wake())
            .join()
            .expect("readiness worker");
        assert!(readiness.wait_until(std::time::Instant::now()));
        assert!(!readiness.take_ready());
    }

    unsafe extern "C" fn retain(context: *mut c_void) {
        // SAFETY: context was produced by Arc::into_raw below.
        unsafe { Arc::increment_strong_count(context.cast::<AtomicUsize>()) };
    }

    unsafe extern "C" fn release(context: *mut c_void) {
        // SAFETY: each call consumes one previously retained reference.
        unsafe { Arc::decrement_strong_count(context.cast::<AtomicUsize>()) };
    }

    unsafe extern "C" fn wake(context: *mut c_void) {
        // SAFETY: a retained reference keeps this atomic live for the callback.
        unsafe { &*context.cast::<AtomicUsize>() }.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn operation_reference_survives_creator_release_and_worker_notification() {
        let count = Arc::new(AtomicUsize::new(0));
        let context = Arc::into_raw(count.clone()).cast_mut().cast::<c_void>();
        let descriptor = HewWaker {
            context,
            wake,
            retain,
            release,
        };
        // SAFETY: descriptor uses the Arc callbacks and one live creator ref.
        let operation = unsafe { OwnedWaker::retain(&descriptor) };
        let cancellation = operation.clone();
        // SAFETY: release the original creator's reference, not the operation's.
        unsafe { release(context) };
        std::thread::spawn(move || operation.wake())
            .join()
            .expect("readiness worker");
        assert_eq!(count.load(Ordering::SeqCst), 1);
        assert_eq!(Arc::strong_count(&count), 2);
        drop(cancellation);
        assert_eq!(Arc::strong_count(&count), 1);
    }
}
