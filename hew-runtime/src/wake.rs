//! Retained readiness notifications shared by native coroutine operations.
//!
//! A waker identifies a readiness target, never storage inside a coroutine
//! frame. Operations retain it while a completion or cancellation can notify
//! that target. Result ownership remains with the operation until it is taken.

use std::ffi::c_void;

/// Compiler-private readiness callback descriptor.
///
/// All callbacks must be thread-safe and must not unwind. `retain` creates one
/// reference to `context`; `release` consumes one. `wake` only reports readiness
/// and must not synchronously resume or destroy a coroutine. The descriptor's
/// context and callback code must outlive every retained reference.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewWaker {
    pub context: *mut c_void,
    pub wake: unsafe extern "C" fn(*mut c_void),
    pub retain: unsafe extern "C" fn(*mut c_void),
    pub release: unsafe extern "C" fn(*mut c_void),
}

/// One retained reference to a readiness target.
#[derive(Debug)]
pub struct OwnedWaker(HewWaker);

// SAFETY: construction requires the descriptor's thread-safe lifetime contract.
unsafe impl Send for OwnedWaker {}
// SAFETY: wake and retain may run concurrently under the descriptor contract.
unsafe impl Sync for OwnedWaker {}

impl OwnedWaker {
    /// Retain a borrowed descriptor before its caller can release it.
    ///
    /// # Safety
    /// `waker` must obey [`HewWaker`]'s callback and lifetime contract, with one
    /// existing reference keeping its context live throughout this call.
    #[must_use]
    pub unsafe fn retain(waker: &HewWaker) -> Self {
        // SAFETY: the caller guarantees a live reference and valid callbacks.
        unsafe { (waker.retain)(waker.context) };
        Self(*waker)
    }

    /// Borrow the descriptor without transferring its retained reference.
    #[must_use]
    pub fn descriptor(&self) -> &HewWaker {
        &self.0
    }

    /// Notify readiness. Invoke outside operation locks: notification may
    /// enqueue an executor that immediately polls the same operation.
    pub fn wake(&self) {
        // SAFETY: this object holds a live reference to the callback context.
        unsafe { (self.0.wake)(self.0.context) };
    }
}

impl Clone for OwnedWaker {
    fn clone(&self) -> Self {
        // SAFETY: self keeps the context live throughout retain.
        unsafe { Self::retain(&self.0) }
    }
}

impl Drop for OwnedWaker {
    fn drop(&mut self) {
        // SAFETY: each OwnedWaker owns exactly one reference to this context.
        unsafe { (self.0.release)(self.0.context) };
    }
}

/// Registrations are weak so cancelling a sender immediately releases its
/// readiness target. A notification retains each live target outside the lock.
#[derive(Debug, Default)]
pub(crate) struct ReadinessRegistrations(std::sync::Mutex<Vec<std::sync::Weak<OwnedWaker>>>);

impl ReadinessRegistrations {
    pub(crate) fn register(&self, waker: &std::sync::Arc<OwnedWaker>) {
        let mut waiters = crate::util::MutexExt::lock_or_recover(&self.0);
        waiters.retain(|waiter| waiter.strong_count() != 0);
        waiters.push(std::sync::Arc::downgrade(waker));
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

    #[derive(Debug, Default)]
    pub struct Readiness {
        pending: Mutex<bool>,
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
            let mut pending = self.pending.lock_or_recover();
            while !*pending {
                pending = self.changed.wait_or_recover(pending);
            }
            *pending = false;
        }

        /// Consume an already pending notification without blocking.
        pub fn take_ready(&self) -> bool {
            std::mem::take(&mut *self.pending.lock_or_recover())
        }

        /// The driver picked this latch's ready-list entry.
        pub(crate) fn picked(&self) {
            self.scheduled.store(false, Ordering::Release);
        }
    }

    unsafe extern "C" fn wake(context: *mut c_void) {
        // SAFETY: descriptor holders retain this Arc allocation.
        let readiness = unsafe { &*context.cast::<Readiness>() };
        let was_pending = std::mem::replace(&mut *readiness.pending.lock_or_recover(), true);
        if !crate::driver::active() {
            readiness.changed.notify_one();
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
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

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
