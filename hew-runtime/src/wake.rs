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

/// A readiness latch for a top-level or task-thread coroutine driver. Actor
/// workers use their scheduler's readiness target instead of waiting here.
#[cfg(not(target_arch = "wasm32"))]
pub mod blocking {
    use super::{HewWaker, OwnedWaker};
    use crate::util::{CondvarExt, MutexExt};
    use std::ffi::c_void;
    use std::sync::{Arc, Condvar, Mutex};

    #[derive(Debug, Default)]
    pub struct Readiness {
        pending: Mutex<bool>,
        changed: Condvar,
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
        pub fn wait(&self) {
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
    }

    unsafe extern "C" fn wake(context: *mut c_void) {
        // SAFETY: descriptor holders retain this Arc allocation.
        let readiness = unsafe { &*context.cast::<Readiness>() };
        *readiness.pending.lock_or_recover() = true;
        readiness.changed.notify_one();
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
