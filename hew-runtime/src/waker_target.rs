//! Retained callback ownership shared by native operations and verification.
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

/// A retained readiness target. Registrations observe its allocation weakly;
/// each implementation keeps notification independent of mutable operation state.
pub(crate) trait WakeTarget: Send + Sync {
    fn owned_waker(&self) -> &OwnedWaker;

    fn wake(&self) {
        self.owned_waker().wake();
    }
}

impl WakeTarget for OwnedWaker {
    fn owned_waker(&self) -> &OwnedWaker {
        self
    }
}
