//! Crate-private `Send`-asserting raw-pointer wrapper.
//!
//! Several runtime subsystems need to keep a `*mut T` inside a container that
//! must itself be `Send` (e.g. a `HashMap` shared between profiler threads, a
//! crossbeam queue exercised from multiple producer threads, or a registry
//! reachable from a worker thread).  Each subsystem previously open-coded the
//! same wrapper:
//!
//! ```ignore
//! struct SendPtr(*mut T);
//! unsafe impl Send for SendPtr {}
//! ```
//!
//! This helper centralizes the marker boilerplate while keeping the safety
//! discipline local.  The constructor is `unsafe` and the inner pointer is
//! private precisely so callers cannot accidentally fabricate a `SendPtr`
//! without thinking about cross-thread soundness.  Every `SendPtr::new` call
//! site must carry a SAFETY comment that explains, for that specific site,
//! why moving the pointer between threads is sound — the helper deliberately
//! does NOT absorb or replace that proof.

#![allow(
    dead_code,
    reason = "API surface used selectively across cfg-gated callers"
)]

use std::fmt;

/// `Send`-asserting wrapper around `*mut T`.
///
/// The constructor is `unsafe` and the wrapped pointer is private so each
/// construction site retains its own SAFETY justification.  The `Send` impl
/// is a marker assertion only; soundness is the caller's responsibility.
pub(crate) struct SendPtr<T: ?Sized>(*mut T);

impl<T: ?Sized> SendPtr<T> {
    /// Wrap a raw pointer so it can be moved between threads.
    ///
    /// # Safety
    ///
    /// The caller must justify, at the construction site, that `ptr` is
    /// safe to send across threads given the surrounding ownership or
    /// synchronization discipline.  This helper provides only the marker
    /// `Send` impl; it does not provide a safety proof.
    #[inline]
    pub(crate) unsafe fn new(ptr: *mut T) -> Self {
        Self(ptr)
    }

    /// Return the wrapped raw pointer.
    #[inline]
    pub(crate) fn as_ptr(&self) -> *mut T {
        self.0
    }
}

// SAFETY: Marker assertion only.  Each `SendPtr::new` call site supplies the
// local proof that sending the pointer across threads is sound for the
// surrounding ownership/synchronization discipline; see the constructor doc
// comment.
unsafe impl<T: ?Sized> Send for SendPtr<T> {}

impl<T: ?Sized> fmt::Debug for SendPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("SendPtr").field(&self.0).finish()
    }
}
