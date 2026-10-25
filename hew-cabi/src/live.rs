//! Same-build storage boundary for native interactive submissions.

use core::ffi::c_void;

/// A host-owned array of individually typed persistent slots. Generated code
/// borrows this context for one exclusive submission; it never owns the array.
#[repr(C)]
#[derive(Debug)]
pub struct HewLiveSession {
    pub slots: *mut *mut c_void,
    pub len: usize,
}
