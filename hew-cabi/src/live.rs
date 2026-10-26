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

/// One canonical leaf of a persistent place, supplied by physical MIR.
/// The registration call copies this borrowed metadata before execution begins.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewLiveLeaf {
    pub storage: *mut c_void,
    pub layout: *const crate::value::HewValueLayout,
    pub path: *const u32,
    pub path_len: usize,
}
