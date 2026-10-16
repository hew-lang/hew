//! Compiler-private invocation storage, embedded in an LLVM coroutine frame.
//!
//! Completion belongs to each invocation. Execution context is opaque and may
//! be borrowed from the suspended caller; the runtime owns its token, readiness
//! target and lexical boundary stack. Destroy the child LLVM frame before
//! releasing this record, then its storage may be initialized for another call.

use core::ffi::c_void;
use core::sync::atomic::{AtomicI32, AtomicU8};

pub const OWNS_CONTEXT: u8 = 1;
pub const HEAP_ALLOCATED: u8 = 2;

/// The native compiler and runtime's exact invocation-record ABI.
#[repr(C)]
#[derive(Debug)]
pub struct HewCoroInvocation {
    pub context: *mut c_void,
    pub status: AtomicI32,
    pub private_status: AtomicI32,
    pub poll_suppression: u32,
    pub lexical_depth: u32,
    pub flags: u8,
    pub cleanup_fault: AtomicU8,
    pub cleanup_abandon_io: AtomicU8,
}

const _: () = {
    use core::mem::offset_of;
    assert!(size_of::<AtomicI32>() == size_of::<i32>());
    assert!(align_of::<AtomicI32>() == align_of::<i32>());
    assert!(size_of::<AtomicU8>() == 1);
    assert!(align_of::<AtomicU8>() == 1);
    assert!(offset_of!(HewCoroInvocation, context) == 0);
    assert!(offset_of!(HewCoroInvocation, status) == size_of::<*mut c_void>());
    assert!(offset_of!(HewCoroInvocation, private_status) == size_of::<*mut c_void>() + 4);
    assert!(offset_of!(HewCoroInvocation, poll_suppression) == size_of::<*mut c_void>() + 8);
    assert!(offset_of!(HewCoroInvocation, lexical_depth) == size_of::<*mut c_void>() + 12);
    assert!(offset_of!(HewCoroInvocation, flags) == size_of::<*mut c_void>() + 16);
    assert!(offset_of!(HewCoroInvocation, cleanup_fault) == size_of::<*mut c_void>() + 17);
    assert!(offset_of!(HewCoroInvocation, cleanup_abandon_io) == size_of::<*mut c_void>() + 18);
};
