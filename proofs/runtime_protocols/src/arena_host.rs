//! OS mapping contract: an aligned live region covering the requested length,
//! or MAP_FAILED. This bounds backing storage, never request arithmetic.
use std::alloc::{alloc, dealloc, Layout};
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, Ordering};

pub const PROT_READ: i32 = 1;
pub const PROT_WRITE: i32 = 2;
pub const MAP_PRIVATE: i32 = 2;
pub const MAP_ANONYMOUS: i32 = 32;
pub const MAP_FAILED: *mut c_void = usize::MAX as *mut c_void;
pub static FAIL_MAPPING: AtomicBool = AtomicBool::new(false);

pub unsafe fn mmap(_: *mut c_void, size: usize, _: i32, _: i32, _: i32, _: i64) -> *mut c_void {
    if size == 0 || size > 128 || FAIL_MAPPING.swap(false, Ordering::Relaxed) {
        return MAP_FAILED;
    }
    let layout = Layout::from_size_align(128, 8).unwrap();
    let base = alloc(layout);
    // Restrict successful host mappings to the OS page-alignment contract.
    kani::assume(base.is_null() || (base as usize) % 4096 == 0);
    if base.is_null() {
        MAP_FAILED
    } else {
        base.cast()
    }
}

pub unsafe fn munmap(base: *mut c_void, size: usize) -> i32 {
    assert!(size > 0 && size <= 128);
    dealloc(base.cast(), Layout::from_size_align(128, 8).unwrap());
    0
}
