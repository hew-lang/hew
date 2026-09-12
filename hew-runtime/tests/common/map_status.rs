//! Assertions shared by successful descriptor protocol fixtures.
use core::ffi::c_void;

/// Assert the status/fault contract and return the initialized scalar result.
///
/// # Safety
/// On status zero, `call` must initialize its result pointer with a valid T.
pub unsafe fn success<T>(call: impl FnOnce(*mut T, *mut *mut c_void) -> i32) -> T {
    let mut value = core::mem::MaybeUninit::uninit();
    let mut fault = core::ptr::null_mut();
    assert_eq!(call(value.as_mut_ptr(), &raw mut fault), 0);
    assert!(fault.is_null());
    // SAFETY: the callback contract initializes the result on success.
    unsafe { value.assume_init() }
}
