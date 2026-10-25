//! Opaque host storage for values with compiler-owned cleanup descriptors.

use std::ffi::c_void;

use hew_cabi::host_error::HewError;
use hew_cabi::value::HewValueLayout;

use crate::release_walker::{HewReleaseCursor, ReleaseDriver};

/// Storage is initialized only after a successful generated export returns.
#[derive(Debug)]
pub struct HewHostValue {
    layout: HewValueLayout,
    storage: *mut c_void,
    _code: crate::code_owner::CodeRoot,
}

/// Allocate uninitialized result storage with an exact compiler descriptor.
/// # Safety
/// `layout` is a live, valid same-build descriptor. The generated wrapper must
/// initialize the value or forget the allocation before returning to C.
#[no_mangle]
pub unsafe extern "C" fn hew_host_value_new(layout: *const HewValueLayout) -> *mut HewHostValue {
    // SAFETY: the generated caller supplies the canonical concrete descriptor.
    let layout = unsafe { *layout };
    // SAFETY: the generated caller pins the descriptor until acquisition.
    let code = unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) };
    // SAFETY: the descriptor supplies valid allocation geometry, including ZSTs.
    let storage = unsafe { crate::mem::hew_alloc(layout.size.max(1) as u64, layout.align as u64) };
    Box::into_raw(Box::new(HewHostValue {
        layout,
        storage: storage.cast(),
        _code: code,
    }))
}

/// Borrow the value slot for one exact generated parameter or result.
/// # Safety
/// `owner` is live and has the expected concrete type and access mode.
#[no_mangle]
pub unsafe extern "C" fn hew_host_value_storage(owner: *const HewHostValue) -> *mut c_void {
    // SAFETY: the wrapper retains the owner throughout its checked invocation.
    unsafe { (*owner).storage }
}

/// Free storage after its bits were consumed, or before initialization succeeded.
/// # Safety
/// No semantic owner remains in the slot and no invocation borrows its storage.
#[no_mangle]
pub unsafe extern "C" fn hew_host_value_forget(owner: *mut HewHostValue) {
    // SAFETY: the wrapper transfers the allocation after the typed call drains.
    let owner = unsafe { Box::from_raw(owner) };
    // SAFETY: this is the original allocation and canonical geometry.
    unsafe {
        crate::mem::hew_dealloc(
            owner.storage.cast(),
            owner.layout.size.max(1) as u64,
            owner.layout.align as u64,
        );
    }
}

/// Consume an initialized opaque value through its canonical release cursor.
/// # Safety
/// `owner` is null or one unique live initialized owner. `error` is an empty
/// writable slot. This hosting boundary must not run on a scheduler worker.
#[no_mangle]
pub unsafe extern "C" fn hew_host_value_release(
    owner: *mut HewHostValue,
    error: *mut *mut HewError,
) -> i32 {
    // SAFETY: the caller provides a writable empty error slot.
    unsafe { *error = std::ptr::null_mut() };
    if owner.is_null() {
        return 0;
    }
    // SAFETY: the caller transfers the typed initialized owner.
    let value = unsafe { &*owner };
    let cursor = HewReleaseCursor::values([(value.storage, value.layout)]);
    let mut release = ReleaseDriver::new(cursor);
    // SAFETY: the existing release driver drains every child before returning.
    unsafe {
        crate::coro_root::with_root_invocation(|state, readiness| {
            while !release.poll(state) {
                readiness.wait();
            }
            0
        });
    }
    let fault = release.take_fault();
    // SAFETY: the cursor consumed the value and destroyed every child frame.
    unsafe { hew_host_value_forget(owner) };
    if fault.is_null() {
        0
    } else {
        // SAFETY: the release driver transfers its combined owned diagnostic.
        let failure = unsafe { crate::fault::hew_fault_into_host_error(fault) };
        // SAFETY: the caller receives one independent owned error.
        unsafe { *error = failure };
        // SAFETY: failure is the live non-null error just created.
        unsafe { crate::host_api::hew_host_error_code(failure) }
    }
}
