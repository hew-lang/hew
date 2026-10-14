//! Native float sorting for `std::sort`.
//!
//! Integer sorting, string sorting, and reversal now live in Hew. The Rust
//! surface stays in place for float sorting while generic Hew sorting has a
//! measured performance cost. Both paths preserve IEEE 754 total order.

// The `data` field is `*mut u8` (matching C `void*`) but always grown via
// `buf_realloc` which guarantees 16-byte alignment. Casts to typed pointers
// are safe.
#![expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data buffer grown via buf_realloc which guarantees 16-byte alignment"
)]
use hew_cabi::vec::HewVec;

/// Clone a `HewVec` via the runtime helper so float elements are copied into a
/// fresh output vector.
///
/// # Safety
///
/// `v` must point to a live, valid `HewVec`.
unsafe fn clone_vec(v: *const HewVec) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe { hew_runtime::vec::hew_vec_clone(v) }
}

/// Sort a vector of `f64` values in ascending order (NaN-aware).
///
/// Returns a **new** vector; the original is not modified.
///
/// # Safety
///
/// `v` must be a valid pointer to a `HewVec` containing `f64` elements.
#[no_mangle]
pub unsafe extern "C" fn hew_sort_floats(v: *mut HewVec) -> *mut HewVec {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `v` is a valid `HewVec` of `f64` elements.
    // `clone_vec` returns a freshly-allocated copy with max-aligned data.
    unsafe {
        let out = clone_vec(v);
        let len = (*out).len;
        if len > 1 {
            let slice = core::slice::from_raw_parts_mut((*out).data.cast::<f64>(), len);
            slice.sort_unstable_by(f64::total_cmp);
        }
        out
    }
}
