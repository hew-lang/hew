//! Hew runtime: remaining Rust shim for `std::sort`.
//!
//! Integer sorting, string sorting, and reversal now live in Hew. The Rust
//! surface stays in place for float sorting until Hew can express
//! `f64::total_cmp` parity.

// The `data` field is `*mut u8` (matching C `void*`) but always allocated via
// `realloc` which guarantees max alignment. Casts to typed pointers are safe.
#![expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data buffer allocated via libc::realloc which guarantees max alignment"
)]

#[cfg(test)]
extern crate hew_runtime;

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

#[cfg(test)]
mod tests {
    use super::*;

    unsafe fn make_f64_vec(vals: &[f64]) -> *mut HewVec {
        // SAFETY: test-only helper; all hew_vec_* calls match expected types.
        unsafe {
            let v = hew_runtime::vec::hew_vec_new_f64();
            for &val in vals {
                hew_runtime::vec::hew_vec_push_f64(v, val);
            }
            v
        }
    }

    unsafe fn read_f64_vec(v: *mut HewVec) -> Vec<f64> {
        // SAFETY: `v` is a valid, non-null HewVec pointer; data is properly
        // aligned and length matches the array size.
        unsafe {
            let len = (*v).len;
            if len == 0 {
                return Vec::new();
            }
            core::slice::from_raw_parts((*v).data.cast::<f64>(), len).to_vec()
        }
    }

    #[test]
    fn sort_floats_ascending() {
        // SAFETY: FFI calls with valid, non-null pointers returned by test helpers.
        unsafe {
            let input = [std::f64::consts::PI, 1.0, std::f64::consts::E, 0.5];
            let v = make_f64_vec(&input);
            let sorted = hew_sort_floats(v);
            assert_eq!(
                read_f64_vec(sorted),
                vec![0.5, 1.0, std::f64::consts::E, std::f64::consts::PI]
            );
            assert_eq!(read_f64_vec(v), input);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_floats_negative() {
        // SAFETY: FFI calls with valid, non-null pointers returned by test helpers.
        unsafe {
            let v = make_f64_vec(&[-3.0, -1.0, -2.0]);
            let sorted = hew_sort_floats(v);
            assert_eq!(read_f64_vec(sorted), vec![-3.0, -2.0, -1.0]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_floats_with_zero() {
        // SAFETY: FFI calls with valid, non-null pointers returned by test helpers.
        unsafe {
            let v = make_f64_vec(&[1.0, 0.0, -1.0]);
            let sorted = hew_sort_floats(v);
            assert_eq!(read_f64_vec(sorted), vec![-1.0, 0.0, 1.0]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_floats_total_cmp_edge_cases() {
        // SAFETY: FFI calls with valid, non-null pointers returned by test helpers.
        unsafe {
            let nan = f64::from_bits(0x7ff8_0000_0000_0001);
            let v = make_f64_vec(&[nan, 1.0, 0.0, -0.0]);
            let sorted = hew_sort_floats(v);
            let bits: Vec<u64> = read_f64_vec(sorted).into_iter().map(f64::to_bits).collect();
            assert_eq!(
                bits,
                vec![
                    (-0.0_f64).to_bits(),
                    0.0_f64.to_bits(),
                    1.0_f64.to_bits(),
                    nan.to_bits()
                ]
            );
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn null_returns_null() {
        // SAFETY: passing a null pointer to the FFI float sort function that explicitly handles null.
        unsafe {
            assert!(hew_sort_floats(std::ptr::null_mut()).is_null());
        }
    }
}
