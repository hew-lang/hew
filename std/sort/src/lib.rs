//! Hew runtime: `std::sort` module.
//!
//! Provides type-specific sorting and reversal utilities for vectors.
//! Each function clones the input vector and returns a new sorted (or
//! reversed) copy, leaving the original unchanged.

// The `data` field is `*mut u8` (matching C `void*`) but always allocated via
// `realloc` which guarantees max alignment.  Casts to typed pointers are safe.
#![expect(
    clippy::cast_ptr_alignment,
    reason = "HewVec data buffer allocated via libc::realloc which guarantees max alignment"
)]

#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::vec::HewVec;
use std::ffi::CStr;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Clone a `HewVec` via the runtime helper so string elements are deep-copied.
///
/// # Safety
///
/// `v` must point to a live, valid `HewVec`.
unsafe fn clone_vec(v: *const HewVec) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe { hew_runtime::vec::hew_vec_clone(v) }
}

// ---------------------------------------------------------------------------
// Public FFI
// ---------------------------------------------------------------------------

/// Sort a vector of `i64` values in ascending order.
///
/// Returns a **new** vector; the original is not modified.
///
/// # Safety
///
/// `v` must be a valid pointer to a `HewVec` containing `i64` elements.
#[no_mangle]
pub unsafe extern "C" fn hew_sort_ints(v: *mut HewVec) -> *mut HewVec {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `v` is a valid `HewVec` of `i64` elements.
    // `clone_vec` returns a freshly-allocated copy whose data pointer is
    // aligned via `realloc`. The slice length matches `(*out).len`.
    unsafe {
        let out = clone_vec(v);
        let len = (*out).len;
        if len > 1 {
            let slice = core::slice::from_raw_parts_mut((*out).data.cast::<i64>(), len);
            slice.sort_unstable();
        }
        out
    }
}

/// Sort a vector of C-string pointers (`*const c_char`) alphabetically.
///
/// Returns a **new** vector; the original is not modified.
///
/// # Safety
///
/// `v` must be a valid pointer to a `HewVec` whose elements are
/// NUL-terminated `*const c_char` string pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_sort_strings(v: *mut HewVec) -> *mut HewVec {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `v` is a valid `HewVec` of `*const c_char`
    // string pointers. `clone_vec` deep-copies each string element.
    unsafe {
        let out = clone_vec(v);
        let len = (*out).len;
        if len > 1 {
            let slice =
                core::slice::from_raw_parts_mut((*out).data.cast::<*const libc::c_char>(), len);
            slice.sort_unstable_by(|a, b| {
                let sa = CStr::from_ptr(*a);
                let sb = CStr::from_ptr(*b);
                sa.cmp(sb)
            });
        }
        out
    }
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

/// Reverse a vector of `i64` values.
///
/// Returns a **new** vector; the original is not modified.
///
/// # Safety
///
/// `v` must be a valid pointer to a `HewVec` containing `i64` elements.
#[no_mangle]
pub unsafe extern "C" fn hew_sort_reverse(v: *mut HewVec) -> *mut HewVec {
    if v.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: caller guarantees `v` is a valid `HewVec` of `i64` elements.
    // `clone_vec` returns a freshly-allocated copy with max-aligned data.
    unsafe {
        let out = clone_vec(v);
        let len = (*out).len;
        if len > 1 {
            let slice = core::slice::from_raw_parts_mut((*out).data.cast::<i64>(), len);
            slice.reverse();
        }
        out
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Push `i64` values into a fresh vec and return the raw pointer.
    unsafe fn make_i64_vec(vals: &[i64]) -> *mut HewVec {
        // SAFETY: test-only helper; all hew_vec_* calls match expected types.
        unsafe {
            let v = hew_runtime::vec::hew_vec_new_i64();
            for &val in vals {
                hew_runtime::vec::hew_vec_push_i64(v, val);
            }
            v
        }
    }

    unsafe fn read_i64_vec(v: *mut HewVec) -> Vec<i64> {
        unsafe {
            let len = (*v).len;
            if len == 0 {
                return Vec::new();
            }
            core::slice::from_raw_parts((*v).data.cast::<i64>(), len).to_vec()
        }
    }

    unsafe fn make_f64_vec(vals: &[f64]) -> *mut HewVec {
        unsafe {
            let v = hew_runtime::vec::hew_vec_new_f64();
            for &val in vals {
                hew_runtime::vec::hew_vec_push_f64(v, val);
            }
            v
        }
    }

    unsafe fn read_f64_vec(v: *mut HewVec) -> Vec<f64> {
        unsafe {
            let len = (*v).len;
            if len == 0 {
                return Vec::new();
            }
            core::slice::from_raw_parts((*v).data.cast::<f64>(), len).to_vec()
        }
    }

    unsafe fn make_str_vec(vals: &[&str]) -> *mut HewVec {
        unsafe {
            let v = hew_runtime::vec::hew_vec_new_str();
            for val in vals {
                let cs = std::ffi::CString::new(*val).unwrap();
                hew_runtime::vec::hew_vec_push_str(v, cs.as_ptr());
            }
            v
        }
    }

    unsafe fn read_str_vec(v: *mut HewVec) -> Vec<String> {
        unsafe {
            let len = (*v).len;
            if len == 0 {
                return Vec::new();
            }
            let ptrs = core::slice::from_raw_parts((*v).data.cast::<*const libc::c_char>(), len);
            ptrs.iter()
                .map(|p| CStr::from_ptr(*p).to_string_lossy().into_owned())
                .collect()
        }
    }

    #[test]
    fn sort_ints_ascending() {
        unsafe {
            let v = make_i64_vec(&[3, 1, 4, 1, 5, 9, 2, 6]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), vec![1, 1, 2, 3, 4, 5, 6, 9]);
            // Original unchanged
            assert_eq!(read_i64_vec(v), vec![3, 1, 4, 1, 5, 9, 2, 6]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_strings_alphabetical() {
        unsafe {
            let v = make_str_vec(&["banana", "apple", "cherry", "date"]);
            let sorted = hew_sort_strings(v);
            assert_eq!(
                read_str_vec(sorted),
                vec!["apple", "banana", "cherry", "date"]
            );
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_floats_ascending() {
        unsafe {
            let v = make_f64_vec(&[3.14, 1.0, 2.71, 0.5]);
            let sorted = hew_sort_floats(v);
            assert_eq!(read_f64_vec(sorted), vec![0.5, 1.0, 2.71, 3.14]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_ints() {
        unsafe {
            let v = make_i64_vec(&[1, 2, 3, 4, 5]);
            let rev = hew_sort_reverse(v);
            assert_eq!(read_i64_vec(rev), vec![5, 4, 3, 2, 1]);
            hew_runtime::vec::hew_vec_free(rev);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn empty_vec_sort() {
        unsafe {
            let v = make_i64_vec(&[]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), Vec::<i64>::new());
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn single_element_sort() {
        unsafe {
            let v = make_i64_vec(&[42]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), vec![42]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_ints_already_sorted() {
        unsafe {
            let v = make_i64_vec(&[1, 2, 3, 4, 5]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), vec![1, 2, 3, 4, 5]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_ints_reverse_sorted() {
        unsafe {
            let v = make_i64_vec(&[5, 4, 3, 2, 1]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), vec![1, 2, 3, 4, 5]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_ints_duplicates() {
        unsafe {
            let v = make_i64_vec(&[3, 1, 3, 1, 2]);
            let sorted = hew_sort_ints(v);
            assert_eq!(read_i64_vec(sorted), vec![1, 1, 2, 3, 3]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_strings_empty() {
        unsafe {
            let v = make_str_vec(&[]);
            let sorted = hew_sort_strings(v);
            assert_eq!(read_str_vec(sorted), Vec::<String>::new());
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn sort_floats_negative() {
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
        unsafe {
            let v = make_f64_vec(&[1.0, 0.0, -1.0]);
            let sorted = hew_sort_floats(v);
            assert_eq!(read_f64_vec(sorted), vec![-1.0, 0.0, 1.0]);
            hew_runtime::vec::hew_vec_free(sorted);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_empty() {
        unsafe {
            let v = make_i64_vec(&[]);
            let rev = hew_sort_reverse(v);
            assert_eq!(read_i64_vec(rev), Vec::<i64>::new());
            hew_runtime::vec::hew_vec_free(rev);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn reverse_single() {
        unsafe {
            let v = make_i64_vec(&[42]);
            let rev = hew_sort_reverse(v);
            assert_eq!(read_i64_vec(rev), vec![42]);
            hew_runtime::vec::hew_vec_free(rev);
            hew_runtime::vec::hew_vec_free(v);
        }
    }

    #[test]
    fn null_returns_null() {
        unsafe {
            assert!(hew_sort_ints(std::ptr::null_mut()).is_null());
            assert!(hew_sort_strings(std::ptr::null_mut()).is_null());
            assert!(hew_sort_floats(std::ptr::null_mut()).is_null());
            assert!(hew_sort_reverse(std::ptr::null_mut()).is_null());
        }
    }
}
