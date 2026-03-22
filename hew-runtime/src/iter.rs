//! Iterator protocol for Hew collections.

// Pointer casts from the data buffer are safe because the backing storage
// is allocated via `libc::realloc` which guarantees max alignment.
#![expect(
    clippy::cast_ptr_alignment,
    reason = "data buffer allocated via libc::realloc which guarantees max alignment"
)]

use core::ffi::{c_char, c_void};

use crate::vec::HewVec;

/// Opaque iterator over a collection.
///
/// Stores a pointer to the `HewVec` itself (not the data pointer) so that
/// the iterator remains valid even after the vec reallocates its backing
/// buffer.
#[repr(C)]
#[derive(Debug)]
pub struct HewIter {
    /// Pointer to the vec being iterated (NOT the data buffer).
    vec: *const HewVec,
    /// Current position. Value accessors read element at `index - 1`
    /// (set by `next()`). Starts at 0 so the first `next()` advances to 1
    /// and the first element (index 0) is read.
    index: usize,
    elem_size: usize,
    kind: IterKind,
}

/// Discriminator for the element type being iterated.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum IterKind {
    /// `Vec<i32>`
    VecI32 = 0,
    /// `Vec<i64>`
    VecI64 = 1,
    /// `Vec<f64>`
    VecF64 = 2,
    /// `Vec<string>`
    VecStr = 3,
    /// Generic (arbitrary `elem_size`)
    VecGeneric = 4,
}

/// Infer `IterKind` from the element size stored in the vec.
fn kind_from_elem_size(elem_size: usize) -> IterKind {
    if elem_size == core::mem::size_of::<i32>() {
        IterKind::VecI32
    } else if elem_size == core::mem::size_of::<i64>() {
        IterKind::VecI64
    } else {
        IterKind::VecGeneric
    }
}

// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------

/// Create an iterator over a `HewVec`.
///
/// The iterator does **not** own the vec — the caller must keep it alive for
/// the lifetime of the iterator. The iterator stores a pointer to the vec
/// itself so it remains valid after vec reallocations.
///
/// # Safety
///
/// `vec` must be a valid, non-null `HewVec` pointer that outlives the
/// returned iterator.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_vec(vec: *const HewVec) -> *mut HewIter {
    // SAFETY: caller guarantees `vec` is valid.
    unsafe {
        let v = &*vec;
        let iter: *mut HewIter = libc::malloc(core::mem::size_of::<HewIter>()).cast();
        if iter.is_null() {
            libc::abort();
        }
        (*iter).vec = vec;
        (*iter).index = 0;
        (*iter).elem_size = v.elem_size;
        (*iter).kind = kind_from_elem_size(v.elem_size);
        iter
    }
}

/// Free the iterator. Does **not** free the underlying collection.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_iter_free(iter: *mut HewIter) {
    if !iter.is_null() {
        // SAFETY: caller guarantees `iter` was allocated with malloc.
        unsafe { libc::free(iter.cast()) };
    }
}

// ---------------------------------------------------------------------------
// Traversal
// ---------------------------------------------------------------------------

/// Advance the iterator. Returns 1 if there is a current value, 0 if
/// iteration is complete. Reads `len` from the vec on each call so the
/// iterator sees mutations.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_next(iter: *mut HewIter) -> i32 {
    // SAFETY: caller guarantees `iter` is valid and the vec is still alive.
    unsafe {
        let len = (*(*iter).vec).len;
        if (*iter).index < len {
            (*iter).index += 1;
            1
        } else {
            0
        }
    }
}

/// Reset the iterator to the beginning.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_reset(iter: *mut HewIter) {
    // SAFETY: caller guarantees `iter` is valid.
    unsafe {
        (*iter).index = 0;
    }
}

// ---------------------------------------------------------------------------
// Value accessors — each reads the element at `index - 1` (set by `next`)
// ---------------------------------------------------------------------------

/// Return the current `i32` value.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds `i32`.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_i32(iter: *const HewIter) -> i32 {
    // SAFETY: caller guarantees `iter` is valid and positioned via next().
    unsafe {
        let idx = (*iter).index - 1;
        let data = (*(*iter).vec).data;
        data.cast::<i32>().add(idx).read()
    }
}

/// Return the current `i64` value.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds `i64`.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_i64(iter: *const HewIter) -> i64 {
    // SAFETY: caller guarantees `iter` is valid and positioned via next().
    unsafe {
        let idx = (*iter).index - 1;
        let data = (*(*iter).vec).data;
        data.cast::<i64>().add(idx).read()
    }
}

/// Return the current `f64` value.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds `f64`.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_f64(iter: *const HewIter) -> f64 {
    // SAFETY: caller guarantees `iter` is valid and positioned via next().
    unsafe {
        let idx = (*iter).index - 1;
        let data = (*(*iter).vec).data;
        data.cast::<f64>().add(idx).read()
    }
}

/// Return the current string value (pointer to the C string stored in the vec).
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds `*const c_char`.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_str(iter: *const HewIter) -> *const c_char {
    // SAFETY: caller guarantees `iter` is valid and positioned via next().
    unsafe {
        let idx = (*iter).index - 1;
        let data = (*(*iter).vec).data;
        data.cast::<*const c_char>().add(idx).read()
    }
}

/// Return a raw pointer to the current element (generic access).
///
/// # Safety
///
/// `iter` must be a valid `HewIter`. `hew_iter_next` must have returned 1
/// before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_ptr(iter: *const HewIter) -> *const c_void {
    // SAFETY: caller guarantees `iter` is valid and positioned via next().
    unsafe {
        let idx = (*iter).index - 1;
        let data = (*(*iter).vec).data;
        data.cast::<u8>().add(idx * (*iter).elem_size).cast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::{
        hew_vec_free, hew_vec_new, hew_vec_new_f64, hew_vec_new_i64, hew_vec_new_str,
        hew_vec_push_f64, hew_vec_push_i32, hew_vec_push_i64, hew_vec_push_str,
    };
    use std::ffi::{CStr, CString};

    // -- Empty vec ----------------------------------------------------------

    #[test]
    fn empty_vec_next_returns_false() {
        // Iterating an empty collection should immediately signal exhaustion.
        // SAFETY: FFI calls use valid pointers returned by hew_vec/iter constructors.
        unsafe {
            let v = hew_vec_new_i64();
            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 0);
            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Single element -----------------------------------------------------

    #[test]
    fn single_i64_yields_value_then_exhausts() {
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 42);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 42);
            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Multiple elements --------------------------------------------------

    #[test]
    fn multiple_i64_iterated_in_order() {
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 10);
            hew_vec_push_i64(v, 20);
            hew_vec_push_i64(v, 30);

            let it = hew_iter_vec(v);
            let mut collected = Vec::new();
            while hew_iter_next(it) == 1 {
                collected.push(hew_iter_value_i64(it));
            }
            assert_eq!(collected, [10, 20, 30]);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Reset --------------------------------------------------------------

    #[test]
    fn reset_restarts_iteration_from_beginning() {
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 100);
            hew_vec_push_i64(v, 200);

            let it = hew_iter_vec(v);
            // Consume all elements.
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 100);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 200);
            assert_eq!(hew_iter_next(it), 0);

            // Reset and iterate again — should see the same values.
            hew_iter_reset(it);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 100);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 200);
            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    #[test]
    fn reset_midway_restarts_from_beginning() {
        // Reset partway through and verify we get the first element again.
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 1);
            hew_vec_push_i64(v, 2);
            hew_vec_push_i64(v, 3);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 1);

            hew_iter_reset(it);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 1);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Type-specific round-trips ------------------------------------------

    #[test]
    fn i32_round_trip() {
        // SAFETY: FFI calls use valid pointers; vec holds i32 elements.
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, -7);
            hew_vec_push_i32(v, 0);
            hew_vec_push_i32(v, i32::MAX);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i32(it), -7);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i32(it), 0);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i32(it), i32::MAX);
            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    #[test]
    fn f64_round_trip() {
        // SAFETY: FFI calls use valid pointers; vec holds f64 elements.
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, -0.5);
            hew_vec_push_f64(v, 2.75);
            hew_vec_push_f64(v, f64::INFINITY);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            // SAFETY: next() returned 1, so value_f64 reads a valid positioned element.
            assert!(
                (hew_iter_value_f64(it) - (-0.5)).abs() < f64::EPSILON,
                "expected -0.5"
            );
            assert_eq!(hew_iter_next(it), 1);
            assert!(
                (hew_iter_value_f64(it) - 2.75).abs() < f64::EPSILON,
                "expected 2.75"
            );
            assert_eq!(hew_iter_next(it), 1);
            assert!(hew_iter_value_f64(it).is_infinite(), "expected infinity");
            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    #[test]
    fn string_round_trip() {
        // SAFETY: FFI calls use valid pointers and valid C strings.
        unsafe {
            let v = hew_vec_new_str();
            let s1 = CString::new("hello").unwrap();
            let s2 = CString::new("").unwrap();
            let s3 = CString::new("world").unwrap();
            hew_vec_push_str(v, s1.as_ptr());
            hew_vec_push_str(v, s2.as_ptr());
            hew_vec_push_str(v, s3.as_ptr());

            let it = hew_iter_vec(v);

            assert_eq!(hew_iter_next(it), 1);
            // SAFETY: value_str returns a strdup'd pointer valid for the vec's lifetime.
            let val = CStr::from_ptr(hew_iter_value_str(it));
            assert_eq!(val.to_str().unwrap(), "hello");

            assert_eq!(hew_iter_next(it), 1);
            let val = CStr::from_ptr(hew_iter_value_str(it));
            assert_eq!(val.to_str().unwrap(), "");

            assert_eq!(hew_iter_next(it), 1);
            let val = CStr::from_ptr(hew_iter_value_str(it));
            assert_eq!(val.to_str().unwrap(), "world");

            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    #[test]
    fn value_ptr_returns_element_address() {
        // Generic pointer accessor should yield the address of each element.
        // SAFETY: FFI calls use valid pointers; vec holds i64 elements.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 0xCAFE);
            hew_vec_push_i64(v, 0xBEEF);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            // SAFETY: value_ptr returns pointer into the vec's data buffer;
            // we read as i64 matching the vec's element type.
            let ptr = hew_iter_value_ptr(it).cast::<i64>();
            assert_eq!(ptr.read(), 0xCAFE);

            assert_eq!(hew_iter_next(it), 1);
            let ptr = hew_iter_value_ptr(it).cast::<i64>();
            assert_eq!(ptr.read(), 0xBEEF);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Boundary: iterate past the end -------------------------------------

    #[test]
    fn next_past_end_keeps_returning_zero() {
        // Calling next() repeatedly after exhaustion should always return 0.
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 1);

            let it = hew_iter_vec(v);
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_next(it), 0);
            assert_eq!(hew_iter_next(it), 0);
            assert_eq!(hew_iter_next(it), 0);

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Mutation after iterator creation -----------------------------------

    #[test]
    fn iter_sees_elements_pushed_after_creation() {
        // The iterator stores a pointer to the vec (not the data buffer), so
        // it must observe mutations — including reallocation — made after the
        // iterator was created.
        // SAFETY: FFI calls use valid pointers; vec outlives iterator.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 1);

            let it = hew_iter_vec(v);

            // Push enough elements to force at least one reallocation (initial
            // capacity is 4).
            for val in 2..=10 {
                hew_vec_push_i64(v, val);
            }

            let mut collected = Vec::new();
            while hew_iter_next(it) == 1 {
                collected.push(hew_iter_value_i64(it));
            }
            assert_eq!(collected, (1..=10).collect::<Vec<i64>>());

            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    // -- Null guards --------------------------------------------------------

    #[test]
    fn free_null_is_safe() {
        // SAFETY: hew_iter_free explicitly handles null pointers.
        unsafe {
            hew_iter_free(core::ptr::null_mut());
        }
    }

    // -- kind_from_elem_size ------------------------------------------------

    #[test]
    fn kind_from_elem_size_i32() {
        assert!(matches!(
            kind_from_elem_size(core::mem::size_of::<i32>()),
            IterKind::VecI32
        ));
    }

    #[test]
    fn kind_from_elem_size_i64() {
        assert!(matches!(
            kind_from_elem_size(core::mem::size_of::<i64>()),
            IterKind::VecI64
        ));
    }

    #[test]
    fn kind_from_elem_size_unusual_falls_back_to_generic() {
        // An unusual elem_size (e.g. 3 bytes) should yield VecGeneric.
        assert!(matches!(kind_from_elem_size(3), IterKind::VecGeneric));
    }
}
