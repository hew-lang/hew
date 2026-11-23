//! Collection iterator runtime support.
//! Iterator protocol for Hew collections.

use core::ffi::c_void;

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
    if vec.is_null() {
        return core::ptr::null_mut();
    }
    // SAFETY: caller guarantees `vec` is valid.
    unsafe {
        let v = &*vec;
        let iter: *mut HewIter = crate::mem::buf_try_alloc(core::mem::size_of::<HewIter>()).cast(); // ALLOCATOR-PAIRING: GlobalAlloc
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
        unsafe { crate::mem::buf_free(iter.cast()) }; // ALLOCATOR-PAIRING: GlobalAlloc
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
    if iter.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `iter` is valid and the vec is still alive.
    unsafe {
        let iter_ref = &mut *iter;
        if iter_ref.vec.is_null() {
            return 0;
        }
        let len = (*iter_ref.vec).len;
        if iter_ref.index < len {
            iter_ref.index += 1;
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
    if iter.is_null() {
        return;
    }
    // SAFETY: caller guarantees `iter` is valid.
    unsafe {
        (*iter).index = 0;
    }
}

// ---------------------------------------------------------------------------
// Value accessors — each reads the element at `index - 1` (set by `next`)
// ---------------------------------------------------------------------------

/// Return a pointer to the current element slot when `iter`, its backing vec,
/// and the cursor bounds are valid.
///
/// # Safety
///
/// If non-null, `iter` and its backing `HewVec` must remain live and valid.
unsafe fn iter_current_slot(
    iter: *const HewIter,
    expected_size: Option<usize>,
) -> Option<*const u8> {
    // SAFETY: caller ensures `iter` is null or points to a live `HewIter`.
    let iter_ref = unsafe { iter.as_ref()? };
    // SAFETY: iterator holds null or a live `HewVec` pointer for its lifetime.
    let vec = unsafe { iter_ref.vec.as_ref()? };
    if vec.data.is_null() {
        return None;
    }
    let idx = iter_ref.index.checked_sub(1)?;
    if idx >= vec.len {
        return None;
    }
    let step = if let Some(expected) = expected_size {
        if iter_ref.elem_size != expected || vec.elem_size != expected {
            return None;
        }
        expected
    } else {
        iter_ref.elem_size
    };
    let byte_offset = idx.checked_mul(step)?;
    // SAFETY: `idx < vec.len` and `byte_offset` lies inside `vec.data`.
    Some(unsafe { vec.data.cast::<u8>().add(byte_offset) })
}

/// Return the current `i32` value.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds `i32`.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_i32(iter: *const HewIter) -> i32 {
    // SAFETY: `iter_current_slot` validated bounds and `size_of::<i32>()`.
    unsafe {
        iter_current_slot(iter, Some(core::mem::size_of::<i32>()))
            .map_or(0, |slot| slot.cast::<i32>().read_unaligned())
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
    // SAFETY: `iter_current_slot` validated bounds and `size_of::<i64>()`.
    unsafe {
        iter_current_slot(iter, Some(core::mem::size_of::<i64>()))
            .map_or(0, |slot| slot.cast::<i64>().read_unaligned())
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
    // SAFETY: `iter_current_slot` validated bounds and `size_of::<f64>()`.
    unsafe {
        iter_current_slot(iter, Some(core::mem::size_of::<f64>()))
            .map_or(0.0, |slot| slot.cast::<f64>().read_unaligned())
    }
}

/// Return the current borrowed managed string handle.
///
/// # Safety
///
/// `iter` must be a valid `HewIter` whose underlying vec holds managed strings.
/// `hew_iter_next` must have returned 1 before calling this.
#[no_mangle]
pub unsafe extern "C" fn hew_iter_value_str(
    iter: *const HewIter,
) -> *const hew_cabi::string::HewString {
    // SAFETY: `iter_current_slot` validated bounds and handle size.
    unsafe {
        iter_current_slot(
            iter,
            Some(core::mem::size_of::<*const hew_cabi::string::HewString>()),
        )
        .map_or(core::ptr::null(), |slot| {
            slot.cast::<*const hew_cabi::string::HewString>()
                .read_unaligned()
        })
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
    // SAFETY: `iter_current_slot` validated non-null and `idx < vec.len`.
    unsafe { iter_current_slot(iter, None).map_or(core::ptr::null(), <*const u8>::cast) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::{
        hew_vec_free, hew_vec_new, hew_vec_new_f64, hew_vec_new_i64, hew_vec_new_str,
        hew_vec_push_f64, hew_vec_push_i32, hew_vec_push_i64, hew_vec_push_str,
    };
    use hew_cabi::string::{string_as_str, string_from_str, string_release};

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
            let s1 = string_from_str("hello");
            let s2 = string_from_str("");
            let s3 = string_from_str("world");
            hew_vec_push_str(v, s1);
            hew_vec_push_str(v, s2);
            hew_vec_push_str(v, s3);
            string_release(s1);
            string_release(s2);
            string_release(s3);

            let it = hew_iter_vec(v);

            assert_eq!(hew_iter_next(it), 1);
            // SAFETY: value_str returns a strdup'd pointer valid for the vec's lifetime.
            assert_eq!(string_as_str(hew_iter_value_str(it)), "hello");

            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(string_as_str(hew_iter_value_str(it)), "");

            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(string_as_str(hew_iter_value_str(it)), "world");

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

    #[test]
    fn wave4b_iter_null_and_before_next_bounds_fail_closed() {
        // SAFETY: tests explicit null, underflow, and mismatched element size guards.
        unsafe {
            assert!(hew_iter_vec(core::ptr::null()).is_null());
            assert_eq!(hew_iter_next(core::ptr::null_mut()), 0);
            hew_iter_reset(core::ptr::null_mut());
            assert_eq!(hew_iter_value_i32(core::ptr::null()), 0);
            assert_eq!(hew_iter_value_i64(core::ptr::null()), 0);
            assert_eq!(
                hew_iter_value_f64(core::ptr::null()).to_bits(),
                0.0_f64.to_bits()
            );
            assert!(hew_iter_value_str(core::ptr::null()).is_null());
            assert!(hew_iter_value_ptr(core::ptr::null()).is_null());

            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 99);
            let it = hew_iter_vec(v);
            assert!(!it.is_null());
            // Before `hew_iter_next`, index == 0 must fail closed without underflow.
            assert_eq!(hew_iter_value_i64(it), 0);
            assert!(hew_iter_value_ptr(it).is_null());
            assert_eq!(hew_iter_next(it), 1);
            assert_eq!(hew_iter_value_i64(it), 99);
            // Mismatched element width must fail closed instead of reading wrong width.
            assert_eq!(hew_iter_value_i32(it), 0);
            assert_eq!(hew_iter_next(it), 0);
            // Mutating index past `vec.len` fails closed via `idx < vec.len`.
            (*it).index = 99;
            assert_eq!(hew_iter_value_i64(it), 0);
            assert!(hew_iter_value_ptr(it).is_null());
            hew_iter_free(it);
            hew_vec_free(v);
        }
    }

    #[test]
    fn iter_values_handle_unaligned_packed_buffers_safely() {
        // SAFETY: points each vector's `data` temporarily at a 1-byte misaligned
        // scratch buffer so Miri verifies unaligned loads succeed across all four types.
        unsafe {
            let mut storage = [0_u8; 32];
            let base = storage.as_mut_ptr().add(1);
            let val_int32: i32 = -123_456;
            let val_long64: i64 = 0x1122_3344_5566_7788;
            let val_double: f64 = 3.25;
            let packed_str = string_from_str("packed");

            let int32_vec = hew_vec_new();
            hew_vec_push_i32(int32_vec, 0);
            let orig_int32_buf = (*int32_vec).data;
            core::ptr::copy_nonoverlapping(
                (&raw const val_int32).cast::<u8>(),
                base,
                core::mem::size_of::<i32>(),
            );
            (*int32_vec).data = base;
            let int32_iter = hew_iter_vec(int32_vec);
            assert_eq!(hew_iter_next(int32_iter), 1);
            assert_eq!(hew_iter_value_i32(int32_iter), val_int32);
            (*int32_vec).data = orig_int32_buf;
            hew_iter_free(int32_iter);
            hew_vec_free(int32_vec);

            let long64_vec = hew_vec_new_i64();
            hew_vec_push_i64(long64_vec, 0);
            let orig_long64_buf = (*long64_vec).data;
            core::ptr::copy_nonoverlapping(
                (&raw const val_long64).cast::<u8>(),
                base,
                core::mem::size_of::<i64>(),
            );
            (*long64_vec).data = base;
            let long64_iter = hew_iter_vec(long64_vec);
            assert_eq!(hew_iter_next(long64_iter), 1);
            assert_eq!(hew_iter_value_i64(long64_iter), val_long64);
            (*long64_vec).data = orig_long64_buf;
            hew_iter_free(long64_iter);
            hew_vec_free(long64_vec);

            let double_vec = hew_vec_new_f64();
            hew_vec_push_f64(double_vec, 0.0);
            let orig_double_buf = (*double_vec).data;
            core::ptr::copy_nonoverlapping(
                (&raw const val_double).cast::<u8>(),
                base,
                core::mem::size_of::<f64>(),
            );
            (*double_vec).data = base;
            let double_iter = hew_iter_vec(double_vec);
            assert_eq!(hew_iter_next(double_iter), 1);
            assert_eq!(
                hew_iter_value_f64(double_iter).to_bits(),
                val_double.to_bits()
            );
            (*double_vec).data = orig_double_buf;
            hew_iter_free(double_iter);
            hew_vec_free(double_vec);

            let str_vec = hew_vec_new_str();
            let placeholder = string_from_str("tmp");
            hew_vec_push_str(str_vec, placeholder);
            string_release(placeholder);
            let orig_str_buf = (*str_vec).data;
            core::ptr::copy_nonoverlapping(
                (&raw const packed_str).cast::<u8>(),
                base,
                core::mem::size_of::<*const hew_cabi::string::HewString>(),
            );
            (*str_vec).data = base;
            let str_iter = hew_iter_vec(str_vec);
            assert_eq!(hew_iter_next(str_iter), 1);
            assert_eq!(hew_iter_value_str(str_iter), packed_str.cast_const());
            (*str_vec).data = orig_str_buf;
            hew_iter_free(str_iter);
            hew_vec_free(str_vec);
            string_release(packed_str);
        }
    }
}
