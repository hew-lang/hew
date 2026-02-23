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
