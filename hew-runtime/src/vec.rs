//! Hew runtime: `vec` module.
//!
//! Dynamic array (`HewVec`) with C ABI, matching the C runtime layout exactly.
//! Supports i32, i64, f64, and string element types.
//!
//! Type definitions (`ElemKind`, `HewVec`) are re-exported from `hew-cabi`.
//! This module provides the actual implementations of all `hew_vec_*` functions.

// The `data` field is `*mut u8` (matching C `void*`) but always allocated via
// `realloc` which guarantees max alignment.  Casts to typed pointers are safe.
#![expect(
    clippy::cast_ptr_alignment,
    reason = "data buffer allocated via libc::realloc which guarantees max alignment"
)]
// ABI boundary uses i64 (Hew's `int`) for sizes/indices; internal code needs usize.
#![expect(
    clippy::cast_sign_loss,
    reason = "i64 index/size from codegen is always non-negative at the ABI boundary"
)]
#![expect(
    clippy::cast_possible_truncation,
    reason = "i64→usize: values originate from usize-range lengths; safe on both 32-bit and 64-bit"
)]

// Re-export types from hew-cabi so `crate::vec::HewVec` etc. continue to work.
pub use hew_cabi::vec::{ElemKind, HewVec};

use core::ffi::{c_char, c_void};
use core::ptr;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/// Write a message to stderr (fd 2) in a signal-safe, cross-platform manner.
///
/// # Safety
///
/// `msg` must be a valid byte slice. This is safe to call in abort paths.
unsafe fn write_stderr(msg: &[u8]) {
    unsafe {
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
    }
}

/// Ensure `v` can hold at least `needed` elements, growing if necessary.
///
/// # Safety
///
/// `v` must point to a valid, non-null `HewVec` allocated with `libc::malloc`.
unsafe fn ensure_cap(v: *mut HewVec, needed: usize) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let vec = &mut *v;
        if vec.cap >= needed {
            return;
        }
        let mut new_cap = if vec.cap == 0 { 4 } else { vec.cap };
        while new_cap < needed {
            new_cap = if let Some(c) = new_cap.checked_mul(2) {
                c
            } else {
                // SAFETY: writing to stderr and aborting is always safe.
                let msg = b"PANIC: Vec capacity overflow\n\0";
                write_stderr(&msg[..msg.len() - 1]);
                libc::abort();
            };
        }
        let Some(alloc_size) = new_cap.checked_mul(vec.elem_size) else {
            // SAFETY: writing to stderr and aborting is always safe.
            let msg = b"PANIC: Vec allocation size overflow\n\0";
            write_stderr(&msg[..msg.len() - 1]);
            libc::abort();
        };
        let new_data = libc::realloc(vec.data.cast(), alloc_size);
        if new_data.is_null() {
            libc::abort();
        }
        vec.data = new_data.cast();
        vec.cap = new_cap;
    }
}

/// Abort with an out-of-bounds message.
unsafe fn abort_oob(index: usize, len: usize) -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec index out of bounds\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        let _ = (index, len); // avoid unused warnings
        libc::abort();
    }
}

/// Abort on pop of empty vec.
unsafe fn abort_pop_empty() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec pop on empty vector\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        libc::abort();
    }
}

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

/// Create a new `HewVec` with the given element size.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_with_elem_size(elem_size: i64) -> *mut HewVec {
    // SAFETY: allocating a zeroed struct with libc::malloc is safe.
    unsafe {
        let v: *mut HewVec = libc::malloc(core::mem::size_of::<HewVec>()).cast();
        if v.is_null() {
            libc::abort();
        }
        (*v).data = ptr::null_mut();
        (*v).len = 0;
        (*v).cap = 0;
        (*v).elem_size = elem_size as usize;
        (*v).elem_kind = ElemKind::Plain;
        v
    }
}

/// Create a new `HewVec` for `i32` elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new() -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`.
    unsafe { hew_vec_new_with_elem_size(core::mem::size_of::<i32>() as i64) }
}

/// Create a new `HewVec` for string (`*const c_char`) elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_str() -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`.
    let v = unsafe { hew_vec_new_with_elem_size(core::mem::size_of::<*const c_char>() as i64) };
    // SAFETY: v is non-null (hew_vec_new_with_elem_size aborts on OOM).
    unsafe { (*v).elem_kind = ElemKind::String };
    v
}

/// Create a new `HewVec` for `i64` elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_i64() -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`.
    unsafe { hew_vec_new_with_elem_size(core::mem::size_of::<i64>() as i64) }
}

/// Create a new `HewVec` for `f64` elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_f64() -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`.
    unsafe { hew_vec_new_with_elem_size(core::mem::size_of::<f64>() as i64) }
}

/// Create a new `HewVec` for pointer-sized elements (e.g. `ActorRef`, handles).
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_ptr() -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`.
    unsafe { hew_vec_new_with_elem_size(core::mem::size_of::<*mut c_void>() as i64) }
}

// ---------------------------------------------------------------------------
// Push
// ---------------------------------------------------------------------------

/// Push an `i32` value onto the vec.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer created by one of the `new` functions.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_i32(v: *mut HewVec, val: i32) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let slot = (*v).data.cast::<i32>().add((*v).len);
        slot.write(val);
        (*v).len += 1;
    }
}

/// Push an `i64` value onto the vec.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_i64(v: *mut HewVec, val: i64) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let slot = (*v).data.cast::<i64>().add((*v).len);
        slot.write(val);
        (*v).len += 1;
    }
}

/// Push a string onto the vec. The string is duplicated with `strdup`.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer. `val` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_str(v: *mut HewVec, val: *const c_char) {
    // SAFETY: caller guarantees `v` and `val` are valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let duped = libc::strdup(val);
        let slot = (*v).data.cast::<*mut c_char>().add((*v).len);
        slot.write(duped);
        (*v).len += 1;
    }
}

/// Push an `f64` value onto the vec.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_f64(v: *mut HewVec, val: f64) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let slot = (*v).data.cast::<f64>().add((*v).len);
        slot.write(val);
        (*v).len += 1;
    }
}

/// Push a raw pointer onto the vec (for `Vec<ActorRef<T>>` etc.).
///
/// # Safety
///
/// `v` must be a valid pointer `HewVec`.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_ptr(v: *mut HewVec, val: *mut c_void) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let slot = (*v).data.cast::<*mut c_void>().add((*v).len);
        slot.write(val);
        (*v).len += 1;
    }
}

// ---------------------------------------------------------------------------
// Get
// ---------------------------------------------------------------------------

/// Get an `i32` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<i32>().add(index).read()
    }
}

/// Get an `i64` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<i64>().add(index).read()
    }
}

/// Get a string pointer at `index`. Returns an internal pointer. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<*const c_char>().add(index).read()
    }
}

/// Get an `f64` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_f64(v: *mut HewVec, index: i64) -> f64 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<f64>().add(index).read()
    }
}

/// Get a raw pointer at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid pointer `HewVec`.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_ptr(v: *mut HewVec, index: i64) -> *mut c_void {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<*mut c_void>().add(index).read()
    }
}

// ---------------------------------------------------------------------------
// Set
// ---------------------------------------------------------------------------

/// Set an `i32` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_i32(v: *mut HewVec, index: i64, val: i32) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<i32>().add(index).write(val);
    }
}

/// Set an `i64` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_i64(v: *mut HewVec, index: i64, val: i64) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<i64>().add(index).write(val);
    }
}

/// Set a string at `index`. Frees the old string and duplicates the new one.
/// Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer. `val` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_str(v: *mut HewVec, index: i64, val: *const c_char) {
    // SAFETY: caller guarantees `v` and `val` are valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        let slot = (*v).data.cast::<*mut c_char>().add(index);
        let old = slot.read();
        if !old.is_null() {
            libc::free(old.cast());
        }
        slot.write(libc::strdup(val));
    }
}

/// Set an `f64` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_f64(v: *mut HewVec, index: i64, val: f64) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<f64>().add(index).write(val);
    }
}

// ---------------------------------------------------------------------------
// Pop
// ---------------------------------------------------------------------------

/// Pop the last `i32`. Aborts if empty.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_i32(v: *mut HewVec) -> i32 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if (*v).len == 0 {
            abort_pop_empty();
        }
        (*v).len -= 1;
        (*v).data.cast::<i32>().add((*v).len).read()
    }
}

/// Pop the last `i64`. Aborts if empty.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_i64(v: *mut HewVec) -> i64 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if (*v).len == 0 {
            abort_pop_empty();
        }
        (*v).len -= 1;
        (*v).data.cast::<i64>().add((*v).len).read()
    }
}

/// Pop the last string pointer. The caller now owns the returned pointer.
/// Aborts if empty.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_str(v: *mut HewVec) -> *const c_char {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if (*v).len == 0 {
            abort_pop_empty();
        }
        (*v).len -= 1;
        (*v).data.cast::<*const c_char>().add((*v).len).read()
    }
}

/// Pop the last `f64`. Aborts if empty.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_f64(v: *mut HewVec) -> f64 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if (*v).len == 0 {
            abort_pop_empty();
        }
        (*v).len -= 1;
        (*v).data.cast::<f64>().add((*v).len).read()
    }
}

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

/// Return the number of elements.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_len(v: *mut HewVec) -> i64 {
    // SAFETY: caller guarantees `v` is valid.
    unsafe { (*v).len as i64 }
}

/// Return whether the vec is empty.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_is_empty(v: *mut HewVec) -> bool {
    // SAFETY: caller guarantees `v` is valid.
    unsafe { (*v).len == 0 }
}

/// Free individual string elements in the range `[0, len)`.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer with `elem_kind == String`.
unsafe fn free_string_elements(v: *mut HewVec) {
    // SAFETY: caller guarantees `v` is a valid string HewVec.
    unsafe {
        let vec = &*v;
        for i in 0..vec.len {
            let slot = vec.data.cast::<*mut c_char>().add(i);
            let ptr = slot.read();
            if !ptr.is_null() {
                libc::free(ptr.cast());
            }
        }
    }
}

/// Clear the vec (set len to 0). Frees individual string elements if
/// `elem_kind == String`.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_clear(v: *mut HewVec) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if (*v).elem_kind == ElemKind::String {
            free_string_elements(v);
        }
        (*v).len = 0;
    }
}

/// Free the vec's backing data and the `HewVec` struct itself. Frees
/// individual string elements if `elem_kind == String`.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer (or null). After this call, `v` is
/// invalid.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_free(v: *mut HewVec) {
    // SAFETY: caller guarantees `v` was allocated with malloc (or is null).
    unsafe {
        if !v.is_null() {
            if !(*v).data.is_null() {
                if (*v).elem_kind == ElemKind::String {
                    free_string_elements(v);
                }
                libc::free((*v).data.cast());
            }
            libc::free(v.cast());
        }
    }
}

// ---------------------------------------------------------------------------
// Sort
// ---------------------------------------------------------------------------

/// Sort an `i32` vec in-place in ascending order.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_sort_i32(v: *mut HewVec) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid i32 HewVec.
    unsafe {
        let len = (*v).len;
        if len <= 1 {
            return;
        }
        let slice = core::slice::from_raw_parts_mut((*v).data.cast::<i32>(), len);
        slice.sort_unstable();
    }
}

/// Sort an `i64` vec in-place in ascending order.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_sort_i64(v: *mut HewVec) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid i64 HewVec.
    unsafe {
        let len = (*v).len;
        if len <= 1 {
            return;
        }
        let slice = core::slice::from_raw_parts_mut((*v).data.cast::<i64>(), len);
        slice.sort_unstable();
    }
}

/// Sort an `f64` vec in-place in ascending order using `total_cmp`.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_sort_f64(v: *mut HewVec) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid f64 HewVec.
    unsafe {
        let len = (*v).len;
        if len <= 1 {
            return;
        }
        let slice = core::slice::from_raw_parts_mut((*v).data.cast::<f64>(), len);
        slice.sort_unstable_by(f64::total_cmp);
    }
}

// ---------------------------------------------------------------------------
// Clone
// ---------------------------------------------------------------------------

/// Deep-clone a `HewVec`. For string vecs, each element is `strdup`'d.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer (or null, which returns null).
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_clone(v: *const HewVec) -> *mut HewVec {
    if v.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let src = &*v;
        let new_v = hew_vec_new_with_elem_size(src.elem_size as i64);
        (*new_v).elem_kind = src.elem_kind;
        if src.len == 0 {
            return new_v;
        }
        ensure_cap(new_v, src.len);
        if src.elem_kind == ElemKind::String {
            for i in 0..src.len {
                let src_ptr = src.data.cast::<*const c_char>().add(i).read();
                let duped = if src_ptr.is_null() {
                    ptr::null_mut()
                } else {
                    libc::strdup(src_ptr)
                };
                (*new_v).data.cast::<*mut c_char>().add(i).write(duped);
            }
        } else {
            let byte_count = src.len * src.elem_size;
            core::ptr::copy_nonoverlapping(src.data, (*new_v).data, byte_count);
        }
        (*new_v).len = src.len;
        new_v
    }
}

// ---------------------------------------------------------------------------
// Append (bulk)
// ---------------------------------------------------------------------------

/// Append all elements from `src` to `dst`.
/// Both vecs must have the same `elem_size`.
///
/// # Safety
///
/// Both `dst` and `src` must be valid `HewVec` pointers (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_append(dst: *mut HewVec, src: *const HewVec) {
    if dst.is_null() || src.is_null() {
        return;
    }
    // SAFETY: caller guarantees both pointers are valid HewVecs with matching
    // elem_size.
    unsafe {
        let src_len = (*src).len;
        if src_len == 0 {
            return;
        }
        ensure_cap(dst, (*dst).len + src_len);
        let elem_size = (*dst).elem_size;
        let dst_ptr = (*dst).data.add((*dst).len * elem_size);
        if (*dst).elem_kind == ElemKind::String {
            for i in 0..src_len {
                let src_str = (*src).data.cast::<*const c_char>().add(i).read();
                let duped = if src_str.is_null() {
                    ptr::null_mut()
                } else {
                    libc::strdup(src_str)
                };
                dst_ptr.cast::<*mut c_char>().add(i).write(duped);
            }
        } else {
            core::ptr::copy_nonoverlapping((*src).data, dst_ptr, src_len * elem_size);
        }
        (*dst).len += src_len;
    }
}

// ---------------------------------------------------------------------------
// Contains
// ---------------------------------------------------------------------------

/// Check if the i32 vec contains `val`. Returns 1 if found, 0 otherwise.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_contains_i32(v: *const HewVec, val: i32) -> i32 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `v` is a valid i32 HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<i32>();
        for i in 0..len {
            if data.add(i).read() == val {
                return 1;
            }
        }
        0
    }
}

/// Remove the first occurrence of `val` from the i32 vec.
/// Shifts subsequent elements left. No-op if not found.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_remove_i32(v: *mut HewVec, val: i32) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid i32 HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<i32>();
        for i in 0..len {
            if data.add(i).read() == val {
                // Shift elements left
                core::ptr::copy(data.add(i + 1), data.add(i), len - i - 1);
                (*v).len -= 1;
                return;
            }
        }
    }
}

/// Remove the first occurrence of `val` from the i64 vec.
/// Shifts subsequent elements left. No-op if not found.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_remove_i64(v: *mut HewVec, val: i64) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid i64 HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<i64>();
        for i in 0..len {
            if data.add(i).read() == val {
                // Shift elements left
                core::ptr::copy(data.add(i + 1), data.add(i), len - i - 1);
                (*v).len -= 1;
                return;
            }
        }
    }
}

/// Remove the first occurrence of `val` (pointer) from the vec.
/// Shifts subsequent elements left. No-op if not found.
///
/// # Safety
///
/// `v` must be a valid pointer `HewVec`.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_remove_ptr(v: *mut HewVec, val: *mut c_void) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid pointer HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<*mut c_void>();
        for i in 0..len {
            if data.add(i).read() == val {
                core::ptr::copy(data.add(i + 1), data.add(i), len - i - 1);
                (*v).len -= 1;
                return;
            }
        }
    }
}

/// Check if the i64 vec contains `val`. Returns 1 if found, 0 otherwise.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_contains_i64(v: *const HewVec, val: i64) -> i32 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `v` is a valid i64 HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<i64>();
        for i in 0..len {
            if data.add(i).read() == val {
                return 1;
            }
        }
        0
    }
}

/// Check if the f64 vec contains `val`. Returns 1 if found, 0 otherwise.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer (or null).
#[no_mangle]
#[expect(
    clippy::float_cmp,
    reason = "C ABI semantics: exact f64 equality match is intentional"
)]
pub unsafe extern "C" fn hew_vec_contains_f64(v: *const HewVec, val: f64) -> i32 {
    if v.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `v` is a valid f64 HewVec.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<f64>();
        for i in 0..len {
            if data.add(i).read() == val {
                return 1;
            }
        }
        0
    }
}

/// Check if the string vec contains `val` (using `strcmp`). Returns 1/0.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer (or null).
/// `val` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_contains_str(v: *const HewVec, val: *const c_char) -> i32 {
    if v.is_null() || val.is_null() {
        return 0;
    }
    // SAFETY: caller guarantees `v` is a valid string HewVec and `val` is valid.
    unsafe {
        let len = (*v).len;
        let data = (*v).data.cast::<*const c_char>();
        for i in 0..len {
            let elem = data.add(i).read();
            if !elem.is_null() && libc::strcmp(elem, val) == 0 {
                return 1;
            }
        }
        0
    }
}

// ---------------------------------------------------------------------------
// Swap
// ---------------------------------------------------------------------------

/// Swap two elements in the vec by index. Aborts if either index is OOB.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_swap(v: *mut HewVec, i: i64, j: i64) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let i = i as usize;
        let j = j as usize;
        let len = (*v).len;
        if i >= len {
            abort_oob(i, len);
        }
        if j >= len {
            abort_oob(j, len);
        }
        if i == j {
            return;
        }
        let elem_size = (*v).elem_size;
        let pi = (*v).data.add(i * elem_size);
        let pj = (*v).data.add(j * elem_size);
        core::ptr::swap_nonoverlapping(pi, pj, elem_size);
    }
}

// ---------------------------------------------------------------------------
// Truncate
// ---------------------------------------------------------------------------

/// Truncate the vec to `new_len`. If the vec holds string elements
/// (`elem_size == sizeof(*const c_char)`), freed elements are `free`'d.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_truncate(v: *mut HewVec, new_len: i64) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let new_len = new_len as usize;
        let vec = &mut *v;
        if new_len >= vec.len {
            return;
        }
        if vec.elem_kind == ElemKind::String {
            for i in new_len..vec.len {
                let slot = vec.data.cast::<*mut c_char>().add(i);
                let ptr = slot.read();
                if !ptr.is_null() {
                    libc::free(ptr.cast());
                }
            }
        }
        vec.len = new_len;
    }
}

// ---------------------------------------------------------------------------
// Reverse
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Generic (arbitrary element size via void* + elem_size)
// ---------------------------------------------------------------------------

/// Create a new `HewVec` for elements of `elem_size` bytes.
///
/// `elem_kind`: 0 = plain value, 1 = string (strdup'd ownership).
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_generic(elem_size: i64, elem_kind: i64) -> *mut HewVec {
    // SAFETY: forwarding to `hew_vec_new_with_elem_size`, then setting kind.
    unsafe {
        let v = hew_vec_new_with_elem_size(elem_size);
        (*v).elem_kind = if elem_kind == 1 {
            ElemKind::String
        } else {
            ElemKind::Plain
        };
        v
    }
}

/// Push an element of `vec.elem_size` bytes by copying from `data`.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer. `data` must point to at least
/// `elem_size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_generic(v: *mut HewVec, data: *const core::ffi::c_void) {
    // SAFETY: caller guarantees `v` and `data` are valid.
    unsafe {
        ensure_cap(v, (*v).len + 1);
        let elem_size = (*v).elem_size;
        let dst = (*v).data.add((*v).len * elem_size);
        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);
        (*v).len += 1;
    }
}

/// Return a pointer to the element at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer. The returned pointer is valid only
/// while the vec is not reallocated.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_generic(
    v: *const HewVec,
    index: i64,
) -> *const core::ffi::c_void {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.add(index * (*v).elem_size).cast()
    }
}

/// Overwrite the element at `index` by copying `elem_size` bytes from `data`.
/// Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer. `data` must point to at least
/// `elem_size` readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_generic(
    v: *mut HewVec,
    index: i64,
    data: *const core::ffi::c_void,
) {
    // SAFETY: caller guarantees `v` and `data` are valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        let elem_size = (*v).elem_size;
        let dst = (*v).data.add(index * elem_size);
        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);
    }
}

/// Pop the last element, copying it into `out`. Returns 1 on success, 0 if
/// the vec is empty.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer. `out` must point to at least
/// `elem_size` writable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_generic(v: *mut HewVec, out: *mut core::ffi::c_void) -> i32 {
    // SAFETY: caller guarantees `v` and `out` are valid.
    unsafe {
        if (*v).len == 0 {
            return 0;
        }
        (*v).len -= 1;
        let elem_size = (*v).elem_size;
        let src = (*v).data.add((*v).len * elem_size);
        core::ptr::copy_nonoverlapping(src, out.cast::<u8>(), elem_size);
        1
    }
}

// ---------------------------------------------------------------------------
// Reverse
// ---------------------------------------------------------------------------

/// Reverse an i32 vec in place.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_vec_reverse_i32(v: *mut HewVec) {
    if v.is_null() {
        return;
    }
    // SAFETY: caller guarantees `v` is a valid i32 HewVec.
    unsafe {
        let len = (*v).len;
        if len <= 1 {
            return;
        }
        let slice = core::slice::from_raw_parts_mut((*v).data.cast::<i32>(), len);
        slice.reverse();
    }
}

// ---------------------------------------------------------------------------
// bytes <-> HewVec helpers (used by codec wrappers)
// ---------------------------------------------------------------------------

/// Extract raw bytes from a `bytes`-typed `HewVec` (i32 elements, one byte per slot).
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` with i32 element size.
pub(crate) unsafe fn hwvec_to_u8(v: *mut HewVec) -> Vec<u8> {
    if v.is_null() {
        return Vec::new();
    }
    // SAFETY: caller guarantees v is a valid HewVec.
    let len = unsafe { hew_vec_len(v) };
    (0..len)
        .map(|i| {
            // SAFETY: i < len.
            #[expect(clippy::cast_sign_loss, reason = "byte values stored as i32 are 0-255")]
            let b = unsafe { hew_vec_get_i32(v, i) } as u8;
            b
        })
        .collect()
}

/// Create a new bytes-typed `HewVec` (i32 elements) from a raw u8 slice.
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
pub(crate) unsafe fn u8_to_hwvec(data: &[u8]) -> *mut HewVec {
    // SAFETY: hew_vec_new allocates a valid HewVec.
    let v = unsafe { hew_vec_new() };
    for &b in data {
        // SAFETY: v is non-null (hew_vec_new aborts on OOM).
        unsafe { hew_vec_push_i32(v, i32::from(b)) };
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_vec_new_and_len() {
        unsafe {
            let v = hew_vec_new();
            assert!(!v.is_null());
            assert_eq!(hew_vec_len(v), 0);
            assert!(hew_vec_is_empty(v));
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            assert_eq!(hew_vec_len(v), 3);
            assert_eq!(hew_vec_get_i32(v, 0), 10);
            assert_eq!(hew_vec_get_i32(v, 1), 20);
            assert_eq!(hew_vec_get_i32(v, 2), 30);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_i64() {
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 100);
            hew_vec_push_i64(v, 200);
            assert_eq!(hew_vec_len(v), 2);
            assert_eq!(hew_vec_get_i64(v, 0), 100);
            assert_eq!(hew_vec_get_i64(v, 1), 200);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_f64() {
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, 1.5);
            hew_vec_push_f64(v, 2.5);
            assert_eq!(hew_vec_len(v), 2);
            assert!((hew_vec_get_f64(v, 0) - 1.5).abs() < f64::EPSILON);
            assert!((hew_vec_get_f64(v, 1) - 2.5).abs() < f64::EPSILON);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_str() {
        unsafe {
            let v = hew_vec_new_str();
            let s1 = CString::new("hello").unwrap();
            let s2 = CString::new("world").unwrap();
            hew_vec_push_str(v, s1.as_ptr());
            hew_vec_push_str(v, s2.as_ptr());
            assert_eq!(hew_vec_len(v), 2);

            let r1 = hew_vec_get_str(v, 0);
            assert!(!r1.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(r1).to_string_lossy(), "hello");

            let r2 = hew_vec_get_str(v, 1);
            assert!(!r2.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(r2).to_string_lossy(), "world");
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_set_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_set_i32(v, 0, 99);
            assert_eq!(hew_vec_get_i32(v, 0), 99);
            assert_eq!(hew_vec_get_i32(v, 1), 2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_pop_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            let popped = hew_vec_pop_i32(v);
            assert_eq!(popped, 20);
            assert_eq!(hew_vec_len(v), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_pop_i64() {
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 100);
            hew_vec_push_i64(v, 200);
            let popped = hew_vec_pop_i64(v);
            assert_eq!(popped, 200);
            assert_eq!(hew_vec_len(v), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_clear() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_push_i32(v, 3);
            hew_vec_clear(v);
            assert_eq!(hew_vec_len(v), 0);
            assert!(hew_vec_is_empty(v));
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_sort_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 3);
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_sort_i32(v);
            assert_eq!(hew_vec_get_i32(v, 0), 1);
            assert_eq!(hew_vec_get_i32(v, 1), 2);
            assert_eq!(hew_vec_get_i32(v, 2), 3);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_contains_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            assert_eq!(hew_vec_contains_i32(v, 10), 1);
            assert_eq!(hew_vec_contains_i32(v, 20), 1);
            assert_eq!(hew_vec_contains_i32(v, 30), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_remove_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            hew_vec_remove_i32(v, 20);
            assert_eq!(hew_vec_len(v), 2);
            assert_eq!(hew_vec_get_i32(v, 0), 10);
            assert_eq!(hew_vec_get_i32(v, 1), 30);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_clone() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            let cloned = hew_vec_clone(v);
            assert_eq!(hew_vec_len(cloned), 2);
            assert_eq!(hew_vec_get_i32(cloned, 0), 1);
            assert_eq!(hew_vec_get_i32(cloned, 1), 2);
            // Mutating original doesn't affect clone
            hew_vec_set_i32(v, 0, 99);
            assert_eq!(hew_vec_get_i32(cloned, 0), 1);
            hew_vec_free(v);
            hew_vec_free(cloned);
        }
    }

    #[test]
    fn test_vec_swap() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 10);
            hew_vec_push_i32(v, 20);
            hew_vec_push_i32(v, 30);
            hew_vec_swap(v, 0, 2);
            assert_eq!(hew_vec_get_i32(v, 0), 30);
            assert_eq!(hew_vec_get_i32(v, 2), 10);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_truncate() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_push_i32(v, 3);
            hew_vec_push_i32(v, 4);
            hew_vec_truncate(v, 2);
            assert_eq!(hew_vec_len(v), 2);
            assert_eq!(hew_vec_get_i32(v, 0), 1);
            assert_eq!(hew_vec_get_i32(v, 1), 2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_many_pushes() {
        unsafe {
            let v = hew_vec_new();
            for i in 0..100 {
                hew_vec_push_i32(v, i);
            }
            assert_eq!(hew_vec_len(v), 100);
            assert_eq!(hew_vec_get_i32(v, 0), 0);
            assert_eq!(hew_vec_get_i32(v, 99), 99);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_reverse_i32() {
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 1);
            hew_vec_push_i32(v, 2);
            hew_vec_push_i32(v, 3);
            hew_vec_reverse_i32(v);
            assert_eq!(hew_vec_get_i32(v, 0), 3);
            assert_eq!(hew_vec_get_i32(v, 1), 2);
            assert_eq!(hew_vec_get_i32(v, 2), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_hwvec_to_u8_roundtrip() {
        unsafe {
            let data: &[u8] = &[72, 101, 108, 108, 111]; // "Hello"
            let v = u8_to_hwvec(data);
            let result = hwvec_to_u8(v);
            assert_eq!(result, data);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_hwvec_to_u8_empty() {
        unsafe {
            let v = u8_to_hwvec(&[]);
            let result = hwvec_to_u8(v);
            assert!(result.is_empty());
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_hwvec_to_u8_null() {
        unsafe {
            let result = hwvec_to_u8(core::ptr::null_mut());
            assert!(result.is_empty());
        }
    }
}
