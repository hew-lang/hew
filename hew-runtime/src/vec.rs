//! Hew runtime: `vec` module.
//!
//! Dynamic array (`HewVec`) with C ABI, matching the C runtime layout exactly.
//! Supports bool, i32, i64, f64, and string element types.
//!
//! Type definitions (`ElemKind`, `HewVec`) are re-exported from `hew-cabi`.
//! This module provides the actual implementations of all `hew_vec_*` functions.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]
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
pub use hew_cabi::vec::{ElemKind, HewTypeLayout, HewTypeOwnershipKind, HewVec, HewVecEqThunk};

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
    // SAFETY: msg.as_ptr() is valid for msg.len() bytes, and fd 2 is stderr.
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
        abort_if_layout_aware(v);
        ensure_cap_raw(v, needed);
    }
}

/// Ensure `v` can hold at least `needed` elements, without applying the
/// legacy "no layout-aware ops" guard.
///
/// # Safety
///
/// `v` must point to a valid, non-null `HewVec` allocated with `libc::malloc`.
/// Callers must have already validated any layout descriptor semantics.
unsafe fn ensure_cap_raw(v: *mut HewVec, needed: usize) {
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

/// C-ABI abort for out-of-bounds access, called by inline codegen bounds checks.
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_abort_oob(index: i64, len: i64) -> ! {
    // SAFETY: abort_oob writes to stderr and aborts; always safe to call.
    unsafe { abort_oob(index as usize, len as usize) }
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

/// Abort on pop of empty vec.
///
/// # Safety
///
/// Always aborts — safe to call from any context.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_abort_pop_empty() -> ! {
    // SAFETY: abort_pop_empty writes to stderr and aborts; always safe to call.
    unsafe { abort_pop_empty() }
}

/// Abort when a non-null layout descriptor reaches an operation whose
/// layout-driven ownership semantics are intentionally not implemented yet.
unsafe fn abort_layout_aware_operation() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec layout-aware operation is not implemented\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        libc::abort();
    }
}

/// Fail-closed abort for the witness-managed Vec clone when the element layout
/// is `LayoutManaged`.
///
/// `HewTypeLayout` carries only `{size, align, ownership_kind}` — it has no
/// per-element clone thunk — so a faithful deep clone of layout-managed
/// elements is impossible until the descriptor is extended (the `Vec<owned>`
/// follow-on). This mirrors `hew_hashmap_clone_layout`'s "layout-managed clone
/// thunk is unavailable" abort: fail closed rather than bit-copy owned handles
/// and reintroduce the W4.045 UAF/double-free class
/// (`codegen-abi-authority` / `lifecycle-symmetry` P0).
unsafe fn abort_layout_managed_clone_unavailable() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec layout-managed element clone thunk is unavailable\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        libc::abort();
    }
}

/// Drop-side companion to [`abort_layout_managed_clone_unavailable`]. Mirrors
/// `hew_hashmap_free_layout`'s fail-closed layout-managed path: dropping a
/// layout-managed element requires a descriptor drop thunk that `HewTypeLayout`
/// does not yet carry, so a populated layout-managed Vec fails closed instead
/// of leaking or mis-freeing element-owned heap.
unsafe fn abort_layout_managed_drop_unavailable() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec layout-managed element drop thunk is unavailable\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        libc::abort();
    }
}

/// Abort when a C caller omits the required layout Vec equality thunk.
unsafe fn abort_null_eq_fn() -> ! {
    // SAFETY: writing to stderr and aborting is always safe.
    unsafe {
        let msg = b"PANIC: Vec layout contains equality thunk is null\n\0";
        write_stderr(&msg[..msg.len() - 1]);
        libc::abort();
    }
}

/// Fail closed for Stage 1 layout-aware Vec operations.
///
/// # Safety
///
/// `v` must point to a valid, non-null `HewVec`.
unsafe fn abort_if_layout_aware(v: *const HewVec) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        if !(*v).layout.is_null() {
            abort_layout_aware_operation();
        }
    }
}

/// Return whether a layout-aware vec contains elements whose drops still need
/// descriptor-driven ownership semantics.
///
/// # Safety
///
/// `v` must point to a valid, non-null `HewVec`.
unsafe fn layout_requires_fail_closed_drop(v: *const HewVec) -> bool {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        !(*v).layout.is_null()
            && (*(*v).layout).ownership_kind == HewTypeOwnershipKind::LayoutManaged
    }
}

/// Validate the static descriptor fields required to allocate a layout-aware Vec.
///
/// # Safety
///
/// `layout` must point to a valid `HewTypeLayout`.
unsafe fn validate_type_layout(layout: *const HewTypeLayout) {
    // SAFETY: caller guarantees `layout` is valid.
    unsafe {
        let descriptor = &*layout;
        if descriptor.size == 0 {
            let msg = b"PANIC: HewTypeLayout size must be non-zero\n\0";
            write_stderr(&msg[..msg.len() - 1]);
            libc::abort();
        }
        if descriptor.align == 0 || !descriptor.align.is_power_of_two() {
            let msg = b"PANIC: HewTypeLayout align must be a non-zero power of two\n\0";
            write_stderr(&msg[..msg.len() - 1]);
            libc::abort();
        }
    }
}

/// Validate that a layout-aware operation is restricted to `BitCopy` elements.
///
/// # Safety
///
/// `v` and `layout` must be valid, non-null pointers.
unsafe fn validate_bitcopy_layout_operation(v: *const HewVec, layout: *const HewTypeLayout) {
    // SAFETY: caller guarantees both pointers are valid.
    unsafe {
        validate_type_layout(layout);
        let vec_layout = (*v).layout;
        if vec_layout.is_null() {
            abort_layout_aware_operation();
        }
        validate_type_layout(vec_layout);
        let requested = &*layout;
        let stored = &*vec_layout;
        if requested.size != stored.size
            || requested.align != stored.align
            || requested.ownership_kind != stored.ownership_kind
            || (*v).elem_size != requested.size
            || (*v).elem_kind != ElemKind::Plain
            || requested.ownership_kind != HewTypeOwnershipKind::Plain
        {
            abort_layout_aware_operation();
        }
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
        let v: *mut HewVec = libc::malloc(core::mem::size_of::<HewVec>()).cast(); // ALLOCATOR-PAIRING: libc
        if v.is_null() {
            libc::abort();
        }
        (*v).data = ptr::null_mut();
        (*v).len = 0;
        (*v).cap = 0;
        (*v).elem_size = elem_size as usize;
        (*v).elem_kind = ElemKind::Plain;
        (*v).layout = ptr::null();
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
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<i32>() is 4, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with a valid element size.
    unsafe {
        hew_vec_new_with_elem_size(core::mem::size_of::<i32>() as i64)
    }
}

/// Create a new `HewVec` for `bool` elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_bool() -> *mut HewVec {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<bool>() is 1, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with a valid element size.
    unsafe {
        hew_vec_new_with_elem_size(core::mem::size_of::<bool>() as i64)
    }
}

/// Create a new `HewVec` for string (`*const c_char`) elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_str() -> *mut HewVec {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<*const c_char>() is 4 or 8, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with pointer-sized elements.
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
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<i64>() is 8, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with a valid element size.
    unsafe {
        hew_vec_new_with_elem_size(core::mem::size_of::<i64>() as i64)
    }
}

/// Create a new `HewVec` for `f64` elements.
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_f64() -> *mut HewVec {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<f64>() is 8, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with a valid element size.
    unsafe {
        hew_vec_new_with_elem_size(core::mem::size_of::<f64>() as i64)
    }
}

/// Create a new `HewVec` for pointer-sized elements (e.g. `ActorRef`, handles).
///
/// # Safety
///
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_ptr() -> *mut HewVec {
    #[expect(
        clippy::cast_possible_wrap,
        reason = "size_of::<*mut c_void>() is 4 or 8, fits in i64"
    )]
    // SAFETY: forwarding to `hew_vec_new_with_elem_size` with pointer-sized elements.
    unsafe {
        hew_vec_new_with_elem_size(core::mem::size_of::<*mut c_void>() as i64)
    }
}

/// Create a `HewVec` of i32 elements from raw byte data, widening each `u8` to `i32`.
///
/// # Safety
///
/// `data` must be valid for `len` bytes (or null if `len == 0`).
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_from_u8_data(data: *const u8, len: u32) -> *mut HewVec {
    // SAFETY: Creates an i32-element vec.
    let v = unsafe { hew_vec_new() };
    if len == 0 || data.is_null() {
        return v;
    }
    // Pre-allocate capacity.
    // SAFETY: v is freshly created and valid.
    unsafe { ensure_cap(v, len as usize) };
    // SAFETY: v is valid and has capacity for len i32 elements after ensure_cap.
    let dst = unsafe { (*v).data.cast::<i32>() };
    for i in 0..len as usize {
        // SAFETY: data is valid for len bytes; dst has capacity for len i32s.
        unsafe {
            let byte = *data.add(i);
            dst.add(i).write(i32::from(byte));
        }
    }
    // SAFETY: v is valid.
    unsafe { (*v).len = len as usize };
    v
}

/// Create a new `HewVec` backed by a runtime type layout descriptor.
///
/// Stage 1 only records the descriptor and fails closed for layout-aware
/// operations that need full clone/drop semantics.
///
/// # Safety
///
/// `layout` must be a valid, non-null pointer that outlives the returned vec.
/// The returned pointer must eventually be freed with [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_new_with_layout(layout: *const HewTypeLayout) -> *mut HewVec {
    cabi_guard!(layout.is_null(), ptr::null_mut());
    // SAFETY: null was rejected above.
    unsafe {
        validate_type_layout(layout);
        let descriptor = &*layout;
        let elem_size = i64::try_from(descriptor.size).unwrap_or_else(|_| {
            let msg = b"PANIC: HewTypeLayout size exceeds Hew ABI range\n\0";
            write_stderr(&msg[..msg.len() - 1]);
            libc::abort();
        });
        let v = hew_vec_new_with_elem_size(elem_size);
        (*v).elem_kind = match descriptor.ownership_kind {
            HewTypeOwnershipKind::String => ElemKind::String,
            HewTypeOwnershipKind::Plain | HewTypeOwnershipKind::LayoutManaged => ElemKind::Plain,
        };
        (*v).layout = layout;
        v
    }
}

// ---------------------------------------------------------------------------
// Push
// ---------------------------------------------------------------------------

macro_rules! vec_push_primitive {
    ($name:ident, $ty:ty) => {
        /// Push a primitive value onto the vec.
        ///
        /// # Safety
        ///
        /// `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *mut HewVec, val: $ty) {
            // SAFETY: caller guarantees `v` is valid.
            unsafe {
                let len = (*v).len;
                let Some(new_len) = len.checked_add(1) else {
                    libc::abort();
                };
                ensure_cap(v, new_len);
                let slot = (*v).data.cast::<$ty>().add(len);
                slot.write(val);
                (*v).len = new_len;
            }
        }
    };
}

vec_push_primitive!(hew_vec_push_i32, i32);
vec_push_primitive!(hew_vec_push_bool, bool);
vec_push_primitive!(hew_vec_push_i64, i64);

/// Push a string onto the vec. The string is duplicated with `strdup`.
///
/// # Safety
///
/// `v` must be a valid string `HewVec` pointer. `val` must be a valid C string.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_str(v: *mut HewVec, val: *const c_char) {
    // SAFETY: caller guarantees `v` and `val` are valid.
    unsafe {
        let len = (*v).len;
        let Some(new_len) = len.checked_add(1) else {
            libc::abort();
        };
        ensure_cap(v, new_len);
        let duped = libc::strdup(val);
        if duped.is_null() {
            libc::abort();
        }
        let slot = (*v).data.cast::<*mut c_char>().add(len);
        slot.write(duped);
        (*v).len = new_len;
    }
}

vec_push_primitive!(hew_vec_push_f64, f64);
vec_push_primitive!(hew_vec_push_ptr, *mut c_void);

// ---------------------------------------------------------------------------
// Get
// ---------------------------------------------------------------------------

macro_rules! vec_get_primitive {
    ($name:ident, $ty:ty) => {
        /// Get a primitive value at `index`. Aborts if out of bounds.
        ///
        /// # Safety
        ///
        /// `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *mut HewVec, index: i64) -> $ty {
            // SAFETY: caller guarantees `v` is valid.
            unsafe {
                let index = index as usize;
                if index >= (*v).len {
                    abort_oob(index, (*v).len);
                }
                (*v).data.cast::<$ty>().add(index).read()
            }
        }
    };
}

vec_get_primitive!(hew_vec_get_i32, i32);
vec_get_primitive!(hew_vec_get_bool, bool);
vec_get_primitive!(hew_vec_get_i64, i64);

/// Get a string pointer at `index`. Aborts if out of bounds.
///
/// **Note:** Returns a `strdup`'d copy. The caller must `free()` the returned string.
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
        let raw = (*v).data.cast::<*const c_char>().add(index).read();
        if raw.is_null() {
            core::ptr::null()
        } else {
            let duped = libc::strdup(raw);
            if duped.is_null() {
                libc::abort();
            }
            duped
        }
    }
}

vec_get_primitive!(hew_vec_get_f64, f64);
vec_get_primitive!(hew_vec_get_ptr, *mut c_void);

// ---------------------------------------------------------------------------
// Range-slice (C-3)
//
// `hew_vec_slice_range_T(v, start, end)` allocates a fresh `HewVec<T>` and
// populates it with elements from `v` in the half-open range `[start, end)`.
//
// Bounds discipline: the MIR emitter performs `start <= end` and
// `end <= len(v)` checks BEFORE calling these functions. The runtime
// repeats the same checks as defence-in-depth (matching `hew_vec_get_T`),
// so a stray caller that forgets the front-end check still fails closed
// rather than reading past the end.
//
// String element ownership: `hew_vec_slice_range_str` strdups each element
// into the fresh vec and sets `elem_kind == String`, so the existing
// free-on-drop path in `hew_vec_free` frees the copies. Other element
// kinds use a single bulk byte-copy. Both shapes follow `hew_vec_clone`.
// ---------------------------------------------------------------------------

/// Helper: validate `start` and `end` are within `[0, len]` and `start <=
/// end`. Aborts with the OOB message if either check fails, matching the
/// `abort_oob` used by the `_get_*` family. The MIR emitter has already
/// checked the same invariants; this runtime check is defence-in-depth.
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer.
unsafe fn check_slice_bounds(v: *mut HewVec, start: i64, end: i64) -> (usize, usize) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let len = (*v).len;
        // Negative endpoints would wrap to a very large usize, which the
        // `> len` check below catches. Match the `_get_*` family which
        // does the same `index as usize` cast and relies on the
        // `>= len` check to catch negatives.
        let start_u = start as usize;
        let end_u = end as usize;
        if start > end || end_u > len || start_u > len {
            abort_oob(start_u, len);
        }
        (start_u, end_u)
    }
}

/// Allocate a new `HewVec` populated from `v[start..end)` for i32 elements.
///
/// # Safety
///
/// `v` must be a valid i32 `HewVec` pointer. The returned pointer must be
/// freed via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_slice_range_i32(
    v: *mut HewVec,
    start: i64,
    end: i64,
) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let (start_u, end_u) = check_slice_bounds(v, start, end);
        let count = end_u - start_u;
        let out = hew_vec_new();
        if count == 0 {
            return out;
        }
        ensure_cap(out, count);
        let src = (*v).data.cast::<i32>().add(start_u);
        let dst = (*out).data.cast::<i32>();
        core::ptr::copy_nonoverlapping(src, dst, count);
        (*out).len = count;
        out
    }
}

/// Allocate a new `HewVec` populated from `v[start..end)` for i64 elements.
///
/// # Safety
///
/// `v` must be a valid i64 `HewVec` pointer. The returned pointer must be
/// freed via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_slice_range_i64(
    v: *mut HewVec,
    start: i64,
    end: i64,
) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let (start_u, end_u) = check_slice_bounds(v, start, end);
        let count = end_u - start_u;
        let out = hew_vec_new_i64();
        if count == 0 {
            return out;
        }
        ensure_cap(out, count);
        let src = (*v).data.cast::<i64>().add(start_u);
        let dst = (*out).data.cast::<i64>();
        core::ptr::copy_nonoverlapping(src, dst, count);
        (*out).len = count;
        out
    }
}

/// Allocate a new `HewVec` populated from `v[start..end)` for f64 elements.
///
/// # Safety
///
/// `v` must be a valid f64 `HewVec` pointer. The returned pointer must be
/// freed via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_slice_range_f64(
    v: *mut HewVec,
    start: i64,
    end: i64,
) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let (start_u, end_u) = check_slice_bounds(v, start, end);
        let count = end_u - start_u;
        let out = hew_vec_new_f64();
        if count == 0 {
            return out;
        }
        ensure_cap(out, count);
        let src = (*v).data.cast::<f64>().add(start_u);
        let dst = (*out).data.cast::<f64>();
        core::ptr::copy_nonoverlapping(src, dst, count);
        (*out).len = count;
        out
    }
}

/// Allocate a new `HewVec` populated from `v[start..end)` for pointer-sized
/// elements (handles, named heap types). The element pointers are byte-
/// copied verbatim; the result vec does not duplicate or refcount them.
///
/// # Safety
///
/// `v` must be a valid pointer `HewVec`. The returned pointer must be freed
/// via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_slice_range_ptr(
    v: *mut HewVec,
    start: i64,
    end: i64,
) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let (start_u, end_u) = check_slice_bounds(v, start, end);
        let count = end_u - start_u;
        let out = hew_vec_new_ptr();
        if count == 0 {
            return out;
        }
        ensure_cap(out, count);
        let src = (*v).data.cast::<*mut c_void>().add(start_u);
        let dst = (*out).data.cast::<*mut c_void>();
        core::ptr::copy_nonoverlapping(src, dst, count);
        (*out).len = count;
        out
    }
}

/// Allocate a new `HewVec` populated from `v[start..end)` for string
/// elements. Each element is `strdup`'d into the fresh vec; the result vec
/// owns the duplicates and frees them via the standard
/// `elem_kind == String` path in [`hew_vec_free`].
///
/// # Safety
///
/// `v` must be a valid string `HewVec` (`elem_kind == String`). The
/// returned pointer must be freed via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_slice_range_str(
    v: *mut HewVec,
    start: i64,
    end: i64,
) -> *mut HewVec {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let (start_u, end_u) = check_slice_bounds(v, start, end);
        let count = end_u - start_u;
        let out = hew_vec_new_str();
        if count == 0 {
            return out;
        }
        ensure_cap(out, count);
        for i in 0..count {
            let src_ptr = (*v).data.cast::<*const c_char>().add(start_u + i).read();
            let duped = if src_ptr.is_null() {
                ptr::null_mut()
            } else {
                let result = libc::strdup(src_ptr);
                if result.is_null() {
                    libc::abort();
                }
                result
            };
            (*out).data.cast::<*mut c_char>().add(i).write(duped);
        }
        (*out).len = count;
        out
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

/// Set a `bool` at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid bool `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_bool(v: *mut HewVec, index: i64, val: bool) {
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<bool>().add(index).write(val);
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
            libc::free(old.cast()); // ALLOCATOR-PAIRING: libc
        }
        let duped = libc::strdup(val);
        if duped.is_null() {
            libc::abort();
        }
        slot.write(duped);
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

macro_rules! vec_pop_primitive {
    ($name:ident, $ty:ty) => {
        /// Pop the last primitive value. Aborts if empty.
        ///
        /// # Safety
        ///
        /// `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *mut HewVec) -> $ty {
            // SAFETY: caller guarantees `v` is valid.
            unsafe {
                if (*v).len == 0 {
                    abort_pop_empty();
                }
                (*v).len -= 1;
                (*v).data.cast::<$ty>().add((*v).len).read()
            }
        }
    };
    ($name:ident, $ty:ty, $null_ret:expr) => {
        /// Pop the last primitive value. Returns the supplied null value for a null vec.
        ///
        /// # Safety
        ///
        /// Non-null `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *mut HewVec) -> $ty {
            cabi_guard!(v.is_null(), $null_ret);
            // SAFETY: caller guarantees `v` is valid.
            unsafe {
                if (*v).len == 0 {
                    abort_pop_empty();
                }
                (*v).len -= 1;
                (*v).data.cast::<$ty>().add((*v).len).read()
            }
        }
    };
}

vec_pop_primitive!(hew_vec_pop_i32, i32);
vec_pop_primitive!(hew_vec_pop_bool, bool);
vec_pop_primitive!(hew_vec_pop_i64, i64);

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

vec_pop_primitive!(hew_vec_pop_f64, f64);
vec_pop_primitive!(hew_vec_pop_ptr, *mut c_void, ptr::null_mut());

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
    #[expect(
        clippy::cast_possible_wrap,
        reason = "vec length won't exceed i64::MAX"
    )]
    // SAFETY: caller guarantees `v` is a valid HewVec pointer.
    unsafe {
        (*v).len as i64
    }
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
                libc::free(ptr.cast()); // ALLOCATOR-PAIRING: libc
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
        if layout_requires_fail_closed_drop(v) && (*v).len != 0 {
            abort_layout_aware_operation();
        }
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
                if layout_requires_fail_closed_drop(v) && (*v).len != 0 {
                    abort_layout_aware_operation();
                }
                if (*v).elem_kind == ElemKind::String {
                    free_string_elements(v);
                }
                libc::free((*v).data.cast()); // ALLOCATOR-PAIRING: libc
            }
            libc::free(v.cast()); // ALLOCATOR-PAIRING: libc
        }
    }
}

/// Free a witness-managed `Vec<T>` handle, applying the element ownership
/// discipline recorded in the handle's layout descriptor.
///
/// This is the drop half of the single layout-witness pair that codegen
/// derives from `collection_layout_witness` (W5.002 F0b). Codegen routes every
/// actor-state `Vec<T>` field through `hew_vec_clone_managed` /
/// `hew_vec_free_managed`, so the allocate side and free side can never select
/// mismatched runtime symbols — the structural retirement of the W4.045 UAF
/// class for Vec (`codegen-abi-authority` / `lifecycle-symmetry` P0). The
/// element layout lives *inside the handle* (`(*v).layout`, stamped by the
/// constructor), exactly as `hew_hashmap_free_layout` reads its value layout
/// from the map, so this symbol is single-arg and never takes a separate
/// descriptor at the call site.
///
/// Ownership dispatch:
/// - layout absent (legacy typed constructor) → `elem_kind`-driven free:
///   `String` frees each slot, every other kind is covered by the bulk buffer.
/// - layout `Plain` → bulk buffer free.
/// - layout `String` → per-slot string free.
/// - layout `LayoutManaged` with live elements → fail closed
///   ([`abort_layout_managed_drop_unavailable`]): `HewTypeLayout` carries no
///   drop thunk, mirroring `hew_hashmap_free_layout`'s fail-closed key path.
///
/// A null `v` is a documented no-op (`boundary-fail-closed` P0: `free(null)`
/// is the only permitted silent-return shape).
///
/// # Safety
///
/// `v` must have been returned by [`hew_vec_clone_managed`] or a `hew_vec_new*`
/// constructor (or be null). After this call, `v` is invalid and must not be
/// used.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_free_managed(v: *mut HewVec) {
    // SAFETY: caller guarantees `v` was allocated with malloc (or is null).
    unsafe {
        if v.is_null() {
            return;
        }
        if layout_requires_fail_closed_drop(v) && (*v).len != 0 {
            abort_layout_managed_drop_unavailable();
        }
        if !(*v).data.is_null() {
            if (*v).elem_kind == ElemKind::String {
                free_string_elements(v);
            }
            libc::free((*v).data.cast()); // ALLOCATOR-PAIRING: libc
        }
        libc::free(v.cast()); // ALLOCATOR-PAIRING: libc
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
    cabi_guard!(v.is_null());
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
    cabi_guard!(v.is_null());
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
    cabi_guard!(v.is_null());
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
    cabi_guard!(v.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let src = &*v;
        abort_if_layout_aware(v);
        let new_v = hew_vec_new_with_elem_size(
            #[expect(clippy::cast_possible_wrap, reason = "elem_size is small, fits in i64")]
            {
                src.elem_size as i64
            },
        );
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
                    let result = libc::strdup(src_ptr);
                    if result.is_null() {
                        libc::abort();
                    }
                    result
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

/// Clone a layout-backed `BitCopy` (Plain ownership) vec by bulk-copying all
/// element bytes into a freshly allocated vec with the same layout descriptor.
///
/// The source vec must have `Plain` ownership; `LayoutManaged` vecs abort
/// fail-closed because their elements require lifecycle callbacks (drop/clone)
/// that are not yet implemented. This mirrors the pattern used by `push`,
/// `get`, `set`, `pop`, and `remove_at` layout operations.
///
/// # Safety
///
/// - `v` must be a valid `HewVec` pointer (or null, which returns null).
/// - `layout` must be a valid `HewTypeLayout` pointer whose `size`, `align`,
///   and `ownership_kind` match the layout stored in `v`.
/// - The returned pointer must eventually be freed via [`hew_vec_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_clone_layout(
    v: *const HewVec,
    layout: *const HewTypeLayout,
) -> *mut HewVec {
    cabi_guard!(v.is_null() || layout.is_null(), ptr::null_mut());
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        let src = &*v;
        let new_v = hew_vec_new_with_layout(layout);
        if new_v.is_null() {
            libc::abort();
        }
        if src.len == 0 {
            return new_v;
        }
        ensure_cap_raw(new_v, src.len);
        let elem_size = (*layout).size;
        let byte_count = src.len * elem_size;
        core::ptr::copy_nonoverlapping(src.data, (*new_v).data, byte_count);
        (*new_v).len = src.len;
        new_v
    }
}

/// Deep-clone a witness-managed `Vec<T>` handle, applying the element ownership
/// discipline recorded in the handle's layout descriptor.
///
/// Clone half of the single layout-witness pair (see [`hew_vec_free_managed`]).
/// Unlike the legacy [`hew_vec_clone`] — which calls `abort_if_layout_aware`
/// and therefore *aborts* on any layout-backed Vec (e.g. a `Vec<Point>`
/// actor-state field) — this entry point clones layout-backed `Plain` and
/// `String` Vecs and only fails closed for `LayoutManaged` elements, whose
/// per-element clone thunk `HewTypeLayout` does not yet carry. The element
/// layout is read from the handle (`(*v).layout`) exactly as
/// `hew_hashmap_clone_layout` reads its embedded value layout, so the symbol
/// is single-arg and pairs with the constructor that stamped the descriptor.
///
/// The clone preserves the source descriptor so it frees with the *same*
/// discipline through [`hew_vec_free_managed`] — clone/free symmetry, the
/// W4.045 UAF guard.
///
/// Ownership dispatch:
/// - layout absent → `elem_kind`-driven clone (`String` `strdup`s each slot,
///   every other kind bulk-copies).
/// - layout `Plain` → fresh layout-backed vec + bulk byte copy.
/// - layout `String` → fresh layout-backed vec + per-slot `strdup`.
/// - layout `LayoutManaged` with live elements → fail closed
///   ([`abort_layout_managed_clone_unavailable`]).
///
/// A null `v` returns null (`boundary-fail-closed` P0).
///
/// # Safety
///
/// `v` must be a valid `HewVec` pointer (or null). The returned pointer must
/// eventually be freed with [`hew_vec_free_managed`].
#[no_mangle]
pub unsafe extern "C" fn hew_vec_clone_managed(v: *const HewVec) -> *mut HewVec {
    cabi_guard!(v.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let src = &*v;
        // Fail closed before allocating anything if the element layout demands
        // descriptor-thunk-driven clone semantics that HewTypeLayout cannot yet
        // express (mirrors hew_hashmap_clone_layout's unavailable-thunk path).
        if layout_requires_fail_closed_drop(v) && src.len != 0 {
            abort_layout_managed_clone_unavailable();
        }
        let new_v = if src.layout.is_null() {
            hew_vec_new_with_elem_size(
                #[expect(clippy::cast_possible_wrap, reason = "elem_size is small, fits in i64")]
                {
                    src.elem_size as i64
                },
            )
        } else {
            // Preserve the layout descriptor so the clone frees with the same
            // discipline (clone/free symmetry — W4.045 UAF class).
            hew_vec_new_with_layout(src.layout)
        };
        if new_v.is_null() {
            libc::abort();
        }
        (*new_v).elem_kind = src.elem_kind;
        if src.len == 0 {
            return new_v;
        }
        // ensure_cap (not ensure_cap_raw) would re-apply the legacy
        // "no layout-aware ops" guard and abort on a layout-backed source.
        ensure_cap_raw(new_v, src.len);
        if src.elem_kind == ElemKind::String {
            for i in 0..src.len {
                let src_ptr = src.data.cast::<*const c_char>().add(i).read();
                let duped = if src_ptr.is_null() {
                    ptr::null_mut()
                } else {
                    let result = libc::strdup(src_ptr);
                    if result.is_null() {
                        libc::abort();
                    }
                    result
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
    cabi_guard!(dst.is_null() || src.is_null());
    // SAFETY: caller guarantees both pointers are valid HewVecs with matching elem_size.
    unsafe {
        abort_if_layout_aware(dst);
        abort_if_layout_aware(src);
        let src_len = (*src).len;
        if src_len == 0 {
            return;
        }
        if (*dst).elem_size != (*src).elem_size || (*dst).elem_kind != (*src).elem_kind {
            libc::abort();
        }
        let Some(new_len) = (*dst).len.checked_add(src_len) else {
            libc::abort();
        };
        ensure_cap(dst, new_len);
        let elem_size = (*dst).elem_size;
        let dst_ptr = (*dst).data.add((*dst).len * elem_size);
        if (*dst).elem_kind == ElemKind::String {
            for i in 0..src_len {
                let src_str = (*src).data.cast::<*const c_char>().add(i).read();
                let duped = if src_str.is_null() {
                    ptr::null_mut()
                } else {
                    let result = libc::strdup(src_str);
                    if result.is_null() {
                        libc::abort();
                    }
                    result
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

macro_rules! vec_contains_primitive {
    ($(#[$meta:meta])* $name:ident, $ty:ty) => {
        $(#[$meta])*
        /// Check whether the vec contains a primitive value. Returns 1 if found, 0 otherwise.
        ///
        /// # Safety
        ///
        /// Non-null `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *const HewVec, val: $ty) -> i32 {
            cabi_guard!(v.is_null(), 0);
            // SAFETY: caller guarantees `v` is a valid HewVec for `$ty`.
            unsafe {
                let len = (*v).len;
                let data = (*v).data.cast::<$ty>();
                for i in 0..len {
                    if data.add(i).read() == val {
                        return 1;
                    }
                }
                0
            }
        }
    };
}

macro_rules! vec_remove_primitive {
    ($(#[$meta:meta])* $name:ident, $ty:ty) => {
        $(#[$meta])*
        /// Remove the first occurrence of a primitive value from the vec.
        ///
        /// # Safety
        ///
        /// Non-null `v` must be a valid `HewVec` pointer whose element storage matches this symbol's type.
        #[no_mangle]
        pub unsafe extern "C" fn $name(v: *mut HewVec, val: $ty) {
            cabi_guard!(v.is_null());
            // SAFETY: caller guarantees `v` is a valid HewVec for `$ty`.
            unsafe {
                let len = (*v).len;
                let data = (*v).data.cast::<$ty>();
                for i in 0..len {
                    if data.add(i).read() == val {
                        core::ptr::copy(data.add(i + 1), data.add(i), len - i - 1);
                        (*v).len -= 1;
                        return;
                    }
                }
            }
        }
    };
}

vec_contains_primitive!(hew_vec_contains_i32, i32);
vec_contains_primitive!(hew_vec_contains_i64, i64);
vec_contains_primitive!(
    #[expect(
        clippy::float_cmp,
        reason = "C ABI semantics: exact f64 equality match is intentional"
    )]
    hew_vec_contains_f64,
    f64
);

vec_remove_primitive!(hew_vec_remove_i32, i32);
vec_remove_primitive!(hew_vec_remove_i64, i64);
vec_remove_primitive!(
    #[expect(
        clippy::float_cmp,
        reason = "exact equality is intentional for element removal"
    )]
    hew_vec_remove_f64,
    f64
);
vec_remove_primitive!(hew_vec_remove_ptr, *mut c_void);

/// Remove the element at `index`, shifting subsequent elements left.
///
/// Aborts if `index >= len`.
///
/// # Panics
///
/// Panics if `index` is out of bounds (`index >= len`).
///
/// # Safety
///
/// `v` must be a valid pointer to a `HewVec`. The element size is taken
/// from the vec's `elem_size` field.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_remove_at(v: *mut HewVec, index: i64) {
    cabi_guard!(v.is_null());
    let idx = index as usize;
    // SAFETY: Caller guarantees `v` is a valid HewVec pointer. We bounds-check
    // `idx` before computing offsets and copying.
    unsafe {
        abort_if_layout_aware(v);
        let len = (*v).len;
        assert!(
            idx < len,
            "hew_vec_remove_at: index {idx} out of bounds (len {len})"
        );
        let elem_size = (*v).elem_size;
        let src = (*v).data.add((idx + 1) * elem_size);
        let dst = (*v).data.add(idx * elem_size);
        let count = (len - idx - 1) * elem_size;
        if count > 0 {
            core::ptr::copy(src, dst, count);
        }
        (*v).len -= 1;
    }
}

/// Remove the `BitCopy` layout-descriptor element at `index`, shifting
/// subsequent elements left by one position.
///
/// This is an index-based remove for layout-backed `Vec<T>` where `T` is a
/// Copy record or tuple type with `HewTypeOwnershipKind::Plain`. It does NOT
/// implement remove-by-value/equality.
///
/// Aborts if `index >= len`. Aborts if `elem_size` is zero (would produce
/// zero-stride pointer arithmetic). Validates the layout descriptor against
/// the stored vec layout before any mutation.
///
/// # Safety
///
/// - `v` must be a valid layout-aware `HewVec` pointer created with a
///   matching plain ownership layout.
/// - `layout` must point to a `HewTypeLayout` whose `size`, `align`, and
///   `ownership_kind` match those stored in the vec.
///
/// # WASM-TODO
///
/// `HewTypeLayout.size` is host `usize`-width. On wasm32 `usize = u32`, so
/// the `elem_size` arithmetic and the descriptor struct shape below must
/// become target-aware before WASM parity can be claimed for this symbol.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_remove_at_layout(
    v: *mut HewVec,
    index: i64,
    layout: *const HewTypeLayout,
) {
    cabi_guard!(v.is_null() || layout.is_null());
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        let len = (*v).len;
        let idx = index as usize;
        if idx >= len {
            abort_oob(idx, len);
        }
        let elem_size = (*layout).size;
        // Guard against zero-stride pointer arithmetic (layout validation
        // already enforces size > 0 via `validate_type_layout`, but an
        // explicit check here avoids UB in the arithmetic below if that
        // invariant were ever violated).
        if elem_size == 0 {
            abort_layout_aware_operation();
        }
        let src = (*v).data.add((idx + 1) * elem_size);
        let dst = (*v).data.add(idx * elem_size);
        let count = (len - idx - 1) * elem_size;
        if count > 0 {
            // memmove semantics: src and dst may overlap (src = dst + elem_size).
            core::ptr::copy(src, dst, count);
        }
        (*v).len -= 1;
    }
}

/// Set a pointer at `index`. Aborts if out of bounds.
///
/// # Safety
///
/// `v` must be a valid pointer `HewVec` pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_ptr(v: *mut HewVec, index: i64, val: *mut c_void) {
    cabi_guard!(v.is_null());
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.cast::<*mut c_void>().add(index).write(val);
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
    cabi_guard!(v.is_null() || val.is_null(), 0);
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
    cabi_guard!(v.is_null());
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
    cabi_guard!(v.is_null());
    // SAFETY: caller guarantees `v` is valid.
    unsafe {
        let new_len = new_len as usize;
        let vec = &mut *v;
        if new_len >= vec.len {
            return;
        }
        if layout_requires_fail_closed_drop(v) {
            abort_layout_aware_operation();
        }
        if vec.elem_kind == ElemKind::String {
            for i in new_len..vec.len {
                let slot = vec.data.cast::<*mut c_char>().add(i);
                let ptr = slot.read();
                if !ptr.is_null() {
                    libc::free(ptr.cast()); // ALLOCATOR-PAIRING: libc
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
        abort_if_layout_aware(v);
        let len = (*v).len;
        let Some(new_len) = len.checked_add(1) else {
            libc::abort();
        };
        ensure_cap(v, new_len);
        let elem_size = (*v).elem_size;
        let dst = (*v).data.add(len * elem_size);
        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);
        (*v).len = new_len;
    }
}

/// Push a `BitCopy` layout-descriptor element by copying `layout.size` bytes.
///
/// # Safety
///
/// `v` must be a valid layout-aware `HewVec` pointer created with a matching
/// plain ownership layout. `data` must point to at least `layout.size`
/// readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_push_layout(
    v: *mut HewVec,
    data: *const core::ffi::c_void,
    layout: *const HewTypeLayout,
) {
    cabi_guard!(v.is_null() || data.is_null() || layout.is_null());
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        let len = (*v).len;
        let Some(new_len) = len.checked_add(1) else {
            libc::abort();
        };
        ensure_cap_raw(v, new_len);
        let elem_size = (*layout).size;
        let dst = (*v).data.add(len * elem_size);
        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);
        (*v).len = new_len;
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
        abort_if_layout_aware(v);
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.add(index * (*v).elem_size).cast()
    }
}

/// Return a pointer to a `BitCopy` layout-descriptor element.
///
/// # Safety
///
/// `v` must be a valid layout-aware `HewVec` pointer created with a matching
/// plain ownership layout. The returned pointer is valid only while the vec is
/// not reallocated.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_get_layout(
    v: *const HewVec,
    index: i64,
    layout: *const HewTypeLayout,
) -> *const core::ffi::c_void {
    cabi_guard!(v.is_null() || layout.is_null(), ptr::null());
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        (*v).data.add(index * (*layout).size).cast()
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
        abort_if_layout_aware(v);
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        let elem_size = (*v).elem_size;
        let dst = (*v).data.add(index * elem_size);
        core::ptr::copy_nonoverlapping(data.cast::<u8>(), dst, elem_size);
    }
}

/// Overwrite a `BitCopy` layout-descriptor element by copying `layout.size` bytes.
///
/// # Safety
///
/// `v` must be a valid layout-aware `HewVec` pointer created with a matching
/// plain ownership layout. `data` must point to at least `layout.size`
/// readable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_set_layout(
    v: *mut HewVec,
    index: i64,
    data: *const core::ffi::c_void,
    layout: *const HewTypeLayout,
) {
    cabi_guard!(v.is_null() || data.is_null() || layout.is_null());
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        let index = index as usize;
        if index >= (*v).len {
            abort_oob(index, (*v).len);
        }
        let elem_size = (*layout).size;
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
        abort_if_layout_aware(v);
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

/// Pop the last `BitCopy` layout-descriptor element, copying it into `out`.
/// Returns 1 on success, 0 if the vec is empty.
///
/// # Safety
///
/// `v` must be a valid layout-aware `HewVec` pointer created with a matching
/// plain ownership layout. `out` must point to at least `layout.size`
/// writable bytes.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_pop_layout(
    v: *mut HewVec,
    out: *mut core::ffi::c_void,
    layout: *const HewTypeLayout,
) -> i32 {
    cabi_guard!(v.is_null() || out.is_null() || layout.is_null(), 0);
    // SAFETY: guards reject null pointers; helper validates BitCopy layout semantics.
    unsafe {
        validate_bitcopy_layout_operation(v, layout);
        if (*v).len == 0 {
            return 0;
        }
        (*v).len -= 1;
        let elem_size = (*layout).size;
        let src = (*v).data.add((*v).len * elem_size);
        core::ptr::copy_nonoverlapping(src, out.cast::<u8>(), elem_size);
        1
    }
}

/// Stage 1 fail-closed stub for layout-descriptor equality/contains.
///
/// # Safety
///
/// This C-ABI entry point always aborts until layout-aware Vec comparison
/// semantics are implemented.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_contains_layout(
    _v: *const HewVec,
    _data: *const core::ffi::c_void,
    _layout: *const HewTypeLayout,
) -> i32 {
    // SAFETY: the staged layout-aware operation must fail closed.
    unsafe { abort_layout_aware_operation() }
}

/// Compare elements of a layout-backed Vec using a caller-provided equality
/// function. Returns 1 if any element is equal to `val`, 0 otherwise.
///
/// The equality thunk is generated for the concrete element type and returns
/// non-zero when equal. This runtime entry point deliberately never compares
/// raw bytes, because padding bytes in aggregate values are not semantic.
///
/// # Safety
///
/// - `v` may be null; null returns 0.
/// - `val` may be null; null returns 0. Otherwise it must point to
///   `(*v).elem_size` readable bytes.
/// - `eq_fn` must be a valid non-null function pointer matching
///   [`HewVecEqThunk`]. A null callback aborts with a named diagnostic.
/// - `v` must be a layout-backed `HewVec` whose stored layout is `Plain`, whose
///   `elem_kind` is [`ElemKind::Plain`], and whose `elem_size` matches the
///   stored layout size. Layout-managed or mismatched vectors abort fail-closed.
/// - The backing buffer must not be modified concurrently.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_contains_thunk(
    v: *const HewVec,
    val: *const core::ffi::c_void,
    eq_fn: Option<HewVecEqThunk>,
) -> i32 {
    cabi_guard!(v.is_null() || val.is_null(), 0);
    let Some(eq_fn) = eq_fn else {
        // SAFETY: abort path is safe at the C ABI boundary.
        unsafe { abort_null_eq_fn() }
    };
    // SAFETY: null pointers were rejected above; helper validates the stored
    // layout descriptor and BitCopy invariants before any element is inspected.
    unsafe {
        let layout = (*v).layout;
        if layout.is_null() {
            abort_layout_aware_operation();
        }
        validate_bitcopy_layout_operation(v, layout);
        let len = (*v).len;
        if len > (*v).cap || (len > 0 && (*v).data.is_null()) {
            abort_layout_aware_operation();
        }
        let elem_size = (*v).elem_size;
        let data = (*v).data;
        for i in 0..len {
            let elem_ptr = data.add(i * elem_size).cast::<core::ffi::c_void>();
            if eq_fn(elem_ptr, val) != 0 {
                return 1;
            }
        }
        0
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
    cabi_guard!(v.is_null());
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
    cabi_guard!(v.is_null(), Vec::new());
    // SAFETY: caller guarantees v is a valid HewVec.
    let len = unsafe { hew_vec_len(v) };
    (0..len)
        .map(|i| {
            // SAFETY: i < len.
            #[expect(clippy::cast_sign_loss, reason = "byte values stored as i32 are 0-255")]
            // SAFETY: i < len, so this read is in-bounds.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
        unsafe {
            let v = hew_vec_new();
            assert!(!v.is_null());
            assert!((*v).layout.is_null());
            assert_eq!(hew_vec_len(v), 0);
            assert!(hew_vec_is_empty(v));
            hew_vec_free(v);
        }
    }

    #[test]
    fn legacy_vec_constructors_keep_layout_null() {
        // SAFETY: FFI calls use valid vec pointers returned by constructors.
        unsafe {
            let vecs = [
                hew_vec_new(),
                hew_vec_new_bool(),
                hew_vec_new_i64(),
                hew_vec_new_f64(),
                hew_vec_new_str(),
                hew_vec_new_ptr(),
                hew_vec_new_generic(i64::try_from(core::mem::size_of::<u64>()).unwrap(), 0),
            ];
            for v in vecs {
                assert!(!v.is_null());
                assert!((*v).layout.is_null());
                hew_vec_free(v);
            }
        }
    }

    #[test]
    fn test_vec_bool_stores_one_byte_values() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_bool.
        unsafe {
            let v = hew_vec_new_bool();
            hew_vec_push_bool(v, true);
            hew_vec_push_bool(v, false);
            hew_vec_push_bool(v, true);

            assert_eq!((*v).elem_size, 1);
            assert_eq!(hew_vec_len(v), 3);
            assert!(hew_vec_get_bool(v, 0));
            assert!(!hew_vec_get_bool(v, 1));
            assert!(hew_vec_get_bool(v, 2));

            let bytes = core::slice::from_raw_parts((*v).data.cast::<u8>(), (*v).len);
            assert_eq!(bytes, &[1, 0, 1]);

            hew_vec_set_bool(v, 1, true);
            let bytes = core::slice::from_raw_parts((*v).data.cast::<u8>(), (*v).len);
            assert_eq!(bytes, &[1, 1, 1]);
            assert!(hew_vec_pop_bool(v));
            assert_eq!(hew_vec_len(v), 2);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_i32() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_i64.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_f64.
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
        // SAFETY: FFI calls use valid vec pointer and valid C strings.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_i64.
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
    fn test_vec_pop_str() {
        // SAFETY: FFI calls use valid vec pointer and valid C strings.
        unsafe {
            let v = hew_vec_new_str();
            let s1 = CString::new("hello").unwrap();
            let s2 = CString::new("world").unwrap();
            hew_vec_push_str(v, s1.as_ptr());
            hew_vec_push_str(v, s2.as_ptr());
            let popped = hew_vec_pop_str(v);
            assert!(!popped.is_null());
            assert_eq!(std::ffi::CStr::from_ptr(popped).to_string_lossy(), "world");
            assert_eq!(hew_vec_len(v), 1);
            libc::free(popped.cast_mut().cast()); // ALLOCATOR-PAIRING: libc
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_pop_f64() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_f64.
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, 1.5);
            hew_vec_push_f64(v, 2.5);
            let popped = hew_vec_pop_f64(v);
            assert!((popped - 2.5).abs() < f64::EPSILON);
            assert_eq!(hew_vec_len(v), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_pop_ptr() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_ptr.
        unsafe {
            let v = hew_vec_new_ptr();
            let ptr1 = 0x1234usize as *mut core::ffi::c_void;
            let ptr2 = 0x5678usize as *mut core::ffi::c_void;
            hew_vec_push_ptr(v, ptr1);
            hew_vec_push_ptr(v, ptr2);
            let popped = hew_vec_pop_ptr(v);
            assert_eq!(popped, ptr2);
            assert_eq!(hew_vec_len(v), 1);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_push_get_generic() {
        #[repr(C)]
        struct Payload {
            a: u64,
            b: u64,
        }

        // SAFETY: FFI calls use valid vec pointer and stack-allocated payloads.
        unsafe {
            let v = hew_vec_new_generic(i64::try_from(core::mem::size_of::<Payload>()).unwrap(), 0);
            let payload = Payload {
                a: 0x0123_4567_89ab_cdef,
                b: 0xfedc_ba98_7654_3210,
            };
            hew_vec_push_generic(v, (&raw const payload).cast());

            let raw = hew_vec_get_generic(v, 0);
            assert!(!raw.is_null());

            let expected = core::slice::from_raw_parts(
                (&raw const payload).cast::<u8>(),
                core::mem::size_of::<Payload>(),
            );
            let mut out = [0u8; core::mem::size_of::<Payload>()];
            core::ptr::copy_nonoverlapping(raw.cast::<u8>(), out.as_mut_ptr(), out.len());
            assert_eq!(out.as_slice(), expected);
            hew_vec_free(v);
        }
    }

    #[test]
    fn test_vec_new_with_layout_records_descriptor() {
        #[repr(C)]
        struct Payload {
            a: u64,
            b: u64,
        }

        let layout = HewTypeLayout {
            size: core::mem::size_of::<Payload>(),
            align: core::mem::align_of::<Payload>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };

        // SAFETY: layout is valid and outlives the returned empty vec.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            assert!(!v.is_null());
            assert_eq!((*v).elem_size, core::mem::size_of::<Payload>());
            assert_eq!((*v).elem_kind, ElemKind::Plain);
            assert_eq!((*v).layout, &raw const layout);
            assert_eq!(hew_vec_len(v), 0);
            hew_vec_free(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_vec_push_layout_stub_fails_closed() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_push_layout_stub_fails_closed",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "layout-aware push must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr)
                .contains("PANIC: Vec layout-aware operation is not implemented"),
            "layout-aware push must report the staged fail-closed diagnostic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for test_vec_push_layout_stub_fails_closed — must abort"]
    fn _helper_vec_push_layout_stub_fails_closed() {
        #[repr(C)]
        struct Payload {
            a: u64,
            b: u64,
        }

        let layout = HewTypeLayout {
            size: core::mem::size_of::<Payload>(),
            align: core::mem::align_of::<Payload>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };

        // SAFETY: FFI calls use a valid descriptor and stack-allocated payload.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let payload = Payload { a: 1, b: 2 };
            hew_vec_push_layout(v, (&raw const payload).cast(), &raw const layout);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn test_vec_get_generic_oob() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_get_generic_oob",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "out-of-bounds get must terminate abnormally"
        );
        assert!(
            String::from_utf8_lossy(&status.stderr).contains("PANIC: Vec index out of bounds"),
            "out-of-bounds get must report the vec bounds panic"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for test_vec_get_generic_oob — must panic"]
    fn _helper_vec_get_generic_oob() {
        #[repr(C)]
        struct Payload {
            a: u64,
            b: u64,
        }

        // SAFETY: FFI calls use valid vec pointer and stack-allocated payload.
        unsafe {
            let v = hew_vec_new_generic(i64::try_from(core::mem::size_of::<Payload>()).unwrap(), 0);
            let payload = Payload { a: 1, b: 2 };
            hew_vec_push_generic(v, (&raw const payload).cast());
            let _ = hew_vec_get_generic(v, 1);
        }
    }

    #[test]
    fn test_vec_clear() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointers returned by hew_vec_new/hew_vec_clone.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new.
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

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_vec_hwvec_to_u8_roundtrip() {
        // SAFETY: FFI calls use valid data slices and vec pointers.
        unsafe {
            let data: &[u8] = &[72, 101, 108, 108, 111]; // "Hello"
            let v = u8_to_hwvec(data);
            let result = hwvec_to_u8(v);
            assert_eq!(result, data);
            hew_vec_free(v);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_vec_hwvec_to_u8_empty() {
        // SAFETY: Empty slice is valid input to u8_to_hwvec.
        unsafe {
            let v = u8_to_hwvec(&[]);
            let result = hwvec_to_u8(v);
            assert!(result.is_empty());
            hew_vec_free(v);
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_vec_hwvec_to_u8_null() {
        // SAFETY: Null is explicitly handled by hwvec_to_u8.
        unsafe {
            let result = hwvec_to_u8(core::ptr::null_mut());
            assert!(result.is_empty());
        }
    }

    // ------------------------------------------------------------------
    // Range-slice (C-3): hew_vec_slice_range_T
    // ------------------------------------------------------------------

    #[test]
    fn slice_range_i64_returns_fresh_vec_with_subrange_elements() {
        // SAFETY: FFI calls use valid vec pointer returned by hew_vec_new_i64.
        unsafe {
            let v = hew_vec_new_i64();
            for i in 0..5_i64 {
                hew_vec_push_i64(v, 10 * (i + 1));
            }
            // [10, 20, 30, 40, 50]
            let sub = hew_vec_slice_range_i64(v, 1, 4); // [20, 30, 40]
            assert!(!sub.is_null());
            assert_ne!(sub, v, "slice must allocate a fresh Vec, not alias");
            assert_eq!(hew_vec_len(sub), 3);
            assert_eq!(hew_vec_get_i64(sub, 0), 20);
            assert_eq!(hew_vec_get_i64(sub, 1), 30);
            assert_eq!(hew_vec_get_i64(sub, 2), 40);
            hew_vec_free(sub);
            // Original vec must be untouched.
            assert_eq!(hew_vec_len(v), 5);
            hew_vec_free(v);
        }
    }

    #[test]
    fn slice_range_i64_empty_range_returns_empty_vec() {
        // SAFETY: FFI calls use valid vec pointer.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 1);
            hew_vec_push_i64(v, 2);
            let sub = hew_vec_slice_range_i64(v, 1, 1);
            assert!(!sub.is_null());
            assert_eq!(hew_vec_len(sub), 0);
            hew_vec_free(sub);
            hew_vec_free(v);
        }
    }

    #[test]
    fn slice_range_i64_full_range_clones_all_elements() {
        // SAFETY: FFI calls use valid vec pointer.
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 7);
            hew_vec_push_i64(v, 11);
            let sub = hew_vec_slice_range_i64(v, 0, 2);
            assert_eq!(hew_vec_len(sub), 2);
            assert_eq!(hew_vec_get_i64(sub, 0), 7);
            assert_eq!(hew_vec_get_i64(sub, 1), 11);
            hew_vec_free(sub);
            hew_vec_free(v);
        }
    }

    #[test]
    fn slice_range_f64_returns_fresh_vec() {
        // SAFETY: FFI calls use valid vec pointer.
        unsafe {
            let v = hew_vec_new_f64();
            hew_vec_push_f64(v, 1.5);
            hew_vec_push_f64(v, 2.5);
            hew_vec_push_f64(v, 3.5);
            let sub = hew_vec_slice_range_f64(v, 0, 2);
            assert_eq!(hew_vec_len(sub), 2);
            assert!((hew_vec_get_f64(sub, 0) - 1.5).abs() < f64::EPSILON);
            assert!((hew_vec_get_f64(sub, 1) - 2.5).abs() < f64::EPSILON);
            hew_vec_free(sub);
            hew_vec_free(v);
        }
    }

    #[test]
    fn slice_range_i32_returns_fresh_vec() {
        // SAFETY: FFI calls use valid vec pointer.
        unsafe {
            let v = hew_vec_new();
            hew_vec_push_i32(v, 100);
            hew_vec_push_i32(v, 200);
            hew_vec_push_i32(v, 300);
            let sub = hew_vec_slice_range_i32(v, 1, 3);
            assert_eq!(hew_vec_len(sub), 2);
            assert_eq!(hew_vec_get_i32(sub, 0), 200);
            assert_eq!(hew_vec_get_i32(sub, 1), 300);
            hew_vec_free(sub);
            hew_vec_free(v);
        }
    }

    // ------------------------------------------------------------------
    // Layout-backed Vec::remove (hew_vec_remove_at_layout)
    // ------------------------------------------------------------------

    /// `Point`-shaped layout descriptor for test use.
    fn point_layout() -> HewTypeLayout {
        #[repr(C)]
        struct Point {
            x: i64,
            y: i64,
        }
        HewTypeLayout {
            size: core::mem::size_of::<Point>(),
            align: core::mem::align_of::<Point>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
        }
    }

    /// Push a `(x, y)` pair into a layout-backed Vec using `hew_vec_push_layout`.
    ///
    /// # Safety
    ///
    /// `v` must be a valid layout-backed `HewVec` with a Plain `{i64, i64}` element layout.
    unsafe fn push_point(v: *mut HewVec, x: i64, y: i64, layout: &HewTypeLayout) {
        #[repr(C)]
        struct Point {
            x: i64,
            y: i64,
        }
        let p = Point { x, y };
        // SAFETY: caller guarantees v and layout are valid.
        unsafe {
            hew_vec_push_layout(v, (&raw const p).cast(), (&raw const *layout).cast());
        }
    }

    /// Read `(x, y)` from slot `index` of a layout-backed Vec.
    ///
    /// # Safety
    ///
    /// `v` must be a valid layout-backed `HewVec` with a Plain `{i64, i64}` element layout.
    unsafe fn get_point(v: *const HewVec, index: i64, layout: &HewTypeLayout) -> (i64, i64) {
        #[repr(C)]
        struct Point {
            x: i64,
            y: i64,
        }
        // SAFETY: caller guarantees v, index, and layout are valid.
        let ptr =
            unsafe { hew_vec_get_layout(v, index, (&raw const *layout).cast()).cast::<Point>() };
        // SAFETY: hew_vec_get_layout returns a valid pointer to the element.
        let p = unsafe { &*ptr };
        (p.x, p.y)
    }

    #[test]
    fn vec_remove_at_layout_removes_middle_element() {
        let layout = point_layout();
        // SAFETY: all FFI calls use valid layout-backed HewVec and Plain Point layout.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 1, 2, &layout);
            push_point(v, 3, 4, &layout);
            push_point(v, 5, 6, &layout);

            hew_vec_remove_at_layout(v, 1, &raw const layout);

            assert_eq!(hew_vec_len(v), 2, "len should be 2 after removing middle");
            assert_eq!(get_point(v, 0, &layout), (1, 2), "first element unchanged");
            assert_eq!(
                get_point(v, 1, &layout),
                (5, 6),
                "last element shifted left"
            );

            hew_vec_free(v);
        }
    }

    #[test]
    fn vec_remove_at_layout_removes_first_element() {
        let layout = point_layout();
        // SAFETY: all FFI calls use valid layout-backed HewVec and Plain Point layout.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 10, 20, &layout);
            push_point(v, 30, 40, &layout);
            push_point(v, 50, 60, &layout);

            hew_vec_remove_at_layout(v, 0, &raw const layout);

            assert_eq!(hew_vec_len(v), 2, "len should be 2 after removing first");
            assert_eq!(
                get_point(v, 0, &layout),
                (30, 40),
                "second element now first"
            );
            assert_eq!(
                get_point(v, 1, &layout),
                (50, 60),
                "third element now second"
            );

            hew_vec_free(v);
        }
    }

    #[test]
    fn vec_remove_at_layout_removes_last_element() {
        let layout = point_layout();
        // SAFETY: all FFI calls use valid layout-backed HewVec and Plain Point layout.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 7, 8, &layout);
            push_point(v, 9, 10, &layout);

            hew_vec_remove_at_layout(v, 1, &raw const layout);

            assert_eq!(hew_vec_len(v), 1, "len should be 1 after removing last");
            assert_eq!(get_point(v, 0, &layout), (7, 8), "first element unchanged");

            hew_vec_free(v);
        }
    }

    #[test]
    fn vec_remove_at_layout_single_element_leaves_empty_vec() {
        let layout = point_layout();
        // SAFETY: all FFI calls use valid layout-backed HewVec and Plain Point layout.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 42, 99, &layout);

            hew_vec_remove_at_layout(v, 0, &raw const layout);

            assert_eq!(
                hew_vec_len(v),
                0,
                "vec should be empty after removing sole element"
            );
            assert!(hew_vec_is_empty(v));

            hew_vec_free(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn vec_remove_at_layout_oob_aborts() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_remove_at_layout_oob",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "out-of-bounds layout remove must terminate abnormally"
        );
        let stderr = String::from_utf8_lossy(&status.stderr);
        assert!(
            stderr.contains("PANIC: Vec index out of bounds"),
            "out-of-bounds remove must report the vec bounds panic; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for vec_remove_at_layout_oob_aborts — must abort"]
    fn _helper_vec_remove_at_layout_oob() {
        let layout = point_layout();
        // SAFETY: FFI calls use valid layout-backed HewVec and Plain Point layout.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 1, 2, &layout);
            // index 1 is out of bounds for a single-element vec
            hew_vec_remove_at_layout(v, 1, &raw const layout);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn vec_remove_at_layout_layout_managed_aborts() {
        // Non-Plain (LayoutManaged) layout must fail closed — the BitCopy
        // operation gate must abort rather than corrupt ownership.
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_remove_at_layout_managed",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "LayoutManaged remove must terminate abnormally"
        );
        let stderr = String::from_utf8_lossy(&status.stderr);
        assert!(
            stderr.contains("PANIC: Vec layout-aware operation is not implemented"),
            "LayoutManaged remove must report the fail-closed diagnostic; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for vec_remove_at_layout_layout_managed_aborts — must abort"]
    fn _helper_vec_remove_at_layout_managed() {
        let layout = HewTypeLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };
        // SAFETY: FFI calls use a valid layout pointer; the abort is expected.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            hew_vec_remove_at_layout(v, 0, &raw const layout);
        }
    }

    #[test]
    fn slice_range_str_strdups_each_element_so_drops_are_independent() {
        // SAFETY: FFI calls use valid vec pointer and valid C strings.
        unsafe {
            let v = hew_vec_new_str();
            let s1 = CString::new("alpha").unwrap();
            let s2 = CString::new("beta").unwrap();
            let s3 = CString::new("gamma").unwrap();
            hew_vec_push_str(v, s1.as_ptr());
            hew_vec_push_str(v, s2.as_ptr());
            hew_vec_push_str(v, s3.as_ptr());

            let sub = hew_vec_slice_range_str(v, 0, 2);
            assert_eq!(hew_vec_len(sub), 2);
            assert_eq!((*sub).elem_kind, ElemKind::String);
            // Freeing the slice must NOT invalidate strings in the original
            // vec — strdup gives each side its own copies.
            hew_vec_free(sub);

            let r0 = hew_vec_get_str(v, 0);
            assert_eq!(std::ffi::CStr::from_ptr(r0).to_string_lossy(), "alpha");
            libc::free(r0.cast_mut().cast()); // ALLOCATOR-PAIRING: libc
            hew_vec_free(v);
        }
    }

    // ------------------------------------------------------------------
    // Layout-backed Vec::clone (hew_vec_clone_layout)
    // ------------------------------------------------------------------

    #[test]
    fn vec_clone_layout_empty_returns_empty_vec() {
        let layout = point_layout();
        // SAFETY: layout is valid and outlives the returned vec.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            let cloned = hew_vec_clone_layout(v, &raw const layout);
            assert!(!cloned.is_null());
            assert_eq!(hew_vec_len(cloned), 0);
            assert_eq!((*cloned).elem_size, layout.size);
            assert!(!(*cloned).layout.is_null());
            hew_vec_free(cloned);
            hew_vec_free(v);
        }
    }

    #[test]
    fn vec_clone_layout_copies_elements_independently() {
        #[repr(C)]
        struct Point {
            x: i64,
            y: i64,
        }
        let layout = point_layout();
        // SAFETY: layout is valid and outlives both vecs.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 10, 20, &layout);
            push_point(v, 30, 40, &layout);
            push_point(v, 50, 60, &layout);

            let cloned = hew_vec_clone_layout(v, &raw const layout);
            assert!(!cloned.is_null());
            assert_eq!(hew_vec_len(cloned), 3);

            // Elements match the source.
            assert_eq!(get_point(cloned, 0, &layout), (10, 20));
            assert_eq!(get_point(cloned, 1, &layout), (30, 40));
            assert_eq!(get_point(cloned, 2, &layout), (50, 60));

            // Cloned data is independent: mutate source via set_layout.
            let new_p = Point { x: 99, y: 99 };
            hew_vec_set_layout(v, 0, (&raw const new_p).cast(), (&raw const layout).cast());
            // Clone is unaffected.
            assert_eq!(get_point(cloned, 0, &layout), (10, 20));

            hew_vec_free(cloned);
            hew_vec_free(v);
        }
    }

    #[test]
    fn vec_clone_layout_clone_has_correct_metadata() {
        let layout = point_layout();
        // SAFETY: layout is valid and outlives both vecs.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 1, 2, &layout);

            let cloned = hew_vec_clone_layout(v, &raw const layout);
            assert_eq!((*cloned).elem_size, layout.size);
            assert_eq!((*cloned).elem_kind, ElemKind::Plain);
            assert!(!(*cloned).layout.is_null());

            hew_vec_free(cloned);
            hew_vec_free(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn vec_clone_layout_layout_managed_aborts() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_clone_layout_managed",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "LayoutManaged clone must terminate abnormally"
        );
        let stderr = String::from_utf8_lossy(&status.stderr);
        assert!(
            stderr.contains("PANIC: Vec layout-aware operation is not implemented"),
            "LayoutManaged clone must report the fail-closed diagnostic; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for vec_clone_layout_layout_managed_aborts — must abort"]
    fn _helper_vec_clone_layout_managed() {
        let layout = HewTypeLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };
        // SAFETY: FFI calls use a valid layout pointer; the abort is expected.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            hew_vec_clone_layout(v, &raw const layout);
        }
    }

    // ------------------------------------------------------------------
    // Witness-managed Vec clone/free pair (hew_vec_clone_managed /
    // hew_vec_free_managed — W5.002 F0b)
    // ------------------------------------------------------------------

    #[test]
    fn vec_clone_managed_null_returns_null() {
        // SAFETY: null is a documented boundary-fail-closed shape.
        unsafe {
            assert!(hew_vec_clone_managed(ptr::null()).is_null());
        }
    }

    #[test]
    fn vec_free_managed_null_is_noop() {
        // SAFETY: free(null) is the only permitted silent-return shape.
        unsafe {
            hew_vec_free_managed(ptr::null_mut());
        }
    }

    #[test]
    fn vec_clone_managed_plain_primitive_roundtrip() {
        // SAFETY: FFI calls use a valid i64 HewVec (layout absent).
        unsafe {
            let v = hew_vec_new_i64();
            hew_vec_push_i64(v, 11);
            hew_vec_push_i64(v, 22);
            hew_vec_push_i64(v, 33);

            let cloned = hew_vec_clone_managed(v);
            assert!(!cloned.is_null());
            assert_eq!(hew_vec_len(cloned), 3);
            assert_eq!(hew_vec_get_i64(cloned, 0), 11);
            assert_eq!(hew_vec_get_i64(cloned, 1), 22);
            assert_eq!(hew_vec_get_i64(cloned, 2), 33);

            // Independence: mutating the source leaves the clone intact.
            hew_vec_set_i64(v, 0, 99);
            assert_eq!(hew_vec_get_i64(cloned, 0), 11);

            hew_vec_free_managed(cloned);
            hew_vec_free_managed(v);
        }
    }

    #[test]
    fn vec_clone_managed_string_elements_are_independent() {
        // SAFETY: FFI calls use a valid string HewVec and valid C strings.
        unsafe {
            let v = hew_vec_new_str();
            let a = CString::new("alpha").unwrap();
            let b = CString::new("beta").unwrap();
            hew_vec_push_str(v, a.as_ptr());
            hew_vec_push_str(v, b.as_ptr());

            let cloned = hew_vec_clone_managed(v);
            assert!(!cloned.is_null());
            assert_eq!(hew_vec_len(cloned), 2);
            assert_eq!((*cloned).elem_kind, ElemKind::String);

            let c0 = hew_vec_get_str(cloned, 0);
            assert_eq!(std::ffi::CStr::from_ptr(c0).to_string_lossy(), "alpha");

            // Freeing the clone (per-slot strdup'd strings) must not invalidate
            // the source strings.
            hew_vec_free_managed(cloned);
            let s0 = hew_vec_get_str(v, 0);
            assert_eq!(std::ffi::CStr::from_ptr(s0).to_string_lossy(), "alpha");
            hew_vec_free_managed(v);
        }
    }

    #[test]
    fn vec_clone_managed_layout_backed_plain_roundtrip() {
        // Legacy `hew_vec_clone` aborts on a layout-backed Vec via
        // `abort_if_layout_aware`; the managed clone must succeed and preserve
        // the descriptor so the clone frees with the same discipline.
        #[repr(C)]
        struct Point {
            x: i64,
            y: i64,
        }
        let layout = point_layout();
        // SAFETY: layout is valid and outlives both vecs.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            push_point(v, 10, 20, &layout);
            push_point(v, 30, 40, &layout);

            let cloned = hew_vec_clone_managed(v);
            assert!(!cloned.is_null());
            assert_eq!(hew_vec_len(cloned), 2);
            assert!(!(*cloned).layout.is_null());
            assert_eq!((*cloned).elem_size, layout.size);
            assert_eq!(get_point(cloned, 0, &layout), (10, 20));
            assert_eq!(get_point(cloned, 1, &layout), (30, 40));

            // Independence across the layout-backed copy.
            let mutated = Point { x: 7, y: 8 };
            hew_vec_set_layout(
                v,
                0,
                (&raw const mutated).cast(),
                (&raw const layout).cast(),
            );
            assert_eq!(get_point(cloned, 0, &layout), (10, 20));

            // Free both through the managed drop (clone/free symmetry).
            hew_vec_free_managed(cloned);
            hew_vec_free_managed(v);
        }
    }

    #[test]
    fn vec_free_managed_empty_layout_backed_is_safe() {
        let layout = point_layout();
        // SAFETY: layout is valid and outlives the vec.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            // Empty layout-managed-free path: no elements, no fail-closed abort.
            hew_vec_free_managed(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn vec_clone_managed_layout_managed_aborts() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_clone_managed_layout_managed",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "LayoutManaged managed-clone must terminate abnormally"
        );
        let stderr = String::from_utf8_lossy(&status.stderr);
        assert!(
            stderr.contains("Vec layout-managed element clone thunk is unavailable"),
            "LayoutManaged managed-clone must report the fail-closed diagnostic; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for vec_clone_managed_layout_managed_aborts — must abort"]
    fn _helper_vec_clone_managed_layout_managed() {
        let layout = HewTypeLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };
        // SAFETY: FFI calls use a valid layout pointer; the abort is expected.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            // Force a live element so the fail-closed guard fires (an empty
            // layout-managed vec is droppable/cloneable without thunks).
            (*v).len = 1;
            hew_vec_clone_managed(v);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn vec_free_managed_layout_managed_aborts() {
        let status = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "vec::tests::_helper_vec_free_managed_layout_managed",
                "--include-ignored",
            ])
            .env("RUST_TEST_THREADS", "1")
            .output()
            .unwrap();
        assert!(
            !status.status.success(),
            "LayoutManaged managed-free must terminate abnormally"
        );
        let stderr = String::from_utf8_lossy(&status.stderr);
        assert!(
            stderr.contains("Vec layout-managed element drop thunk is unavailable"),
            "LayoutManaged managed-free must report the fail-closed diagnostic; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[ignore = "helper for vec_free_managed_layout_managed_aborts — must abort"]
    fn _helper_vec_free_managed_layout_managed() {
        let layout = HewTypeLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        };
        // SAFETY: FFI calls use a valid layout pointer; the abort is expected.
        unsafe {
            let v = hew_vec_new_with_layout(&raw const layout);
            // Pretend a live element exists so the fail-closed drop guard fires.
            (*v).len = 1;
            hew_vec_free_managed(v);
        }
    }
}
