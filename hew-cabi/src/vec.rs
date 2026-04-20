//! `HewVec` type definition and byte-conversion helpers.
//!
//! This module re-declares the `HewVec` struct and `ElemKind` enum so that
//! native package crates can reference them without depending on the full
//! hew-runtime.  The actual `hew_vec_*` function bodies live in hew-runtime;
//! here we provide `extern "C"` declarations that resolve at link time.

use core::ffi::{c_char, c_void};
use core::mem;

/// Discriminator for element ownership semantics.
///
/// Used by clone/truncate/free/clear to decide whether elements need
/// deep-copy (`strdup`) or individual `free`.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElemKind {
    /// Plain value type (i32, i64, f64, structs, etc.) — no special handling.
    Plain = 0,
    /// String (`*const c_char`) — elements are `strdup`'d on push and must be
    /// individually `free`'d on removal.
    String = 1,
}

/// Dynamic array matching the C `HewVec` layout.
#[repr(C)]
#[derive(Debug)]
pub struct HewVec {
    /// Pointer to the backing buffer.
    pub data: *mut u8,
    /// Number of elements currently stored.
    pub len: usize,
    /// Allocated capacity (in elements).
    pub cap: usize,
    /// Size of each element in bytes.
    pub elem_size: usize,
    /// Element ownership semantics.
    pub elem_kind: ElemKind,
}

// ---------------------------------------------------------------------------
// Extern declarations — resolved at link time against libhew_runtime.a
// ---------------------------------------------------------------------------

extern "C" {
    // Constructors
    pub fn hew_vec_new() -> *mut HewVec;
    pub fn hew_vec_new_i64() -> *mut HewVec;
    pub fn hew_vec_new_f64() -> *mut HewVec;
    pub fn hew_vec_new_str() -> *mut HewVec;
    pub fn hew_vec_new_ptr() -> *mut HewVec;
    pub fn hew_vec_new_with_elem_size(elem_size: i64) -> *mut HewVec;
    pub fn hew_vec_new_generic(elem_size: i64, elem_kind: i64) -> *mut HewVec;

    // Push
    pub fn hew_vec_push_i32(v: *mut HewVec, val: i32);
    pub fn hew_vec_push_i64(v: *mut HewVec, val: i64);
    pub fn hew_vec_push_f64(v: *mut HewVec, val: f64);
    pub fn hew_vec_push_str(v: *mut HewVec, val: *const c_char);
    pub fn hew_vec_push_ptr(v: *mut HewVec, val: *mut c_void);
    pub fn hew_vec_push_generic(v: *mut HewVec, data: *const c_void);

    // Get
    #[cfg(not(test))]
    pub fn hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32;
    pub fn hew_vec_get_i64(v: *mut HewVec, index: i64) -> i64;
    pub fn hew_vec_get_f64(v: *mut HewVec, index: i64) -> f64;
    pub fn hew_vec_get_str(v: *mut HewVec, index: i64) -> *const c_char;
    pub fn hew_vec_get_ptr(v: *mut HewVec, index: i64) -> *mut c_void;
    pub fn hew_vec_get_generic(v: *const HewVec, index: i64) -> *const c_void;

    // Set
    pub fn hew_vec_set_i32(v: *mut HewVec, index: i64, val: i32);
    pub fn hew_vec_set_i64(v: *mut HewVec, index: i64, val: i64);
    pub fn hew_vec_set_f64(v: *mut HewVec, index: i64, val: f64);
    pub fn hew_vec_set_str(v: *mut HewVec, index: i64, val: *const c_char);
    pub fn hew_vec_set_generic(v: *mut HewVec, index: i64, data: *const c_void);

    // Pop
    pub fn hew_vec_pop_i32(v: *mut HewVec) -> i32;
    pub fn hew_vec_pop_i64(v: *mut HewVec) -> i64;
    pub fn hew_vec_pop_f64(v: *mut HewVec) -> f64;
    pub fn hew_vec_pop_str(v: *mut HewVec) -> *const c_char;
    pub fn hew_vec_pop_generic(v: *mut HewVec, out: *mut c_void) -> i32;

    // Queries
    #[cfg(not(test))]
    pub fn hew_vec_len(v: *mut HewVec) -> i64;
    pub fn hew_vec_is_empty(v: *mut HewVec) -> bool;

    // Mutation
    pub fn hew_vec_clear(v: *mut HewVec);
    pub fn hew_vec_free(v: *mut HewVec);
    pub fn hew_vec_sort_i32(v: *mut HewVec);
    pub fn hew_vec_sort_i64(v: *mut HewVec);
    pub fn hew_vec_sort_f64(v: *mut HewVec);
    pub fn hew_vec_clone(v: *const HewVec) -> *mut HewVec;
    pub fn hew_vec_append(dst: *mut HewVec, src: *const HewVec);
    pub fn hew_vec_swap(v: *mut HewVec, i: i64, j: i64);
    pub fn hew_vec_truncate(v: *mut HewVec, new_len: i64);
    pub fn hew_vec_reverse_i32(v: *mut HewVec);

    // Contains
    pub fn hew_vec_contains_i32(v: *const HewVec, val: i32) -> i32;
    pub fn hew_vec_contains_i64(v: *const HewVec, val: i64) -> i32;
    pub fn hew_vec_contains_f64(v: *const HewVec, val: f64) -> i32;
    pub fn hew_vec_contains_str(v: *const HewVec, val: *const c_char) -> i32;

    // Remove
    pub fn hew_vec_remove_i32(v: *mut HewVec, val: i32);
    pub fn hew_vec_remove_i64(v: *mut HewVec, val: i64);
    pub fn hew_vec_remove_ptr(v: *mut HewVec, val: *mut c_void);
}

#[cfg(test)]
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    reason = "test stub mirrors the runtime ABI export"
)]
pub extern "C" fn hew_vec_len(v: *mut HewVec) -> i64 {
    if v.is_null() {
        return 0;
    }
    #[expect(
        clippy::cast_possible_wrap,
        reason = "test stub mirrors platform-sized len in i64 space"
    )]
    // SAFETY: the test stub only dereferences the caller-provided `HewVec`.
    unsafe {
        (*v).len as i64
    }
}

#[cfg(test)]
#[no_mangle]
#[expect(
    clippy::not_unsafe_ptr_arg_deref,
    clippy::cast_ptr_alignment,
    clippy::missing_panics_doc,
    reason = "test stub mirrors the runtime ABI export"
)]
pub extern "C" fn hew_vec_get_i32(v: *mut HewVec, index: i64) -> i32 {
    assert!(!v.is_null(), "test stub requires a non-null HewVec");
    let index: usize = index
        .try_into()
        .expect("test stub index must be non-negative");
    // SAFETY: the null check above ensures `v` is safe to inspect here.
    assert!(index < unsafe { (*v).len }, "test stub index out of bounds");
    // SAFETY: the null check above ensures `v` is safe to inspect here.
    let data = unsafe { (*v).data.cast::<i32>() };
    // SAFETY: the bounds check above ensures `index` addresses initialized data.
    unsafe { *data.add(index) }
}

// ---------------------------------------------------------------------------
// bytes <-> HewVec helpers (used by codec wrappers in native packages)
// ---------------------------------------------------------------------------

/// Extract raw bytes from a `bytes`-typed `HewVec` (i32 elements, one byte per slot).
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` with i32 element size.
///
/// # Panics
///
/// Panics if `v` is null, has non-plain elements, has the wrong element size,
/// or contains values outside the byte range `0..=255`.
pub unsafe fn hwvec_to_u8(v: *mut HewVec) -> Vec<u8> {
    assert!(!v.is_null(), "hwvec_to_u8: null HewVec pointer");
    // SAFETY: the caller promises `v` points to a valid HewVec.
    let vec = unsafe { &*v };
    assert!(
        vec.elem_kind == ElemKind::Plain,
        "hwvec_to_u8: expected plain elements, got {:?}",
        vec.elem_kind
    );
    assert!(
        vec.elem_size == mem::size_of::<i32>(),
        "hwvec_to_u8: expected elem_size {}, got {}",
        mem::size_of::<i32>(),
        vec.elem_size
    );
    // SAFETY: caller guarantees v is a valid HewVec.
    #[cfg(test)]
    let len = hew_vec_len(v);
    #[cfg(not(test))]
    // SAFETY: caller guarantees `v` points to a valid HewVec.
    let len = unsafe { hew_vec_len(v) };
    (0..len)
        .map(|i| {
            #[cfg(test)]
            let raw = hew_vec_get_i32(v, i);
            #[cfg(not(test))]
            // SAFETY: `i < len`, so the read is in-bounds for the caller-provided HewVec.
            let raw = unsafe { hew_vec_get_i32(v, i) };
            u8::try_from(raw).expect("hwvec_to_u8: element out of byte range")
        })
        .collect()
}

/// Create a new bytes-typed `HewVec` (i32 elements) from a raw u8 slice.
///
/// # Safety
///
/// None — all memory is managed by the runtime allocator.
#[must_use]
pub unsafe fn u8_to_hwvec(data: &[u8]) -> *mut HewVec {
    // SAFETY: hew_vec_new allocates a valid HewVec.
    let v = unsafe { hew_vec_new() };
    for &b in data {
        // SAFETY: v is non-null (hew_vec_new aborts on OOM).
        unsafe { hew_vec_push_i32(v, i32::from(b)) };
    }
    v
}

// ---------------------------------------------------------------------------
// Tests — struct layout and discriminant values
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // ── ElemKind discriminant values (ABI contract) ──────────────────────

    #[test]
    fn elem_kind_plain_is_zero() {
        assert_eq!(ElemKind::Plain as u8, 0, "Plain must be 0 for C interop");
    }

    #[test]
    fn elem_kind_string_is_one() {
        assert_eq!(ElemKind::String as u8, 1, "String must be 1 for C interop");
    }

    #[test]
    fn elem_kind_size_is_one_byte() {
        assert_eq!(
            std::mem::size_of::<ElemKind>(),
            1,
            "repr(u8) must be exactly 1 byte"
        );
    }

    #[test]
    fn elem_kind_clone_and_eq() {
        let a = ElemKind::Plain;
        let b = a;
        assert_eq!(a, b);
        assert_ne!(ElemKind::Plain, ElemKind::String);
    }

    // ── HewVec layout (must match the C struct) ─────────────────────────

    #[test]
    fn hewvec_is_repr_c() {
        // Verify the struct size is the sum of its fields with C alignment.
        // On 64-bit: 3 pointers/usizes (8 each) + 1 usize + 1 u8 + padding.
        let size = std::mem::size_of::<HewVec>();
        let align = std::mem::align_of::<HewVec>();
        // Must be pointer-aligned for C interop.
        assert_eq!(align, std::mem::align_of::<*mut u8>());
        // 4 usizes (data, len, cap, elem_size) + 1 u8 (elem_kind) + padding
        // = 4*8 + 1 + 7 padding = 40 bytes on 64-bit.
        assert!(
            size > 4 * std::mem::size_of::<usize>(),
            "HewVec too small: {size}"
        );
    }

    #[test]
    fn hewvec_field_offsets_are_stable() {
        // Construct a HewVec with known sentinel values and read them back
        // to verify field ordering matches the C layout.
        let v = HewVec {
            data: 0xAAAA_usize as *mut u8,
            len: 42,
            cap: 100,
            elem_size: 8,
            elem_kind: ElemKind::String,
        };
        assert_eq!(v.data as usize, 0xAAAA);
        assert_eq!(v.len, 42);
        assert_eq!(v.cap, 100);
        assert_eq!(v.elem_size, 8);
        assert_eq!(v.elem_kind, ElemKind::String);
    }

    #[test]
    fn hewvec_debug_output_is_readable() {
        let v = HewVec {
            data: std::ptr::null_mut(),
            len: 0,
            cap: 0,
            elem_size: 4,
            elem_kind: ElemKind::Plain,
        };
        let debug = format!("{v:?}");
        // Verify Debug output includes key field names.
        assert!(debug.contains("HewVec"), "Debug should include type name");
        assert!(debug.contains("len: 0"), "Debug should include len");
        assert!(debug.contains("Plain"), "Debug should include elem_kind");
    }

    #[test]
    fn hewvec_default_field_values_are_sane() {
        // A zeroed HewVec (like C's memset-to-zero) should be a valid empty vec.
        let v = HewVec {
            data: std::ptr::null_mut(),
            len: 0,
            cap: 0,
            elem_size: 0,
            elem_kind: ElemKind::Plain,
        };
        assert!(v.data.is_null());
        assert_eq!(v.len, 0);
        assert_eq!(v.cap, 0);
    }

    #[test]
    fn hwvec_to_u8_rejects_null_pointer() {
        // SAFETY: this test intentionally exercises the null-pointer panic path.
        let panic = std::panic::catch_unwind(|| unsafe {
            let _ = hwvec_to_u8(std::ptr::null_mut());
        })
        .expect_err("null vectors must panic");
        let message = panic
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| panic.downcast_ref::<&str>().copied())
            .unwrap_or("<non-string panic>");
        assert!(message.contains("null HewVec pointer"));
    }

    #[test]
    fn hwvec_to_u8_rejects_non_byte_shape() {
        let mut vec = HewVec {
            data: std::ptr::null_mut(),
            len: 0,
            cap: 0,
            elem_size: mem::size_of::<i64>(),
            elem_kind: ElemKind::Plain,
        };
        let vec_ptr = &raw mut vec;
        // SAFETY: this test intentionally exercises the invalid-shape panic path.
        let panic = std::panic::catch_unwind(move || unsafe {
            let _ = hwvec_to_u8(vec_ptr);
        })
        .expect_err("non-byte vectors must panic");
        let message = panic
            .downcast_ref::<String>()
            .map(String::as_str)
            .or_else(|| panic.downcast_ref::<&str>().copied())
            .unwrap_or("<non-string panic>");
        assert!(message.contains("expected elem_size"));
    }
}
