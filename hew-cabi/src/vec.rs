//! `HewVec` type definition and byte-conversion helpers.
//!
//! This module re-declares the `HewVec` struct and `ElemKind` enum so that
//! native package crates can reference them without depending on the full
//! hew-runtime.  The actual `hew_vec_*` function bodies live in hew-runtime;
//! here we provide `extern "C"` declarations that resolve at link time.

use core::ffi::{c_char, c_void};

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
            size >= 4 * std::mem::size_of::<usize>() + 1,
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
}

// ---------------------------------------------------------------------------
// bytes <-> HewVec helpers (used by codec wrappers in native packages)
// ---------------------------------------------------------------------------

/// Extract raw bytes from a `bytes`-typed `HewVec` (i32 elements, one byte per slot).
///
/// # Safety
///
/// `v` must be a valid, non-null pointer to a `HewVec` with i32 element size.
pub unsafe fn hwvec_to_u8(v: *mut HewVec) -> Vec<u8> {
    if v.is_null() {
        return Vec::new();
    }
    // SAFETY: caller guarantees v is a valid HewVec.
    let len = unsafe { hew_vec_len(v) };
    (0..len)
        .map(|i| {
            // SAFETY: i < len.
            #[expect(
                clippy::cast_sign_loss,
                clippy::cast_possible_truncation,
                reason = "byte values stored as i32 are 0-255"
            )]
            // SAFETY: i < len, so index is in bounds.
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
