//! Layout descriptor types for key/value identity in `HashMap` and `HashSet`.
//!
//! This module defines the FFI-safe layout descriptor structs that describe
//! how the runtime must hash and compare opaque key blobs, and how it must
//! manage opaque value blobs, for layout-backed maps and sets.
//!
//! # Ownership contract (LESSONS `ffi-ownership-contracts` P0)
//!
//! - The `hash_fn` and `eq_fn` thunks receive read-only pointers to key blobs.
//!   **Neither thunk owns nor frees the blob it receives.**
//! - The runtime owns all Plain/BitCopy blob copies once they are inserted.
//!
//! # Fail-closed contract (LESSONS `boundary-fail-closed` P0)
//!
//! - `hash_fn` and `eq_fn` are stored as `Option<...>` rather than bare fn ptrs.
//!   `None` means "no thunk provided".  The runtime (C-1b) **must** detect
//!   `None` and abort fail-closed rather than silently returning a wrong result.
//! - Constructing a `HewMapKeyLayout` with `None` thunks is valid Rust; runtime
//!   enforcement is the runtime's responsibility.
//!
//! # Niche-optimization invariant
//!
//! `Option<HewMapKeyHashThunk>` and `Option<HewMapKeyEqThunk>` carry the same
//! ABI width as the underlying fn pointer because Rust guarantees that fn
//! pointers are non-null, enabling the null value as the `None` discriminant.
//! Compile-time `const` asserts below lock this invariant.

use core::ffi::c_void;

pub use crate::vec::HewTypeOwnershipKind;

// ---------------------------------------------------------------------------
// Hash and equality thunk type aliases
// ---------------------------------------------------------------------------

/// Thunk that hashes an opaque key blob.
///
/// - `key` — non-null read-only pointer to the key blob.  The thunk **must
///   not** free or write through `key`.
/// - Returns a `u64` hash value.  The hash function must be deterministic and
///   must hash **typed field values only** — never padding bytes.
///
/// `unsafe` because the caller must guarantee `key` is valid for the
/// type's layout (size + alignment).
pub type HewMapKeyHashThunk = unsafe extern "C" fn(key: *const c_void) -> u64;

/// Thunk that compares two opaque key blobs for equality.
///
/// - `lhs`, `rhs` — non-null read-only pointers to key blobs of the same type.
///   The thunk **must not** free or write through either pointer.
/// - Returns non-zero (`1`) when the blobs represent equal keys, zero when not.
///
/// `unsafe` because the caller must guarantee both pointers are valid for the
/// type's layout (size + alignment).
pub type HewMapKeyEqThunk = unsafe extern "C" fn(lhs: *const c_void, rhs: *const c_void) -> i32;

// ---------------------------------------------------------------------------
// Niche-optimization compile-time assertions
// ---------------------------------------------------------------------------

// Rust guarantees that fn pointers are non-null, so Option<fn(...)> uses the
// null value as the None discriminant -- giving Option<fn> the same size as fn.
// These asserts lock the invariant so a future Rust change would break loudly
// here rather than silently at the C boundary.
const _: () = assert!(
    size_of::<Option<HewMapKeyHashThunk>>() == size_of::<HewMapKeyHashThunk>(),
    "Option<HewMapKeyHashThunk> must be niche-optimised to the same size as HewMapKeyHashThunk",
);
const _: () = assert!(
    size_of::<Option<HewMapKeyEqThunk>>() == size_of::<HewMapKeyEqThunk>(),
    "Option<HewMapKeyEqThunk> must be niche-optimised to the same size as HewMapKeyEqThunk",
);

// ---------------------------------------------------------------------------
// Layout descriptor structs
// ---------------------------------------------------------------------------

/// Describes the memory layout and identity semantics of a map key type.
///
/// The runtime uses `size` and `align` to allocate and copy key blobs, and
/// `hash_fn` / `eq_fn` to hash and compare them.
///
/// Both thunk fields are `Option<...>` rather than bare fn ptrs (see module-level
/// doc for the fail-closed contract).  Pass `None` only when the runtime is
/// expected to abort fail-closed on first use.
///
/// # C layout
///
/// `#[repr(C)]` guarantees field order and alignment match the C struct:
///
/// ```c
/// typedef struct {
///     size_t               size;
///     size_t               align;
///     HewTypeOwnershipKind ownership_kind;
///     /* padding to pointer alignment */
///     HewMapKeyHashThunk   hash_fn;   /* may be NULL */
///     HewMapKeyEqThunk     eq_fn;     /* may be NULL */
/// } HewMapKeyLayout;
/// ```
#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(
    dead_code,
    reason = "consumed by C-1b runtime ABI and C-3a codegen synthesis"
)]
pub struct HewMapKeyLayout {
    /// Size of the key blob in bytes.
    pub size: usize,
    /// Alignment of the key blob in bytes (must be a power of two).
    pub align: usize,
    /// Ownership/copy semantics of the key type.
    pub ownership_kind: HewTypeOwnershipKind,
    /// Hash thunk: `None` -> runtime must abort fail-closed on hash attempt.
    pub hash_fn: Option<HewMapKeyHashThunk>,
    /// Equality thunk: `None` -> runtime must abort fail-closed on eq attempt.
    pub eq_fn: Option<HewMapKeyEqThunk>,
}

/// Describes the memory layout and ownership semantics of a map value type.
///
/// Values have no identity thunks -- the runtime manages them purely by their
/// opaque blob shape (size + align + copy semantics).
///
/// # C layout
///
/// ```c
/// typedef struct {
///     size_t               size;
///     size_t               align;
///     HewTypeOwnershipKind ownership_kind;
/// } HewMapValueLayout;
/// ```
#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(
    dead_code,
    reason = "consumed by C-1b runtime ABI and C-3a codegen synthesis"
)]
pub struct HewMapValueLayout {
    /// Size of the value blob in bytes.
    pub size: usize,
    /// Alignment of the value blob in bytes (must be a power of two).
    pub align: usize,
    /// Ownership/copy semantics of the value type.
    pub ownership_kind: HewTypeOwnershipKind,
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    // -- niche-optimization assertions (also exercised at compile time above) --

    #[test]
    fn option_hash_thunk_has_same_size_as_raw_fn_ptr() {
        // The compile-time const assert above already covers this; the test
        // makes the invariant visible in test output and catches dynamic
        // platforms where the const may not fire.
        assert_eq!(
            size_of::<Option<HewMapKeyHashThunk>>(),
            size_of::<HewMapKeyHashThunk>(),
            "Option<HewMapKeyHashThunk> must have the same size as HewMapKeyHashThunk \
             (fn-pointer niche optimisation)",
        );
    }

    #[test]
    fn option_eq_thunk_has_same_size_as_raw_fn_ptr() {
        assert_eq!(
            size_of::<Option<HewMapKeyEqThunk>>(),
            size_of::<HewMapKeyEqThunk>(),
            "Option<HewMapKeyEqThunk> must have the same size as HewMapKeyEqThunk \
             (fn-pointer niche optimisation)",
        );
    }

    // -- HewMapKeyLayout repr --

    #[test]
    fn hashmap_key_layout_has_correct_repr() {
        // Layout on 64-bit (x86_64 / aarch64):
        //   size:           usize  = 8
        //   align:          usize  = 8
        //   ownership_kind: u8     = 1  (+7 padding to reach pointer alignment)
        //   hash_fn:        fn ptr = 8  (Option<fn> has same size via niche opt)
        //   eq_fn:          fn ptr = 8  (same)
        //   total = 8 + 8 + (1 + 7 pad) + 8 + 8 = 40 bytes; align = 8.
        // Verified on aarch64-apple-darwin with dbg!(size_of::<HewMapKeyLayout>()).
        assert_eq!(
            size_of::<HewMapKeyLayout>(),
            5 * size_of::<usize>(),
            "HewMapKeyLayout must be exactly 5 pointer-sized slots (40 bytes on 64-bit)",
        );
        assert_eq!(
            align_of::<HewMapKeyLayout>(),
            size_of::<usize>(),
            "HewMapKeyLayout must be pointer-aligned (8 bytes on 64-bit)",
        );
    }

    #[test]
    fn hashmap_key_layout_field_values_are_readable() {
        unsafe extern "C" fn dummy_hash(_key: *const c_void) -> u64 {
            0
        }
        unsafe extern "C" fn dummy_eq(_lhs: *const c_void, _rhs: *const c_void) -> i32 {
            1
        }

        let layout = HewMapKeyLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::Plain,
            hash_fn: Some(dummy_hash),
            eq_fn: Some(dummy_eq),
        };
        assert_eq!(layout.size, 16);
        assert_eq!(layout.align, 8);
        assert_eq!(layout.ownership_kind, HewTypeOwnershipKind::Plain);
        assert!(layout.hash_fn.is_some());
        assert!(layout.eq_fn.is_some());
    }

    // -- HewMapValueLayout repr --

    #[test]
    fn hashmap_value_layout_has_correct_repr() {
        // Layout on 64-bit (x86_64 / aarch64):
        //   size:           usize = 8
        //   align:          usize = 8
        //   ownership_kind: u8    = 1  (+7 padding to reach pointer alignment)
        //   total = 8 + 8 + (1 + 7 pad) = 24 bytes; align = 8.
        // Verified on aarch64-apple-darwin with dbg!(size_of::<HewMapValueLayout>()).
        assert_eq!(
            size_of::<HewMapValueLayout>(),
            3 * size_of::<usize>(),
            "HewMapValueLayout must be exactly 3 pointer-sized slots (24 bytes on 64-bit)",
        );
        assert_eq!(
            align_of::<HewMapValueLayout>(),
            size_of::<usize>(),
            "HewMapValueLayout must be pointer-aligned (8 bytes on 64-bit)",
        );
    }

    #[test]
    fn hashmap_value_layout_field_values_are_readable() {
        let layout = HewMapValueLayout {
            size: 24,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::Plain,
        };
        assert_eq!(layout.size, 24);
        assert_eq!(layout.align, 8);
        assert_eq!(layout.ownership_kind, HewTypeOwnershipKind::Plain);
    }

    // -- HewTypeOwnershipKind discriminant values (ABI contract) --

    #[test]
    fn hashmap_key_layout_ownership_kind_plain_discriminant() {
        // Plain must be 0: the C runtime default-initialises structs to zero;
        // a zero ownership_kind must mean "plain scalar / POD".
        // Convention confirmed from vec.rs HewTypeOwnershipKind definition.
        assert_eq!(
            HewTypeOwnershipKind::Plain as u8,
            0,
            "Plain must be discriminant 0 for C interop (zero-init is Plain)",
        );
    }

    // -- null-safe thunk construction --

    #[test]
    fn hashmap_key_layout_thunk_fields_are_null_safe() {
        // Constructing with None thunks must compile and produce a layout where
        // both thunks are absent.  The runtime's job (C-1b) is to detect None
        // and abort fail-closed.
        let layout = HewMapKeyLayout {
            size: 8,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::Plain,
            hash_fn: None,
            eq_fn: None,
        };
        assert!(
            layout.hash_fn.is_none(),
            "hash_fn must be None when not supplied",
        );
        assert!(
            layout.eq_fn.is_none(),
            "eq_fn must be None when not supplied",
        );
        // Verify that None is represented as a null-sized discriminant slot
        // (same size as Some(fn), proving the niche optimisation holds).
        assert_eq!(
            size_of_val(&layout.hash_fn),
            size_of::<HewMapKeyHashThunk>(),
            "None hash_fn slot must have the same footprint as a fn pointer",
        );
    }

    // -- Debug impls are present --

    #[test]
    fn hew_map_key_layout_debug_output() {
        let layout = HewMapKeyLayout {
            size: 4,
            align: 4,
            ownership_kind: HewTypeOwnershipKind::Plain,
            hash_fn: None,
            eq_fn: None,
        };
        let dbg = format!("{layout:?}");
        assert!(
            dbg.contains("HewMapKeyLayout"),
            "Debug must include type name"
        );
        assert!(dbg.contains("size: 4"), "Debug must include size");
        assert!(dbg.contains("Plain"), "Debug must include ownership_kind");
    }

    #[test]
    fn hew_map_value_layout_debug_output() {
        let layout = HewMapValueLayout {
            size: 8,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::Plain,
        };
        let dbg = format!("{layout:?}");
        assert!(
            dbg.contains("HewMapValueLayout"),
            "Debug must include type name",
        );
        assert!(dbg.contains("Plain"), "Debug must include ownership_kind");
    }
}
