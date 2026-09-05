//! Hash identity metadata for descriptor-backed maps and sets.
//!
//! Keys and values use the same value copy/drop descriptor as vector elements.
//! A key adds borrowed hash/equality callbacks; neither callback owns its input.
//! Descriptors are copied into each collection at construction, so the caller
//! need not retain the descriptor storage.
//!
//! Insertion transfers the value on both vacant and occupied entries. It
//! transfers the key only when vacant; the caller retains a duplicate key on
//! replacement. Lookups borrow keys, and cloning lookups produce independent
//! values. Removal, replacement, clear and destruction release stored owners
//! through the value descriptor. A clone callback must roll back its own partial
//! output before returning failure.

use core::ffi::c_void;

pub use crate::vec::{
    HewTypeOwnershipKind, HewVecElemCloneThunk, HewVecElemDropThunk, HewVecElemLayout,
};

/// Hash a borrowed key's typed values, excluding padding bytes.
///
/// # Safety
/// The input must be valid for the key's concrete value descriptor.
pub type HewMapKeyHashThunk = unsafe extern "C" fn(key: *const c_void) -> u64;

/// Compare two borrowed keys of the same concrete type, returning non-zero
/// exactly when their values are equal.
///
/// # Safety
/// Both inputs must be valid for the key's concrete value descriptor.
pub type HewMapKeyEqThunk = unsafe extern "C" fn(lhs: *const c_void, rhs: *const c_void) -> i32;

/// A shared value protocol plus key identity callbacks.
///
/// Missing hash/equality callbacks are rejected at construction. Plain values
/// need no clone/drop callbacks. Owning values require a drop callback; copying
/// also requires a clone callback. Hashing and equality borrow the complete
/// value and never read padding or release its owners.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewMapKeyLayout {
    /// Size, alignment and semantic copy/drop protocol shared with vectors.
    pub value: HewVecElemLayout,
    /// Hash the borrowed key value.
    pub hash_fn: Option<HewMapKeyHashThunk>,
    /// Compare two borrowed key values.
    pub eq_fn: Option<HewMapKeyEqThunk>,
}

// ---------------------------------------------------------------------------
// Static layout descriptor symbols.
// ---------------------------------------------------------------------------
//
// `hew-runtime/src/layout_intrinsics.rs` defines `#[no_mangle] pub static`
// instances of `HewMapKeyLayout` / `HewVecElemLayout` for the supported
// types (`i32, i64, u32, u64, f32, f64, bool, char, string, bytes, unit`).
// Re-declaring them here as `extern "C"` statics lets codegen-rs and other
// back-ends take the address of the descriptor
// through the cabi surface without depending on hew-runtime directly.
//
// These are checker-visible artifacts and are linked by the
// `resolved_call_kernel_symbols` integration test. Keep the declarations in
// sync with the runtime definitions so missing symbols fail at link time.
//
// Float descriptors ship with `hash_fn = None` / `eq_fn = None`; callers must
// reject hashing or equality when those operations are unavailable.
//
// **WASM parity (#1820):** the layout-backed HashMap/HashSet path is supported
// on wasm32-wasip1. These descriptors are pure data, and codegen may take their
// addresses for wasm modules linked against the wasm runtime archive.

extern "C" {
    // ---- HewMapKeyLayout descriptors ----
    pub static hew_layout_key_i32: HewMapKeyLayout;
    pub static hew_layout_key_i64: HewMapKeyLayout;
    pub static hew_layout_key_u32: HewMapKeyLayout;
    pub static hew_layout_key_u64: HewMapKeyLayout;
    /// Fail-closed: `hash_fn = None` / `eq_fn = None` (DI-003).
    pub static hew_layout_key_f32: HewMapKeyLayout;
    /// Fail-closed: `hash_fn = None` / `eq_fn = None` (DI-003).
    pub static hew_layout_key_f64: HewMapKeyLayout;
    pub static hew_layout_key_bool: HewMapKeyLayout;
    pub static hew_layout_key_char: HewMapKeyLayout;
    pub static hew_layout_key_string: HewMapKeyLayout;
    pub static hew_layout_key_bytes: HewMapKeyLayout;

    // ---- HewVecElemLayout descriptors ----
    pub static hew_layout_val_i32: HewVecElemLayout;
    pub static hew_layout_val_i64: HewVecElemLayout;
    pub static hew_layout_val_u32: HewVecElemLayout;
    pub static hew_layout_val_u64: HewVecElemLayout;
    pub static hew_layout_val_f32: HewVecElemLayout;
    pub static hew_layout_val_f64: HewVecElemLayout;
    pub static hew_layout_val_bool: HewVecElemLayout;
    pub static hew_layout_val_char: HewVecElemLayout;
    pub static hew_layout_val_string: HewVecElemLayout;
    pub static hew_layout_val_bytes: HewVecElemLayout;
    /// Zero-size value descriptor for the `HashSet<T>` = `HashMap<T, ()>`
    /// pattern. `size = 0, align = 1` (the kernel admits ZST V only at
    /// `align == 1` — see `hew-runtime/src/hashmap.rs:980-983`).
    pub static hew_layout_val_unit: HewVecElemLayout;
}

/// All key-layout descriptor symbols exported by `hew-runtime`.
///
/// Stable order; consumed by `resolved_call_kernel_symbols.rs` and the
/// stdlib-catalog coverage gate to enumerate the expected ABI surface.
pub const KEY_LAYOUT_DESCRIPTOR_SYMBOLS: &[&str] = &[
    "hew_layout_key_i32",
    "hew_layout_key_i64",
    "hew_layout_key_u32",
    "hew_layout_key_u64",
    "hew_layout_key_f32",
    "hew_layout_key_f64",
    "hew_layout_key_bool",
    "hew_layout_key_char",
    "hew_layout_key_string",
    "hew_layout_key_bytes",
];

/// All value-layout descriptor symbols exported by `hew-runtime`.
pub const VAL_LAYOUT_DESCRIPTOR_SYMBOLS: &[&str] = &[
    "hew_layout_val_i32",
    "hew_layout_val_i64",
    "hew_layout_val_u32",
    "hew_layout_val_u64",
    "hew_layout_val_f32",
    "hew_layout_val_f64",
    "hew_layout_val_bool",
    "hew_layout_val_char",
    "hew_layout_val_string",
    "hew_layout_val_bytes",
    "hew_layout_val_unit",
];

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

    #[test]
    fn option_drop_thunk_has_same_size_as_raw_fn_ptr() {
        assert_eq!(
            size_of::<Option<HewVecElemDropThunk>>(),
            size_of::<HewVecElemDropThunk>(),
            "Option<HewVecElemDropThunk> must have the same size as HewVecElemDropThunk \
             (fn-pointer niche optimisation)",
        );
    }

    #[test]
    fn option_clone_thunk_has_same_size_as_raw_fn_ptr() {
        assert_eq!(
            size_of::<Option<HewVecElemCloneThunk>>(),
            size_of::<HewVecElemCloneThunk>(),
            "Option<HewVecElemCloneThunk> must have the same size as HewVecElemCloneThunk \
             (fn-pointer niche optimisation)",
        );
    }

    #[test]
    fn hashmap_key_layout_embeds_the_shared_value_protocol() {
        assert_eq!(core::mem::offset_of!(HewMapKeyLayout, value), 0);
        assert_eq!(
            core::mem::offset_of!(HewMapKeyLayout, hash_fn),
            size_of::<HewVecElemLayout>(),
        );
        assert_eq!(
            core::mem::offset_of!(HewMapKeyLayout, eq_fn),
            size_of::<HewVecElemLayout>() + size_of::<HewMapKeyHashThunk>(),
        );
        assert_eq!(size_of::<HewMapKeyLayout>(), 7 * size_of::<usize>());
        assert_eq!(
            align_of::<HewMapKeyLayout>(),
            align_of::<HewVecElemLayout>()
        );
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
}
