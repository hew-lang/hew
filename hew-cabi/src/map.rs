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
//! # Acquisition / ownership contract for owned K and V (W4.001 Stage C0a;
//! plan rev6 §4 "Acquisition / ownership contract")
//!
//! The kernel implements the following responsibilities verbatim. Codegen
//! materialisers (Stage C HIR consumer) are bound by the *caller* side.
//!
//! | Kernel entry                              | K argument              | V argument              | Kernel responsibility                                                                                                                              |
//! |-------------------------------------------|-------------------------|-------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|
//! | `hew_hashmap_insert_layout` (vacant slot) | **owned** (transferred) | **owned** (transferred) | Raw-copy K and V bytes into the slot; possession transfers to the map. No clone, no drop.                                                          |
//! | `hew_hashmap_insert_layout` (occupied)    | **owned** (eq to stored K — caller drops/recycles the duplicate `K_in`) | **owned** (transferred) | Drop the **old V** via `val_layout.drop_fn`; raw-copy new V into the slot. The slot K stays in place; the kernel **never** drops the stored K here. |
//! | `hew_hashmap_get_layout`                  | **borrowed**            | n/a                     | Returns `*const c_void` pointing into the slot; lifetime valid until the next mutation. Caller must not free.                                       |
//! | `hew_hashmap_remove_layout`               | **borrowed**            | n/a                     | Invoke `key_layout.drop_fn` on the slot K and `val_layout.drop_fn` on the slot V before tombstoning. Caller's lookup K is untouched.                |
//! | `hew_hashmap_free_layout`                 | n/a                     | n/a                     | Iterate occupied slots, invoke key + value `drop_fn` on each, then deallocate the entries buffer. Tombstoned slots already had their blobs dropped at remove-time. |
//!
//! **Invariants (P0):**
//!
//! 1. Insert transfers ownership of K and V into the map (vacant) or of V
//!    only (occupied — the slot K is reused).
//! 2. Get borrows; no `clone_fn` is invoked on the standard `_get_layout`.
//! 3. Remove drops stored K + V via the descriptor thunks; the lookup K
//!    is borrowed and never dropped by the kernel.
//! 4. Free drops every occupied K + V exactly once; tombstoned slots are
//!    not re-dropped.
//! 5. **Overwrite never drops K, always drops old V.** The duplicate K
//!    the caller passed in is the caller's responsibility to drop or
//!    recycle, hoisted to the HIR consumer per plan §4.
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
// Drop and clone thunk type aliases (W4.001 Stage C0a)
// ---------------------------------------------------------------------------

/// Thunk that drops an owned key or value blob in place.
///
/// - `blob` — non-null mutable pointer to the owned blob. The thunk runs the
///   type's drop plan (e.g. `free` of an inner C string pointer, drop of inner
///   Layout-managed sub-records) but **does not** deallocate the slot bytes
///   themselves — the kernel owns the slot storage and either reuses or
///   deallocates it.
/// - The thunk is invoked exactly once per dropped slot blob, in the kernel's
///   three drop contexts: (a) insert-overwrite (V only — see contract table),
///   (b) remove (K + V), and (c) free (K + V on every OCCUPIED slot).
///
/// `extern "C" fn(*mut u8)` (Q281=A discipline: raw extern C function
/// pointer, never a closure or trait object — round-trips through the
/// `#[repr(C)]` descriptor cleanly).
pub type HewMapValueDropThunk = extern "C" fn(blob: *mut c_void);

/// Thunk that clones an owned value blob from `src` to `dst`.
///
/// Optional — never invoked by the standard `hew_hashmap_get_layout` path
/// (which borrows). Consulted only by an opt-in cloning-get variant when the
/// HIR consumer requires owned-V return.
///
/// `extern "C" fn(*const u8, *mut u8)` (Q281=A discipline).
pub type HewMapValueCloneThunk = extern "C" fn(src: *const c_void, dst: *mut c_void);

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
const _: () = assert!(
    size_of::<Option<HewMapValueDropThunk>>() == size_of::<HewMapValueDropThunk>(),
    "Option<HewMapValueDropThunk> must be niche-optimised to the same size as HewMapValueDropThunk",
);
const _: () = assert!(
    size_of::<Option<HewMapValueCloneThunk>>() == size_of::<HewMapValueCloneThunk>(),
    "Option<HewMapValueCloneThunk> must be niche-optimised to the same size as HewMapValueCloneThunk",
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
///     size_t                 size;
///     size_t                 align;
///     HewTypeOwnershipKind   ownership_kind;
///     /* padding to pointer alignment */
///     HewMapKeyHashThunk     hash_fn;   /* may be NULL */
///     HewMapKeyEqThunk       eq_fn;     /* may be NULL */
///     HewMapValueDropThunk   drop_fn;   /* may be NULL when ownership_kind == Plain */
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
    /// Drop thunk invoked on stored K at remove + free (see ownership
    /// contract above). `None` is valid only for `ownership_kind == Plain`;
    /// `String` and `LayoutManaged` require `Some(_)` (fail-closed
    /// constructor check in `hew_hashmap_new_with_layout`). W4.001 Stage C0a.
    pub drop_fn: Option<HewMapValueDropThunk>,
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
///     size_t                 size;
///     size_t                 align;
///     HewTypeOwnershipKind   ownership_kind;
///     /* padding to pointer alignment */
///     HewMapValueDropThunk   drop_fn;   /* may be NULL when ownership_kind == Plain */
///     HewMapValueCloneThunk  clone_fn;  /* may be NULL — never required by _get_layout */
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
    /// Drop thunk invoked on stored V at insert-overwrite + remove + free.
    /// `None` is valid only for `ownership_kind == Plain`; `String` and
    /// `LayoutManaged` require `Some(_)` (fail-closed constructor check).
    /// W4.001 Stage C0a.
    pub drop_fn: Option<HewMapValueDropThunk>,
    /// Optional clone thunk consulted only by the opt-in cloning-get
    /// variant; the standard `hew_hashmap_get_layout` ignores it.
    /// W4.001 Stage C0a.
    pub clone_fn: Option<HewMapValueCloneThunk>,
}

// ---------------------------------------------------------------------------
// Static layout descriptor symbols (W4.001 Stage C0b)
// ---------------------------------------------------------------------------
//
// `hew-runtime/src/layout_intrinsics.rs` defines `#[no_mangle] pub static`
// instances of `HewMapKeyLayout` / `HewMapValueLayout` for the C0a-scoped
// types (`i32, i64, u32, u64, f32, f64, bool, char, string, bytes, unit`).
// Re-declaring them here as `extern "C"` statics lets codegen-rs (Stage C
// consumer) and parallel back-ends take the address of the descriptor
// through the cabi surface without depending on hew-runtime directly.
//
// **C0b boundary:** these are checker-visible artifacts only. The first
// production reader is Stage C's `HashMapLoweringFact` materialiser. No
// consumer in C0b references these symbols at compile time, so a missing
// link would not surface until Stage C — the `resolved_call_kernel_symbols`
// integration test in `hew-types/tests/` is the C0b-time linkage gate.
//
// **Float K descriptors** ship with `hash_fn = None` / `eq_fn = None`;
// belt-and-suspenders DI-003 fail-closed-by-absence per plan §4 Stage C0b.
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

    // ---- HewMapValueLayout descriptors ----
    pub static hew_layout_val_i32: HewMapValueLayout;
    pub static hew_layout_val_i64: HewMapValueLayout;
    pub static hew_layout_val_u32: HewMapValueLayout;
    pub static hew_layout_val_u64: HewMapValueLayout;
    pub static hew_layout_val_f32: HewMapValueLayout;
    pub static hew_layout_val_f64: HewMapValueLayout;
    pub static hew_layout_val_bool: HewMapValueLayout;
    pub static hew_layout_val_char: HewMapValueLayout;
    pub static hew_layout_val_string: HewMapValueLayout;
    pub static hew_layout_val_bytes: HewMapValueLayout;
    /// Zero-size value descriptor for the `HashSet<T>` = `HashMap<T, ()>`
    /// pattern. `size = 0, align = 1` (the kernel admits ZST V only at
    /// `align == 1` — see `hew-runtime/src/hashmap.rs:980-983`).
    pub static hew_layout_val_unit: HewMapValueLayout;
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
            size_of::<Option<HewMapValueDropThunk>>(),
            size_of::<HewMapValueDropThunk>(),
            "Option<HewMapValueDropThunk> must have the same size as HewMapValueDropThunk \
             (fn-pointer niche optimisation)",
        );
    }

    #[test]
    fn option_clone_thunk_has_same_size_as_raw_fn_ptr() {
        assert_eq!(
            size_of::<Option<HewMapValueCloneThunk>>(),
            size_of::<HewMapValueCloneThunk>(),
            "Option<HewMapValueCloneThunk> must have the same size as HewMapValueCloneThunk \
             (fn-pointer niche optimisation)",
        );
    }

    // -- HewMapKeyLayout repr --

    #[test]
    fn hashmap_key_layout_has_correct_repr() {
        // Layout on 64-bit (x86_64 / aarch64) — W4.001 Stage C0a:
        //   size:           usize  = 8
        //   align:          usize  = 8
        //   ownership_kind: u8     = 1  (+7 padding to reach pointer alignment)
        //   hash_fn:        fn ptr = 8  (Option<fn> via niche opt)
        //   eq_fn:          fn ptr = 8
        //   drop_fn:        fn ptr = 8  (new — Stage C0a)
        //   total = 8 + 8 + (1 + 7 pad) + 8 + 8 + 8 = 48 bytes; align = 8.
        assert_eq!(
            size_of::<HewMapKeyLayout>(),
            6 * size_of::<usize>(),
            "HewMapKeyLayout must be exactly 6 pointer-sized slots (48 bytes on 64-bit) post-C0a",
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
            drop_fn: None,
        };
        assert_eq!(layout.size, 16);
        assert_eq!(layout.align, 8);
        assert_eq!(layout.ownership_kind, HewTypeOwnershipKind::Plain);
        assert!(layout.hash_fn.is_some());
        assert!(layout.eq_fn.is_some());
        assert!(layout.drop_fn.is_none());
    }

    // -- HewMapValueLayout repr --

    #[test]
    fn hashmap_value_layout_has_correct_repr() {
        // Layout on 64-bit (x86_64 / aarch64) — W4.001 Stage C0a:
        //   size:           usize = 8
        //   align:          usize = 8
        //   ownership_kind: u8    = 1  (+7 padding to reach pointer alignment)
        //   drop_fn:        fn ptr = 8 (new — Stage C0a)
        //   clone_fn:       fn ptr = 8 (new — Stage C0a)
        //   total = 8 + 8 + (1 + 7 pad) + 8 + 8 = 40 bytes; align = 8.
        assert_eq!(
            size_of::<HewMapValueLayout>(),
            5 * size_of::<usize>(),
            "HewMapValueLayout must be exactly 5 pointer-sized slots (40 bytes on 64-bit) post-C0a",
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
            drop_fn: None,
            clone_fn: None,
        };
        assert_eq!(layout.size, 24);
        assert_eq!(layout.align, 8);
        assert_eq!(layout.ownership_kind, HewTypeOwnershipKind::Plain);
        assert!(layout.drop_fn.is_none());
        assert!(layout.clone_fn.is_none());
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
            drop_fn: None,
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
            drop_fn: None,
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
            drop_fn: None,
            clone_fn: None,
        };
        let dbg = format!("{layout:?}");
        assert!(
            dbg.contains("HewMapValueLayout"),
            "Debug must include type name",
        );
        assert!(dbg.contains("Plain"), "Debug must include ownership_kind");
    }
}
