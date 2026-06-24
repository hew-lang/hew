//! Runtime ABI types for `dyn Trait` (trait-object) values.
//!
//! This module defines the substrate that polymorphic trait dispatch
//! lowers against. It is intentionally narrow: two `#[repr(C)]` layout
//! types and one diagnostic helper. The actual dispatch sequence
//! (GEP into the vtable, load the slot, indirect-call) is emitted
//! inline by codegen — see `runtime-trait-object-abi.md`
//! design section **D-4**, which explicitly rejects routing dispatch
//! through a runtime helper. Routing through a runtime helper would
//! force argument boxing and break the producer-bridge invariant
//! (the runtime would not know the trait's signature).
//!
//! # Layout
//!
//! - [`HewTraitObject`] is a two-word fat pointer: a data pointer
//!   plus a vtable pointer. Two-word `#[repr(C)]` structs are
//!   returned by-value in two integer registers on every Hew
//!   target ABI (`x86_64` `SysV` `rax`/`rdx`, `AAPCS64` `x0`/`x1`,
//!   wasm32 multivalue). This composes cleanly with the existing
//!   two-word `ChildLookupResult` shape that already crosses the
//!   FFI boundary today.
//!
//! - [`HewVtable`] models the **prefix triple** every vtable carries:
//!   `drop_in_place`, `size_of`, `align_of`. Codegen-emitted vtable
//!   statics widen this struct with one function-pointer slot per
//!   trait method. Slot indices (per design D-2):
//!
//!   | Slot | Field            | Notes                                  |
//!   |------|------------------|----------------------------------------|
//!   | 0    | `drop_in_place`  | `unsafe extern "C" fn(*mut u8)`        |
//!   | 1    | `size_of`        | `usize` (data, not function)           |
//!   | 2    | `align_of`       | `usize` (data, not function)           |
//!   | 3..N | trait methods    | source-declaration order               |
//!
//! # Out of scope for TO-1
//!
//! TO-1 lands only the runtime types and the diagnostic OOB-panic
//! helper. No vtables for actual user traits are emitted here — that
//! is TO-4 (codegen emission). No checker or MIR changes — TO-2/TO-3.

use std::alloc::{self, Layout};
use std::ffi::c_void;

/// Fat-pointer representation of an owned `dyn Trait` value.
///
/// # Fields
///
/// - `data`: a `*mut u8` pointing at the underlying value. Mutable
///   because Hew uses move semantics and trait methods may operate
///   on the value's storage. Lifetime/ownership is the caller's
///   responsibility — the trait object's drop slot (`vtable[0]`)
///   is invoked by the elaborated drop pass at scope exit, after
///   which the storage is freed.
///
/// - `vtable`: a `*const HewVtable` pointing at a per-`(Trait, ImplType)`
///   constant emitted by codegen. The pointed-to layout is the
///   prefix triple followed by one function-pointer slot per
///   trait method, indexed past the prefix.
///
/// # ABI
///
/// `#[repr(C)]` with two pointer-width fields. On every target
/// the Hew runtime builds for:
///
/// | Target          | Size (bytes) | Return-by-value convention |
/// |-----------------|--------------|----------------------------|
/// | `x86_64` `SysV`     | 16           | `rax`/`rdx` (two eightbytes) |
/// | aarch64 `AAPCS64` | 16           | `x0`/`x1`                  |
/// | wasm32          | 16           | multivalue / sret pair     |
///
/// A static assertion below pins the size at compile time.
#[repr(C)]
#[derive(Debug)]
pub struct HewTraitObject {
    /// Pointer to the underlying value. Caller-owned storage.
    pub data: *mut u8,
    /// Pointer to the per-`(Trait, ImplType)` vtable constant.
    /// Must be non-null at every well-formed call site; null is
    /// reserved for fail-closed diagnostics and is treated as a
    /// program bug rather than a recoverable condition.
    pub vtable: *const HewVtable,
}

// SAFETY: `HewTraitObject` is a raw pair of pointers. It is `Send` iff
// every concrete impl that lowers into it agrees its data is `Send`. The
// per-impl check is the checker's responsibility (Risks §5 in the lane
// plan: actor-message types reject `dyn Trait` outright in v0.5). The
// FFI surface itself imposes no thread-safety claim on the wire.

/// Vtable prefix triple shared by every trait-object vtable.
///
/// The on-disk LLVM `private constant` emitted by codegen widens this
/// struct with one function-pointer slot per trait method (per design
/// D-2). The Rust-side model only covers the prefix because the
/// per-trait widening is determined at codegen time, not at runtime.
///
/// # Slot indexing
///
/// Codegen's GEP sequence treats the vtable as an array of word-sized
/// slots where:
///
/// - Slot 0 is `drop_in_place` (the only function pointer in the
///   prefix).
/// - Slots 1 and 2 are `size_of` and `align_of` (data, not functions).
///   These exist so a future `Sized`-erasing alloc/free path has
///   the layout it needs without re-deriving it from the type
///   identity.
/// - Slots 3..N are the trait's methods, in source-declaration order.
#[repr(C)]
#[derive(Debug)]
pub struct HewVtable {
    /// Drop-in-place function — invoked by drop elaboration at the
    /// owning scope's exit. Must run user-defined drop code (if any)
    /// without freeing the storage; storage release is the caller's
    /// responsibility after this function returns.
    ///
    /// # Safety
    ///
    /// `data` must point at a live value of the impl type the
    /// vtable was emitted for. After this function returns the
    /// pointed-to value is invalid for further use; only its
    /// backing storage may be freed.
    pub drop_in_place: unsafe extern "C" fn(data: *mut u8),
    /// `size_of::<ImplType>()` — the byte size of the value the
    /// vtable was emitted for. Carried in the prefix so a
    /// future allocator-aware drop path can free the storage
    /// without consulting the type identity.
    pub size_of: usize,
    /// `align_of::<ImplType>()` — companion to `size_of`.
    pub align_of: usize,
}

// SAFETY checks: the fat pointer must be exactly two pointer-widths.
// Codegen depends on this for the construction (`insertvalue` pair)
// and the call sequence (GEP through `(*const HewVtable)`). Drift
// here breaks every dispatch site at the same time, so the static
// assertion is the cheapest possible defense.
const _: () = assert!(
    std::mem::size_of::<HewTraitObject>() == 2 * std::mem::size_of::<*const c_void>(),
    "HewTraitObject must be exactly two pointer-widths for codegen's \
     insertvalue/extractvalue pair to be ABI-correct."
);
const _: () = assert!(
    std::mem::align_of::<HewTraitObject>() == std::mem::align_of::<*const c_void>(),
    "HewTraitObject must align to pointer width."
);

// Vtable layout pins. The prefix triple is one function pointer plus
// two `usize` words. On every supported target that is three pointer
// widths. Codegen indexes past this prefix when computing method
// slots, so any drift here renumbers every method slot.
const _: () = assert!(
    std::mem::size_of::<HewVtable>() == 3 * std::mem::size_of::<*const c_void>(),
    "HewVtable prefix must be exactly three pointer-widths \
     (drop_in_place, size_of, align_of)."
);

/// Build the panic message that [`hew_vtable_dispatch_panic_on_oob`]
/// emits. Factored out so unit tests can exercise the message
/// shape directly — `panic = "abort"` is the workspace profile,
/// so `std::panic::catch_unwind` cannot observe a panic from the
/// extern-C entry. Codegen never calls this helper; only the
/// extern-C wrapper below is wire-visible.
#[must_use]
pub fn vtable_dispatch_oob_message(slot: u32, max: u32) -> String {
    format!(
        "hew_vtable_dispatch_panic_on_oob: slot {slot} is out of range \
         for a vtable with {max} method slot(s) (valid: 0..{max}). \
         This indicates a codegen or producer bug — every reachable \
         dispatch site must name a slot the trait declares."
    )
}

/// Diagnostic helper: trap with a slot-out-of-range message.
///
/// Codegen emits this on the unreachable arm of a vtable-slot match
/// when the static slot index falls outside `[0, max)` for the
/// trait at hand. It is also the wire target codegen uses to fail
/// closed on a null vtable pointer — both conditions indicate a
/// codegen or producer bug rather than a recoverable runtime
/// condition, so the helper diverges (`-> !`).
///
/// Registered in `hew-mir/src/runtime_symbols.rs`'s allowlist and in
/// `scripts/jit-symbol-classification.toml`'s `stable` list, per
/// LESSONS row P0 `boundary-fail-closed`: every codegen-named
/// symbol must be declared by the runtime ahead of any emission.
///
/// # Parameters
///
/// - `slot`: the offending slot index that codegen tried to dispatch.
/// - `max`: the trait's known method-slot count (slots `3..3+N`),
///   passed through so the panic message names the trait's
///   advertised width rather than a magic number.
///
/// # Panics
///
/// Always — the function is diverging (`-> !`). Calls `panic!`
/// with the message built by [`vtable_dispatch_oob_message`].
/// Under the workspace's `panic = "abort"` profile this terminates
/// the process; under `panic = "unwind"` (not configured today) it
/// would unwind. Both are acceptable termination modes for an
/// unreachable arm; the caller has no recovery path either way.
#[no_mangle]
pub extern "C" fn hew_vtable_dispatch_panic_on_oob(slot: u32, max: u32) -> ! {
    // The format! happens on the Rust side of the FFI boundary so
    // the producer (codegen) does not have to materialise the
    // message itself.
    panic!("{}", vtable_dispatch_oob_message(slot, max));
}

// ---------------------------------------------------------------------------
// Heap-box ABI for return-by-value `dyn Trait` values (W3.031 Stage 0)
// ---------------------------------------------------------------------------
//
// §1.7.3 of the design: a function returning `dyn Trait` cannot point its
// fat-pointer's `data` word at callee-frame storage (use-after-free at
// the call boundary). The v0.5 design heap-allocates a buffer sized
// from the concrete type's `(size_of, align_of)` (the vtable's prefix
// slots 1 and 2), memcpy's the concrete value into it, and returns
// `(heap_ptr, vtable_ptr)`. The receiving site's `DropKind::TraitObject`
// elaboration runs slot-0 (drop_in_place) on the value, then calls
// `hew_dyn_box_free` with the same `(size, align)` triple to release
// the buffer.
//
// Two ABI symbols, mirrored in `hew-mir/src/runtime_symbols.rs`'s
// allowlist and `scripts/jit-symbol-classification.toml`'s `stable`
// list — same wire path as `hew_vtable_dispatch_panic_on_oob`.
//
// FAIL-CLOSED DISCIPLINE
//
// Both entries treat invalid inputs as program bugs, not recoverable
// runtime conditions:
//
// - `align == 0` is a codegen invariant violation (the vtable's
//   `align_of` is always sourced from `core::mem::align_of::<T>()`
//   which is never zero). Both entries panic.
// - A `Layout::from_size_align` rejection (alignment not a power of
//   two, or `size` overflows `isize::MAX` rounded to alignment) is
//   the same kind of violation. Both entries panic with the
//   `LayoutError` displayed.
// - On alloc failure the allocator's `handle_alloc_error` runs —
//   the workspace's `panic = "abort"` profile turns this into a
//   process abort, which is the documented fail-closed mode for an
//   OOM crossing a substrate ABI boundary.
// - A null `ptr` passed to `free` is a program bug (the alloc entry
//   never returns null for a well-formed layout) and panics.
//
// SIZE-ZERO CONVENTION
//
// `Layout::from_size_align(0, align)` is well-formed and yields a
// zero-sized layout. The Rust allocator contract permits `alloc` to
// return a non-null but possibly-dangling pointer for such layouts;
// the conventional choice is to skip the allocator entirely and
// return the alignment itself cast to a pointer (`align as *mut u8`),
// matching `NonNull::<T>::dangling()` for ZSTs. `free` mirrors the
// convention: a zero-`size` call is a no-op. A `dyn Trait` whose
// concrete type is a ZST is rare but legal (e.g. unit-struct impls);
// failing closed here would convert that legality into a runtime
// trap, so we honour the layout rules instead.

/// Validate `(size, align)` against `Layout::from_size_align` and return
/// the resulting `Layout` on success.
///
/// Factored out so the validation contract is directly unit-testable
/// without depending on `panic = "abort"` behaviour from the FFI
/// entries. The order matters: `Layout::from_size_align` is called
/// **before** any `size == 0` short-circuit so that invalid ZST
/// layouts (e.g. `(size=0, align=3)`) still fail closed. Both
/// `hew_dyn_box_alloc` and `hew_dyn_box_free` route through this
/// helper before honouring the ZST sentinel/no-op convention.
fn dyn_box_layout(size: usize, align: usize) -> Result<Layout, std::alloc::LayoutError> {
    // `Layout::from_size_align` rejects `align == 0`, non-power-of-two
    // alignments, and `size` values that would overflow `isize::MAX`
    // when rounded up to alignment. We deliberately do **not** filter
    // any input before this call — the contract is "every invalid
    // layout traps", including invalid ZST layouts.
    Layout::from_size_align(size, align)
}

/// Allocate a heap buffer of `size` bytes with `align` alignment to back
/// a returned `dyn Trait` value.
///
/// See the module-level documentation above for the fail-closed and
/// size-zero conventions. Codegen sources `size` and `align` from the
/// vtable's prefix slots 1 and 2 at the return-bridge call site.
///
/// # Parameters
///
/// - `size`: byte size of the concrete value the heap buffer will
///   hold. Sourced from the vtable's `size_of` slot.
/// - `align`: alignment of the concrete value. Sourced from the
///   vtable's `align_of` slot. Must be a non-zero power of two.
///
/// # Returns
///
/// A non-null pointer to a buffer of at least `size` bytes aligned to
/// `align`. The buffer is uninitialised; the caller is responsible for
/// memcpy'ing the concrete value into it before constructing the
/// fat pointer.
///
/// # Panics
///
/// - `Layout::from_size_align(size, align)` rejects the pair (this
///   covers `align == 0`, non-power-of-two alignments, and `size`
///   overflowing `isize::MAX` when rounded up to alignment — and is
///   checked **before** the `size == 0` short-circuit so invalid
///   ZST layouts trap rather than returning a sentinel).
/// - The allocator returns null for a non-zero-sized layout
///   (`handle_alloc_error` traps).
///
/// # Safety
///
/// The function itself is safe to call from any thread. The returned
/// pointer must be released exactly once via [`hew_dyn_box_free`] with
/// the same `(size, align)` pair; mismatched layouts are undefined
/// behaviour per the global allocator contract.
#[no_mangle]
pub unsafe extern "C" fn hew_dyn_box_alloc(size: usize, align: usize) -> *mut u8 {
    let layout = match dyn_box_layout(size, align) {
        Ok(layout) => layout,
        Err(err) => panic!(
            "hew_dyn_box_alloc: invalid layout (size={size}, align={align}): {err}. \
             Codegen must source size/align from the vtable's prefix slots, which \
             are core::mem::size_of/align_of of a real concrete type."
        ),
    };
    if size == 0 {
        // ZST convention — only reached after `dyn_box_layout`
        // accepted `(0, align)`. Return a non-null but dangling
        // pointer matching `NonNull::<T>::dangling()`. The caller
        // must not dereference it for reads/writes of non-zero size;
        // the matching `free` call is a no-op.
        return align as *mut u8;
    }
    // SAFETY: `layout` has non-zero `size` (checked above) and a
    // well-formed alignment (validated by `Layout::from_size_align`).
    // `std::alloc::alloc` is sound for any such layout.
    let ptr = unsafe { alloc::alloc(layout) };
    if ptr.is_null() {
        // `handle_alloc_error` is the documented fail-closed path for
        // OOM crossing the global allocator. Under the workspace's
        // `panic = "abort"` profile this aborts the process.
        alloc::handle_alloc_error(layout);
    }
    ptr
}

/// Release a heap buffer previously returned by [`hew_dyn_box_alloc`].
///
/// See the module-level documentation for the fail-closed and
/// size-zero conventions. Codegen emits this call as the second half
/// of the `DropKind::TraitObject { storage: HeapBoxed }` ritual — after
/// vtable slot 0 (`drop_in_place`) has run on the value but before the
/// fat pointer's storage goes out of scope.
///
/// # Parameters
///
/// - `ptr`: the pointer returned by the matching `hew_dyn_box_alloc`.
///   Must be non-null and must not have been freed already.
/// - `size`: must equal the `size` passed to the matching alloc.
///   Sourced from the vtable's `size_of` slot at the drop site.
/// - `align`: must equal the `align` passed to the matching alloc.
///   Sourced from the vtable's `align_of` slot at the drop site.
///
/// # Panics
///
/// - `Layout::from_size_align(size, align)` rejects the pair (checked
///   **before** the `size == 0` short-circuit so invalid ZST layouts
///   like `(size=0, align=3)` trap rather than no-op'ing).
/// - `ptr.is_null()` for a non-zero `size` — alloc never returns null
///   for a well-formed layout, so a null here indicates a producer
///   double-free or a stale fat pointer.
///
/// # Safety
///
/// The `(ptr, size, align)` triple must be exactly the triple a prior
/// `hew_dyn_box_alloc` returned — mismatched layouts are undefined
/// behaviour per the global allocator contract.
#[no_mangle]
pub unsafe extern "C" fn hew_dyn_box_free(ptr: *mut u8, size: usize, align: usize) {
    let layout = match dyn_box_layout(size, align) {
        Ok(layout) => layout,
        Err(err) => panic!(
            "hew_dyn_box_free: invalid layout (size={size}, align={align}): {err}. \
             Must match the layout the corresponding hew_dyn_box_alloc was \
             called with."
        ),
    };
    if size == 0 {
        // Matching ZST convention with `hew_dyn_box_alloc`: zero-size
        // allocs were never routed through the allocator, so freeing
        // them is a no-op. Only reached after `dyn_box_layout`
        // accepted `(0, align)` — invalid ZST layouts trap above.
        return;
    }
    assert!(
        !ptr.is_null(),
        "hew_dyn_box_free: ptr must be non-null for non-zero size \
         (got size={size}, align={align}). A null pointer here indicates \
         a producer double-free or a stale fat pointer reaching drop \
         elaboration."
    );
    // SAFETY: per the function's safety contract the caller guarantees
    // `ptr` came from `hew_dyn_box_alloc` with the same `(size, align)`
    // triple. `Layout` round-trips identically because both entries
    // reconstruct it from the same inputs.
    unsafe { alloc::dealloc(ptr, layout) };
}

#[cfg(test)]
mod tests {
    //! Unit tests for the layout-validation helper.
    //!
    //! The FFI entries themselves panic on bad input, and the workspace
    //! profile is `panic = "abort"`, which neither `catch_unwind` nor
    //! `#[should_panic]` can observe. The validation contract is
    //! therefore exercised through `dyn_box_layout`, whose `Result`
    //! return is directly testable.

    use super::dyn_box_layout;

    #[test]
    fn dyn_box_layout_accepts_well_formed_pairs() {
        // Representative concrete-type layouts codegen will emit.
        for &(size, align) in &[(0_usize, 1_usize), (4, 4), (8, 8), (16, 8), (64, 64)] {
            let layout = dyn_box_layout(size, align)
                .unwrap_or_else(|err| panic!("({size}, {align}) must be accepted: {err}"));
            assert_eq!(layout.size(), size);
            assert_eq!(layout.align(), align);
        }
    }

    #[test]
    fn dyn_box_layout_rejects_zero_align_regardless_of_size() {
        // Zero alignment is a codegen invariant violation. It must be
        // rejected for both non-zero and zero `size` — the ZST
        // short-circuit in the FFI entries must not run before this
        // check.
        for size in [0_usize, 1, 4, 8, 64] {
            assert!(
                dyn_box_layout(size, 0).is_err(),
                "(size={size}, align=0) must be rejected by dyn_box_layout"
            );
        }
    }

    #[test]
    fn dyn_box_layout_rejects_non_power_of_two_align_for_zst() {
        // Pin the blocking-review regression: a zero-size pair with a
        // non-power-of-two alignment (e.g. align=3) is an invalid
        // layout and must fail closed, NOT take the ZST sentinel /
        // no-op fast path. Pre-revision this case slipped past the
        // `size == 0` short-circuit in both alloc and free.
        for &align in &[3_usize, 5, 6, 7, 9, 10, 12] {
            assert!(
                dyn_box_layout(0, align).is_err(),
                "(size=0, align={align}) must be rejected — align is not a power of two"
            );
        }
    }

    #[test]
    fn dyn_box_layout_rejects_non_power_of_two_align_for_non_zero_size() {
        for &(size, align) in &[(4_usize, 3_usize), (8, 5), (16, 6)] {
            assert!(
                dyn_box_layout(size, align).is_err(),
                "(size={size}, align={align}) must be rejected — align is not a power of two"
            );
        }
    }

    #[test]
    fn dyn_box_layout_rejects_size_overflowing_isize_max_when_rounded() {
        // `Layout::from_size_align` rejects sizes that, rounded up to
        // alignment, exceed `isize::MAX`. Pick a size just past the
        // boundary at the largest power-of-two alignment we use.
        let bad_size = (isize::MAX as usize) + 1;
        assert!(
            dyn_box_layout(bad_size, 8).is_err(),
            "size > isize::MAX must be rejected"
        );
    }
}
