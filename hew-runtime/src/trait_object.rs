//! Runtime ABI types for `dyn Trait` (trait-object) values.
//!
//! This module defines the substrate that polymorphic trait dispatch
//! lowers against. It is intentionally narrow: two `#[repr(C)]` layout
//! types and one diagnostic helper. The actual dispatch sequence
//! (GEP into the vtable, load the slot, indirect-call) is emitted
//! inline by codegen — see the lane plan `runtime-trait-object-abi.md`
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
