//! TO-1 acceptance tests for the runtime trait-object ABI substrate.
//!
//! Verifies the wire layout of `HewTraitObject` / `HewVtable`, the
//! drop-slot invocation path codegen will emit, the method-slot
//! call path codegen will emit, and the diagnostic OOB-panic helper.
//!
//! These tests exercise the substrate directly with hand-built
//! vtables — no codegen, no checker, no MIR. Per the lane plan, that
//! cross-component composition lands in TO-3 (MIR) and TO-4
//! (codegen).

#![expect(
    clippy::undocumented_unsafe_blocks,
    reason = "FFI test harness — safety invariants are documented per-test."
)]
#![expect(
    clippy::cast_ptr_alignment,
    reason = "FFI tests model codegen's GEP+load sequence through `*mut u8` — \
              alignment is guaranteed by the test-owned stack storage the \
              pointer was taken from."
)]

use std::ffi::c_void;
use std::mem;
use std::sync::atomic::{AtomicU32, Ordering};

use hew_runtime::trait_object::{
    hew_vtable_dispatch_panic_on_oob, vtable_dispatch_oob_message, HewTraitObject, HewVtable,
};

// ---------------------------------------------------------------------------
// Layout pins
// ---------------------------------------------------------------------------

#[test]
fn hew_trait_object_is_two_pointer_words() {
    // Codegen lowers `ConstructTraitObject` as an `insertvalue` pair
    // into a two-word `{ptr, ptr}` aggregate. If this drifts, every
    // dispatch site is silently misaligned.
    assert_eq!(
        mem::size_of::<HewTraitObject>(),
        2 * mem::size_of::<*const c_void>(),
        "HewTraitObject must be exactly two pointer-widths."
    );
    assert_eq!(
        mem::align_of::<HewTraitObject>(),
        mem::align_of::<*const c_void>(),
        "HewTraitObject must align to pointer width."
    );
    // On every Hew target (x86_64 SysV, aarch64 AAPCS, wasm32) this
    // means 16 bytes — symmetric with `ChildLookupResult`.
    assert_eq!(mem::size_of::<HewTraitObject>(), 16);
}

#[test]
fn hew_vtable_prefix_is_three_pointer_words() {
    // Codegen indexes past the prefix to reach the method slots.
    // Drift here renumbers every emitted method slot in lockstep.
    assert_eq!(
        mem::size_of::<HewVtable>(),
        3 * mem::size_of::<*const c_void>(),
        "HewVtable prefix triple must be three pointer-widths."
    );
}

// ---------------------------------------------------------------------------
// Drop-slot invocation
// ---------------------------------------------------------------------------

/// Global counter the synthetic drop fn bumps. Each test that uses
/// the drop slot picks its own value to write.
static DROP_OBSERVED: AtomicU32 = AtomicU32::new(0);

unsafe extern "C" fn synthetic_drop(data: *mut u8) {
    // The caller's data pointer carries the value the test wants
    // observed in `DROP_OBSERVED`. We read it once and record it.
    let token = unsafe { *data.cast::<u32>() };
    DROP_OBSERVED.store(token, Ordering::SeqCst);
}

unsafe extern "C" fn never_called_drop(_data: *mut u8) {
    panic!("drop slot should not have been invoked in this test");
}

#[test]
fn vtable_drop_slot_runs_when_invoked() {
    // Hand-build a vtable with only the prefix populated. Codegen
    // would widen this with method slots; the prefix triple suffices
    // for the drop path.
    let vt = HewVtable {
        drop_in_place: synthetic_drop,
        size_of: mem::size_of::<u32>(),
        align_of: mem::align_of::<u32>(),
    };
    let mut payload: u32 = 0xDEAD_BEEF;
    let obj = HewTraitObject {
        data: (&raw mut payload).cast::<u8>(),
        vtable: &raw const vt,
    };

    DROP_OBSERVED.store(0, Ordering::SeqCst);
    unsafe {
        let drop_fn = (*obj.vtable).drop_in_place;
        drop_fn(obj.data);
    }

    assert_eq!(
        DROP_OBSERVED.load(Ordering::SeqCst),
        0xDEAD_BEEF,
        "the drop_in_place slot must observe the live data pointer."
    );
}

// ---------------------------------------------------------------------------
// Method-slot invocation
// ---------------------------------------------------------------------------

/// Synthetic trait method: takes a receiver pointer, returns the
/// receiver's value cast to `i64`. Modelled on the signature shape
/// codegen will emit for `fn fmt(val: Self) -> i64` style methods
/// (the `string`-returning real `Display` shape arrives in TO-7).
unsafe extern "C" fn method_return_value(receiver: *mut u8) -> i64 {
    i64::from(unsafe { *receiver.cast::<u32>() })
}

/// Widened vtable shape codegen will emit for a one-method trait.
/// `#[repr(C)]` so the prefix triple is byte-identical to
/// `HewVtable` and slot 3 follows immediately.
#[repr(C)]
struct WidenedVtable {
    drop_in_place: unsafe extern "C" fn(*mut u8),
    size_of: usize,
    align_of: usize,
    /// Slot 3 — the first trait method.
    method0: unsafe extern "C" fn(*mut u8) -> i64,
}

#[test]
fn vtable_method_slot_dispatches_through_pointer() {
    let widened = WidenedVtable {
        drop_in_place: never_called_drop,
        size_of: mem::size_of::<u32>(),
        align_of: mem::align_of::<u32>(),
        method0: method_return_value,
    };
    let mut payload: u32 = 12_345;
    let obj = HewTraitObject {
        data: (&raw mut payload).cast::<u8>(),
        // SAFETY: WidenedVtable starts with the HewVtable prefix
        // byte-for-byte (both `#[repr(C)]`); reading it as a
        // `*const HewVtable` is the same operation codegen performs
        // when it GEPs a vtable static.
        vtable: (&raw const widened).cast::<HewVtable>(),
    };

    // The dispatch sequence codegen will emit:
    //   1. extractvalue the vtable pointer from the trait object.
    //   2. GEP past the prefix to slot 3 (here: `method0`).
    //   3. load the function pointer.
    //   4. indirect-call with the receiver pointer.
    //
    // The test approximates that by reading through the widened
    // struct directly, which models the same byte offsets.
    let result = unsafe {
        let vt_ptr = obj.vtable.cast::<WidenedVtable>();
        ((*vt_ptr).method0)(obj.data)
    };

    assert_eq!(
        result, 12_345,
        "method0 dispatch must receive the trait object's data pointer."
    );
}

// ---------------------------------------------------------------------------
// Fail-closed: OOB-panic helper
// ---------------------------------------------------------------------------

#[test]
fn dispatch_oob_message_names_offending_slot_and_width() {
    // The workspace profile is `panic = "abort"`, so we cannot
    // `catch_unwind` the extern-C entry. The message builder is
    // factored out for exactly this reason: it is what
    // `hew_vtable_dispatch_panic_on_oob` panics with, and naming
    // the offending slot + the trait's advertised width is the
    // diagnosable surface codegen relies on.
    let msg = vtable_dispatch_oob_message(7, 3);
    assert!(
        msg.contains("slot 7"),
        "panic message must name the offending slot; got: {msg}"
    );
    assert!(
        msg.contains("3 method slot"),
        "panic message must name the trait's advertised width; got: {msg}"
    );
    assert!(
        msg.contains("valid: 0..3"),
        "panic message must name the valid range; got: {msg}"
    );
}

#[test]
fn dispatch_panic_helper_is_addressable_and_diverges() {
    // The diagnostic helper must be addressable from Rust so codegen's
    // extern-C declaration can name it at link time. The function-
    // pointer cast below would fail to compile if the symbol's
    // signature drifted from `extern "C" fn(u32, u32) -> !`. We
    // never call through the pointer — invocation aborts under
    // `panic = "abort"` and a `should_panic` test cannot observe an
    // abort either — but the type-check is what TO-1 needs to lock.
    let fn_ptr: extern "C" fn(u32, u32) -> ! = hew_vtable_dispatch_panic_on_oob;
    // The cast itself is the lock: it would fail to compile if the
    // signature drifted. Reading the address through `core::ptr::fn_addr_eq`
    // would be overkill; an explicit `_ =` is enough to keep the
    // binding alive against future dead-code lints.
    let _ = fn_ptr;
}

// ---------------------------------------------------------------------------
// Fail-closed: null vtable
// ---------------------------------------------------------------------------

#[test]
fn null_vtable_is_observable_before_dispatch() {
    // Codegen's null-vtable arm reads `obj.vtable` and branches to
    // `hew_vtable_dispatch_panic_on_oob` when it is null. We model
    // the producer-side check here: the runtime substrate exposes
    // the field plainly so codegen can fail closed without
    // dereferencing. Dereferencing a null vtable is a program bug,
    // never a defined runtime behaviour.
    let obj = HewTraitObject {
        data: std::ptr::null_mut(),
        vtable: std::ptr::null(),
    };
    assert!(
        obj.vtable.is_null(),
        "null vtable must be observable as null — codegen branches on this."
    );
}
