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
    hew_dyn_box_alloc, hew_dyn_box_free, hew_vtable_dispatch_panic_on_oob,
    vtable_dispatch_oob_message, HewTraitObject, HewVtable,
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

// ---------------------------------------------------------------------------
// Heap-box ABI (W3.031 Stage 0): hew_dyn_box_alloc / hew_dyn_box_free
// ---------------------------------------------------------------------------
//
// These tests model the codegen call sequence for a `dyn Trait` returned
// by value: callee allocates a buffer sized from the vtable's
// `(size_of, align_of)` prefix slots, memcpy's the concrete value in,
// and returns the fat pointer. The receiving `DropKind::TraitObject
// { storage: HeapBoxed }` ritual then runs slot-0 (drop_in_place) and
// frees the buffer with the same `(size, align)` triple.
//
// The matrix below covers:
//   - representative scalar layouts (i32, i64, large struct)
//   - pointer-width-sized layouts on both native (8) and wasm32 (4),
//     so the test pins the substrate's correctness for both ABIs
//   - ZST convention (size == 0)
//   - alignment overlarge relative to size (vtable for a `repr(align(64))`
//     struct holding a single i32)

#[test]
fn dyn_box_alloc_and_free_round_trip_for_scalar_layouts() {
    // (size, align) pairs that cover the layouts codegen will emit for
    // typical concrete impls: 4/4 (i32), 8/8 (i64 / native pointer),
    // 16/8 (two-pointer struct), 4/4 (wasm32 pointer-width).
    for &(size, align) in &[(4_usize, 4_usize), (8, 8), (16, 8), (4, 4), (1, 1), (2, 2)] {
        let ptr = unsafe { hew_dyn_box_alloc(size, align) };
        assert!(
            !ptr.is_null(),
            "alloc must return non-null for size={size}, align={align}"
        );
        assert_eq!(
            ptr as usize % align,
            0,
            "alloc must return a buffer aligned to {align}; got ptr={ptr:p}"
        );
        // The buffer is uninitialised; codegen would memcpy the
        // concrete value in here. We model that by writing the entire
        // buffer to a known pattern and reading it back — the
        // round-trip pins that the alloc honoured `size`.
        unsafe {
            std::ptr::write_bytes(ptr, 0xA5, size);
            for off in 0..size {
                assert_eq!(
                    *ptr.add(off),
                    0xA5,
                    "byte {off} of {size}-byte buffer must round-trip 0xA5"
                );
            }
            hew_dyn_box_free(ptr, size, align);
        }
    }
}

#[test]
fn dyn_box_alloc_honors_overaligned_layout() {
    // Models a concrete impl declared `#[repr(align(64))]` — the
    // vtable's align_of slot would be 64, size_of would be the
    // padded struct size (here 64 for a single-field struct).
    let size = 64_usize;
    let align = 64_usize;
    let ptr = unsafe { hew_dyn_box_alloc(size, align) };
    assert!(!ptr.is_null(), "alloc must return non-null");
    assert_eq!(
        ptr as usize % align,
        0,
        "alloc must honour the over-large alignment ({align}); got ptr={ptr:p}"
    );
    unsafe { hew_dyn_box_free(ptr, size, align) };
}

#[test]
fn dyn_box_alloc_zero_size_returns_dangling_non_null() {
    // ZST convention: `Layout::from_size_align(0, align)` is well-
    // formed; codegen for a `dyn Trait` whose concrete type is a
    // unit struct would still construct a fat pointer. The alloc
    // entry skips the allocator and returns `align as *mut u8` —
    // a non-null but dangling pointer matching `NonNull::dangling`.
    // The matching free is a no-op.
    for &align in &[1_usize, 4, 8, 16] {
        let ptr = unsafe { hew_dyn_box_alloc(0, align) };
        assert!(
            !ptr.is_null(),
            "ZST alloc must return non-null sentinel for align={align}"
        );
        assert_eq!(
            ptr as usize, align,
            "ZST sentinel must equal the alignment per NonNull::dangling convention"
        );
        // The matching free must be a no-op (does not call into the
        // allocator with a null pointer or a zero-sized layout).
        unsafe { hew_dyn_box_free(ptr, 0, align) };
    }
}

#[test]
fn dyn_box_free_zero_size_with_null_ptr_is_noop() {
    // The codegen drop ritual sources `(ptr, size, align)` from the
    // fat pointer and the vtable. For a ZST concrete type, `size==0`,
    // and the matching free must be a no-op regardless of `ptr` —
    // including the `null_mut()` carrier the codegen-emitted null-out
    // pattern (Stage 7) would leave behind after a consuming dispatch.
    // We accept either a null or a dangling sentinel here because the
    // ZST branch never touches the pointer.
    unsafe {
        hew_dyn_box_free(std::ptr::null_mut(), 0, 8);
    }
}

#[test]
fn dyn_box_alloc_function_pointer_is_addressable() {
    // The cast pins the extern-C signature codegen will declare. If
    // either entry's ABI drifted, this would fail to compile.
    let alloc_fn: unsafe extern "C" fn(usize, usize) -> *mut u8 = hew_dyn_box_alloc;
    let free_fn: unsafe extern "C" fn(*mut u8, usize, usize) = hew_dyn_box_free;
    let _ = alloc_fn;
    let _ = free_fn;
}

// ---------------------------------------------------------------------------
// End-to-end: alloc, vtable construction, slot-0 drop, free
// ---------------------------------------------------------------------------

static HEAP_BOX_DROP_OBSERVED: AtomicU32 = AtomicU32::new(0);

unsafe extern "C" fn heap_box_drop(data: *mut u8) {
    // Same shape as `synthetic_drop` above but writes to a separate
    // observation cell so the two test families do not interleave.
    let token = unsafe { *data.cast::<u32>() };
    HEAP_BOX_DROP_OBSERVED.store(token, Ordering::SeqCst);
}

#[test]
fn heap_boxed_dyn_trait_drop_ritual_round_trip() {
    // Model the full Stage 0 surface a Stage 5/6 codegen emission
    // would compose:
    //   1. callee: alloc(size, align)            -> data_ptr
    //              memcpy(value, data_ptr, size)
    //              return HewTraitObject { data, vtable }
    //   2. caller: at scope exit
    //              vt.drop_in_place(data_ptr)    -- slot 0
    //              hew_dyn_box_free(data_ptr, size, align)
    //
    // This pins all three of (alloc honours layout), (drop slot fires
    // on heap storage), (free releases the same buffer).
    let vt = HewVtable {
        drop_in_place: heap_box_drop,
        size_of: mem::size_of::<u32>(),
        align_of: mem::align_of::<u32>(),
    };

    let size = vt.size_of;
    let align = vt.align_of;
    let data = unsafe { hew_dyn_box_alloc(size, align) };
    assert!(!data.is_null());

    // memcpy the concrete value into the heap buffer.
    let concrete: u32 = 0xCAFE_F00D;
    unsafe {
        std::ptr::copy_nonoverlapping(
            (&raw const concrete).cast::<u8>(),
            data,
            mem::size_of::<u32>(),
        );
    }
    let obj = HewTraitObject {
        data,
        vtable: &raw const vt,
    };

    HEAP_BOX_DROP_OBSERVED.store(0, Ordering::SeqCst);
    unsafe {
        // Slot-0 drop dispatch on heap-backed data.
        ((*obj.vtable).drop_in_place)(obj.data);
        // Free the heap buffer using the vtable's (size, align) prefix.
        hew_dyn_box_free(obj.data, (*obj.vtable).size_of, (*obj.vtable).align_of);
    }
    assert_eq!(
        HEAP_BOX_DROP_OBSERVED.load(Ordering::SeqCst),
        0xCAFE_F00D,
        "slot-0 drop must observe the heap-boxed concrete value before free."
    );
}
