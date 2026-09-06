//! Erased callable storage and compiler-generated invocation adapters.
//!
//! SIR decides capture ownership and whether a borrowed invocation is legal.
//! The environment's value layout owns its initialization mask and clone/drop
//! behaviour. Runtime helpers allocate and release storage without interpreting
//! captures, argument types or call permissions. Descriptors and their callback
//! code must outlive every carrier that refers to them.

use core::ffi::c_void;

use crate::value::HewValueLayout;

/// Compiler-generated adapter for one exact callable signature.
///
/// A borrowed adapter leaves environment ownership with its caller. The once
/// adapter consumes it and disposes its storage on both success and failure;
/// the caller must invalidate the transferred carrier before invoking it.
/// Argument ownership and slot layouts are established by the compiler.
///
/// The adapter returns its LLVM continuation and publishes completion through
/// the invocation state. Successful completion initializes `result_out` (unless
/// unit) and clears `fault_out`; failure leaves the result untouched and transfers
/// the owned fault. The caller drives the frame until completion, then destroys
/// it before releasing invocation state. Adapters must not unwind or longjmp.
///
/// # Safety
/// The environment and argument slots must match the adapter's exact compiler
/// signature and ownership contract. Output slots must have the corresponding
/// size, alignment and validity; a unit result may use a null result slot.
pub type HewCallableInvoke = unsafe extern "C" fn(
    environment: *mut c_void,
    argument_slots: *const *mut c_void,
    result_out: *mut c_void,
    fault_out: *mut *mut c_void,
    invocation_state: *mut c_void,
) -> *mut c_void;

/// Immutable compiler-authored environment and invocation contract.
#[repr(C)]
#[derive(Debug)]
pub struct HewCallableDescriptor {
    /// Exact allocation layout and mask-aware capture clone/drop callbacks.
    /// An empty function has a Plain, zero-size, align-one layout with no
    /// clone/drop callbacks. Captured zero-sized values still need nonzero
    /// environment storage for the compiler's initialization mask.
    pub environment: *const HewValueLayout,
    /// Borrowed adapter, if supported. Its presence does not grant permission:
    /// SIR decides whether each read or mutable invocation is legal.
    pub invoke_borrow: Option<HewCallableInvoke>,
    /// Consuming adapter, always present, including for empty functions.
    pub invoke_once: HewCallableInvoke,
}

/// One owning callable value, passed by address to the runtime helpers.
///
/// Copying these two pointer bits transfers or aliases representation only;
/// an independent owning copy requires `hew_callable_clone`. A transferred or
/// dropped carrier is cleared to two null pointers. Empty functions retain a
/// valid descriptor while their environment pointer is null.
#[repr(C)]
#[derive(Debug)]
pub struct HewCallableValue {
    /// Environment storage owned by this carrier, or null for an empty function.
    pub environment: *mut c_void,
    /// Immutable descriptor that outlives the environment and its invocations.
    pub descriptor: *const HewCallableDescriptor,
}

const _: () = {
    assert!(size_of::<Option<HewCallableInvoke>>() == size_of::<HewCallableInvoke>());
    assert!(align_of::<Option<HewCallableInvoke>>() == align_of::<HewCallableInvoke>());
};

#[cfg(target_pointer_width = "64")]
const _: () = {
    assert!(core::mem::offset_of!(HewCallableValue, environment) == 0);
    assert!(core::mem::offset_of!(HewCallableValue, descriptor) == 8);
    assert!(size_of::<HewCallableValue>() == 16);
    assert!(align_of::<HewCallableValue>() == 8);
    assert!(core::mem::offset_of!(HewCallableDescriptor, environment) == 0);
    assert!(core::mem::offset_of!(HewCallableDescriptor, invoke_borrow) == 8);
    assert!(core::mem::offset_of!(HewCallableDescriptor, invoke_once) == 16);
    assert!(size_of::<HewCallableDescriptor>() == 24);
    assert!(align_of::<HewCallableDescriptor>() == 8);
};

#[cfg(target_pointer_width = "32")]
const _: () = {
    assert!(core::mem::offset_of!(HewCallableValue, environment) == 0);
    assert!(core::mem::offset_of!(HewCallableValue, descriptor) == 4);
    assert!(size_of::<HewCallableValue>() == 8);
    assert!(align_of::<HewCallableValue>() == 4);
    assert!(core::mem::offset_of!(HewCallableDescriptor, environment) == 0);
    assert!(core::mem::offset_of!(HewCallableDescriptor, invoke_borrow) == 4);
    assert!(core::mem::offset_of!(HewCallableDescriptor, invoke_once) == 8);
    assert!(size_of::<HewCallableDescriptor>() == 12);
    assert!(align_of::<HewCallableDescriptor>() == 4);
};

extern "C" {
    /// Allocate a zeroed environment with this descriptor's exact layout.
    ///
    /// Returns null only for an empty function. Invalid descriptors and genuine
    /// allocation failure abort. Zeroed bytes initialize capture masks, not
    /// capture values; the compiler marks each capture only after initialization.
    ///
    /// # Safety
    /// The descriptor and its environment layout must be valid and outlive the
    /// returned allocation. The caller must eventually transfer or drop it.
    pub fn hew_callable_env_alloc(descriptor: *const HewCallableDescriptor) -> *mut c_void;

    /// Create an independent callable, publishing `out` only on success.
    ///
    /// Copies environment bytes before invoking the value clone thunk. A thunk
    /// failure must already have released its partial capture copies; the helper
    /// then frees only the outer allocation and returns that status. Missing
    /// non-Plain clone callbacks or invalid carriers return -1. All failures
    /// leave `out` untouched. Zero indicates a complete new owner in `out`.
    ///
    /// # Safety
    /// Non-null pointers must name valid, non-overlapping carrier slots. `src`
    /// must own a valid environment. `out` must be writable and must not contain
    /// a live environment owner when the call succeeds. Descriptor callbacks
    /// obey the shared value protocol and must not unwind across the C boundary.
    pub fn hew_callable_clone(src: *const HewCallableValue, out: *mut HewCallableValue) -> i32;

    /// Clear a carrier, drop its initialized captures, and free its environment.
    ///
    /// Null pointers and cleared carriers are inert. Descriptor drop callbacks
    /// release captures only; this helper releases the exact outer allocation.
    /// Invalid live carriers abort.
    ///
    /// # Safety
    /// A non-null pointer must name a writable carrier with sole ownership of
    /// its environment, or a cleared carrier. Its descriptor and drop callback
    /// must remain valid until cleanup completes.
    pub fn hew_callable_drop(value: *mut HewCallableValue);
}
