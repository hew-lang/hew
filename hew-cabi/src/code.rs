//! Same-build ownership of executable code referenced by typed descriptors.

use core::ffi::c_void;

/// A host-owned executable root. Callbacks are thread-safe, cannot unwind, and
/// retain/release one reference to the defining code generation. The record
/// itself remains valid while any root or the loaded generation remains live.
#[repr(C)]
#[derive(Debug)]
pub struct HewCodeLease {
    pub context: *const c_void,
    pub retain: unsafe extern "C" fn(*const c_void),
    pub release: unsafe extern "C" fn(*const c_void),
}

/// Module-owned indirection populated once before any generated entry runs.
/// A null lease denotes statically resident code. After publication the host
/// never changes this slot, including while a callback or destructor runs.
#[repr(C)]
#[derive(Debug)]
pub struct HewCodeLeaseSlot {
    pub lease: *const HewCodeLease,
}
