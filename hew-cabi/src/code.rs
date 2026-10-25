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

// SAFETY: the host contract requires immutable data and thread-safe callbacks.
unsafe impl Send for HewCodeLease {}
// SAFETY: invoking a lease callback remains unsafe; shared reads are immutable.
unsafe impl Sync for HewCodeLease {}

/// Module-owned indirection populated once before any generated entry runs.
/// A null lease denotes statically resident code. After publication the host
/// never changes this slot, including while a callback or destructor runs.
#[repr(C)]
#[derive(Debug)]
pub struct HewCodeLeaseSlot {
    pub lease: *const HewCodeLease,
}

extern "C" {
    /// Acquire one native defining-code root from a loader-published slot.
    pub fn hew_code_root_acquire(slot: *const HewCodeLeaseSlot) -> *const HewCodeLease;
    /// Release one acquired defining-code root after callback/frame completion.
    pub fn hew_code_root_release(lease: *const HewCodeLease);
}
