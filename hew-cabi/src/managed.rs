//! Typed traversal of language-managed backing storage.
//!
//! A value descriptor's slot width uses canonical language units. Its visitor
//! reports actual backing capacity and follows only typed, live descendants;
//! allocator headers, reference counts and provider bookkeeping are excluded.
//! The owner chooses whether a backing's descendants need visiting. This makes
//! shared aliases cheap after the first acquisition and prevents repeated DAG
//! traversal. These callbacks never allocate language values or unwind.

use core::ffi::c_void;

/// Continue into a backing's typed descendants.
pub const DESCEND: i32 = 0;
/// This owner already retains the backing, or still retains another alias.
pub const SKIP_DESCENDANTS: i32 = 1;
/// Missing metadata must fail closed rather than describe an empty graph.
pub const UNDESCRIBED: i32 = 2;
/// Canonical payload arithmetic overflow is an exceeded budget.
pub const HEAP_EXCEEDED: i32 = 3;
/// Transfer preparation is waiting for a provider's existing executor gate.
/// This is internal readiness, never a language fault or budget refusal.
pub const TRANSFER_PENDING: i32 = 4;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewManagedWalkKind {
    Retain,
    Release,
    /// Preflight a semantic copy. A null storage identity describes fresh
    /// private capacity, while an existing identity describes a shared retain.
    ClonePlan,
    /// Freeze provider admission/mutation before an ownership transfer.
    PrepareTransfer,
    /// Rebind provider ownership while the entire graph remains frozen.
    CommitTransfer,
    /// Resume the original provider owner after refused adoption.
    AbortTransfer,
    /// Resume providers only after every destination binding is committed.
    ResumeTransfer,
}

/// Called once for each encountered backing ownership edge. The callback
/// records that edge and returns `DESCEND`, `SKIP_DESCENDANTS` or a failure status.
pub type HewManagedStorageVisit =
    unsafe extern "C" fn(context: *mut c_void, identity: *const c_void, payload_bytes: u64) -> i32;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewManagedStorageVisitor {
    pub context: *mut c_void,
    pub storage: HewManagedStorageVisit,
    pub kind: HewManagedWalkKind,
}

/// Traverse a live concrete value slot. Ordinary retain/release/copy walks only
/// inspect payloads; explicit transfer phases update provider admission gates
/// and ownership bindings. A nonzero acquisition failure preserves the value.
pub type HewManagedValueVisit =
    unsafe extern "C" fn(slot: *const c_void, visitor: *const HewManagedStorageVisitor) -> i32;

/// Immutable accounting facts attached to the existing semantic value layout.
/// A missing visitor is valid only when this concrete value owns no backing.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewManagedValueDescriptor {
    pub slot_bytes: u64,
    pub visit: Option<HewManagedValueVisit>,
}

/// Borrowed live root view, pinned by its suspended native invocation.
/// Slot occupancy is the compiler's ordinary ownership initialization bit.
#[repr(C)]
#[derive(Debug)]
pub struct HewManagedFrameSlot {
    pub layout: *const crate::value::HewValueLayout,
    pub slot: *const c_void,
    pub initialized: *const u8,
}

#[repr(C)]
#[derive(Debug)]
pub struct HewManagedFrame {
    pub slots: *const HewManagedFrameSlot,
    pub count: usize,
    pub child: *const *const crate::coro::HewCoroInvocation,
    pub operation: *const HewManagedFrameOperation,
}

/// The current compiler-owned protocol handle, using its canonical provider
/// descriptor. Protocol bookkeeping contributes no payload of its own.
#[repr(C)]
#[derive(Debug)]
pub struct HewManagedFrameOperation {
    pub layout: *const crate::value::HewValueLayout,
    pub slot: *const c_void,
}

/// Visit the managed descendants described by one canonical value layout.
///
/// # Safety
/// The slot must match the live layout, and visitor/context must remain live
/// throughout its synchronous traversal. Zero-sized slots may be null.
#[must_use]
pub unsafe fn visit_value(
    layout: &crate::value::HewValueLayout,
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the caller holds the descriptor's defining code/storage roots.
    let Some(managed) = (unsafe { layout.managed.as_ref() }) else {
        return UNDESCRIBED;
    };
    match managed.visit {
        // SAFETY: the caller supplies the concrete slot and live visitor.
        Some(visit) => unsafe { visit(slot, visitor) },
        None => 0,
    }
}
