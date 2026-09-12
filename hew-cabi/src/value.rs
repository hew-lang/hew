//! Shared value layout and semantic copy/drop protocol.
//!
//! Collections copy this descriptor into their own storage; callback code must
//! outlive every value managed through it. Size and alignment describe storage,
//! while callbacks define ownership of the concrete value's fields.
//!
//! Before cloning, the caller copies all source bytes into the destination.
//! The clone callback replaces owning fields with independent logical copies;
//! scalar fields and tags are already initialized. On failure it releases its
//! partial copies and leaves no live destination obligation. A raw byte copy
//! alone is a semantic copy only for plain values; move-in operations instead
//! transfer the existing cleanup obligation without invoking clone glue.
//!
//! Drop callbacks release nested owners without freeing the outer storage.
//! Release-only descriptors may omit cloning, but copy operations reject them.

use core::ffi::c_void;

/// Descriptor-level ownership discipline for runtime-managed aggregate values.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewTypeOwnershipKind {
    /// Values can be copied and dropped as raw bytes.
    Plain = 0,
    /// String values with refcounted retain/release ownership. Descriptor
    /// callbacks supply the concrete representation's clone/drop operations.
    String = 1,
    /// Values use the descriptor's clone and drop callbacks.
    LayoutManaged = 2,
    /// Values are `bytes` values (`BytesTriple` at the ABI). Used by the
    /// channel/stream element witness, where a bytes element travels as its
    /// contents (the queue envelope IS the value, matching what platform
    /// stream backings produce natively). Collection constructors reject this
    /// kind fail-closed — `Vec`/`HashMap` describe bytes values through
    /// their own dedicated paths, never through this discriminant.
    Bytes = 3,
}

/// Thunk that clones an owned value from `src` to `dst`.
///
/// - `src` — non-null read-only pointer to the source value blob.
/// - `dst` — non-null mutable pointer to the destination slot. The caller MUST
///   have memcpy'd `dst <- src` (size bytes) BEFORE invoking this thunk so that
///   `BitCopy` fields and any enum tag/inactive bytes are already correct; the
///   thunk deep-clones only the owned (heap-owning) fields in place.
/// - Returns 0 on success, non-zero when a partial clone was rolled back.
///
pub type HewValueCloneThunk = unsafe extern "C" fn(src: *const c_void, dst: *mut c_void) -> i32;

/// Thunk that drops an owned value in place.
///
/// - `slot` — non-null mutable pointer to the owned value blob. The thunk
///   releases the value's managed fields and nested containers, but does
///   not deallocate the storage itself — the caller owns and releases it.
/// - Invoked exactly once for each destroyed value obligation, including
///   replacement, removal and final release.
///
pub type HewValueDropThunk = unsafe extern "C" fn(slot: *mut c_void);

/// Poll a uniquely borrowed owner through cooperative cleanup. The invocation
/// state supplies cancellation and readiness; the result uses `CoroStatus`.
/// Pending retains the borrow. A terminal fault transfers one fault owner.
pub type HewValueClosePoll = unsafe extern "C" fn(
    owner: *mut c_void,
    invocation_state: *mut c_void,
    fault_out: *mut *mut c_void,
) -> i32;

/// Visit initialized children whose release requires cooperative cleanup.
/// Generated from the same field recipes as drop, including their masks and
/// variant tags. `context` is the runtime's close collector. Visiting borrows
/// children without changing their initialization or releasing storage.
pub type HewValueCloseVisit = unsafe extern "C" fn(slot: *mut c_void, context: *mut c_void);

// Rust guarantees fn pointers are non-null, so `Option<fn>` niche-optimises to
// the underlying fn-pointer width — the null value is the `None` discriminant.
// These asserts lock the invariant so a future Rust change breaks loudly here
// rather than silently at the C boundary.
const _: () = assert!(
    size_of::<Option<HewValueCloneThunk>>() == size_of::<HewValueCloneThunk>(),
    "Option<HewValueCloneThunk> must be niche-optimised to the same size as HewValueCloneThunk",
);
const _: () = assert!(
    size_of::<Option<HewValueDropThunk>>() == size_of::<HewValueDropThunk>(),
    "Option<HewValueDropThunk> must be niche-optimised to the same size as HewValueDropThunk",
);

/// Shared layout and ownership protocol for a concrete Hew value type.
///
/// The callbacks implement the concrete value's semantic copy and cleanup.
/// Plain values may omit both; release-only values may omit cloning.
///
/// # C layout
///
/// `#[repr(C)]` guarantees field order and alignment match the C struct:
///
/// ```c
/// typedef struct {
///     size_t                  size;
///     size_t                  align;
///     HewTypeOwnershipKind    ownership_kind;
///     /* padding to pointer alignment */
///     HewValueCloneThunk       clone_fn;  /* NULL for plain or release-only values */
///     HewValueDropThunk        drop_fn;   /* may be NULL only when ownership_kind == Plain */
///     HewValueCloseVisit      visit_close; /* optional child walk before drop */
/// } HewValueLayout;
/// ```
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HewValueLayout {
    /// Size of one value in bytes. Zero-sized values copy no payload bytes.
    pub size: usize,
    /// Required alignment in bytes (a non-zero power of two).
    pub align: usize,
    /// Ownership discipline of the value type.
    pub ownership_kind: HewTypeOwnershipKind,
    /// Semantic clone thunk invoked when creating an independent value. `None`
    /// is valid for Plain values and release-only descriptors; copy operations reject a
    /// missing thunk when semantic cloning is required.
    pub clone_fn: Option<HewValueCloneThunk>,
    /// Cleanup thunk invoked when destroying an owned value. `None` is
    /// valid only for `ownership_kind == Plain`.
    pub drop_fn: Option<HewValueDropThunk>,
    /// Optional initialized-child walk performed before synchronous drop.
    pub visit_close: Option<HewValueCloseVisit>,
}

#[cfg(target_pointer_width = "64")]
const _: () = {
    assert!(core::mem::offset_of!(HewValueLayout, size) == 0);
    assert!(core::mem::offset_of!(HewValueLayout, align) == 8);
    assert!(core::mem::offset_of!(HewValueLayout, ownership_kind) == 16);
    assert!(core::mem::offset_of!(HewValueLayout, clone_fn) == 24);
    assert!(core::mem::offset_of!(HewValueLayout, drop_fn) == 32);
    assert!(core::mem::offset_of!(HewValueLayout, visit_close) == 40);
    assert!(core::mem::size_of::<HewValueLayout>() == 48);
};

#[cfg(target_pointer_width = "32")]
const _: () = {
    assert!(core::mem::offset_of!(HewValueLayout, size) == 0);
    assert!(core::mem::offset_of!(HewValueLayout, align) == 4);
    assert!(core::mem::offset_of!(HewValueLayout, ownership_kind) == 8);
    assert!(core::mem::offset_of!(HewValueLayout, clone_fn) == 12);
    assert!(core::mem::offset_of!(HewValueLayout, drop_fn) == 16);
    assert!(core::mem::offset_of!(HewValueLayout, visit_close) == 20);
    assert!(core::mem::size_of::<HewValueLayout>() == 24);
};
