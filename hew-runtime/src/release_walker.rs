//! One iterative release walker for descriptor-backed collections.
//!
//! Releasing a nested structure used to recurse once per nesting level: a Vec's
//! release dropped each element through the generated descriptor thunk, which
//! for a nested collection re-entered the Vec release. A 30000-deep structure
//! therefore overran the native stack while tearing down. Every
//! descriptor-backed collection release now runs through this worklist instead,
//! so work is proportional to the structure released and the native stack stays
//! flat for ordinary data (D457).
//!
//! Two entries share the one loop:
//!
//! * [`release_deferred`] is emitted where the whole released subtree is
//!   ordinary data — no resource close, no callable environment, no erased
//!   vtable drop. It queues onto a walk already in progress instead of nesting,
//!   which is what flattens a deep chain. Deferring such a release is
//!   unobservable: it runs no user code and completes inside the same
//!   synchronous release.
//! * [`release_now`] always drains its own item to completion before returning.
//!   Anything that can reach a resource is released this way, so a declared
//!   `close` still runs in exactly the order it does today: reverse field
//!   order, forward element order, whole subtree before the next sibling.
//!
//! The worklist is a resumable stack rather than a call chain: a wide
//! collection carries a cursor and gives up one element per step, so a later
//! runtime bulk-cleanup pass can drive the same items in quanta. This lane
//! always drives it to empty.

use std::cell::{Cell, RefCell};

use hew_cabi::vec::HewVec;

use crate::hashmap::HewLayoutHashMap;

/// One pending step of a release.
///
/// Each item borrows a structure the walker exclusively owns for the duration
/// of the walk, and the thread that queued an item is the thread that runs it.
#[derive(Clone, Copy)]
pub(crate) enum ReleaseItem {
    /// Expand a vector into its elements and then its own storage.
    Vector { vec: *mut HewVec, reverse: bool },
    /// Release elements `[next, end)` of `vec`, one per step.
    VectorElements {
        vec: *mut HewVec,
        next: usize,
        end: usize,
        reverse: bool,
    },
    /// Release the vector's element buffer and header.
    VectorStorage { vec: *mut HewVec },
    /// Expand a map into its occupied slots and then its own storage.
    Map { map: *mut HewLayoutHashMap },
    /// Release occupied slots from `next` upward, one per step.
    MapSlots {
        map: *mut HewLayoutHashMap,
        next: usize,
    },
    /// Release the map's entry buffer and header.
    MapStorage { map: *mut HewLayoutHashMap },
}

thread_local! {
    /// Pending steps of the walk in progress, reused across releases on this
    /// thread so a release allocates nothing per element.
    static PENDING: RefCell<Vec<ReleaseItem>> = const { RefCell::new(Vec::new()) };
    /// How many drains are running on this thread. Non-zero means a deferred
    /// release can join the walk instead of nesting.
    static DRAINS: Cell<usize> = const { Cell::new(0) };
}

/// Queue one step for the walk in progress.
pub(crate) fn queue(item: ReleaseItem) {
    PENDING.with(|pending| pending.borrow_mut().push(item));
}

/// Take the next step above `base`, or `None` once this drain is finished.
fn take_above(base: usize) -> Option<ReleaseItem> {
    PENDING.with(|pending| {
        let mut pending = pending.borrow_mut();
        if pending.len() > base {
            pending.pop()
        } else {
            None
        }
    })
}

/// Whether a walk is already draining on this thread.
fn walking() -> bool {
    DRAINS.with(|drains| drains.get() != 0)
}

/// Drive `item` and everything it queues to completion.
///
/// The drain owns the worklist above its own entry, so a nested `release_now`
/// finishes its subtree without disturbing the outer walk.
///
/// # Safety
///
/// `item` must name a structure this call exclusively owns.
unsafe fn drain(item: ReleaseItem) {
    let base = PENDING.with(|pending| {
        let mut pending = pending.borrow_mut();
        pending.push(item);
        pending.len() - 1
    });
    DRAINS.with(|drains| drains.set(drains.get() + 1));
    while let Some(step) = take_above(base) {
        // SAFETY: every queued step names storage the walk owns; the release
        // boundary is `extern "C"`, so a panic in a drop thunk aborts rather
        // than unwinding through the worklist.
        unsafe { run(step) };
    }
    DRAINS.with(|drains| drains.set(drains.get() - 1));
}

/// Release `item` synchronously, before returning to the caller.
///
/// # Safety
///
/// `item` must name a structure this call exclusively owns.
pub(crate) unsafe fn release_now(item: ReleaseItem) {
    // SAFETY: forwarded ownership contract.
    unsafe { drain(item) };
}

/// Release `item`, joining a walk already in progress when there is one.
///
/// Only ordinary data reaches here, so completing later within the same
/// release is unobservable.
///
/// # Safety
///
/// `item` must name a structure this call exclusively owns, and its whole
/// reachable subtree must be ordinary data.
pub(crate) unsafe fn release_deferred(item: ReleaseItem) {
    if walking() {
        queue(item);
        return;
    }
    // SAFETY: forwarded ownership contract.
    unsafe { drain(item) };
}

/// Perform one step, queueing whatever it uncovers.
///
/// # Safety
///
/// `item` must name storage the walk in progress owns.
unsafe fn run(item: ReleaseItem) {
    // SAFETY: forwarded ownership contract; each helper documents its own.
    unsafe {
        match item {
            ReleaseItem::Vector { vec, reverse } => crate::vec::expand_vector(vec, reverse),
            ReleaseItem::VectorElements {
                vec,
                next,
                end,
                reverse,
            } => crate::vec::release_one_element(vec, next, end, reverse),
            ReleaseItem::VectorStorage { vec } => crate::vec::free_vector_storage(vec),
            ReleaseItem::Map { map } => crate::hashmap::expand_map(map),
            ReleaseItem::MapSlots { map, next } => crate::hashmap::release_one_slot(map, next),
            ReleaseItem::MapStorage { map } => crate::hashmap::free_map_storage(map),
        }
    }
}
