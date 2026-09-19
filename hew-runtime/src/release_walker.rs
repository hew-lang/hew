//! One storage traversal for synchronous and resumable value release.
//!
//! The worklist visits collection elements in forward order, map keys before
//! values, and each complete subtree before its next sibling. The synchronous
//! driver calls pure drop thunks; an owning cursor instead yields initialized
//! slots to a caller that drives their consuming release continuations. Both
//! drivers release raw storage only after all nested obligations complete.
//!
//! Ordinary-data destruction may join an active thread-local walk to keep deep
//! values off the native call stack. Resumable cursors retain their own worklist
//! across suspension and never carry a thread-local borrow into authored code.
//!
//! Synchronous fallible closes retain their faults in the innermost release sink
//! until the release finishes. A continuation caller owns and combines its faults
//! while draining every yielded owner, including after cancellation.

use std::cell::{Cell, RefCell};
use std::ffi::c_void;
use std::ptr;

use hew_cabi::value::HewValueLayout;
use hew_cabi::vec::HewVec;

use crate::hashmap::HewLayoutHashMap;

/// One pending step of a release.
///
/// Each item borrows a structure the walker exclusively owns for the duration
/// of the walk, and the thread that queued an item is the thread that runs it.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ReleaseItem {
    /// One initialized value; the descriptor consumes its semantic owners.
    Value {
        slot: *mut c_void,
        layout: HewValueLayout,
    },
    /// An ordinary string owner from the legacy scalar vector representation.
    String {
        value: *mut hew_cabi::string::HewString,
    },
    /// Raw storage or a reference-count obligation after its value has closed.
    Storage {
        owner: *mut c_void,
        free: unsafe fn(*mut c_void),
    },
    /// An exactly laid out outer allocation, after its value has been consumed.
    Allocation {
        pointer: *mut c_void,
        size: usize,
        align: usize,
    },
    /// Expand a vector into its elements and then its own storage.
    Vector { vec: *mut HewVec },
    /// Release elements `[next, end)` of `vec`, one per step.
    VectorElements {
        vec: *mut HewVec,
        next: usize,
        end: usize,
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
    /// Releases in progress on this thread, innermost last, each holding the
    /// fault its `close` raised and the status that close returned. A frame
    /// arms one around a release the runtime performs for it; a walk arms one
    /// of its own so it finishes releasing before the fault leaves it.
    static SINKS: RefCell<Vec<Option<(i32, *mut crate::fault::HewFault)>>> =
        const { RefCell::new(Vec::new()) };
}

/// How many collection elements one worklist step releases. A step stays
/// bounded so a wide collection can be driven in quanta, while the chunk keeps
/// the worklist traffic off the per-element path.
pub(crate) const STEP_ELEMENTS: usize = 64;

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
    arm_release_sink();
    {
        let walk = Walk::enter(item);
        while let Some(step) = take_above(walk.base) {
            // SAFETY: every queued step names storage the walk owns. A drop
            // thunk whose `close` fails hands its fault to `held_fault`
            // instead of raising here, so no step unwinds through the
            // worklist and every remaining element is still released.
            unsafe { run(step) };
        }
    }
    // The walk is complete and every element is released exactly once, so the
    // fault a `close` raised during it can now leave it: into the sink the
    // caller armed, or, with none, out through the trap path.
    if let Some((code, fault)) = disarm_release_sink() {
        // SAFETY: `held_fault` transferred one unique fault owner.
        unsafe { crate::fault::hew_fault_trap(code, fault) };
    }
}

/// Arm a sink for the release about to run on this thread.
pub(crate) fn arm_release_sink() {
    SINKS.with(|sinks| sinks.borrow_mut().push(None));
}

/// Take back the innermost sink and whatever fault it collected.
pub(crate) fn disarm_release_sink() -> Option<(i32, *mut crate::fault::HewFault)> {
    SINKS.with(|sinks| sinks.borrow_mut().pop().flatten())
}

/// Arm a release sink around a release the runtime performs for a frame.
///
/// Generated code brackets a collection, shared-handle, callable or erased
/// release with this pair so a `close` that fails inside it reaches the frame
/// that asked for the release instead of the trap path.
#[no_mangle]
pub extern "C" fn hew_release_fault_begin() {
    arm_release_sink();
}

/// End the innermost release sink, transferring the fault it collected.
///
/// Returns null when the release raised nothing. `status_out` receives the
/// status the failing `close` returned, which is the status the frame keeps
/// when this fault becomes its own.
///
/// # Safety
/// `status_out` must be a live, writable `i32`.
#[no_mangle]
#[must_use]
pub unsafe extern "C" fn hew_release_fault_end(
    status_out: *mut i32,
) -> *mut crate::fault::HewFault {
    let Some((code, fault)) = disarm_release_sink() else {
        return std::ptr::null_mut();
    };
    // SAFETY: the caller supplies one live status slot.
    unsafe { *status_out = code };
    fault
}

/// The walk in progress, restoring the worklist even if a step unwinds.
struct Walk {
    base: usize,
}

impl Walk {
    fn enter(item: ReleaseItem) -> Self {
        let base = PENDING.with(|pending| {
            let mut pending = pending.borrow_mut();
            pending.push(item);
            pending.len() - 1
        });
        DRAINS.with(|drains| drains.set(drains.get() + 1));
        Self { base }
    }
}

impl Drop for Walk {
    fn drop(&mut self) {
        PENDING.with(|pending| pending.borrow_mut().truncate(self.base));
        DRAINS.with(|drains| drains.set(drains.get() - 1));
    }
}

/// Record a failing `close`'s fault against the release in progress.
///
/// Returns `false` with no sink armed, where the caller raises the fault
/// itself. With one armed, the fault stays until that release finishes, so a
/// `close` that fails partway through does not strand what the release still
/// owns, and the frame that asked for the release is the one that owns the
/// outcome.
///
/// # Safety
///
/// `fault` must transfer one live, unique, non-null fault owner.
pub(crate) unsafe fn held_fault(code: i32, fault: *mut crate::fault::HewFault) -> bool {
    SINKS.with(|sinks| {
        let mut sinks = sinks.borrow_mut();
        let Some(sink) = sinks.last_mut() else {
            return false;
        };
        *sink = Some(match sink.take() {
            // The first failing `close` names the failure; a later one during
            // the same release joins it as a secondary diagnostic.
            Some((first, primary)) => (first, unsafe {
                // SAFETY: both owners are unique and distinct.
                crate::fault::hew_fault_combine(primary, fault)
            }),
            None => (code, fault),
        });
        true
    })
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
        PENDING.with(|pending| pending.borrow_mut().push(item));
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
    if let ReleaseItem::Value { slot, layout } = item {
        if layout.release_start.is_some() {
            // A synchronous caller cannot discharge a resumable obligation.
            std::process::abort();
        }
        if let Some(drop) = layout.drop_fn {
            // SAFETY: this walk owns the initialized slot and exact descriptor.
            unsafe { drop(slot) };
        }
    } else {
        PENDING.with(|pending| {
            // SAFETY: expansion calls no authored code and cannot re-enter the walk.
            unsafe { expand(item, &mut pending.borrow_mut()) };
        });
    }
}

/// Expand storage without executing any selected semantic value callback.
unsafe fn expand(item: ReleaseItem, pending: &mut Vec<ReleaseItem>) {
    // SAFETY: the caller owns the item and retains queued storage until release.
    unsafe {
        match item {
            ReleaseItem::Value { .. } => pending.push(item),
            ReleaseItem::String { value } => hew_cabi::string::string_release(value),
            ReleaseItem::Storage { owner, free } => free(owner),
            ReleaseItem::Allocation {
                pointer,
                size,
                align,
            } => {
                crate::mem::hew_dealloc(pointer.cast(), size as u64, align as u64);
            }
            ReleaseItem::Vector { vec } => crate::vec::expand_vector(vec, pending),
            ReleaseItem::VectorElements { vec, next, end } => {
                crate::vec::release_element_chunk(vec, next, end, pending);
            }
            ReleaseItem::VectorStorage { vec } => crate::vec::free_vector_storage(vec),
            ReleaseItem::Map { map } => crate::hashmap::expand_map(map, pending),
            ReleaseItem::MapSlots { map, next } => {
                crate::hashmap::release_slot_chunk(map, next, pending);
            }
            ReleaseItem::MapStorage { map } => crate::hashmap::free_map_storage(map),
        }
    }
}

/// An owning release traversal. Each yielded slot remains live until the next
/// call, which is permitted only after its consuming callback has completed.
#[derive(Debug)]
pub struct HewReleaseCursor {
    pending: Vec<ReleaseItem>,
    current_layout: Option<HewValueLayout>,
}

impl HewReleaseCursor {
    pub(crate) fn new(pending: Vec<ReleaseItem>) -> *mut Self {
        Box::into_raw(Box::new(Self {
            pending,
            current_layout: None,
        }))
    }

    pub(crate) unsafe fn payload(
        slot: *mut c_void,
        size: usize,
        start: hew_cabi::value::HewValueReleaseStart,
        free_storage: bool,
    ) -> *mut Self {
        unsafe fn free_payload(slot: *mut c_void) {
            // SAFETY: the caller transferred a sized-block payload allocation.
            unsafe { crate::mem::buf_free(slot) };
        }
        let mut pending = Vec::new();
        if free_storage {
            pending.push(ReleaseItem::Storage {
                owner: slot,
                free: free_payload,
            });
        }
        if !slot.is_null() {
            pending.push(ReleaseItem::Value {
                slot,
                layout: HewValueLayout {
                    size,
                    align: 1,
                    ownership_kind: hew_cabi::value::HewTypeOwnershipKind::LayoutManaged,
                    clone_fn: None,
                    drop_fn: None,
                    visit_close: None,
                    release_start: Some(start),
                },
            });
        }
        Self::new(pending)
    }

    pub(crate) unsafe fn after(cursor: *mut Self, item: ReleaseItem) {
        // SAFETY: the caller owns this fresh cursor before any consumption.
        unsafe { (*cursor).pending.insert(0, item) };
    }

    /// Join freshly created cursors in consuming source order.
    pub(crate) unsafe fn join(cursors: Vec<*mut Self>) -> *mut Self {
        let mut pending = Vec::new();
        for cursor in cursors.into_iter().rev() {
            if !cursor.is_null() {
                // SAFETY: each cursor is uniquely transferred before its first step.
                let cursor = unsafe { Box::from_raw(cursor) };
                assert!(cursor.current_layout.is_none());
                pending.extend(cursor.pending);
            }
        }
        if pending.is_empty() {
            std::ptr::null_mut()
        } else {
            Self::new(pending)
        }
    }

    pub(crate) fn envelopes(
        values: Vec<Vec<u8>>,
        layout: Option<HewValueLayout>,
        after: ReleaseItem,
    ) -> *mut Self {
        unsafe fn free_envelope(owner: *mut c_void) {
            // SAFETY: each item transfers one Box<Vec<u8>> after its value closes.
            drop(unsafe { Box::from_raw(owner.cast::<Vec<u8>>()) });
        }
        let mut pending = vec![after];
        for value in values.into_iter().rev() {
            let mut value = Box::new(value);
            let slot = value.as_mut_ptr().cast();
            if let Some(layout) = layout {
                if layout.ownership_kind == hew_cabi::value::HewTypeOwnershipKind::LayoutManaged
                    && value.len() != layout.size
                {
                    std::process::abort();
                }
            }
            pending.push(ReleaseItem::Storage {
                owner: Box::into_raw(value).cast(),
                free: free_envelope,
            });
            if let Some(layout) = layout {
                if layout.ownership_kind == hew_cabi::value::HewTypeOwnershipKind::LayoutManaged {
                    pending.push(ReleaseItem::Value { slot, layout });
                }
            }
        }
        Self::new(pending)
    }
}

/// A runtime operation's consuming callback driver. Kept boxed because a
/// pending generated frame retains the address of its child fault output.
#[derive(Debug)]
pub(crate) struct ReleaseDriver {
    cursor: *mut HewReleaseCursor,
    state: *mut crate::coro_state::HewCoroState,
    frame: *mut c_void,
    child_fault: *mut crate::fault::HewFault,
    fault: *mut crate::fault::HewFault,
    done: bool,
}

impl ReleaseDriver {
    pub(crate) fn new(cursor: *mut HewReleaseCursor) -> Box<Self> {
        Box::new(Self {
            cursor,
            state: ptr::null_mut(),
            frame: ptr::null_mut(),
            child_fault: ptr::null_mut(),
            fault: ptr::null_mut(),
            done: cursor.is_null(),
        })
    }

    /// # Safety
    /// The driver remains boxed and uniquely driven through completion. The
    /// invocation is live through this poll and supplies its retained waker.
    pub(crate) unsafe fn poll(&mut self, parent: *mut crate::coro_state::HewCoroState) -> bool {
        use crate::coro_state::*;
        // SAFETY: this owner serializes every callback and retains each slot
        // until its child frame has completed and been destroyed.
        unsafe {
            while !self.done {
                if !self.state.is_null() {
                    if hew_coro_state_status(self.state) == CoroStatus::Pending as i32 {
                        if self.frame.is_null() {
                            std::process::abort();
                        }
                        crate::cont::hew_cont_resume(self.frame);
                    }
                    if hew_coro_state_status(self.state) == CoroStatus::Pending as i32 {
                        return false;
                    }
                    if !self.frame.is_null() {
                        if !crate::cont::hew_cont_done(self.frame) {
                            std::process::abort();
                        }
                        crate::cont::hew_cont_destroy(self.frame);
                        self.frame = ptr::null_mut();
                    }
                    hew_coro_state_free(self.state);
                    self.state = ptr::null_mut();
                    self.fault = crate::fault::hew_fault_combine(
                        self.fault,
                        std::mem::take(&mut self.child_fault),
                    );
                }
                let slot = hew_release_next(self.cursor);
                if slot.is_null() {
                    hew_release_finish(self.cursor);
                    self.cursor = ptr::null_mut();
                    self.done = true;
                    break;
                }
                let layout = *hew_release_layout(self.cursor);
                if let Some(start) = layout.release_start {
                    hew_coro_state_set_cleanup_fault(parent, self.fault);
                    self.state = hew_coro_state_cleanup_child(parent);
                    if self.state.is_null() {
                        std::process::abort();
                    }
                    self.frame = start(slot, (&raw mut self.child_fault).cast(), self.state.cast());
                    if hew_coro_state_status(self.state) == CoroStatus::Pending as i32 {
                        return false;
                    }
                } else if let Some(drop) = layout.drop_fn {
                    arm_release_sink();
                    drop(slot);
                    if let Some((_, fault)) = disarm_release_sink() {
                        self.fault = crate::fault::hew_fault_combine(self.fault, fault);
                    }
                }
            }
            true
        }
    }

    pub(crate) fn take_fault(&mut self) -> *mut crate::fault::HewFault {
        if !self.done {
            std::process::abort();
        }
        std::mem::take(&mut self.fault)
    }
}

impl Drop for ReleaseDriver {
    fn drop(&mut self) {
        if !self.done {
            std::process::abort();
        }
        // SAFETY: a completed driver owns its untransferred diagnostic.
        unsafe { crate::fault::hew_fault_drop(self.fault) };
    }
}

/// Transfer a vector and its initialized values to a consuming traversal.
/// # Safety
/// `value` is null or uniquely owned and may not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_vec_release_begin(value: *mut HewVec) -> *mut HewReleaseCursor {
    HewReleaseCursor::new(if value.is_null() {
        Vec::new()
    } else {
        vec![ReleaseItem::Vector { vec: value }]
    })
}

/// Transfer a fixed array to the shared forward-index release traversal.
/// # Safety
/// Same allocation and ownership contract as `hew_vec_release_begin`.
#[no_mangle]
pub unsafe extern "C" fn hew_array_release_begin(value: *mut HewVec) -> *mut HewReleaseCursor {
    // SAFETY: fixed arrays share the exact vector allocation representation.
    unsafe { hew_vec_release_begin(value) }
}

/// Transfer a map and its occupied slots to a consuming traversal.
/// # Safety
/// `value` is null or uniquely owned and may not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_hashmap_release_begin(
    value: *mut HewLayoutHashMap,
) -> *mut HewReleaseCursor {
    HewReleaseCursor::new(if value.is_null() {
        Vec::new()
    } else {
        vec![ReleaseItem::Map { map: value }]
    })
}

/// Yield the next initialized owner, or null once all storage is released.
/// # Safety
/// The cursor is uniquely owned. Its previous yielded callback has completed,
/// including after failure or cancellation; the caller must drain all items.
#[no_mangle]
pub unsafe extern "C" fn hew_release_next(cursor: *mut HewReleaseCursor) -> *mut c_void {
    // SAFETY: the caller owns this cursor through completion.
    let cursor = unsafe { &mut *cursor };
    cursor.current_layout = None;
    while let Some(item) = cursor.pending.pop() {
        if let ReleaseItem::Value { slot, layout } = item {
            cursor.current_layout = Some(layout);
            return slot;
        }
        // SAFETY: the cursor owns each item and all storage below its children.
        unsafe { expand(item, &mut cursor.pending) };
    }
    ptr::null_mut()
}

/// Borrow the exact descriptor of the current yielded slot.
/// # Safety
/// `cursor` is live; the descriptor is borrowed until `hew_release_next`.
#[no_mangle]
pub unsafe extern "C" fn hew_release_layout(
    cursor: *const HewReleaseCursor,
) -> *const HewValueLayout {
    // SAFETY: the caller retains the cursor while using its current descriptor.
    unsafe { &*cursor }
        .current_layout
        .as_ref()
        .map_or(ptr::null(), ptr::from_ref)
}

/// Release a fully drained traversal. This never abandons pending owners.
/// # Safety
/// The cursor is uniquely owned and its most recent `next` returned null.
#[no_mangle]
pub unsafe extern "C" fn hew_release_finish(cursor: *mut HewReleaseCursor) {
    if cursor.is_null() {
        return;
    }
    // SAFETY: the caller relinquishes the unique cursor allocation.
    let cursor = unsafe { Box::from_raw(cursor) };
    if !cursor.pending.is_empty() || cursor.current_layout.is_some() {
        std::process::abort();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::value::HewTypeOwnershipKind;

    unsafe extern "C" fn selected_release(
        _slot: *mut c_void,
        _fault: *mut *mut c_void,
        _state: *mut c_void,
    ) -> *mut c_void {
        unreachable!("the cursor must leave authored callbacks to its caller")
    }

    const LAYOUT: HewValueLayout = HewValueLayout {
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: None,
        drop_fn: None,
        visit_close: None,
        release_start: Some(selected_release),
    };

    #[test]
    fn consuming_cursor_retains_slots_between_forward_release_steps() {
        // SAFETY: the test transfers each initialized owner once and completes
        // every yielded semantic release before advancing the storage cursor.
        unsafe {
            let vector = crate::vec::hew_vec_new_with_elem_layout(&LAYOUT);
            for value in [11_i64, 22, 33] {
                crate::vec::hew_vec_push_owned_move(vector, (&raw const value).cast());
            }
            let cursor = hew_vec_release_begin(vector);
            for expected in [11, 22, 33] {
                let slot = hew_release_next(cursor).cast::<i64>();
                assert!(!slot.is_null());
                assert_eq!(slot.read(), expected);
                let layout = &*hew_release_layout(cursor);
                assert_eq!(layout.size, size_of::<i64>());
                assert!(layout.release_start.is_some());
                assert!(layout.drop_fn.is_none());
                // Pending cleanup retains this exact slot; only its caller
                // consumes it, and the next sibling is still initialized.
                slot.write(0);
            }
            assert!(hew_release_next(cursor).is_null());
            hew_release_finish(cursor);
        }
    }

    #[test]
    fn consuming_rc_cursor_pins_payload_after_last_external_weak_drops() {
        // SAFETY: the cursor transfers the last strong owner, while a distinct
        // weak owner is released during the simulated suspended destructor.
        unsafe {
            let value = 42_i64;
            let rc =
                crate::rc::hew_rc_new((&raw const value).cast(), LAYOUT.size, LAYOUT.align, None);
            let weak = crate::rc::hew_rc_downgrade(rc);
            let cursor = crate::rc::hew_rc_release_begin(rc, &LAYOUT);
            let slot = hew_release_next(cursor).cast::<i64>();
            crate::rc::hew_weak_drop_rc(weak);
            assert_eq!(slot.read(), 42);
            slot.write(0);
            assert!(hew_release_next(cursor).is_null());
            hew_release_finish(cursor);
        }
    }
}
