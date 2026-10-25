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
/// of the walk. One consuming owner drives each cursor; synchronous release
/// sinks remain thread-local.
#[derive(Debug)]
pub(crate) enum ReleaseItem {
    /// Defining code retained until all preceding value and storage work finishes.
    CodeRoot(crate::code_owner::CodeRoot),
    /// Prepare and consume one actor-owned Rc survivor wave incrementally.
    RcSurvivors(Box<crate::rc::RcSurvivors>),
    /// Expand endpoint owners after its final I/O operation is quiescent.
    Sink { sink: *mut crate::stream::HewSink },
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
    /// Canonical payload backing, retained until preceding typed children close.
    ManagedBacking {
        pointer: *mut c_void,
        bytes: u64,
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
            // SAFETY: the walk's value owner pins the canonical descriptor.
            let _code = unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) };
            // SAFETY: this walk owns the initialized slot and exact descriptor.
            unsafe { drop(slot) };
        }
    } else {
        PENDING.with(|pending| {
            // SAFETY: expansion calls no authored code and cannot re-enter the walk.
            let _ = unsafe { expand(item, &mut pending.borrow_mut()) };
        });
    }
}

/// Expand storage without executing any selected semantic value callback.
unsafe fn expand(
    item: ReleaseItem,
    pending: &mut Vec<ReleaseItem>,
) -> Option<hew_observe_protocol::RcSweep> {
    // SAFETY: the caller owns the item and retains queued storage until release.
    unsafe {
        match item {
            ReleaseItem::CodeRoot(root) => drop(root),
            ReleaseItem::RcSurvivors(wave) => return wave.step(pending),
            ReleaseItem::Sink { sink } => crate::stream::expand_sink(sink, pending),
            ReleaseItem::Value { .. } => pending.push(item),
            ReleaseItem::String { value } => hew_cabi::string::string_release(value),
            ReleaseItem::Storage { owner, free } => free(owner),
            ReleaseItem::ManagedBacking {
                pointer,
                bytes,
                free,
            } => {
                if let Some(owner) = crate::owned_value_budget::current_owner() {
                    owner.release_backing(pointer, bytes);
                }
                free(pointer);
            }
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
    None
}

/// An owning release traversal. Each yielded slot remains live until the next
/// call, which is permitted only after its consuming callback has completed.
#[derive(Debug)]
pub struct HewReleaseCursor {
    pending: Vec<ReleaseItem>,
    current_layout: Option<HewValueLayout>,
    current_slot: *mut c_void,
    remaining: usize,
    rc_sweep: Option<hew_observe_protocol::RcSweep>,
    value_owner: Option<Option<crate::owned_value_budget::ValueOwner>>,
}

impl HewReleaseCursor {
    /// Stamp cleanup with the account that owns its unconsumed payload.
    pub(crate) fn set_value_owner(&mut self, owner: Option<crate::owned_value_budget::ValueOwner>) {
        self.value_owner = Some(owner);
    }

    /// Move an owner into independent storage before its receiver is mutated.
    /// # Safety
    /// `slot` is initialized with this exact layout. The caller must cease
    /// owning its old bits after this call; no semantic clone occurs.
    pub(crate) unsafe fn detached(slot: *const c_void, layout: HewValueLayout) -> *mut Self {
        if layout.drop_fn.is_none() && layout.release_start.is_none() {
            return ptr::null_mut();
        }
        // SAFETY: the exact descriptor supplies valid allocation geometry;
        // even zero-sized owners need a non-null cursor slot.
        unsafe {
            let size = layout.size.max(1);
            let storage = crate::mem::hew_alloc(size as u64, layout.align as u64);
            ptr::copy_nonoverlapping(slot.cast::<u8>(), storage, layout.size);
            Self::new(vec![
                ReleaseItem::Allocation {
                    pointer: storage.cast(),
                    size,
                    align: layout.align,
                },
                ReleaseItem::Value {
                    slot: storage.cast(),
                    layout,
                },
            ])
        }
    }

    pub(crate) fn new(pending: Vec<ReleaseItem>) -> *mut Self {
        Box::into_raw(Box::new(Self {
            pending,
            current_layout: None,
            current_slot: ptr::null_mut(),
            remaining: STEP_ELEMENTS,
            rc_sweep: None,
            value_owner: None,
        }))
    }

    /// Transfer initialized value obligations while their enclosing owner pins storage.
    pub(crate) fn values(
        values: impl IntoIterator<Item = (*mut c_void, HewValueLayout)>,
    ) -> *mut Self {
        let mut pending: Vec<_> = values
            .into_iter()
            .filter_map(|(slot, layout)| {
                (layout.drop_fn.is_some() || layout.release_start.is_some())
                    .then_some(ReleaseItem::Value { slot, layout })
            })
            .collect();
        pending.reverse();
        if pending.is_empty() {
            ptr::null_mut()
        } else {
            Self::new(pending)
        }
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
                    code_owner: core::ptr::null(),
                    managed: core::ptr::null(),
                    size,
                    align: 1,
                    ownership_kind: hew_cabi::value::HewTypeOwnershipKind::LayoutManaged,
                    clone_fn: None,
                    drop_fn: None,

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
    pub(crate) unsafe fn join(
        cursors: impl IntoIterator<Item = *mut Self, IntoIter: DoubleEndedIterator>,
    ) -> *mut Self {
        let mut pending = Vec::new();
        let mut value_owner: Option<Option<crate::owned_value_budget::ValueOwner>> = None;
        for cursor in cursors.into_iter().rev() {
            if !cursor.is_null() {
                // SAFETY: each cursor is uniquely transferred before its first step.
                let cursor = unsafe { Box::from_raw(cursor) };
                assert!(cursor.current_layout.is_none());
                if let Some(owner) = cursor.value_owner {
                    if let Some(previous) = &value_owner {
                        assert!(
                            match (previous, &owner) {
                                (None, None) => true,
                                (Some(previous), Some(owner)) => previous.same_account(owner),
                                _ => false,
                            },
                            "joined cleanup cursors belong to different accounts"
                        );
                    } else {
                        value_owner = Some(owner);
                    }
                }
                pending.extend(cursor.pending);
            }
        }
        if pending.is_empty() {
            std::ptr::null_mut()
        } else {
            let joined = Self::new(pending);
            // SAFETY: the fresh joined cursor owns the same operation graph.
            unsafe { (*joined).value_owner = value_owner };
            joined
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
    rc_sweep: Option<hew_observe_protocol::RcSweep>,
    active_code: Option<crate::code_owner::CodeRoot>,
    value_owner: Option<crate::owned_value_budget::ValueOwner>,
}

impl ReleaseDriver {
    /// Cancel the active callback while keeping this driver's remaining
    /// release slots available for normal cleanup.
    pub(crate) fn cancel_active(&self) {
        if !self.state.is_null() {
            // SAFETY: the driver owns this child invocation until it finishes.
            unsafe { crate::coro_state::hew_coro_state_cancel(self.state) };
        }
    }

    #[expect(
        clippy::unnecessary_box_returns,
        reason = "generated child frames retain fault output addresses in this allocation"
    )]
    pub(crate) fn new(cursor: *mut HewReleaseCursor) -> Box<Self> {
        // SAFETY: the caller transfers its live optional cursor to this driver.
        let owner = unsafe { cursor.as_ref() }
            .and_then(|cursor| cursor.value_owner.clone())
            .unwrap_or_else(crate::owned_value_budget::current_owner);
        Self::with_value_owner(cursor, owner)
    }

    /// Retain the payload's account across asynchronous authored cleanup.
    #[expect(
        clippy::unnecessary_box_returns,
        reason = "Generated callbacks retain driver output addresses"
    )]
    pub(crate) fn with_value_owner(
        cursor: *mut HewReleaseCursor,
        value_owner: Option<crate::owned_value_budget::ValueOwner>,
    ) -> Box<Self> {
        Box::new(Self {
            cursor,
            state: ptr::null_mut(),
            frame: ptr::null_mut(),
            child_fault: ptr::null_mut(),
            fault: ptr::null_mut(),
            done: cursor.is_null(),
            rc_sweep: None,
            active_code: None,
            value_owner,
        })
    }

    /// # Safety
    /// The driver remains boxed and uniquely driven through completion. The
    /// invocation is live through this poll and supplies its retained waker.
    pub(crate) unsafe fn poll(&mut self, parent: *mut crate::coro_state::HewCoroState) -> bool {
        // SAFETY: the caller owns this driver and its parent through the poll.
        let owner = self.value_owner.clone();
        crate::owned_value_budget::with_value_owner(&owner, || unsafe {
            self.poll_inner(parent, None)
        })
    }

    /// Drive a terminal hook with its executing child visible to stop polling.
    /// # Safety
    /// The caller owns the actor activation, driver and parent through the poll.
    pub(crate) unsafe fn poll_terminal_hook(
        &mut self,
        parent: *mut crate::coro_state::HewCoroState,
        actor: &crate::actor::HewActor,
    ) -> bool {
        // SAFETY: activation ownership pins the actor and both invocations.
        let owner = self.value_owner.clone();
        crate::owned_value_budget::with_value_owner(&owner, || unsafe {
            self.poll_inner(parent, Some(actor))
        })
    }

    unsafe fn poll_inner(
        &mut self,
        parent: *mut crate::coro_state::HewCoroState,
        terminal_actor: Option<&crate::actor::HewActor>,
    ) -> bool {
        use crate::coro_state::{
            hew_coro_state_cleanup_child, hew_coro_state_free, hew_coro_state_set_cleanup_fault,
            hew_coro_state_status, CoroStatus,
        };
        // SAFETY: this owner serializes every callback and retains each slot
        // until its child frame has completed and been destroyed.
        unsafe {
            while !self.done {
                if !self.state.is_null() {
                    if hew_coro_state_status(self.state) == CoroStatus::Pending as i32 {
                        if self.frame.is_null() {
                            std::process::abort();
                        }
                        let _binding = terminal_actor.map(|actor| {
                            crate::actor_native::bind_checked_invocation(actor, self.state)
                        });
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
                    // The callback frame is gone; releasing its last carrier
                    // could not retire the code while that frame was active.
                    self.active_code = None;
                    self.fault = crate::fault::hew_fault_combine(
                        self.fault,
                        std::mem::take(&mut self.child_fault),
                    );
                }
                let mut slot = ptr::null_mut();
                let status = hew_release_poll(self.cursor, parent, &raw mut slot);
                if status == 0 {
                    return false;
                }
                if status == 2 {
                    self.rc_sweep = (*self.cursor).rc_sweep.take();
                    hew_release_finish(self.cursor);
                    self.cursor = ptr::null_mut();
                    self.done = true;
                    break;
                }
                let layout = *hew_release_layout(self.cursor);
                if let Some(start) = layout.release_start {
                    self.active_code =
                        Some(crate::code_owner::CodeRoot::acquire(layout.code_owner));
                    if !self.fault.is_null() {
                        hew_coro_state_set_cleanup_fault(parent, self.fault);
                    }
                    self.state = hew_coro_state_cleanup_child(parent);
                    if self.state.is_null() {
                        std::process::abort();
                    }
                    (*self.state).execution_mut().value_owner = self.value_owner.clone();
                    {
                        let _binding = terminal_actor.map(|actor| {
                            crate::actor_native::bind_checked_invocation(actor, self.state)
                        });
                        self.frame =
                            start(slot, (&raw mut self.child_fault).cast(), self.state.cast());
                    }
                    if hew_coro_state_status(self.state) == CoroStatus::Pending as i32 {
                        return false;
                    }
                } else if let Some(drop) = layout.drop_fn {
                    let _code = crate::code_owner::CodeRoot::acquire(layout.code_owner);
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

    pub(crate) fn take_rc_sweep(&mut self) -> Option<hew_observe_protocol::RcSweep> {
        assert!(self.done);
        self.rc_sweep.take()
    }

    pub(crate) fn take_fault(&mut self) -> *mut crate::fault::HewFault {
        if !self.done {
            std::process::abort();
        }
        std::mem::take(&mut self.fault)
    }
}

/// Transfer a typed cursor into the common asynchronous callback driver.
/// # Safety
/// The caller transfers one fresh optional cursor and drives it to completion.
#[no_mangle]
pub unsafe extern "C" fn hew_release_driver_new(cursor: *mut HewReleaseCursor) -> *mut c_void {
    Box::into_raw(ReleaseDriver::new(cursor)).cast()
}

/// # Safety
/// Driver is exclusively retained and parent invocation is live through poll.
#[no_mangle]
pub unsafe extern "C" fn hew_release_driver_poll(
    driver: *mut c_void,
    parent: *mut crate::coro_state::HewCoroState,
) -> i32 {
    // SAFETY: generated cleanup exclusively owns the boxed driver.
    i32::from(unsafe { (&mut *driver.cast::<ReleaseDriver>()).poll(parent) })
}

/// # Safety
/// Driver completed; this transfers its one remaining fault owner.
#[no_mangle]
pub unsafe extern "C" fn hew_release_driver_fault(
    driver: *mut c_void,
) -> *mut crate::fault::HewFault {
    // SAFETY: terminal driver is exclusively retained by its cleanup frame.
    unsafe { (&mut *driver.cast::<ReleaseDriver>()).take_fault() }
}

/// # Safety
/// Driver completed and its fault was taken; the caller relinquishes it.
#[no_mangle]
pub unsafe extern "C" fn hew_release_driver_free(driver: *mut c_void) {
    // SAFETY: the caller transfers this terminal allocation exactly once.
    drop(unsafe { Box::from_raw(driver.cast::<ReleaseDriver>()) });
}

/// Visit the typed obligations still owned by one suspended cleanup driver.
/// # Safety
/// Its provider freezes execution and retains the driver throughout this walk.
#[no_mangle]
pub unsafe extern "C" fn hew_managed_visit_release_driver(
    driver: *mut c_void,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
) -> i32 {
    use crate::owned_value_budget::transfer;
    use hew_cabi::managed::HewManagedWalkKind;
    // SAFETY: the provider lends its exclusive frozen cleanup state.
    unsafe {
        let Some(driver) = driver.cast::<ReleaseDriver>().as_mut() else {
            return 0;
        };
        if (*visitor).kind == HewManagedWalkKind::CommitTransfer {
            driver.value_owner = transfer::destination_owner(visitor);
            if let Some(cursor) = driver.cursor.as_mut() {
                cursor.set_value_owner(driver.value_owner.clone());
            }
        }
        let status =
            crate::owned_value_budget::frame::hew_managed_visit_invocation(driver.state, visitor);
        if status != 0 {
            return status;
        }
        visit_cursor(driver.cursor, visitor, driver.state.is_null())
    }
}

/// Visit the cursor's remaining typed obligations without constructing a driver.
/// # Safety
/// The caller freezes the cursor and owns its current slot unless an active child does.
pub(crate) unsafe fn visit_cursor(
    cursor: *mut HewReleaseCursor,
    visitor: *const hew_cabi::managed::HewManagedStorageVisitor,
    visit_current: bool,
) -> i32 {
    use crate::owned_value_budget::{transfer, visit};
    use hew_cabi::managed::{visit_value, HewManagedWalkKind, UNDESCRIBED};
    // SAFETY: the provider retains and freezes the exact pending cursor items.
    unsafe {
        let Some(cursor) = cursor.as_mut() else {
            return 0;
        };
        if (*visitor).kind == HewManagedWalkKind::CommitTransfer {
            cursor.set_value_owner(transfer::destination_owner(visitor));
        }
        // Once a callback starts, its ordinary invocation occupancy owns the
        // transferred value; the original cursor slot may already be consumed.
        if visit_current {
            if let Some(layout) = cursor.current_layout.as_ref() {
                let status = visit_value(layout, cursor.current_slot, visitor);
                if status != 0 {
                    return status;
                }
            }
        }
        for item in cursor.pending.iter().rev() {
            let status = match item {
                ReleaseItem::Value { slot, layout } => visit_value(layout, *slot, visitor),
                ReleaseItem::String { value } => visit::hew_managed_visit_string(
                    (value as *const *mut hew_cabi::string::HewString).cast(),
                    visitor,
                ),
                ReleaseItem::ManagedBacking { pointer, bytes, .. } => {
                    match visit::storage(visitor, *pointer, *bytes) {
                        hew_cabi::managed::DESCEND | hew_cabi::managed::SKIP_DESCENDANTS => 0,
                        failure => failure,
                    }
                }
                ReleaseItem::Vector { vec } => {
                    visit::hew_managed_visit_vector((vec as *const *mut HewVec).cast(), visitor)
                }
                ReleaseItem::VectorElements { vec, next, end } => {
                    visit::visit_vector_range(*vec, *next, *end, false, visitor)
                }
                ReleaseItem::VectorStorage { vec } => {
                    visit::visit_vector_range(*vec, 0, 0, true, visitor)
                }
                ReleaseItem::Map { map } => visit::hew_managed_visit_map(
                    (map as *const *mut HewLayoutHashMap).cast(),
                    visitor,
                ),
                ReleaseItem::MapSlots { map, next } => {
                    visit::visit_map_range(*map, *next, false, visitor)
                }
                ReleaseItem::MapStorage { map } => {
                    visit::visit_map_range(*map, (**map).cap, true, visitor)
                }
                ReleaseItem::Sink { sink } => crate::stream::sender::hew_managed_visit_sink(
                    (sink as *const *mut crate::stream::HewSink).cast(),
                    visitor,
                ),
                ReleaseItem::RcSurvivors(_) => UNDESCRIBED,
                ReleaseItem::CodeRoot(_)
                | ReleaseItem::Storage { .. }
                | ReleaseItem::Allocation { .. } => 0,
            };
            if status != 0 {
                return status;
            }
        }
        0
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
    cursor.current_slot = ptr::null_mut();
    while let Some(item) = cursor.pending.pop() {
        if let ReleaseItem::Value { slot, layout } = item {
            cursor.current_layout = Some(layout);
            cursor.current_slot = slot;
            return slot;
        }
        // SAFETY: the cursor owns each item and all storage below its children.
        if let Some(summary) = unsafe { expand(item, &mut cursor.pending) } {
            cursor.rc_sweep = Some(summary);
        }
    }
    ptr::null_mut()
}

/// Poll at most one cursor quantum, returning pending (0), a payload (1), or
/// completion (2). Internal work retains the same invocation's wake target.
///
/// # Safety
/// The cursor obeys `hew_release_next`'s ownership contract; `state` is live,
/// and `slot` points to writable pointer storage through this call.
#[no_mangle]
pub unsafe extern "C" fn hew_release_poll(
    cursor: *mut HewReleaseCursor,
    state: *const crate::coro_state::HewCoroState,
    slot: *mut *mut c_void,
) -> i32 {
    // SAFETY: the caller exclusively drives this cursor and owns its slot.
    let cursor = unsafe { &mut *cursor };
    cursor.current_layout = None;
    cursor.current_slot = ptr::null_mut();
    // SAFETY: the caller supplies writable pointer storage.
    unsafe { slot.write(ptr::null_mut()) };
    while !cursor.pending.is_empty() {
        if cursor.remaining == 0 {
            cursor.remaining = STEP_ELEMENTS;
            // SAFETY: the invocation retains this descriptor through polling.
            let waker = unsafe { &*crate::coro_state::hew_coro_state_waker(state) };
            // SAFETY: the retained invocation owns the callback and its context.
            unsafe { (waker.wake)(waker.context) };
            return 0;
        }
        cursor.remaining -= 1;
        let Some(item) = cursor.pending.pop() else {
            break;
        };
        if let ReleaseItem::Value {
            slot: value,
            layout,
        } = item
        {
            cursor.current_layout = Some(layout);
            cursor.current_slot = value;
            // SAFETY: this output slot remains writable through polling.
            unsafe { slot.write(value) };
            return 1;
        }
        // SAFETY: expansion consumes only one cursor-owned structural step.
        if let Some(summary) = unsafe { expand(item, &mut cursor.pending) } {
            cursor.rc_sweep = Some(summary);
        }
    }
    2
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

/// Consume a traversal whose selected value recipes cannot suspend.
/// # Panics
/// Panics if the caller transfers a cursor with an outstanding yielded slot.
/// # Safety
/// The caller transfers the fresh cursor and retains any outer release fault
/// sink. Every descriptor must have no consuming continuation.
#[no_mangle]
pub unsafe extern "C-unwind" fn hew_release_sync(cursor: *mut HewReleaseCursor) {
    if cursor.is_null() {
        return;
    }
    // SAFETY: the caller transfers exclusive ownership of all pending items.
    let cursor = unsafe { Box::from_raw(cursor) };
    assert!(cursor.current_layout.is_none());
    // SAFETY: the caller transfers every pending owner in consuming order.
    let HewReleaseCursor {
        value_owner,
        pending,
        ..
    } = *cursor;
    let owner = value_owner.unwrap_or_else(crate::owned_value_budget::current_owner);
    crate::owned_value_budget::with_value_owner(&owner, || unsafe {
        release_sync_items(pending.into_iter().rev())
    });
}

/// Consume detached obligations in order, retaining faults until all finish.
/// # Safety
/// Every item is uniquely owned and has no consuming continuation.
pub(crate) unsafe fn release_sync_items(items: impl IntoIterator<Item = ReleaseItem>) {
    arm_release_sink();
    for item in items {
        // SAFETY: each detached item and its storage remain owned by this walk.
        unsafe { release_now(item) };
    }
    if let Some((code, fault)) = disarm_release_sink() {
        // SAFETY: all owners are consumed before transferring the retained fault.
        unsafe { crate::fault::hew_fault_trap(code, fault) };
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
        code_owner: core::ptr::null(),
        managed: core::ptr::null(),
        size: size_of::<i64>(),
        align: align_of::<i64>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: None,
        drop_fn: None,

        release_start: Some(selected_release),
    };

    #[test]
    fn joined_cleanup_preserves_retained_account_outside_invocation() {
        use crate::owned_value_budget::{ManagedStorage, ValueOwner};
        unsafe fn free(pointer: *mut c_void) {
            // SAFETY: each cursor transfers its own sized-block allocation.
            unsafe { crate::mem::buf_free(pointer) };
        }
        // SAFETY: both allocations are consumed once by the joined cursor.
        unsafe {
            let owner = ValueOwner::new(Some(17));
            let first = crate::mem::buf_alloc(8);
            let second = crate::mem::buf_alloc(9);
            owner
                .acquire(&[
                    ManagedStorage {
                        identity: first as usize,
                        bytes: 8,
                    },
                    ManagedStorage {
                        identity: second as usize,
                        bytes: 9,
                    },
                ])
                .unwrap()
                .commit();
            let first = HewReleaseCursor::new(vec![ReleaseItem::ManagedBacking {
                pointer: first,
                bytes: 8,
                free,
            }]);
            (*first).set_value_owner(Some(owner.clone()));
            let second = HewReleaseCursor::new(vec![ReleaseItem::ManagedBacking {
                pointer: second,
                bytes: 9,
                free,
            }]);
            (*second).set_value_owner(Some(owner.clone()));
            let joined = HewReleaseCursor::join([first, second]);
            let joined = HewReleaseCursor::join([ptr::null_mut(), joined]);
            assert!(crate::owned_value_budget::current_owner().is_none());
            hew_release_sync(joined);
            assert_eq!(owner.live_bytes(), 0);
        }
    }

    #[test]
    fn cleanup_transfer_retains_flat_payload_and_refunds_destination() {
        use crate::owned_value_budget::{transfer::ValueTransfer, ValueOwner};
        use hew_cabi::managed::{HewManagedStorageVisitor, HewManagedValueDescriptor};
        unsafe extern "C" fn visit_driver(
            slot: *const c_void,
            visitor: *const HewManagedStorageVisitor,
        ) -> i32 {
            // SAFETY: this test lends its pinned, exclusively owned driver handle.
            unsafe { hew_managed_visit_release_driver(slot.cast::<*mut c_void>().read(), visitor) }
        }
        unsafe extern "C-unwind" fn drop_string(slot: *mut c_void) {
            // SAFETY: this exact slot transfers one charged string handle.
            unsafe {
                crate::string::hew_string_drop_managed(
                    slot.cast::<*mut hew_cabi::string::HewString>().read(),
                )
            };
        }
        unsafe fn free(pointer: *mut c_void) {
            // SAFETY: the cursor owns this exact sized-block allocation.
            unsafe { crate::mem::buf_free(pointer) };
        }
        static STRING_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(crate::owned_value_budget::visit::hew_managed_visit_string),
        };
        static STRING: HewValueLayout = HewValueLayout {
            size: size_of::<*mut c_void>(),
            align: align_of::<*mut c_void>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(drop_string),
            release_start: None,
            code_owner: ptr::null(),
            managed: &STRING_MANAGED,
        };
        static DRIVER_MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 0,
            visit: Some(visit_driver),
        };
        static DRIVER: HewValueLayout = HewValueLayout {
            size: size_of::<*mut c_void>(),
            align: align_of::<*mut c_void>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: ptr::null(),
            managed: &DRIVER_MANAGED,
        };
        // SAFETY: each value/backing has one semantic release and stays pinned
        // while the refused and successful transfer inspect the same worklist.
        unsafe {
            let text = hew_cabi::string::string_from_str("test");
            let payload =
                crate::mem::buf_alloc(STRING.size).cast::<*mut hew_cabi::string::HewString>();
            payload.write(text);
            let source = ValueOwner::new(Some(12));
            let cursor = HewReleaseCursor::new(vec![
                ReleaseItem::ManagedBacking {
                    pointer: payload.cast(),
                    bytes: 8,
                    free,
                },
                ReleaseItem::Value {
                    slot: payload.cast(),
                    layout: STRING,
                },
            ]);
            let mut driver = ReleaseDriver::with_value_owner(cursor, Some(source.clone()));
            let handle = ptr::from_mut(driver.as_mut()).cast::<c_void>();
            source
                .acquire_value(&DRIVER, (&raw const handle).cast())
                .unwrap()
                .commit();
            assert_eq!(source.live_bytes(), 12);
            let small = ValueOwner::new(Some(11));
            assert!(ValueTransfer::prepare(
                Some(source.clone()),
                Some(small.clone()),
                &DRIVER,
                (&raw const handle).cast()
            )
            .is_err());
            assert_eq!(source.live_bytes(), 12);
            assert_eq!(small.live_bytes(), 0);
            let destination = ValueOwner::new(Some(12));
            ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                &DRIVER,
                (&raw const handle).cast(),
            )
            .unwrap()
            .commit()
            .unwrap();
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), 12);
            let (_ready, waker) = crate::wake::blocking::Readiness::new();
            let state = crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
            assert!(driver.poll(state));
            assert!(driver.take_fault().is_null());
            crate::coro_state::hew_coro_state_free(state);
            assert_eq!(destination.live_bytes(), 0);
        }
    }

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
            let rc = crate::rc::hew_rc_new(
                (&raw const value).cast(),
                &LAYOUT,
                c"test".as_ptr(),
                ptr::null(),
            );
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
