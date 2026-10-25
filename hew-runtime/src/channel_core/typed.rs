//! Raw typed Pipe slot images. Content channels keep their existing byte
//! envelopes; a typed Pipe never serializes strings or bytes on actor handoff.

use super::{ChannelCore, NativeRegistration};
use crate::owned_value_budget::{current_owner, transfer, ManagedStorage, ValueOwner};
use crate::wake::OwnedWaker;
use core::ffi::c_void;
use core::ptr::NonNull;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS,
    TRANSFER_PENDING,
};
use hew_cabi::value::HewValueLayout;
use std::sync::Arc;

#[derive(Debug)]
pub(super) struct State {
    pub(super) layout: HewValueLayout,
    pub(super) width: u64,
    owner: Option<ValueOwner>,
    binding: Option<transfer::DestinationBinding>,
    frozen: Option<usize>,
    prepared_children: bool,
    active: bool,
    transfer_wakes: Vec<OwnedWaker>,
    root_live: bool,
}

impl ChannelCore {
    /// Create the unique receiving provider before either endpoint is exposed.
    /// # Safety
    /// The compiler retains the exact descriptor through this construction.
    pub(crate) unsafe fn new_typed(capacity: usize, layout: HewValueLayout) -> Arc<Self> {
        let core = Arc::new(Self::new(capacity));
        // SAFETY: the compiler supplies canonical managed metadata for T.
        let width = unsafe { layout.managed.as_ref() }
            .expect("typed Pipe element metadata")
            .slot_bytes;
        let owner = current_owner();
        if let Some(owner) = &owner {
            owner
                .acquire(&[ManagedStorage {
                    identity: Arc::as_ptr(&core) as usize,
                    bytes: 0,
                }])
                .expect("an empty Pipe has no managed payload")
                .commit();
        }
        core.stamp_elem_layout(&layout);
        core.locked().typed = Some(State {
            layout,
            width,
            owner,
            binding: transfer::current_binding(),
            frozen: None,
            prepared_children: false,
            active: false,
            transfer_wakes: Vec::new(),
            root_live: true,
        });
        core
    }

    pub(crate) fn is_typed(&self) -> bool {
        self.locked().typed.is_some()
    }

    fn typed_bytes(&self, count: usize, width: u64) -> Result<u64, i32> {
        width
            .checked_mul(count as u64)
            .ok_or(hew_cabi::managed::HEAP_EXCEEDED)
    }

    fn finish_typed_activity(&self) {
        let (wakes, reader, writers) = {
            let mut inner = self.locked();
            let state = inner.typed.as_mut().expect("typed Pipe activity");
            state.active = false;
            (
                std::mem::take(&mut state.transfer_wakes),
                inner.native_consumer.take(),
                std::mem::take(&mut inner.native_producers),
            )
        };
        for wake in wakes {
            wake.wake();
        }
        if let Some(reader) = reader.and_then(NativeRegistration::into_waker) {
            reader.wake();
        }
        Self::wake_native(writers);
        self.cv.notify_all();
    }

    /// Visit the receiving provider only; producer endpoints do not retain it.
    /// # Safety
    /// A transfer freezes this provider before account traversal. Ordinary
    /// account walks retain that same exclusive receiving Stream lifetime.
    pub(crate) unsafe fn visit_typed(&self, visitor: *const HewManagedStorageVisitor) -> i32 {
        // SAFETY: the owning descriptor pins this callback and its context.
        unsafe {
            let kind = (*visitor).kind;
            let (width, count, live) = {
                let inner = self.locked();
                let Some(state) = &inner.typed else {
                    return 0;
                };
                (state.width, inner.queue.len(), state.root_live)
            };
            if !live {
                return 0;
            }
            let bytes = match self.typed_bytes(count, width) {
                Ok(bytes) => bytes,
                Err(error) => return error,
            };
            match ((*visitor).storage)((*visitor).context, (self as *const Self).cast(), bytes) {
                SKIP_DESCENDANTS => return 0,
                DESCEND => {}
                error => return error,
            }
            let transfer_kind = matches!(
                kind,
                HewManagedWalkKind::PrepareTransfer
                    | HewManagedWalkKind::CommitTransfer
                    | HewManagedWalkKind::AbortTransfer
                    | HewManagedWalkKind::ResumeTransfer
            );
            let key = transfer_kind.then(|| transfer::transfer_identity(visitor) as usize);
            let mut walk_children = true;
            let slots = {
                let mut inner = self.locked();
                let state = inner.typed.as_mut().expect("typed Pipe descriptor");
                if matches!(
                    kind,
                    HewManagedWalkKind::AbortTransfer | HewManagedWalkKind::ResumeTransfer
                ) && state.frozen != key
                {
                    return 0;
                }
                if kind == HewManagedWalkKind::PrepareTransfer {
                    if state.frozen.is_some_and(|held| Some(held) != key) || state.active {
                        if let Some(waker) = transfer::transfer_waker(visitor) {
                            state.transfer_wakes.push(waker);
                        }
                        // Establish this gate before awaiting the active publisher,
                        // so a later producer cannot overtake the transfer.
                        if state.frozen.is_none() {
                            state.frozen = key;
                        }
                        return TRANSFER_PENDING;
                    }
                    state.frozen = key;
                    state.prepared_children = true;
                }
                if kind == HewManagedWalkKind::AbortTransfer {
                    walk_children = state.prepared_children;
                }
                if kind == HewManagedWalkKind::CommitTransfer {
                    state.owner = transfer::destination_owner(visitor);
                    state.binding = transfer::destination_binding(visitor);
                }
                if walk_children {
                    inner
                        .queue
                        .iter()
                        .map(|item| match item {
                            QueueItem::Typed(slot) => (slot.layout, slot.as_ptr()),
                            QueueItem::Content(_) => std::process::abort(),
                        })
                        .collect::<Vec<_>>()
                } else {
                    Vec::new()
                }
            };
            for (layout, slot) in slots {
                let status = visit_value(&layout, slot, visitor);
                if status != 0 {
                    return status;
                }
            }
            if matches!(
                kind,
                HewManagedWalkKind::AbortTransfer | HewManagedWalkKind::ResumeTransfer
            ) {
                let (wakes, transfers) = {
                    let mut inner = self.locked();
                    let state = inner.typed.as_mut().expect("typed Pipe descriptor");
                    if state.frozen == key {
                        state.frozen = None;
                        state.prepared_children = false;
                    }
                    let transfers = std::mem::take(&mut state.transfer_wakes);
                    (std::mem::take(&mut inner.native_producers), transfers)
                };
                Self::wake_native(wakes);
                for wake in transfers {
                    wake.wake();
                }
                self.cv.notify_all();
            }
            0
        }
    }
}
use std::alloc::{alloc, dealloc, handle_alloc_error, Layout};

#[derive(Debug)]
pub(super) enum QueueItem {
    Content(Vec<u8>),
    Typed(Slot),
}

impl From<Vec<u8>> for QueueItem {
    fn from(value: Vec<u8>) -> Self {
        Self::Content(value)
    }
}

impl QueueItem {
    pub(super) fn into_content(self) -> Vec<u8> {
        match self {
            Self::Content(bytes) => bytes,
            Self::Typed(_) => crate::channel_common::abort_elem_witness(
                "typed Pipe",
                "a raw typed slot reached a content-only queue operation",
            ),
        }
    }
}

/// Aligned storage for one ordinary typed value image. Slot ownership is
/// transferred explicitly by queue admission, read take and semantic cleanup;
/// dropping this carrier frees only its representation storage.
#[derive(Debug)]
pub(crate) struct Slot {
    pointer: NonNull<u8>,
    allocation: Layout,
    pub(crate) layout: HewValueLayout,
    _code: crate::code_owner::CodeRoot,
}

impl Slot {
    /// # Safety
    /// Input is an initialized value of this descriptor. The copied image
    /// borrows its owners until the caller commits the semantic transfer.
    pub(crate) unsafe fn copy_image(layout: HewValueLayout, input: *const c_void) -> Self {
        let allocation = Layout::from_size_align(layout.size.max(1), layout.align)
            .expect("verified Pipe element geometry");
        // SAFETY: the descriptor supplies a valid nonzero allocation geometry.
        let pointer = unsafe { alloc(allocation) };
        let pointer = NonNull::new(pointer).unwrap_or_else(|| handle_alloc_error(allocation));
        if layout.size != 0 {
            // SAFETY: input is live and the new aligned allocation is disjoint.
            unsafe {
                core::ptr::copy_nonoverlapping(input.cast::<u8>(), pointer.as_ptr(), layout.size)
            };
        }
        Self {
            pointer,
            allocation,
            layout,
            // SAFETY: the input pins its descriptor's defining code.
            _code: unsafe { crate::code_owner::CodeRoot::acquire(layout.code_owner) },
        }
    }

    pub(crate) fn as_ptr(&self) -> *mut c_void {
        self.pointer.as_ptr().cast()
    }
}

impl Drop for Slot {
    fn drop(&mut self) {
        // SAFETY: this carrier exclusively owns the exact aligned allocation.
        unsafe { dealloc(self.pointer.as_ptr(), self.allocation) };
    }
}

// SAFETY: a queued slot has one owner; access is serialized by its Pipe core.
unsafe impl Send for Slot {}

/// A reserved, unpublished send borrowing its enclosing operation's typed slot.
/// The parent remains responsible for that slot until poll returns acceptance.
pub(crate) struct TypedWrite {
    core: Arc<ChannelCore>,
    input: *mut c_void,
    layout: HewValueLayout,
    parent: *const crate::coro_state::HewCoroState,
    waker: Arc<OwnedWaker>,
    transfer: Option<transfer::ValueTransfer>,
    backing: Option<crate::owned_value_budget::BackingChange>,
    reserved: bool,
    accepted: bool,
}

impl TypedWrite {
    /// # Safety
    /// The retained parent pins its initialized input until acceptance/cleanup.
    pub(crate) unsafe fn new(
        core: Arc<ChannelCore>,
        input: *mut c_void,
        layout: HewValueLayout,
        parent: *const crate::coro_state::HewCoroState,
        waker: Arc<OwnedWaker>,
    ) -> Box<Self> {
        Box::new(Self {
            core,
            input,
            layout,
            parent,
            waker,
            transfer: None,
            backing: None,
            reserved: false,
            accepted: false,
        })
    }

    pub(crate) fn cancel(&mut self) {
        // Aborting nested provider gates must precede releasing core admission.
        self.transfer.take();
        self.backing.take();
        if self.reserved {
            self.reserved = false;
            self.core.finish_typed_activity();
        }
    }

    pub(crate) fn committed(&self) -> bool {
        self.accepted
    }

    /// Quiesce the child protocol before the parent's input graph is visited.
    /// # Safety
    /// The operation retains this receipt and its input through the walk.
    pub(crate) unsafe fn visit_protocol(
        &mut self,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        if unsafe { (*visitor).kind } == HewManagedWalkKind::PrepareTransfer {
            self.cancel();
        }
        0
    }

    /// Reserve actual space before acquiring the recipient's typed graph.
    pub(crate) fn poll(&mut self) -> i32 {
        if self.accepted {
            return 1;
        }
        if !self.reserved {
            let (owner, binding, old, width) = {
                let mut inner = self.core.locked();
                if inner.stream_closed || inner.sink_closed || inner.sink_fault {
                    return 2;
                }
                let state = inner.typed.as_ref().expect("typed Pipe write");
                if state.active || state.frozen.is_some() || inner.queue.len() >= inner.capacity {
                    inner
                        .native_producers
                        .push(NativeRegistration::Observe(Arc::downgrade(&self.waker)));
                    return 0;
                }
                let old = match self.core.typed_bytes(inner.queue.len(), state.width) {
                    Ok(bytes) => bytes,
                    Err(_) => {
                        std::process::abort();
                    }
                };
                let state = inner.typed.as_mut().expect("typed Pipe write");
                state.active = true;
                let snapshot = (state.owner.clone(), state.binding.clone(), old, state.width);
                inner
                    .native_producers
                    .push(NativeRegistration::Observe(Arc::downgrade(&self.waker)));
                snapshot
            };
            self.reserved = true;
            if let Some(owner) = &owner {
                let Some(bytes) = old.checked_add(width) else {
                    return self.refuse(owner.clone());
                };
                match owner.prepare_backing_change(
                    Some(ManagedStorage {
                        identity: Arc::as_ptr(&self.core) as usize,
                        bytes: old,
                    }),
                    bytes,
                    true,
                ) {
                    Ok(backing) => self.backing = Some(backing),
                    Err(_) => return self.refuse(owner.clone()),
                }
            }
            // SAFETY: the enclosing operation retains its parent and input;
            // this reservation prevents transfer of the receiving provider.
            let source =
                unsafe { crate::owned_value_budget::materialize::invocation_owner(self.parent) };
            let mut transfer = unsafe {
                transfer::ValueTransfer::begin(source, owner, &raw const self.layout, self.input)
            };
            transfer.set_destination_binding(binding);
            transfer.set_waker((*self.waker).clone());
            self.transfer = Some(transfer);
        }
        // A close while providers were quiescing refuses before ownership moves.
        if self.core.locked().stream_closed {
            self.cancel();
            return 2;
        }
        // SAFETY: the source and destination remain pinned by this receipt.
        match unsafe {
            self.transfer
                .as_mut()
                .expect("reserved Pipe transfer")
                .poll_prepare()
        } {
            Ok(false) => return 0,
            Err(error) => {
                let owner = self
                    .core
                    .locked()
                    .typed
                    .as_ref()
                    .and_then(|s| s.owner.clone());
                self.cancel();
                if error == hew_cabi::managed::HEAP_EXCEEDED {
                    self.core.prepare_typed_close(None);
                    if let Some(owner) = owner {
                        owner.heap_exceeded();
                    }
                    return 2;
                }
                crate::channel_common::abort_elem_witness(
                    "typed Pipe write",
                    "typed transfer descriptor failed",
                );
            }
            Ok(true) => {}
        }
        if self.core.locked().stream_closed {
            self.cancel();
            return 2;
        }
        // The active gate linearizes this accepted send before any later close.
        // The unpublished image borrows owners until the common graph commit.
        let slot = unsafe { Slot::copy_image(self.layout, self.input) };
        if unsafe {
            self.transfer
                .take()
                .expect("prepared Pipe transfer")
                .commit()
        }
        .is_err()
        {
            std::process::abort();
        }
        if let Some(backing) = self.backing.take() {
            backing.commit(Arc::as_ptr(&self.core) as usize);
        }
        self.core.locked().queue.push_back(QueueItem::Typed(slot));
        self.accepted = true;
        self.reserved = false;
        self.core.finish_typed_activity();
        1
    }

    fn refuse(&mut self, owner: ValueOwner) -> i32 {
        self.cancel();
        self.core.prepare_typed_close(None);
        owner.heap_exceeded();
        2
    }
}
impl Drop for TypedWrite {
    fn drop(&mut self) {
        self.cancel();
    }
}
// SAFETY: the enclosing invocation serializes polling and pins the borrowed slot.
unsafe impl Send for TypedWrite {}

/// An acquired read owns one shallow slot independently of the receiving queue.
pub(crate) struct TypedRead {
    core: Arc<ChannelCore>,
    waker: Option<Arc<OwnedWaker>>,
    slot: Option<Slot>,
    owner: Option<ValueOwner>,
}
impl TypedRead {
    pub(crate) fn new(core: Arc<ChannelCore>, waker: Arc<OwnedWaker>) -> Box<Self> {
        Self::unobserved(core, Some(waker))
    }
    fn unobserved(core: Arc<ChannelCore>, waker: Option<Arc<OwnedWaker>>) -> Box<Self> {
        Box::new(Self {
            core,
            waker,
            slot: None,
            owner: None,
        })
    }
    pub(crate) fn poll(&mut self) -> i32 {
        if self.slot.is_some() {
            return 1;
        }
        let (slot, owner, old, width) = {
            let mut inner = self.core.locked();
            let state = inner.typed.as_ref().expect("typed Pipe read");
            if state.active || state.frozen.is_some() {
                if let Some(waker) = &self.waker {
                    inner.native_consumer =
                        Some(NativeRegistration::Observe(Arc::downgrade(waker)));
                }
                return 0;
            }
            if inner.queue.is_empty() {
                if inner.sink_fault {
                    return 3;
                }
                if inner.sink_closed || inner.stream_closed {
                    return 2;
                }
                if let Some(waker) = &self.waker {
                    inner.native_consumer =
                        Some(NativeRegistration::Observe(Arc::downgrade(waker)));
                }
                return 0;
            }
            let old = self
                .core
                .typed_bytes(inner.queue.len(), state.width)
                .expect("retained Pipe width");
            let state = inner.typed.as_mut().expect("typed Pipe read");
            state.active = true;
            let owner = state.owner.clone();
            let width = state.width;
            let Some(QueueItem::Typed(slot)) = inner.queue.pop_front() else {
                std::process::abort();
            };
            (slot, owner, old, width)
        };
        if let Some(owner) = &owner {
            owner
                .repartition_backing(
                    ManagedStorage {
                        identity: Arc::as_ptr(&self.core) as usize,
                        bytes: old,
                    },
                    &[
                        ManagedStorage {
                            identity: Arc::as_ptr(&self.core) as usize,
                            bytes: old - width,
                        },
                        ManagedStorage {
                            identity: self as *const Self as usize,
                            bytes: width,
                        },
                    ],
                )
                .expect("a Pipe read preserves its exact managed payload");
        }
        self.owner = owner;
        self.slot = Some(slot);
        self.core.finish_typed_activity();
        1
    }
    pub(crate) fn cancel(&mut self) {
        self.waker = None;
    }
    pub(crate) fn committed(&self) -> bool {
        self.slot.is_some()
    }

    /// # Safety
    /// Output is writable storage of the exact element descriptor.
    pub(crate) unsafe fn take(&mut self, out: *mut c_void) -> bool {
        let Some(slot) = self.slot.take() else {
            return false;
        };
        if slot.layout.size != 0 {
            unsafe {
                core::ptr::copy_nonoverlapping(
                    slot.as_ptr().cast::<u8>(),
                    out.cast::<u8>(),
                    slot.layout.size,
                )
            };
        }
        if let Some(owner) = &self.owner {
            let width = unsafe { (*slot.layout.managed).slot_bytes };
            owner.release_backing((self as *const Self).cast(), width);
        }
        true
    }

    /// # Safety
    /// This receipt and its initialized value stay pinned for the walk.
    pub(crate) unsafe fn visit(&mut self, visitor: *const HewManagedStorageVisitor) -> i32 {
        let Some(slot) = &self.slot else {
            return 0;
        };
        let width = unsafe { (*slot.layout.managed).slot_bytes };
        match unsafe {
            ((*visitor).storage)((*visitor).context, (self as *const Self).cast(), width)
        } {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            error => return error,
        }
        if unsafe { (*visitor).kind } == HewManagedWalkKind::CommitTransfer {
            self.owner = unsafe { transfer::destination_owner(visitor) };
        }
        unsafe { visit_value(&slot.layout, slot.as_ptr(), visitor) }
    }

    pub(crate) fn release_begin(self: Box<Self>) -> *mut crate::release_walker::HewReleaseCursor {
        use crate::release_walker::{HewReleaseCursor, ReleaseItem};
        if self.slot.is_none() {
            return core::ptr::null_mut();
        }
        let owner = self.owner.clone();
        let ptr = Box::into_raw(self);
        // SAFETY: the cursor now uniquely owns the stable receipt and image.
        let slot = unsafe { (*ptr).slot.as_ref().expect("acquired Pipe read") };
        let width = unsafe { (*slot.layout.managed).slot_bytes };
        unsafe fn free(owner: *mut c_void) {
            unsafe { drop(Box::from_raw(owner.cast::<TypedRead>())) };
        }
        unsafe fn no_free(_: *mut c_void) {}
        let cursor = HewReleaseCursor::new(vec![
            ReleaseItem::Storage {
                owner: ptr.cast(),
                free,
            },
            ReleaseItem::ManagedBacking {
                pointer: ptr.cast(),
                bytes: width,
                free: no_free,
            },
            ReleaseItem::Value {
                slot: slot.as_ptr(),
                layout: slot.layout,
            },
        ]);
        unsafe { (*cursor).set_value_owner(owner) };
        cursor
    }
}
// SAFETY: one enclosing operation exclusively polls or transfers the receipt.
unsafe impl Send for TypedRead {}

impl ChannelCore {
    pub(crate) fn typed_owner(&self) -> Option<ValueOwner> {
        self.locked()
            .typed
            .as_ref()
            .and_then(|state| state.owner.clone())
    }

    /// Close admission, then await the one unpublished sender without holding
    /// the queue lock across provider abort, cleanup or scheduler notification.
    pub(crate) fn prepare_typed_close(&self, waker: Option<OwnedWaker>) -> bool {
        let (ready, writers) = {
            let mut inner = self.locked();
            inner.stream_closed = true;
            let state = inner.typed.as_mut().expect("typed Pipe close");
            let ready = !state.active && state.frozen.is_none();
            if !ready {
                if let Some(waker) = waker {
                    state.transfer_wakes.push(waker);
                }
            }
            (ready, std::mem::take(&mut inner.native_producers))
        };
        Self::wake_native(writers);
        self.cv.notify_all();
        ready
    }

    /// Transfer the aggregate root and queued slots to ordinary semantic
    /// cleanup. Each aligned image remains pinned until its children finish.
    pub(crate) fn expand_typed_close(&self, pending: &mut Vec<crate::release_walker::ReleaseItem>) {
        use crate::release_walker::ReleaseItem;
        let (slots, bytes) = {
            let mut inner = self.locked();
            let state = inner.typed.as_mut().expect("typed Pipe cleanup");
            assert!(!state.active && state.frozen.is_none());
            if !state.root_live {
                return;
            }
            state.root_live = false;
            let width = state.width;
            let bytes = self
                .typed_bytes(inner.queue.len(), width)
                .expect("retained Pipe bytes");
            let slots = inner
                .queue
                .drain(..)
                .map(|item| match item {
                    QueueItem::Typed(slot) => slot,
                    QueueItem::Content(_) => std::process::abort(),
                })
                .collect::<Vec<_>>();
            (slots, bytes)
        };
        let slots = Box::into_raw(Box::new(slots));
        unsafe fn free(owner: *mut c_void) {
            unsafe { drop(Box::from_raw(owner.cast::<Vec<Slot>>())) };
        }
        unsafe fn no_free(_: *mut c_void) {}
        pending.push(ReleaseItem::Storage {
            owner: slots.cast(),
            free,
        });
        pending.push(ReleaseItem::ManagedBacking {
            pointer: (self as *const Self).cast_mut().cast(),
            bytes,
            free: no_free,
        });
        // SAFETY: the cursor retains this vector unchanged until all values close.
        for slot in unsafe { &*slots }.iter().rev() {
            pending.push(ReleaseItem::Value {
                slot: slot.as_ptr(),
                layout: slot.layout,
            });
        }
    }

    pub(crate) fn close_typed_sync(&self) {
        use crate::release_walker::HewReleaseCursor;
        if !self.prepare_typed_close(None) {
            crate::channel_common::abort_elem_witness(
                "typed Pipe close",
                "synchronous close cannot drain an active provider",
            );
        }
        let mut pending = Vec::new();
        self.expand_typed_close(&mut pending);
        let cursor = HewReleaseCursor::new(pending);
        // SAFETY: the queue's current account and pinned images move to this cursor.
        unsafe {
            (*cursor).set_value_owner(self.typed_owner());
            crate::release_walker::hew_release_sync(cursor);
        }
    }

    /// Content adapters retain their established byte-oriented contract; only
    /// this explicit host boundary encodes a raw typed Pipe element.
    pub(crate) fn typed_content_recv(self: &Arc<Self>, blocking: bool) -> Option<Vec<u8>> {
        let mut read = TypedRead::unobserved(self.clone(), None);
        loop {
            match read.poll() {
                1 => break,
                0 if blocking => {
                    let guard = self.locked();
                    let _ = self
                        .cv
                        .wait_timeout(guard, std::time::Duration::from_millis(10));
                }
                0 | 2 => return None,
                3 => panic!("{}", self.fault_message()),
                _ => unreachable!(),
            }
        }
        let slot = read.slot.as_ref().expect("received typed image");
        // SAFETY: encoding borrows the exact live value; its original typed
        // ownership then follows the ordinary receiving cleanup recipe.
        let content = unsafe {
            crate::channel_common::encode_elem_envelope(
                slot.as_ptr(),
                &slot.layout,
                "typed Pipe content adapter",
            )
        };
        unsafe { crate::release_walker::hew_release_sync(read.release_begin()) };
        Some(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::owned_value_budget::with_value_owner;
    use hew_cabi::managed::HewManagedValueDescriptor;
    use hew_cabi::vec::HewTypeOwnershipKind;

    static PLAIN: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: None,
    };
    static STRING: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(crate::owned_value_budget::visit::hew_managed_visit_string),
    };
    unsafe extern "C-unwind" fn drop_string(slot: *mut c_void) {
        // A queue's shallow payload stays charged through child cleanup.
        let owner = current_owner().expect("typed cleanup owner");
        assert!(owner.live_bytes() >= 8 + 6);
        unsafe {
            crate::string::hew_string_drop_managed(*slot.cast::<*mut hew_cabi::string::HewString>())
        };
    }
    fn layout(string: bool) -> HewValueLayout {
        HewValueLayout {
            code_owner: core::ptr::null(),
            managed: if string { &STRING } else { &PLAIN },
            release_start: None,
            size: 8,
            align: 8,
            ownership_kind: if string {
                HewTypeOwnershipKind::String
            } else {
                HewTypeOwnershipKind::Plain
            },
            clone_fn: None,
            drop_fn: string.then_some(drop_string),
        }
    }
    fn waker() -> Arc<OwnedWaker> {
        unsafe extern "C" fn no_op(_: *mut c_void) {}
        Arc::new(unsafe {
            OwnedWaker::retain(&crate::wake::HewWaker {
                context: core::ptr::null_mut(),
                wake: no_op,
                retain: no_op,
                release: no_op,
            })
        })
    }
    fn core(owner: &ValueOwner, capacity: usize, layout: HewValueLayout) -> Arc<ChannelCore> {
        with_value_owner(&Some(owner.clone()), || unsafe {
            ChannelCore::new_typed(capacity, layout)
        })
    }
    fn send(core: Arc<ChannelCore>, value: *mut c_void, layout: HewValueLayout) -> Box<TypedWrite> {
        unsafe { TypedWrite::new(core, value, layout, core::ptr::null(), waker()) }
    }

    #[test]
    fn typed_pipe_preserves_string_identity_and_receiver_charge_through_cleanup() {
        let source = ValueOwner::new(Some(6));
        let receiver = ValueOwner::new(Some(14));
        let layout = layout(true);
        let core = core(&receiver, 1, layout);
        let mut value = hew_cabi::string::string_from_str("retain");
        unsafe {
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit()
        };
        let mut write = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 1);
        assert_eq!(source.live_bytes(), 0);
        assert_eq!(receiver.live_bytes(), 14);
        let inner = core.locked();
        let Some(QueueItem::Typed(slot)) = inner.queue.front() else {
            panic!("typed value");
        };
        assert_eq!(
            unsafe { *slot.as_ptr().cast::<*mut hew_cabi::string::HewString>() },
            value
        );
        drop(inner);
        core.close_typed_sync();
        assert_eq!(receiver.live_bytes(), 0);
    }

    #[test]
    fn typed_read_repartitions_at_limit_and_take_refunds_only_shallow_slot() {
        let owner = ValueOwner::new(Some(8));
        let layout = layout(false);
        let core = core(&owner, 1, layout);
        let mut value = 41_i64;
        assert_eq!(
            with_value_owner(&Some(owner.clone()), || send(
                core.clone(),
                (&raw mut value).cast(),
                layout
            )
            .poll()),
            1
        );
        let mut second = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(owner.clone()), || second.poll()), 0);
        assert!(!core.locked().stream_closed);
        let mut read = TypedRead::new(core.clone(), waker());
        assert_eq!(read.poll(), 1);
        assert_eq!(owner.live_bytes(), 8);
        let mut out = 0_i64;
        assert!(unsafe { read.take((&raw mut out).cast()) });
        assert_eq!(out, 41);
        assert_eq!(owner.live_bytes(), 0);
        assert_eq!(with_value_owner(&Some(owner.clone()), || second.poll()), 1);
        core.close_typed_sync();
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn typed_pipe_refusal_preserves_source_and_closes_recipient_admission() {
        let receiver = ValueOwner::new(Some(13));
        let source = ValueOwner::new(Some(6));
        let layout = layout(true);
        let core = core(&receiver, 1, layout);
        let mut value = hew_cabi::string::string_from_str("retain");
        unsafe {
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit()
        };
        assert_eq!(
            with_value_owner(&Some(source.clone()), || send(
                core.clone(),
                (&raw mut value).cast(),
                layout
            )
            .poll()),
            2
        );
        assert_eq!(source.live_bytes(), 6);
        assert_eq!(receiver.live_bytes(), 0);
        assert!(core.locked().stream_closed);
        assert_eq!(
            unsafe { hew_cabi::string::string_as_bytes(value) },
            b"retain"
        );
        with_value_owner(&Some(source.clone()), || unsafe {
            crate::string::hew_string_drop_managed(value)
        });
        core.close_typed_sync();
        assert_eq!(source.live_bytes(), 0);
    }

    unsafe extern "C" fn visit_stream(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        unsafe { (&**slot.cast::<*const ChannelCore>()).visit_typed(visitor) }
    }
    static STREAM: HewManagedValueDescriptor = HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(visit_stream),
    };
    #[test]
    fn typed_stream_transfer_adopts_queued_graph_without_sink_membership() {
        let source = ValueOwner::new(Some(14));
        let receiver = ValueOwner::new(Some(14));
        let element = layout(true);
        let core = core(&source, 1, element);
        let mut value = hew_cabi::string::string_from_str("retain");
        unsafe {
            source
                .acquire_value(&element, (&raw const value).cast())
                .unwrap()
                .commit()
        };
        assert_eq!(
            with_value_owner(&Some(source.clone()), || send(
                core.clone(),
                (&raw mut value).cast(),
                element
            )
            .poll()),
            1
        );
        let stream = Arc::as_ptr(&core);
        let mut descriptor = layout(false);
        descriptor.managed = &STREAM;
        let transfer = unsafe {
            transfer::ValueTransfer::prepare(
                Some(source.clone()),
                Some(receiver.clone()),
                &descriptor,
                (&raw const stream).cast(),
            )
        }
        .unwrap();
        unsafe { transfer.commit() }.unwrap();
        assert_eq!(source.live_bytes(), 0);
        assert_eq!(receiver.live_bytes(), 14);
        core.close_typed_sync();
        assert_eq!(receiver.live_bytes(), 0);
    }
}
