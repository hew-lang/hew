//! Raw typed Pipe slot images. Content channels keep their existing byte
//! envelopes; a typed Pipe never serializes strings or bytes on actor handoff.

use super::{ChannelCore, NativeRegistration};
use crate::owned_value_budget::{current_owner, transfer, ManagedStorage, ValueOwner};
use crate::wake::OwnedWaker;
use core::ffi::c_void;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS,
    TRANSFER_PENDING,
};
use hew_cabi::value::HewValueLayout;
use std::sync::Arc;

#[derive(Debug)]
pub(super) struct State {
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

    fn typed_bytes(count: usize, width: u64) -> Result<u64, i32> {
        width
            .checked_mul(count as u64)
            .ok_or(hew_cabi::managed::HEAP_EXCEEDED)
    }

    fn finish_typed_activity(&self) {
        let (wakes, reader, writers, notify_foreign) = {
            let mut inner = self.locked();
            let state = inner.typed.as_mut().expect("typed Pipe activity");
            state.active = false;
            (
                std::mem::take(&mut state.transfer_wakes),
                inner.native_consumer.take(),
                std::mem::take(&mut inner.native_producers),
                inner.foreign_waiters != 0,
            )
        };
        for wake in wakes {
            wake.wake();
        }
        if let Some(reader) = reader.and_then(NativeRegistration::into_waker) {
            reader.wake();
        }
        Self::wake_native(writers);
        if notify_foreign {
            self.cv.notify_all();
        }
    }

    fn finish_typed_transfer(&self, key: Option<usize>) {
        let (wakes, transfers, notify_foreign) = {
            let mut inner = self.locked();
            let state = inner.typed.as_mut().expect("typed Pipe descriptor");
            if state.frozen == key {
                state.frozen = None;
                state.prepared_children = false;
            }
            let transfers = std::mem::take(&mut state.transfer_wakes);
            (
                std::mem::take(&mut inner.native_producers),
                transfers,
                inner.foreign_waiters != 0,
            )
        };
        Self::wake_native(wakes);
        for wake in transfers {
            wake.wake();
        }
        if notify_foreign {
            self.cv.notify_all();
        }
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
            let bytes = match Self::typed_bytes(count, width) {
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
                self.finish_typed_transfer(key);
            }
            0
        }
    }
}
pub(crate) use super::item::{QueueItem, Slot};

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

    /// Quiesce the child protocol before the parent's input graph is visited.
    /// # Safety
    /// The operation retains this receipt and its input through the walk.
    pub(crate) unsafe fn visit_protocol(
        &mut self,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: the enclosing operation pins this synchronous visitor.
        if unsafe { (*visitor).kind } == HewManagedWalkKind::PrepareTransfer {
            self.cancel();
        }
        0
    }

    fn reserve(&mut self) -> Result<(), i32> {
        let (owner, binding, old, width) =
            {
                let mut inner = self.core.locked();
                if inner.stream_closed || inner.sink_closed || inner.sink_fault {
                    return Err(2);
                }
                let state = inner.typed.as_ref().expect("typed Pipe write");
                if state.active || state.frozen.is_some() || inner.queue.len() >= inner.capacity {
                    inner.native_producers.push(NativeRegistration::Observe(
                        Arc::<OwnedWaker>::downgrade(&self.waker),
                    ));
                    return Err(0);
                }
                let Ok(old) = ChannelCore::typed_bytes(inner.queue.len(), state.width) else {
                    std::process::abort();
                };
                let state = inner.typed.as_mut().expect("typed Pipe write");
                state.active = true;
                let snapshot = (state.owner.clone(), state.binding.clone(), old, state.width);
                inner.native_producers.push(NativeRegistration::Observe(
                    Arc::<OwnedWaker>::downgrade(&self.waker),
                ));
                snapshot
            };
        self.reserved = true;
        if let Some(owner) = &owner {
            let Some(bytes) = old.checked_add(width) else {
                return Err(self.refuse(owner));
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
                Err(_) => return Err(self.refuse(owner)),
            }
        }
        // SAFETY: the enclosing operation retains its parent and input;
        // this reservation prevents transfer of the receiving provider.
        let source =
            unsafe { crate::owned_value_budget::materialize::invocation_owner(self.parent) };
        // SAFETY: the parent pins the same initialized input until commit/drop.
        let mut transfer = unsafe {
            transfer::ValueTransfer::begin(source, owner, &raw const self.layout, self.input)
        };
        transfer.set_destination_binding(binding);
        transfer.set_waker((*self.waker).clone());
        self.transfer = Some(transfer);
        Ok(())
    }

    fn channel_closed(&self) -> bool {
        let inner = self.core.locked();
        inner.stream_closed || inner.sink_closed || inner.sink_fault
    }

    /// Reserve actual space before acquiring the recipient's typed graph.
    pub(crate) fn poll(&mut self) -> i32 {
        if self.accepted {
            return 1;
        }
        if !self.reserved {
            if let Err(status) = self.reserve() {
                return status;
            }
        }
        // Every terminal edge refuses unpublished input while providers quiesce.
        if self.channel_closed() {
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
        if self.channel_closed() {
            self.cancel();
            return 2;
        }
        // The active gate linearizes this accepted send before any later close.
        // The unpublished image borrows owners until the common graph commit.
        // SAFETY: reservation pins the unchanged input and its exact layout.
        let slot = unsafe { Slot::copy_image(self.layout, self.input) };
        // SAFETY: capacity and destination acquisition are accepted; publish follows resume.
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

    fn refuse(&mut self, owner: &ValueOwner) -> i32 {
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
    #[expect(
        clippy::unnecessary_box_returns,
        reason = "The acquired receipt address is its stable ledger identity."
    )]
    pub(crate) fn new(core: Arc<ChannelCore>, waker: Arc<OwnedWaker>) -> Box<Self> {
        Self::unobserved(core, Some(waker))
    }
    #[expect(
        clippy::unnecessary_box_returns,
        reason = "The acquired receipt address is its stable ledger identity."
    )]
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
                    inner.native_consumer = Some(NativeRegistration::Observe(
                        Arc::<OwnedWaker>::downgrade(waker),
                    ));
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
                    inner.native_consumer = Some(NativeRegistration::Observe(
                        Arc::<OwnedWaker>::downgrade(waker),
                    ));
                }
                return 0;
            }
            let old = ChannelCore::typed_bytes(inner.queue.len(), state.width)
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
    pub(crate) fn fault_message(&self) -> Option<String> {
        let faulted = self.core.locked().sink_fault;
        faulted.then(|| self.core.fault_message())
    }

    pub(crate) fn cancel(&mut self) {
        self.waker = None;
    }

    /// # Safety
    /// Output is writable storage of the exact element descriptor.
    pub(crate) unsafe fn take(&mut self, out: *mut c_void) -> bool {
        let Some(slot) = self.slot.take() else {
            return false;
        };
        if slot.layout.size != 0 {
            // SAFETY: this receipt owns the exact image and output is disjoint writable storage.
            unsafe {
                core::ptr::copy_nonoverlapping(
                    slot.as_ptr().cast::<u8>(),
                    out.cast::<u8>(),
                    slot.layout.size,
                );
            };
        }
        if let Some(owner) = &self.owner {
            // SAFETY: the descriptor/code root stays retained by the slot.
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
        // SAFETY: the acquired slot retains canonical managed metadata.
        let width = unsafe { (*slot.layout.managed).slot_bytes };
        // SAFETY: the enclosing provider pins this synchronous visitor and receipt.
        match unsafe {
            ((*visitor).storage)((*visitor).context, (self as *const Self).cast(), width)
        } {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            error => return error,
        }
        // SAFETY: the live visitor lends the explicit transfer phase.
        if unsafe { (*visitor).kind } == HewManagedWalkKind::CommitTransfer {
            // SAFETY: only the runtime's CommitTransfer visitor reaches this branch.
            self.owner = unsafe { transfer::destination_owner(visitor) };
        }
        // SAFETY: the receipt pins its initialized image and defining layout.
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
        // SAFETY: the retained image pins canonical metadata until cursor retirement.
        let width = unsafe { (*slot.layout.managed).slot_bytes };
        let cursor = HewReleaseCursor::new(vec![
            ReleaseItem::Storage {
                owner: ptr.cast(),
                free: free_read,
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
        // SAFETY: this fresh cursor remains exclusively owned before publication.
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
        let (ready, writers, actor_stream_cancellation, notify_foreign) = {
            let mut inner = self.locked();
            inner.stream_closed = true;
            let state = inner.typed.as_mut().expect("typed Pipe close");
            let ready = !state.active && state.frozen.is_none();
            if !ready {
                if let Some(waker) = waker {
                    state.transfer_wakes.push(waker);
                }
            }
            (
                ready,
                std::mem::take(&mut inner.native_producers),
                inner.actor_stream_cancellation.take(),
                inner.foreign_waiters != 0,
            )
        };
        if let Some(cancellation) = actor_stream_cancellation {
            cancellation.close_reader();
        }
        Self::wake_native(writers);
        if notify_foreign {
            self.cv.notify_all();
        }
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
            let bytes = Self::typed_bytes(inner.queue.len(), width).expect("retained Pipe bytes");
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
        pending.push(ReleaseItem::Storage {
            owner: slots.cast(),
            free: free_slots,
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
                    let mut guard = self.locked();
                    let state = guard.typed.as_ref().expect("typed content read");
                    if !state.active
                        && state.frozen.is_none()
                        && (!guard.queue.is_empty()
                            || guard.sink_closed
                            || guard.stream_closed
                            || guard.sink_fault)
                    {
                        continue;
                    }
                    // Register under the publication predicate lock, before wait releases it.
                    guard.foreign_waiters += 1;
                    #[cfg(test)]
                    self.rendezvous_before_wait();
                    let (mut guard, _) = self
                        .cv
                        .wait_timeout(guard, std::time::Duration::from_millis(10))
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    guard.foreign_waiters -= 1;
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
        // SAFETY: this synchronous foreign adapter owns the acquired receipt and selected recipe.
        unsafe { crate::release_walker::hew_release_sync(read.release_begin()) };
        Some(content)
    }
}

impl ChannelCore {
    /// Legacy foreign content writers create a typed image only at this
    /// explicit boundary. Generated source uses the borrowing checked writer.
    pub(crate) fn typed_content_send(self: &Arc<Self>, content: &[u8]) {
        unsafe extern "C" fn no_op(_: *mut c_void) {}
        if current_owner().is_some() {
            crate::channel_common::abort_elem_witness(
                "typed Pipe content writer",
                "a bounded invocation requires the checked typed operation",
            );
        }
        let layout = self
            .locked()
            .elem_layout
            .expect("typed Pipe element layout");
        let zero = vec![0_u8; layout.size];
        // SAFETY: the representation is initialized by decode before publication.
        let slot = unsafe { Slot::copy_image(layout, zero.as_ptr().cast()) };
        // SAFETY: decode initializes fresh aligned storage of this exact descriptor.
        if unsafe {
            crate::channel_common::decode_elem_envelope(
                Some(content.to_vec()),
                slot.as_ptr(),
                &layout,
                "typed Pipe content writer",
            )
        } != 1
        {
            return;
        }
        // SAFETY: these static no-op callbacks never dereference the null context.
        let waker = Arc::new(unsafe {
            OwnedWaker::retain(&crate::wake::HewWaker {
                context: core::ptr::null_mut(),
                wake: no_op,
                retain: no_op,
                release: no_op,
            })
        });
        // SAFETY: this synchronous frame retains its fresh image until terminal completion.
        let mut write = unsafe {
            TypedWrite::new(
                self.clone(),
                slot.as_ptr(),
                layout,
                core::ptr::null(),
                waker,
            )
        };
        loop {
            match write.poll() {
                1 => return,
                0 => {
                    let mut guard = self.locked();
                    let state = guard.typed.as_ref().expect("typed content write");
                    if guard.sink_closed
                        || guard.stream_closed
                        || guard.sink_fault
                        || (!state.active
                            && state.frozen.is_none()
                            && guard.queue.len() < guard.capacity)
                    {
                        continue;
                    }
                    // Register under the publication predicate lock, before wait releases it.
                    guard.foreign_waiters += 1;
                    #[cfg(test)]
                    self.rendezvous_before_wait();
                    let (mut guard, _) = self
                        .cv
                        .wait_timeout(guard, std::time::Duration::from_millis(10))
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    guard.foreign_waiters -= 1;
                }
                2 => {
                    // SAFETY: no semantic transfer occurred; the foreign-created image
                    // still owns its children independently of the receiver.
                    unsafe {
                        crate::release_walker::hew_release_sync(
                            crate::release_walker::HewReleaseCursor::detached(
                                slot.as_ptr(),
                                layout,
                            ),
                        );
                    };
                    return;
                }
                _ => unreachable!(),
            }
        }
    }
}

unsafe fn no_free(_: *mut c_void) {}
unsafe fn free_read(owner: *mut c_void) {
    // SAFETY: the consuming cursor drained all typed children before retiring this unique Box.
    unsafe { drop(Box::from_raw(owner.cast::<TypedRead>())) };
}
unsafe fn free_slots(owner: *mut c_void) {
    // SAFETY: the consuming cursor retained this exact Box until every slot closed.
    unsafe { drop(Box::from_raw(owner.cast::<Vec<Slot>>())) };
}

#[cfg(test)]
#[allow(
    clippy::undocumented_unsafe_blocks,
    reason = "Each fixture owns its fresh typed slots, descriptors and protocol lifetimes."
)]
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
            crate::string::hew_string_drop_managed(
                *slot.cast::<*mut hew_cabi::string::HewString>(),
            );
        };
    }
    fn layout(string: bool) -> HewValueLayout {
        HewValueLayout {
            code_owner: core::ptr::null(),
            managed: if string {
                &raw const STRING
            } else {
                &raw const PLAIN
            },
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
    fn foreign_typed_receive_registers_and_retires_under_publication_lock() {
        use std::sync::Barrier;
        let metadata = hew_cabi::managed::HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: None,
        };
        let layout = HewValueLayout {
            size: 8,
            align: 8,
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &raw const metadata,
        };
        // SAFETY: the descriptor and metadata outlive the scoped foreign reader and queue.
        let core = unsafe { ChannelCore::new_typed(1, layout) };
        let rendezvous = Arc::new(Barrier::new(2));
        *core.wait_probe.lock().unwrap() = Some(rendezvous.clone());
        std::thread::scope(|scope| {
            let reader = scope.spawn(|| core.typed_content_recv(true));
            rendezvous.wait();
            // The reader holds the predicate lock until its condvar wait.
            assert_eq!(core.locked().foreign_waiters, 1);
            core.typed_content_send(&41_i64.to_ne_bytes());
            assert_eq!(reader.join().unwrap(), Some(41_i64.to_ne_bytes().to_vec()));
        });
        assert_eq!(core.locked().foreign_waiters, 0);
        core.close_typed_sync();
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
                .commit();
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
                .commit();
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
            crate::string::hew_string_drop_managed(value);
        });
        core.close_typed_sync();
        assert_eq!(source.live_bytes(), 0);
    }

    unsafe extern "C-unwind" fn drop_bytes(slot: *mut c_void) {
        unsafe {
            crate::bytes::hew_bytes_drop_managed((*slot.cast::<crate::bytes::BytesTriple>()).ptr);
        };
    }
    #[test]
    fn typed_bytes_slice_keeps_original_backing_and_uses_canonical_handle_width() {
        static BYTES: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(crate::owned_value_budget::visit::hew_managed_visit_bytes),
        };
        let layout = HewValueLayout {
            code_owner: core::ptr::null(),
            managed: &raw const BYTES,
            release_start: None,
            size: size_of::<crate::bytes::BytesTriple>(),
            align: align_of::<crate::bytes::BytesTriple>(),
            ownership_kind: HewTypeOwnershipKind::Bytes,
            clone_fn: None,
            drop_fn: Some(drop_bytes),
        };
        let source = ValueOwner::new(Some(32));
        let receiver = ValueOwner::new(Some(40));
        let core = core(&receiver, 1, layout);
        let full = unsafe {
            crate::bytes::hew_bytes_from_static(b"0123456789abcdef0123456789abcdef".as_ptr(), 32)
        };
        let mut slice =
            unsafe { crate::bytes::hew_bytes_slice(full.ptr, full.offset, full.len, 0, 1) };
        for slot in [(&raw const full).cast(), (&raw const slice).cast()] {
            unsafe { source.acquire_value(&layout, slot).unwrap().commit() };
        }
        let mut write = send(core.clone(), (&raw mut slice).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 1);
        assert_eq!(
            source.live_bytes(),
            32,
            "the sender still owns its distinct original alias"
        );
        assert_eq!(
            receiver.live_bytes(),
            40,
            "canonical handle8 plus full retained backing32"
        );
        let mut read = TypedRead::new(core.clone(), waker());
        assert_eq!(read.poll(), 1);
        let mut out = crate::bytes::BytesTriple {
            ptr: core::ptr::null_mut(),
            offset: 0,
            len: 0,
        };
        assert!(unsafe { read.take((&raw mut out).cast()) });
        assert_eq!(out.ptr, full.ptr);
        assert_eq!(out.len, 1);
        assert_eq!(receiver.live_bytes(), 32);
        with_value_owner(&Some(receiver.clone()), || unsafe {
            crate::bytes::hew_bytes_drop_managed(out.ptr);
        });
        assert_eq!(receiver.live_bytes(), 0);
        assert_eq!(unsafe { *full.ptr }, b'0');
        with_value_owner(&Some(source.clone()), || unsafe {
            crate::bytes::hew_bytes_drop_managed(full.ptr);
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
    #[derive(Clone, Copy)]
    enum TerminalEdge {
        ReaderClosed,
        SinkClosed,
        ProducerFault,
    }
    impl TerminalEdge {
        fn publish(self, core: &ChannelCore) {
            match self {
                Self::ReaderClosed => {
                    core.prepare_typed_close(None);
                }
                Self::SinkClosed => core.close_sink(),
                Self::ProducerFault => core.fault_close(73),
            }
        }
    }
    struct ProtocolProbe {
        core: Arc<ChannelCore>,
        ready: bool,
        terminal_on_prepare: Option<TerminalEdge>,
        terminal_on_commit: Option<TerminalEdge>,
        aborted: Arc<std::sync::atomic::AtomicBool>,
        resumed: Arc<std::sync::atomic::AtomicBool>,
    }
    unsafe extern "C" fn visit_probe(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        use std::sync::atomic::Ordering;
        let probe = unsafe { &**slot.cast::<*mut ProtocolProbe>() };
        let identity = (probe as *const ProtocolProbe).cast();
        match unsafe { ((*visitor).storage)((*visitor).context, identity, 0) } {
            SKIP_DESCENDANTS => return 0,
            DESCEND => {}
            error => return error,
        }
        match unsafe { (*visitor).kind } {
            HewManagedWalkKind::PrepareTransfer => {
                if let Some(edge) = probe.terminal_on_prepare {
                    edge.publish(&probe.core);
                }
                if probe.ready {
                    0
                } else {
                    TRANSFER_PENDING
                }
            }
            HewManagedWalkKind::CommitTransfer => {
                if let Some(edge) = probe.terminal_on_commit {
                    edge.publish(&probe.core);
                }
                0
            }
            HewManagedWalkKind::ResumeTransfer => {
                assert!(
                    probe.core.locked().queue.is_empty(),
                    "resume precedes publication"
                );
                probe.resumed.store(true, Ordering::SeqCst);
                0
            }
            HewManagedWalkKind::AbortTransfer => {
                probe.aborted.store(true, Ordering::SeqCst);
                0
            }
            _ => 0,
        }
    }
    unsafe extern "C-unwind" fn drop_probe(slot: *mut c_void) {
        let probe = unsafe { *slot.cast::<*mut ProtocolProbe>() };
        if let Some(owner) = current_owner() {
            owner.release_backing(probe.cast(), 0);
        }
        unsafe { drop(Box::from_raw(probe)) };
    }
    fn probe_layout() -> HewValueLayout {
        static PROBE: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(visit_probe),
        };
        let mut layout = layout(false);
        layout.managed = &raw const PROBE;
        layout.drop_fn = Some(drop_probe);
        layout.ownership_kind = HewTypeOwnershipKind::LayoutManaged;
        layout
    }
    fn probe(core: Arc<ChannelCore>, ready: bool, close_on_commit: bool) -> *mut ProtocolProbe {
        Box::into_raw(Box::new(ProtocolProbe {
            core,
            ready,
            terminal_on_prepare: None,
            terminal_on_commit: close_on_commit.then_some(TerminalEdge::ReaderClosed),
            aborted: Arc::default(),
            resumed: Arc::default(),
        }))
    }

    #[test]
    fn receiver_close_aborts_unpublished_pending_provider_and_reserved_capacity() {
        use std::sync::atomic::Ordering;
        let source = ValueOwner::new(Some(0));
        let receiver = ValueOwner::new(Some(8));
        let layout = probe_layout();
        let core = core(&receiver, 1, layout);
        let mut value = probe(core.clone(), false, false);
        let aborted = unsafe { (*value).aborted.clone() };
        unsafe {
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit();
        };
        let mut write = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 0);
        assert!(core.locked().queue.is_empty());
        assert!(!core.prepare_typed_close(Some((*waker()).clone())));
        assert_eq!(write.poll(), 2);
        assert!(aborted.load(Ordering::SeqCst));
        assert!(core.prepare_typed_close(None));
        // The rolled-back reservation leaves all eight bytes available.
        let reservation = receiver
            .acquire(&[ManagedStorage {
                identity: 123,
                bytes: 8,
            }])
            .unwrap();
        drop(reservation);
        with_value_owner(&Some(source), || unsafe {
            drop_probe((&raw mut value).cast());
        });
        core.close_typed_sync();
        assert_eq!(receiver.live_bytes(), 0);
    }

    #[test]
    fn producer_fault_aborts_unpublished_pending_provider_and_releases_reader() {
        use std::sync::atomic::Ordering;
        let source = ValueOwner::new(Some(0));
        let receiver = ValueOwner::new(Some(8));
        let layout = probe_layout();
        let core = core(&receiver, 1, layout);
        let mut value = probe(core.clone(), false, false);
        let aborted = unsafe { (*value).aborted.clone() };
        unsafe {
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit();
        }
        let mut write = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 0);
        core.fault_close(73);
        let status = write.poll();
        let read_status = TypedRead::new(core.clone(), waker()).poll();
        let was_aborted = aborted.load(Ordering::SeqCst);
        // Clean the fixture even when exercising the broken implementation.
        write.cancel();
        with_value_owner(&Some(source.clone()), || unsafe {
            drop_probe((&raw mut value).cast());
        });
        core.close_typed_sync();
        assert_eq!(source.live_bytes(), 0);
        assert_eq!(receiver.live_bytes(), 0);
        assert_eq!(status, 2, "producer fault must refuse unpublished input");
        assert!(
            was_aborted,
            "producer fault must abort provider acquisition"
        );
        assert_eq!(read_status, 3, "reader must observe the producer fault");
    }

    #[test]
    fn terminal_edges_during_prepare_refuse_input_and_return_capacity() {
        use std::sync::atomic::Ordering;
        for edge in [
            TerminalEdge::ReaderClosed,
            TerminalEdge::SinkClosed,
            TerminalEdge::ProducerFault,
        ] {
            let source = ValueOwner::new(Some(0));
            let receiver = ValueOwner::new(Some(8));
            let layout = probe_layout();
            let core = core(&receiver, 1, layout);
            let mut value = probe(core.clone(), false, false);
            let aborted = unsafe { (*value).aborted.clone() };
            unsafe {
                source
                    .acquire_value(&layout, (&raw const value).cast())
                    .unwrap()
                    .commit();
            }
            let mut write = send(core.clone(), (&raw mut value).cast(), layout);
            assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 0);
            unsafe {
                (*value).ready = true;
                (*value).terminal_on_prepare = Some(edge);
            }
            assert_eq!(write.poll(), 2);
            assert!(aborted.load(Ordering::SeqCst));
            assert!(core.locked().queue.is_empty());
            let expected = if matches!(edge, TerminalEdge::ProducerFault) {
                3
            } else {
                2
            };
            assert_eq!(TypedRead::new(core.clone(), waker()).poll(), expected);
            let reservation = receiver
                .acquire(&[ManagedStorage {
                    identity: 123,
                    bytes: 8,
                }])
                .unwrap();
            drop(reservation);
            with_value_owner(&Some(source.clone()), || unsafe {
                drop_probe((&raw mut value).cast());
            });
            core.close_typed_sync();
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(receiver.live_bytes(), 0);
        }
    }

    #[test]
    fn producer_fault_during_commit_preserves_accepted_item_before_fault() {
        use std::sync::atomic::Ordering;
        let source = ValueOwner::new(Some(0));
        let receiver = ValueOwner::new(Some(8));
        let layout = probe_layout();
        let core = core(&receiver, 1, layout);
        let mut value = probe(core.clone(), true, false);
        let resumed = unsafe { (*value).resumed.clone() };
        unsafe {
            (*value).terminal_on_commit = Some(TerminalEdge::ProducerFault);
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit();
        }
        let mut write = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 1);
        assert!(resumed.load(Ordering::SeqCst));
        let mut read = TypedRead::new(core.clone(), waker());
        assert_eq!(read.poll(), 1);
        let mut output = core::ptr::null_mut::<ProtocolProbe>();
        unsafe {
            assert!(read.take((&raw mut output).cast()));
        }
        assert_eq!(output, value);
        with_value_owner(&Some(receiver.clone()), || unsafe {
            drop_probe((&raw mut output).cast());
        });
        assert_eq!(TypedRead::new(core.clone(), waker()).poll(), 3);
        core.close_typed_sync();
        assert_eq!(source.live_bytes(), 0);
        assert_eq!(receiver.live_bytes(), 0);
    }

    #[test]
    fn late_close_drains_only_after_graph_commit_and_final_resume() {
        use std::sync::atomic::Ordering;
        let source = ValueOwner::new(Some(0));
        let receiver = ValueOwner::new(Some(8));
        let layout = probe_layout();
        let core = core(&receiver, 1, layout);
        let mut value = probe(core.clone(), true, true);
        let resumed = unsafe { (*value).resumed.clone() };
        let aborted = unsafe { (*value).aborted.clone() };
        unsafe {
            source
                .acquire_value(&layout, (&raw const value).cast())
                .unwrap()
                .commit();
        };
        let mut write = send(core.clone(), (&raw mut value).cast(), layout);
        assert_eq!(with_value_owner(&Some(source.clone()), || write.poll()), 1);
        assert!(resumed.load(Ordering::SeqCst));
        assert!(!aborted.load(Ordering::SeqCst));
        assert_eq!(core.locked().queue.len(), 1);
        assert_eq!(receiver.live_bytes(), 8);
        assert!(core.prepare_typed_close(None));
        core.close_typed_sync();
        assert_eq!(receiver.live_bytes(), 0);
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
                .commit();
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
        descriptor.managed = &raw const STREAM;
        let transfer = unsafe {
            transfer::ValueTransfer::prepare(
                Some(source.clone()),
                Some(receiver.clone()),
                &raw const descriptor,
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
