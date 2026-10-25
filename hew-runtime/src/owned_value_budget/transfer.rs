//! Exact owner transfer through the existing typed value visitor. Providers
//! freeze admission with state flags, never a held mutex across ledger work.

use super::{Acquisition, ValueOwner};
use core::ffi::c_void;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS,
    TRANSFER_PENDING,
};
use hew_cabi::value::HewValueLayout;
use std::collections::HashSet;

#[derive(Debug)]
struct TransferWalk {
    destination: Option<ValueOwner>,
    binding: Option<DestinationBinding>,
    waker: Option<crate::wake::OwnedWaker>,
    seen: HashSet<usize>,
}

/// Execution ownership already retained by the destination incarnation.
/// This is independent of whether that actor enabled a managed value limit.
#[derive(Clone, Debug)]
pub(crate) struct DestinationBinding {
    pub(crate) actor_turn: crate::lifetime::live_actors::ActorIncarnation,
    pub(crate) rc_owner: Option<std::sync::Arc<crate::rc::RcAllocations>>,
}

impl DestinationBinding {
    /// # Safety
    /// The caller pins this actor while its canonical cleanup owners are read.
    pub(crate) unsafe fn for_actor(actor: *mut crate::actor::HewActor) -> Self {
        // SAFETY: admission retains the exact actor through this snapshot.
        let actor_turn = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(actor) };
        let rc_owner = unsafe { actor.as_ref() }
            .and_then(|actor| actor.native_completion.as_ref())
            .map(|completion| completion.cleanup.rc.clone());
        Self {
            actor_turn,
            rc_owner,
        }
    }
}

impl ValueOwner {
    pub(crate) fn execution_binding(&self) -> Option<DestinationBinding> {
        let identity = *self.0.incarnation.get()?;
        crate::lifetime::live_actors::with_live_incarnation(identity, |pin| {
            // SAFETY: the registry pins the matching incarnation for this read.
            unsafe { DestinationBinding::for_actor(pin.as_ptr()) }
        })
    }
}

/// Canonical actor/Rc execution owner for a successful provider rebind.
/// # Safety
/// Visitor belongs to this module's explicit transfer phase.
#[must_use]
pub(crate) unsafe fn destination_binding(
    visitor: *const HewManagedStorageVisitor,
) -> Option<DestinationBinding> {
    // SAFETY: the transfer retains this immutable binding snapshot.
    unsafe { &*(*visitor).context.cast::<TransferWalk>() }
        .binding
        .clone()
}

unsafe extern "C" fn backing(context: *mut c_void, identity: *const c_void, _bytes: u64) -> i32 {
    // SAFETY: a transfer phase exclusively lends this synchronous context.
    let walk = unsafe { &mut *context.cast::<TransferWalk>() };
    if walk.seen.insert(identity as usize) {
        DESCEND
    } else {
        SKIP_DESCENDANTS
    }
}

/// Read the destination binding during an explicit provider transfer phase.
/// `None` is the unbounded destination account, not the sending actor.
///
/// # Safety
/// Visitor belongs to this module's transfer walk and has a transfer kind.
#[must_use]
pub unsafe fn destination_owner(visitor: *const HewManagedStorageVisitor) -> Option<ValueOwner> {
    // SAFETY: only provider transfer callbacks invoke this helper.
    let visitor = unsafe { &*visitor };
    assert!(matches!(
        visitor.kind,
        HewManagedWalkKind::PrepareTransfer
            | HewManagedWalkKind::CommitTransfer
            | HewManagedWalkKind::AbortTransfer
            | HewManagedWalkKind::ResumeTransfer
    ));
    // SAFETY: transfer walks carry this exact runtime-owned context.
    unsafe { &*visitor.context.cast::<TransferWalk>() }
        .destination
        .clone()
}

/// Stable identity for a provider's idempotent transfer gate.
/// # Safety
/// Visitor belongs to the same transfer walk contract as `destination_owner`.
#[must_use]
pub unsafe fn transfer_identity(visitor: *const HewManagedStorageVisitor) -> *const c_void {
    // SAFETY: the transfer owns this boxed context until commit or abort.
    unsafe { (*visitor).context.cast_const() }
}

/// Retain the admission attempt's notification target while a provider pauses.
/// # Safety
/// Visitor belongs to this module's explicit transfer phase.
#[must_use]
pub unsafe fn transfer_waker(
    visitor: *const HewManagedStorageVisitor,
) -> Option<crate::wake::OwnedWaker> {
    // SAFETY: the stable boxed context retains this independent wake target.
    unsafe { &*(*visitor).context.cast::<TransferWalk>() }
        .waker
        .clone()
}

/// Frozen typed graph with destination acquisition held until actual admission.
/// Dropping a refused/waiting attempt restores every provider's original gate.
#[derive(Debug)]
pub struct ValueTransfer {
    source: Option<ValueOwner>,
    destination: Option<Acquisition>,
    walk: Box<TransferWalk>,
    layout: *const HewValueLayout,
    slot: *const c_void,
    prepared: bool,
    ready: bool,
}

impl ValueTransfer {
    unsafe fn phase(&mut self, kind: HewManagedWalkKind) -> i32 {
        self.walk.seen.clear();
        let visitor = HewManagedStorageVisitor {
            context: (&raw mut *self.walk).cast(),
            storage: backing,
            kind,
        };
        // SAFETY: transfer retains the same slot and defining layout to finish.
        unsafe { visit_value(&*self.layout, self.slot, &raw const visitor) }
    }

    /// Freeze nested providers and acquire the recipient before ownership moves.
    /// A full mailbox must drop this attempt so the sender remains its owner.
    ///
    /// # Safety
    /// Slot/layout stay live and stable through this transfer's commit or drop.
    pub unsafe fn begin(
        source: Option<ValueOwner>,
        destination: Option<ValueOwner>,
        layout: *const HewValueLayout,
        slot: *const c_void,
    ) -> Self {
        let same = match (&source, &destination) {
            (None, None) => true,
            (Some(a), Some(b)) => a.same_account(b),
            _ => false,
        };
        let mut transfer = Self {
            source,
            destination: None,
            walk: Box::new(TransferWalk {
                binding: destination.as_ref().and_then(ValueOwner::execution_binding),
                destination,
                waker: None,
                seen: HashSet::new(),
            }),
            layout,
            slot,
            prepared: false,
            ready: same,
        };
        if same {
            return transfer;
        }
        transfer.prepared = true;
        transfer
    }

    /// The waiting operation retains its existing wake target through retries.
    pub fn set_waker(&mut self, waker: crate::wake::OwnedWaker) {
        self.walk.waker = Some(waker);
    }

    /// Supply execution identity even when the recipient has no budget account.
    /// Must precede preparation; budget acquisition remains disabled for it.
    /// # Safety
    /// The actor is live and pinned by the admission operation during this call.
    pub unsafe fn set_destination_actor(&mut self, actor: *mut crate::actor::HewActor) {
        assert!(
            self.destination.is_none(),
            "destination changed after acquisition"
        );
        self.walk.binding = Some(unsafe { DestinationBinding::for_actor(actor) });
        self.prepared = true;
        self.ready = false;
    }

    /// Poll provider quiescence while retaining its transfer gates. Pending
    /// work stays sender-owned and cannot publish into either account.
    /// # Safety
    /// The original slot is pinned and unchanged until commit or guard drop.
    pub unsafe fn poll_prepare(&mut self) -> Result<bool, i32> {
        if self.ready {
            return Ok(true);
        }
        // SAFETY: this phase changes only provider gate state, not ownership.
        let status = unsafe { self.phase(HewManagedWalkKind::PrepareTransfer) };
        if status == TRANSFER_PENDING {
            return Ok(false);
        }
        if status != 0 {
            return Err(status);
        }
        if let Some(owner) = &self.walk.destination {
            // SAFETY: providers are now frozen; slots remain sender-owned.
            self.destination = Some(unsafe { owner.acquire_value(&*self.layout, self.slot) }?);
        }
        self.ready = true;
        Ok(true)
    }

    /// Prepare an immediately quiescent graph. Asynchronous providers retain
    /// the guard from `begin` and poll readiness instead.
    /// # Safety
    /// Slot/layout stay live and stable through this transfer's commit or drop.
    pub unsafe fn prepare(
        source: Option<ValueOwner>,
        destination: Option<ValueOwner>,
        layout: *const HewValueLayout,
        slot: *const c_void,
    ) -> Result<Self, i32> {
        // SAFETY: this convenience operation retains the same original slot.
        let mut transfer = unsafe { Self::begin(source, destination, layout, slot) };
        if unsafe { transfer.poll_prepare() }? {
            Ok(transfer)
        } else {
            Err(TRANSFER_PENDING)
        }
    }

    /// Finish an admitted transfer before providers can receive new payloads.
    ///
    /// # Safety
    /// Caller has won actual admission, and the original frozen slot is live.
    pub unsafe fn commit(mut self) -> Result<(), i32> {
        assert!(self.ready, "transfer committed before provider quiescence");
        if !self.prepared {
            return Ok(());
        }
        if let Some(owner) = &self.source {
            // SAFETY: provider gates still freeze exactly the source graph.
            unsafe { owner.release_value(&*self.layout, self.slot) }?;
        }
        // SAFETY: destination acquisition and source refund completed atomically
        // with respect to the frozen providers. Commit only rebinds; the whole graph stays frozen.
        if unsafe { self.phase(HewManagedWalkKind::CommitTransfer) } != 0 {
            std::process::abort();
        }
        if let Some(acquisition) = self.destination.take() {
            acquisition.commit();
        }
        // Rebind the complete graph before any child executor can run.
        if unsafe { self.phase(HewManagedWalkKind::ResumeTransfer) } != 0 {
            std::process::abort();
        }
        self.prepared = false;
        Ok(())
    }
}

impl Drop for ValueTransfer {
    fn drop(&mut self) {
        if self.prepared {
            // SAFETY: caller retains the same slot until this guard is dropped.
            // A prepare walk may stop at an undescribed foreign boundary.
            // Abort follows the same prefix and releases every gate prepared
            // before it; that unavailable suffix never acquired ownership.
            let _ = unsafe { self.phase(HewManagedWalkKind::AbortTransfer) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::managed::HEAP_EXCEEDED;

    #[test]
    fn pending_provider_keeps_its_gate_and_original_account_until_adoption() {
        use core::sync::atomic::{AtomicBool, Ordering};
        use hew_cabi::managed::HewManagedValueDescriptor;
        use hew_cabi::value::HewTypeOwnershipKind;
        struct Provider {
            ready: AtomicBool,
            frozen: AtomicBool,
        }
        unsafe extern "C" fn visit(
            slot: *const c_void,
            visitor: *const HewManagedStorageVisitor,
        ) -> i32 {
            // SAFETY: this test descriptor lends the same live provider.
            let provider = unsafe { &*slot.cast::<*const Provider>().read() };
            let walk = unsafe { &*visitor };
            match walk.kind {
                HewManagedWalkKind::PrepareTransfer => {
                    provider.frozen.store(true, Ordering::Release);
                    if provider.ready.load(Ordering::Acquire) {
                        0
                    } else {
                        TRANSFER_PENDING
                    }
                }
                HewManagedWalkKind::ResumeTransfer | HewManagedWalkKind::AbortTransfer => {
                    provider.frozen.store(false, Ordering::Release);
                    0
                }
                HewManagedWalkKind::CommitTransfer => {
                    assert!(provider.frozen.load(Ordering::Acquire));
                    0
                }
                _ => {
                    let status = unsafe {
                        (walk.storage)(walk.context, core::ptr::from_ref(provider).cast(), 6)
                    };
                    if status == SKIP_DESCENDANTS {
                        0
                    } else {
                        status
                    }
                }
            }
        }
        let provider = Provider {
            ready: AtomicBool::new(false),
            frozen: AtomicBool::new(false),
        };
        let pointer = &raw const provider;
        let managed = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(visit),
        };
        let layout = HewValueLayout {
            size: size_of::<*const Provider>(),
            align: align_of::<*const Provider>(),
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &raw const managed,
        };
        let source = ValueOwner::new(Some(6));
        let destination = ValueOwner::new(Some(6));
        // SAFETY: the provider and descriptor outlive the pending transfer.
        unsafe {
            let slot = (&raw const pointer).cast();
            source.acquire_value(&layout, slot).unwrap().commit();
            let mut transfer = ValueTransfer::begin(
                Some(source.clone()),
                Some(destination.clone()),
                &raw const layout,
                slot,
            );
            assert!(!transfer.poll_prepare().unwrap());
            assert!(provider.frozen.load(Ordering::Acquire));
            assert_eq!((source.live_bytes(), destination.live_bytes()), (6, 0));
            provider.ready.store(true, Ordering::Release);
            assert!(transfer.poll_prepare().unwrap());
            assert_eq!((source.live_bytes(), destination.live_bytes()), (6, 6));
            transfer.commit().unwrap();
            assert!(!provider.frozen.load(Ordering::Acquire));
            assert_eq!((source.live_bytes(), destination.live_bytes()), (0, 6));
            destination.release_value(&layout, slot).unwrap();
        }
    }

    #[test]
    fn refused_or_abandoned_adoption_preserves_the_sending_owner() {
        let source = ValueOwner::new(Some(6));
        let small = ValueOwner::new(Some(5));
        let destination = ValueOwner::new(Some(6));
        let layout = &crate::layout_intrinsics::hew_layout_val_string;
        // SAFETY: one live string slot remains unchanged through every attempt.
        unsafe {
            let text = hew_cabi::string::string_from_str("retain");
            let slot = (&raw const text).cast();
            source.acquire_value(layout, slot).unwrap().commit();
            assert!(matches!(
                ValueTransfer::prepare(Some(source.clone()), Some(small.clone()), layout, slot),
                Err(HEAP_EXCEEDED)
            ));
            assert_eq!(source.live_bytes(), 6);
            assert_eq!(small.live_bytes(), 0);
            let attempt = ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                layout,
                slot,
            )
            .unwrap();
            assert_eq!(source.live_bytes(), 6);
            assert_eq!(destination.live_bytes(), 6);
            drop(attempt);
            assert_eq!(source.live_bytes(), 6);
            assert_eq!(destination.live_bytes(), 0);
            ValueTransfer::prepare(
                Some(source.clone()),
                Some(destination.clone()),
                layout,
                slot,
            )
            .unwrap()
            .commit()
            .unwrap();
            assert_eq!(source.live_bytes(), 0);
            assert_eq!(destination.live_bytes(), 6);
            destination.release_value(layout, slot).unwrap();
            hew_cabi::string::string_release(text);
        }
    }

    #[test]
    fn unchanged_or_unbounded_ownership_does_not_read_value_metadata() {
        let owner = ValueOwner::new(Some(0));
        // SAFETY: these no-op paths must never inspect a slot or descriptor.
        unsafe {
            ValueTransfer::prepare(None, None, core::ptr::null(), core::ptr::null())
                .unwrap()
                .commit()
                .unwrap();
            ValueTransfer::prepare(
                Some(owner.clone()),
                Some(owner),
                core::ptr::null(),
                core::ptr::null(),
            )
            .unwrap()
            .commit()
            .unwrap();
        }
    }
}
