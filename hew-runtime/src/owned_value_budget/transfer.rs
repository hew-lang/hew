//! Exact owner transfer through the existing typed value visitor. Providers
//! freeze admission with state flags, never a held mutex across ledger work.

use super::{Acquisition, ValueOwner};
use core::ffi::c_void;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, SKIP_DESCENDANTS,
};
use hew_cabi::value::HewValueLayout;
use std::collections::HashSet;

#[derive(Debug)]
struct TransferWalk {
    destination: Option<ValueOwner>,
    seen: HashSet<usize>,
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
    ));
    // SAFETY: transfer walks carry this exact runtime-owned context.
    unsafe { &*visitor.context.cast::<TransferWalk>() }
        .destination
        .clone()
}

/// Frozen typed graph with destination acquisition held until actual admission.
/// Dropping a refused/waiting attempt restores every provider's original gate.
#[derive(Debug)]
pub struct ValueTransfer {
    source: Option<ValueOwner>,
    destination: Option<Acquisition>,
    walk: TransferWalk,
    layout: *const HewValueLayout,
    slot: *const c_void,
    prepared: bool,
}

impl ValueTransfer {
    unsafe fn phase(&mut self, kind: HewManagedWalkKind) -> i32 {
        self.walk.seen.clear();
        let visitor = HewManagedStorageVisitor {
            context: (&raw mut self.walk).cast(),
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
    pub unsafe fn prepare(
        source: Option<ValueOwner>,
        destination: Option<ValueOwner>,
        layout: *const HewValueLayout,
        slot: *const c_void,
    ) -> Result<Self, i32> {
        let same = match (&source, &destination) {
            (None, None) => true,
            (Some(a), Some(b)) => a.same_account(b),
            _ => false,
        };
        let mut transfer = Self {
            source,
            destination: None,
            walk: TransferWalk {
                destination,
                seen: HashSet::new(),
            },
            layout,
            slot,
            prepared: false,
        };
        if same {
            return Ok(transfer);
        }
        transfer.prepared = true;
        // SAFETY: this phase changes only provider gate state, not ownership.
        let status = unsafe { transfer.phase(HewManagedWalkKind::PrepareTransfer) };
        if status != 0 {
            return Err(status);
        }
        if let Some(owner) = &transfer.walk.destination {
            // SAFETY: providers are now frozen; slots remain sender-owned.
            transfer.destination = Some(unsafe { owner.acquire_value(&*layout, slot) }?);
        }
        Ok(transfer)
    }

    /// Finish an admitted transfer before providers can receive new payloads.
    ///
    /// # Safety
    /// Caller has won actual admission, and the original frozen slot is live.
    pub unsafe fn commit(mut self) -> Result<(), i32> {
        if !self.prepared {
            return Ok(());
        }
        if let Some(owner) = &self.source {
            // SAFETY: provider gates still freeze exactly the source graph.
            unsafe { owner.release_value(&*self.layout, self.slot) }?;
        }
        // SAFETY: destination acquisition and source refund completed atomically
        // with respect to the frozen providers. Commit only rebinds/unfreezes.
        if unsafe { self.phase(HewManagedWalkKind::CommitTransfer) } != 0 {
            std::process::abort();
        }
        if let Some(acquisition) = self.destination.take() {
            acquisition.commit();
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
