//! Incarnation-owned accounts for canonical managed payload storage.
//!
//! Typed value descriptors supply backing identities and payload capacities.
//! This ledger never estimates Rust layouts, allocator blocks or process heap
//! use. Each retaining owner pays once for a backing, regardless of its aliases.
//! Unbounded execution carries no account and does not call these operations.
//!
//! Acquisition and growth reservations precede ownership or storage mutation.
//! Their guards refund abandoned work without unwinding a language fault through
//! an allocator or C ABI. Accounts remain stable across worker migration.

use std::sync::Arc;

pub mod actor;
pub mod frame;
pub mod materialize;
pub mod transfer;
pub mod visit;
mod walk;

mod account;
mod budget_limit;
pub use account::{
    Acquisition, BackingChange, BudgetExceeded, ManagedStorage, ResizeError, ResizePermit,
    StorageReservation, ValueOwner,
};
use account::{Ledger, Retention};

/// Borrow the current invocation's retained account, including independently
/// scheduled scoped tasks, before falling back to its actor incarnation.
#[must_use]
pub fn current_owner() -> Option<ValueOwner> {
    let context = crate::execution_context::current_context();
    if context.is_null() {
        return None;
    }
    // SAFETY: explicit release bindings live through this synchronous poll.
    if let Some(owner) = unsafe { (*context).value_owner.as_ref() } {
        return owner.clone();
    }
    // SAFETY: the canonical execution context pins its active task scope.
    let scope = unsafe { (*context).task_scope };
    if !scope.is_null() {
        // An unbounded scope is authoritative too; do not substitute a
        // different actor merely because its account is disabled.
        // SAFETY: the installed execution context retains its lexical scope.
        return unsafe { (*scope).value_owner.clone() };
    }
    // SAFETY: a present carrier retains its actor through this synchronous poll.
    let actor = unsafe { (*context).actor };
    if actor.is_null() {
        return None;
    }
    // SAFETY: the canonical dispatch context retains this incarnation.
    unsafe { &*actor }
        .native_completion
        .as_ref()
        .and_then(|completion| completion.cleanup.value_owner.clone())
}

/// Install a borrowed account on the canonical execution carrier for one
/// synchronous poll. The driver retains it across suspension and reinstalls it
/// on each worker; no thread-local account or value graph survives this call.
/// `None` explicitly describes unpublished internal storage, independent of an
/// enclosing bounded actor. The previous binding is restored on return or panic.
pub fn with_value_owner<R>(owner: &Option<ValueOwner>, run: impl FnOnce() -> R) -> R {
    use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
    struct Restore {
        context: *mut HewExecutionContext,
        previous: *mut HewExecutionContext,
        owner: *const Option<ValueOwner>,
    }
    impl Drop for Restore {
        fn drop(&mut self) {
            // SAFETY: the installed carrier remains live until this guard drops.
            unsafe { (*self.context).value_owner = self.owner };
            let _ = set_current_context(self.previous);
        }
    }
    let previous = current_context();
    let mut fallback = HewExecutionContext::default();
    let context = if previous.is_null() {
        &raw mut fallback
    } else {
        previous
    };
    // SAFETY: this thread exclusively lends the installed carrier for this poll.
    let old = unsafe { std::mem::replace(&mut (*context).value_owner, owner) };
    let _restore = Restore {
        context,
        previous,
        owner: old,
    };
    let _ = set_current_context(context);
    run()
}

/// Refund one compiler-described allocation immediately before physical free.
///
/// # Safety
/// The current account owns this exact canonical backing edge, retained through
/// completed child cleanup. Unpublished candidates run with an explicit `None`.
#[no_mangle]
pub unsafe extern "C" fn hew_budget_release_backing(identity: *const std::ffi::c_void, bytes: u64) {
    if let Some(owner) = current_owner() {
        owner.release_backing(identity, bytes);
    }
}

impl ValueOwner {
    /// Bind once to the actual incarnation after its identity is reserved.
    /// A restarted role receives a different account and cannot retarget this one.
    pub(crate) unsafe fn bind_incarnation(&self, actor: *mut crate::actor::HewActor) {
        // SAFETY: construction owns the newly reserved actor allocation.
        let identity = unsafe { crate::lifetime::live_actors::ActorIncarnation::of(actor) };
        if let Some(previous) = self.0.incarnation.get() {
            assert_eq!(
                *previous, identity,
                "managed account retargeted across incarnations"
            );
        } else {
            self.0
                .incarnation
                .set(identity)
                .expect("managed incarnation bound concurrently");
        }
    }

    /// A provider's asynchronous acquisition refusal belongs to the retaining
    /// actor, using its existing external-fault latch and checked terminal drain.
    pub(crate) fn heap_exceeded(&self) {
        let Some(identity) = self.0.incarnation.get().copied() else {
            return;
        };
        let _ = crate::lifetime::live_actors::with_live_incarnation(identity, |pin| {
            // SAFETY: the exact-incarnation pin outlives the external trap request.
            unsafe {
                crate::actor::hew_actor_trap(
                    pin.as_ptr(),
                    crate::internal::types::HEW_TRAP_HEAP_EXCEEDED,
                );
            };
        });
    }

    #[must_use]
    pub(crate) fn same_account(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn storage(identity: usize, bytes: u64) -> ManagedStorage {
        ManagedStorage { identity, bytes }
    }

    #[test]
    fn shallow_repartition_moves_a_full_accounts_charge_without_headroom() {
        let owner = ValueOwner::new(Some(16));
        let queue = storage(11, 16);
        owner.acquire(&[queue]).unwrap().commit();
        let remaining = storage(11, 8);
        let read = storage(12, 8);
        owner
            .repartition_backing(queue, &[remaining, read])
            .unwrap();
        assert_eq!(owner.live_bytes(), 16);
        assert!(owner
            .repartition_backing(remaining, &[storage(11, 0), storage(12, 8)])
            .is_err());
        assert_eq!(owner.live_bytes(), 16);
        owner.release(&[read]);
        owner
            .repartition_backing(remaining, &[storage(11, 0), storage(13, 8)])
            .unwrap();
        owner.release(&[storage(13, 8), storage(11, 0)]);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn backing_refund_keeps_other_retaining_accounts_charged() {
        let first = ValueOwner::new(Some(6));
        let second = ValueOwner::new(Some(6));
        let backing = ManagedStorage {
            identity: 123,
            bytes: 6,
        };
        first.acquire(&[backing]).unwrap().commit();
        second.acquire(&[backing]).unwrap().commit();
        first.release_backing(backing.identity as *const std::ffi::c_void, backing.bytes);
        assert_eq!((first.live_bytes(), second.live_bytes()), (0, 6));
        second.release_backing(backing.identity as *const std::ffi::c_void, backing.bytes);
        assert_eq!(second.live_bytes(), 0);
    }

    #[test]
    fn release_context_keeps_explicit_owner_and_restores_nested_unbounded_binding() {
        let owner = ValueOwner::new(Some(12));
        let previous = crate::execution_context::current_context();
        with_value_owner(&Some(owner.clone()), || {
            assert!(current_owner().unwrap().same_account(&owner));
            with_value_owner(&None, || assert!(current_owner().is_none()));
            assert!(current_owner().unwrap().same_account(&owner));
        });
        assert_eq!(crate::execution_context::current_context(), previous);
    }

    #[test]
    fn graph_aliases_charge_each_owner_once_and_rollback_atomically() {
        let a = ValueOwner::new(Some(64));
        let b = ValueOwner::new(Some(64));
        let graph = [storage(1, 32), storage(1, 32), storage(2, 32)];
        a.acquire(&graph).unwrap().commit();
        assert_eq!(a.live_bytes(), 64);
        drop(b.acquire(&graph).unwrap());
        assert_eq!(b.live_bytes(), 0);
        b.acquire(&graph).unwrap().commit();
        a.release(&graph[..1]);
        assert_eq!(a.live_bytes(), 64);
        a.release(&graph[1..]);
        b.release(&graph);
        assert_eq!((a.live_bytes(), b.live_bytes()), (0, 0));
    }

    #[test]
    fn refused_graph_and_transfer_preserve_original_owners() {
        let source = ValueOwner::new(Some(64));
        let small = ValueOwner::new(Some(63));
        let target = ValueOwner::new(Some(64));
        let graph = [storage(1, 32), storage(2, 32)];
        source.acquire(&graph).unwrap().commit();
        assert!(source.transfer(&small, &graph).is_err());
        assert_eq!((source.live_bytes(), small.live_bytes()), (64, 0));
        source.transfer(&target, &graph).unwrap();
        assert_eq!((source.live_bytes(), target.live_bytes()), (0, 64));
        target.release(&graph);
    }

    #[test]
    fn new_payload_reservation_rolls_back_and_refunds_discovered_aliases() {
        let owner = ValueOwner::new(Some(64));
        let held = [storage(1, 32)];
        owner.acquire(&held).unwrap().commit();
        let pending = owner.reserve_new(32).unwrap();
        assert!(owner.reserve_new(1).is_err());
        drop(pending);
        let pending = owner.reserve_new(32).unwrap();
        pending.commit(&held);
        assert_eq!(owner.live_bytes(), 32);
        owner.release(&held);
        owner.release(&held);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn resize_preflights_growth_aliases_and_relocation() {
        let owner = ValueOwner::new(Some(64));
        let old = storage(1, 32);
        owner.acquire(&[old]).unwrap().commit();
        drop(owner.prepare_resize(old, 64).unwrap());
        assert!(matches!(
            owner.prepare_resize(old, 65),
            Err(ResizeError::Exceeded(_))
        ));
        owner.acquire(&[old]).unwrap().commit();
        assert!(matches!(
            owner.prepare_resize(old, 64),
            Err(ResizeError::SharedStorage)
        ));
        owner.release(&[old]);
        owner.prepare_resize(old, 64).unwrap().commit(2);
        assert_eq!(owner.live_bytes(), 64);
        owner.prepare_resize(storage(2, 64), 16).unwrap().commit(2);
        owner.release(&[storage(2, 16)]);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn backing_merge_credits_moved_inputs_and_rolls_back_refusal() {
        let owner = ValueOwner::new(Some(64));
        let receiver = storage(1, 16);
        let child = storage(2, 32);
        owner.acquire(&[receiver, child]).unwrap().commit();
        let retired = [(receiver, true), (child, true)];
        assert!(owner.prepare_backing_replacement(&retired, 65).is_err());
        let pending = owner.prepare_backing_replacement(&retired, 64).unwrap();
        assert!(owner.reserve_new(1).is_err());
        drop(pending);
        assert_eq!(owner.live_bytes(), 48);
        owner
            .prepare_backing_replacement(&retired, 64)
            .unwrap()
            .commit(1);
        assert_eq!(owner.live_bytes(), 64);
        owner.release(&[storage(1, 64)]);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn backing_merge_keeps_surviving_aliases_charged() {
        let owner = ValueOwner::new(Some(80));
        let receiver = storage(1, 16);
        let child = storage(2, 20);
        owner.acquire(&[receiver, child, child]).unwrap().commit();
        let retired = [(receiver, true), (child, true)];
        assert!(owner.prepare_backing_replacement(&retired, 64).is_err());
        owner.release(&[child]);
        owner
            .prepare_backing_replacement(&retired, 64)
            .unwrap()
            .commit(1);
        owner.release(&[storage(1, 64)]);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn backing_merge_deduplicates_credit_and_reserves_cow_copy() {
        let owner = ValueOwner::new(Some(64));
        let shared = storage(1, 32);
        owner.acquire(&[shared, shared]).unwrap().commit();
        assert!(owner
            .prepare_backing_replacement(&[(shared, true), (shared, true)], 80)
            .is_err());
        owner
            .prepare_backing_replacement(&[(shared, true), (shared, true)], 64)
            .unwrap()
            .commit(2);
        owner.release(&[storage(2, 64)]);
        owner.acquire(&[shared]).unwrap().commit();
        assert!(owner
            .prepare_backing_change(Some(shared), 40, false)
            .is_err());
        owner
            .prepare_backing_change(Some(shared), 32, false)
            .unwrap()
            .commit(2);
        owner.release(&[storage(2, 32)]);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn worker_migration_keeps_incarnation_identity() {
        let owner = ValueOwner::new(Some(64));
        let graph = [storage(1, 64)];
        owner.acquire(&graph).unwrap().commit();
        let cleanup = owner.clone();
        std::thread::spawn(move || cleanup.release(&graph))
            .join()
            .unwrap();
        assert_eq!(owner.live_bytes(), 0);
        let restarted = ValueOwner::new(Some(64));
        restarted.acquire(&graph).unwrap().commit();
        assert_eq!((owner.live_bytes(), restarted.live_bytes()), (0, 64));
        restarted.release(&graph);
    }

    #[test]
    fn zero_and_overflow_fail_without_partial_graph_admission() {
        let zero = ValueOwner::new(Some(0));
        zero.acquire(&[storage(0, 0)]).unwrap().commit();
        assert!(zero.acquire(&[storage(1, 1)]).is_err());
        let unbounded = ValueOwner::new(None);
        let graph = [storage(1, u64::MAX), storage(2, 1)];
        assert!(unbounded.acquire(&graph).is_err());
        assert_eq!(unbounded.live_bytes(), 0);
    }
}
