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

use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};

pub mod actor;
pub mod frame;
pub mod materialize;
pub mod transfer;
pub mod visit;
mod walk;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BudgetExceeded {
    pub limit: Option<u64>,
    pub live: u64,
    pub requested: u64,
}

/// A live backing reported by the concrete type's descriptor. Identity is the
/// allocation's stable address, not a value slot, actor role or worker address.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ManagedStorage {
    pub identity: usize,
    pub bytes: u64,
}

#[derive(Clone, Copy, Debug)]
struct Retention {
    bytes: u64,
    references: u64,
}

#[derive(Debug, Default)]
struct Ledger {
    live: u64,
    reserved: u64,
    storage: HashMap<usize, Retention>,
}

#[derive(Debug)]
struct Account {
    limit: Option<u64>,
    ledger: Mutex<Ledger>,
    incarnation: std::sync::OnceLock<crate::lifetime::live_actors::ActorIncarnation>,
}

/// Stable actor-incarnation identity, independent of its current worker.
/// The caller represents an unbounded actor with `None`, avoiding an account
/// allocation and any descriptor walk on ordinary unbounded retain/release.
#[derive(Clone, Debug)]
pub struct ValueOwner(Arc<Account>);

/// Borrow the current invocation's retained account, including independently
/// scheduled scoped tasks, before falling back to its actor incarnation.
pub(crate) fn current_owner() -> Option<ValueOwner> {
    let context = crate::execution_context::current_context();
    if !context.is_null() {
        // SAFETY: explicit release bindings live through this synchronous poll.
        if let Some(owner) = unsafe { (*context).value_owner.as_ref() } {
            return owner.clone();
        }
        // SAFETY: the canonical execution context pins its active task scope.
        let scope = unsafe { (*context).task_scope };
        if !scope.is_null() {
            // An unbounded scope is authoritative too; do not substitute a
            // different actor merely because its account is disabled.
            return unsafe { (*scope).value_owner.clone() };
        }
    }
    let actor = crate::actor::hew_actor_self();
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
pub(crate) fn with_value_owner<R>(owner: &Option<ValueOwner>, run: impl FnOnce() -> R) -> R {
    use crate::execution_context::{current_context, set_current_context, HewExecutionContext};
    let previous = current_context();
    let mut fallback = HewExecutionContext::default();
    let context = if previous.is_null() {
        &raw mut fallback
    } else {
        previous
    };
    // SAFETY: this thread exclusively lends the installed carrier for this poll.
    let old = unsafe { std::mem::replace(&mut (*context).value_owner, owner) };
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
    let _restore = Restore {
        context,
        previous,
        owner: old,
    };
    let _ = set_current_context(context);
    run()
}

impl ValueOwner {
    #[must_use]
    pub fn new(limit: Option<u64>) -> Self {
        Self(Arc::new(Account {
            limit,
            ledger: Mutex::new(Ledger::default()),
            incarnation: std::sync::OnceLock::new(),
        }))
    }

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
                )
            };
        });
    }

    #[must_use]
    pub(crate) fn same_account(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }

    fn ledger(&self) -> MutexGuard<'_, Ledger> {
        self.0
            .ledger
            .lock()
            .expect("managed value account lock poisoned")
    }

    #[must_use]
    pub fn live_bytes(&self) -> u64 {
        self.ledger().live
    }

    fn check(&self, ledger: &Ledger, bytes: u64) -> Result<(), BudgetExceeded> {
        if ledger
            .live
            .checked_add(ledger.reserved)
            .and_then(|n| n.checked_add(bytes))
            .is_some_and(|n| self.0.limit.is_none_or(|limit| n <= limit))
        {
            Ok(())
        } else {
            Err(BudgetExceeded {
                limit: self.0.limit,
                live: ledger.live,
                requested: bytes,
            })
        }
    }

    /// Acquire an entire typed ownership graph atomically. Repeated backing
    /// identities add references but charge their payload once. No language
    /// callback runs while the account lock is held.
    pub fn acquire(&self, storage: &[ManagedStorage]) -> Result<Acquisition, BudgetExceeded> {
        let pending = storage_counts(storage);
        let mut ledger = self.ledger();
        let bytes = additional_bytes(&ledger, &pending).ok_or(BudgetExceeded {
            limit: self.0.limit,
            live: ledger.live,
            requested: u64::MAX,
        })?;
        self.check(&ledger, bytes)?;
        insert_storage(&mut ledger, pending);
        ledger.live += bytes;
        Ok(Acquisition {
            owner: self.clone(),
            storage: storage.to_vec(),
            committed: false,
        })
    }

    /// End one backing edge after its typed children finish semantic release.
    /// The caller supplies the same canonical capacity used for acquisition.
    pub(crate) fn release_backing(&self, identity: *const std::ffi::c_void, bytes: u64) {
        self.release(&[ManagedStorage {
            identity: identity as usize,
            bytes,
        }]);
    }

    /// Release exactly the graph whose ownership obligation has ended, even
    /// when cleanup runs on a different worker from the acquisition.
    pub fn release(&self, storage: &[ManagedStorage]) {
        let pending = storage_counts(storage);
        let mut ledger = self.ledger();
        for (identity, release) in &pending {
            let retained = ledger
                .storage
                .get(identity)
                .expect("unrelated managed storage release");
            assert_eq!(
                retained.bytes, release.bytes,
                "managed backing capacity changed without reservation"
            );
            assert!(
                retained.references >= release.references,
                "managed reference underflow"
            );
        }
        for (identity, release) in pending {
            let retained = ledger.storage.get_mut(&identity).unwrap();
            retained.references -= release.references;
            if retained.references == 0 {
                ledger.storage.remove(&identity);
                ledger.live -= release.bytes;
            }
        }
    }

    /// A refused recipient acquisition leaves the sender's ownership intact.
    /// Commit the destination before disposing the original request according
    /// to the actor operation's established admission/completion contract.
    pub fn transfer(
        &self,
        destination: &Self,
        storage: &[ManagedStorage],
    ) -> Result<(), BudgetExceeded> {
        if Arc::ptr_eq(&self.0, &destination.0) {
            return Ok(());
        }
        let acquired = destination.acquire(storage)?;
        self.release(storage);
        acquired.commit();
        Ok(())
    }

    /// Reserve exact new payload capacity before allocating private storage.
    /// The descriptor's post-construction walk supplies its actual identities.
    pub fn reserve_new(&self, bytes: u64) -> Result<StorageReservation, BudgetExceeded> {
        let mut ledger = self.ledger();
        self.check(&ledger, bytes)?;
        ledger.reserved += bytes;
        Ok(StorageReservation {
            owner: self.clone(),
            bytes,
            committed: false,
        })
    }

    /// Reserve one container replacement without holding the ledger while
    /// typed copy callbacks execute. Unique growth credits existing capacity;
    /// COW retains old storage until the final replacement commits.
    ///
    /// # Errors
    /// Returns the unchanged account's budget refusal when growth will not fit.
    /// # Panics
    /// Panics if `old` does not describe this owner's current backing.
    pub fn prepare_backing_change(
        &self,
        old: Option<ManagedStorage>,
        bytes: u64,
        unique: bool,
    ) -> Result<BackingChange, BudgetExceeded> {
        let credit = if unique {
            old.map_or(0, |old| old.bytes)
        } else {
            0
        };
        let reserved = bytes.saturating_sub(credit);
        let mut ledger = self.ledger();
        self.check(&ledger, reserved)?;
        if let Some(old) = old {
            let held = ledger
                .storage
                .get(&old.identity)
                .expect("unowned managed mutation");
            assert_eq!(held.bytes, old.bytes);
            if unique {
                assert_eq!(held.references, 1);
            }
        }
        ledger.reserved += reserved;
        Ok(BackingChange {
            owner: self.clone(),
            old,
            bytes,
            reserved,
            committed: false,
        })
    }

    /// Reserve a unique backing's growth before its allocator can relocate it.
    /// Native COW uniqueness remains the container's authority; a retained
    /// alias also refuses this path and must acquire a distinct backing.
    pub fn prepare_resize(
        &self,
        storage: ManagedStorage,
        bytes: u64,
    ) -> Result<ResizePermit<'_>, ResizeError> {
        let mut ledger = self.ledger();
        let retained = ledger
            .storage
            .get(&storage.identity)
            .expect("unowned resize");
        assert_eq!(
            retained.bytes, storage.bytes,
            "stale managed backing capacity"
        );
        if retained.references != 1 {
            return Err(ResizeError::SharedStorage);
        }
        let growth = bytes.saturating_sub(storage.bytes);
        self.check(&ledger, growth).map_err(ResizeError::Exceeded)?;
        ledger.reserved += growth;
        Ok(ResizePermit {
            ledger,
            storage,
            bytes,
            reserved: growth,
            committed: false,
        })
    }
}

fn storage_counts(storage: &[ManagedStorage]) -> HashMap<usize, Retention> {
    let mut pending = HashMap::<usize, Retention>::new();
    for backing in storage {
        if backing.identity == 0 && backing.bytes == 0 {
            continue;
        }
        assert_ne!(
            backing.identity, 0,
            "nonempty managed backing has no identity"
        );
        let retained = pending.entry(backing.identity).or_insert(Retention {
            bytes: backing.bytes,
            references: 0,
        });
        assert_eq!(
            retained.bytes, backing.bytes,
            "inconsistent typed backing capacity"
        );
        retained.references = retained
            .references
            .checked_add(1)
            .expect("managed reference overflow");
    }
    pending
}

fn additional_bytes(ledger: &Ledger, pending: &HashMap<usize, Retention>) -> Option<u64> {
    pending
        .iter()
        .try_fold(0_u64, |bytes, (identity, acquired)| {
            if let Some(retained) = ledger.storage.get(identity) {
                assert_eq!(
                    retained.bytes, acquired.bytes,
                    "managed backing capacity changed without reservation"
                );
                Some(bytes)
            } else {
                bytes.checked_add(acquired.bytes)
            }
        })
}

fn insert_storage(ledger: &mut Ledger, pending: HashMap<usize, Retention>) {
    for (identity, acquired) in pending {
        let retained = ledger.storage.entry(identity).or_insert(Retention {
            bytes: acquired.bytes,
            references: 0,
        });
        retained.references = retained
            .references
            .checked_add(acquired.references)
            .expect("managed reference overflow");
    }
}

/// Container backing replacement. Typed child edges move with the existing
/// elements; only the changed backing edge is replaced by this permit.
#[derive(Debug)]
pub struct BackingChange {
    owner: ValueOwner,
    old: Option<ManagedStorage>,
    bytes: u64,
    reserved: u64,
    committed: bool,
}
impl BackingChange {
    /// Publish the replacement backing after the reserved mutation succeeds.
    /// # Panics
    /// Panics if its actual identity contradicts the reserved storage plan.
    pub fn commit(mut self, identity: usize) {
        let mut ledger = self.owner.ledger();
        if let Some(old) = self.old {
            let entry = ledger
                .storage
                .get_mut(&old.identity)
                .expect("lost managed backing");
            entry.references -= 1;
            if entry.references == 0 {
                ledger.storage.remove(&old.identity);
                ledger.live -= old.bytes;
            }
        }
        ledger.reserved -= self.reserved;
        let new = storage_counts(&[ManagedStorage {
            identity,
            bytes: self.bytes,
        }]);
        let added = additional_bytes(&ledger, &new).expect("invalid managed replacement");
        self.owner
            .check(&ledger, added)
            .expect("reserved managed replacement exceeded its plan");
        ledger.live += added;
        insert_storage(&mut ledger, new);
        self.committed = true;
    }
}
impl Drop for BackingChange {
    fn drop(&mut self) {
        if !self.committed {
            self.owner.ledger().reserved -= self.reserved;
        }
    }
}

/// A pre-commit acquisition. Refusing the surrounding ownership transition
/// releases every acquired reference, including aliases, without partial state.
#[derive(Debug)]
pub struct Acquisition {
    owner: ValueOwner,
    storage: Vec<ManagedStorage>,
    committed: bool,
}
impl Acquisition {
    pub fn commit(mut self) {
        self.committed = true;
    }
}
impl Drop for Acquisition {
    fn drop(&mut self) {
        if !self.committed {
            self.owner.release(&self.storage);
        }
    }
}

#[derive(Debug)]
pub struct StorageReservation {
    owner: ValueOwner,
    bytes: u64,
    committed: bool,
}
impl StorageReservation {
    /// Publish only already-reserved new payload. Identity sharing discovered
    /// by the actual typed walk refunds the unused part of the reservation.
    pub fn commit(mut self, storage: &[ManagedStorage]) {
        let pending = storage_counts(storage);
        let mut ledger = self.owner.ledger();
        let bytes = additional_bytes(&ledger, &pending).expect("reserved payload overflow");
        assert!(
            bytes <= self.bytes,
            "typed allocation exceeded its reserved payload"
        );
        ledger.reserved -= self.bytes;
        ledger.live += bytes;
        insert_storage(&mut ledger, pending);
        self.committed = true;
    }
}
impl Drop for StorageReservation {
    fn drop(&mut self) {
        if !self.committed {
            self.owner.ledger().reserved -= self.bytes;
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResizeError {
    Exceeded(BudgetExceeded),
    SharedStorage,
}

#[derive(Debug)]
pub struct ResizePermit<'a> {
    // Keep same-owner admission from adding an alias between uniqueness
    // preflight and relocation. Allocator work runs no language callbacks.
    ledger: MutexGuard<'a, Ledger>,
    storage: ManagedStorage,
    bytes: u64,
    reserved: u64,
    committed: bool,
}
impl ResizePermit<'_> {
    pub fn commit(mut self, identity: usize) {
        let ledger = &mut self.ledger;
        let previous = ledger
            .storage
            .remove(&self.storage.identity)
            .expect("resized backing lost its owner");
        assert_eq!(
            previous.references, 1,
            "resized backing acquired another alias"
        );
        ledger.reserved -= self.reserved;
        ledger.live = ledger.live - previous.bytes + self.bytes;
        if self.bytes != 0 {
            assert_ne!(identity, 0, "resized backing has no identity");
            assert!(
                ledger
                    .storage
                    .insert(
                        identity,
                        Retention {
                            bytes: self.bytes,
                            references: 1
                        }
                    )
                    .is_none(),
                "resize collided with retained storage"
            );
        }
        self.committed = true;
    }
}
impl Drop for ResizePermit<'_> {
    fn drop(&mut self) {
        if !self.committed {
            self.ledger.reserved -= self.reserved;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn storage(identity: usize, bytes: u64) -> ManagedStorage {
        ManagedStorage { identity, bytes }
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
