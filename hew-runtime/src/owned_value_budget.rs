//! Owner accounts for the language's managed payload storage.
//!
//! A charge describes typed payload capacity, not an allocator's block size or
//! the process heap. Allocator headers, reference counts and indexing caches do
//! not contribute. Storage descriptors supply canonical, target-independent
//! widths; this module never infers a charge from a Rust or host-object layout.
//!
//! Each owner pays once for a backing allocation while retaining any reference
//! to it. Separate owners each pay its full charge, including shared immutable
//! storage. Accounts survive worker migration and delayed release. Reservations
//! fail before mutation and return an ordinary error for the checked language
//! fault continuation; no allocator or C-ABI frame unwinds here.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};

/// A failed reservation. `None` is an unbounded account whose arithmetic
/// overflowed; an explicit zero limit permits no positive payload charge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BudgetExceeded {
    pub limit: Option<u64>,
    pub live: u64,
    pub requested: u64,
}

#[derive(Debug)]
struct Account {
    limit: Option<u64>,
    live: AtomicU64,
}

/// Stable ownership identity, independent of a worker or actor's address.
#[derive(Clone, Debug)]
pub struct ValueOwner(Arc<Account>);

impl ValueOwner {
    #[must_use]
    pub fn new(limit: Option<u64>) -> Self {
        Self(Arc::new(Account {
            limit,
            live: AtomicU64::new(0),
        }))
    }

    #[must_use]
    pub fn live_bytes(&self) -> u64 {
        self.0.live.load(Ordering::Acquire)
    }

    fn identity(&self) -> usize {
        Arc::as_ptr(&self.0) as usize
    }

    fn reserve(&self, bytes: u64) -> Result<(), BudgetExceeded> {
        self.0
            .live
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |live| {
                live.checked_add(bytes)
                    .filter(|next| self.0.limit.is_none_or(|limit| *next <= limit))
            })
            .map(|_| ())
            .map_err(|live| BudgetExceeded {
                limit: self.0.limit,
                live,
                requested: bytes,
            })
    }

    fn refund(&self, bytes: u64) {
        let previous = self.0.live.fetch_sub(bytes, Ordering::AcqRel);
        assert!(previous >= bytes, "managed payload charge underflow");
    }
}

#[derive(Debug)]
struct Retention {
    owner: ValueOwner,
    references: u64,
}

#[derive(Debug)]
struct StorageState {
    bytes: u64,
    owners: HashMap<usize, Retention>,
}

impl Drop for StorageState {
    fn drop(&mut self) {
        assert!(self.owners.is_empty(), "retained managed storage was lost");
    }
}

/// One backing allocation's charge identity. Retaining this descriptor alone
/// does not own a language value; use `retain`/`release` at ownership boundaries.
#[derive(Clone, Debug)]
pub struct StorageCharge(Arc<Mutex<StorageState>>);

impl StorageCharge {
    #[must_use]
    pub fn new(bytes: u64) -> Self {
        Self(Arc::new(Mutex::new(StorageState {
            bytes,
            owners: HashMap::new(),
        })))
    }

    fn state(&self) -> MutexGuard<'_, StorageState> {
        self.0
            .lock()
            .expect("managed storage account lock poisoned")
    }

    /// Reserve a new owner's complete payload charge before publishing its
    /// retained value. An alias within the same owner adds no payload bytes.
    pub fn retain(&self, owner: &ValueOwner) -> Result<(), BudgetExceeded> {
        let mut state = self.state();
        if let Some(retention) = state.owners.get_mut(&owner.identity()) {
            retention.references = retention
                .references
                .checked_add(1)
                .expect("managed storage reference count overflow");
            return Ok(());
        }
        owner.reserve(state.bytes)?;
        state.owners.insert(
            owner.identity(),
            Retention {
                owner: owner.clone(),
                references: 1,
            },
        );
        Ok(())
    }

    /// Release the named owner, even when cleanup runs on another worker.
    pub fn release(&self, owner: &ValueOwner) {
        let mut state = self.state();
        let retention = state
            .owners
            .get_mut(&owner.identity())
            .expect("managed storage released by an unrelated owner");
        retention.references -= 1;
        if retention.references == 0 {
            let retention = state
                .owners
                .remove(&owner.identity())
                .expect("managed storage owner disappeared");
            retention.owner.refund(state.bytes);
        }
    }

    /// Adopt a moved value transactionally. A refused destination leaves the
    /// source owner and its charge intact.
    pub fn transfer(
        &self,
        source: &ValueOwner,
        destination: &ValueOwner,
    ) -> Result<(), BudgetExceeded> {
        if Arc::ptr_eq(&source.0, &destination.0) {
            return Ok(());
        }
        self.retain(destination)?;
        self.release(source);
        Ok(())
    }

    /// Reserve growth before a unique buffer is reallocated. Committing follows
    /// successful reallocation; dropping the permit rolls the reservation back.
    /// Shared backing must first take the ordinary COW path with a new charge.
    pub fn prepare_resize<'a>(
        &'a mut self,
        owner: &ValueOwner,
        new_bytes: u64,
    ) -> Result<ResizePermit<'a>, ResizeError> {
        let state = self.state();
        let unique = state.owners.len() == 1
            && state
                .owners
                .get(&owner.identity())
                .is_some_and(|retention| retention.references == 1);
        if !unique {
            return Err(ResizeError::SharedStorage);
        }
        let reserved = new_bytes.saturating_sub(state.bytes);
        owner.reserve(reserved).map_err(ResizeError::Exceeded)?;
        Ok(ResizePermit {
            state,
            owner: owner.clone(),
            new_bytes,
            reserved,
            committed: false,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ResizeError {
    Exceeded(BudgetExceeded),
    SharedStorage,
}

/// Holds only the storage's bookkeeping lock during a backing allocation's
/// resize. No language callback or failure continuation may run under it.
#[derive(Debug)]
pub struct ResizePermit<'a> {
    state: MutexGuard<'a, StorageState>,
    owner: ValueOwner,
    new_bytes: u64,
    reserved: u64,
    committed: bool,
}

impl ResizePermit<'_> {
    pub fn commit(mut self) {
        let refund = self.state.bytes.saturating_sub(self.new_bytes);
        self.state.bytes = self.new_bytes;
        self.owner.refund(refund);
        self.committed = true;
    }
}

impl Drop for ResizePermit<'_> {
    fn drop(&mut self) {
        if !self.committed {
            self.owner.refund(self.reserved);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aliases_charge_each_owner_once_until_its_last_release() {
        let first = ValueOwner::new(Some(64));
        let second = ValueOwner::new(Some(64));
        let storage = StorageCharge::new(64);
        storage.retain(&first).unwrap();
        storage.retain(&first).unwrap();
        storage.retain(&second).unwrap();
        assert_eq!((first.live_bytes(), second.live_bytes()), (64, 64));
        storage.release(&first);
        assert_eq!(first.live_bytes(), 64);
        storage.release(&second);
        assert_eq!((first.live_bytes(), second.live_bytes()), (64, 0));
        storage.release(&first);
        assert_eq!(first.live_bytes(), 0);
    }

    #[test]
    fn refused_transfer_keeps_source_and_successful_transfer_refunds_it() {
        let source = ValueOwner::new(Some(64));
        let too_small = ValueOwner::new(Some(63));
        let destination = ValueOwner::new(Some(64));
        let storage = StorageCharge::new(64);
        storage.retain(&source).unwrap();
        assert!(storage.transfer(&source, &too_small).is_err());
        assert_eq!((source.live_bytes(), too_small.live_bytes()), (64, 0));
        storage.transfer(&source, &destination).unwrap();
        assert_eq!((source.live_bytes(), destination.live_bytes()), (0, 64));
        storage.release(&destination);
    }

    #[test]
    fn abandoned_resize_and_refused_growth_preserve_original_charge() {
        let owner = ValueOwner::new(Some(64));
        let mut storage = StorageCharge::new(32);
        storage.retain(&owner).unwrap();
        drop(storage.prepare_resize(&owner, 64).unwrap());
        assert_eq!(owner.live_bytes(), 32);
        assert!(matches!(
            storage.prepare_resize(&owner, 65),
            Err(ResizeError::Exceeded(_))
        ));
        assert_eq!(owner.live_bytes(), 32);
        storage.prepare_resize(&owner, 64).unwrap().commit();
        assert_eq!(owner.live_bytes(), 64);
        storage.prepare_resize(&owner, 16).unwrap().commit();
        assert_eq!(owner.live_bytes(), 16);
        storage.release(&owner);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn shared_storage_requires_cow_before_resize() {
        let owner = ValueOwner::new(Some(128));
        let mut storage = StorageCharge::new(32);
        storage.retain(&owner).unwrap();
        storage.retain(&owner).unwrap();
        assert!(matches!(
            storage.prepare_resize(&owner, 64),
            Err(ResizeError::SharedStorage)
        ));
        storage.release(&owner);
        storage.release(&owner);
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn release_on_another_worker_refunds_the_retained_account() {
        let owner = ValueOwner::new(Some(64));
        let storage = StorageCharge::new(64);
        storage.retain(&owner).unwrap();
        let cleanup_owner = owner.clone();
        std::thread::spawn(move || storage.release(&cleanup_owner))
            .join()
            .unwrap();
        assert_eq!(owner.live_bytes(), 0);
    }

    #[test]
    fn zero_is_a_real_limit_and_overflow_never_admits_storage() {
        let zero = ValueOwner::new(Some(0));
        let empty = StorageCharge::new(0);
        empty.retain(&zero).unwrap();
        empty.release(&zero);
        assert!(StorageCharge::new(1).retain(&zero).is_err());
        let unbounded = ValueOwner::new(None);
        let enormous = StorageCharge::new(u64::MAX);
        enormous.retain(&unbounded).unwrap();
        assert!(StorageCharge::new(1).retain(&unbounded).is_err());
        enormous.release(&unbounded);
        assert_eq!(unbounded.live_bytes(), 0);
    }
}
