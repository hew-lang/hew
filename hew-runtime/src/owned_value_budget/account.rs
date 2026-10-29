//! Managed heap ledger and transactional allocation guards used by `ValueOwner`.
use crate::runtime_sync::{Mutex, MutexGuard};
use std::collections::HashMap;
use std::sync::Arc;

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
pub(super) struct Retention {
    pub(super) bytes: u64,
    pub(super) references: u64,
}

#[derive(Debug, Default)]
pub(super) struct Ledger {
    pub(super) live: u64,
    pub(super) reserved: u64,
    pub(super) storage: HashMap<usize, Retention>,
}

#[derive(Debug)]
pub(super) struct Account {
    pub(super) limit: Option<u64>,
    pub(super) ledger: Mutex<Ledger>,
    pub(super) incarnation: std::sync::OnceLock<crate::lifetime::live_actors::ActorIncarnation>,
}

/// Stable actor-incarnation identity, independent of its current worker.
/// The caller represents an unbounded actor with `None`, avoiding an account
/// allocation and any descriptor walk on ordinary unbounded retain/release.
#[derive(Clone, Debug)]
pub struct ValueOwner(pub(super) Arc<Account>);

impl ValueOwner {
    #[must_use]
    pub fn new(limit: Option<u64>) -> Self {
        Self(Arc::new(Account {
            limit,
            ledger: Mutex::new(Ledger::default()),
            incarnation: std::sync::OnceLock::new(),
        }))
    }

    pub(super) fn ledger(&self) -> MutexGuard<'_, Ledger> {
        self.0
            .ledger
            .lock()
            .expect("managed value account lock poisoned")
    }

    #[must_use]
    pub fn live_bytes(&self) -> u64 {
        self.ledger().live
    }

    /// Move an exclusive shallow payload charge into its new storage carriers.
    /// Typed descendants retain their existing edges; the total charge never
    /// changes, so a queue pop succeeds even when the account is at its limit.
    ///
    /// # Errors
    /// Returns `UNDESCRIBED` for stale, shared, colliding or unequal geometry.
    pub(crate) fn repartition_backing(
        &self,
        source: ManagedStorage,
        replacements: &[ManagedStorage],
    ) -> Result<(), i32> {
        use hew_cabi::managed::UNDESCRIBED;
        let mut identities = std::collections::HashSet::new();
        let mut total = 0_u64;
        for replacement in replacements {
            if replacement.identity == 0 || !identities.insert(replacement.identity) {
                return Err(UNDESCRIBED);
            }
            total = total.checked_add(replacement.bytes).ok_or(UNDESCRIBED)?;
        }
        if total != source.bytes {
            return Err(UNDESCRIBED);
        }
        let mut ledger = self.ledger();
        if !ledger
            .storage
            .get(&source.identity)
            .is_some_and(|held| held.bytes == source.bytes && held.references == 1)
            || replacements.iter().any(|replacement| {
                replacement.identity != source.identity
                    && ledger.storage.contains_key(&replacement.identity)
            })
        {
            return Err(UNDESCRIBED);
        }
        ledger.storage.remove(&source.identity);
        for replacement in replacements {
            ledger.storage.insert(
                replacement.identity,
                Retention {
                    bytes: replacement.bytes,
                    references: 1,
                },
            );
        }
        Ok(())
    }

    pub(super) fn check(&self, ledger: &Ledger, bytes: u64) -> Result<(), BudgetExceeded> {
        if super::budget_limit::admits(ledger.live, ledger.reserved, bytes, self.0.limit) {
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
    /// # Errors
    /// Returns a quota refusal if the complete acquisition cannot fit.
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
    /// # Panics
    /// Panics if a backing, capacity or reference count differs from its retained ownership.
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
    /// # Errors
    /// Returns the destination quota refusal, leaving source ownership intact.
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
    /// # Errors
    /// Returns a quota refusal if the additional payload cannot fit.
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
        match old {
            Some(old) => self.prepare_backing_replacement(&[(old, unique)], bytes),
            None => self.prepare_backing_replacement(&[], bytes),
        }
    }

    /// Reserve a replacement that retires several flat backing interests.
    /// Each boolean permits credit for an allocation moved without a semantic
    /// copy. Credit applies once per identity and only when all its interests
    /// retire; surviving aliases and COW copies keep the old payload charged.
    ///
    /// # Errors
    /// Returns a budget refusal without retiring any input interest.
    /// # Panics
    /// Panics if an input does not describe its current owned backing or
    /// requests more interests than the account retains.
    pub fn prepare_backing_replacement(
        &self,
        retired: &[(ManagedStorage, bool)],
        bytes: u64,
    ) -> Result<BackingChange, BudgetExceeded> {
        let old: Vec<_> = retired.iter().map(|(storage, _)| *storage).collect();
        let counts = storage_counts(&old);
        let mut ledger = self.ledger();
        let mut credit = 0_u64;
        for (identity, interests) in &counts {
            let held = ledger
                .storage
                .get(identity)
                .expect("unowned managed mutation");
            assert_eq!(held.bytes, interests.bytes);
            assert!(interests.references <= held.references);
            if interests.references == held.references
                && retired
                    .iter()
                    .filter(|(storage, _)| storage.identity == *identity)
                    .all(|(_, allow_credit)| *allow_credit)
            {
                credit = credit
                    .checked_add(held.bytes)
                    .expect("invalid backing credit");
            }
        }
        let reserved = bytes.saturating_sub(credit);
        self.check(&ledger, reserved)?;
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
    /// # Errors
    /// Returns shared-storage or quota refusal without changing the backing.
    /// # Panics
    /// Panics if the supplied backing identity or capacity is stale.
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
    old: Vec<ManagedStorage>,
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
        for old in &self.old {
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
    pub(super) owner: ValueOwner,
    pub(super) storage: Vec<ManagedStorage>,
    pub(super) committed: bool,
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
    pub(super) owner: ValueOwner,
    pub(super) bytes: u64,
    pub(super) committed: bool,
}
impl StorageReservation {
    /// Publish only already-reserved new payload. Identity sharing discovered
    /// by the actual typed walk refunds the unused part of the reservation.
    /// # Panics
    /// Panics if the published payload exceeds its reservation or changes retained backing geometry.
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
    /// # Panics
    /// Panics if the reserved backing changed ownership or its replacement identity collides.
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
