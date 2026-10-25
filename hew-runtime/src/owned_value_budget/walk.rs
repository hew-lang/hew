//! Transactional use of the canonical typed storage visitor. A retained backing
//! contributes its descendants once to an incarnation, independently of aliases.

use super::{Acquisition, Ledger, ManagedStorage, Retention, StorageReservation, ValueOwner};
use core::ffi::c_void;
use hew_cabi::managed::{
    visit_value, HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, HEAP_EXCEEDED,
    SKIP_DESCENDANTS, UNDESCRIBED,
};
use hew_cabi::value::HewValueLayout;

struct Walk<'a> {
    owner: &'a ValueOwner,
    ledger: &'a mut Ledger,
    kind: HewManagedWalkKind,
    undo: Vec<(usize, Option<Retention>)>,
    edges: Vec<ManagedStorage>,
    fresh: u64,
}

impl Walk<'_> {
    fn rollback(&mut self) {
        for (identity, previous) in self.undo.drain(..).rev() {
            if let Some(current) = self.ledger.storage.remove(&identity) {
                self.ledger.live -= current.bytes;
            }
            if let Some(previous) = previous {
                self.ledger.live += previous.bytes;
                self.ledger.storage.insert(identity, previous);
            }
        }
    }

    fn edge(&mut self, identity: usize, bytes: u64) -> i32 {
        if identity == 0 {
            if self.kind != HewManagedWalkKind::ClonePlan {
                return UNDESCRIBED;
            }
            let Some(fresh) = self.fresh.checked_add(bytes) else {
                return HEAP_EXCEEDED;
            };
            if self.owner.check(self.ledger, fresh).is_err() {
                return HEAP_EXCEEDED;
            }
            self.fresh = fresh;
            return DESCEND;
        }
        let previous = self.ledger.storage.get(&identity).copied();
        if previous.is_some_and(|retained| retained.bytes != bytes) {
            return UNDESCRIBED;
        }
        if self.kind == HewManagedWalkKind::Release {
            let Some(previous) = previous else {
                return UNDESCRIBED;
            };
            self.undo.push((identity, Some(previous)));
            if previous.references == 1 {
                self.ledger.storage.remove(&identity);
                self.ledger.live -= bytes;
                DESCEND
            } else {
                self.ledger.storage.get_mut(&identity).unwrap().references -= 1;
                SKIP_DESCENDANTS
            }
        } else {
            let references = match previous {
                Some(previous) => match previous.references.checked_add(1) {
                    Some(references) => references,
                    None => return HEAP_EXCEEDED,
                },
                None => {
                    if bytes
                        .checked_add(self.fresh)
                        .is_none_or(|charge| self.owner.check(self.ledger, charge).is_err())
                    {
                        return HEAP_EXCEEDED;
                    }
                    self.ledger.live += bytes;
                    1
                }
            };
            self.undo.push((identity, previous));
            self.ledger
                .storage
                .insert(identity, Retention { bytes, references });
            // Zero-width nodes still deduplicate their typed descendants.
            self.edges.push(ManagedStorage { identity, bytes });
            if previous.is_some() {
                SKIP_DESCENDANTS
            } else {
                DESCEND
            }
        }
    }

    unsafe fn value(&mut self, layout: &HewValueLayout, slot: *const c_void) -> i32 {
        let visitor = HewManagedStorageVisitor {
            context: (self as *mut Self).cast(),
            storage: edge,
            kind: self.kind,
        };
        // SAFETY: the caller retains the concrete slot and its defining layout.
        unsafe { visit_value(layout, slot, &raw const visitor) }
    }
}

unsafe extern "C" fn edge(context: *mut c_void, identity: *const c_void, bytes: u64) -> i32 {
    // SAFETY: the synchronous typed walk exclusively lends its transaction.
    unsafe { (&mut *context.cast::<Walk<'_>>()).edge(identity as usize, bytes) }
}

impl ValueOwner {
    /// Acquire typed descendants atomically. The guard rolls back every edge
    /// until the surrounding ownership transition commits.
    ///
    /// # Safety
    /// The live slot matches layout and cannot mutate during the traversal.
    pub unsafe fn acquire_value(
        &self,
        layout: &HewValueLayout,
        slot: *const c_void,
    ) -> Result<Acquisition, i32> {
        let mut ledger = self.ledger();
        let mut walk = Walk {
            owner: self,
            ledger: &mut ledger,
            kind: HewManagedWalkKind::Retain,
            undo: Vec::new(),
            edges: Vec::new(),
            fresh: 0,
        };
        // SAFETY: the caller supplies a stable concrete value.
        let status = unsafe { walk.value(layout, slot) };
        if status != 0 {
            walk.rollback();
            return Err(status);
        }
        Ok(Acquisition {
            owner: self.clone(),
            storage: walk.edges,
            committed: false,
        })
    }

    /// Release descendants only when the owner's last edge to a backing ends.
    /// Missing metadata refuses the whole transition without partial refunds.
    ///
    /// # Safety
    /// The value has not yet been destroyed and belongs to this owner.
    pub unsafe fn release_value(
        &self,
        layout: &HewValueLayout,
        slot: *const c_void,
    ) -> Result<(), i32> {
        let mut ledger = self.ledger();
        let mut walk = Walk {
            owner: self,
            ledger: &mut ledger,
            kind: HewManagedWalkKind::Release,
            undo: Vec::new(),
            edges: Vec::new(),
            fresh: 0,
        };
        // SAFETY: the caller retains the value until after its release walk.
        let status = unsafe { walk.value(layout, slot) };
        if status != 0 {
            walk.rollback();
            return Err(status);
        }
        Ok(())
    }

    /// Reserve an exact semantic copy before running its consuming clone glue.
    /// Shared edges charge only new owner retention; private copies report their
    /// actual planned capacity through the same descriptor visitor.
    ///
    /// # Safety
    /// The source matches layout and remains live through copy publication.
    pub unsafe fn reserve_clone(
        &self,
        layout: &HewValueLayout,
        slot: *const c_void,
    ) -> Result<StorageReservation, i32> {
        // SAFETY: this is the single-source form of the same typed copy plan.
        unsafe { self.reserve_materialization(0, &[(layout, slot, 1)]) }
    }

    /// Reserve private payload and typed semantic copies as one acquisition.
    ///
    /// # Safety
    /// Every source matches its descriptor and remains live through publication.
    pub unsafe fn reserve_materialization(
        &self,
        fresh: u64,
        copies: &[(&HewValueLayout, *const c_void, u64)],
    ) -> Result<StorageReservation, i32> {
        let mut ledger = self.ledger();
        self.check(&ledger, fresh).map_err(|_| HEAP_EXCEEDED)?;
        let before = ledger.live;
        let mut walk = Walk {
            owner: self,
            ledger: &mut ledger,
            kind: HewManagedWalkKind::ClonePlan,
            undo: Vec::new(),
            edges: Vec::new(),
            fresh,
        };
        let mut status = 0;
        'sources: for (layout, slot, count) in copies {
            for _ in 0..*count {
                // SAFETY: clone planning only reads live typed storage.
                status = unsafe { walk.value(layout, *slot) };
                if status != 0 {
                    break 'sources;
                }
            }
        }
        let bytes = (walk.ledger.live - before).checked_add(walk.fresh);
        walk.rollback();
        if status != 0 {
            return Err(status);
        }
        let bytes = bytes.ok_or(HEAP_EXCEEDED)?;
        self.check(&ledger, bytes).map_err(|_| HEAP_EXCEEDED)?;
        ledger.reserved += bytes;
        Ok(StorageReservation {
            owner: self.clone(),
            bytes,
            committed: false,
        })
    }
}

impl StorageReservation {
    /// Publish a typed result against the capacity acquired before construction.
    /// The concrete walk remains the authority for retained backing identities.
    ///
    /// # Safety
    /// The result matches layout and remains unpublished until this succeeds.
    pub unsafe fn commit_value(
        mut self,
        layout: &HewValueLayout,
        slot: *const c_void,
    ) -> Result<(), i32> {
        let mut ledger = self.owner.ledger();
        ledger.reserved -= self.bytes;
        let before = ledger.live;
        let mut walk = Walk {
            owner: &self.owner,
            ledger: &mut ledger,
            kind: HewManagedWalkKind::Retain,
            undo: Vec::new(),
            edges: Vec::new(),
            fresh: 0,
        };
        // SAFETY: the caller retains the freshly constructed, unpublished value.
        let status = unsafe { walk.value(layout, slot) };
        if status != 0 || walk.ledger.live - before > self.bytes {
            walk.rollback();
            ledger.reserved += self.bytes;
            return Err(if status == 0 { UNDESCRIBED } else { status });
        }
        self.committed = true;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::managed::HewManagedValueDescriptor;
    use hew_cabi::value::HewTypeOwnershipKind;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static DESCENDANTS: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn tree(
        slot: *const c_void,
        visitor: *const HewManagedStorageVisitor,
    ) -> i32 {
        // SAFETY: each test lends two stable backing identities to this visitor.
        unsafe {
            let nodes = &*slot.cast::<[usize; 2]>();
            let visitor = &*visitor;
            match (visitor.storage)(visitor.context, nodes[0] as *const c_void, 8) {
                SKIP_DESCENDANTS => return 0,
                DESCEND => {}
                failure => return failure,
            }
            DESCENDANTS.fetch_add(1, Ordering::Relaxed);
            match (visitor.storage)(visitor.context, nodes[1] as *const c_void, 16) {
                DESCEND | SKIP_DESCENDANTS => 0,
                failure => failure,
            }
        }
    }

    fn layout() -> HewValueLayout {
        static MANAGED: HewManagedValueDescriptor = HewManagedValueDescriptor {
            slot_bytes: 8,
            visit: Some(tree),
        };
        HewValueLayout {
            size: 16,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed: &raw const MANAGED,
        }
    }

    #[test]
    fn graph_aliases_skip_descendants_until_last_owner_release() {
        let owner = ValueOwner::new(Some(24));
        let other = ValueOwner::new(Some(24));
        let nodes = [1_usize, 2];
        let layout = layout();
        DESCENDANTS.store(0, Ordering::Relaxed);
        // SAFETY: this descriptor reads only the test's stable identities.
        unsafe {
            owner
                .acquire_value(&layout, (&raw const nodes).cast())
                .unwrap()
                .commit();
            owner
                .acquire_value(&layout, (&raw const nodes).cast())
                .unwrap()
                .commit();
            assert_eq!(DESCENDANTS.load(Ordering::Relaxed), 1);
            other
                .acquire_value(&layout, (&raw const nodes).cast())
                .unwrap()
                .commit();
            assert_eq!(owner.live_bytes(), 24);
            assert_eq!(other.live_bytes(), 24);
            owner
                .release_value(&layout, (&raw const nodes).cast())
                .unwrap();
            assert_eq!(owner.live_bytes(), 24);
            owner
                .release_value(&layout, (&raw const nodes).cast())
                .unwrap();
            assert_eq!(owner.live_bytes(), 0);
            assert_eq!(other.live_bytes(), 24);
            other
                .release_value(&layout, (&raw const nodes).cast())
                .unwrap();
            assert_eq!(other.live_bytes(), 0);
        }
    }

    #[test]
    fn refused_descendant_rolls_back_parent_and_reservation() {
        let owner = ValueOwner::new(Some(16));
        let nodes = [3_usize, 4];
        let layout = layout();
        // SAFETY: the test's descriptor never dereferences its backing IDs.
        unsafe {
            assert!(matches!(
                owner.acquire_value(&layout, (&raw const nodes).cast()),
                Err(HEAP_EXCEEDED)
            ));
            assert_eq!(owner.live_bytes(), 0);
            assert!(matches!(
                owner.reserve_clone(&layout, (&raw const nodes).cast()),
                Err(HEAP_EXCEEDED)
            ));
            assert_eq!(owner.live_bytes(), 0);
            owner
                .acquire(&[ManagedStorage {
                    identity: 3,
                    bytes: 16,
                }])
                .unwrap()
                .commit();
            assert_eq!(owner.live_bytes(), 16);
        }
    }
}
