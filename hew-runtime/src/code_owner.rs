//! Executable roots carried by canonical descriptors and active native frames.

use hew_cabi::code::{HewCodeLease, HewCodeLeaseSlot};

/// One defining-generation reference, independent of a value's payload owner.
#[derive(Debug)]
pub(crate) struct CodeRoot {
    lease: *const HewCodeLease,
}

// SAFETY: the descriptor contract requires thread-safe retain/release callbacks.
unsafe impl Send for CodeRoot {}

impl CodeRoot {
    /// Transfer a root into a carrier whose descriptor records its lease.
    /// # Safety
    /// The slot obeys `acquire`; the carrier must later transfer that root back
    /// through `take` exactly once, after its final use of descriptor code.
    pub(crate) unsafe fn retain_for_carrier(slot: *const HewCodeLeaseSlot) {
        // SAFETY: forwarded slot and caller-root obligations.
        std::mem::forget(unsafe { Self::acquire(slot) });
    }

    /// Take an existing carrier root without incrementing its reference count.
    /// # Safety
    /// The caller transfers exactly one retained carrier root for this slot.
    pub(crate) unsafe fn take(slot: *const HewCodeLeaseSlot) -> Self {
        // SAFETY: the transferred root retains the slot and its published lease.
        let lease = unsafe { slot.as_ref() }.map_or(std::ptr::null(), |slot| slot.lease);
        Self { lease }
    }

    /// Acquire an independent executable root before invoking descriptor code.
    /// # Safety
    /// A non-null slot and its published lease are valid and immutable. A caller
    /// root keeps the generation live until this acquisition completes.
    pub(crate) unsafe fn acquire(slot: *const HewCodeLeaseSlot) -> Self {
        // SAFETY: slot validity and publication are caller obligations.
        let lease = unsafe { slot.as_ref() }.map_or(std::ptr::null(), |slot| slot.lease);
        // SAFETY: the published lease remains valid throughout acquisition.
        if let Some(lease) = unsafe { lease.as_ref() } {
            // SAFETY: callback and context obey the host's thread-safe contract.
            unsafe { (lease.retain)(lease.context) };
        }
        Self { lease }
    }
}

impl Drop for CodeRoot {
    fn drop(&mut self) {
        // SAFETY: this root retains the lease record until the callback returns.
        if let Some(lease) = unsafe { self.lease.as_ref() } {
            let context = lease.context;
            let release = lease.release;
            // SAFETY: the root transfers its final owned reference. Nothing
            // accesses the descriptor or lease after this callback returns.
            unsafe { release(context) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::value::{HewTypeOwnershipKind, HewValueLayout};
    use std::ffi::c_void;
    use std::sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex,
    };

    struct Fixture {
        roots: AtomicUsize,
        carrier: Mutex<Option<CodeRoot>>,
        observed: AtomicUsize,
    }

    unsafe extern "C" fn retain(context: *const c_void) {
        // SAFETY: the fixture outlives every root and callback.
        unsafe { &*context.cast::<Fixture>() }
            .roots
            .fetch_add(1, Ordering::SeqCst);
    }

    unsafe extern "C" fn release(context: *const c_void) {
        // SAFETY: the fixture outlives every root and callback.
        let fixture = unsafe { &*context.cast::<Fixture>() };
        assert!(fixture.roots.fetch_sub(1, Ordering::SeqCst) > 0);
    }

    unsafe extern "C-unwind" fn close(slot: *mut c_void) {
        // SAFETY: the release cursor borrows this uniquely driven fixture.
        let fixture = unsafe { &*slot.cast::<Fixture>() };
        drop(fixture.carrier.lock().unwrap().take());
        // A collector must still see the active callback's independent root,
        // even after authored cleanup released the last retained carrier.
        fixture
            .observed
            .store(fixture.roots.load(Ordering::SeqCst), Ordering::SeqCst);
    }

    #[test]
    fn code_owner_last_carrier_cannot_retire_an_executing_destructor() {
        let fixture = Fixture {
            roots: AtomicUsize::new(0),
            carrier: Mutex::new(None),
            observed: AtomicUsize::new(0),
        };
        let lease = HewCodeLease {
            context: (&raw const fixture).cast(),
            retain,
            release,
        };
        let slot = HewCodeLeaseSlot {
            lease: &raw const lease,
        };
        // SAFETY: both records remain live until the complete release returns.
        *fixture.carrier.lock().unwrap() = Some(unsafe { CodeRoot::acquire(&raw const slot) });
        let layout = HewValueLayout {
            size: size_of::<Fixture>(),
            align: align_of::<Fixture>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: Some(close),
            release_start: None,
            code_owner: &raw const slot,
        };
        let cursor = crate::release_walker::HewReleaseCursor::values([(
            (&raw const fixture).cast_mut().cast(),
            layout,
        )]);
        // SAFETY: the cursor owns the semantic callback obligation; its outer
        // fixture storage and metadata remain valid through the synchronous walk.
        unsafe { crate::release_walker::hew_release_sync(cursor) };
        assert_eq!(fixture.observed.load(Ordering::SeqCst), 1);
        assert_eq!(fixture.roots.load(Ordering::SeqCst), 0);
    }
    #[test]
    fn code_owner_collection_snapshots_and_rc_weak_metadata_retain_code() {
        let fixture = Fixture {
            roots: AtomicUsize::new(0),
            carrier: Mutex::new(None),
            observed: AtomicUsize::new(0),
        };
        let lease = HewCodeLease {
            context: (&raw const fixture).cast(),
            retain,
            release,
        };
        let slot = HewCodeLeaseSlot {
            lease: &raw const lease,
        };
        let layout = HewValueLayout {
            size: 8,
            align: 8,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: &raw const slot,
        };
        // SAFETY: all carriers use this exact descriptor and are released before
        // its stack-backed fixture and lease go out of scope.
        unsafe {
            let vector = crate::vec::hew_vec_new_with_elem_layout(&raw const layout);
            let snapshot = crate::vec::hew_vec_clone_owned(vector);
            crate::vec::hew_vec_free_owned(vector);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 1);
            crate::vec::hew_vec_free_owned(snapshot);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 0);

            let mut key = hew_cabi::map::hew_layout_key_i64;
            key.value = layout;
            let map =
                crate::hashmap::hew_hashmap_new_with_layout(&raw const key, &raw const layout);
            let snapshot = crate::hashmap::hew_hashmap_clone_layout(map);
            crate::hashmap::hew_hashmap_free_layout(map);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 2);
            crate::hashmap::hew_hashmap_free_layout(snapshot);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 0);

            let value = 42_u64;
            let rc = crate::rc::hew_rc_new(
                (&raw const value).cast(),
                &raw const layout,
                c"retained".as_ptr(),
                std::ptr::null(),
            );
            let weak = crate::rc::hew_rc_downgrade(rc);
            crate::rc::hew_rc_drop(rc);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 1);
            crate::rc::hew_weak_drop_rc(weak);
            assert_eq!(fixture.roots.load(Ordering::SeqCst), 0);
        }
    }
}
