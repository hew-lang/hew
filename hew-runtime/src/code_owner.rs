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
}
