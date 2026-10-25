//! Typed Rc publication, creator retention and cell-transfer controls.

use super::*;
use crate::owned_value_budget::{
    transfer::{DestinationBinding, ValueTransfer},
    with_value_owner, ValueOwner,
};
use crate::release_walker::hew_release_sync;
use hew_cabi::managed::{visit_value, HewManagedStorageVisitor, HewManagedValueDescriptor};
use hew_cabi::string::HewString;
use hew_cabi::value::HewTypeOwnershipKind;

#[repr(C)]
struct Node {
    next: *mut u8,
    text: *mut HewString,
}

unsafe extern "C" fn visit_node(
    slot: *const c_void,
    visitor: *const HewManagedStorageVisitor,
) -> i32 {
    // SAFETY: the fixture owns this exact initialized record through every walk.
    unsafe {
        let node = &*slot.cast::<Node>();
        let status = visit_value(&RC, (&raw const node.next).cast(), visitor);
        if status != 0 {
            return status;
        }
        visit_value(&TEXT, (&raw const node.text).cast(), visitor)
    }
}
unsafe extern "C-unwind" fn drop_node(slot: *mut c_void) {
    // SAFETY: the cursor transfers this record once, retaining it through nested release.
    unsafe {
        let node = &mut *slot.cast::<Node>();
        hew_release_sync(hew_rc_release_begin(node.next, &raw const NODE));
        crate::string::hew_string_drop_managed(node.text);
    }
}
unsafe extern "C-unwind" fn drop_rc(slot: *mut c_void) {
    // SAFETY: this exact descriptor transfers one strong handle to semantic cleanup.
    unsafe {
        let value = slot.cast::<*mut u8>().read();
        if !value.is_null() {
            hew_release_sync(hew_rc_release_begin(
                value,
                &raw const (*header_from_data(value)).layout,
            ));
        }
    };
}
unsafe extern "C-unwind" fn drop_text(slot: *mut c_void) {
    // SAFETY: this exact slot transfers one accounted string handle.
    unsafe { crate::string::hew_string_drop_managed(slot.cast::<*mut HewString>().read()) };
}
static NODE: HewValueLayout = HewValueLayout {
    size: size_of::<Node>(),
    align: align_of::<Node>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_node),
    release_start: None,
    code_owner: ptr::null(),
    managed: &HewManagedValueDescriptor {
        slot_bytes: 16,
        visit: Some(visit_node),
    },
};
static RC: HewValueLayout = HewValueLayout {
    size: size_of::<*mut u8>(),
    align: align_of::<*mut u8>(),
    ownership_kind: HewTypeOwnershipKind::LayoutManaged,
    clone_fn: None,
    drop_fn: Some(drop_rc),
    release_start: None,
    code_owner: ptr::null(),
    managed: &HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(crate::owned_value_budget::visit::hew_managed_visit_shared),
    },
};
static TEXT: HewValueLayout = HewValueLayout {
    size: size_of::<*mut HewString>(),
    align: align_of::<*mut HewString>(),
    ownership_kind: HewTypeOwnershipKind::String,
    clone_fn: None,
    drop_fn: Some(drop_text),
    release_start: None,
    code_owner: ptr::null(),
    managed: &HewManagedValueDescriptor {
        slot_bytes: 8,
        visit: Some(crate::owned_value_budget::visit::hew_managed_visit_string),
    },
};
unsafe fn text(owner: &ValueOwner) -> *mut HewString {
    // SAFETY: the fresh fixture string is retained until its final typed release.
    unsafe {
        let text = hew_cabi::string::string_from_str("retain");
        owner
            .acquire_value(&TEXT, (&raw const text).cast())
            .unwrap()
            .commit();
        text
    }
}
unsafe fn new_node(owner: &ValueOwner, registry: &Arc<RcAllocations>, node: &Node) -> *mut u8 {
    // SAFETY: the account and both source/result descriptors cover this live record.
    unsafe {
        let mut status = 0;
        let value = with_value_owner(&Some(owner.clone()), || {
            hew_rc_new_managed(
                ptr::from_ref(node).cast(),
                &raw const NODE,
                c"Node".as_ptr(),
                ptr::null(),
                &raw const RC,
                &raw mut status,
            )
        });
        assert_eq!(status, 0);
        assert!(!value.is_null());
        registry.register(header_from_data(value));
        value
    }
}
unsafe fn clone_rc(owner: &ValueOwner, value: *mut u8) -> *mut u8 {
    // SAFETY: both the physical and managed strong edges are adopted by the fixture.
    unsafe {
        let alias = hew_rc_clone(value);
        owner
            .acquire_value(&RC, (&raw const alias).cast())
            .unwrap()
            .commit();
        alias
    }
}
unsafe fn release_node(
    owner: &ValueOwner,
    value: *mut u8,
) -> *mut crate::release_walker::HewReleaseCursor {
    with_value_owner(&Some(owner.clone()), || {
        // SAFETY: the fixture binds this typed source handle's actual account.
        unsafe { hew_rc_release_begin(value, &raw const NODE) }
    })
}

#[test]
fn synchronous_source_release_refunds_before_reuse() {
    // SAFETY: each checked constructor transfers the exact Node input to its cell.
    unsafe {
        let owner = ValueOwner::new(Some(22));
        let registry = Arc::new(RcAllocations::default());
        for _ in 0..2 {
            let text = text(&owner);
            let value = new_node(
                &owner,
                &registry,
                &Node {
                    next: ptr::null_mut(),
                    text,
                },
            );
            assert_eq!(owner.live_bytes(), 22);
            // A physical candidate has a raw retained handle but no account edge.
            let candidate = hew_rc_clone(value);
            with_value_owner(&None, || {
                hew_release_sync(hew_rc_release_begin(candidate, &raw const NODE));
            });
            assert_eq!(owner.live_bytes(), 22);
            with_value_owner(&Some(owner.clone()), || hew_rc_drop_managed(value));
            assert_eq!(owner.live_bytes(), 0);
            assert!(registry.is_empty());
        }
    }
}
unsafe fn cycle(owner: &ValueOwner, registry: &Arc<RcAllocations>) -> *mut u8 {
    // SAFETY: all record inputs are typed owners; each set moves them only after admission.
    unsafe {
        let first_text = text(owner);
        let first = new_node(
            owner,
            registry,
            &Node {
                next: ptr::null_mut(),
                text: first_text,
            },
        );
        let second_text = crate::string::hew_string_clone(first_text);
        owner
            .acquire_value(&TEXT, (&raw const second_text).cast())
            .unwrap()
            .commit();
        let second = new_node(
            owner,
            registry,
            &Node {
                next: clone_rc(owner, first),
                text: second_text,
            },
        );
        let replacement_text = crate::string::hew_string_clone(first_text);
        owner
            .acquire_value(&TEXT, (&raw const replacement_text).cast())
            .unwrap()
            .commit();
        let mut replacement = Node {
            next: clone_rc(owner, second),
            text: replacement_text,
        };
        let mut status = 0;
        let old = hew_rc_set_release_checked(
            first,
            (&raw mut replacement).cast(),
            &raw const NODE,
            &raw mut status,
        );
        assert_eq!(status, 0);
        hew_release_sync(old);
        hew_release_sync(release_node(owner, second));
        first
    }
}

#[test]
fn quota_refusal_preserves_input_and_final_alias_holds_payload_until_cleanup() {
    // SAFETY: all raw slots are uniquely owned by this test through their typed release.
    unsafe {
        let small = ValueOwner::new(Some(21));
        let original = text(&small);
        let node = Node {
            next: ptr::null_mut(),
            text: original,
        };
        let mut status = 0;
        let refused = with_value_owner(&Some(small.clone()), || {
            hew_rc_new_managed(
                (&raw const node).cast(),
                &raw const NODE,
                c"Node".as_ptr(),
                ptr::null(),
                &raw const RC,
                &raw mut status,
            )
        });
        assert!(refused.is_null());
        assert_eq!(status, hew_cabi::managed::HEAP_EXCEEDED);
        assert_eq!(node.text, original);
        assert_eq!(small.live_bytes(), 6);
        with_value_owner(&Some(small.clone()), || {
            crate::string::hew_string_drop_managed(original);
        });
        assert_eq!(small.live_bytes(), 0);

        let owner = ValueOwner::new(Some(22));
        let registry = Arc::new(RcAllocations::default());
        let text = text(&owner);
        let value = new_node(
            &owner,
            &registry,
            &Node {
                next: ptr::null_mut(),
                text,
            },
        );
        let alias = hew_rc_clone_managed(value);
        let weak = hew_rc_downgrade(value);
        let upgraded = hew_weak_upgrade_rc_managed(weak);
        assert_eq!(upgraded, value);
        hew_release_sync(release_node(&owner, upgraded));
        hew_release_sync(release_node(&owner, value));
        assert_eq!(owner.live_bytes(), 22);
        let cursor = release_node(&owner, alias);
        assert!(hew_weak_upgrade_rc_managed(weak).is_null());
        hew_weak_drop_rc(weak);
        assert!(registry.is_empty());
        assert_eq!(owner.live_bytes(), 22);
        hew_release_sync(cursor);
        assert_eq!(owner.live_bytes(), 0);
    }
}

#[test]
#[expect(
    clippy::cast_ptr_alignment,
    reason = "the checked Rc constructor allocates this exact Node layout"
)]
fn checked_set_keeps_displaced_children_charged_and_rolls_back_refusal() {
    // SAFETY: the replacement slot stays live until its returned cursor drains.
    unsafe {
        let owner = ValueOwner::new(Some(28));
        let registry = Arc::new(RcAllocations::default());
        let old_text = text(&owner);
        let value = new_node(
            &owner,
            &registry,
            &Node {
                next: ptr::null_mut(),
                text: old_text,
            },
        );
        let new_text = text(&owner);
        let mut replacement = Node {
            next: ptr::null_mut(),
            text: new_text,
        };
        let mut status = 0;
        let cursor = hew_rc_set_release_checked(
            value,
            (&raw mut replacement).cast(),
            &raw const NODE,
            &raw mut status,
        );
        assert_eq!(status, 0);
        assert_eq!(replacement.text, old_text);
        assert_eq!((*value.cast::<Node>()).text, new_text);
        assert_eq!(owner.live_bytes(), 28);
        hew_release_sync(cursor);
        assert_eq!(owner.live_bytes(), 22);
        let foreign = hew_cabi::string::string_from_str("too much replacement");
        let mut refused = Node {
            next: ptr::null_mut(),
            text: foreign,
        };
        let cursor = hew_rc_set_release_checked(
            value,
            (&raw mut refused).cast(),
            &raw const NODE,
            &raw mut status,
        );
        assert!(cursor.is_null());
        assert_eq!(status, hew_cabi::managed::HEAP_EXCEEDED);
        assert_eq!(refused.text, foreign);
        assert_eq!((*value.cast::<Node>()).text, new_text);
        assert_eq!(owner.live_bytes(), 22);
        crate::string::hew_string_drop(foreign);
        hew_release_sync(release_node(&owner, value));
        assert_eq!(owner.live_bytes(), 0);
    }
}

#[test]
fn cyclic_cells_rehome_shared_descendants_without_leaving_creator_debt() {
    // SAFETY: the transfer owns the unique external root; both registries outlive cleanup.
    unsafe {
        let source = ValueOwner::new(Some(38));
        let destination = ValueOwner::new(Some(38));
        let old = Arc::new(RcAllocations::default());
        let new = Arc::new(RcAllocations::default());
        let value = cycle(&source, &old);
        assert_eq!(source.live_bytes(), 38);
        let mut transfer = ValueTransfer::begin(
            Some(source.clone()),
            Some(destination.clone()),
            &raw const RC,
            (&raw const value).cast(),
        );
        transfer.set_destination_binding(Some(DestinationBinding {
            actor_turn: crate::lifetime::live_actors::ActorIncarnation::NONE,
            rc_owner: Some(new.clone()),
        }));
        assert!(transfer.poll_prepare().unwrap());
        transfer.commit().unwrap();
        assert_eq!((source.live_bytes(), destination.live_bytes()), (0, 38));
        assert!(old.close_empty());
        assert_eq!(new.inner.lock_or_recover().blocks.len(), 2);
        hew_release_sync(release_node(&destination, value));
        assert_eq!(destination.live_bytes(), 38);
        let cursor = new.sweep().unwrap();
        (*cursor).set_value_owner(Some(destination.clone()));
        hew_release_sync(cursor);
        assert_eq!(destination.live_bytes(), 0);
    }
}

#[test]
fn abandoned_transfer_holds_sweep_until_abort_then_refunds_cycle() {
    // SAFETY: the pending transfer and sweep retain the same cell graph until abort.
    unsafe {
        let source = ValueOwner::new(Some(38));
        let destination = ValueOwner::new(Some(37));
        let registry = Arc::new(RcAllocations::default());
        let value = cycle(&source, &registry);
        let mut transfer = ValueTransfer::begin(
            Some(source.clone()),
            Some(destination.clone()),
            &raw const RC,
            (&raw const value).cast(),
        );
        assert!(transfer.poll_freeze().unwrap());
        let cursor = registry.sweep().unwrap();
        (*cursor).set_value_owner(Some(source.clone()));
        let (ready, waker) = crate::wake::blocking::Readiness::new();
        let state = crate::coro_state::hew_coro_state_new(waker.descriptor(), ptr::null_mut());
        let mut slot = ptr::null_mut();
        assert_eq!(
            crate::release_walker::hew_release_poll(cursor, state, &raw mut slot),
            0
        );
        assert!(!registry.close_empty());
        assert_eq!(transfer.acquire(), Err(hew_cabi::managed::HEAP_EXCEEDED));
        drop(transfer);
        assert_eq!((source.live_bytes(), destination.live_bytes()), (38, 0));
        ready.wait();
        hew_release_sync(release_node(&source, value));
        hew_release_sync(cursor);
        assert_eq!(source.live_bytes(), 0);
        crate::coro_state::hew_coro_state_free(state);
    }
}
