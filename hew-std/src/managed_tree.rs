//! Canonical payload facts for the encoding providers' indivisible value trees.
//!
//! Node widths describe the format's typed value, not serde's Rust layout.
//! JSON/TOML copies own independent trees; YAML shares a whole immutable tree.
//! No public operation shares a subtree independently of its root.

use core::ffi::c_void;
use hew_cabi::managed::{
    HewManagedStorageVisitor, HewManagedWalkKind, DESCEND, HEAP_EXCEEDED, SKIP_DESCENDANTS,
    UNDESCRIBED,
};

pub(crate) type Payload = Result<u64, i32>;

/// Publish one tree's complete payload. The callback controls acquisition,
/// release and refusal; providers neither mutate values nor update accounts.
///
/// # Safety
/// `visitor` and its context must remain live for this synchronous call.
pub(crate) unsafe fn tree(
    visitor: *const HewManagedStorageVisitor,
    identity: *const c_void,
    shared_copy: bool,
    payload: impl FnOnce(bool) -> Payload,
) -> i32 {
    // SAFETY: the caller supplies a valid visitor or null.
    let Some(visitor) = (unsafe { visitor.as_ref() }) else {
        return UNDESCRIBED;
    };
    let copy = visitor.kind == HewManagedWalkKind::ClonePlan && !shared_copy;
    let bytes = match payload(copy) {
        Ok(bytes) => bytes,
        Err(status) => return status,
    };
    let identity = if copy { core::ptr::null() } else { identity };
    // SAFETY: the callback borrows its live context and never takes the tree.
    match unsafe { (visitor.storage)(visitor.context, identity, bytes) } {
        DESCEND | SKIP_DESCENDANTS => 0,
        failure => failure,
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[derive(Default)]
    pub(crate) struct Capture {
        pub entries: Vec<(usize, u64)>,
        pub status: i32,
    }

    unsafe extern "C" fn record(context: *mut c_void, identity: *const c_void, bytes: u64) -> i32 {
        // SAFETY: the test retains Capture for the complete visitor call.
        let capture = unsafe { &mut *context.cast::<Capture>() };
        capture.entries.push((identity as usize, bytes));
        capture.status
    }

    impl Capture {
        pub(crate) fn visitor(&mut self, kind: HewManagedWalkKind) -> HewManagedStorageVisitor {
            HewManagedStorageVisitor {
                context: core::ptr::from_mut(self).cast(),
                storage: record,
                kind,
            }
        }
    }

    #[test]
    fn managed_tree_refusal_and_skip_are_preserved() {
        let mut capture = Capture {
            status: HEAP_EXCEEDED,
            ..Capture::default()
        };
        let visitor = capture.visitor(HewManagedWalkKind::Retain);
        // SAFETY: the visitor borrows capture; this test tree has no children.
        let status = unsafe { tree(&raw const visitor, core::ptr::null(), false, |_| Ok(12)) };
        assert_eq!(status, HEAP_EXCEEDED);
        capture.status = SKIP_DESCENDANTS;
        let visitor = capture.visitor(HewManagedWalkKind::Release);
        // SAFETY: the visitor and context are live for this call.
        let status = unsafe { tree(&raw const visitor, core::ptr::null(), false, |_| Ok(12)) };
        assert_eq!(status, 0);
    }
}
