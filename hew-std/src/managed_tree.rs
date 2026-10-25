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

/// Reserve an indivisible tree replacement, including a moved child root.
/// No input ownership changes until the returned permit is committed.
pub(crate) fn replacement(
    owner: Option<&hew_runtime::owned_value_budget::ValueOwner>,
    retired: &[(hew_runtime::owned_value_budget::ManagedStorage, bool)],
    bytes: Option<u64>,
) -> Result<Option<hew_runtime::owned_value_budget::BackingChange>, i32> {
    let Some(owner) = owner else {
        return Ok(None);
    };
    owner
        .prepare_backing_replacement(retired, bytes.ok_or(HEAP_EXCEEDED)?)
        .map(Some)
        .map_err(|_| HEAP_EXCEEDED)
}

/// Allocate replacement capacity before changing the live array. Elements move
/// only after quota admission, and a refused growth leaves the original intact.
pub(crate) fn array_growth<T>(values: &Vec<T>) -> Result<Option<Vec<T>>, i32> {
    if values.len() < values.capacity() {
        return Ok(None);
    }
    let capacity = values
        .capacity()
        .checked_mul(2)
        .ok_or(HEAP_EXCEEDED)?
        .max(4);
    let mut replacement = Vec::new();
    replacement
        .try_reserve_exact(capacity)
        .map_err(|_| HEAP_EXCEEDED)?;
    Ok(Some(replacement))
}

pub(crate) fn push<T>(values: &mut Vec<T>, growth: Option<Vec<T>>, child: T) {
    if let Some(mut replacement) = growth {
        replacement.append(values);
        *values = replacement;
    }
    values.push(child);
}

/// Adopt a private constructor result before exposing its owned Hew value.
/// Parsing and serialization can discover their exact capacities only after
/// building the private tree or text; the compiler's result descriptor remains
/// the authority for acquisition. Refusal disposes only that unpublished result.
///
/// # Safety
/// `value` has `layout`'s representation, `status` is writable, and `dispose`
/// releases the physical result without releasing an unacquired account edge.
pub(crate) unsafe fn publish<T: Copy>(
    value: T,
    empty: T,
    layout: *const hew_cabi::value::HewValueLayout,
    status: *mut i32,
    dispose: impl FnOnce(T),
) -> T {
    // SAFETY: the checked ABI supplies its writable status slot.
    unsafe { status.write(0) };
    let Some(owner) = hew_runtime::owned_value_budget::current_owner() else {
        return value;
    };
    // SAFETY: a bounded invocation supplies its live result descriptor.
    let acquired = match unsafe { layout.as_ref() } {
        Some(layout) => {
            // SAFETY: the caller retains the unpublished concrete result slot.
            unsafe { owner.acquire_value(layout, core::ptr::from_ref(&value).cast()) }
        }
        None => Err(UNDESCRIBED),
    };
    match acquired {
        Ok(acquired) => {
            acquired.commit();
            value
        }
        Err(failure) => {
            hew_runtime::owned_value_budget::with_value_owner(&None, || dispose(value));
            // SAFETY: the status slot remains valid until this call returns.
            unsafe { status.write(failure) };
            empty
        }
    }
}

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

    pub(crate) fn layout(
        managed: &hew_cabi::managed::HewManagedValueDescriptor,
    ) -> hew_cabi::value::HewValueLayout {
        hew_cabi::value::HewValueLayout {
            size: core::mem::size_of::<usize>(),
            align: core::mem::align_of::<usize>(),
            ownership_kind: hew_cabi::value::HewTypeOwnershipKind::LayoutManaged,
            clone_fn: None,
            drop_fn: None,
            release_start: None,
            code_owner: core::ptr::null(),
            managed,
        }
    }

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
