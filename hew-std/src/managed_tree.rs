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

/// Canonical width of a retained string or collection handle.
pub const HANDLE_BYTES: u64 = 8;
/// Four-byte discriminant plus an eight-byte scalar or backing handle.
pub const JSON_NODE_BYTES: u64 = 12;
/// Four-byte discriminant plus an eight-byte scalar or backing handle.
pub const YAML_NODE_BYTES: u64 = 12;
/// Date presence, time-field presence and offset kind (three bytes), year
/// (u16), month/day/hour/minute/second (five u8s), nanoseconds (u32), and
/// offset minutes (i16). Missing fields retain their canonical slot.
pub const TOML_DATETIME_BYTES: u64 = 16;
/// Four-byte discriminant plus the largest typed payload: a datetime.
pub const TOML_NODE_BYTES: u64 = 4 + TOML_DATETIME_BYTES;

/// Provider facts projected into sandbox managed-type rows.
#[derive(Debug, Clone, Copy)]
pub struct ManagedTreeLayout {
    /// Width of one inline format value, including its discriminant.
    pub node_bytes: u64,
    /// Width of a canonical backing handle.
    pub handle_bytes: u64,
    /// Inline width of one map key (a string handle or a YAML value).
    pub map_key_bytes: u64,
    /// Whether reserved map entry capacity is exposed by this provider.
    pub map_capacity: bool,
    /// Inline payload of a boxed tagged node, excluding tag UTF-8.
    pub tagged_bytes: u64,
    /// Whether semantic copies retain the same immutable whole tree.
    pub shared_copy: bool,
}

/// JSON trees own their nodes and B-tree entries independently.
pub const JSON_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: JSON_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: HANDLE_BYTES,
    map_capacity: false,
    tagged_bytes: 0,
    shared_copy: false,
};

/// YAML trees support arbitrary typed keys, retained map capacity and tags.
pub const YAML_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: YAML_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: YAML_NODE_BYTES,
    map_capacity: true,
    tagged_bytes: HANDLE_BYTES + YAML_NODE_BYTES,
    shared_copy: true,
};

/// TOML trees include typed datetime slots and string-keyed B-tree entries.
pub const TOML_LAYOUT: ManagedTreeLayout = ManagedTreeLayout {
    node_bytes: TOML_NODE_BYTES,
    handle_bytes: HANDLE_BYTES,
    map_key_bytes: HANDLE_BYTES,
    map_capacity: false,
    tagged_bytes: 0,
    shared_copy: false,
};

pub(crate) type Payload = Result<u64, i32>;

pub(crate) fn add(left: u64, right: u64) -> Payload {
    left.checked_add(right).ok_or(HEAP_EXCEEDED)
}

pub(crate) fn slots(count: usize, width: u64) -> Payload {
    u64::try_from(count)
        .ok()
        .and_then(|count| count.checked_mul(width))
        .ok_or(HEAP_EXCEEDED)
}

pub(crate) fn text(value: &String, copy: bool) -> Payload {
    slots(if copy { value.len() } else { value.capacity() }, 1)
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
    fn managed_tree_overflow_and_refusal_are_preserved() {
        assert_eq!(add(u64::MAX, 1), Err(HEAP_EXCEEDED));
        assert_eq!(slots(2, u64::MAX), Err(HEAP_EXCEEDED));
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
