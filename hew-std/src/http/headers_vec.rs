//! Shared `HewValueLayout` descriptor for `Vec<(string, string)>` HTTP header pairs.
//!
//! `hew_http_response_headers` and `hew_http_request_headers` both return a
//! `Vec<(string, string)>` backed by this descriptor.  The Hew drop spine calls
//! `hew_vec_free_owned` on the binding when it goes out of scope, which walks
//! the buffer and calls `string_pair_drop_thunk` on each element — freeing both
//! managed strings via `string_release`.
//!
use std::ffi::c_void;

use hew_cabi::{
    string::{string_release, string_retain, HewString},
    value::{HewTypeOwnershipKind, HewValueCloneThunk, HewValueDropThunk, HewValueLayout},
};

/// In-memory layout of one `(string, string)` element: two adjacent `*mut HewString`
/// pointers, each pointing at a managed string.
///
/// `#[repr(C)]` pins the field order so the size/align match what
/// `string_pair_elem_layout` reports and what the Hew compiler emits for
/// `(string, string)` tuples (two pointer-width fields).
#[repr(C)]
pub(crate) struct HewStringPair {
    pub name: *mut HewString,
    pub value: *mut HewString,
}

/// Clone thunk for `(string, string)` elements.
///
/// Called by `hew_vec_push_owned` / `hew_vec_set_owned` / `hew_vec_clone_owned`
/// AFTER the element has been `memcpy`'d into `dst`.  Both `dst.name` and
/// `dst.value` already alias the source strings.  This thunk retains each
/// (bumps the refcount) so that `dst` and `src` are independent co-owners.
///
/// # Safety
///
/// `src` and `dst` must each point to a valid `HewStringPair`-sized blob
/// with managed string handles, including canonical empty values.
pub(crate) unsafe extern "C" fn string_pair_clone_thunk(
    _src: *const c_void,
    dst: *mut c_void,
) -> i32 {
    // SAFETY: dst was just memcpy'd by the vec push and points to a valid pair.
    unsafe {
        let pair = &mut *dst.cast::<HewStringPair>();
        // Retain both string fields: bump refcount so src and dst own
        // independent references to the same heap buffers.
        let _ = string_retain(pair.name);
        let _ = string_retain(pair.value);
    }
    0
}

/// Drop thunk for `(string, string)` elements.
///
/// Called by `hew_vec_free_owned` on each live element, and by
/// `hew_vec_set_owned` on the overwritten element.  Releases both strings via
/// `string_release`. The vector owns the slot storage.
///
/// # Safety
///
/// `slot` must point to a live `HewStringPair`-sized blob whose string
/// pointers were allocated with `string_from_str`.
pub(crate) unsafe extern "C" fn string_pair_drop_thunk(slot: *mut c_void) {
    // SAFETY: slot is a live pair element owning both managed string fields.
    unsafe {
        let pair = &mut *slot.cast::<HewStringPair>();
        if !pair.name.is_null() {
            string_release(pair.name);
            pair.name = std::ptr::null_mut();
        }
        if !pair.value.is_null() {
            string_release(pair.value);
            pair.value = std::ptr::null_mut();
        }
    }
}

/// Build the `HewValueLayout` descriptor for `Vec<(string, string)>` header pairs.
///
/// Inlined at each call site so it lives on the caller's stack and is passed by
/// pointer to `hew_vec_new_with_elem_layout`, which copies it into the vec's
/// inline storage.  The resulting `HewVec` uses `hew_vec_push_owned` for push
/// and `hew_vec_free_owned` for free/drop.
#[inline]
pub(crate) fn string_pair_elem_layout() -> HewValueLayout {
    HewValueLayout {
        visit_close: None,
        size: std::mem::size_of::<HewStringPair>(),
        align: std::mem::align_of::<HewStringPair>(),
        ownership_kind: HewTypeOwnershipKind::LayoutManaged,
        clone_fn: Some(string_pair_clone_thunk as HewValueCloneThunk),
        drop_fn: Some(string_pair_drop_thunk as HewValueDropThunk),
    }
}

// ---------------------------------------------------------------------------
// Tests — allocator-pairing contract and thunk correctness
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use hew_cabi::{
        string::string_from_str,
        vec::{hew_vec_free_owned, hew_vec_len, hew_vec_new_with_elem_layout, hew_vec_push_owned},
    };

    /// Build a `Vec<(string,string)>` owned-pair vector with a single element,
    /// then free it.  Verifies that both strings are freed exactly once:
    /// no abort, no double-free, 0 leaks.
    ///
    /// This is the regression gate for the P0: before the fix this call chain
    /// aborted via `abort_owned_descriptor_missing` when the vec was freed.
    #[test]
    fn drop_without_consume_does_not_abort() {
        let layout = string_pair_elem_layout();
        // SAFETY: layout is valid and lives for the duration of this call.
        let vec = unsafe { hew_vec_new_with_elem_layout(&raw const layout) };
        assert!(!vec.is_null(), "allocation must succeed");

        // Allocate two managed strings.
        let name_ptr = string_from_str("content-type");
        let value_ptr = string_from_str("application/json");
        assert!(!name_ptr.is_null() && !value_ptr.is_null());

        let pair = HewStringPair {
            name: name_ptr,
            value: value_ptr,
        };
        // push_owned: memcpy + clone_thunk (retains both strings, rc→2).
        // SAFETY: vec and pair are valid; layout matches HewStringPair.
        unsafe {
            hew_vec_push_owned(vec, (&raw const pair).cast::<c_void>());
        }
        // Release the source copy (rc 2→1); the vec slot holds the sole owner.
        // SAFETY: name_ptr / value_ptr are managed strings.
        unsafe {
            string_release(pair.name);
            string_release(pair.value);
        }

        // SAFETY: vec is a valid HewVec; hew_vec_len is safe to call on any non-null vec.
        assert_eq!(unsafe { hew_vec_len(vec) }, 1);

        // Drop without a for-in consume — this is the bug path.  Must not abort.
        // SAFETY: vec was created with hew_vec_new_with_elem_layout.
        unsafe { hew_vec_free_owned(vec) };
        // If we reach here: no abort, thunk freed both strings correctly.
    }

    /// Same as above but with zero elements: an empty owned-layout vec must
    /// also free cleanly.
    #[test]
    fn empty_owned_vec_frees_cleanly() {
        let layout = string_pair_elem_layout();
        // SAFETY: layout is valid.
        let vec = unsafe { hew_vec_new_with_elem_layout(&raw const layout) };
        assert!(!vec.is_null());
        // SAFETY: vec was created with the owned constructor.
        unsafe { hew_vec_free_owned(vec) };
    }

    /// Clone-then-free: the clone has independent ownership; freeing both does
    /// not double-free any string.
    #[test]
    fn clone_then_free_both_no_double_free() {
        use hew_cabi::vec::hew_vec_clone_owned;
        let layout = string_pair_elem_layout();
        // SAFETY: layout is valid.
        let vec = unsafe { hew_vec_new_with_elem_layout(&raw const layout) };

        let name_ptr = string_from_str("x-request-id");
        let value_ptr = string_from_str("abc-123");
        let pair = HewStringPair {
            name: name_ptr,
            value: value_ptr,
        };
        // SAFETY: vec is a valid owned-layout HewVec; pair is a valid HewStringPair.
        // string_release releases the source copy after push_owned retained it.
        unsafe {
            hew_vec_push_owned(vec, (&raw const pair).cast::<c_void>());
            string_release(pair.name);
            string_release(pair.value);
        }

        // Deep-clone: each string's refcount goes to 2.
        // SAFETY: vec is a valid owned-layout vec.
        let cloned = unsafe { hew_vec_clone_owned(vec) };
        assert!(!cloned.is_null());

        // Free original (rc 2→1) then clone (rc 1→0, heap freed). No double-free.
        // SAFETY: both vecs were allocated by the owned constructor.
        unsafe {
            hew_vec_free_owned(vec);
            hew_vec_free_owned(cloned);
        }
    }
}
