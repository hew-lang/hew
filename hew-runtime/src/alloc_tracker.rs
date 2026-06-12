// live on not(wasm32) — lambda_actor (native-only); dead on wasm32 (debug_assertions still gate items)
#![cfg_attr(target_arch = "wasm32", allow(dead_code))]
//! Runtime-wide allocator-pairing tracker (debug builds only).
//!
//! Maintains a set of pointers allocated via `libc::malloc` so that:
//!
//! * `libc::free` call-sites can assert the pointer **is** libc-tracked
//!   (i.e. was actually libc-allocated inside this runtime).
//! * `Box::from_raw` call-sites can assert the pointer is **not** libc-tracked
//!   (i.e. was allocated via `Box` / Rust's `GlobalAlloc`, not libc).
//!
//! Gated entirely on `#[cfg(debug_assertions)]`; zero overhead in release builds.
//!
//! # Usage
//!
//! ```ignore
//! // After a successful libc::malloc:
//! #[cfg(debug_assertions)]
//! if !ptr.is_null() {
//!     crate::alloc_tracker::debug_track_libc_alloc(ptr.cast());
//! }
//!
//! // Before a libc::free of a runtime-internal allocation:
//! #[cfg(debug_assertions)]
//! {
//!     debug_assert!(
//!         crate::alloc_tracker::debug_is_libc_tracked(ptr.cast()),
//!         "allocator-pairing: ptr is not libc-tracked",
//!     );
//!     crate::alloc_tracker::debug_untrack_libc_alloc(ptr.cast());
//! }
//!
//! // Before Box::from_raw on a pointer that was Box-allocated:
//! #[cfg(debug_assertions)]
//! debug_assert!(
//!     !crate::alloc_tracker::debug_is_libc_tracked(ptr.cast()),
//!     "allocator-pairing: ptr is libc-tracked; use libc::free, not Box::from_raw",
//! );
//! ```

#[cfg(debug_assertions)]
static LIBC_ALLOC_SET: std::sync::Mutex<Option<std::collections::HashSet<usize>>> =
    std::sync::Mutex::new(None);

/// Register a pointer as libc-allocated (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_track_libc_alloc(ptr: *mut u8) {
    LIBC_ALLOC_SET
        .lock()
        .unwrap()
        .get_or_insert_with(Default::default)
        .insert(ptr as usize);
}

/// Deregister a pointer from the libc-alloc tracker (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_untrack_libc_alloc(ptr: *mut u8) {
    if let Some(s) = LIBC_ALLOC_SET.lock().unwrap().as_mut() {
        s.remove(&(ptr as usize));
    }
}

/// Returns `true` if the pointer is registered as libc-allocated (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_is_libc_tracked(ptr: *const u8) -> bool {
    LIBC_ALLOC_SET
        .lock()
        .unwrap()
        .as_ref()
        .is_some_and(|s| s.contains(&(ptr as usize)))
}

/// Serialises every test that touches `LIBC_ALLOC_SET` so that parallel test
/// execution does not cause pointer entries from one test to appear in
/// another's assertions.  Mirrors the `TICKER_TEST_MUTEX` pattern in
/// `timer_periodic`.
#[cfg(test)]
pub(crate) static ALLOC_TRACKER_TEST_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify the basic `track` → `is_tracked` → `untrack` → `!is_tracked` lifecycle.
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_tracker_lifecycle() {
        let _guard = ALLOC_TRACKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: libc::malloc for a small test buffer; null-checked immediately.
        let ptr = unsafe { libc::malloc(8) }.cast::<u8>();
        assert!(!ptr.is_null());
        // After tracking, it should be found.
        debug_track_libc_alloc(ptr);
        assert!(debug_is_libc_tracked(ptr));
        // After untracking, it should be gone.
        debug_untrack_libc_alloc(ptr);
        assert!(!debug_is_libc_tracked(ptr));
        // ALLOCATOR-PAIRING: libc — symmetric deallocation.
        // SAFETY: ptr was allocated above via libc::malloc.
        unsafe { libc::free(ptr.cast()) };
    }

    /// A freshly Box-allocated pointer must never appear as libc-tracked.
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_globalalloc_ptr_not_libc_tracked() {
        let _guard = ALLOC_TRACKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let b: Box<u8> = Box::new(0);
        // ALLOCATOR-PAIRING: GlobalAlloc — Box::into_raw for testing only.
        let ptr = Box::into_raw(b);
        assert!(!debug_is_libc_tracked(ptr));
        // Restore ownership to avoid leak.
        // ALLOCATOR-PAIRING: GlobalAlloc — matching Box::from_raw.
        // SAFETY: ptr was produced by Box::into_raw above; ownership returned here.
        unsafe { drop(Box::from_raw(ptr)) };
    }

    /// Two independently tracked pointers are independent in the set.
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_tracker_two_ptrs_independent() {
        let _guard = ALLOC_TRACKER_TEST_MUTEX
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        // SAFETY: libc::malloc for small test buffers; null-checked immediately.
        let p1 = unsafe { libc::malloc(4) }.cast::<u8>();
        // SAFETY: same as p1.
        let p2 = unsafe { libc::malloc(4) }.cast::<u8>();
        assert!(!p1.is_null());
        assert!(!p2.is_null());
        debug_track_libc_alloc(p1);
        debug_track_libc_alloc(p2);
        assert!(debug_is_libc_tracked(p1));
        assert!(debug_is_libc_tracked(p2));
        // Untracking p1 must not affect p2.
        debug_untrack_libc_alloc(p1);
        assert!(!debug_is_libc_tracked(p1));
        assert!(debug_is_libc_tracked(p2));
        debug_untrack_libc_alloc(p2);
        // ALLOCATOR-PAIRING: libc — symmetric deallocation.
        // SAFETY: p1 and p2 were allocated above via libc::malloc.
        unsafe {
            libc::free(p1.cast());
            libc::free(p2.cast());
        }
    }
}
