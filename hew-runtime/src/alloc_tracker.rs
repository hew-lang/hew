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

// --- Set-scoped tracking primitives -------------------------------------------
//
// The `track`/`untrack`/`is_tracked` semantics are defined here against an
// explicit set so there is exactly one authority for the tracking behaviour.
// Production code drives them through the process-global `LIBC_ALLOC_SET`
// (below); the unit tests drive them against a *local, test-owned* set so that
// concurrent runtime allocations on other threads can never inject a foreign
// entry into the set a test asserts over (issue #2353). This makes the test's
// view structurally isolated under any runner (nextest process-per-test *or* a
// plain in-process `cargo test`), rather than relying on a mutex that cannot
// fence the lock-free production mutators in `lambda_actor`/`reply_channel`.

/// Register `ptr` as libc-allocated in `set`.
#[cfg(debug_assertions)]
fn track_in(set: &mut std::collections::HashSet<usize>, ptr: *mut u8) {
    set.insert(ptr as usize);
}

/// Deregister `ptr` from `set`.
#[cfg(debug_assertions)]
fn untrack_in(set: &mut std::collections::HashSet<usize>, ptr: *mut u8) {
    set.remove(&(ptr as usize));
}

/// Returns `true` if `ptr` is registered in `set`.
#[cfg(debug_assertions)]
fn is_tracked_in(set: &std::collections::HashSet<usize>, ptr: *const u8) -> bool {
    set.contains(&(ptr as usize))
}

// --- Process-global tracker (production entry points) -------------------------

/// Register a pointer as libc-allocated (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_track_libc_alloc(ptr: *mut u8) {
    track_in(
        LIBC_ALLOC_SET
            .lock()
            .unwrap()
            .get_or_insert_with(Default::default),
        ptr,
    );
}

/// Deregister a pointer from the libc-alloc tracker (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_untrack_libc_alloc(ptr: *mut u8) {
    if let Some(s) = LIBC_ALLOC_SET.lock().unwrap().as_mut() {
        untrack_in(s, ptr);
    }
}

/// Returns `true` if the pointer is registered as libc-allocated (debug builds only).
#[cfg(debug_assertions)]
pub(crate) fn debug_is_libc_tracked(ptr: *const u8) -> bool {
    LIBC_ALLOC_SET
        .lock()
        .unwrap()
        .as_ref()
        .is_some_and(|s| is_tracked_in(s, ptr))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify the basic `track` → `is_tracked` → `untrack` → `!is_tracked` lifecycle.
    ///
    /// Runs against a *local* set so a concurrent runtime allocation on another
    /// thread cannot inject a foreign entry into the set under assertion (#2353).
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_tracker_lifecycle() {
        let mut set = std::collections::HashSet::new();
        // SAFETY: libc::malloc for a small test buffer; null-checked immediately.
        let ptr = unsafe { libc::malloc(8) }.cast::<u8>();
        assert!(!ptr.is_null());
        // After tracking, it should be found.
        track_in(&mut set, ptr);
        assert!(is_tracked_in(&set, ptr));
        // After untracking, it should be gone.
        untrack_in(&mut set, ptr);
        assert!(!is_tracked_in(&set, ptr));
        // ALLOCATOR-PAIRING: libc — symmetric deallocation.
        // SAFETY: ptr was allocated above via libc::malloc.
        unsafe { libc::free(ptr.cast()) };
    }

    /// A freshly Box-allocated pointer must never appear as libc-tracked.
    ///
    /// Uses a *local* set so the negative assertion cannot be flipped by an
    /// unrelated concurrent libc allocation that happens to reuse this address
    /// in the process-global set (#2353).
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_globalalloc_ptr_not_libc_tracked() {
        let set: std::collections::HashSet<usize> = std::collections::HashSet::new();
        let b: Box<u8> = Box::new(0);
        // ALLOCATOR-PAIRING: GlobalAlloc — Box::into_raw for testing only.
        let ptr = Box::into_raw(b);
        assert!(!is_tracked_in(&set, ptr));
        // Restore ownership to avoid leak.
        // ALLOCATOR-PAIRING: GlobalAlloc — matching Box::from_raw.
        // SAFETY: ptr was produced by Box::into_raw above; ownership returned here.
        unsafe { drop(Box::from_raw(ptr)) };
    }

    /// Two independently tracked pointers are independent in the set.
    ///
    /// Runs against a *local* set for the same isolation reason as the other
    /// tracker tests (#2353).
    #[test]
    #[cfg(debug_assertions)]
    fn allocator_pairing_tracker_two_ptrs_independent() {
        let mut set = std::collections::HashSet::new();
        // SAFETY: libc::malloc for small test buffers; null-checked immediately.
        let p1 = unsafe { libc::malloc(4) }.cast::<u8>();
        // SAFETY: same as p1.
        let p2 = unsafe { libc::malloc(4) }.cast::<u8>();
        assert!(!p1.is_null());
        assert!(!p2.is_null());
        track_in(&mut set, p1);
        track_in(&mut set, p2);
        assert!(is_tracked_in(&set, p1));
        assert!(is_tracked_in(&set, p2));
        // Untracking p1 must not affect p2.
        untrack_in(&mut set, p1);
        assert!(!is_tracked_in(&set, p1));
        assert!(is_tracked_in(&set, p2));
        untrack_in(&mut set, p2);
        // ALLOCATOR-PAIRING: libc — symmetric deallocation.
        // SAFETY: p1 and p2 were allocated above via libc::malloc.
        unsafe {
            libc::free(p1.cast());
            libc::free(p2.cast());
        }
    }
}
