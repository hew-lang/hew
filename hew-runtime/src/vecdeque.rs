//! Hew runtime: double-ended queue (deque) for user programmes.
//!
//! Provides a `VecDeque<i64>` behind an opaque pointer, exposed via
//! `#[no_mangle] extern "C"` functions so compiled Hew code can create
//! and manipulate deques through the `std::deque` module.

use std::collections::VecDeque;

/// Opaque handle to a `VecDeque<i64>`.
pub struct HewDeque {
    inner: VecDeque<i64>,
}

// ── Constructor ─────────────────────────────────────────────────────────

/// Create a new, empty deque.
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_deque_free`].
#[no_mangle]
pub extern "C" fn hew_deque_new() -> *mut HewDeque {
    Box::into_raw(Box::new(HewDeque {
        inner: VecDeque::new(),
    }))
}

// ── Push ────────────────────────────────────────────────────────────────

/// Push a value onto the front of the deque.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_push_front(dq: *mut HewDeque, value: i64) {
    if dq.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &mut *dq }.inner.push_front(value);
}

/// Push a value onto the back of the deque.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_push_back(dq: *mut HewDeque, value: i64) {
    if dq.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &mut *dq }.inner.push_back(value);
}

// ── Pop ─────────────────────────────────────────────────────────────────

/// Remove and return the front element.  Returns 0 if the deque is empty.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_pop_front(dq: *mut HewDeque) -> i64 {
    if dq.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &mut *dq }.inner.pop_front().unwrap_or(0)
}

/// Remove and return the back element.  Returns 0 if the deque is empty.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_pop_back(dq: *mut HewDeque) -> i64 {
    if dq.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &mut *dq }.inner.pop_back().unwrap_or(0)
}

// ── Query ───────────────────────────────────────────────────────────────

/// Return the number of elements in the deque.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_len(dq: *const HewDeque) -> i64 {
    if dq.is_null() {
        return 0;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &*dq }.inner.len() as i64
}

/// Return `true` if the deque is empty.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_deque_is_empty(dq: *const HewDeque) -> bool {
    if dq.is_null() {
        return true;
    }
    // SAFETY: Caller guarantees `dq` is valid.
    unsafe { &*dq }.inner.is_empty()
}

// ── Cleanup ─────────────────────────────────────────────────────────────

/// Free a deque.
///
/// # Safety
///
/// `dq` must have been returned by [`hew_deque_new`] and must not be
/// used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_deque_free(dq: *mut HewDeque) {
    if dq.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `dq` was Box-allocated and is exclusively owned.
    unsafe {
        drop(Box::from_raw(dq));
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_and_free() {
        let dq = hew_deque_new();
        assert!(!dq.is_null());
        // SAFETY: `dq` was just created above.
        unsafe {
            assert!(hew_deque_is_empty(dq));
            assert_eq!(hew_deque_len(dq), 0);
            hew_deque_free(dq);
        }
    }

    #[test]
    fn push_back_pop_front_fifo() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            hew_deque_push_back(dq, 10);
            hew_deque_push_back(dq, 20);
            hew_deque_push_back(dq, 30);
            assert_eq!(hew_deque_len(dq), 3);
            assert_eq!(hew_deque_pop_front(dq), 10);
            assert_eq!(hew_deque_pop_front(dq), 20);
            assert_eq!(hew_deque_pop_front(dq), 30);
            assert!(hew_deque_is_empty(dq));
            hew_deque_free(dq);
        }
    }

    #[test]
    fn push_front_pop_back_fifo() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            hew_deque_push_front(dq, 1);
            hew_deque_push_front(dq, 2);
            hew_deque_push_front(dq, 3);
            assert_eq!(hew_deque_pop_back(dq), 1);
            assert_eq!(hew_deque_pop_back(dq), 2);
            assert_eq!(hew_deque_pop_back(dq), 3);
            hew_deque_free(dq);
        }
    }

    #[test]
    fn push_front_pop_front_lifo() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            hew_deque_push_front(dq, 100);
            hew_deque_push_front(dq, 200);
            assert_eq!(hew_deque_pop_front(dq), 200);
            assert_eq!(hew_deque_pop_front(dq), 100);
            hew_deque_free(dq);
        }
    }

    #[test]
    fn pop_empty_returns_zero() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            assert_eq!(hew_deque_pop_front(dq), 0);
            assert_eq!(hew_deque_pop_back(dq), 0);
            hew_deque_free(dq);
        }
    }

    #[test]
    fn mixed_operations() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            hew_deque_push_back(dq, 1);
            hew_deque_push_front(dq, 2);
            hew_deque_push_back(dq, 3);
            // deque: [2, 1, 3]
            assert_eq!(hew_deque_len(dq), 3);
            assert_eq!(hew_deque_pop_front(dq), 2);
            assert_eq!(hew_deque_pop_back(dq), 3);
            assert_eq!(hew_deque_pop_front(dq), 1);
            let empty = hew_deque_is_empty(dq);
            assert!(empty);
            hew_deque_free(dq);
        }
    }

    #[test]
    fn null_pointer_safety() {
        // SAFETY: testing null-safety of all deque functions.
        unsafe {
            hew_deque_push_front(std::ptr::null_mut(), 1);
            hew_deque_push_back(std::ptr::null_mut(), 1);
            assert_eq!(hew_deque_pop_front(std::ptr::null_mut()), 0);
            assert_eq!(hew_deque_pop_back(std::ptr::null_mut()), 0);
            assert_eq!(hew_deque_len(std::ptr::null()), 0);
            assert!(hew_deque_is_empty(std::ptr::null()));
            hew_deque_free(std::ptr::null_mut());
        }
    }

    #[test]
    fn len_tracks_operations() {
        let dq = hew_deque_new();
        // SAFETY: `dq` was just created above.
        unsafe {
            assert_eq!(hew_deque_len(dq), 0);
            hew_deque_push_back(dq, 10);
            assert_eq!(hew_deque_len(dq), 1);
            hew_deque_push_front(dq, 20);
            assert_eq!(hew_deque_len(dq), 2);
            hew_deque_pop_front(dq);
            assert_eq!(hew_deque_len(dq), 1);
            hew_deque_pop_back(dq);
            assert_eq!(hew_deque_len(dq), 0);
            hew_deque_free(dq);
        }
    }
}
