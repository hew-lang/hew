//! Hew runtime: double-ended queue (deque) for user programmes.
//!
//! Provides a `VecDeque<i64>` behind an opaque pointer, exposed via
//! `#[no_mangle] extern "C"` functions so compiled Hew code can create
//! and manipulate deques through the `std::deque` module.

use crate::internal::types::HEW_TRAP_INDEX_OUT_OF_BOUNDS;
use crate::trap_code::runtime_bounds_trap;
use std::collections::VecDeque;

/// Opaque handle to a `VecDeque<i64>`.
#[derive(Debug)]
pub struct HewDeque {
    inner: VecDeque<i64>,
}

/// Write a message to stderr.
///
/// # Safety
///
/// `msg` must be valid for reads for its full length.
unsafe fn write_stderr(msg: &[u8]) {
    // SAFETY: msg.as_ptr() is valid for msg.len() bytes, and fd 2 is stderr.
    unsafe {
        #[cfg(not(target_os = "windows"))]
        libc::write(2, msg.as_ptr().cast(), msg.len());
        #[cfg(target_os = "windows")]
        libc::write(2, msg.as_ptr().cast(), msg.len() as core::ffi::c_uint);
    }
}

/// Emit a deque empty-pop diagnostic and route through the trap seam.
///
/// # Safety
///
/// Call only from a fail-closed deque empty-pop path.
unsafe fn deque_bounds_trap(message: &str) -> ! {
    // SAFETY: writing the diagnostic and trapping is the terminal failure path.
    unsafe {
        write_stderr(message.as_bytes());
        runtime_bounds_trap(HEW_TRAP_INDEX_OUT_OF_BOUNDS);
    }
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

/// Remove and return the front element.
///
/// Traps via [`deque_bounds_trap`] when the deque is empty (spec
/// `pop_front() -> i64`: no `Option`, so the empty case fails closed like
/// `Vec.pop()`/`bytes.pop()`).
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
    match unsafe { &mut *dq }.inner.pop_front() {
        Some(value) => value,
        // SAFETY: this is the terminal empty-pop path.
        None => unsafe { deque_bounds_trap("PANIC: Deque.pop_front() on an empty deque\n") },
    }
}

/// Remove and return the back element.
///
/// Traps via [`deque_bounds_trap`] when the deque is empty (spec
/// `pop_back() -> i64`: no `Option`, so the empty case fails closed like
/// `Vec.pop()`/`bytes.pop()`).
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
    match unsafe { &mut *dq }.inner.pop_back() {
        Some(value) => value,
        // SAFETY: this is the terminal empty-pop path.
        None => unsafe { deque_bounds_trap("PANIC: Deque.pop_back() on an empty deque\n") },
    }
}

// ── Query ───────────────────────────────────────────────────────────────

/// Return the number of elements in the deque.
///
/// # Safety
///
/// `dq` must be a valid pointer returned by [`hew_deque_new`].
#[no_mangle]
#[expect(
    clippy::cast_possible_wrap,
    reason = "VecDeque length is bounded in practice"
)]
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

    #[cfg(not(target_arch = "wasm32"))]
    fn run_deque_death_helper(helper: &str) -> std::process::Output {
        std::process::Command::new(std::env::current_exe().unwrap())
            .args(["--exact", "--nocapture", helper])
            .env("RUST_TEST_THREADS", "1")
            .env("HEW_DEATH_TEST", helper)
            .output()
            .unwrap()
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess to observe abort(); Miri cannot posix_spawn"
    )]
    fn test_deque_pop_front_empty_traps_main_context() {
        let output = run_deque_death_helper("vecdeque::tests::_helper_deque_pop_front_empty");
        assert!(
            !output.status.success(),
            "empty Deque.pop_front() must terminate without actor context"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("PANIC: Deque.pop_front() on an empty deque"),
            "empty Deque.pop_front() must report the operation; got: {stderr}"
        );
        assert!(
            stderr.contains("hew: trap in main context: IndexOutOfBounds"),
            "empty Deque.pop_front() must route through the trap code; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn _helper_deque_pop_front_empty() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |value| {
            value != "vecdeque::tests::_helper_deque_pop_front_empty"
        }) {
            return;
        }
        // SAFETY: FFI calls use a valid deque; the pop is intentionally empty.
        unsafe {
            let dq = hew_deque_new();
            let _ = hew_deque_pop_front(dq);
        }
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    #[cfg_attr(
        miri,
        ignore = "spawns a subprocess to observe abort(); Miri cannot posix_spawn"
    )]
    fn test_deque_pop_back_empty_traps_main_context() {
        let output = run_deque_death_helper("vecdeque::tests::_helper_deque_pop_back_empty");
        assert!(
            !output.status.success(),
            "empty Deque.pop_back() must terminate without actor context"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("PANIC: Deque.pop_back() on an empty deque"),
            "empty Deque.pop_back() must report the operation; got: {stderr}"
        );
        assert!(
            stderr.contains("hew: trap in main context: IndexOutOfBounds"),
            "empty Deque.pop_back() must route through the trap code; got: {stderr}"
        );
    }

    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn _helper_deque_pop_back_empty() {
        if std::env::var("HEW_DEATH_TEST").map_or(true, |value| {
            value != "vecdeque::tests::_helper_deque_pop_back_empty"
        }) {
            return;
        }
        // SAFETY: FFI calls use a valid deque; the pop is intentionally empty.
        unsafe {
            let dq = hew_deque_new();
            let _ = hew_deque_pop_back(dq);
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
