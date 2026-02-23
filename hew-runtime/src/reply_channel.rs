//! Hew runtime: reply channel for the actor ask pattern.
//!
//! A reply channel provides a one-shot synchronisation primitive: the
//! sender calls [`hew_reply`] to deposit a value, and the receiver
//! blocks in [`hew_reply_wait`] (or [`hew_reply_wait_timeout`]) until
//! the value is ready.

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// ── Reply channel ───────────────────────────────────────────────────────

/// One-shot reply channel for the actor ask pattern.
///
/// Thread-safety contract: exactly one thread calls [`hew_reply`],
/// exactly one thread calls [`hew_reply_wait`] (or the timeout variant).
#[repr(C)]
pub struct HewReplyChannel {
    /// Set to `true` once the reply value has been deposited.
    ready: AtomicBool,
    /// Set to `true` if the waiter has cancelled.
    pub(crate) cancelled: AtomicBool,
    /// Reply payload (malloc'd by [`hew_reply`], owned by the waiter).
    value: *mut c_void,
    /// Size of `value` in bytes.
    value_size: usize,
    /// Mutex protecting the condvar wait.
    lock: Mutex<()>,
    /// Condvar signalled by [`hew_reply`].
    cond: Condvar,
}

// SAFETY: `HewReplyChannel` is designed for cross-thread use. The atomic
// `ready` flag provides the release/acquire barrier between writer and
// reader. Raw pointers are only accessed after the barrier.
unsafe impl Send for HewReplyChannel {}
// SAFETY: Concurrent access is synchronised through atomics + condvar.
unsafe impl Sync for HewReplyChannel {}

impl std::fmt::Debug for HewReplyChannel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewReplyChannel")
            .field("ready", &self.ready)
            .field("cancelled", &self.cancelled)
            .finish_non_exhaustive()
    }
}

// ── Constructors ────────────────────────────────────────────────────────

/// Create a new reply channel.
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_reply_channel_free`].
#[no_mangle]
pub extern "C" fn hew_reply_channel_new() -> *mut HewReplyChannel {
    Box::into_raw(Box::new(HewReplyChannel {
        ready: AtomicBool::new(false),
        cancelled: AtomicBool::new(false),
        value: ptr::null_mut(),
        value_size: 0,
        lock: Mutex::new(()),
        cond: Condvar::new(),
    }))
}

// ── Reply (sender side) ─────────────────────────────────────────────────

/// Deposit a reply value and wake the waiter.
///
/// The payload is deep-copied so the caller retains ownership of `value`.
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - `value` must point to at least `size` readable bytes (or be null
///   when `size` is 0).
/// - Must be called at most once per channel.
#[no_mangle]
pub unsafe extern "C" fn hew_reply(ch: *mut HewReplyChannel, value: *mut c_void, size: usize) {
    if ch.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `ch` is valid and single-writer.
    unsafe {
        // If the waiter already timed out and cancelled, clean up and bail.
        if (*ch).cancelled.load(Ordering::Acquire) {
            if size > 0 && !value.is_null() {
                // Value was never deposited; nothing to free.
            }
            // The channel is orphaned — free it now.
            hew_reply_channel_free(ch);
            return;
        }

        if size > 0 && !value.is_null() {
            // SAFETY: malloc for deep copy of reply payload.
            let buf = libc::malloc(size);
            if !buf.is_null() {
                ptr::copy_nonoverlapping(value.cast::<u8>(), buf.cast::<u8>(), size);
                (*ch).value = buf;
                (*ch).value_size = size;
            }
        }

        // Release barrier ensures value writes are visible to the waiter.
        (*ch).ready.store(true, Ordering::Release);

        // Wake the condvar waiter.
        let _guard = match (*ch).lock.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        (*ch).cond.notify_one();
    }
}

// ── Wait (receiver side) ────────────────────────────────────────────────

/// Block until a reply is available, then return the value.
///
/// The caller owns the returned pointer and must free it with
/// [`libc::free`].
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - Must be called at most once per channel.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_wait(ch: *mut HewReplyChannel) -> *mut c_void {
    if ch.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees `ch` is valid and single-reader.
    unsafe {
        // Fast path: check atomic flag without locking.
        if (*ch).ready.load(Ordering::Acquire) {
            let result = (*ch).value;
            (*ch).value = ptr::null_mut();
            return result;
        }

        // Slow path: wait on condvar.
        let mut guard = match (*ch).lock.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        while !(*ch).ready.load(Ordering::Acquire) {
            guard = match (*ch).cond.wait(guard) {
                Ok(g) => g,
                Err(e) => e.into_inner(),
            };
        }
        let result = (*ch).value;
        (*ch).value = ptr::null_mut();
        drop(guard);
        result
    }
}

/// Block until a reply is available or the timeout expires.
///
/// Returns the reply value on success, or null on timeout.
///
/// # Safety
///
/// Same requirements as [`hew_reply_wait`].
#[no_mangle]
pub unsafe extern "C" fn hew_reply_wait_timeout(
    ch: *mut HewReplyChannel,
    timeout_ms: i32,
) -> *mut c_void {
    if ch.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees `ch` is valid and single-reader.
    unsafe {
        // Fast path.
        if (*ch).ready.load(Ordering::Acquire) {
            let result = (*ch).value;
            (*ch).value = ptr::null_mut();
            return result;
        }

        let deadline =
            Instant::now() + Duration::from_millis(u64::try_from(timeout_ms.max(0)).unwrap_or(0));
        let mut guard = match (*ch).lock.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };

        while !(*ch).ready.load(Ordering::Acquire) {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return ptr::null_mut();
            }
            let (new_guard, wait_result) = match (*ch).cond.wait_timeout(guard, remaining) {
                Ok(result) => result,
                Err(e) => e.into_inner(),
            };
            guard = new_guard;
            if wait_result.timed_out() && !(*ch).ready.load(Ordering::Acquire) {
                return ptr::null_mut();
            }
        }

        let result = (*ch).value;
        (*ch).value = ptr::null_mut();
        drop(guard);
        result
    }
}

// ── Cleanup ─────────────────────────────────────────────────────────────

/// Free a reply channel and any uncollected reply value.
///
/// # Safety
///
/// `ch` must have been returned by [`hew_reply_channel_new`] and must
/// not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_free(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `ch` was Box-allocated and is exclusively owned.
    unsafe {
        if !(*ch).value.is_null() {
            libc::free((*ch).value);
        }
        drop(Box::from_raw(ch));
    }
}

// ── Select ─────────────────────────────────────────────────────────────

/// Poll multiple reply channels, returning the index of the first one
/// that becomes ready.  Returns -1 on timeout.
///
/// # Safety
///
/// `channels` must point to a valid array of `count` valid
/// `*mut HewReplyChannel` pointers.  `timeout_ms` of -1 means
/// wait indefinitely.
#[no_mangle]
#[expect(
    clippy::cast_sign_loss,
    reason = "C ABI: timeout_ms validated non-negative by caller"
)]
#[expect(clippy::cast_possible_wrap, reason = "C ABI: reply count fits in i32")]
pub unsafe extern "C" fn hew_select_first(
    channels: *mut *mut HewReplyChannel,
    count: i32,
    timeout_ms: i32,
) -> i32 {
    if channels.is_null() || count <= 0 {
        return -1;
    }
    #[expect(clippy::cast_sign_loss, reason = "count validated > 0")]
    let n = count as usize;
    let deadline = if timeout_ms >= 0 {
        Some(Instant::now() + Duration::from_millis(timeout_ms as u64))
    } else {
        None
    };

    loop {
        for i in 0..n {
            // SAFETY: caller guarantees array and elements are valid.
            let ch = unsafe { *channels.add(i) };
            if ch.is_null() {
                continue;
            }
            // SAFETY: ch is valid per caller contract.
            if unsafe { (*ch).ready.load(Ordering::Acquire) } {
                // Cancel all other channels.
                for j in 0..n {
                    if j != i {
                        // SAFETY: same array validity guarantee.
                        let other = unsafe { *channels.add(j) };
                        if !other.is_null() {
                            // SAFETY: other is valid per contract.
                            unsafe { (*other).cancelled.store(true, Ordering::Release) };
                        }
                    }
                }
                #[expect(clippy::cast_possible_truncation, reason = "index fits in i32")]
                return i as i32;
            }
        }
        if let Some(dl) = deadline {
            if Instant::now() >= dl {
                return -1;
            }
        }
        std::thread::sleep(Duration::from_millis(1));
    }
}
