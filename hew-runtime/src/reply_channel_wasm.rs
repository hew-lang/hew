//! WASM reply channel for the actor ask pattern (single-threaded).
//!
//! This is the WASM counterpart of [`crate::reply_channel`]. Since WASM
//! is single-threaded, there is no condvar, no mutex, and no atomic
//! ready flag. The reply is deposited synchronously during cooperative
//! dispatch and read immediately after.

use std::ffi::c_void;
use std::ptr;
#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};

#[cfg(test)]
static ACTIVE_CHANNELS: AtomicUsize = AtomicUsize::new(0);

/// One-shot reply channel for the WASM ask pattern.
///
/// On WASM, the ask pattern is cooperative: the caller sends a message,
/// runs the scheduler until the dispatch function calls [`hew_reply`],
/// then reads the reply synchronously.
#[derive(Debug)]
#[repr(C)]
pub struct WasmReplyChannel {
    /// Manual reference count shared by the waiting side and the in-flight reply.
    refs: usize,
    /// Reply payload (malloc'd by [`hew_reply`], owned by the waiter).
    value: *mut c_void,
    /// Size of `value` in bytes.
    value_size: usize,
    /// Whether a reply has been deposited.
    replied: bool,
    /// Whether the waiter abandoned the channel.
    cancelled: bool,
}

/// Create a new WASM reply channel.
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_reply_channel_free`].
#[cfg_attr(target_arch = "wasm32", no_mangle)]
#[must_use]
pub extern "C" fn hew_reply_channel_new() -> *mut WasmReplyChannel {
    #[cfg(test)]
    ACTIVE_CHANNELS.fetch_add(1, Ordering::Relaxed);
    Box::into_raw(Box::new(WasmReplyChannel {
        refs: 1,
        value: ptr::null_mut(),
        value_size: 0,
        replied: false,
        cancelled: false,
    }))
}

/// Retain an additional reference to a WASM reply channel.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_retain(ch: *mut WasmReplyChannel) {
    cabi_guard!(ch.is_null());

    // SAFETY: Caller guarantees `ch` is valid while retaining a new reference.
    unsafe {
        (*ch).refs += 1;
    }
}

/// Deposit a reply value (WASM version).
///
/// The payload is deep-copied so the caller retains ownership of `value`.
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - `value` must point to at least `size` readable bytes (or be null
///   when `size` is 0).
/// - Must be called at most once per channel.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply(ch: *mut WasmReplyChannel, value: *mut c_void, size: usize) {
    cabi_guard!(ch.is_null());

    // SAFETY: Caller guarantees `ch` is valid and single-writer.
    unsafe {
        debug_assert!(
            !(*ch).replied,
            "WASM reply channels must not be replied to more than once"
        );
        if (*ch).cancelled {
            hew_reply_channel_free(ch);
            return;
        }
        if size > 0 && !value.is_null() {
            let buf = libc::malloc(size);
            if !buf.is_null() {
                ptr::copy_nonoverlapping(value.cast::<u8>(), buf.cast::<u8>(), size);
                (*ch).value = buf;
                (*ch).value_size = size;
            }
        }
        (*ch).replied = true;
        hew_reply_channel_free(ch);
    }
}

/// Whether a reply has already been deposited on the channel.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
pub(crate) unsafe fn reply_ready(ch: *mut WasmReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `ch` is valid.
    unsafe { (*ch).replied }
}

/// Read the reply value from the channel.
///
/// Returns the reply pointer (caller must free with [`libc::free`]),
/// or null if no reply was deposited.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
pub(crate) unsafe fn reply_take(ch: *mut WasmReplyChannel) -> *mut c_void {
    if ch.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: Caller guarantees `ch` is valid.
    unsafe {
        let result = (*ch).value;
        (*ch).value = ptr::null_mut();
        result
    }
}

/// Release a WASM reply channel reference and free it when the count reaches zero.
///
/// # Safety
///
/// `ch` must have been returned by [`hew_reply_channel_new`] and must
/// not be used after the final release.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_free(ch: *mut WasmReplyChannel) {
    cabi_guard!(ch.is_null());

    // SAFETY: Caller guarantees `ch` is a live reply channel reference.
    unsafe {
        debug_assert!((*ch).refs > 0, "reply channel release on released channel");
        (*ch).refs -= 1;
        if (*ch).refs != 0 {
            return;
        }
        if !(*ch).value.is_null() {
            libc::free((*ch).value);
        }
        #[cfg(test)]
        ACTIVE_CHANNELS.fetch_sub(1, Ordering::Relaxed);
        drop(Box::from_raw(ch));
    }
}

/// Mark a WASM reply channel as abandoned.
///
/// # Safety
///
/// `ch` must have been returned by [`hew_reply_channel_new`] and must
/// remain valid until its remaining references are released.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_cancel(ch: *mut WasmReplyChannel) {
    cabi_guard!(ch.is_null());

    // SAFETY: Caller guarantees `ch` is valid; WASM reply channels are single-threaded.
    unsafe {
        (*ch).cancelled = true;
    }
}

#[cfg(test)]
pub(crate) fn active_channel_count() -> usize {
    ACTIVE_CHANNELS.load(Ordering::Relaxed)
}

#[cfg(test)]
pub(crate) unsafe fn test_ref_count(ch: *mut WasmReplyChannel) -> usize {
    if ch.is_null() {
        return 0;
    }
    // SAFETY: Test callers only pass live reply channels they own.
    unsafe { (*ch).refs }
}

#[cfg(test)]
pub(crate) unsafe fn test_replied(ch: *mut WasmReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }
    // SAFETY: Test callers only pass live reply channels they own.
    unsafe { (*ch).replied }
}

#[cfg(test)]
pub(crate) unsafe fn test_cancelled(ch: *mut WasmReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }
    // SAFETY: Test callers only pass live reply channels they own.
    unsafe { (*ch).cancelled }
}
