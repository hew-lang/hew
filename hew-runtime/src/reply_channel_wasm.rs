//! WASM reply channel for the actor ask pattern (single-threaded).
//!
//! This is the WASM counterpart of [`crate::reply_channel`]. Since WASM
//! is single-threaded, there is no condvar, no mutex, and no atomic
//! ready flag. The reply is deposited synchronously during cooperative
//! dispatch and read immediately after.
//!
//! # Select parity
//!
//! [`hew_select_first`] and [`hew_reply_wait`] complete the select surface
//! for WASM.  The implementation drives the cooperative scheduler in a
//! tight loop (one activation at a time via [`hew_wasm_sched_tick`]) until
//! the first channel becomes ready.  This mirrors the native busy-poll
//! approach but uses cooperative yields instead of `thread::sleep`.
//!
//! Finite-timeout select (`timeout_ms >= 0`) uses the same monotonic
//! deadline-loop pattern as single-arm timed ask on `wasm32-wasip1`.

use std::ffi::c_void;
use std::ptr;
#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

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
    /// Set by [`retire_reply_channel`] (in `mailbox_wasm`) so the ask caller
    /// can distinguish mailbox-teardown null from a legitimate null reply.
    pub(crate) orphaned: bool,
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
        orphaned: false,
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
        crate::scheduler_wasm::mark_current_reply_channel_consumed(ch.cast());
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

/// Return `true` if the channel was retired by mailbox teardown rather than
/// by the handler explicitly calling [`hew_reply`].
///
/// Used by the WASM ask loop to distinguish a null reply deposited by
/// `retire_reply_channel` (mailbox teardown) from a legitimate null reply
/// deposited by the handler itself.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
pub(crate) unsafe fn reply_is_orphaned(ch: *mut WasmReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }
    // SAFETY: Caller guarantees `ch` is valid.
    unsafe { (*ch).orphaned }
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

// ── Select parity ───────────────────────────────────────────────────────

/// Block until a reply has been deposited on `ch`, then return the value.
///
/// On WASM, replies are deposited synchronously during cooperative dispatch.
/// If the channel is not yet ready this function drives the scheduler one
/// activation at a time until the reply arrives or the run queue drains.
///
/// The caller owns the returned pointer and must free it with
/// [`libc::free`].
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - Must be called at most once per channel.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_wait(ch: *mut WasmReplyChannel) -> *mut c_void {
    if ch.is_null() {
        return ptr::null_mut();
    }
    // Drive the cooperative scheduler until the reply lands or all work
    // is done (which would indicate a bug in the caller's program, but we
    // must not spin forever).
    //
    // SAFETY: Single-threaded on WASM; hew_wasm_sched_tick is re-entrant-safe
    // for the WASM cooperative scheduler.
    unsafe {
        while !reply_ready(ch) {
            let remaining = crate::scheduler_wasm::hew_wasm_sched_tick(1);
            if remaining == 0 {
                // Run queue empty but no reply yet: caller program likely
                // has an ask with no responder.  Return null rather than
                // spinning forever.
                break;
            }
        }
        reply_take(ch)
    }
}

/// Poll `count` reply channels and return the index of the first one that
/// becomes ready.  Returns -1 when the run queue drains before any channel
/// is answered (i.e. all actors have been exhausted with no winner).
///
/// On WASM all dispatch is cooperative, so this drives the scheduler one
/// activation at a time between polls.
///
/// # Timeout behaviour
///
/// `timeout_ms == -1` means "wait indefinitely" (until a winner replies or
/// the run queue is empty). Finite timeouts use a monotonic deadline loop.
///
/// # Safety
///
/// `channels` must point to a valid array of `count` valid
/// `*mut WasmReplyChannel` pointers.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
#[expect(clippy::cast_sign_loss, reason = "count validated > 0")]
#[expect(clippy::cast_possible_wrap, reason = "C ABI: reply count fits in i32")]
pub unsafe extern "C" fn hew_select_first(
    channels: *mut *mut WasmReplyChannel,
    count: i32,
    timeout_ms: i32,
) -> i32 {
    if channels.is_null() || count <= 0 {
        return -1;
    }
    let n = count as usize;
    let deadline = (timeout_ms >= 0).then(|| {
        Instant::now() + Duration::from_millis(u64::try_from(timeout_ms.max(0)).unwrap_or(0))
    });

    // SAFETY: Single-threaded on WASM; hew_wasm_sched_tick is re-entrant-safe.
    unsafe {
        loop {
            // Scan for a ready channel first (avoids a tick when reply is
            // already present, e.g. the winner was inline-dispatched by a
            // previous sched_tick call).
            for i in 0..n {
                let ch = *channels.add(i);
                if reply_ready(ch) {
                    #[expect(clippy::cast_possible_truncation, reason = "index fits in i32")]
                    return i as i32;
                }
            }
            if deadline.is_some_and(|limit| Instant::now() >= limit) {
                return -1;
            }
            // Drive one activation to make progress toward a reply.
            let remaining = crate::scheduler_wasm::hew_wasm_sched_tick(1);
            if deadline.is_some_and(|limit| Instant::now() >= limit) {
                return -1;
            }
            if remaining == 0 {
                // Run queue exhausted; do a final scan.
                for i in 0..n {
                    let ch = *channels.add(i);
                    if reply_ready(ch) {
                        #[expect(clippy::cast_possible_truncation, reason = "index fits in i32")]
                        return i as i32;
                    }
                }
                // No winner; all actors finished without replying to any of
                // the select channels (program-level stall or all channels
                // were cancelled before being added).
                return -1;
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cancel_then_late_reply_does_not_resurrect_cancelled_channel() {
        let ch = hew_reply_channel_new();
        let value = 7_i32;

        // SAFETY: `ch` is a live reply channel for the duration of the test.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_cancel(ch);

            assert!(test_cancelled(ch));
            assert_eq!(test_ref_count(ch), 2);
            assert!(!test_replied(ch));
            assert!(!reply_ready(ch));

            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );

            assert_eq!(test_ref_count(ch), 1);
            assert!(test_cancelled(ch));
            assert!(!test_replied(ch));
            assert!(!reply_ready(ch));
            assert!(
                reply_take(ch).is_null(),
                "late replies must not deposit a value after cancellation"
            );

            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn reply_then_cancel_preserves_ready_value_until_owner_releases() {
        let ch = hew_reply_channel_new();
        let value = 42_i32;

        // SAFETY: `ch` remains live until the final owner release below.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );

            assert_eq!(test_ref_count(ch), 1);
            assert!(test_replied(ch));
            assert!(!test_cancelled(ch));
            assert!(reply_ready(ch));

            hew_reply_channel_cancel(ch);
            assert_eq!(test_ref_count(ch), 1);
            assert!(test_cancelled(ch));
            assert!(test_replied(ch));
            assert!(reply_ready(ch));

            let reply = reply_take(ch).cast::<i32>();
            assert!(!reply.is_null());
            assert_eq!(*reply, 42);
            libc::free(reply.cast());
            assert!(reply_take(ch).is_null());

            hew_reply_channel_free(ch);
        }
    }

    // ── hew_reply_wait tests ────────────────────────────────────────────

    #[test]
    fn reply_wait_null_channel_returns_null() {
        // SAFETY: null input is explicitly handled.
        let result = unsafe { hew_reply_wait(ptr::null_mut()) };
        assert!(result.is_null());
    }

    #[test]
    fn reply_wait_returns_deposited_value() {
        let ch = hew_reply_channel_new();
        let value = 99_i32;

        // SAFETY: ch is valid; retain + reply mimics the ask/reply pattern.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            // Channel is ready synchronously; hew_reply_wait should return immediately.
            let result = hew_reply_wait(ch).cast::<i32>();
            assert!(!result.is_null());
            assert_eq!(*result, 99);
            libc::free(result.cast());
            hew_reply_channel_free(ch);
        }
    }

    // ── hew_select_first tests ──────────────────────────────────────────

    #[test]
    fn select_first_null_channels_returns_minus_one() {
        // SAFETY: null input is explicitly handled.
        let result = unsafe { hew_select_first(ptr::null_mut(), 0, -1) };
        assert_eq!(result, -1);
    }

    #[test]
    fn select_first_zero_count_returns_minus_one() {
        let mut ch = hew_reply_channel_new();
        // SAFETY: channels pointer is valid; count=0 triggers early return.
        let result = unsafe { hew_select_first(&raw mut ch, 0, -1) };
        assert_eq!(result, -1);
        // SAFETY: clean up the channel we allocated.
        unsafe { hew_reply_channel_free(ch) };
    }

    #[test]
    fn select_first_finite_timeout_ready_channel_returns_zero() {
        let ch = hew_reply_channel_new();
        let value = 7_i32;
        // SAFETY: ch is valid; retain so it won't be freed by hew_reply.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
        }
        let mut ch_arr = [ch];
        // Even though the channel is ready, a finite timeout returns -1
        // (we refuse to guess at elapsed time without WASI clock support).
        #[expect(
            clippy::cast_possible_truncation,
            reason = "test array length fits in i32"
        )]
        #[expect(clippy::cast_possible_wrap, reason = "test array length fits in i32")]
        // SAFETY: ch_arr contains a live reply channel owned by this test.
        let result = unsafe { hew_select_first(ch_arr.as_mut_ptr(), ch_arr.len() as i32, 10) };
        assert_eq!(result, 0, "ready reply must win before the finite timeout");
        // SAFETY: ch is valid; reply_take consumes the reply written above, and
        // hew_reply_channel_free releases the channel after the reply is taken.
        unsafe {
            let result = reply_take(ch);
            assert!(!result.is_null());
            libc::free(result);
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn select_first_zero_timeout_without_ready_channels_returns_minus_one() {
        let ch0 = hew_reply_channel_new();
        let ch1 = hew_reply_channel_new();
        let mut ch_arr = [ch0, ch1];
        // SAFETY: channels are valid and no reply is ready before the zero-ms deadline.
        let result = unsafe { hew_select_first(ch_arr.as_mut_ptr(), 2, 0) };
        assert_eq!(
            result, -1,
            "zero-ms timeout must fire before the scheduler ticks"
        );
        // SAFETY: ch0 and ch1 are valid channels created above; cancel + free
        // is the required teardown sequence when no reply was sent.
        unsafe {
            hew_reply_channel_cancel(ch0);
            hew_reply_channel_free(ch0);
            hew_reply_channel_cancel(ch1);
            hew_reply_channel_free(ch1);
        }
    }

    #[test]
    fn select_first_single_ready_channel_returns_zero() {
        let ch = hew_reply_channel_new();
        let value = 5_i32;

        // SAFETY: ch is valid; retain + reply mimics the ask/reply flow.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
        }
        let mut ch_arr = [ch];
        // SAFETY: ch_arr contains a live reply channel; count=1 fits in i32.
        let winner = unsafe { hew_select_first(ch_arr.as_mut_ptr(), 1, -1) };
        assert_eq!(winner, 0);
        // Clean up value + channel.
        // SAFETY: ch is still live; reply_take + free are balanced.
        unsafe {
            let _ = reply_take(ch);
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn select_first_picks_first_ready_among_multiple() {
        let ch0 = hew_reply_channel_new();
        let ch1 = hew_reply_channel_new();
        let ch2 = hew_reply_channel_new();
        let value0 = 10_i32;
        let value2 = 30_i32;

        // ch0 and ch2 are ready; ch1 is not.
        // SAFETY: channels are valid; retain to balance hew_reply's release.
        unsafe {
            hew_reply_channel_retain(ch0);
            hew_reply(
                ch0,
                (&raw const value0).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            hew_reply_channel_retain(ch2);
            hew_reply(
                ch2,
                (&raw const value2).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
        }
        let mut ch_arr = [ch0, ch1, ch2];
        // Should return 0 (first ready channel).
        // SAFETY: ch_arr contains live reply channels; count=3 fits in i32.
        let winner = unsafe { hew_select_first(ch_arr.as_mut_ptr(), 3, -1) };
        assert_eq!(winner, 0, "should pick lowest-index ready channel");
        // Clean up.
        // SAFETY: channels are live; cancel + free are balanced.
        unsafe {
            let _ = reply_take(ch0);
            hew_reply_channel_free(ch0);
            hew_reply_channel_cancel(ch1);
            hew_reply_channel_free(ch1);
            let _ = reply_take(ch2);
            hew_reply_channel_free(ch2);
        }
    }

    #[test]
    fn select_first_no_ready_channels_empty_queue_returns_minus_one() {
        // Neither channel gets a reply; scheduler queue is empty so we
        // should return -1 rather than spin forever.
        let ch0 = hew_reply_channel_new();
        let ch1 = hew_reply_channel_new();
        let mut ch_arr = [ch0, ch1];
        // SAFETY: channels are valid; scheduler is either uninitialised or empty.
        let result = unsafe { hew_select_first(ch_arr.as_mut_ptr(), 2, -1) };
        assert_eq!(result, -1, "no reply and no scheduler work must return -1");
        // Clean up.
        // SAFETY: channels are live; cancel + free are balanced.
        unsafe {
            hew_reply_channel_cancel(ch0);
            hew_reply_channel_free(ch0);
            hew_reply_channel_cancel(ch1);
            hew_reply_channel_free(ch1);
        }
    }
}
