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
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::actor::HewActor;
use std::ffi::c_void;
use std::ptr;
#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

#[cfg(test)]
static ACTIVE_CHANNELS: AtomicUsize = AtomicUsize::new(0);
#[cfg(test)]
static FORCE_REPLY_ALLOC_FAILURE: AtomicUsize = AtomicUsize::new(0);

/// Typed destructor for a delivered-but-never-consumed reply payload (WASM
/// parity counterpart of [`crate::reply_channel::HewReplyDropFn`]). Defined
/// locally because the native module is absent on a real `wasm32` build.
///
/// [`hew_reply`] byte-copies the reply value into the channel's `value` buffer,
/// aliasing any heap embedded in the reply type `R` into it. On the consumed
/// leg the cooperative ask loop takes the buffer and its scope-exit drop
/// releases that heap; on the never-consumed leg (timeout/cancel/orphan-retire)
/// the channel itself releases it via this destructor, run on the copied buffer
/// before `libc::free`. Registered once by the ask caller via
/// [`hew_reply_channel_set_reply_drop_fn`]; `None` for a bit-copy `R`.
pub type HewReplyDropFn = unsafe extern "C" fn(*mut c_void);

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
    /// Optional typed destructor for the reply payload (`R`'s embedded heap),
    /// run on the delivered-but-never-consumed leg in
    /// [`hew_reply_channel_free`] before `value` is `libc::free`d. `None` (the
    /// default) ⇒ bit-copy reply, buffer free alone suffices. Set once by the
    /// ask caller before submit; single-threaded so a plain field suffices (no
    /// atomic, mirroring the other WASM channel fields).
    reply_drop_fn: Option<HewReplyDropFn>,
    /// Distinguishes allocator failure from a legitimate null reply.
    allocation_failed: bool,
    /// Whether a reply has been deposited.
    replied: bool,
    /// Whether the waiter abandoned the channel.
    cancelled: bool,
    /// Set by [`retire_reply_channel`] (in `mailbox_wasm`) so the ask caller
    /// can distinguish mailbox-teardown null from a legitimate null reply.
    pub(crate) orphaned: bool,
    /// Waiter-kind discriminator (W6.010), the wasm parity counterpart of the
    /// native `HewReplyChannel::caller_actor`. When non-null, the waiter is a
    /// PARKED CONTINUATION belonging to this actor and a reply wakes it via the
    /// wasm `scheduler_wasm::enqueue_resume`; when null (the default), the
    /// waiter is the cooperative ask loop that drives the scheduler tick and
    /// reads the deposited reply directly. Single-threaded: a plain pointer (no
    /// atomic) suffices. Parity-only — no wasm actor-await e2e (E10).
    caller_actor: *mut HewActor,
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
        reply_drop_fn: None,
        allocation_failed: false,
        replied: false,
        cancelled: false,
        orphaned: false,
        caller_actor: ptr::null_mut(),
    }))
}

/// Record that the waiter on this reply channel is a PARKED CONTINUATION
/// belonging to `actor` (W6.010 wasm parity). A reply then wakes it via
/// `scheduler_wasm::enqueue_resume` instead of leaving it for the cooperative
/// ask loop. Parity counterpart of the native
/// `hew_reply_channel_set_parked_waiter`.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`]. `actor`,
/// if non-null, must reference the live `HewActor` whose continuation is parked.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_set_parked_waiter(
    ch: *mut WasmReplyChannel,
    actor: *mut HewActor,
) {
    if ch.is_null() {
        return;
    }
    // SAFETY: caller guarantees `ch` is a live reply-channel reference;
    // single-threaded so a plain store is sufficient.
    unsafe {
        (*ch).caller_actor = actor;
    }
}

/// Register the typed destructor for this channel's reply payload (`R`).
///
/// WASM parity counterpart of the native
/// [`crate::reply_channel::hew_reply_channel_set_reply_drop_fn`]. Set once by
/// the ask caller before the ask is submitted; `f` is the drop thunk for `R`
/// operating on the channel's copied reply buffer, `None` for a bit-copy `R`.
/// The destructor runs on the delivered-but-never-consumed teardown leg in
/// [`hew_reply_channel_free`].
///
/// # Safety
///
/// `ch` must be null or a live reply channel reference. `f`, when `Some`, must
/// be a valid drop thunk for this channel's reply type that operates on the
/// copied reply buffer passed to it.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_set_reply_drop_fn(
    ch: *mut WasmReplyChannel,
    f: Option<HewReplyDropFn>,
) {
    cabi_guard!(ch.is_null());
    // SAFETY: caller guarantees `ch` is a live reply-channel reference;
    // single-threaded so a plain store is sufficient.
    unsafe {
        (*ch).reply_drop_fn = f;
    }
}

unsafe fn alloc_reply_buffer(size: usize) -> *mut c_void {
    #[cfg(test)]
    if FORCE_REPLY_ALLOC_FAILURE.swap(0, Ordering::AcqRel) == 1 {
        return ptr::null_mut();
    }
    // SAFETY: delegates to libc allocator for the requested reply payload size.
    unsafe { libc::malloc(size) }
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
/// Returns `true` if the channel took ownership of the `size` bytes at
/// `value` (the waiter will receive them and own any heap pointers
/// referenced). Returns `false` if the reply was not delivered (channel
/// cancelled or per-reply allocation failed) — in that case the caller
/// still owns any heap-allocated payload and must free it.
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - `value` must point to at least `size` readable bytes (or be null
///   when `size` is 0).
/// - Must be called at most once per channel.
#[must_use = "hew_reply returns false when the channel did not take \
              ownership of the payload (cancelled or OOM); the caller \
              must free any deep-cloned heap payload in that case"]
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply(
    ch: *mut WasmReplyChannel,
    value: *mut c_void,
    size: usize,
) -> bool {
    if ch.is_null() {
        return false;
    }

    // SAFETY: Caller guarantees `ch` is valid and single-writer.
    unsafe {
        crate::scheduler_wasm::mark_current_reply_channel_consumed(ch.cast());
        debug_assert!(
            !(*ch).replied,
            "WASM reply channels must not be replied to more than once"
        );
        if (*ch).cancelled {
            hew_reply_channel_free(ch);
            return false;
        }
        let mut delivered = true;
        if size > 0 && !value.is_null() {
            let buf = alloc_reply_buffer(size);
            if buf.is_null() {
                (*ch).allocation_failed = true;
                delivered = false;
            } else {
                ptr::copy_nonoverlapping(value.cast::<u8>(), buf.cast::<u8>(), size);
                (*ch).value = buf;
                (*ch).value_size = size;
            }
        }
        (*ch).replied = true;
        // Waiter-kind branch (W6.010 wasm parity, E5/E6). A parked-continuation
        // waiter (`caller_actor` non-null) is re-enqueued so the cooperative
        // scheduler resumes it; the resumed continuation reads the now-ready
        // reply from `ch` on its resume edge. A null `caller_actor` (the
        // default) leaves the channel for the cooperative ask loop, which polls
        // `reply_ready` directly — no wake needed (single-threaded). The wasm
        // actor-decl gate (E10) means no wasm program reaches this parked branch
        // today; it stays SYMMETRIC with native for parity (native-wasm-parity).
        let caller_actor = (*ch).caller_actor;
        if !caller_actor.is_null() {
            // The wasm scheduler owns a layout-identical `scheduler_wasm::HewActor`
            // view (verified by the module's `offset_of!` parity assertions); the
            // cooperative `enqueue_resume` takes that type, so cast the
            // `actor::HewActor` handle the setter stored. Parity-only (E10).
            crate::scheduler_wasm::enqueue_resume(caller_actor.cast(), ptr::null_mut());
        }
        hew_reply_channel_free(ch);
        delivered
    }
}

/// Mark a retained WASM reply-channel reference ready without depositing a payload.
///
/// WASM parity: this is the single-threaded counterpart of the native
/// readiness-proxy ABI. It is callback-compatible with `void (*)(void*)`,
/// consumes one retained producer/observer reference, and publishes no reply
/// payload. If cancellation already won, it fail-closes by releasing the
/// retained reference without marking the channel ready.
///
/// # Safety
///
/// `ch` must be either null or a retained `WasmReplyChannel*` reference. When
/// non-null, this function consumes exactly one reference. It must be called
/// at most once for that retained reference.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_channel_signal_ready(ch: *mut c_void) {
    let ch = ch.cast::<WasmReplyChannel>();
    if ch.is_null() {
        return;
    }

    // SAFETY: caller provides a retained producer/observer reference; the
    // function consumes it on both the cancellation and ready paths.
    unsafe {
        if (*ch).cancelled {
            hew_reply_channel_free(ch);
            return;
        }
        debug_assert!(
            !(*ch).replied,
            "WASM reply channels must not be signalled ready more than once"
        );
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
        if result.is_null() && (*ch).allocation_failed {
            (*ch).allocation_failed = false;
            crate::set_last_error("hew_reply: allocation failed while copying reply payload");
        }
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

// ── Reply wait / timeout parity ─────────────────────────────────────────

/// Block until a reply has been deposited on `ch` or `timeout_ms`
/// milliseconds have elapsed, then return the value (or null on timeout).
///
/// WASM counterpart of [`crate::reply_channel::hew_reply_wait_timeout`].
/// Uses the same monotonic deadline-loop pattern as [`hew_select_first`]:
/// drives the cooperative scheduler one activation at a time until the
/// reply arrives, the deadline expires, or the run queue drains.
///
/// The caller owns the returned pointer and must free it with
/// [`libc::free`].
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - Must be called at most once per channel.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_reply_wait_timeout(
    ch: *mut WasmReplyChannel,
    timeout_ms: i32,
) -> *mut c_void {
    if ch.is_null() {
        return ptr::null_mut();
    }

    let deadline =
        Instant::now() + Duration::from_millis(u64::try_from(timeout_ms.max(0)).unwrap_or(0));

    // SAFETY: Single-threaded on WASM; hew_wasm_sched_tick is re-entrant-safe.
    unsafe {
        loop {
            if reply_ready(ch) {
                return reply_take(ch);
            }
            if Instant::now() >= deadline {
                return ptr::null_mut();
            }
            let remaining = crate::scheduler_wasm::hew_wasm_sched_tick(1);
            if Instant::now() >= deadline {
                // Check one last time in case the tick deposited the reply.
                if reply_ready(ch) {
                    return reply_take(ch);
                }
                return ptr::null_mut();
            }
            if remaining == 0 && crate::scheduler_wasm::hew_wasm_sleeping_count() == 0 {
                // Both run queue and sleep queue are empty — no further
                // progress is possible.  Do a final readiness check.
                if reply_ready(ch) {
                    return reply_take(ch);
                }
                return ptr::null_mut();
            }
        }
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
            if remaining == 0 && crate::scheduler_wasm::hew_wasm_sleeping_count() == 0 {
                // Both run queue and sleep queue are empty; no reply will
                // ever arrive.  Return null rather than spinning forever.
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
            if remaining == 0 && crate::scheduler_wasm::hew_wasm_sleeping_count() == 0 {
                // Both run queue and sleep queue are exhausted; do a final scan.
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

            let delivered = hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            assert!(
                !delivered,
                "WASM hew_reply must report not-delivered after the channel is cancelled \
                 so the caller can free any deep-cloned owned payload"
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
            let _ = hew_reply(
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

    #[test]
    fn signal_ready_marks_channel_ready_without_payload() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();

        // SAFETY: the test retains an observer-side reference that
        // `hew_reply_channel_signal_ready` consumes, leaving the original
        // waiter reference live for select/wait/free.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_signal_ready(ch.cast());

            assert!(reply_ready(ch));
            assert!(test_replied(ch));
            let mut channels = [ch];
            assert_eq!(hew_select_first(channels.as_mut_ptr(), 1, 0), 0);
            assert!(
                reply_take(ch).is_null(),
                "readiness proxy must not fabricate a reply payload"
            );
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn signal_ready_after_cancel_consumes_observer_reference() {
        let _guard = crate::runtime_test_guard();
        let pre_new = active_channel_count();
        let ch = hew_reply_channel_new();

        // SAFETY: the retained observer reference keeps `ch` live after the
        // waiter cancels and releases its own reference.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_cancel(ch);
            hew_reply_channel_free(ch);
            assert_eq!(active_channel_count(), pre_new + 1);

            hew_reply_channel_signal_ready(ch.cast());
        }

        assert_eq!(
            active_channel_count(),
            pre_new,
            "late readiness callback must release its retained channel reference"
        );
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
            let _ = hew_reply(
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

    #[test]
    fn reply_wait_surfaces_copy_oom() {
        crate::hew_clear_error();
        let ch = hew_reply_channel_new();
        let value = 99_i32;

        // SAFETY: test exercises the forced-allocation-failure path on a live channel.
        unsafe {
            hew_reply_channel_retain(ch);
            FORCE_REPLY_ALLOC_FAILURE.store(1, Ordering::Release);
            let _ = hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            let result = hew_reply_wait(ch);
            assert!(result.is_null());
            let err = std::ffi::CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap();
            assert!(err.contains("allocation failed"));
            hew_reply_channel_free(ch);
        }
    }

    // ── hew_reply_wait_timeout tests ────────────────────────────────────

    #[test]
    fn reply_wait_timeout_null_channel_returns_null() {
        // SAFETY: null input is explicitly handled.
        let result = unsafe { hew_reply_wait_timeout(ptr::null_mut(), 100) };
        assert!(result.is_null());
    }

    #[test]
    fn reply_wait_timeout_returns_deposited_value() {
        let ch = hew_reply_channel_new();
        let value = 77_i32;

        // SAFETY: ch is valid; retain + reply mimics the ask/reply pattern.
        unsafe {
            hew_reply_channel_retain(ch);
            let _ = hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            // Reply is already deposited; timeout should return immediately.
            let result = hew_reply_wait_timeout(ch, 1000).cast::<i32>();
            assert!(!result.is_null());
            assert_eq!(*result, 77);
            libc::free(result.cast());
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn reply_wait_timeout_zero_ms_no_reply_returns_null() {
        let ch = hew_reply_channel_new();

        // SAFETY: ch is valid; no reply deposited, zero-ms deadline expires immediately.
        unsafe {
            let result = hew_reply_wait_timeout(ch, 0);
            assert!(
                result.is_null(),
                "zero-ms timeout with no reply must return null"
            );
            hew_reply_channel_cancel(ch);
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn reply_wait_timeout_ready_before_deadline_returns_value() {
        let ch = hew_reply_channel_new();
        let value = 42_i32;

        // SAFETY: ch is valid; deposit reply before calling wait_timeout.
        unsafe {
            hew_reply_channel_retain(ch);
            let _ = hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            // Even with a short timeout, the ready reply should be returned.
            let result = hew_reply_wait_timeout(ch, 1).cast::<i32>();
            assert!(!result.is_null());
            assert_eq!(*result, 42);
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
            let _ = hew_reply(
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
            let _ = hew_reply(
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
        // SAFETY: ch is still live; reply_take transfers ownership of the
        // malloc'd reply buffer — free it before releasing the channel.
        unsafe {
            let val = reply_take(ch);
            if !val.is_null() {
                libc::free(val);
            }
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
            let _ = hew_reply(
                ch0,
                (&raw const value0).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            hew_reply_channel_retain(ch2);
            let _ = hew_reply(
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
        // SAFETY: channels are live; reply_take transfers ownership of the
        // malloc'd reply buffer — free each before releasing its channel.
        unsafe {
            let val0 = reply_take(ch0);
            if !val0.is_null() {
                libc::free(val0);
            }
            hew_reply_channel_free(ch0);
            hew_reply_channel_cancel(ch1);
            hew_reply_channel_free(ch1);
            let val2 = reply_take(ch2);
            if !val2.is_null() {
                libc::free(val2);
            }
            hew_reply_channel_free(ch2);
        }
    }

    #[test]
    fn select_first_no_ready_channels_empty_queue_returns_minus_one() {
        // Acquire the runtime test lock: hew_select_first calls hew_wasm_sched_tick
        // which mutates COOPERATIVE_TICK_DEPTH (non-atomic static mut).  Running
        // concurrently with scheduler_wasm tests that also touch that global
        // causes a subtract-with-overflow abort.
        let _guard = crate::runtime_test_guard();
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

    // ── #1739: delivered-but-never-consumed owned-reply typed destructor ──────

    /// WASM parity counter for [`wasm_test_reply_string_dtor`]. Serialised with
    /// the crate via `runtime_test_guard` in each test, so reset-at-start is
    /// race-free.
    static WASM_REPLY_DTOR_CALLS: AtomicUsize = AtomicUsize::new(0);

    /// WASM mirror of the native `test_reply_string_dtor`: the channel buffer is
    /// a byte-copy of the reply value, here a single embedded `*mut c_char`.
    /// Release that embedded heap and count the call.
    unsafe extern "C" fn wasm_test_reply_string_dtor(buf: *mut c_void) {
        WASM_REPLY_DTOR_CALLS.fetch_add(1, Ordering::AcqRel);
        if buf.is_null() {
            return;
        }
        // SAFETY: `buf` is the copied reply buffer holding one `*mut c_char`.
        unsafe {
            let embedded = *(buf.cast::<*mut libc::c_char>());
            if !embedded.is_null() {
                libc::free(embedded.cast());
            }
        }
    }

    /// Deposit a heap "owned reply" flat pointer into `ch` via `hew_reply`
    /// (byte-copied into the channel buffer), the way a WASM ask handler
    /// returning `string` does.
    unsafe fn wasm_deliver_owned_string_reply(ch: *mut WasmReplyChannel, payload: &str) {
        // SAFETY: `ch` is a live, retained sender-side channel reference.
        unsafe {
            let original = std::ffi::CString::new(payload).unwrap();
            let embedded: *mut libc::c_char = libc::strdup(original.as_ptr());
            assert!(!embedded.is_null(), "strdup must succeed");
            let mut reply_value: *mut libc::c_char = embedded;
            let delivered = hew_reply(
                ch,
                (&raw mut reply_value).cast(),
                std::mem::size_of::<*mut libc::c_char>(),
            );
            assert!(
                delivered,
                "owned reply must be delivered into the channel buffer"
            );
        }
    }

    /// Regression (#1739), WASM parity: a delivered owned reply that is never
    /// consumed must run its registered typed destructor on the channel teardown
    /// leg. `reply_take` nulls `value` only on consume, so a non-null `value` at
    /// final free is the never-consumed leg. Pre-fix the destructor is never
    /// invoked (embedded strdup leaks under ASan/LSan; counter stays 0).
    #[test]
    #[ignore = "RED until the reply_drop_fn run-leg lands in the WASM \
                hew_reply_channel_free (#1739 WASM-parity commit)"]
    fn delivered_owned_reply_never_consumed_runs_typed_destructor() {
        let _guard = crate::runtime_test_guard();
        WASM_REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: live channel; register the reply drop thunk, deliver, never
        // consume, tear down.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(wasm_test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            wasm_deliver_owned_string_reply(ch, "owned-reply-payload");

            hew_reply_channel_free(ch);

            assert_eq!(
                WASM_REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "a delivered-but-never-consumed owned reply must run its typed \
                 destructor exactly once on the WASM channel teardown leg (#1739)"
            );
        }
    }

    /// Exactly-once boundary (WASM): the consumed leg must NOT run the channel
    /// destructor — `reply_take` nulls `value`, transferring buffer ownership to
    /// the cooperative ask loop. Green at every stage.
    #[test]
    fn consumed_owned_reply_does_not_run_channel_destructor() {
        let _guard = crate::runtime_test_guard();
        WASM_REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: live channel; deliver then consume via reply_take.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(wasm_test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            wasm_deliver_owned_string_reply(ch, "consumed-reply");

            let buf = reply_take(ch);
            assert!(!buf.is_null());
            let embedded_back = *(buf.cast::<*mut libc::c_char>());
            assert!(!embedded_back.is_null());
            libc::free(embedded_back.cast()); // ask-loop scope-exit drop of `R`
            libc::free(buf); // free the copied buffer (WASM uses plain libc)

            hew_reply_channel_free(ch);

            assert_eq!(
                WASM_REPLY_DTOR_CALLS.load(Ordering::Acquire),
                0,
                "consumed reply must NOT run the WASM channel destructor"
            );
        }
    }

    /// Teardown routing (WASM): a reply delivered and THEN cancelled still runs
    /// its typed destructor on the free leg.
    #[test]
    #[ignore = "RED until the reply_drop_fn run-leg lands in the WASM \
                hew_reply_channel_free (#1739 WASM-parity commit)"]
    fn delivered_then_cancelled_reply_runs_destructor_once_on_free() {
        let _guard = crate::runtime_test_guard();
        WASM_REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: live channel; deliver, then cancel after the reply landed.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(wasm_test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            wasm_deliver_owned_string_reply(ch, "delivered-then-cancelled");

            hew_reply_channel_cancel(ch);
            hew_reply_channel_free(ch);

            assert_eq!(
                WASM_REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "a delivered-then-cancelled reply must still run its destructor \
                 exactly once on the final release (WASM)"
            );
        }
    }
}
