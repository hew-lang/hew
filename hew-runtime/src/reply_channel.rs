//! Hew runtime: reply channel for the actor ask pattern.
//!
//! A reply channel provides a one-shot synchronisation primitive: the
//! sender calls [`hew_reply`] to deposit a value, and the receiver
//! blocks in [`hew_reply_wait`] (or [`hew_reply_wait_timeout`]) until
//! the value is ready.

use crate::util::{CondvarExt, MutexExt};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// ── Reply channel ───────────────────────────────────────────────────────

/// One-shot reply channel for the actor ask pattern.
///
/// Thread-safety contract: exactly one thread calls [`hew_reply`],
/// exactly one thread calls [`hew_reply_wait`] (or the timeout variant).
#[repr(C)]
pub struct HewReplyChannel {
    /// Manual reference count shared by the waiting side and the in-flight reply.
    refs: AtomicUsize,
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
        refs: AtomicUsize::new(1),
        ready: AtomicBool::new(false),
        cancelled: AtomicBool::new(false),
        value: ptr::null_mut(),
        value_size: 0,
        lock: Mutex::new(()),
        cond: Condvar::new(),
    }))
}

/// Retain an additional reference to a reply channel.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_retain(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `ch` is valid while retaining a new reference.
    unsafe {
        let prev = (*ch).refs.fetch_add(1, Ordering::Relaxed);
        debug_assert!(prev > 0, "reply channel retain on released channel");
    }
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
            // The channel is orphaned — release the sender's reference.
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
        let guard = (*ch).lock.lock_or_recover();
        (*ch).cond.notify_one();
        drop(guard);
        hew_reply_channel_free(ch);
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
        let mut guard = (*ch).lock.lock_or_recover();
        while !(*ch).ready.load(Ordering::Acquire) {
            guard = (*ch).cond.wait_or_recover(guard);
        }
        let result = (*ch).value;
        (*ch).value = ptr::null_mut();
        drop(guard);
        result
    }
}

/// Block until a reply is available, returning both value and size.
///
/// Writes the reply size to `*out_size`. The caller owns the returned
/// pointer and must free it with [`libc::free`].
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - `out_size` must be a valid, non-null writable pointer.
/// - Must be called at most once per channel.
pub(crate) unsafe fn hew_reply_wait_with_size(
    ch: *mut HewReplyChannel,
    out_size: *mut usize,
) -> *mut c_void {
    if ch.is_null() || out_size.is_null() {
        return ptr::null_mut();
    }

    // SAFETY: Caller guarantees `ch` is valid and single-reader.
    unsafe {
        // Fast path: check atomic flag without locking.
        if (*ch).ready.load(Ordering::Acquire) {
            let result = (*ch).value;
            *out_size = (*ch).value_size;
            (*ch).value = ptr::null_mut();
            return result;
        }

        // Slow path: wait on condvar.
        let mut guard = (*ch).lock.lock_or_recover();
        while !(*ch).ready.load(Ordering::Acquire) {
            guard = (*ch).cond.wait_or_recover(guard);
        }
        let result = (*ch).value;
        *out_size = (*ch).value_size;
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
        let mut guard = (*ch).lock.lock_or_recover();

        while !(*ch).ready.load(Ordering::Acquire) {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() {
                return ptr::null_mut();
            }
            let (new_guard, wait_result) = (*ch).cond.wait_timeout_or_recover(guard, remaining);
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

/// Release a reply channel reference and free the channel when it reaches zero.
///
/// # Safety
///
/// `ch` must have been returned by [`hew_reply_channel_new`] and must
/// not be used after the final release.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_free(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `ch` is a live reply channel reference.
    unsafe {
        let prev = (*ch).refs.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev > 0, "reply channel release on released channel");
        if prev != 1 {
            return;
        }
        if !(*ch).value.is_null() {
            libc::free((*ch).value);
        }
        drop(Box::from_raw(ch));
    }
}

/// Mark a reply channel as abandoned.
///
/// Late repliers observe the cancelled flag and free the channel themselves,
/// avoiding use-after-free when a select times out or chooses another arm.
///
/// # Safety
///
/// `ch` must have been returned by [`hew_reply_channel_new`] and must
/// remain valid until its remaining references are released.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_cancel(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `ch` is valid while cancellation is recorded.
    unsafe {
        (*ch).cancelled.store(true, Ordering::Release);
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

#[cfg(test)]
pub(crate) unsafe fn hew_reply_channel_is_ready_for_test(ch: *mut HewReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }

    // SAFETY: test callers pass a valid channel pointer and only read the
    // atomic readiness flag; no ownership changes occur here.
    unsafe { (*ch).ready.load(Ordering::Acquire) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cancel_then_owner_release_leaves_sender_reference_for_late_reply() {
        let ch = hew_reply_channel_new();

        // SAFETY: ch is a valid channel pointer; FFI calls test ref-counting behaviour.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_cancel(ch);

            assert_eq!((*ch).refs.load(Ordering::Acquire), 2);

            hew_reply_channel_free(ch);
            assert_eq!((*ch).refs.load(Ordering::Acquire), 1);

            hew_reply(ch, ptr::null_mut(), 0);
        }
    }

    #[test]
    fn reply_then_cancel_preserves_ready_value_until_owner_releases() {
        let ch = hew_reply_channel_new();
        let value = 42_i32;

        // SAFETY: ch is a valid channel pointer; value address is valid for the block scope.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const value).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );

            assert_eq!((*ch).refs.load(Ordering::Acquire), 1);
            assert!((*ch).ready.load(Ordering::Acquire));

            hew_reply_channel_cancel(ch);

            let reply = hew_reply_wait(ch).cast::<i32>();
            assert!(!reply.is_null());
            assert_eq!(*reply, 42);
            libc::free(reply.cast());

            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn send_recv_roundtrip() {
        let ch = hew_reply_channel_new();
        let payload = 99_i64;

        // SAFETY: ch is a valid channel pointer; payload address is valid for the block scope.
        unsafe {
            // Sender retains so the channel survives the reply call.
            hew_reply_channel_retain(ch);
            hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i64>(),
            );

            let result = hew_reply_wait(ch).cast::<i64>();
            assert!(!result.is_null());
            assert_eq!(*result, 99);
            libc::free(result.cast());
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn timeout_expires_returns_null() {
        let ch = hew_reply_channel_new();

        // SAFETY: ch is a valid channel pointer; testing timeout with no reply pending.
        unsafe {
            let result = hew_reply_wait_timeout(ch, 10);
            assert!(result.is_null());
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn null_channel_safety() {
        // All functions should handle null gracefully.
        // SAFETY: All functions are documented to handle null pointers gracefully.
        unsafe {
            hew_reply(ptr::null_mut(), ptr::null_mut(), 0);
            assert!(hew_reply_wait(ptr::null_mut()).is_null());
            assert!(hew_reply_wait_timeout(ptr::null_mut(), 100).is_null());
            hew_reply_channel_retain(ptr::null_mut());
            hew_reply_channel_free(ptr::null_mut());
            hew_reply_channel_cancel(ptr::null_mut());
        }
    }

    #[test]
    fn threaded_send_recv() {
        let ch = hew_reply_channel_new();
        let value = 77_i32;

        // SAFETY: ch is a valid channel; retain/free manage ref count across threads.
        unsafe {
            hew_reply_channel_retain(ch);

            let ch_ptr = ch as usize;
            let handle = std::thread::spawn(move || {
                let ch = ch_ptr as *mut HewReplyChannel;
                std::thread::sleep(Duration::from_millis(10));
                let v = 77_i32;
                hew_reply(
                    ch,
                    (&raw const v).cast_mut().cast(),
                    std::mem::size_of::<i32>(),
                );
            });

            let result = hew_reply_wait(ch).cast::<i32>();
            assert!(!result.is_null());
            assert_eq!(*result, value);
            libc::free(result.cast());
            hew_reply_channel_free(ch);
            handle.join().unwrap();
        }
    }

    #[test]
    fn select_first_null_returns_negative_one() {
        // SAFETY: Null pointer is passed deliberately to verify graceful handling.
        let result = unsafe { hew_select_first(ptr::null_mut(), 0, 10) };
        assert_eq!(result, -1);
    }

    // ── Concurrency stress tests (TSAN targets) ──────────────────────────
    //
    // These tests run the cancel / late-reply / select-cancel races that are
    // hardest to catch with single-threaded unit tests.  They are the primary
    // targets for the nightly TSan CI job; they also run under ASan to guard
    // against use-after-free in the same paths.

    /// Cancel racing an in-flight reply across threads.
    ///
    /// Pattern: waiter cancels immediately after creating the channel while
    /// the sender concurrently calls `hew_reply`.  The ref-count must prevent
    /// use-after-free; the acquire/release barrier on `cancelled` must be
    /// correctly ordered so TSAN can verify no data race on `value` or `refs`.
    #[test]
    fn threaded_cancel_races_late_reply() {
        const ITERS: usize = 500;
        for _ in 0..ITERS {
            // SAFETY: ch is valid; ref counts are managed explicitly below.
            unsafe {
                let ch = hew_reply_channel_new();
                hew_reply_channel_retain(ch); // sender's reference

                let ch_usize = ch as usize;
                let sender = std::thread::spawn(move || {
                    let ch = ch_usize as *mut HewReplyChannel;
                    let v = 42_i32;
                    // SAFETY: ch is valid until hew_reply drops the sender ref.
                    hew_reply(
                        ch,
                        (&raw const v).cast_mut().cast(),
                        std::mem::size_of::<i32>(),
                    );
                });

                // Waiter: cancel immediately then release the waiter's ref.
                // The sender sees `cancelled=true` and takes the cleanup path.
                hew_reply_channel_cancel(ch);
                hew_reply_channel_free(ch);
                sender.join().unwrap();
            }
        }
    }

    /// `hew_select_first` cancel + concurrent late replies.
    ///
    /// Three channels race; whichever fires first is consumed via
    /// `hew_select_first`.  The two losing channels are cancelled while their
    /// sender threads may still be in flight, exercising the cancelled-path
    /// inside `hew_reply`.  TSAN checks that cancellation and the sender's
    /// check of the `cancelled` flag are properly release/acquire-ordered.
    #[test]
    fn threaded_select_cancel_with_late_replies() {
        const ARMS: usize = 3;
        const ITERS: usize = 200;
        for _ in 0..ITERS {
            // SAFETY: channel lifetimes are managed through ref counts;
            // all sender threads join before the next iteration.
            unsafe {
                let mut chs: [*mut HewReplyChannel; ARMS] = [ptr::null_mut(); ARMS];
                for ch in &mut chs {
                    *ch = hew_reply_channel_new();
                    hew_reply_channel_retain(*ch); // sender's reference
                }

                let mut senders = Vec::with_capacity(ARMS);
                for (i, &ch) in chs.iter().enumerate() {
                    let ch_usize = ch as usize;
                    senders.push(std::thread::spawn(move || {
                        let ch = ch_usize as *mut HewReplyChannel;
                        let v = i32::try_from(i).expect("ARMS fits in i32");
                        // SAFETY: ch valid until hew_reply frees the sender ref.
                        hew_reply(
                            ch,
                            (&raw const v).cast_mut().cast(),
                            std::mem::size_of::<i32>(),
                        );
                    }));
                }

                // Block until one channel fires (indefinite timeout = -1).
                let winner = hew_select_first(
                    chs.as_mut_ptr(),
                    i32::try_from(ARMS).expect("ARMS fits in i32"),
                    -1,
                );
                assert!(winner >= 0 && winner < i32::try_from(ARMS).expect("ARMS fits in i32"));
                let winner = usize::try_from(winner).expect("winner is non-negative");

                // Consume the winning value.
                let val = hew_reply_wait(chs[winner]);
                if !val.is_null() {
                    libc::free(val);
                }
                hew_reply_channel_free(chs[winner]);

                // Cancel and release the losers. Their senders observe
                // `cancelled=true` and free the channel themselves.
                for (i, &ch) in chs.iter().enumerate() {
                    if i != winner {
                        hew_reply_channel_cancel(ch);
                        hew_reply_channel_free(ch);
                    }
                }

                for s in senders {
                    s.join().unwrap();
                }
            }
        }
    }

    /// High-concurrency parallel ask-reply roundtrips.
    ///
    /// Multiple independent pairs run simultaneously to stress the condvar
    /// slow-path in `hew_reply_wait` and the ref-counting paths under
    /// concurrent load.  TSAN checks ordering across the `ready` flag.
    #[test]
    fn threaded_parallel_roundtrips() {
        const THREADS: usize = 8;
        const ROUNDS: usize = 64;

        let handles: Vec<_> = (0..THREADS)
            .map(|t| {
                std::thread::spawn(move || {
                    for r in 0..ROUNDS {
                        // SAFETY: each channel is created, used, and destroyed
                        // entirely within this block; no aliasing across rounds.
                        unsafe {
                            let ch = hew_reply_channel_new();
                            hew_reply_channel_retain(ch); // sender's reference
                            let ch_usize = ch as usize;
                            let expected =
                                i32::try_from(t * ROUNDS + r).expect("index fits in i32");

                            let sender = std::thread::spawn(move || {
                                let ch = ch_usize as *mut HewReplyChannel;
                                // SAFETY: ch valid until hew_reply frees sender ref.
                                hew_reply(
                                    ch,
                                    (&raw const expected).cast_mut().cast(),
                                    std::mem::size_of::<i32>(),
                                );
                            });

                            let result = hew_reply_wait(ch).cast::<i32>();
                            assert!(!result.is_null());
                            assert_eq!(*result, expected);
                            libc::free(result.cast());
                            hew_reply_channel_free(ch);
                            sender.join().unwrap();
                        }
                    }
                })
            })
            .collect();

        for h in handles {
            h.join().unwrap();
        }
    }
}
