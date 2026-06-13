//! Hew runtime: reply channel for the actor ask pattern.
//!
//! A reply channel provides a one-shot synchronisation primitive: the
//! sender calls [`hew_reply`] to deposit a value, and the receiver
//! blocks in [`hew_reply_wait`] (or [`hew_reply_wait_timeout`]) until
//! the value is ready.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use crate::actor::HewActor;
use crate::await_cancel::{
    hew_await_cancel_complete, hew_await_cancel_free, hew_await_cancel_retain,
};
use crate::await_cancel::{hew_await_cancel_status, AwaitCancelStatus, HewAwaitCancel};
use crate::util::{CondvarExt, MutexExt};
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicUsize, Ordering};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant};

// ── Reply channel ───────────────────────────────────────────────────────

#[cfg(test)]
static ACTIVE_CHANNELS: AtomicUsize = AtomicUsize::new(0);
#[cfg(test)]
static FORCE_REPLY_ALLOC_FAILURE: AtomicBool = AtomicBool::new(false);

/// Force the next `alloc_reply_buffer` call to fail (return null), simulating
/// OOM in the reply-buffer copy path inside `hew_reply`. Exposed to other
/// runtime modules (e.g. `lambda_actor` tests) so they can pin the
/// status=Ok / payload=null edge case the codegen ask-payload guard catches.
/// Auto-clears after the next call to `alloc_reply_buffer`.
#[cfg(test)]
pub(crate) fn force_reply_alloc_failure_for_test() {
    FORCE_REPLY_ALLOC_FAILURE.store(true, Ordering::Release);
}

// ── Debug allocator-pairing tracker ────────────────────────────────────────
//
// Reply payloads allocated here via `libc::malloc` are registered in the
// runtime-wide tracker (crate::alloc_tracker).  Lambda-actor body reply
// buffers use Rust's `GlobalAlloc` (`Box::into_raw`) and are freed via
// `lambda_actor::free_body_reply_buf` — each free site in that module asserts
// the pointer is NOT in the set, catching any allocator crossing before it
// reaches `Box::from_raw`.
//
// Active only in debug builds; zero overhead in release.

#[cfg(debug_assertions)]
use crate::alloc_tracker::{
    debug_is_libc_tracked, debug_track_libc_alloc, debug_untrack_libc_alloc,
};

/// Typed destructor for a delivered-but-never-consumed reply payload.
///
/// [`hew_reply`] byte-copies the reply value into the channel's `value`
/// buffer, aliasing any heap pointers embedded in the reply type `R`
/// (`String`/`Vec`/`HashMap`/`Closure`/struct-with-owned-fields) into that
/// buffer. On the consumed leg the waiter takes the buffer and its scope-exit
/// drop releases that heap; on the never-consumed leg (timeout/cancel/
/// await-cancel/orphan-retire/shutdown) the channel itself must release it.
/// This is that release: the semantic counterpart to the buffer's `libc::free`
/// (`alias-byte-copy-not-semantic-clone`). It receives the copied buffer
/// (`value`) and drops `R` in place. Registered once by the ask caller (which
/// knows `R` statically) via [`hew_reply_channel_set_reply_drop_fn`], before
/// the ask is submitted; `None`/null for a bit-copy `R` with no embedded heap.
pub type HewReplyDropFn = unsafe extern "C" fn(*mut c_void);

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
    /// Set to `true` by [`hew_reply_channel_retire_orphaned_ask_sender_ref`]
    /// so the waiter can distinguish a mailbox-teardown null from a
    /// legitimate null reply deposited by the handler.
    pub(crate) orphaned: AtomicBool,
    /// Reply payload (malloc'd by [`hew_reply`], owned by the waiter).
    value: *mut c_void,
    /// Size of `value` in bytes.
    value_size: usize,
    /// Optional typed destructor for the reply payload (`R`'s embedded heap),
    /// run on the delivered-but-never-consumed leg in
    /// [`hew_reply_channel_free`] before `value` is `libc::free`d. Null (the
    /// default) ⇒ the reply type is bit-copy (no embedded heap) and the buffer
    /// free alone suffices — current behaviour. Stored type-erased as the bits
    /// of a [`HewReplyDropFn`]; set once by the ask caller before submit (no
    /// race with any reply), read once at final free (after `refs` hits 0).
    reply_drop_fn: AtomicPtr<c_void>,
    /// Distinguishes allocator failure from a legitimate null reply.
    allocation_failed: AtomicBool,
    /// The waiter-kind discriminator (W6.010). When non-null, the waiter is a
    /// PARKED CONTINUATION belonging to this actor: a reply wakes it via
    /// `scheduler::enqueue_resume(caller_actor, ..)` (the suspend edge owns the
    /// `suspended_cont` handle; the FG3 two-phase park inside `enqueue_resume`
    /// covers a reply that fires mid-park). When null (the default), the waiter
    /// is a CONDVAR-blocked foreign/main thread woken by `cond.notify_one()`
    /// (E6 — the foreign-thread ask path stays on the condvar). Set BEFORE the
    /// ask is submitted (so before any possible reply) by
    /// [`hew_reply_channel_set_parked_waiter`].
    caller_actor: AtomicPtr<HewActor>,
    /// Mutex protecting the condvar wait.
    lock: Mutex<()>,
    /// Condvar signalled by [`hew_reply`].
    cond: Condvar,
    /// Optional common cancellation/deadline record attached to a suspended ask.
    await_cancel: AtomicPtr<HewAwaitCancel>,
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
    #[cfg(test)]
    ACTIVE_CHANNELS.fetch_add(1, Ordering::Relaxed);
    Box::into_raw(Box::new(HewReplyChannel {
        refs: AtomicUsize::new(1),
        ready: AtomicBool::new(false),
        cancelled: AtomicBool::new(false),
        orphaned: AtomicBool::new(false),
        value: ptr::null_mut(),
        value_size: 0,
        reply_drop_fn: AtomicPtr::new(ptr::null_mut()),
        allocation_failed: AtomicBool::new(false),
        caller_actor: AtomicPtr::new(ptr::null_mut()),
        lock: Mutex::new(()),
        cond: Condvar::new(),
        await_cancel: AtomicPtr::new(ptr::null_mut()),
    }))
}

/// Record that the waiter on this reply channel is a PARKED CONTINUATION
/// belonging to `actor` (W6.010), so [`hew_reply`] wakes it via
/// `scheduler::enqueue_resume` instead of the condvar.
///
/// Called by the suspendable-caller ask emission BEFORE submitting the ask, so
/// the waiter-kind is visible to any reply (no race: the set happens-before the
/// send, the send happens-before the handler can reply). A null `actor` is a
/// no-op that leaves the channel on the condvar path (the foreign-thread
/// default, E6).
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`]. `actor`,
/// if non-null, must reference the live `HewActor` whose continuation is being
/// parked on this ask; it must outlive the reply (upheld by the suspend edge
/// owning the parked continuation).
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_set_parked_waiter(
    ch: *mut HewReplyChannel,
    actor: *mut HewActor,
) {
    if ch.is_null() {
        return;
    }
    // SAFETY: caller guarantees `ch` is a live reply-channel reference.
    unsafe {
        (*ch).caller_actor.store(actor, Ordering::Release);
    }
}

/// Attach a shared await-cancellation registration to this suspended reply wait.
///
/// # Safety
///
/// `ch` must be a live reply channel and `reg`, when non-null, must be a live
/// await-cancellation registration. The channel retains the registration until
/// final free or replacement.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_set_await_cancel(
    ch: *mut HewReplyChannel,
    reg: *mut HewAwaitCancel,
) {
    if ch.is_null() {
        return;
    }
    if !reg.is_null() {
        // SAFETY: caller provides a live registration to retain for the channel.
        unsafe { hew_await_cancel_retain(reg) };
    }
    // SAFETY: caller holds a live channel reference.
    let old = unsafe { (*ch).await_cancel.swap(reg, Ordering::AcqRel) };
    if !old.is_null() {
        // SAFETY: releases the channel's previous retained registration.
        unsafe { hew_await_cancel_free(old) };
    }
}

/// Register the typed destructor for this channel's reply payload (`R`).
///
/// Set once by the ask caller, BEFORE the ask is submitted: the caller knows
/// the reply type `R` statically, and submitting-before-setting would race the
/// reply. `f` is the drop thunk for `R`, operating on the channel's copied
/// reply buffer; pass `None` for a bit-copy `R` with no embedded heap (the
/// default — equivalent to never calling this). The destructor runs on the
/// delivered-but-never-consumed teardown leg in [`hew_reply_channel_free`].
///
/// # Safety
///
/// `ch` must be null or a live reply channel reference. `f`, when `Some`, must
/// be a valid drop thunk for this channel's reply type that operates on the
/// copied reply buffer passed to it (`value`).
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_set_reply_drop_fn(
    ch: *mut HewReplyChannel,
    f: Option<HewReplyDropFn>,
) {
    if ch.is_null() {
        return;
    }
    // Type-erase the fn pointer into the AtomicPtr slot (round-tripped back via
    // transmute in `hew_reply_channel_free`). None ⇒ null ⇒ no-op free leg.
    let raw = f.map_or(ptr::null_mut(), |f| f as *mut c_void);
    // SAFETY: caller guarantees `ch` is a live reply channel reference; the
    // Release store pairs with the Acquire load at final free.
    unsafe {
        (*ch).reply_drop_fn.store(raw, Ordering::Release);
    }
}

/// Cleanup callback for [`crate::await_cancel::HewAwaitCancel`] reply waits.
///
/// # Safety
///
/// `source` must be a live reply-channel reference retained by the suspended ask
/// source. The callback is invoked at most once by the common await CAS.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_cancel_cleanup(source: *mut c_void, _status: i32) {
    let ch = source.cast::<HewReplyChannel>();
    if ch.is_null() {
        return;
    }
    // SAFETY: await-cancel source contract supplies a live channel reference.
    unsafe {
        (*ch).cancelled.store(true, Ordering::Release);
        let guard = (*ch).lock.lock_or_recover();
        (*ch).cond.notify_one();
        drop(guard);
    }
}

unsafe fn alloc_reply_buffer(size: usize) -> *mut c_void {
    #[cfg(test)]
    if FORCE_REPLY_ALLOC_FAILURE.swap(false, Ordering::AcqRel) {
        return ptr::null_mut();
    }
    // SAFETY: delegates to libc allocator for the requested reply payload size.
    let ptr = unsafe { libc::malloc(size) };
    #[cfg(debug_assertions)]
    if !ptr.is_null() {
        debug_track_libc_alloc(ptr.cast());
    }
    ptr
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

/// Run the channel's registered reply destructor on a producer-side `value`
/// that the channel did NOT take (the cancel and OOM legs of [`hew_reply`]),
/// reclaiming any heap embedded in `R` before the producer's stack slot is
/// discarded.
///
/// #1739: the registered `reply_drop_fn` is the **single canonical release
/// path for every non-consume edge** — consumed (the waiter drops it) XOR
/// never-consumed/cancelled/OOM (the channel drops it). This is the cancel/OOM
/// analog of the never-consumed leg in [`hew_reply_channel_free`].
///
/// No-op when `value` is null or no destructor is registered. A null
/// `reply_drop_fn` means either a bit-copy `R` (no embedded heap to reclaim)
/// or the legacy manual-reclaim contract used by the in-process unit tests
/// (`strdup` + `libc::free` on the `false` leg) — in that case the caller
/// still owns `value` and the returned `false` signals it must free it.
unsafe fn run_registered_reply_drop_on_value(ch: *mut HewReplyChannel, value: *mut c_void) {
    if value.is_null() {
        return;
    }
    // SAFETY: caller guarantees `ch` is a live channel and `value` points at a
    // valid `R` the destructor was registered for.
    unsafe {
        let drop_raw = (*ch).reply_drop_fn.load(Ordering::Acquire);
        if !drop_raw.is_null() {
            let drop_fn = std::mem::transmute::<*mut c_void, HewReplyDropFn>(drop_raw);
            drop_fn(value);
        }
    }
}

unsafe fn release_sender_ref_if_cancelled(ch: *mut HewReplyChannel, value: *mut c_void) -> bool {
    // SAFETY: caller guarantees `ch` is a live sender-side reply channel reference.
    unsafe {
        if (*ch).cancelled.load(Ordering::Acquire) {
            // #1739 cancelled-before-delivery leg: the reply buffer was never
            // published, so the producer's `value` is the only live copy of the
            // reply. Reclaim its embedded heap via the registered destructor
            // before releasing this sender reference.
            run_registered_reply_drop_on_value(ch, value);
            hew_reply_channel_free(ch);
            return true;
        }
    }
    false
}

unsafe fn publish_reply_from_sender_ref(
    ch: *mut HewReplyChannel,
    value: *mut c_void,
    value_size: usize,
) {
    // SAFETY: caller guarantees `ch` is valid, not cancelled, and single-writer.
    unsafe {
        debug_assert!(
            !(*ch).ready.load(Ordering::Acquire),
            "reply channel published more than once"
        );
        (*ch).value = value;
        (*ch).value_size = value_size;
        // Release barrier ensures value writes are visible to the waiter.
        (*ch).ready.store(true, Ordering::Release);
        // NEW-6b exactly-one-waker: the reply only owns the wake if it WINS the
        // completion arbiter. When a deadline registration is attached,
        // `hew_await_cancel_complete` returns 1 iff this reply won the one-shot
        // CAS (claiming the reply and cancelling the timer); it returns 0 if the
        // deadline timer already won (and has already, or will, wake the caller).
        // Re-enqueuing on the losing edge produces a spurious wake that lingers
        // into the caller's NEXT park (the scheduler's `pending_wake`), so the
        // parked-continuation wake below is gated on winning. With NO registration
        // (a plain ask, no `| after d`) the reply is the only waker and always
        // wakes. The arbiter `complete` itself MUST still run unconditionally so a
        // winning reply cancels the timer.
        let await_cancel = (*ch).await_cancel.load(Ordering::Acquire);
        let reply_won = if await_cancel.is_null() {
            true
        } else {
            // SAFETY: the channel holds a retained registration reference.
            hew_await_cancel_complete(await_cancel) != 0
        };

        // Waiter-kind branch (W6.010, E5/E6). A parked-continuation waiter
        // (`caller_actor` non-null) is woken by re-enqueuing its actor; the
        // resumed continuation reads the now-ready reply value from `ch` on its
        // resume edge. A condvar-blocked foreign/main thread (`caller_actor`
        // null — the default, E6) is woken by `cond.notify_one()`. The load is
        // Acquire-paired with the Release store in
        // `hew_reply_channel_set_parked_waiter`, which happens-before the ask
        // submission and therefore before any reply can fire.
        let caller_actor = (*ch).caller_actor.load(Ordering::Acquire);
        if caller_actor.is_null() {
            // Foreign/main-thread condvar waiter (E6 — unchanged). A condvar
            // waiter re-checks its `ready`/`cancelled` predicate under the lock,
            // so a notify on the losing edge is harmless; it is left ungated.
            let guard = (*ch).lock.lock_or_recover();
            (*ch).cond.notify_one();
            drop(guard);
        } else if reply_won {
            // Parked-continuation waiter: re-enqueue the caller actor ONLY when
            // the reply won the arbiter (or carried no deadline registration). The
            // continuation handle is owned by the scheduler's suspend edge (the
            // `suspended_cont` slot); `enqueue_resume` reads the slot itself and
            // records a `pending_wake` if the reply fired in the FG3 park window
            // (so a mid-park reply is never lost). The `cont` argument is unused
            // by the resume edge (the slot is authoritative), so pass null.
            // SAFETY: `caller_actor` references the live `HewActor` whose
            // continuation is parked on this ask; the suspend edge keeps it
            // alive until the resume reclaims the parked continuation.
            crate::scheduler::enqueue_resume(caller_actor, ptr::null_mut());
        }
        hew_reply_channel_free(ch);
    }
}

unsafe fn take_ready_reply(ch: *mut HewReplyChannel, out_size: Option<*mut usize>) -> *mut c_void {
    // SAFETY: caller guarantees `ch` is a ready, live reply channel.
    unsafe {
        let result = (*ch).value;
        if let Some(out_size) = out_size {
            *out_size = (*ch).value_size;
        }
        (*ch).value = ptr::null_mut();
        if result.is_null() && (*ch).allocation_failed.swap(false, Ordering::AcqRel) {
            crate::set_last_error("hew_reply: allocation failed while copying reply payload");
        }
        result
    }
}

pub(crate) unsafe fn hew_reply_channel_mark_allocation_failed(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }

    // SAFETY: caller guarantees `ch` is a live reply channel reference.
    unsafe {
        (*ch).allocation_failed.store(true, Ordering::Release);
    }
}

/// Retire an ask sender reference whose mailbox ownership ends before dispatch.
///
/// This is the explicit mailbox-teardown path for orphaned ask waiters
/// (mailbox free, queue eviction, coalesce replacement). It consumes the
/// sender-side reference held by the mailbox, publishing the same empty reply
/// that older fallback paths emitted to avoid hanging waiters.
pub(crate) unsafe fn hew_reply_channel_retire_orphaned_ask_sender_ref(ch: *mut HewReplyChannel) {
    if ch.is_null() {
        return;
    }

    // SAFETY: caller guarantees `ch` is the mailbox-owned sender-side reference.
    unsafe {
        if release_sender_ref_if_cancelled(ch, ptr::null_mut()) {
            return;
        }

        debug_assert!(
            (*ch).value.is_null() && (*ch).value_size == 0,
            "orphaned ask teardown must not overwrite an existing reply"
        );
        // Mark the channel as orphaned before publishing the null sentinel so
        // the waiter can distinguish mailbox teardown from a legitimate null reply.
        // DROP-SAFETY: this flag is set before the condvar signal that wakes the
        // waiter, ensuring the waiter sees orphaned=true on the first Acquire load.
        (*ch).orphaned.store(true, Ordering::Release);
        publish_reply_from_sender_ref(ch, ptr::null_mut(), 0);
    }
}

// ── Reply (sender side) ─────────────────────────────────────────────────

/// Deposit a reply value and wake the waiter.
///
/// The payload is deep-copied so the caller retains ownership of `value`.
///
/// Returns `true` if the channel took ownership of the `size` bytes at
/// `value` (i.e., they were copied into the channel buffer and will be
/// delivered to the waiter — any heap pointers embedded in those bytes
/// transfer their ownership to the waiter). Returns `false` if the reply
/// was not delivered: the channel was cancelled, or the per-reply
/// allocation failed.
///
/// #1739 single-reaper contract: when a typed reply destructor is registered
/// on the channel (via [`hew_reply_channel_set_reply_drop_fn`], as codegen
/// does at the ask-caller site for every owned `R`), the `false` legs run that
/// destructor on `value` here, so the channel is the **single canonical
/// release path for every non-consume edge** (cancel/OOM mirror the
/// never-consumed leg in [`hew_reply_channel_free`]). The producer must NOT
/// also free `value` in that case. When **no** destructor is registered
/// (a bit-copy `R` with no embedded heap, or the in-process unit-test contract
/// that `strdup`s + `libc::free`s its own clone), the `false` return signals
/// the caller still owns `value` and must free it with the matching
/// type-specific destructor — without one of these two paths, those clones
/// leak.
///
/// # Safety
///
/// - `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
/// - `value` must point to at least `size` readable bytes (or be null
///   when `size` is 0).
/// - Must be called at most once per channel.
#[must_use = "hew_reply returns false when the channel did not take \
              ownership of the payload (cancelled or OOM); the caller \
              must free any deep-cloned heap payload in that case. \
              Internal Rust call sites that pass stack values or null \
              should bind the result to `_` to acknowledge the contract."]
#[no_mangle]
pub unsafe extern "C" fn hew_reply(
    ch: *mut HewReplyChannel,
    value: *mut c_void,
    size: usize,
) -> bool {
    if ch.is_null() {
        return false;
    }

    // SAFETY: Caller guarantees `ch` is valid and single-writer.
    unsafe {
        crate::scheduler::mark_current_reply_channel_consumed(ch.cast());
        if release_sender_ref_if_cancelled(ch, value) {
            // Channel was cancelled before delivery. The registered reply
            // destructor (when present) has reclaimed `value`'s embedded heap
            // above; if none is registered the caller still owns `value` and
            // must free it (see `run_registered_reply_drop_on_value`).
            return false;
        }

        let mut copied_value = ptr::null_mut();
        let mut copied_size = 0;
        let mut delivered = true;
        if size > 0 && !value.is_null() {
            // SAFETY: malloc for deep copy of reply payload.
            let buf = alloc_reply_buffer(size);
            if buf.is_null() {
                (*ch).allocation_failed.store(true, Ordering::Release);
                // The reply buffer could not be allocated; the waiter will
                // observe a null reply and the allocation-failed flag. The
                // channel never took `value`, so reclaim its embedded heap via
                // the registered destructor (the single-reaper contract, same
                // as the cancel leg). When no destructor is registered the
                // caller still owns `value` and must free its deep clone — the
                // returned `false` signals that.
                run_registered_reply_drop_on_value(ch, value);
                delivered = false;
            } else {
                ptr::copy_nonoverlapping(value.cast::<u8>(), buf.cast::<u8>(), size);
                copied_value = buf;
                copied_size = size;
            }
        }
        publish_reply_from_sender_ref(ch, copied_value, copied_size);
        delivered
    }
}

/// Mark a retained reply-channel reference ready without depositing a payload.
///
/// This is the callback-compatible readiness proxy for multiplexed waits:
/// pass a retained `HewReplyChannel*` as the observer context, and this
/// function consumes that retained producer/observer reference when it fires.
/// The original waiter reference remains owned by the caller and must still
/// be released with [`hew_reply_channel_free`].
///
/// # Safety
///
/// `ch` must be either null or a retained `HewReplyChannel*` reference. When
/// non-null, this function consumes exactly one reference. It must be called
/// at most once for that retained reference.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_signal_ready(ch: *mut c_void) {
    let ch = ch.cast::<HewReplyChannel>();
    if ch.is_null() {
        return;
    }

    // SAFETY: caller provides a retained producer/observer reference. The
    // helper either consumes it on cancellation or publishes a null payload and
    // releases it, matching `hew_reply`'s sender-reference ownership model.
    unsafe {
        if release_sender_ref_if_cancelled(ch, ptr::null_mut()) {
            return;
        }
        publish_reply_from_sender_ref(ch, ptr::null_mut(), 0);
    }
}

// ── Reply payload free ────────────────────────────────────────────────────

/// Free a reply payload returned by [`hew_reply_wait`], [`hew_reply_wait_timeout`],
/// or [`hew_lambda_actor_ask`].
///
/// # Allocator pairing contract
///
/// Reply payloads are allocated via `libc::malloc` inside [`alloc_reply_buffer`].
/// They **must** be freed with this function (which calls `libc::free`) — NOT
/// with `hew_duplex_payload_free`, which uses Rust's `GlobalAlloc` and would
/// produce **undefined behaviour** on any platform where `GlobalAlloc ≠ libc
/// malloc` (e.g. jemalloc, mimalloc).
///
/// Lambda-actor body reply buffers use `GlobalAlloc` (`Box::into_raw`) and are
/// freed via `lambda_actor::free_body_reply_buf` — see that module for the
/// counterpart free path. The two allocators must never be crossed.
///
/// Passing `ptr = null` is safe and a no-op.
///
/// # Safety
///
/// `ptr` must be a pointer previously returned by a successful reply wait call
/// (or `hew_lambda_actor_ask`). The pointer is invalid after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_payload_free(ptr: *mut u8, _len: usize) {
    if ptr.is_null() {
        return;
    }
    #[cfg(debug_assertions)]
    {
        debug_assert!(
            debug_is_libc_tracked(ptr),
            "allocator-pairing contract violation: {ptr:p} is not libc-tracked; \
             use hew_reply_payload_free only for reply-wait payloads (libc::malloc). \
             Body reply buffers (Box/GlobalAlloc) are freed by lambda_actor::free_body_reply_buf.",
        );
        debug_untrack_libc_alloc(ptr);
    }
    // SAFETY: ptr was allocated by alloc_reply_buffer → libc::malloc.
    // Symmetric deallocation via libc::free preserves allocator pairing on
    // every platform regardless of Rust's GlobalAlloc configuration.
    unsafe { libc::free(ptr.cast()) };
}

// ── Wait (receiver side) ────────────────────────────────────────────────

/// Block until a reply is available, then return the value.
///
/// The caller owns the returned pointer and must free it with
/// [`hew_reply_payload_free`].
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
            return take_ready_reply(ch, None);
        }

        // Slow path: wait on condvar.
        let mut guard = (*ch).lock.lock_or_recover();
        while !(*ch).ready.load(Ordering::Acquire) {
            guard = (*ch).cond.wait_or_recover(guard);
        }
        let result = take_ready_reply(ch, None);
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
            return take_ready_reply(ch, Some(out_size));
        }

        // Slow path: wait on condvar.
        let mut guard = (*ch).lock.lock_or_recover();
        while !(*ch).ready.load(Ordering::Acquire) {
            guard = (*ch).cond.wait_or_recover(guard);
        }
        let result = take_ready_reply(ch, Some(out_size));
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
            return take_ready_reply(ch, None);
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

        let result = take_ready_reply(ch, None);
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
            // #1739: a non-null `value` at the final release is the
            // delivered-but-never-consumed leg — `take_ready_reply` nulls
            // `value` on consume, so reaching here with it set means no waiter
            // ever took the buffer. Run the reply type's registered destructor
            // on the byte-copied buffer to release any heap embedded in `R`
            // (String/Vec/HashMap/Closure/owned-field struct) BEFORE freeing
            // the buffer itself; without it that embedded heap leaks (and a
            // Closure reply's captures are dropped). `refs` has reached 0, so
            // no deliver/consume can race and the slot is read exactly once
            // here: the destructor runs exactly once. A null slot (a bit-copy
            // `R` with no embedded heap) keeps the prior behaviour — the buffer
            // free alone suffices.
            let drop_raw = (*ch).reply_drop_fn.load(Ordering::Acquire);
            if !drop_raw.is_null() {
                let drop_fn = std::mem::transmute::<*mut c_void, HewReplyDropFn>(drop_raw);
                drop_fn((*ch).value);
            }
            #[cfg(debug_assertions)]
            debug_untrack_libc_alloc((*ch).value.cast());
            libc::free((*ch).value);
        }
        let await_cancel = (*ch).await_cancel.load(Ordering::Acquire);
        if !await_cancel.is_null() {
            hew_await_cancel_free(await_cancel);
        }
        #[cfg(test)]
        ACTIVE_CHANNELS.fetch_sub(1, Ordering::Relaxed);
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

/// Return the attached suspended-await status for a reply channel.
///
/// When no common registration is attached, this reports the legacy channel
/// state (`Completed` if ready, `Cancelled` if tombstoned, otherwise `Pending`).
///
/// # Safety
///
/// `ch` must be null or a live reply channel reference.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_await_status(ch: *mut HewReplyChannel) -> i32 {
    if ch.is_null() {
        return AwaitCancelStatus::Cancelled as i32;
    }
    // SAFETY: caller holds a live channel reference.
    let await_cancel = unsafe { (*ch).await_cancel.load(Ordering::Acquire) };
    if !await_cancel.is_null() {
        // SAFETY: the channel holds a retained registration reference.
        return unsafe { hew_await_cancel_status(await_cancel) };
    }
    // SAFETY: caller holds a live channel reference.
    unsafe {
        if (*ch).ready.load(Ordering::Acquire) {
            AwaitCancelStatus::Completed as i32
        } else if (*ch).cancelled.load(Ordering::Acquire) {
            AwaitCancelStatus::Cancelled as i32
        } else {
            AwaitCancelStatus::Pending as i32
        }
    }
}

// ── Select ─────────────────────────────────────────────────────────────

/// Return `true` if a reply has been deposited on `ch`.
///
/// Used by ask callers to distinguish a timed-out wait (channel not yet
/// ready) from a legitimate null reply (channel ready, value is null).
/// The caller must still hold a reference to `ch` when calling this.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`].
pub(crate) unsafe fn hew_reply_channel_is_ready(ch: *mut HewReplyChannel) -> bool {
    if ch.is_null() {
        return false;
    }
    // SAFETY: caller holds a reference that keeps `ch` alive.
    unsafe { (*ch).ready.load(Ordering::Acquire) }
}

/// Return `1` if the channel was marked orphaned by mailbox teardown
/// ([`hew_reply_channel_retire_orphaned_ask_sender_ref`]), `0` otherwise.
///
/// The orphaned flag distinguishes a mailbox-teardown null reply (the actor's
/// mailbox was torn down before the handler called `hew_reply`) from a
/// legitimate null reply the handler deposited. The blocking ask path reads it
/// directly to bind [`AskError::OrphanedAsk`]; the codegen suspending-ask
/// resume edge calls this FFI to bind the SAME discriminator instead of the
/// TLS last-error slot (which never carries the channel-local orphaned fact).
/// Must be called on the caller-side reference BEFORE it is released.
///
/// # Safety
///
/// `ch` must be a valid pointer returned by [`hew_reply_channel_new`] that the
/// caller still holds a reference to.
#[no_mangle]
pub unsafe extern "C" fn hew_reply_channel_is_orphaned(ch: *mut HewReplyChannel) -> i32 {
    if ch.is_null() {
        return 0;
    }
    // SAFETY: caller holds a reference that keeps `ch` alive.
    i32::from(unsafe { (*ch).orphaned.load(Ordering::Acquire) })
}

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
pub(crate) fn active_channel_count() -> usize {
    ACTIVE_CHANNELS.load(Ordering::Relaxed)
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
pub(crate) unsafe fn hew_reply_channel_allocation_failed_for_test(
    ch: *mut HewReplyChannel,
) -> bool {
    if ch.is_null() {
        return false;
    }

    // SAFETY: test callers pass a valid channel pointer and only read the
    // allocation-failure flag; no ownership changes occur here.
    unsafe { (*ch).allocation_failed.load(Ordering::Acquire) }
}

#[cfg(test)]
mod tests {
    use super::*;

    // All tests that create or free a reply channel must hold
    // `crate::runtime_test_guard()` for their entire body.  The
    // `ACTIVE_CHANNELS` counter is process-wide; any concurrent
    // allocation or free in another test (including actor::tests) shifts
    // the counter and corrupts delta measurements.  `runtime_test_guard`
    // is the crate-wide serialisation primitive — shared by actor::tests
    // — so holding it here excludes all other allocating tests across
    // every module.  The only exempt test is
    // `select_first_null_returns_negative_one`, which never allocates.

    #[test]
    fn cancel_then_owner_release_leaves_sender_reference_for_late_reply() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();

        // SAFETY: ch is a valid channel pointer; FFI calls test ref-counting behaviour.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_cancel(ch);

            assert_eq!((*ch).refs.load(Ordering::Acquire), 2);

            hew_reply_channel_free(ch);
            assert_eq!((*ch).refs.load(Ordering::Acquire), 1);

            let _ = hew_reply(ch, ptr::null_mut(), 0);
        }
    }

    #[test]
    fn await_cancel_cleanup_tombstones_reply_channel_exactly_once() {
        static CLEANUPS: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn cleanup(source: *mut c_void, status: i32) {
            let _ = status;
            CLEANUPS.fetch_add(1, Ordering::AcqRel);
            // SAFETY: the registration source is the live reply channel for this test.
            unsafe {
                hew_reply_channel_cancel_cleanup(source, AwaitCancelStatus::Cancelled as i32);
            };
        }

        let _guard = crate::runtime_test_guard();
        CLEANUPS.store(0, Ordering::Release);
        let pre = active_channel_count();
        let ch = hew_reply_channel_new();
        // SAFETY: this test owns the newly-created channel and registration until
        // the final frees below.
        unsafe {
            hew_reply_channel_retain(ch);
            let reg = crate::await_cancel::hew_await_cancel_new(
                ptr::null_mut(),
                Some(cleanup),
                ch.cast(),
            );
            hew_reply_channel_set_await_cancel(ch, reg);

            assert_eq!(
                crate::await_cancel::hew_await_cancel_cancel(
                    reg,
                    AwaitCancelStatus::Cancelled as i32,
                    0,
                ),
                1
            );
            assert_eq!(
                crate::await_cancel::hew_await_cancel_cancel(
                    reg,
                    AwaitCancelStatus::TimedOut as i32,
                    0,
                ),
                0
            );
            assert_eq!(CLEANUPS.load(Ordering::Acquire), 1);
            assert_eq!(
                hew_reply_channel_await_status(ch),
                AwaitCancelStatus::Cancelled as i32
            );

            hew_reply_channel_free(ch);
            let _ = hew_reply(ch, ptr::null_mut(), 0);
            hew_await_cancel_free(reg);
        }
        assert_eq!(
            active_channel_count(),
            pre,
            "cancelled suspended ask must release the waiter and late sender exactly once"
        );
    }

    // NEW-6b regression: exactly-one-waker under the deadline-vs-reply split-brain
    // race. The reviewer's P1 was the loser-stale-wake: a reply that passes the
    // pre-publish `cancelled` check just before the timer's cleanup, then reaches
    // `publish_reply_from_sender_ref` and LOSES the completion arbiter, used to
    // re-enqueue the caller UNCONDITIONALLY. That spurious wake survived as a
    // `pending_wake` into the caller's NEXT park and could resume a SECOND await
    // before its own source was ready. The fix gates the parked-continuation wake
    // on winning the arbiter (or carrying no registration). This test drives the
    // exact split-brain ordering deterministically — no timing, no flake — by
    // terminalising the registration as `TimedOut` (the timer won) and then
    // calling `publish_reply_from_sender_ref` directly (the reply that already
    // passed the pre-publish check). The observable is the caller's
    // `pending_wake`: the loser edge must leave it FALSE (so a following park /
    // second await is not spuriously resumed), while a winning reply and a
    // no-registration (plain ask) reply must each set it TRUE (the legitimate,
    // sole waker still fires).
    #[test]
    fn loser_reply_does_not_wake_caller_winner_and_plain_ask_do() {
        use crate::internal::types::{ContTag, HewActorState};
        use std::sync::atomic::{AtomicI32, AtomicU64};

        // A tracked caller actor whose state is `Running` with a null
        // `suspended_cont`: in that shape `enqueue_resume` records the wake via
        // `mark_pending_wake` and returns without touching the global run queue,
        // so `take_pending_wake` is a clean, isolated observation of whether the
        // reply path woke the caller.
        fn caller_actor(id: u64) -> Box<HewActor> {
            Box::new(HewActor {
                sched_link_next: AtomicPtr::new(ptr::null_mut()),
                id,
                state: ptr::null_mut(),
                state_size: 0,
                dispatch: None,
                mailbox: ptr::null_mut(),
                actor_state: AtomicI32::new(HewActorState::Running as i32),
                budget: AtomicI32::new(0),
                init_state: ptr::null_mut(),
                init_state_size: 0,
                coalesce_key_fn: None,
                terminate_fn: None,
                state_drop_fn: None,
                state_clone_fn: None,
                terminate_called: AtomicBool::new(false),
                terminate_finished: AtomicBool::new(false),
                error_code: AtomicI32::new(0),
                supervisor: ptr::null_mut(),
                supervisor_child_index: 0,
                priority: AtomicI32::new(1),
                reductions: AtomicI32::new(0),
                idle_count: AtomicI32::new(0),
                hibernation_threshold: AtomicI32::new(0),
                hibernating: AtomicI32::new(0),
                prof_messages_processed: AtomicU64::new(0),
                prof_processing_time_ns: AtomicU64::new(0),
                arena: ptr::null_mut(),
                suspended_cont: AtomicPtr::new(ptr::null_mut()),
                cont_tag: AtomicI32::new(ContTag::Empty as i32),
                pending_wake: AtomicBool::new(false),
                suspended_reply_channel: AtomicPtr::new(ptr::null_mut()),
                suspended_cancel_token: AtomicPtr::new(ptr::null_mut()),
            })
        }

        unsafe extern "C" fn cleanup(source: *mut c_void, status: i32) {
            // SAFETY: the registration source is the live reply channel for this test.
            unsafe { hew_reply_channel_cancel_cleanup(source, status) };
        }

        let _guard = crate::runtime_test_guard();
        let pre = active_channel_count();

        let mut actor = caller_actor(0x00C0_FFEE);
        let actor_ptr: *mut HewActor = &raw mut *actor;
        // SAFETY: `actor` outlives the registry tracking (untracked before drop).
        unsafe { crate::lifetime::live_actors::track_actor(actor_ptr) };

        // ── Case 1: the reply LOSES the arbiter (timer already won). ──────────
        // SAFETY: this test owns the channel + registration for the whole case.
        unsafe {
            let ch = hew_reply_channel_new();
            hew_reply_channel_set_parked_waiter(ch, actor_ptr);
            let reg =
                crate::await_cancel::hew_await_cancel_new(actor_ptr, Some(cleanup), ch.cast());
            hew_reply_channel_set_await_cancel(ch, reg);

            // The deadline timer wins first: terminalise the registration as
            // TimedOut (wake_actor=0 — we are isolating the REPLY edge, so the
            // timer's own wake is suppressed here).
            assert_eq!(
                crate::await_cancel::hew_await_cancel_cancel(
                    reg,
                    AwaitCancelStatus::TimedOut as i32,
                    0,
                ),
                1,
                "timer must win the one-shot arbiter"
            );
            actor.pending_wake.store(false, Ordering::Release);

            // The reply that already passed the pre-publish cancelled check now
            // reaches publish and LOSES (`hew_await_cancel_complete` returns 0).
            publish_reply_from_sender_ref(ch, ptr::null_mut(), 0);

            crate::await_cancel::hew_await_cancel_free(reg);
        }
        assert!(
            !crate::coro_exec::take_pending_wake(&actor),
            "a reply that lost the deadline arbiter must NOT wake the caller — \
             a stale wake would spuriously resume the caller's next park"
        );

        // ── Case 2: the reply WINS the arbiter (no timer). ───────────────────
        // SAFETY: this test owns the channel + registration for the whole case.
        unsafe {
            let ch = hew_reply_channel_new();
            hew_reply_channel_set_parked_waiter(ch, actor_ptr);
            let reg =
                crate::await_cancel::hew_await_cancel_new(actor_ptr, Some(cleanup), ch.cast());
            hew_reply_channel_set_await_cancel(ch, reg);
            actor.pending_wake.store(false, Ordering::Release);

            publish_reply_from_sender_ref(ch, ptr::null_mut(), 0);

            crate::await_cancel::hew_await_cancel_free(reg);
        }
        assert!(
            crate::coro_exec::take_pending_wake(&actor),
            "a reply that won the deadline arbiter MUST wake the caller"
        );

        // ── Case 3: a plain ask with NO deadline registration still wakes. ───
        // SAFETY: this test owns the channel for the whole case.
        unsafe {
            let ch = hew_reply_channel_new();
            hew_reply_channel_set_parked_waiter(ch, actor_ptr);
            actor.pending_wake.store(false, Ordering::Release);

            publish_reply_from_sender_ref(ch, ptr::null_mut(), 0);
        }
        assert!(
            crate::coro_exec::take_pending_wake(&actor),
            "a plain ask (no deadline registration) must always wake the caller"
        );

        crate::lifetime::live_actors::untrack_actor(actor_ptr);
        assert_eq!(
            active_channel_count(),
            pre,
            "every channel allocated by this test must be freed (no leak)"
        );
    }

    #[test]
    fn loser_cleanup_sequence_does_not_leak_channel() {
        // Pins the ABI sequence codegen will emit when a select{}
        // actor-ask arm loses: allocate the reply channel, then
        // (because the ask either failed to dispatch or the arm lost
        // the race) cancel + free without ever waiting for a reply.
        // The pair must be a net-zero on `ACTIVE_CHANNELS`: the
        // single `new` increments it by 1, and the matching `free`
        // must decrement it by 1 once the last reference drops.
        //
        // COUNTER_LOCK serialises the measurement window. Without it,
        // a concurrent test allocating or freeing a channel between the
        // pre/post observations can shift the counter and produce a
        // spurious failure.
        let _guard = crate::runtime_test_guard();
        let pre_new = active_channel_count();

        let ch = hew_reply_channel_new();
        assert!(!ch.is_null(), "hew_reply_channel_new must not return null");
        let post_new = active_channel_count();
        assert!(
            post_new > pre_new,
            "hew_reply_channel_new must increment ACTIVE_CHANNELS \
             (pre={pre_new}, post={post_new})"
        );

        // SAFETY: ch is a valid channel pointer produced by
        // hew_reply_channel_new immediately above. No sender was
        // retained, so `cancel` is a no-op on the refcount and `free`
        // drops the only outstanding reference.
        unsafe {
            hew_reply_channel_cancel(ch);
            hew_reply_channel_free(ch);
        }

        let post_free = active_channel_count();
        assert!(
            post_free < post_new,
            "loser-cleanup sequence (cancel + free) must decrement \
             ACTIVE_CHANNELS (post_new={post_new}, post_free={post_free})"
        );
    }

    #[test]
    fn reply_then_cancel_preserves_ready_value_until_owner_releases() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();
        let value = 42_i32;

        // SAFETY: ch is a valid channel pointer; value address is valid for the block scope.
        unsafe {
            hew_reply_channel_retain(ch);
            let _ = hew_reply(
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
    fn signal_ready_marks_channel_ready_without_payload() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();

        // SAFETY: the test retains an observer-side reference that
        // `hew_reply_channel_signal_ready` consumes, leaving the original
        // waiter reference live for select/wait/free.
        unsafe {
            hew_reply_channel_retain(ch);
            hew_reply_channel_signal_ready(ch.cast());

            assert!(hew_reply_channel_is_ready_for_test(ch));
            let mut channels = [ch];
            assert_eq!(hew_select_first(channels.as_mut_ptr(), 1, 0), 0);
            assert!(
                hew_reply_wait(ch).is_null(),
                "readiness proxy must not fabricate a reply payload"
            );
            hew_reply_channel_free(ch);
        }
    }

    #[test]
    fn is_orphaned_reports_mailbox_teardown_flag() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();

        // SAFETY: ch is a fresh, live channel for this scope.
        unsafe {
            // A fresh channel is not orphaned.
            assert_eq!(
                hew_reply_channel_is_orphaned(ch),
                0,
                "a fresh reply channel must not report orphaned"
            );

            // Mailbox-teardown sets the orphaned flag before publishing the null
            // sentinel; the FFI must surface it for the suspending-ask resume edge
            // to bind AskError::OrphanedAsk (matching the blocking path).
            hew_reply_channel_retain(ch);
            hew_reply_channel_retire_orphaned_ask_sender_ref(ch);
            assert_eq!(
                hew_reply_channel_is_orphaned(ch),
                1,
                "mailbox teardown must make is_orphaned report 1"
            );

            hew_reply_channel_free(ch);
        }

        // A null channel is treated as not-orphaned (fail-safe, no deref).
        // SAFETY: passing null is an explicit no-op contract.
        assert_eq!(unsafe { hew_reply_channel_is_orphaned(ptr::null_mut()) }, 0);
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

    #[test]
    fn task_completion_observer_can_signal_select_readiness_proxy() {
        let _guard = crate::runtime_test_guard();

        // SAFETY: the test owns all scope/task/channel pointers exclusively.
        unsafe {
            let scope = crate::task_scope::hew_task_scope_new();
            let task = crate::task_scope::hew_task_new();
            crate::task_scope::hew_task_scope_spawn(scope, task);
            let ch = hew_reply_channel_new();

            hew_reply_channel_retain(ch);
            assert_eq!(
                crate::task_scope::hew_task_completion_observe(
                    scope,
                    task,
                    Some(hew_reply_channel_signal_ready),
                    ch.cast(),
                ),
                0
            );

            let mut channels = [ch];
            assert_eq!(hew_select_first(channels.as_mut_ptr(), 1, 0), -1);
            crate::task_scope::hew_task_scope_complete_task(scope, task);
            assert_eq!(hew_select_first(channels.as_mut_ptr(), 1, 0), 0);
            assert!(hew_reply_wait(ch).is_null());

            hew_reply_channel_free(ch);
            crate::task_scope::hew_task_scope_destroy(scope);
        }
    }

    #[test]
    fn send_recv_roundtrip() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();
        let payload = 99_i64;

        // SAFETY: ch is a valid channel pointer; payload address is valid for the block scope.
        unsafe {
            // Sender retains so the channel survives the reply call.
            hew_reply_channel_retain(ch);
            let _ = hew_reply(
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
        let _guard = crate::runtime_test_guard();
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
            let _ = hew_reply(ptr::null_mut(), ptr::null_mut(), 0);
            assert!(hew_reply_wait(ptr::null_mut()).is_null());
            assert!(hew_reply_wait_timeout(ptr::null_mut(), 100).is_null());
            hew_reply_channel_retain(ptr::null_mut());
            hew_reply_channel_free(ptr::null_mut());
            hew_reply_channel_cancel(ptr::null_mut());
            hew_reply_channel_signal_ready(ptr::null_mut());
        }
    }

    #[test]
    fn threaded_send_recv() {
        let _guard = crate::runtime_test_guard();
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
                let _ = hew_reply(
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
    fn reply_wait_surfaces_copy_oom() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();
        let ch = hew_reply_channel_new();
        let payload = 55_i32;

        // SAFETY: test exercises the forced-allocation-failure path on a live channel.
        unsafe {
            hew_reply_channel_retain(ch);
            FORCE_REPLY_ALLOC_FAILURE.store(true, Ordering::Release);
            let delivered = hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            assert!(
                !delivered,
                "hew_reply must report not-delivered when the channel buffer alloc fails"
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

    /// Regression: `hew_reply` must report `false` when the channel has
    /// already been cancelled, so codegen can free deep-cloned owned
    /// payloads (e.g. `strdup`'d Strings, `hew_vec_clone`'d Vecs) on the
    /// ask-cancel and ask-timeout paths instead of leaking them. This is
    /// the contract `ReceiveOpLowering` relies on to keep Drop Safety on
    /// the cancel/timeout legs of the ask seam.
    #[test]
    fn hew_reply_returns_false_when_channel_was_cancelled() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();
        let payload = 42_i32;

        // SAFETY: `ch` is a live reply channel reference; we retain so the
        // sender-side call sees the cancellation rather than freeing the
        // channel out from under the waiter.
        unsafe {
            hew_reply_channel_retain(ch); // sender's reference
            hew_reply_channel_cancel(ch);

            let delivered = hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            assert!(
                !delivered,
                "hew_reply must report not-delivered after the channel is cancelled \
                 so the caller can free any deep-cloned owned payload"
            );

            hew_reply_channel_free(ch);
        }
    }

    /// Regression: when delivery succeeds, `hew_reply` must report `true`
    /// so codegen knows the waiter took ownership of the payload bytes
    /// and the deep-cloned heap object lifetime transfers to the caller.
    #[test]
    fn hew_reply_returns_true_on_successful_delivery() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();
        let payload = 99_i32;

        // SAFETY: `ch` is a live reply channel; retain for the sender side.
        unsafe {
            hew_reply_channel_retain(ch);
            let delivered = hew_reply(
                ch,
                (&raw const payload).cast_mut().cast(),
                std::mem::size_of::<i32>(),
            );
            assert!(delivered, "successful reply must report delivered=true");

            let result = hew_reply_wait(ch).cast::<i32>();
            assert!(!result.is_null());
            assert_eq!(*result, 99);
            libc::free(result.cast());
            hew_reply_channel_free(ch);
        }
    }

    /// Regression: a deep-cloned owned reply (modelling codegen's
    /// `strdup(state_string)` clone) must not leak when the waiter has
    /// already cancelled. This pins the **no-destructor** contract: with no
    /// `reply_drop_fn` registered, `hew_reply` returns `false` and the caller
    /// (here standing in for a bit-copy `R` / the legacy manual-reclaim path)
    /// frees the clone itself, and no leak remains. The dtor-registered analog
    /// — where the channel reaps the clone on the cancel leg and the caller
    /// must NOT free it — is
    /// `cancelled_before_delivery_runs_registered_destructor_on_producer_value`.
    #[test]
    fn cancelled_channel_lets_caller_reclaim_deep_cloned_string_payload() {
        let _guard = crate::runtime_test_guard();
        let ch = hew_reply_channel_new();
        // Allocate a heap "state-owned" string and the cloned reply, the
        // way codegen does immediately before invoking hew_reply.
        let original = std::ffi::CString::new("owned-state").unwrap();

        // SAFETY: cloned points at a libc-allocated buffer that we own and
        // either pass to hew_reply (success → waiter frees) or free on the
        // `delivered=false` cancel path.
        unsafe {
            let cloned: *mut libc::c_char = libc::strdup(original.as_ptr());
            assert!(!cloned.is_null(), "strdup must succeed");

            hew_reply_channel_retain(ch); // sender's reference
            hew_reply_channel_cancel(ch); // waiter cancels before the handler replies

            let mut cloned_ptr: *mut libc::c_char = cloned;
            let delivered = hew_reply(
                ch,
                (&raw mut cloned_ptr).cast(),
                std::mem::size_of::<*mut libc::c_char>(),
            );
            assert!(
                !delivered,
                "cancelled channel must report not-delivered so caller can reclaim its clone"
            );

            // No `reply_drop_fn` is registered on this channel, so the cancel
            // leg leaves `cloned_ptr` to the caller: this is the bit-copy `R` /
            // legacy manual-reclaim contract. Free the deep clone ourselves.
            // (When codegen registers a destructor for an owned `R`, the channel
            // reaps it on this leg instead — see the dtor-registered test — and
            // the caller must NOT also free it.) ASan/leak-checkers will flag a
            // leak if the contract is wrong.
            libc::free(cloned_ptr.cast());

            hew_reply_channel_free(ch);
        }
    }

    /// Regression: matching the cancel path, OOM in the channel buffer
    /// alloc must report `false` so the caller can reclaim its clone. Pins the
    /// **no-destructor** contract (no `reply_drop_fn` registered → the caller
    /// frees its own clone). The dtor-registered analog — channel reaps on the
    /// OOM leg — is `alloc_failure_runs_registered_destructor_on_producer_value`.
    #[test]
    fn alloc_failure_lets_caller_reclaim_deep_cloned_payload() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();
        let ch = hew_reply_channel_new();
        let original = std::ffi::CString::new("owned-state").unwrap();

        // SAFETY: cloned is libc-allocated; we either hand it off via
        // hew_reply (success path) or free it on the alloc-fail path.
        unsafe {
            let cloned: *mut libc::c_char = libc::strdup(original.as_ptr());
            assert!(!cloned.is_null(), "strdup must succeed");

            hew_reply_channel_retain(ch);
            FORCE_REPLY_ALLOC_FAILURE.store(true, Ordering::Release);

            let mut cloned_ptr: *mut libc::c_char = cloned;
            let delivered = hew_reply(
                ch,
                (&raw mut cloned_ptr).cast(),
                std::mem::size_of::<*mut libc::c_char>(),
            );
            assert!(
                !delivered,
                "alloc-fail must report not-delivered so caller can reclaim its clone"
            );

            libc::free(cloned_ptr.cast());

            // Drain the alloc-fail flag via reply_wait so other tests see a
            // clean error slot.
            let result = hew_reply_wait(ch);
            assert!(result.is_null());
            hew_reply_channel_free(ch);
        }
    }

    // ── #1739: delivered-but-never-consumed owned-reply typed destructor ──────

    /// Counts invocations of [`test_reply_string_dtor`] so the exactly-once
    /// contract is observable without a sanitizer (the ASan/LSan leak is the
    /// second barrel). Serialised with the rest of the module by
    /// `runtime_test_guard`, so a plain reset-at-start is race-free.
    static REPLY_DTOR_CALLS: AtomicUsize = AtomicUsize::new(0);

    /// Test reply-type drop thunk modelling codegen's destructor for a `string`
    /// reply: the channel buffer is a byte-copy of the reply value — here a
    /// single embedded `*mut c_char` (a Hew `string` is a flat heap pointer).
    /// Release that embedded heap (mirroring `hew_string_drop`) and count the
    /// call. This is the `void(*)(void*)` shape `hew_reply_channel_set_reply_drop_fn`
    /// registers.
    unsafe extern "C" fn test_reply_string_dtor(buf: *mut c_void) {
        REPLY_DTOR_CALLS.fetch_add(1, Ordering::AcqRel);
        if buf.is_null() {
            return;
        }
        // SAFETY: `buf` is the channel's copied reply buffer holding one
        // `*mut c_char` (the delivered reply value). Load and free the embedded
        // heap the byte-copy aliased into the buffer.
        unsafe {
            let embedded = *(buf.cast::<*mut libc::c_char>());
            if !embedded.is_null() {
                libc::free(embedded.cast());
            }
        }
    }

    /// Allocate a heap "owned reply" and deposit its flat pointer value into
    /// `ch` via `hew_reply` (byte-copied into the channel buffer, aliasing the
    /// embedded heap into `value`), the way an ask handler returning `string`
    /// does. Returns once delivered. The channel keeps the only live alias to
    /// the embedded heap.
    unsafe fn deliver_owned_string_reply(ch: *mut HewReplyChannel, payload: &str) {
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

    /// Regression (#1739): a delivered owned reply that is NEVER consumed (the
    /// waiter timed out / cancelled / shut down before reaching
    /// `hew_reply_wait`) must run its registered typed destructor on the channel
    /// teardown leg, reclaiming the embedded heap the byte-copy aliased into
    /// `value`. `take_ready_reply` nulls `value` only on consume, so a non-null
    /// `value` at the final `refs` 1→0 free is exactly the never-consumed leg.
    ///
    /// Pre-fix the destructor is never invoked: the embedded strdup leaks
    /// (ASan/LSan) and `REPLY_DTOR_CALLS` stays 0 (this assertion fails). The runtime
    /// destructor commit runs it exactly once on the non-null-`value` free leg.
    #[test]
    fn delivered_owned_reply_never_consumed_runs_typed_destructor() {
        let _guard = crate::runtime_test_guard();
        REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: `ch` is live; we model the codegen ask-caller setup (register
        // the reply-type drop thunk before submit) and a handler delivering a
        // heap reply the waiter never consumes.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            deliver_owned_string_reply(ch, "owned-reply-payload");

            // Never consume: no hew_reply_wait. Tear the channel down directly —
            // the timeout/cancel/shutdown leg. `value` is still non-null.
            hew_reply_channel_free(ch);

            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "a delivered-but-never-consumed owned reply must run its typed \
                 destructor exactly once on the channel teardown leg (#1739)"
            );
        }
    }

    /// Exactly-once boundary: the CONSUMED leg must NOT run the channel
    /// destructor. `hew_reply_wait` (`take_ready_reply`) nulls `value`,
    /// transferring buffer ownership to the waiter, whose scope-exit drop
    /// releases the embedded heap; running the channel destructor too would
    /// double-free. Green at every stage (the run-leg is gated on non-null
    /// `value`). Pairs with the never-consumed test to pin the XOR.
    #[test]
    fn consumed_owned_reply_does_not_run_channel_destructor() {
        let _guard = crate::runtime_test_guard();
        REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: live channel; deliver then consume normally.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            deliver_owned_string_reply(ch, "consumed-reply");

            // Waiter consumes: take the buffer (value nulled), then — as the
            // waiter scope-exit drop does — release the embedded heap and free
            // the copied buffer.
            let buf = hew_reply_wait(ch);
            assert!(!buf.is_null());
            let embedded_back = *(buf.cast::<*mut libc::c_char>());
            assert!(!embedded_back.is_null());
            libc::free(embedded_back.cast()); // waiter's scope-exit drop of `R`
            hew_reply_payload_free(buf.cast(), 0); // free the copied buffer

            hew_reply_channel_free(ch);

            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                0,
                "consumed reply must NOT run the channel destructor — the waiter \
                 owns the buffer once take_ready_reply nulls value"
            );
        }
    }

    /// Teardown routing: a reply delivered and THEN cancelled (the select-loser
    /// / timeout race where the reply lands just before the waiter abandons the
    /// channel) still runs its typed destructor on the free leg.
    /// `hew_reply_channel_cancel` only sets the flag; the non-null `value`
    /// reaches the final free, which is the single site that reclaims it. Also
    /// covers double-release idempotence: the sender ref is released inside
    /// `hew_reply`, the waiter ref here — the destructor runs exactly once.
    #[test]
    fn delivered_then_cancelled_reply_runs_destructor_once_on_free() {
        let _guard = crate::runtime_test_guard();
        REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: live channel; deliver, then cancel after the reply landed.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference
            deliver_owned_string_reply(ch, "delivered-then-cancelled");

            // Waiter abandons the channel AFTER the reply already landed.
            hew_reply_channel_cancel(ch);
            hew_reply_channel_free(ch);

            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "a delivered-then-cancelled reply must still run its destructor \
                 exactly once on the final release"
            );
        }
    }

    /// Regression (#1739): a reply CANCELLED BEFORE DELIVERY (the waiter
    /// abandoned the channel before the handler called `hew_reply`) must run
    /// the registered destructor on the producer's `value` — the only live copy
    /// of the reply, since nothing was ever published into the channel buffer.
    ///
    /// This is the cancel analog of the never-consumed leg, and the path the
    /// real declared-actor dispatch trampoline depends on: it ignores
    /// `hew_reply`'s `false` return, so unless the channel reaps `value` here
    /// the producer's owned reply leaks. With a destructor registered the caller
    /// must NOT free `value` — doing so would double-free (`ASan` would abort).
    #[test]
    fn cancelled_before_delivery_runs_registered_destructor_on_producer_value() {
        let _guard = crate::runtime_test_guard();
        REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: `ch` is live; we model the codegen ask-caller setup (register
        // the reply-type drop thunk) and a producer that replies an owned heap
        // value AFTER the waiter cancelled.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference

            let original = std::ffi::CString::new("cancel-before-delivery").unwrap();
            let embedded: *mut libc::c_char = libc::strdup(original.as_ptr());
            assert!(!embedded.is_null(), "strdup must succeed");

            hew_reply_channel_cancel(ch); // waiter abandons BEFORE the reply

            let mut reply_value: *mut libc::c_char = embedded;
            let delivered = hew_reply(
                ch,
                (&raw mut reply_value).cast(),
                std::mem::size_of::<*mut libc::c_char>(),
            );
            assert!(
                !delivered,
                "cancelled-before-delivery reply must report not-delivered"
            );

            // Do NOT free `reply_value`: the channel's registered destructor
            // reaped its embedded heap on the cancel leg. Freeing here would
            // double-free.
            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "cancelled-before-delivery owned reply must run its registered \
                 destructor exactly once on the cancel leg (#1739)"
            );

            // The cancel leg released the sender ref (refs 2→1); release the
            // waiter ref to free the channel. Nothing was published, so `value`
            // is null and the never-consumed leg must NOT re-run the destructor.
            hew_reply_channel_free(ch);
            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "the final channel free must not double-run the destructor \
                 (nothing was published on the cancel leg)"
            );
        }
    }

    /// Regression (#1739): the OOM leg mirrors the cancel leg. When the channel
    /// buffer allocation fails, the reply is never published, so the producer's
    /// `value` is the only live copy. The registered destructor reclaims its
    /// embedded heap; the caller must NOT also free it.
    #[test]
    fn alloc_failure_runs_registered_destructor_on_producer_value() {
        let _guard = crate::runtime_test_guard();
        crate::hew_clear_error();
        REPLY_DTOR_CALLS.store(0, Ordering::Release);
        let ch = hew_reply_channel_new();

        // SAFETY: `ch` is live; force the per-reply buffer alloc to fail so
        // `hew_reply` takes the OOM leg with a destructor registered.
        unsafe {
            hew_reply_channel_set_reply_drop_fn(ch, Some(test_reply_string_dtor));
            hew_reply_channel_retain(ch); // sender's reference

            let original = std::ffi::CString::new("oom-before-delivery").unwrap();
            let embedded: *mut libc::c_char = libc::strdup(original.as_ptr());
            assert!(!embedded.is_null(), "strdup must succeed");

            FORCE_REPLY_ALLOC_FAILURE.store(true, Ordering::Release);

            let mut reply_value: *mut libc::c_char = embedded;
            let delivered = hew_reply(
                ch,
                (&raw mut reply_value).cast(),
                std::mem::size_of::<*mut libc::c_char>(),
            );
            assert!(!delivered, "alloc-fail reply must report not-delivered");

            assert_eq!(
                REPLY_DTOR_CALLS.load(Ordering::Acquire),
                1,
                "alloc-fail owned reply must run its registered destructor \
                 exactly once on the OOM leg (#1739)"
            );

            // Drain the alloc-fail flag so other tests see a clean error slot.
            let result = hew_reply_wait(ch);
            assert!(result.is_null());
            hew_reply_channel_free(ch);
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
        let _guard = crate::runtime_test_guard();
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
                    let _ = hew_reply(
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
        let _guard = crate::runtime_test_guard();
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
                        let _ = hew_reply(
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
        let _guard = crate::runtime_test_guard();

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
                                let _ = hew_reply(
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
