//! Hew runtime: typed MPSC (multi-producer, single-consumer) channels.
//!
//! Provides a bounded, blocking channel for passing string messages between
//! threads. Multiple senders can be cloned from a single sender handle;
//! exactly one receiver drains the channel.
//!
//! ## ABI conventions
//!
//! All functions use `#[no_mangle] extern "C"` with opaque pointers.
//! Received strings are malloc-allocated, NUL-terminated, and owned by the
//! caller (must be freed with `hew_string_drop`).
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::ffi::{c_char, c_void, CStr};
use std::ptr;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use crate::channel_common::{bytes_to_cstr, free_channel_pair};
use crate::channel_core::ChannelCore;

// ── Handle types ────────────────────────────────────────────────────────────

/// Opaque sender handle. Cloneable for multi-producer use.
///
/// Multiple senders share one [`ChannelCore`] plus a `senders` refcount; when
/// the last sender drops, the core's sink is closed so a parked / blocked
/// consumer observes channel closure (`None`). The MPSC close semantics that
/// `std::sync::mpsc` provided implicitly (sender drop → disconnect) are
/// reproduced here over the suspendable `ChannelCore` substrate so that
/// `await rx.recv()` can park a coroutine continuation instead of blocking a
/// scheduler worker (NEW-4).
#[derive(Debug)]
pub struct HewChannelSender {
    core: Arc<ChannelCore>,
    /// Live-sender refcount shared by every clone of this sender. The last
    /// sender to drop (count 1 → 0) closes the core's sink.
    senders: Arc<AtomicUsize>,
}

impl Drop for HewChannelSender {
    fn drop(&mut self) {
        // The last surviving sender closing reproduces mpsc disconnect: the
        // core's sink closes so a parked / blocking consumer wakes with `None`.
        if self.senders.fetch_sub(1, Ordering::AcqRel) == 1 {
            self.core.close_sink();
        }
    }
}

/// Opaque receiver handle. Only one receiver per channel.
///
/// `pending_read` / `pending_state` back the `select{}` channel-recv arm
/// (NEW-4), mirroring `HewStream`'s poll registration: `hew_channel_poll`
/// parks a foreign thread that waits for readiness and fires the select
/// readiness callback; `hew_channel_cancel_pending_read` withdraws a losing
/// arm's registration.
#[derive(Debug)]
pub struct HewChannelReceiver {
    core: Arc<ChannelCore>,
    /// Active pending-read id, or 0 if none. The "at most one pending poll per
    /// receiver" invariant is enforced by CAS on this field.
    pending_read: AtomicU64,
    /// Shared cancel flag + park-state for the active poll, `None` when idle.
    pending_state: Mutex<Option<Arc<ChannelPoll>>>,
}

/// Park-thread state for a `select{}` channel-recv poll (NEW-4). Mirrors the
/// stream `ParkState` machine: `Pending → Done` when the poll thread fires the
/// readiness callback, `Pending → Cancelled` when a losing arm withdraws.
#[derive(Debug)]
struct ChannelPoll {
    cancelled: std::sync::atomic::AtomicBool,
    state: Mutex<ChannelPollState>,
    /// The retained observer reference handed to [`hew_channel_poll`] as
    /// `userdata` (a `HewReplyChannel*` for the `select{}` arm) and the
    /// function that releases exactly one of its references.
    ///
    /// EXACTLY ONE path consumes this retained reference:
    /// - the readiness callback firing on the `Pending → Done` transition (the
    ///   park thread invokes `callback(userdata)`, which consumes the ref), OR
    /// - this `release(observer)` on the `Pending → Cancelled` transition (a
    ///   losing/withdrawn arm — the callback will NOT fire).
    ///
    /// The `state` mutex serializes the `Pending → {Done, Cancelled}`
    /// transition, so the two paths are mutually exclusive: the ref is neither
    /// leaked (cancel now releases it) nor double-released.
    observer: usize,
    release: extern "C" fn(*mut c_void),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChannelPollState {
    Pending,
    Cancelled,
    Done,
}

/// Monotonic counter for channel poll ids; never reuses an id within a process.
static NEXT_CHANNEL_POLL_ID: AtomicU64 = AtomicU64::new(1);

/// Temporary pair returned by [`hew_channel_new`].
pub struct HewChannelPair {
    sender: *mut HewChannelSender,
    receiver: *mut HewChannelReceiver,
}

fn decode_i64_payload(payload: &[u8], context: &str) -> Result<i64, ()> {
    let bytes: [u8; 8] = payload.try_into().map_err(|_| {
        crate::set_last_error(format!(
            "{context}: expected 8-byte int payload, got {} bytes",
            payload.len()
        ));
    })?;
    Ok(i64::from_le_bytes(bytes))
}

impl std::fmt::Debug for HewChannelPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewChannelPair")
            .field("sender", &self.sender)
            .field("receiver", &self.receiver)
            .finish()
    }
}

// ── Constructor ─────────────────────────────────────────────────────────────

/// Create a bounded MPSC channel with the given capacity.
///
/// Returns a `HewChannelPair*` from which the sender and receiver must be
/// extracted with [`hew_channel_pair_sender`] / [`hew_channel_pair_receiver`],
/// then freed with [`hew_channel_pair_free`].
///
/// Returns null on invalid capacity (sets last error).
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_channel_pair_free`].
#[no_mangle]
pub extern "C" fn hew_channel_new(capacity: i64) -> *mut HewChannelPair {
    if capacity < 0 {
        crate::set_last_error(format!(
            "hew_channel_new: invalid capacity {capacity} (must be >= 0)"
        ));
        return ptr::null_mut();
    }
    let Ok(cap) = usize::try_from(capacity.max(1)) else {
        crate::set_last_error(format!(
            "hew_channel_new: capacity {capacity} exceeds platform maximum"
        ));
        return ptr::null_mut();
    };
    let core = Arc::new(ChannelCore::new(cap));
    let senders = Arc::new(AtomicUsize::new(1));

    let sender = Box::into_raw(Box::new(HewChannelSender {
        core: Arc::clone(&core),
        senders,
    })); // ALLOCATOR-PAIRING: GlobalAlloc
    let receiver = Box::into_raw(Box::new(HewChannelReceiver {
        core,
        pending_read: AtomicU64::new(0),
        pending_state: Mutex::new(None),
    })); // ALLOCATOR-PAIRING: GlobalAlloc

    Box::into_raw(Box::new(HewChannelPair { sender, receiver })) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Extract the sender from a channel pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
/// The sender must not be extracted more than once.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_pair_sender(
    pair: *mut HewChannelPair,
) -> *mut HewChannelSender {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: pair is valid per caller contract; extract then null the handle.
    let s = unsafe { (*pair).sender };
    // SAFETY: pair is valid per caller contract — nulling extracted handle.
    unsafe { (*pair).sender = ptr::null_mut() };
    s
}

/// Extract the receiver from a channel pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
/// The receiver must not be extracted more than once.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_pair_receiver(
    pair: *mut HewChannelPair,
) -> *mut HewChannelReceiver {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: pair is valid per caller contract; extract then null the handle.
    let r = unsafe { (*pair).receiver };
    // SAFETY: pair is valid per caller contract — nulling extracted handle.
    unsafe { (*pair).receiver = ptr::null_mut() };
    r
}

/// Free the channel pair struct. Any handles not yet extracted are dropped.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_channel_pair_free(pair: *mut HewChannelPair) {
    // SAFETY: caller guarantees `pair` came from `hew_channel_new`.
    unsafe { free_channel_pair(pair, |pair| (&mut pair.sender, &mut pair.receiver)) };
}

// ── Send ────────────────────────────────────────────────────────────────────

/// Send a NUL-terminated string through the channel. Blocks if the channel
/// is full (backpressure).
///
/// # Safety
///
/// `sender` must be a valid pointer. `data` must be a valid NUL-terminated
/// C string.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_send(sender: *mut HewChannelSender, data: *const c_char) {
    cabi_guard!(sender.is_null() || data.is_null());
    // SAFETY: data is a valid C string per caller contract.
    let s = unsafe { CStr::from_ptr(data) };
    let bytes = s.to_bytes().to_vec();
    // SAFETY: sender is valid per caller contract.
    unsafe { (*sender).core.blocking_send(bytes) };
}

/// Send a 64-bit integer through the channel. Blocks if the channel is full.
///
/// The integer is serialised as 8 little-endian bytes for the internal
/// `Vec<u8>` transport.
///
/// # Safety
///
/// `sender` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_send_int(sender: *mut HewChannelSender, value: i64) {
    cabi_guard!(sender.is_null());
    let bytes = value.to_le_bytes().to_vec();
    // SAFETY: sender is valid per caller contract.
    unsafe { (*sender).core.blocking_send(bytes) };
}

// ── Receive ─────────────────────────────────────────────────────────────────

/// Block until a message is available and return it as a malloc-allocated
/// NUL-terminated string. Returns NULL when the channel is closed (all
/// senders dropped), letting codegen wrap the result as `Option<String>`.
///
/// The caller must free the returned pointer with `hew_string_drop` when non-NULL.
///
/// # Safety
///
/// `receiver` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv(receiver: *mut HewChannelReceiver) -> *mut c_char {
    cabi_guard!(receiver.is_null(), ptr::null_mut());
    // SAFETY: receiver is valid per caller contract.
    match unsafe { (*receiver).core.blocking_recv() } {
        Some(item) => bytes_to_cstr(&item),
        // Channel closed — return NULL so codegen wraps as None.
        None => ptr::null_mut(),
    }
}

/// Try to receive a message without blocking.
///
/// Returns a malloc-allocated NUL-terminated string if a message was
/// available, or NULL if the channel is empty or closed. This lets the
/// caller distinguish "received empty string" (`Some("")`) from
/// "nothing available" (`None`).
///
/// The caller must free the returned pointer with `hew_string_drop` when non-NULL.
///
/// # Safety
///
/// `receiver` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_try_recv(receiver: *mut HewChannelReceiver) -> *mut c_char {
    cabi_guard!(receiver.is_null(), ptr::null_mut());
    // SAFETY: receiver is valid per caller contract.
    match unsafe { (*receiver).core.try_recv() } {
        Some(item) => bytes_to_cstr(&item),
        None => ptr::null_mut(),
    }
}

/// Block until an integer message is available.
///
/// Returns the integer value and sets `*out_valid` to 1 if a message was
/// received. Sets `*out_valid` to 0 and returns 0 when the channel is
/// closed (all senders dropped), letting codegen wrap as `Option<int>`.
///
/// # Safety
///
/// `receiver` and `out_valid` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv_int(
    receiver: *mut HewChannelReceiver,
    out_valid: *mut i32,
) -> i64 {
    cabi_guard!(receiver.is_null() || out_valid.is_null(), 0);
    // SAFETY: receiver is valid per caller contract.
    if let Some(item) = unsafe { (*receiver).core.blocking_recv() } {
        if let Ok(value) = decode_i64_payload(&item, "hew_channel_recv_int") {
            // SAFETY: out_valid is valid per caller contract.
            unsafe { *out_valid = 1 };
            return value;
        }
        // SAFETY: out_valid is valid per caller contract.
        unsafe { *out_valid = 0 };
        return 0;
    }
    // SAFETY: out_valid is valid per caller contract.
    unsafe { *out_valid = 0 };
    0
}

/// Try to receive an integer without blocking.
///
/// Returns the integer value and sets `*out_valid` to 1 if a message was
/// available. Sets `*out_valid` to 0 and returns 0 if the channel is
/// empty or closed.
///
/// # Safety
///
/// `receiver` and `out_valid` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_try_recv_int(
    receiver: *mut HewChannelReceiver,
    out_valid: *mut i32,
) -> i64 {
    cabi_guard!(receiver.is_null() || out_valid.is_null(), 0);
    // SAFETY: receiver is valid per caller contract.
    let Some(item) = (unsafe { (*receiver).core.try_recv() }) else {
        // SAFETY: out_valid is valid per caller contract.
        unsafe { *out_valid = 0 };
        return 0;
    };
    if let Ok(value) = decode_i64_payload(&item, "hew_channel_try_recv_int") {
        // SAFETY: out_valid is valid per caller contract.
        unsafe { *out_valid = 1 };
        value
    } else {
        // SAFETY: out_valid is valid per caller contract.
        unsafe { *out_valid = 0 };
        0
    }
}

// ── Clone ───────────────────────────────────────────────────────────────────

/// Clone a sender handle for multi-producer use.
///
/// # Safety
///
/// `sender` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_sender_clone(
    sender: *mut HewChannelSender,
) -> *mut HewChannelSender {
    cabi_guard!(sender.is_null(), ptr::null_mut());
    // SAFETY: sender is valid per caller contract.
    let sender_ref = unsafe { &*sender };
    sender_ref.senders.fetch_add(1, Ordering::AcqRel);
    Box::into_raw(Box::new(HewChannelSender {
        core: Arc::clone(&sender_ref.core),
        senders: Arc::clone(&sender_ref.senders),
    })) // ALLOCATOR-PAIRING: GlobalAlloc
}

// ── Close / Free ────────────────────────────────────────────────────────────

/// Close and free a sender handle.
///
/// When the last sender is closed, the receiver observes channel closure: a
/// parked / blocking `recv` wakes and returns `None`. The `Drop` impl on
/// [`HewChannelSender`] performs the refcount decrement + `close_sink`.
///
/// # Safety
///
/// `sender` must have been returned by [`hew_channel_pair_sender`] or
/// [`hew_channel_sender_clone`] and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_sender_close(sender: *mut HewChannelSender) {
    if sender.is_null() {
        return;
    }
    // SAFETY: caller guarantees sender was Box-allocated and is exclusively
    // owned. Dropping the box runs `HewChannelSender::drop`, which decrements
    // the live-sender refcount and closes the sink on the last sender.
    unsafe { drop(Box::from_raw(sender)) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Close and free a receiver handle.
///
/// Closes the consumer side of the core (waking any parked producer so its
/// `send` resolves as a no-op) before releasing the handle. Receive must never
/// double-close: only this consuming `close` tears the consumer side down.
///
/// # Safety
///
/// `receiver` must have been returned by [`hew_channel_pair_receiver`]
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_receiver_close(receiver: *mut HewChannelReceiver) {
    if receiver.is_null() {
        return;
    }
    // SAFETY: caller guarantees receiver was Box-allocated and is exclusively owned.
    let receiver = unsafe { Box::from_raw(receiver) }; // ALLOCATOR-PAIRING: GlobalAlloc
    receiver.core.close_stream();
    drop(receiver);
}

// ── Suspending receive (NEW-4) ───────────────────────────────────────────────
//
// These entries flip `await rx.recv()` from a worker-blocking call onto the
// read-slot / `enqueue_resume` substrate when the caller carries an execution
// context (actor handler / closure / task entry). The codegen suspend ramp
// calls `hew_channel_await_recv` to register, suspends on
// `STREAM_AWAIT_SUSPEND`, and on the resume / immediate-ready edge binds the
// `Option<T>` by popping through the existing non-blocking `hew_channel_try_recv`
// / `hew_channel_try_recv_int` (the single consumer pops the queued item exactly
// once, or `None` on close). The receiver handle is BORROWED across the suspend
// — never consumed or double-closed. See `crate::channel_core` for the wake
// discipline.

/// Register a suspending consumer for `await rx.recv()`.
///
/// Returns [`crate::channel_core::STREAM_AWAIT_READY`] when the bind can proceed
/// immediately (an item is queued or the channel is closed), or
/// [`crate::channel_core::STREAM_AWAIT_SUSPEND`] after parking the consumer's
/// continuation on `slot`. The caller MUST `coro.suspend` on SUSPEND and bind
/// via `hew_channel_try_recv*` on READY / resume.
///
/// # Safety
///
/// `receiver` is a live receiver handle; `actor` is the awaiting actor
/// (`hew_actor_self`); `slot` is a live read slot the caller created and holds
/// the creator ref to.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_await_recv(
    receiver: *mut HewChannelReceiver,
    actor: *mut crate::actor::HewActor,
    slot: *mut crate::read_slot::HewReadSlot,
) -> i32 {
    if receiver.is_null() {
        return crate::channel_core::STREAM_AWAIT_READY;
    }
    // SAFETY: receiver is a valid handle per caller contract; the core is alive
    // (the receiver holds an Arc clone). `actor` / `slot` validity is the
    // caller's contract (`await_next` never derefs `actor`).
    unsafe { (*receiver).core.await_next(actor, slot) }
}

/// Detach an abandoned suspending consumer (the codegen abandon edge). Releases
/// the channel core's in-flight ref on `slot` if it is still registered.
///
/// # Safety
///
/// `receiver` is a valid receiver handle; `slot` is the consumer's read slot.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_detach_recv(
    receiver: *mut HewChannelReceiver,
    slot: *mut crate::read_slot::HewReadSlot,
) {
    if receiver.is_null() {
        return;
    }
    // SAFETY: receiver is valid; the core is alive (receiver holds an Arc
    // clone); slot is the consumer's read slot.
    unsafe { (*receiver).core.detach_consumer(slot) };
}

// ── select{} channel-recv arm poll (NEW-4) ───────────────────────────────────
//
// `hew_channel_poll` / `hew_channel_cancel_pending_read` integrate a channel
// receive into the `select{}` waitset, mirroring `hew_stream_poll` for streams.
// Unlike the stream poll (which consumes the item in the park thread), the
// channel poll is SIGNAL-ONLY: it waits for readiness WITHOUT consuming and
// fires the readiness callback (`hew_reply_channel_signal_ready`) so
// `hew_select_first` returns this arm as the winner; the winning codegen edge
// then pops the item itself via the non-blocking `hew_channel_try_recv*`. A
// losing arm is withdrawn by `hew_channel_cancel_pending_read`, leaving its
// queued item intact for the next consumer (no item loss, no Option<string>
// ownership crossing the park thread → no leak on a non-winning arm).
//
// The select substrate parks a foreign thread per arm and is NOT worker-free
// (it matches the established stream/actor-ask select arms); the worker-free
// receive path is `await rx.recv()` (`Terminator::SuspendingChannelRecv`).

/// Register a `select{}` readiness poll on a channel receiver. Spawns a park
/// thread that waits for the channel to become readable (item queued or sink
/// closed) and then invokes `callback(userdata)` exactly once — UNLESS the poll
/// is cancelled first. Returns a non-zero pending-read id, or 0 on a null
/// receiver. At most one poll may be active per receiver (CAS-enforced; a
/// violation aborts, matching `hew_stream_poll`).
///
/// ## Observer-reference ownership (exactly-once)
///
/// `userdata` is a retained observer reference (the `select{}` arm's
/// `HewReplyChannel*`). EXACTLY ONE of two paths consumes it:
/// - the readiness `callback(userdata)` firing (the `Pending → Done`
///   transition), or
/// - `release(userdata)` on cancel (the `Pending → Cancelled` transition, when
///   [`hew_channel_cancel_pending_read`] withdraws this poll before it fires).
///
/// `release` releases exactly one reference WITHOUT firing the readiness
/// callback (e.g. `hew_reply_channel_free`). The two paths are mutually
/// exclusive — the `Pending → {Done, Cancelled}` transition is serialized under
/// one mutex — so the retained reference is neither leaked nor double-released.
///
/// # Safety
///
/// `receiver` is a live receiver handle that outlives the park thread (the
/// caller cancels every losing arm before the select site returns).
/// `callback` and `userdata` are a `hew_reply_channel_signal_ready` fn pointer
/// and its `*mut HewReplyChannel`; `release` releases exactly one reference of
/// that same `userdata` (e.g. `hew_reply_channel_free`).
#[no_mangle]
pub unsafe extern "C" fn hew_channel_poll(
    receiver: *mut HewChannelReceiver,
    callback: extern "C" fn(*mut c_void),
    userdata: *mut c_void,
    release: extern "C" fn(*mut c_void),
) -> u64 {
    if receiver.is_null() {
        return 0;
    }
    let id = NEXT_CHANNEL_POLL_ID.fetch_add(1, Ordering::Relaxed);

    // SAFETY: receiver is valid per caller contract; pending_read is an
    // AtomicU64 designed for concurrent access.
    let prev = unsafe {
        (*receiver)
            .pending_read
            .compare_exchange(0, id, Ordering::AcqRel, Ordering::Acquire)
    };
    if let Err(existing) = prev {
        eprintln!(
            "hew_channel_poll: at most one pending poll per receiver \
             (existing id={existing}, refused id={id})"
        );
        std::process::abort();
    }

    let poll = Arc::new(ChannelPoll {
        cancelled: std::sync::atomic::AtomicBool::new(false),
        state: Mutex::new(ChannelPollState::Pending),
        observer: userdata as usize,
        release,
    });
    // SAFETY: receiver is valid; pending_state is a Mutex.
    unsafe {
        let mut slot = (*receiver)
            .pending_state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *slot = Some(Arc::clone(&poll));
    }

    // SAFETY: the core is alive (the receiver holds an Arc clone); clone the Arc
    // so the park thread keeps the core alive independent of a racing close.
    let core = unsafe { Arc::clone(&(*receiver).core) };
    let receiver_addr = receiver as usize;
    let userdata_addr = userdata as usize;
    std::thread::spawn(move || {
        // Wait for readiness without consuming; returns false if cancelled.
        let ready = core.wait_ready(&poll.cancelled);

        let mut state = poll
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match *state {
            ChannelPollState::Pending if ready => {
                *state = ChannelPollState::Done;
                drop(state);
                // Clear the registration before firing so the receiver can be
                // re-polled (no recursion within the callback).
                let receiver = receiver_addr as *mut HewChannelReceiver;
                // SAFETY: the receiver outlives the park thread per the caller
                // contract; pending_read / pending_state are the sync fields.
                unsafe {
                    (*receiver).pending_read.store(0, Ordering::Release);
                    let mut s = (*receiver)
                        .pending_state
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    *s = None;
                }
                callback(userdata_addr as *mut c_void);
            }
            // Cancelled won the race, or wait returned not-ready (cancelled):
            // no callback. The cancel path cleared the registration.
            _ => {}
        }
    });

    id
}

/// Withdraw a `select{}` channel poll registered by [`hew_channel_poll`] (a
/// losing arm at the select dispatch site). Idempotent; safe if the poll
/// already fired. Wakes the park thread so it observes the cancel promptly.
///
/// # Safety
///
/// `receiver` is a live receiver handle; `id` is a value returned by
/// [`hew_channel_poll`].
#[no_mangle]
pub unsafe extern "C" fn hew_channel_cancel_pending_read(
    receiver: *mut HewChannelReceiver,
    id: u64,
) {
    if receiver.is_null() || id == 0 {
        return;
    }
    // SAFETY: receiver is valid per caller contract.
    let receiver = unsafe { &*receiver };
    // Only cancel if THIS id is the active one.
    if receiver.pending_read.load(Ordering::Acquire) != id {
        return;
    }
    let poll = {
        let mut slot = receiver
            .pending_state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        slot.take()
    };
    if let Some(poll) = poll {
        let won = {
            let mut state = poll
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if *state == ChannelPollState::Pending {
                *state = ChannelPollState::Cancelled;
                true
            } else {
                // The park thread already fired (Done) — it owns and consumed
                // the observer reference. Nothing to release here.
                false
            }
        };
        receiver.pending_read.store(0, Ordering::Release);
        // H2: set the cancel predicate AND wake the park thread under the core's
        // mutex so the store cannot slip between `wait_ready`'s predicate check
        // and its park — the cancel wake can never be lost.
        receiver.core.cancel_wait(&poll.cancelled);
        // H1: this cancel won the `Pending → Cancelled` transition, so the
        // readiness callback will NOT fire. Ownership of the retained observer
        // reference handed to this path — release it exactly once.
        if won {
            (poll.release)(poll.observer as *mut c_void);
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn create_and_free() {
        let pair = hew_channel_new(4);
        assert!(!pair.is_null());
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            assert!(!tx.is_null());
            assert!(!rx.is_null());
            hew_channel_pair_free(pair);
            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// Regression: `hew_channel_send`'s contract requires a NUL-terminated
    /// C string, and its `CStr::from_ptr` will scan past any caller buffer
    /// that omits the terminator.  This guards the in-crate test surface
    /// against re-introducing the pattern (the previous offender lived in
    /// `recv_int_rejects_non_i64_payloads` and was caught by `ASan` as a
    /// global-buffer-overflow).  We round-trip a short, properly NUL-
    /// terminated payload to keep the sanitizer lane green.
    #[test]
    fn send_requires_nul_terminated_payload() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Exactly the shape that previously triggered the overflow:
            // a 3-byte payload — but here we include the terminator so
            // `CStr::from_ptr` stays inside the allocation.
            let payload = b"bad\0";
            hew_channel_send(tx, payload.as_ptr().cast());

            let result = hew_channel_recv(rx);
            assert!(!result.is_null());
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_bytes(), b"bad");
            crate::cabi::free_cstring(result); // CSTRING-FREE: str-open (test frees hew_channel_recv string output; header-aware in S1)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn send_recv_basic() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let msg = b"hello\0";
            hew_channel_send(tx, msg.as_ptr().cast());

            let result = hew_channel_recv(rx);
            assert!(!result.is_null());
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_str().unwrap(), "hello");
            crate::cabi::free_cstring(result); // CSTRING-FREE: str-open (test frees hew_channel_recv string output; header-aware in S1)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_returns_null_on_closed() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_sender_close(tx);
            let result = hew_channel_recv(rx);
            // Closed channel returns NULL so codegen can wrap as None.
            assert!(result.is_null());

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_returns_null_when_empty() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let result = hew_channel_try_recv(rx);
            assert!(result.is_null(), "empty channel should return NULL");

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_distinguishes_empty_string_from_no_message() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Send an actual empty string.
            let empty = b"\0";
            hew_channel_send(tx, empty.as_ptr().cast());

            // try_recv should return a valid (non-NULL) pointer to a NUL byte.
            let result = hew_channel_try_recv(rx);
            assert!(!result.is_null(), "empty string message should be non-NULL");
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_str().unwrap(), "");
            crate::cabi::free_cstring(result); // CSTRING-FREE: str-open (test frees hew_channel_recv string output; header-aware in S1)

            // Now the channel is empty — try_recv should return NULL.
            let result2 = hew_channel_try_recv(rx);
            assert!(result2.is_null(), "drained channel should return NULL");

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_distinguishes_zero_from_no_message() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Send the value 0.
            hew_channel_send_int(tx, 0);

            let mut valid: i32 = -1;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1, "received 0 should set valid=1");
            assert_eq!(val, 0);

            // Now the channel is empty.
            let val2 = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "drained channel should set valid=0");
            assert_eq!(val2, 0);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn multi_producer() {
        let pair = hew_channel_new(8);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let tx2 = hew_channel_sender_clone(tx);
            let tx_addr = tx as usize;
            let tx2_addr = tx2 as usize;

            let t1 = thread::spawn(move || {
                let tx = tx_addr as *mut HewChannelSender;
                let msg = b"from-1\0";
                // SAFETY: tx is valid for the lifetime of this test.
                hew_channel_send(tx, msg.as_ptr().cast());
                hew_channel_sender_close(tx);
            });

            let t2 = thread::spawn(move || {
                let tx2 = tx2_addr as *mut HewChannelSender;
                let msg = b"from-2\0";
                // SAFETY: tx2 is valid for the lifetime of this test.
                hew_channel_send(tx2, msg.as_ptr().cast());
                hew_channel_sender_close(tx2);
            });

            t1.join().unwrap();
            t2.join().unwrap();

            let mut messages = Vec::new();
            loop {
                let result = hew_channel_recv(rx);
                if result.is_null() {
                    break;
                }
                let s = CStr::from_ptr(result).to_str().unwrap().to_owned();
                crate::cabi::free_cstring(result); // CSTRING-FREE: str-open (test frees hew_channel_recv string output; header-aware in S1)
                messages.push(s);
            }

            messages.sort();
            assert_eq!(messages, vec!["from-1", "from-2"]);

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn send_recv_int_basic() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_send_int(tx, 42);
            hew_channel_send_int(tx, -7);

            let mut valid: i32 = 0;
            let v1 = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1);
            assert_eq!(v1, 42);
            let v2 = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1);
            assert_eq!(v2, -7);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_int_returns_invalid_on_closed() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_sender_close(tx);
            let mut valid: i32 = -1;
            let val = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "closed channel should set out_valid to 0");
            assert_eq!(val, 0);

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_returns_invalid_when_empty() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let mut valid: i32 = -1;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "empty channel should set valid=0");
            assert_eq!(val, 0);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_returns_value_when_available() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_send_int(tx, 99);
            let mut valid: i32 = 0;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1, "message available should set valid=1");
            assert_eq!(val, 99);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_rejects_non_i64_payloads() {
        crate::hew_clear_error();
        let pair = hew_channel_new(4);
        // SAFETY: test owns both channel handles and only sends stack-backed test payloads.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let payload = b"oops\0";
            hew_channel_send(tx, payload.as_ptr().cast());

            let mut valid = -1;
            let value = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0);
            assert_eq!(value, 0);
            let err = std::ffi::CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected 8-byte int payload"),
                "unexpected error: {err}"
            );

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_int_rejects_non_i64_payloads() {
        crate::hew_clear_error();
        let pair = hew_channel_new(4);
        // SAFETY: test owns both channel handles and only sends stack-backed test payloads.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // NUL-terminated so `hew_channel_send`'s `CStr::from_ptr` does
            // not read past the global; the transmitted payload is still
            // the 3-byte "bad", which is rejected as a non-i64 message.
            let payload = b"bad\0";
            hew_channel_send(tx, payload.as_ptr().cast());

            let mut valid = -1;
            let value = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0);
            assert_eq!(value, 0);
            let err = std::ffi::CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected 8-byte int payload"),
                "unexpected error: {err}"
            );

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn int_multi_producer() {
        let pair = hew_channel_new(8);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let tx2 = hew_channel_sender_clone(tx);
            let tx_addr = tx as usize;
            let tx2_addr = tx2 as usize;

            let t1 = thread::spawn(move || {
                let tx = tx_addr as *mut HewChannelSender;
                hew_channel_send_int(tx, 100);
                hew_channel_sender_close(tx);
            });

            let t2 = thread::spawn(move || {
                let tx2 = tx2_addr as *mut HewChannelSender;
                hew_channel_send_int(tx2, 200);
                hew_channel_sender_close(tx2);
            });

            t1.join().unwrap();
            t2.join().unwrap();

            let mut values = Vec::new();
            let mut valid: i32 = 0;
            loop {
                let v = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
                if valid == 0 {
                    break;
                }
                values.push(v);
            }

            values.sort_unstable();
            assert_eq!(values, vec![100, 200]);

            hew_channel_receiver_close(rx);
        }
    }

    // ── NEW-4 suspending receive + select poll ───────────────────────────────

    use crate::read_slot::{
        hew_read_slot_free, hew_read_slot_new, hew_read_slot_status, ReadStatus,
    };
    /// `hew_channel_await_recv` returns READY (no park) when an item is already
    /// queued — the codegen ramp binds immediately.
    #[test]
    fn await_recv_ready_when_item_queued() {
        let pair = hew_channel_new(2);
        // SAFETY: pair just created.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_send(tx, c"hi".as_ptr());
            let slot = hew_read_slot_new();
            let rc = hew_channel_await_recv(rx, std::ptr::null_mut(), slot);
            assert_eq!(rc, crate::channel_core::STREAM_AWAIT_READY);
            hew_channel_detach_recv(rx, slot);
            hew_read_slot_free(slot);

            // The bind edge pops the queued item via try_recv.
            let got = hew_channel_try_recv(rx);
            assert_eq!(CStr::from_ptr(got).to_bytes(), b"hi");
            crate::cabi::free_cstring(got); // CSTRING-FREE: str-open (test)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// Worker-free park/resume: `hew_channel_await_recv` parks on an empty,
    /// live channel (SUSPEND, freeing the worker); a later `send` deposits a
    /// Data readiness signal onto the slot (the wake the scheduler resumes),
    /// and the bind edge pops the item exactly once.
    #[test]
    fn await_recv_parks_then_send_deposits_and_resumes() {
        let pair = hew_channel_new(2);
        // SAFETY: pair just created.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let slot = hew_read_slot_new();
            let rc = hew_channel_await_recv(rx, std::ptr::null_mut(), slot);
            assert_eq!(
                rc,
                crate::channel_core::STREAM_AWAIT_SUSPEND,
                "empty + live channel must park (worker-free)"
            );

            // A producer deposit signals readiness on the parked slot.
            hew_channel_send(tx, c"woke".as_ptr());
            assert_eq!(hew_read_slot_status(slot), ReadStatus::Data as i32);

            // Resume edge pops the actual item.
            let got = hew_channel_try_recv(rx);
            assert_eq!(CStr::from_ptr(got).to_bytes(), b"woke");
            crate::cabi::free_cstring(got); // CSTRING-FREE: str-open (test)

            hew_read_slot_free(slot);
            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// A parked receive woken by the LAST sender closing binds `None` (EOF):
    /// the bind edge's `try_recv` on an empty-and-closed channel returns null.
    #[test]
    fn await_recv_parks_then_close_resumes_none() {
        let pair = hew_channel_new(2);
        // SAFETY: pair just created.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let slot = hew_read_slot_new();
            let rc = hew_channel_await_recv(rx, std::ptr::null_mut(), slot);
            assert_eq!(rc, crate::channel_core::STREAM_AWAIT_SUSPEND);

            hew_channel_sender_close(tx); // last sender → close_sink wakes consumer
            assert_eq!(hew_read_slot_status(slot), ReadStatus::Data as i32);

            let got = hew_channel_try_recv(rx);
            assert!(got.is_null(), "closed + empty must bind None");

            hew_read_slot_free(slot);
            hew_channel_receiver_close(rx);
        }
    }

    extern "C" fn test_select_signal(userdata: *mut c_void) {
        // SAFETY: the test passes a &PollObserver as userdata.
        let obs = unsafe { &*(userdata as *const PollObserver) };
        obs.fired.fetch_add(1, Ordering::SeqCst);
    }

    /// Stand-in for the `select{}` observer reference released on cancel. The
    /// production `release` is `hew_reply_channel_free`; the test counts calls
    /// to assert the exactly-once ownership handoff between the readiness
    /// callback (fire) and this release (cancel).
    extern "C" fn test_select_release(userdata: *mut c_void) {
        // SAFETY: the test passes a &PollObserver as userdata.
        let obs = unsafe { &*(userdata as *const PollObserver) };
        obs.released.fetch_add(1, Ordering::SeqCst);
    }

    /// Test observer: counts readiness-callback fires and observer releases so
    /// the regressions can assert EXACTLY ONE of the two consumes the retained
    /// reference (H1 ownership handoff).
    struct PollObserver {
        fired: AtomicUsize,
        released: AtomicUsize,
    }

    /// `hew_channel_poll` fires the select readiness callback when the channel
    /// becomes readable (a sender deposits), WITHOUT consuming the item — the
    /// winning arm pops it itself. H1: the late-ready path consumes the retained
    /// observer reference EXACTLY ONCE via the callback; `release` is NOT called.
    #[test]
    fn poll_fires_signal_on_send_without_consuming() {
        let pair = hew_channel_new(2);
        let obs = PollObserver {
            fired: AtomicUsize::new(0),
            released: AtomicUsize::new(0),
        };
        // SAFETY: pair just created; obs outlives the poll thread (joined via
        // the spin below before the function returns).
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let id = hew_channel_poll(
                rx,
                test_select_signal,
                std::ptr::addr_of!(obs) as *mut c_void,
                test_select_release,
            );
            assert_ne!(id, 0);

            hew_channel_send(tx, c"item".as_ptr());

            // Spin-wait for the poll thread to fire the callback.
            let mut spins = 0;
            while obs.fired.load(Ordering::SeqCst) == 0 {
                std::thread::yield_now();
                spins += 1;
                assert!(spins < 1_000_000, "poll callback never fired");
            }

            // H1: the callback consumed the observer reference exactly once; the
            // cancel-path release was NOT taken (no leak, no double-release).
            assert_eq!(obs.fired.load(Ordering::SeqCst), 1, "callback fires once");
            assert_eq!(
                obs.released.load(Ordering::SeqCst),
                0,
                "a fired poll must not also release via the cancel path"
            );

            // Not consumed by the poll: the item is still queued for the winner.
            let got = hew_channel_try_recv(rx);
            assert_eq!(CStr::from_ptr(got).to_bytes(), b"item");
            crate::cabi::free_cstring(got); // CSTRING-FREE: str-open (test)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// `hew_channel_cancel_pending_read` withdraws a losing select arm's poll;
    /// the callback never fires and the queued item is left intact. H1: the
    /// losing arm releases the retained observer reference EXACTLY ONCE via the
    /// cancel ownership handoff (the readiness callback is NOT fired), closing
    /// the per-loser `HewReplyChannel` leak.
    #[test]
    fn poll_cancel_withdraws_and_leaves_item() {
        let pair = hew_channel_new(2);
        let obs = PollObserver {
            fired: AtomicUsize::new(0),
            released: AtomicUsize::new(0),
        };
        // SAFETY: pair just created.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let id = hew_channel_poll(
                rx,
                test_select_signal,
                std::ptr::addr_of!(obs) as *mut c_void,
                test_select_release,
            );
            assert_ne!(id, 0);
            hew_channel_cancel_pending_read(rx, id);

            // H1: cancelling a still-pending poll hands ownership of the retained
            // observer reference to the cancel path, which releases it exactly
            // once. (Production `release` is `hew_reply_channel_free`, so this is
            // the per-losing-arm `HewReplyChannel` that previously leaked.)
            assert_eq!(
                obs.released.load(Ordering::SeqCst),
                1,
                "cancel must release the retained observer reference exactly once"
            );

            // A later send must not fire the cancelled callback.
            hew_channel_send(tx, c"late".as_ptr());
            std::thread::sleep(std::time::Duration::from_millis(50));
            assert_eq!(
                obs.fired.load(Ordering::SeqCst),
                0,
                "cancelled poll must not fire"
            );
            assert_eq!(
                obs.released.load(Ordering::SeqCst),
                1,
                "release stays exactly-once after a late send"
            );

            // The item is left intact for the next consumer.
            let got = hew_channel_try_recv(rx);
            assert_eq!(CStr::from_ptr(got).to_bytes(), b"late");
            crate::cabi::free_cstring(got); // CSTRING-FREE: str-open (test)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }
}
