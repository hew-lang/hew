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

use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use crate::channel_common::free_channel_pair;
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

// ── Layout-witness element path (generic Sender<T> / Receiver<T> width) ─────
//
// The `*_layout` entries carry ANY element type the compiler can describe
// through one mechanism: a `HewVecElemLayout` witness (the W5.016 descriptor)
// selects the envelope encoding and the ownership discipline. See the table in
// `crate::channel_common` for the per-kind envelope contract.
//
// Ownership (FFI contract):
// - `hew_channel_send_layout` — the element is deep-copied INTO the envelope
//   (`clone_fn` for layout-managed elements); the caller keeps its value and
//   the queue owns the envelope.
// - `hew_channel_recv_layout` / `hew_channel_try_recv_layout` — the decoded
//   element MOVES to the consumer's out slot; the consumer drops it (the MIR
//   drop spine / `drop_fn`). The queue performs no clone and no drop.
// - Envelopes never consumed (receiver dropped with items queued, post-close
//   sends) are released by `ChannelCore` via the stamped witness exactly once.

/// Send one element of any witness-describable type through the channel.
/// Blocks if the channel is full (backpressure).
///
/// # Safety
///
/// `sender` must be a valid pointer. `data` must point to one live element of
/// the witness's type (see [`crate::channel_common::encode_elem_envelope`]).
/// `layout` must point to a valid `HewVecElemLayout` for the duration of the
/// call (in practice a codegen static).
#[no_mangle]
pub unsafe extern "C" fn hew_channel_send_layout(
    sender: *mut HewChannelSender,
    data: *const c_void,
    layout: *const crate::vec::HewVecElemLayout,
) {
    cabi_guard!(sender.is_null() || data.is_null());
    // SAFETY: layout validity is the caller's contract; the helper aborts
    // fail-closed on a malformed witness.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_channel_send_layout") };
    // SAFETY: data points to one live element per caller contract.
    let env = unsafe {
        crate::channel_common::encode_elem_envelope(data, layout, "hew_channel_send_layout")
    };
    // SAFETY: sender is valid per caller contract.
    let core = unsafe { &(*sender).core };
    if layout.ownership_kind == crate::vec::HewTypeOwnershipKind::LayoutManaged {
        // Stamp BEFORE enqueue so every later discard exit can release the
        // envelope's owned heap.
        core.stamp_elem_layout(layout);
    }
    core.blocking_send(env);
}

/// Block until an element is available and decode it into `out`.
///
/// Returns 1 when an element was written to `out` (ownership transfers to the
/// caller), or 0 when the channel is closed (all senders dropped), letting
/// codegen wrap the result as `Option<T>`.
///
/// # Safety
///
/// `receiver` must be a valid pointer. `out` must point to one writable
/// element slot of the witness's type. `layout` must be a valid witness.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv_layout(
    receiver: *mut HewChannelReceiver,
    out: *mut c_void,
    layout: *const crate::vec::HewVecElemLayout,
) -> i32 {
    cabi_guard!(receiver.is_null() || out.is_null(), 0);
    // SAFETY: layout validity is the caller's contract.
    let layout =
        unsafe { crate::channel_common::elem_layout_witness(layout, "hew_channel_recv_layout") };
    // SAFETY: receiver is valid per caller contract.
    let item = unsafe { (*receiver).core.blocking_recv() };
    // SAFETY: out points to one writable element slot per caller contract.
    unsafe {
        crate::channel_common::decode_elem_envelope(item, out, layout, "hew_channel_recv_layout")
    }
}

/// Try to receive an element without blocking.
///
/// Returns 1 when an element was written to `out` (ownership transfers to the
/// caller), or 0 when the channel is empty or closed.
///
/// # Safety
///
/// `receiver` must be a valid pointer. `out` must point to one writable
/// element slot of the witness's type. `layout` must be a valid witness.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_try_recv_layout(
    receiver: *mut HewChannelReceiver,
    out: *mut c_void,
    layout: *const crate::vec::HewVecElemLayout,
) -> i32 {
    cabi_guard!(receiver.is_null() || out.is_null(), 0);
    // SAFETY: layout validity is the caller's contract.
    let layout = unsafe {
        crate::channel_common::elem_layout_witness(layout, "hew_channel_try_recv_layout")
    };
    // SAFETY: receiver is valid per caller contract.
    let item = unsafe { (*receiver).core.try_recv() };
    // SAFETY: out points to one writable element slot per caller contract.
    unsafe {
        crate::channel_common::decode_elem_envelope(
            item,
            out,
            layout,
            "hew_channel_try_recv_layout",
        )
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
// `Option<T>` by popping through the non-blocking layout-witness
// `hew_channel_try_recv_layout` (the single consumer pops the queued item
// exactly once, or `None` on close). The receiver handle is BORROWED across the suspend
// — never consumed or double-closed. See `crate::channel_core` for the wake
// discipline.

/// Register a suspending consumer for `await rx.recv()`.
///
/// Returns [`crate::channel_core::STREAM_AWAIT_READY`] when the bind can proceed
/// immediately (an item is queued or the channel is closed), or
/// [`crate::channel_core::STREAM_AWAIT_SUSPEND`] after parking the consumer's
/// continuation on `slot`. The caller MUST `coro.suspend` on SUSPEND and bind
/// via `hew_channel_try_recv_layout` on READY / resume.
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
// then pops the item itself via the non-blocking `hew_channel_try_recv_layout`. A
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

// ── Deadline-cancel cleanup callback (NEW-6b) ───────────────────────────────
//
// `hew_channel_recv_cancel_cleanup` bridges the `HewAwaitCleanup` callback ABI
// (`fn(*mut c_void, i32)`) to the channel-recv cancel path.  Codegen allocates a
// `HewChannelRecvCancelCtx` in the coroutine frame before the coro.suspend and
// passes a pointer to it as the `source` argument to `hew_await_cancel_new`.
// When the deadline timer fires (or an explicit cancel wins the CAS), the
// await-cancel arbiter calls this cleanup function.

/// Per-suspend cancel context: carries both the read slot and the channel
/// receiver handle so the cleanup callback can cancel + detach in one call.
///
/// Allocated as an alloca in the coroutine frame (codegen side) so its lifetime
/// spans the coro.suspend — the spilling pass moves it into the frame object.
#[repr(C)]
#[allow(
    dead_code,
    reason = "fields accessed via FFI from codegen-emitted LLVM IR, not from Rust"
)]
pub struct HewChannelRecvCancelCtx {
    /// The `HewReadSlot` this recv registered against.
    pub slot: *mut crate::read_slot::HewReadSlot,
    /// The `HewChannelReceiver` handle the recv is registered against.
    pub receiver: *mut HewChannelReceiver,
}

impl std::fmt::Debug for HewChannelRecvCancelCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewChannelRecvCancelCtx")
            .field("slot", &self.slot)
            .field("receiver", &self.receiver)
            .finish()
    }
}

/// Await-cancel cleanup callback for a suspending `await rx.recv()`.
///
/// Fires when the deadline timer wins the one-shot CAS (`TimedOut`) or an
/// explicit cancel wins (`Cancelled`).  Cancels the read slot and detaches the
/// channel-core's in-flight consumer reference so the core never tries to wake
/// a freed slot.
///
/// # Safety
///
/// `source` must be a `*mut HewChannelRecvCancelCtx` allocated in the caller's
/// coroutine frame; it remains valid until the actor resumes and the frame is
/// freed.  The underlying slot and receiver must still be alive (they are kept
/// alive by the coroutine frame's alloca lifetime).
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv_cancel_cleanup(source: *mut c_void, _status: i32) {
    let ctx = source.cast::<HewChannelRecvCancelCtx>();
    if ctx.is_null() {
        return;
    }
    // SAFETY: ctx is a valid frame-alloca; slot and receiver pointers are alive.
    let slot = unsafe { (*ctx).slot };
    // SAFETY: same frame-alloca guarantee as slot above.
    let receiver = unsafe { (*ctx).receiver };
    if !slot.is_null() {
        // SAFETY: slot is alive (frame alloca) and still owned by this recv path.
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
    }
    if !receiver.is_null() {
        // SAFETY: receiver is alive and core still holds a reference to the slot.
        unsafe { hew_channel_detach_recv(receiver, slot) };
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{c_char, CStr};
    use std::thread;

    /// Send a NUL-terminated string element through the layout-witness entry
    /// (the single send mechanism; the per-type symbols are retired). The
    /// slot address of the caller's string pointer is what the ABI carries.
    unsafe fn send_str(tx: *mut HewChannelSender, s: &CStr) {
        let layout = string_layout();
        let slot: *const c_char = s.as_ptr();
        // SAFETY: forwarded from caller; slot/layout are live locals.
        unsafe { hew_channel_send_layout(tx, std::ptr::addr_of!(slot).cast(), &raw const layout) };
    }

    /// Blocking recv of one string element via the layout witness; returns the
    /// owned header-aware cstring, or null when the channel is closed.
    unsafe fn recv_str(rx: *mut HewChannelReceiver) -> *mut c_char {
        let layout = string_layout();
        let mut out: *mut c_char = ptr::null_mut();
        // SAFETY: forwarded from caller; out/layout are live locals.
        let rc = unsafe {
            hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout)
        };
        if rc == 1 {
            out
        } else {
            ptr::null_mut()
        }
    }

    /// Non-blocking recv of one string element via the layout witness; null
    /// when the queue is empty or closed.
    unsafe fn try_recv_str(rx: *mut HewChannelReceiver) -> *mut c_char {
        let layout = string_layout();
        let mut out: *mut c_char = ptr::null_mut();
        // SAFETY: forwarded from caller; out/layout are live locals.
        let rc = unsafe {
            hew_channel_try_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout)
        };
        if rc == 1 {
            out
        } else {
            ptr::null_mut()
        }
    }

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
                // SAFETY: tx is valid for the lifetime of this test.
                send_str(tx, c"from-1");
                hew_channel_sender_close(tx);
            });

            let t2 = thread::spawn(move || {
                let tx2 = tx2_addr as *mut HewChannelSender;
                // SAFETY: tx2 is valid for the lifetime of this test.
                send_str(tx2, c"from-2");
                hew_channel_sender_close(tx2);
            });

            t1.join().unwrap();
            t2.join().unwrap();

            let mut messages = Vec::new();
            loop {
                let result = recv_str(rx);
                if result.is_null() {
                    break;
                }
                let s = CStr::from_ptr(result).to_str().unwrap().to_owned();
                crate::cabi::free_cstring(result); // CSTRING-FREE: str-open (test frees layout recv string output; header-aware)
                messages.push(s);
            }

            messages.sort();
            assert_eq!(messages, vec!["from-1", "from-2"]);

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

            send_str(tx, c"hi");
            let slot = hew_read_slot_new();
            let rc = hew_channel_await_recv(rx, std::ptr::null_mut(), slot);
            assert_eq!(rc, crate::channel_core::STREAM_AWAIT_READY);
            hew_channel_detach_recv(rx, slot);
            hew_read_slot_free(slot);

            // The bind edge pops the queued item via try_recv.
            let got = try_recv_str(rx);
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
            send_str(tx, c"woke");
            assert_eq!(hew_read_slot_status(slot), ReadStatus::Data as i32);

            // Resume edge pops the actual item.
            let got = try_recv_str(rx);
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

            let got = try_recv_str(rx);
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

            send_str(tx, c"item");

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
            let got = try_recv_str(rx);
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
            send_str(tx, c"late");
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
            let got = try_recv_str(rx);
            assert_eq!(CStr::from_ptr(got).to_bytes(), b"late");
            crate::cabi::free_cstring(got); // CSTRING-FREE: str-open (test)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    // ── Layout-witness element path (generic element width) ─────────────────

    use crate::vec::{HewTypeOwnershipKind, HewVecElemLayout};

    fn plain_layout(size: usize, align: usize) -> HewVecElemLayout {
        HewVecElemLayout {
            size,
            align,
            ownership_kind: HewTypeOwnershipKind::Plain,
            clone_fn: None,
            drop_fn: None,
        }
    }

    fn string_layout() -> HewVecElemLayout {
        HewVecElemLayout {
            size: size_of::<*const c_char>(),
            align: align_of::<*const c_char>(),
            ownership_kind: HewTypeOwnershipKind::String,
            clone_fn: None,
            drop_fn: None,
        }
    }

    /// Round-trip an i64 element through the witness path: the envelope is the
    /// raw 8-byte representation and decode moves it into the out slot.
    #[test]
    fn layout_roundtrip_plain_i64() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = plain_layout(8, 8);
            let value: i64 = -40_204;
            hew_channel_send_layout(tx, std::ptr::addr_of!(value).cast(), &raw const layout);

            let mut out: i64 = 0;
            let rc =
                hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout);
            assert_eq!(rc, 1);
            assert_eq!(out, -40_204);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// A 4-byte element (i32) rides the same mechanism — the witness width,
    /// not a fixed 8-byte wire, decides the envelope size.
    #[test]
    fn layout_roundtrip_plain_i32() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = plain_layout(4, 4);
            let value: i32 = 7;
            hew_channel_send_layout(tx, std::ptr::addr_of!(value).cast(), &raw const layout);

            let mut out: i32 = 0;
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 1);
            assert_eq!(out, 7);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// String elements stay content-encoded: send reads the caller's string
    /// slot, recv materialises a fresh header-aware cstring the consumer owns.
    /// An empty string is `Some("")` (rc 1), never `None`.
    #[test]
    fn layout_roundtrip_string_preserves_empty() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = string_layout();
            let hello: *const c_char = c"hello".as_ptr();
            hew_channel_send_layout(tx, std::ptr::addr_of!(hello).cast(), &raw const layout);
            let empty: *const c_char = c"".as_ptr();
            hew_channel_send_layout(tx, std::ptr::addr_of!(empty).cast(), &raw const layout);

            let mut out: *mut c_char = ptr::null_mut();
            let rc =
                hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout);
            assert_eq!(rc, 1);
            assert_eq!(CStr::from_ptr(out).to_bytes(), b"hello");
            crate::cabi::free_cstring(out); // CSTRING-FREE: str-open (test)

            let rc =
                hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout);
            assert_eq!(rc, 1, "empty string element is Some(\"\"), not None");
            assert_eq!(CStr::from_ptr(out).to_bytes(), b"");
            crate::cabi::free_cstring(out); // CSTRING-FREE: str-open (test)

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    /// Closed + empty binds no value (rc 0) so codegen wraps `None`; the same
    /// holds for `try_recv` on an empty-but-open channel.
    #[test]
    fn layout_recv_closed_and_try_recv_empty_return_zero() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = plain_layout(8, 8);
            let mut out: i64 = -1;
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const layout,
            );
            assert_eq!(rc, 0, "empty channel must bind no value");

            hew_channel_sender_close(tx);
            let rc =
                hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout);
            assert_eq!(rc, 0, "closed channel must bind no value");

            hew_channel_receiver_close(rx);
        }
    }

    /// A Plain envelope of the wrong width is a malformed message: recv binds
    /// no value and records a diagnostic (mirrors the legacy int decode).
    #[test]
    fn layout_plain_width_mismatch_binds_none_with_error() {
        crate::hew_clear_error();
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let wide = plain_layout(8, 8);
            let value: i64 = 1;
            hew_channel_send_layout(tx, std::ptr::addr_of!(value).cast(), &raw const wide);

            let narrow = plain_layout(4, 4);
            let mut out: i32 = -1;
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(out).cast(),
                &raw const narrow,
            );
            assert_eq!(rc, 0);
            let err = CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected 4-byte element payload"),
                "unexpected error: {err}"
            );

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    // Heap-owning element thunks for the channel-level round trip. Same shape
    // as the `channel_core` mocks; separate statics keep counter attribution
    // local to this module's lock.
    static CH_OWNED_LOCK: Mutex<()> = Mutex::new(());
    static CH_OWNED_CLONES: AtomicUsize = AtomicUsize::new(0);
    static CH_OWNED_DROPS: AtomicUsize = AtomicUsize::new(0);

    #[repr(C)]
    struct ChOwnedElem {
        tag: u64,
        heap: *mut u8,
    }

    unsafe extern "C" fn ch_owned_clone(src: *const c_void, dst: *mut c_void) -> i32 {
        // SAFETY: thunk contract — src is a live element.
        let s = unsafe { &*src.cast::<ChOwnedElem>() };
        // SAFETY: thunk contract — dst holds a writable memcpy of src.
        let d = unsafe { &mut *dst.cast::<ChOwnedElem>() };
        // SAFETY: plain allocation; freed by ch_owned_drop.
        let dup = unsafe { libc::malloc(8).cast::<u8>() };
        if !s.heap.is_null() {
            // SAFETY: both buffers are 8 bytes.
            unsafe { std::ptr::copy_nonoverlapping(s.heap, dup, 8) };
        }
        d.heap = dup;
        CH_OWNED_CLONES.fetch_add(1, Ordering::SeqCst);
        0
    }

    unsafe extern "C" fn ch_owned_drop(slot: *mut c_void) {
        // SAFETY: thunk contract — slot is a live element being released.
        let e = unsafe { &mut *slot.cast::<ChOwnedElem>() };
        if !e.heap.is_null() {
            // SAFETY: heap was malloc'd by ch_owned_clone / the test body.
            unsafe { libc::free(e.heap.cast()) };
            e.heap = ptr::null_mut();
        }
        CH_OWNED_DROPS.fetch_add(1, Ordering::SeqCst);
    }

    fn ch_owned_layout() -> HewVecElemLayout {
        HewVecElemLayout {
            size: size_of::<ChOwnedElem>(),
            align: align_of::<ChOwnedElem>(),
            ownership_kind: HewTypeOwnershipKind::LayoutManaged,
            clone_fn: Some(ch_owned_clone),
            drop_fn: Some(ch_owned_drop),
        }
    }

    /// Heap-owning element round trip: send deep-copies in (clone 1), the
    /// caller keeps + frees its own value, recv moves out (no drop by the
    /// queue), and the consumer releases the received element exactly once.
    #[test]
    fn layout_roundtrip_owned_element_clone_on_send_move_on_recv() {
        let _g = CH_OWNED_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let clones_before = CH_OWNED_CLONES.load(Ordering::SeqCst);
        let drops_before = CH_OWNED_DROPS.load(Ordering::SeqCst);

        let pair = hew_channel_new(4);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = ch_owned_layout();
            let heap = libc::malloc(8).cast::<u8>();
            std::ptr::write_bytes(heap, 0x5A, 8);
            let value = ChOwnedElem { tag: 11, heap };
            hew_channel_send_layout(tx, std::ptr::addr_of!(value).cast(), &raw const layout);
            assert_eq!(
                CH_OWNED_CLONES.load(Ordering::SeqCst) - clones_before,
                1,
                "send deep-copies the element in exactly once"
            );
            // The caller still owns its value; release it independently.
            libc::free(value.heap.cast());

            let mut out = ChOwnedElem {
                tag: 0,
                heap: ptr::null_mut(),
            };
            let rc =
                hew_channel_recv_layout(rx, std::ptr::addr_of_mut!(out).cast(), &raw const layout);
            assert_eq!(rc, 1);
            assert_eq!(out.tag, 11);
            assert!(!out.heap.is_null(), "deep copy must own its own buffer");
            assert_eq!(
                CH_OWNED_DROPS.load(Ordering::SeqCst) - drops_before,
                0,
                "recv moves ownership out — the queue must not drop"
            );
            // The consumer owns the element; release it exactly once.
            ch_owned_drop(std::ptr::addr_of_mut!(out).cast());

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
        assert_eq!(CH_OWNED_DROPS.load(Ordering::SeqCst) - drops_before, 1);
    }

    /// Receiver dropped with owned items still queued: the queue releases each
    /// unconsumed envelope exactly once (no leak, no double-free).
    #[test]
    fn layout_receiver_close_releases_queued_owned_elements() {
        let _g = CH_OWNED_LOCK
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let drops_before = CH_OWNED_DROPS.load(Ordering::SeqCst);

        let pair = hew_channel_new(8);
        // SAFETY: pair was just created; slots are live locals.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let layout = ch_owned_layout();
            for tag in 0..2u64 {
                let heap = libc::malloc(8).cast::<u8>();
                let value = ChOwnedElem { tag, heap };
                hew_channel_send_layout(tx, std::ptr::addr_of!(value).cast(), &raw const layout);
                libc::free(value.heap.cast());
            }

            hew_channel_sender_close(tx);
            // Both envelopes are still queued; closing the receiver drops the
            // last core reference and must release each exactly once.
            hew_channel_receiver_close(rx);
        }
        assert_eq!(
            CH_OWNED_DROPS.load(Ordering::SeqCst) - drops_before,
            2,
            "queue teardown must release each unconsumed owned element once"
        );
    }
}
