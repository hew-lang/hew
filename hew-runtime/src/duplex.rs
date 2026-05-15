//! Hew runtime: dual-queue `HewDuplex` substrate.
//!
//! Substrate for the M2 unified-concurrency surface. A `HewDuplex<S, R>` is
//! a refcounted handle holding two independently bounded queues:
//!
//! - the **S-direction** queue, which this handle writes into;
//! - the **R-direction** queue, which this handle reads from.
//!
//! Construction surfaces:
//!
//! - [`HewDuplex::new_loopback`] — one handle whose S-direction and
//!   R-direction queues are the same allocation. Sends loop back to
//!   recv. Useful for trivial single-endpoint cases.
//! - [`HewDuplex::new_pair`] — two cross-wired handles `(a, b)` where
//!   `a.s_queue == b.r_queue` and `a.r_queue == b.s_queue`. The MIR
//!   surface `duplex_pair::<S, R>(N)` lowers to this.
//!
//! Half-handle extraction ([`HewDuplex::send_half`], [`HewDuplex::recv_half`])
//! splits the unified Duplex handle into direction-only aliases (`SendHalf`
//! holds only the S-write capability; `RecvHalf` holds only the R-read
//! capability) per the slice-3 MIR `SendHalf` / `RecvHalf` `Place` variants.
//!
//! ## Close protocol
//!
//! Each direction (S, R) tracks two refcounts on its queue:
//!
//! - **senders** — the number of capabilities permitting writes to this
//!   queue. A `HewDuplex` handle holds 1 sender on its S-queue. A
//!   `HewSendHalf` holds 1 sender on the parent's S-queue.
//! - **receivers** — the number of capabilities permitting reads from
//!   this queue. A `HewDuplex` handle holds 1 receiver on its R-queue.
//!   A `HewRecvHalf` holds 1 receiver on the parent's R-queue.
//!
//! When a queue's `senders` reaches zero, the queue is closed for writes;
//! pending receivers wake and observe `RecvError::Closed`. When a queue's
//! `receivers` reaches zero, the queue is closed for reads; in-flight
//! sends complete normally if there is space, then subsequent sends
//! fail with `SendError::Closed`.
//!
//! Dropping a unified `HewDuplex` decrements *both* directions' senders
//! and receivers (close-both-directions). Dropping a `HewSendHalf`
//! decrements only the S-queue's `senders`; dropping a `HewRecvHalf`
//! decrements only the R-queue's `receivers`.
//!
//! ## §5.7 race discipline
//!
//! No `thread::sleep`. Blocking `send` / `recv` use `Condvar::wait`
//! (`not_full` / `not_empty`) and signal on every state change.

#![cfg_attr(target_arch = "wasm32", allow(dead_code))]
#![cfg(not(target_arch = "wasm32"))]
// SendError / RecvError are exhaustive discriminants — their docstrings
// enumerate every failure mode. The `# Errors` section would be a verbatim
// restatement of the type. Refcount-bump clones (`clone_handle`) are
// intentionally side-effecting (they touch atomic refcounts), so the
// `must_use` clippy lint is noise rather than signal here.
#![allow(
    clippy::missing_errors_doc,
    reason = "SendError/RecvError enumerate every failure mode in their type docs"
)]
#![allow(
    clippy::must_use_candidate,
    reason = "clone_handle is intentionally side-effecting via atomic refcount bumps"
)]

use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::util::{CondvarExt, MutexExt};

// ── Error discriminants ────────────────────────────────────────────────────
//
// Mirrored verbatim by the C-ABI surface (`hew_duplex_send` returns these
// as `i32`). The discriminants are stable; codegen pattern-matches them.

/// Failure modes for [`Queue::send`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum SendError {
    /// Send succeeded.
    Ok = 0,
    /// The queue's receiver-refcount reached zero before the send
    /// completed: no consumer can observe the message.
    Closed = 1,
    /// A `try_send` saw the queue full (capacity exhausted) and the
    /// caller asked for non-blocking semantics. Surfaced for the
    /// `drop_new` backpressure mode (TBD post-M2).
    Full = 2,
    /// The lambda-actor's external strong refcount reached zero before
    /// this send (the body-side weak-ref upgrade failed). Specific to
    /// the lambda-actor wrapper.
    ActorStopped = 3,
}

/// Failure modes for [`Queue::recv`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(i32)]
pub enum RecvError {
    /// Recv succeeded.
    Ok = 0,
    /// The queue's sender-refcount reached zero and the queue is
    /// drained: no further messages will arrive.
    Closed = 1,
    /// A `try_recv` saw the queue empty and the caller asked for
    /// non-blocking semantics.
    Empty = 2,
}

// ── Per-direction queue ────────────────────────────────────────────────────

/// One direction's bounded queue. Shared (via `Arc`) between the
/// unified `HewDuplex`, the paired `HewDuplex`, and any half-handle
/// aliases the surface produces.
#[derive(Debug)]
pub struct Queue {
    /// Backing buffer + capacity. The `Mutex` is the linearisation
    /// point for every state transition.
    state: Mutex<QueueState>,
    /// Signalled when the queue transitions empty → non-empty, or
    /// when `senders` drops to zero (so blocked receivers wake and
    /// observe `Closed`).
    not_empty: Condvar,
    /// Signalled when the queue transitions full → non-full, or when
    /// `receivers` drops to zero (so blocked senders wake and observe
    /// `Closed`).
    not_full: Condvar,
    /// Active sender-capability count. The queue closes for writes
    /// when this reaches zero.
    senders: AtomicUsize,
    /// Active receiver-capability count. The queue closes for reads
    /// when this reaches zero.
    receivers: AtomicUsize,
    /// Snapshot flag used by `try_*` paths to short-circuit without
    /// acquiring the mutex. Lazily synchronised with the refcounts;
    /// authoritative state is recomputed under the mutex.
    closed_for_send: AtomicBool,
    closed_for_recv: AtomicBool,
}

#[derive(Debug)]
struct QueueState {
    buffer: VecDeque<Vec<u8>>,
    capacity: usize,
}

impl Queue {
    fn new(capacity: usize) -> Arc<Self> {
        Arc::new(Self {
            state: Mutex::new(QueueState {
                buffer: VecDeque::new(),
                capacity: capacity.max(1),
            }),
            not_empty: Condvar::new(),
            not_full: Condvar::new(),
            // Constructed with zero capabilities — the caller bumps
            // when handing the queue to a Duplex / SendHalf / RecvHalf.
            senders: AtomicUsize::new(0),
            receivers: AtomicUsize::new(0),
            closed_for_send: AtomicBool::new(false),
            closed_for_recv: AtomicBool::new(false),
        })
    }

    /// Atomically bump the sender-capability count. Returns the prior
    /// value purely for assertion / diagnostics; callers do not gate
    /// behaviour on it.
    fn acquire_sender(&self) {
        self.senders.fetch_add(1, Ordering::AcqRel);
    }

    fn acquire_receiver(&self) {
        self.receivers.fetch_add(1, Ordering::AcqRel);
    }

    /// Release a sender capability. When the last sender goes, the
    /// queue closes for writes; any blocked receivers wake.
    fn release_sender(&self) {
        let prev = self.senders.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev > 0, "release_sender below zero");
        if prev == 1 {
            self.closed_for_send.store(true, Ordering::Release);
            // Wake any receiver blocked in `not_empty.wait` so it can
            // observe the close.
            let _g = self.state.lock_or_recover();
            self.not_empty.notify_all();
            // Also wake senders blocked on capacity — though by
            // construction the last sender just left, this is defensive
            // (parallel senders could be mid-wait when the count hits
            // zero from another side).
            self.not_full.notify_all();
        }
    }

    fn release_receiver(&self) {
        let prev = self.receivers.fetch_sub(1, Ordering::AcqRel);
        debug_assert!(prev > 0, "release_receiver below zero");
        if prev == 1 {
            self.closed_for_recv.store(true, Ordering::Release);
            let _g = self.state.lock_or_recover();
            self.not_full.notify_all();
            self.not_empty.notify_all();
        }
    }

    /// Blocking send. Copies `msg` into the queue's buffer. Returns
    /// `SendError::Closed` if `receivers` reached zero while waiting
    /// or before entry.
    fn send(&self, msg: Vec<u8>) -> SendError {
        if self.closed_for_recv.load(Ordering::Acquire) {
            return SendError::Closed;
        }
        let mut state = self.state.lock_or_recover();
        loop {
            // Re-check under the lock — close-for-recv might have
            // happened between the entry probe and acquiring the
            // mutex.
            if self.receivers.load(Ordering::Acquire) == 0 {
                return SendError::Closed;
            }
            if state.buffer.len() < state.capacity {
                state.buffer.push_back(msg);
                self.not_empty.notify_one();
                return SendError::Ok;
            }
            state = self.not_full.wait_or_recover(state);
        }
    }

    /// Non-blocking send. Returns `Full` if the queue is at capacity
    /// and there is at least one receiver still alive.
    fn try_send(&self, msg: Vec<u8>) -> SendError {
        if self.closed_for_recv.load(Ordering::Acquire) {
            return SendError::Closed;
        }
        let mut state = self.state.lock_or_recover();
        if self.receivers.load(Ordering::Acquire) == 0 {
            return SendError::Closed;
        }
        if state.buffer.len() < state.capacity {
            state.buffer.push_back(msg);
            self.not_empty.notify_one();
            SendError::Ok
        } else {
            SendError::Full
        }
    }

    /// Blocking recv. Returns the next buffered message, or
    /// `RecvError::Closed` once the queue is drained AND `senders`
    /// has reached zero.
    fn recv(&self) -> Result<Vec<u8>, RecvError> {
        let mut state = self.state.lock_or_recover();
        loop {
            if let Some(msg) = state.buffer.pop_front() {
                self.not_full.notify_one();
                return Ok(msg);
            }
            // Buffer empty: only Closed once no sender remains. If a
            // sender is alive, wait for it to push.
            if self.senders.load(Ordering::Acquire) == 0 {
                return Err(RecvError::Closed);
            }
            state = self.not_empty.wait_or_recover(state);
        }
    }

    /// Non-blocking recv.
    fn try_recv(&self) -> Result<Vec<u8>, RecvError> {
        let mut state = self.state.lock_or_recover();
        if let Some(msg) = state.buffer.pop_front() {
            self.not_full.notify_one();
            return Ok(msg);
        }
        if self.senders.load(Ordering::Acquire) == 0 {
            Err(RecvError::Closed)
        } else {
            Err(RecvError::Empty)
        }
    }
}

// ── Duplex handle ──────────────────────────────────────────────────────────

/// Unified Duplex handle. Holds one sender-cap on its S-queue and one
/// receiver-cap on its R-queue.
///
/// Cloning the handle bumps both refcounts; dropping a clone releases
/// both (close-both-dirs is the per-handle drop contract).
#[derive(Debug)]
pub struct HewDuplex {
    s_queue: Arc<Queue>,
    r_queue: Arc<Queue>,
}

impl HewDuplex {
    /// Construct a self-loopback Duplex. Sends to `s_queue` are
    /// observable via `r_queue` (because they are the same queue).
    /// The loopback shape is what `hew_duplex_new` exposes for the
    /// solo-construction surface; the more useful shape is `new_pair`.
    pub fn new_loopback(capacity: usize) -> Self {
        let queue = Queue::new(capacity);
        queue.acquire_sender();
        queue.acquire_receiver();
        Self {
            s_queue: Arc::clone(&queue),
            r_queue: queue,
        }
    }

    /// Construct a cross-wired pair. Handle `a`'s S-queue is handle
    /// `b`'s R-queue and vice versa. Each handle starts with exactly
    /// one sender on its outbound queue and one receiver on its
    /// inbound queue.
    pub fn new_pair(s_cap: usize, r_cap: usize) -> (Self, Self) {
        // Naming convention: q_ab carries `a -> b` traffic (a sends, b receives).
        let q_ab = Queue::new(s_cap);
        let q_ba = Queue::new(r_cap);
        // a writes to q_ab, reads from q_ba.
        q_ab.acquire_sender();
        q_ba.acquire_receiver();
        // b writes to q_ba, reads from q_ab.
        q_ba.acquire_sender();
        q_ab.acquire_receiver();
        let a = HewDuplex {
            s_queue: Arc::clone(&q_ab),
            r_queue: Arc::clone(&q_ba),
        };
        let b = HewDuplex {
            s_queue: q_ba,
            r_queue: q_ab,
        };
        (a, b)
    }

    /// Send a payload on the S-direction. Blocks while the queue is
    /// full and at least one receiver remains.
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        self.s_queue.send(msg)
    }

    /// Non-blocking variant of [`send`].
    pub fn try_send(&self, msg: Vec<u8>) -> SendError {
        self.s_queue.try_send(msg)
    }

    /// Receive from the R-direction. Blocks until a message arrives
    /// or every sender on the R-direction has dropped.
    pub fn recv(&self) -> Result<Vec<u8>, RecvError> {
        self.r_queue.recv()
    }

    pub fn try_recv(&self) -> Result<Vec<u8>, RecvError> {
        self.r_queue.try_recv()
    }

    /// Split into a `SendHalf` retaining only S-write capability.
    /// Consumes one sender-cap from the original handle's S-queue
    /// budget: callers MUST drop the unified handle after splitting
    /// (the MIR layer enforces this via `Place::SendHalf(parent)`).
    pub fn into_send_half(self) -> HewSendHalf {
        // Steal the s_queue handle (its sender-cap is already counted
        // for this Duplex). Release the r_queue's receiver-cap because
        // SendHalf does not hold one.
        self.r_queue.release_receiver();
        let s_queue = Arc::clone(&self.s_queue);
        // Suppress the unified Drop path so we don't double-release
        // the s_queue's sender-cap.
        std::mem::forget(self);
        HewSendHalf { s_queue }
    }

    /// Split into a `RecvHalf` retaining only R-read capability.
    pub fn into_recv_half(self) -> HewRecvHalf {
        self.s_queue.release_sender();
        let r_queue = Arc::clone(&self.r_queue);
        std::mem::forget(self);
        HewRecvHalf { r_queue }
    }

    /// Refcount-bump clone. Both directions get a new capability,
    /// matching the unified handle's two-cap contract.
    #[must_use = "dropping the clone immediately releases the refcount it just bumped"]
    pub fn clone_handle(&self) -> Self {
        self.s_queue.acquire_sender();
        self.r_queue.acquire_receiver();
        Self {
            s_queue: Arc::clone(&self.s_queue),
            r_queue: Arc::clone(&self.r_queue),
        }
    }
}

impl Drop for HewDuplex {
    fn drop(&mut self) {
        // Close-both-directions: release the sender-cap held against
        // s_queue and the receiver-cap held against r_queue.
        self.s_queue.release_sender();
        self.r_queue.release_receiver();
    }
}

// ── Half handles ───────────────────────────────────────────────────────────

/// Send-only alias for a Duplex's S-direction. Holds exactly one
/// sender-cap on the parent S-queue.
#[derive(Debug)]
pub struct HewSendHalf {
    s_queue: Arc<Queue>,
}

impl HewSendHalf {
    pub fn send(&self, msg: Vec<u8>) -> SendError {
        self.s_queue.send(msg)
    }
    pub fn try_send(&self, msg: Vec<u8>) -> SendError {
        self.s_queue.try_send(msg)
    }
    #[must_use = "dropping the clone immediately releases the sender-cap it just bumped"]
    pub fn clone_handle(&self) -> Self {
        self.s_queue.acquire_sender();
        Self {
            s_queue: Arc::clone(&self.s_queue),
        }
    }
}

impl Drop for HewSendHalf {
    fn drop(&mut self) {
        self.s_queue.release_sender();
    }
}

/// Recv-only alias for a Duplex's R-direction. Holds exactly one
/// receiver-cap on the parent R-queue.
#[derive(Debug)]
pub struct HewRecvHalf {
    r_queue: Arc<Queue>,
}

impl HewRecvHalf {
    pub fn recv(&self) -> Result<Vec<u8>, RecvError> {
        self.r_queue.recv()
    }
    pub fn try_recv(&self) -> Result<Vec<u8>, RecvError> {
        self.r_queue.try_recv()
    }
    #[must_use = "dropping the clone immediately releases the receiver-cap it just bumped"]
    pub fn clone_handle(&self) -> Self {
        self.r_queue.acquire_receiver();
        Self {
            r_queue: Arc::clone(&self.r_queue),
        }
    }
}

impl Drop for HewRecvHalf {
    fn drop(&mut self) {
        self.r_queue.release_receiver();
    }
}

// ── C-ABI entries ──────────────────────────────────────────────────────────
//
// These are the symbols slice-5 codegen calls from emitted LLVM IR for
// `Duplex<S, R>` construction, send, recv, half-extraction, and close
// (`DropKind::DuplexClose` / `DropKind::DuplexHalfClose(Direction)` in MIR).
//
// Payload convention matches the existing `hew_channel_*` ABI: an opaque
// byte slice `(msg: *const u8, len: usize)` for send; a caller-owned
// out-buffer `(out: *mut u8, len: usize)` for recv. The receive ABI here
// reports the actual message length via an `out_len: *mut usize` pointer
// and signals truncation by returning a separate discriminant; callers
// (codegen, in slice 5) can round-trip arbitrary value types through this
// surface. The exact serialization of `S` and `R` payloads is the codegen
// layer's concern — runtime treats every payload as opaque bytes.

use std::ffi::c_void;
use std::ptr;

/// Direction selector for `hew_duplex_close_half`. Mirrors the MIR
/// `hew_mir::Direction` enum from slice 3's `DropKind::DuplexHalfClose`.
/// Discriminants are stable across the FFI boundary.
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HewDuplexDirection {
    /// S-direction (the queue the unified handle writes into).
    Send = 0,
    /// R-direction (the queue the unified handle reads from).
    Recv = 1,
}

/// Allocate a self-loopback Duplex with the given per-direction capacities.
/// `s_cap` and `r_cap` are accepted symmetrically so the ABI also covers
/// the future split-capacity case; the loopback uses one queue and
/// honours `s_cap` (R-cap is ignored — they're the same queue).
///
/// Returns null on invalid input.
///
/// # Safety
///
/// The returned pointer is owned by the runtime; callers must release it
/// with [`hew_duplex_close`] (close-both-dirs) or convert into halves with
/// [`hew_duplex_send_half`] / [`hew_duplex_recv_half`] before dropping.
#[no_mangle]
pub extern "C" fn hew_duplex_new(s_cap: usize, r_cap: usize) -> *mut HewDuplex {
    if s_cap == 0 || r_cap == 0 {
        crate::set_last_error(format!(
            "hew_duplex_new: capacity must be > 0 (got s_cap={s_cap}, r_cap={r_cap})"
        ));
        return ptr::null_mut();
    }
    let d = HewDuplex::new_loopback(s_cap);
    Box::into_raw(Box::new(d))
}

/// Allocate a cross-wired pair. On success writes the two handle
/// pointers into `out_a` and `out_b`. Both must be non-null.
///
/// # Safety
///
/// `out_a` and `out_b` must be non-null and writeable; they are
/// overwritten with handle pointers the caller owns thereafter (release
/// each via [`hew_duplex_close`]).
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_pair(
    s_cap: usize,
    r_cap: usize,
    out_a: *mut *mut HewDuplex,
    out_b: *mut *mut HewDuplex,
) -> i32 {
    if out_a.is_null() || out_b.is_null() {
        crate::set_last_error("hew_duplex_pair: out_a / out_b must not be null".to_string());
        return SendError::Closed as i32;
    }
    if s_cap == 0 || r_cap == 0 {
        crate::set_last_error(format!(
            "hew_duplex_pair: capacity must be > 0 (got s_cap={s_cap}, r_cap={r_cap})"
        ));
        return SendError::Closed as i32;
    }
    let (a, b) = HewDuplex::new_pair(s_cap, r_cap);
    let a_ptr = Box::into_raw(Box::new(a));
    let b_ptr = Box::into_raw(Box::new(b));
    // SAFETY:
    // - Provenance: caller-provided write targets verified non-null above.
    // - Type tag: target slot is `*mut HewDuplex`, matches what we write.
    // - Lifetime owner: ownership transfers from this function's Box-allocated
    //   handles to the caller-supplied slots; no aliasing arises.
    // - Aliasing concurrency: caller-supplied pointers may not be shared with
    //   another thread; this is the caller's contract for an FFI out-param.
    // - Bounds: writing a single pointer-sized value into a non-null slot.
    // - Failure mode: precondition checked above; if upheld, write cannot fault.
    unsafe {
        ptr::write(out_a, a_ptr);
        ptr::write(out_b, b_ptr);
    }
    SendError::Ok as i32
}

/// Send a payload on the unified Duplex's S-direction. Returns the
/// `SendError` discriminant as `i32` (0 = Ok, 1 = Closed, ...).
///
/// Blocks if the queue is full and at least one receiver remains.
///
/// # Safety
///
/// `d` must point to a valid Duplex returned by `hew_duplex_new` or
/// `hew_duplex_pair`. `msg` must be valid for `len` bytes (may be null
/// if `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_send(d: *mut HewDuplex, msg: *const u8, len: usize) -> i32 {
    if d.is_null() {
        crate::set_last_error("hew_duplex_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // SAFETY:
    // - Provenance: `msg` is caller-owned; we copy into an owned Vec, so
    //   no pointer escapes this frame.
    // - Type tag: bytes are opaque; we don't reinterpret.
    // - Lifetime owner: payload becomes owned by the Vec the moment we
    //   copy, before the function returns.
    // - Aliasing concurrency: caller must not mutate `msg` concurrently
    //   with this call (standard FFI immutable-read contract).
    // - Bounds: `len` bytes read from `msg`; null `msg` is only legal
    //   when `len == 0`, which is the empty-slice case.
    // - Failure mode: on null+nonzero we'd UB; rejected above (len == 0
    //   yields an empty Vec without dereferencing).
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_duplex_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY: see the six-axis breakdown of the `msg` pointer above
        // the `let payload` binding — null + non-zero rejected; remaining
        // case is a caller-owned non-null `msg` valid for `len` bytes;
        // `to_vec` copies before this frame returns.
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY:
    // - Provenance: caller guarantees `d` came from hew_duplex_new / pair.
    // - Type tag: `*mut HewDuplex` matches the originating type.
    // - Lifetime owner: caller still owns the box; we take only a shared
    //   reference for the send call.
    // - Aliasing concurrency: `HewDuplex` is Sync (every field's mutation
    //   is guarded by an internal Mutex / atomic), so shared access from
    //   parallel threads is sound.
    // - Bounds: dereferencing a single non-null aligned pointer to a Sized type.
    // - Failure mode: dangling handle is the caller's contract violation;
    //   the runtime cannot detect this and behaviour is UB per FFI norms.
    let res = unsafe { (*d).send(payload) };
    res as i32
}

/// Non-blocking variant of [`hew_duplex_send`]. Returns
/// `SendError::Full` if the queue is at capacity.
///
/// # Safety
///
/// Same as `hew_duplex_send`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_try_send(d: *mut HewDuplex, msg: *const u8, len: usize) -> i32 {
    if d.is_null() {
        crate::set_last_error("hew_duplex_try_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_duplex_try_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY: see hew_duplex_send for the six-axis breakdown — the
        // contract is identical (caller-owned immutable slice for `len`
        // bytes; copied into an owned Vec before return).
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY: see hew_duplex_send for the six-axis breakdown of
    // `*mut HewDuplex` dereference. `try_send` does not block, but the
    // pointer-validity contract is identical.
    let res = unsafe { (*d).try_send(payload) };
    res as i32
}

/// Block until a message is available on the unified Duplex's
/// R-direction. The payload is allocated by the runtime (malloc'd via
/// the system allocator) and ownership transfers to the caller, who
/// must release it with [`hew_duplex_payload_free`]. On success,
/// `*out_ptr` points at the payload bytes and `*out_len` carries the
/// length. On closed-or-error, `*out_ptr` is null, `*out_len` is 0,
/// and the return value carries the error discriminant.
///
/// # Safety
///
/// `d`, `out_ptr`, and `out_len` must all be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_recv(
    d: *mut HewDuplex,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if d.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_duplex_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_send dereference axis-by-axis; identical
    // (Sync handle, non-null aligned pointer, single shared borrow).
    let outcome = unsafe { (*d).recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: `out_ptr` / `out_len` validity checked above; we
            // write one pointer + one usize into caller-owned slots.
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: out-param slots null-checked above.
            unsafe {
                ptr::write(out_ptr, ptr::null_mut());
                ptr::write(out_len, 0usize);
            }
            e as i32
        }
    }
}

/// Non-blocking variant of [`hew_duplex_recv`]. Returns
/// `RecvError::Empty` if no message is waiting.
///
/// # Safety
///
/// Same as `hew_duplex_recv`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_try_recv(
    d: *mut HewDuplex,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if d.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_duplex_try_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_recv — identical six-axis profile.
    let outcome = unsafe { (*d).try_recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: out-param slots null-checked above.
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: out-param slots null-checked above.
            unsafe {
                ptr::write(out_ptr, ptr::null_mut());
                ptr::write(out_len, 0usize);
            }
            e as i32
        }
    }
}

/// Release a payload buffer returned by [`hew_duplex_recv`].
///
/// # Safety
///
/// `ptr` must have come from a successful [`hew_duplex_recv`] (or
/// `try_recv`) call and `len` must match its reported length. The
/// buffer is freed in place; the pointer is invalid thereafter.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_payload_free(ptr: *mut u8, len: usize) {
    if ptr.is_null() {
        return;
    }
    // SAFETY:
    // - Provenance: caller guarantees ptr came from a Box<[u8]> we
    //   leaked via `Box::into_raw(buf)` where buf was the boxed slice.
    // - Type tag: reconstituting `*mut [u8]` from the data ptr + len.
    // - Lifetime owner: ownership returns from the caller to this
    //   frame; the Box drop reclaims the heap allocation.
    // - Aliasing concurrency: caller must not retain a live alias to
    //   the payload past the free call (standard free contract).
    // - Bounds: slice length matches what `hew_duplex_recv` wrote into
    //   `*out_len` — caller's responsibility per the docstring.
    // - Failure mode: a mismatched `len` is UB per Box's contract;
    //   detection is not possible at this layer.
    unsafe {
        let slice = std::slice::from_raw_parts_mut(ptr, len);
        drop(Box::from_raw(slice as *mut [u8]));
    }
}

/// Close-both-directions and free the Duplex handle. Decrements both
/// sender and receiver caps on the underlying queues; if any cap was
/// the last alive, that direction's blocked peer wakes with `Closed`.
///
/// # Safety
///
/// `d` must have been returned by [`hew_duplex_new`] / [`hew_duplex_pair`]
/// and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_close(d: *mut HewDuplex) {
    if d.is_null() {
        return;
    }
    // SAFETY:
    // - Provenance: caller guarantees `d` came from hew_duplex_new /
    //   hew_duplex_pair, which both call `Box::into_raw`.
    // - Type tag: `*mut HewDuplex` matches the Box's element type.
    // - Lifetime owner: ownership returns to this frame for the
    //   single, final drop. Caller's contract pins exclusivity.
    // - Aliasing concurrency: this is the destructor — exclusive
    //   ownership is required by the caller's contract for a final
    //   close call (cf. close-on-last-drop in the MIR drop plan).
    // - Bounds: a single fat-aware free; no offset arithmetic.
    // - Failure mode: double-free is the caller's contract violation;
    //   detection requires guard pages or a poison flag, neither of
    //   which the substrate carries today.
    unsafe {
        drop(Box::from_raw(d));
    }
}

/// Convert a unified Duplex into a `HewSendHalf` (send-only alias).
/// Consumes the input handle (caller MUST NOT use `d` after this call).
/// The returned `HewSendHalf` pointer must be released via
/// [`hew_duplex_send_half_close`] / [`hew_duplex_close_half`] with
/// `Direction::Send`.
///
/// # Safety
///
/// `d` must have been returned by `hew_duplex_new` / `hew_duplex_pair`.
/// On success `d` is consumed and must not be used again.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_send_half(d: *mut HewDuplex) -> *mut HewSendHalf {
    if d.is_null() {
        crate::set_last_error("hew_duplex_send_half: null handle".to_string());
        return ptr::null_mut();
    }
    // SAFETY:
    // - Provenance: caller guarantees Box origin.
    // - Type tag: `*mut HewDuplex` matches Box element.
    // - Lifetime owner: this call consumes the Duplex (the API contract
    //   declares `d` invalid afterwards). We reconstitute the Box,
    //   call into_send_half (which forgets the Duplex internally), and
    //   wrap the result in a new Box.
    // - Aliasing concurrency: exclusive ownership required, as for any
    //   consuming operation.
    // - Bounds: single fat-aware reconstitution.
    // - Failure mode: caller reusing `d` is UB; not detectable here.
    // SAFETY (Box::from_raw): see above contract — pointer is a
    // Box<HewDuplex> produced by Box::into_raw with exclusive ownership.
    let duplex = unsafe { Box::from_raw(d) };
    // Move out of the Box without re-running Drop: take the inner value.
    // `into_send_half` handles the cap accounting (drops r_queue
    // receiver-cap, keeps s_queue sender-cap).
    let send_half = (*duplex).into_send_half();
    // The Box itself was zero-sized after the move-out; deallocating it
    // is a regular Box deallocation — the inner Duplex is gone.
    Box::into_raw(Box::new(send_half))
}

/// Convert a unified Duplex into a `HewRecvHalf` (recv-only alias).
///
/// # Safety
///
/// Same as `hew_duplex_send_half`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_recv_half(d: *mut HewDuplex) -> *mut HewRecvHalf {
    if d.is_null() {
        crate::set_last_error("hew_duplex_recv_half: null handle".to_string());
        return ptr::null_mut();
    }
    // SAFETY: see hew_duplex_send_half — symmetric profile, consuming
    // the input Duplex per the caller contract.
    // SAFETY (Box::from_raw): see hew_duplex_send_half — symmetric.
    let duplex = unsafe { Box::from_raw(d) };
    let recv_half = (*duplex).into_recv_half();
    Box::into_raw(Box::new(recv_half))
}

/// Send a payload on a `HewSendHalf`.
///
/// # Safety
///
/// `half` must point to a valid `HewSendHalf` returned by
/// [`hew_duplex_send_half`] (or its `*_clone` companion). `msg` must
/// be valid for `len` bytes (null if `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_send_half_send(
    half: *mut HewSendHalf,
    msg: *const u8,
    len: usize,
) -> i32 {
    if half.is_null() {
        crate::set_last_error("hew_send_half_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_send_half_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY: see hew_duplex_send dereference axis; identical contract.
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY: see hew_duplex_send — pointer-validity contract identical;
    // HewSendHalf is Sync via its internal Arc<Queue>.
    let res = unsafe { (*half).send(payload) };
    res as i32
}

/// Receive a payload from a `HewRecvHalf`.
///
/// # Safety
///
/// `half`, `out_ptr`, and `out_len` must all be valid pointers; the
/// payload is owned by the caller and must be released with
/// [`hew_duplex_payload_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_recv_half_recv(
    half: *mut HewRecvHalf,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if half.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_recv_half_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_recv — symmetric profile.
    let outcome = unsafe { (*half).recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: out-param slots null-checked above.
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: out-param slots null-checked above.
            unsafe {
                ptr::write(out_ptr, ptr::null_mut());
                ptr::write(out_len, 0usize);
            }
            e as i32
        }
    }
}

/// Close-one-direction on a Duplex. Used by codegen for the MIR
/// `DropKind::DuplexHalfClose(Direction)` shape: the half-handle's
/// Drop fires this entry rather than [`hew_duplex_close`].
///
/// Behaviour:
///
/// - `direction == Send`: treats `half` as a `HewSendHalf*`; frees it
///   (releasing the `s_queue` sender-cap).
/// - `direction == Recv`: treats `half` as a `HewRecvHalf*`; frees it
///   (releasing the `r_queue` receiver-cap).
///
/// # Safety
///
/// `half` must point to the matching half-handle type for `direction`.
/// A mismatched direction is UB.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_close_half(half: *mut c_void, direction: i32) {
    if half.is_null() {
        return;
    }
    match direction {
        x if x == HewDuplexDirection::Send as i32 => {
            // SAFETY: per the caller contract, `half` is a `*mut HewSendHalf`.
            unsafe { drop(Box::from_raw(half.cast::<HewSendHalf>())) };
        }
        x if x == HewDuplexDirection::Recv as i32 => {
            // SAFETY: per the caller contract, `half` is a `*mut HewRecvHalf`.
            unsafe { drop(Box::from_raw(half.cast::<HewRecvHalf>())) };
        }
        _ => {
            crate::set_last_error(format!(
                "hew_duplex_close_half: invalid direction discriminant {direction}"
            ));
        }
    }
}

/// Refcount-bump clone of a unified Duplex handle.
///
/// # Safety
///
/// `d` must be a valid Duplex handle.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_clone(d: *mut HewDuplex) -> *mut HewDuplex {
    if d.is_null() {
        crate::set_last_error("hew_duplex_clone: null handle".to_string());
        return ptr::null_mut();
    }
    // SAFETY: see hew_duplex_send for `*mut HewDuplex` dereference profile.
    let cloned = unsafe { (*d).clone_handle() };
    Box::into_raw(Box::new(cloned))
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn loopback_send_then_recv_roundtrips() {
        let d = HewDuplex::new_loopback(4);
        assert_eq!(d.send(b"hello".to_vec()), SendError::Ok);
        let got = d.recv().expect("recv");
        assert_eq!(got, b"hello");
    }

    #[test]
    fn pair_sends_cross_to_other_handle() {
        let (a, b) = HewDuplex::new_pair(4, 4);
        assert_eq!(a.send(b"a->b".to_vec()), SendError::Ok);
        assert_eq!(b.send(b"b->a".to_vec()), SendError::Ok);
        assert_eq!(b.recv().unwrap(), b"a->b");
        assert_eq!(a.recv().unwrap(), b"b->a");
    }

    #[test]
    fn close_both_dirs_on_last_handle_drop() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        // a sends one, then drops; b should still drain the message
        // and only after that observe Closed.
        a.send(b"final".to_vec());
        drop(a);
        assert_eq!(b.recv().unwrap(), b"final");
        // b's recv side is the queue a was writing to; senders=0 now.
        match b.recv() {
            Err(RecvError::Closed) => {}
            other => panic!("expected Closed after a dropped, got {other:?}"),
        }
        // And b's sends are also failing because a's drop released
        // a's receiver-cap on the b->a queue.
        assert_eq!(b.send(b"too late".to_vec()), SendError::Closed);
    }

    #[test]
    fn half_split_isolates_directions() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let a_send = a.into_send_half();
        let b_recv = b.into_recv_half();
        // a_send can still write into the a->b queue; b_recv can
        // still read from it.
        assert_eq!(a_send.send(b"split".to_vec()), SendError::Ok);
        assert_eq!(b_recv.recv().unwrap(), b"split");
        // b_recv has dropped its sender-cap on the b->a queue (via
        // into_recv_half), so the a-side cannot receive anything: we
        // didn't keep an a_recv. Verify b_recv's recv blocks only on
        // a_send's drop.
        drop(a_send);
        assert!(matches!(b_recv.recv(), Err(RecvError::Closed)));
    }

    #[test]
    fn clone_handle_holds_open() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let a2 = a.clone_handle();
        drop(a);
        // a2 still holds sender + receiver caps, so b is not closed.
        assert_eq!(a2.send(b"clone".to_vec()), SendError::Ok);
        assert_eq!(b.recv().unwrap(), b"clone");
        drop(a2);
        assert!(matches!(b.recv(), Err(RecvError::Closed)));
    }

    #[test]
    fn blocking_send_unblocks_on_drain() {
        let (a, b) = HewDuplex::new_pair(1, 1);
        // Fill capacity.
        assert_eq!(a.send(b"first".to_vec()), SendError::Ok);
        let a_addr = Box::into_raw(Box::new(a)) as usize;
        let h = thread::spawn(move || {
            // SAFETY: test owns the Box and reconstructs it exactly once.
            let a = unsafe { Box::from_raw(a_addr as *mut HewDuplex) };
            // This will block until b drains.
            let r = a.send(b"second".to_vec());
            assert_eq!(r, SendError::Ok);
        });
        assert_eq!(b.recv().unwrap(), b"first");
        // Drain second too — sender thread should complete shortly.
        assert_eq!(b.recv().unwrap(), b"second");
        h.join().unwrap();
    }

    #[test]
    fn try_send_full_when_buffer_at_capacity() {
        let d = HewDuplex::new_loopback(1);
        assert_eq!(d.try_send(b"a".to_vec()), SendError::Ok);
        assert_eq!(d.try_send(b"b".to_vec()), SendError::Full);
    }

    #[test]
    fn try_recv_empty_when_no_messages() {
        let d = HewDuplex::new_loopback(2);
        assert!(matches!(d.try_recv(), Err(RecvError::Empty)));
    }

    // ── C-ABI tests ────────────────────────────────────────────────────────

    #[test]
    fn cabi_new_send_recv_close_loopback() {
        let d = hew_duplex_new(4, 4);
        assert!(!d.is_null());
        let msg = b"abi-loop";
        // SAFETY: d non-null, msg valid for its length, payload-free
        // contract per docstring.
        unsafe {
            let send_rc = hew_duplex_send(d, msg.as_ptr(), msg.len());
            assert_eq!(send_rc, SendError::Ok as i32);
            let mut out_ptr: *mut u8 = ptr::null_mut();
            let mut out_len: usize = 0;
            let recv_rc = hew_duplex_recv(
                d,
                std::ptr::addr_of_mut!(out_ptr),
                std::ptr::addr_of_mut!(out_len),
            );
            assert_eq!(recv_rc, RecvError::Ok as i32);
            assert_eq!(out_len, msg.len());
            let recovered = std::slice::from_raw_parts(out_ptr, out_len);
            assert_eq!(recovered, msg);
            hew_duplex_payload_free(out_ptr, out_len);
            hew_duplex_close(d);
        }
    }

    #[test]
    fn cabi_pair_cross_wires() {
        let mut a: *mut HewDuplex = ptr::null_mut();
        let mut b: *mut HewDuplex = ptr::null_mut();
        // SAFETY: out_a / out_b are valid stack slots; capacities positive.
        let rc =
            unsafe { hew_duplex_pair(4, 4, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        assert_eq!(rc, SendError::Ok as i32);
        assert!(!a.is_null() && !b.is_null());
        // SAFETY: handles came from hew_duplex_pair above.
        unsafe {
            let payload = b"a-to-b";
            assert_eq!(
                hew_duplex_send(a, payload.as_ptr(), payload.len()),
                SendError::Ok as i32
            );
            let mut p: *mut u8 = ptr::null_mut();
            let mut n: usize = 0;
            let rc = hew_duplex_recv(b, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n));
            assert_eq!(rc, RecvError::Ok as i32);
            let bytes = std::slice::from_raw_parts(p, n).to_vec();
            assert_eq!(bytes, payload);
            hew_duplex_payload_free(p, n);
            hew_duplex_close(a);
            hew_duplex_close(b);
        }
    }

    #[test]
    fn cabi_close_both_dirs_on_last_drop() {
        let mut a: *mut HewDuplex = ptr::null_mut();
        let mut b: *mut HewDuplex = ptr::null_mut();
        // SAFETY: valid out-params.
        unsafe {
            assert_eq!(
                hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b),),
                SendError::Ok as i32
            );
            // a sends final, then closes; b drains and then sees Closed.
            let payload = b"bye";
            hew_duplex_send(a, payload.as_ptr(), payload.len());
            hew_duplex_close(a);
            let mut p: *mut u8 = ptr::null_mut();
            let mut n: usize = 0;
            assert_eq!(
                hew_duplex_recv(b, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)),
                RecvError::Ok as i32
            );
            hew_duplex_payload_free(p, n);
            // Now no senders remain.
            assert_eq!(
                hew_duplex_recv(b, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)),
                RecvError::Closed as i32
            );
            assert!(p.is_null());
            // b's send fails closed (a released receiver).
            let payload2 = b"too-late";
            assert_eq!(
                hew_duplex_send(b, payload2.as_ptr(), payload2.len()),
                SendError::Closed as i32
            );
            hew_duplex_close(b);
        }
    }

    #[test]
    fn cabi_half_split_and_close_directions() {
        let mut a: *mut HewDuplex = ptr::null_mut();
        let mut b: *mut HewDuplex = ptr::null_mut();
        // SAFETY: stack out-slots.
        unsafe {
            hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
            let a_send = hew_duplex_send_half(a);
            let b_recv = hew_duplex_recv_half(b);
            assert!(!a_send.is_null() && !b_recv.is_null());
            let payload = b"split-abi";
            assert_eq!(
                hew_send_half_send(a_send, payload.as_ptr(), payload.len()),
                SendError::Ok as i32
            );
            let mut p: *mut u8 = ptr::null_mut();
            let mut n: usize = 0;
            assert_eq!(
                hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n),),
                RecvError::Ok as i32
            );
            hew_duplex_payload_free(p, n);
            hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32);
            // After a_send closes, b_recv must see Closed (no senders).
            assert_eq!(
                hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n),),
                RecvError::Closed as i32
            );
            hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32);
        }
    }

    #[test]
    fn cabi_new_rejects_zero_capacity() {
        let d = hew_duplex_new(0, 4);
        assert!(d.is_null());
        let d2 = hew_duplex_new(4, 0);
        assert!(d2.is_null());
    }

    #[test]
    fn cabi_pair_rejects_null_out_param() {
        let mut a: *mut HewDuplex = ptr::null_mut();
        // SAFETY: deliberately exercising the null-out-param branch.
        let rc = unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), ptr::null_mut()) };
        assert_eq!(rc, SendError::Closed as i32);
    }
}
