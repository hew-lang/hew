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
}
