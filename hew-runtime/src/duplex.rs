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
//! - [`HewDuplex::new_pair`] — two cross-wired handles `(a, b)` where
//!   `a.s_queue == b.r_queue` and `a.r_queue == b.s_queue`. The MIR
//!   surface `duplex_pair::<S, R>(N)` lowers to this. This is the
//!   user-facing construction shape (a Duplex is a connection between
//!   two endpoints; cf. §8.3).
//! - [`HewDuplex::new_loopback`] — internal-only constructor where a
//!   single handle's S- and R-queues alias each other. Used by the
//!   lambda-actor mailbox composition and by tests. Not exposed on
//!   the C-ABI surface.
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

use crate::cluster::PartitionRegistry;
use crate::util::{CondvarExt, MutexExt};

// ── Error discriminants ────────────────────────────────────────────────────
//
// Mirrored verbatim by the C-ABI surface (`hew_duplex_send` returns these
// as `i32`). The discriminants are stable; codegen pattern-matches them.

/// Failure modes for [`Queue::send`].
///
/// Discriminants are stable across the C-ABI boundary; codegen
/// pattern-matches them as `i32`. New variants must be assigned
/// strictly increasing discriminant values so existing callers
/// (generated and hand-written) are never silently re-mapped.
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
    /// A close or release function was called on a handle that has
    /// already been closed. The inner resources were already released
    /// on the first close; this call is a no-op. Per design principle
    /// D4 ("Hew is designed for failure") this is a typed error rather
    /// than undefined behaviour.
    ///
    /// Exported as `4` across the C-ABI surface.
    DoubleClose = 4,
    /// An ask completed but the reply channel was orphaned because the
    /// actor's body panicked or returned a non-zero error code before
    /// delivering a reply. The reply out-parameter is null.
    ///
    /// Distinct from `ActorStopped` (which covers pre-send stop checks)
    /// so callers can distinguish "actor stopped before we sent" from
    /// "actor died during handling of our ask". Mirrors `AskError::OrphanedAsk`.
    ///
    /// Exported as `5` across the C-ABI surface.
    OrphanedAsk = 5,
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
    /// The recv side detected a node-level partition: the peer node
    /// is unreachable (heartbeat timeout or explicit partition injection
    /// in tests). Unlike `Closed`, the partition may heal — the
    /// application layer decides whether to retry or escalate.
    ///
    /// In v0.5, reachable via [`Queue::force_partition`] directly or
    /// through the `PartitionRegistry` cluster-membership seam when a
    /// peer is declared DEAD.
    ///
    /// WHY unit variant: `#[repr(i32)]` + stable ABI discriminants
    /// mean a payload (peer-id) would require a separate out-parameter
    /// or a repr change. The Q48/A25 spec says "marker + peer-id minimal
    /// for v0.5 (a)-tier"; peer-id expansion belongs to M3 with node
    /// peering (partition tier (b)).
    /// WHEN obsolete: when the multi-node peer layer lands (M3+) and
    /// adds a payload-carrying `PartitionDetectedWithPeer { peer_id }`
    /// variant on a richer type.
    /// WHAT the real solution looks like: a non-repr(i32) rich error type
    /// with an optional `PeerId` field, returned via an out-parameter.
    PartitionDetected = 3,
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
    /// Partition flag: when set, `recv`/`try_recv` return
    /// `RecvError::PartitionDetected` instead of blocking or delivering.
    ///
    /// Set by the partition-injection seam (`PartitionRegistry::on_member_dead`)
    /// when the cluster declares a peer node DEAD, or directly in tests via
    /// `force_partition`. Once set, it is not cleared — the caller decides
    /// whether to retry or escalate.
    partitioned: AtomicBool,
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
            partitioned: AtomicBool::new(false),
        })
    }

    /// Inject a partition signal: the next `recv` or `try_recv` will return
    /// `RecvError::PartitionDetected` instead of blocking or delivering.
    ///
    /// Wakes any blocked receiver immediately. Called by the
    /// `PartitionRegistry` fan-out when a cluster peer is declared DEAD,
    /// and directly by tests.
    ///
    /// Fail-closed: if no receiver is blocked, the flag persists so the
    /// next call to `recv`/`try_recv` observes the partition without delay.
    /// The flag is never cleared once set — the caller owns the decision
    /// to retry or escalate (per `RecvError::PartitionDetected` semantics).
    pub(crate) fn force_partition(&self) {
        self.partitioned.store(true, Ordering::Release);
        let _g = self.state.lock_or_recover();
        self.not_empty.notify_all();
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
    /// has reached zero, or `RecvError::PartitionDetected` if a
    /// partition was injected via [`Self::force_partition`]
    /// or the `PartitionRegistry` cluster-membership seam.
    fn recv(&self) -> Result<Vec<u8>, RecvError> {
        let mut state = self.state.lock_or_recover();
        loop {
            // Partition check takes priority over buffered messages:
            // once a partition is detected, the connection is considered
            // degraded and the caller decides whether to retry.
            if self.partitioned.load(Ordering::Acquire) {
                return Err(RecvError::PartitionDetected);
            }
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

    /// Non-blocking recv. Returns `RecvError::PartitionDetected` if a
    /// partition was injected, `RecvError::Empty` if no message is
    /// waiting, or `RecvError::Closed` if all senders dropped.
    fn try_recv(&self) -> Result<Vec<u8>, RecvError> {
        // Partition check before acquiring the mutex: avoids holding the
        // lock for a fast-path rejection.
        if self.partitioned.load(Ordering::Acquire) {
            return Err(RecvError::PartitionDetected);
        }
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
    /// This is an internal Rust constructor used by tests and by
    /// the lambda-actor mailbox composition; the user-facing surface
    /// is `new_pair`, which models the Duplex as a connection between
    /// two endpoints (cf. the §8.3 Duplex-as-TCP-connection design).
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
        // Move both `Arc<Queue>` fields out of `self` *by value* via
        // `ManuallyDrop`, so the unified handle's `Drop` is suppressed
        // (it would double-release the s_queue's sender-cap) yet the
        // moved Arcs still decrement their Arc refcount on drop. A
        // prior implementation cloned the Arc and `mem::forget`d the
        // outer handle — that stranded both `Arc<Queue>` refcounts
        // (each call leaked the inner allocation).
        let me = std::mem::ManuallyDrop::new(self);
        // SAFETY:
        // - Provenance: `&me.s_queue` / `&me.r_queue` are references
        //   into a `ManuallyDrop<HewDuplex>` we own by value; the
        //   ManuallyDrop suppresses the outer Drop so the Arcs are
        //   never freed by it.
        // - Type tag: each read reconstitutes `Arc<Queue>` from a
        //   reference of the same type.
        // - Lifetime owner: ownership transfers into the local
        //   bindings; `me` is never observed again.
        // - Aliasing concurrency: exclusive ownership inside this
        //   function frame.
        // - Bounds: pointer-aligned reads of `Sized` Arc fields.
        // - Failure mode: each field is read exactly once; no double
        //   move.
        let s_queue = unsafe { std::ptr::read(&raw const me.s_queue) };
        // SAFETY: identical contract — second of two single reads of
        // disjoint fields inside the ManuallyDrop-suppressed handle.
        let r_queue = unsafe { std::ptr::read(&raw const me.r_queue) };
        // SendHalf does not retain the receiver-cap on r_queue; release
        // it. The r_queue's Arc refcount decrements when the local
        // binding drops at function exit.
        r_queue.release_receiver();
        drop(r_queue);
        HewSendHalf { s_queue }
    }

    /// Split into a `RecvHalf` retaining only R-read capability.
    pub fn into_recv_half(self) -> HewRecvHalf {
        // See `into_send_half` for the ManuallyDrop + ptr::read
        // discipline; mirrored here for the R-direction.
        let me = std::mem::ManuallyDrop::new(self);
        // SAFETY: see `into_send_half` — symmetric six-axis profile.
        let s_queue = unsafe { std::ptr::read(&raw const me.s_queue) };
        // SAFETY: see `into_send_half` — second of two disjoint reads.
        let r_queue = unsafe { std::ptr::read(&raw const me.r_queue) };
        s_queue.release_sender();
        drop(s_queue);
        HewRecvHalf { r_queue }
    }

    /// Test-only access to the inner queue Arcs so a test can hold a
    /// `Weak<Queue>` and verify that splitting and dropping the halves
    /// frees the underlying queue allocation (regression for the
    /// pre-`ManuallyDrop` Arc leak in `into_send_half`/`into_recv_half`).
    #[cfg(test)]
    pub(crate) fn queue_arcs_for_test(&self) -> (Arc<Queue>, Arc<Queue>) {
        (Arc::clone(&self.s_queue), Arc::clone(&self.r_queue))
    }

    /// Inject a partition signal on the R-direction (recv side).
    /// The next `recv` or `try_recv` on this handle will return
    /// `RecvError::PartitionDetected`. Wakes any blocked receiver.
    ///
    /// See [`Queue::force_partition`].
    // WHY: force_partition is called in duplex unit tests and through the
    // register_recv_with_partition_registry seam. Clippy sees it as dead in
    // --lib mode (no tests); #[allow] suppresses without erroring in
    // --all-targets mode where tests make it live.
    #[allow(
        dead_code,
        reason = "called in unit tests and via register_recv_with_partition_registry seam; \
                  not invoked from crate lib code in non-test compilation"
    )]
    pub(crate) fn force_partition(&self) {
        self.r_queue.force_partition();
    }

    /// Register this handle's R-direction queue with a `PartitionRegistry`.
    ///
    /// When the cluster declares `node_id` DEAD, the registry will call
    /// `force_partition` on this queue, waking any blocked `recv` with
    /// `RecvError::PartitionDetected`.
    ///
    /// Only a `Weak` reference is stored — the registry does not extend
    /// the queue's lifetime. Dead refs are pruned automatically.
    pub fn register_recv_with_partition_registry(
        &self,
        registry: &PartitionRegistry,
        node_id: u16,
    ) {
        registry.register_remote_queue(node_id, Arc::downgrade(&self.r_queue));
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

    /// Inject a partition signal: the next `recv` or `try_recv` will
    /// return `RecvError::PartitionDetected`. Wakes any blocked receiver.
    ///
    /// See [`Queue::force_partition`].
    // WHY: see the matching comment on HewDuplex::force_partition above.
    #[allow(
        dead_code,
        reason = "called in unit tests and via register_recv_with_partition_registry seam; \
                  not invoked from crate lib code in non-test compilation"
    )]
    pub(crate) fn force_partition(&self) {
        self.r_queue.force_partition();
    }

    /// Register this recv half's queue with a `PartitionRegistry`.
    ///
    /// When the cluster declares `node_id` DEAD, the registry will call
    /// `force_partition` on this queue. Only a `Weak` ref is stored.
    pub fn register_recv_with_partition_registry(
        &self,
        registry: &PartitionRegistry,
        node_id: u16,
    ) {
        registry.register_remote_queue(node_id, Arc::downgrade(&self.r_queue));
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

    // ── C-ABI tests ────────────────────────────────────────────────────────

    #[test]
    fn into_send_half_frees_inner_queues_when_half_drops() {
        // Regression for the Arc<Queue> leak in the prior `clone +
        // mem::forget` implementation of into_send_half / into_recv_half:
        // each split stranded both inner Arcs (the queue allocations
        // were never freed even after every handle dropped).
        //
        // Hold `Weak<Queue>` snapshots; assert the in-use queue stays
        // alive while a half-handle still holds it, and that every
        // queue is freed once every handle is dropped — the canonical
        // Arc-leak diagnostic. The leak in the prior implementation
        // would have kept *every* Arc alive forever.
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (q_ab, q_ba) = a.queue_arcs_for_test();
        let weak_ab = Arc::downgrade(&q_ab);
        let weak_ba = Arc::downgrade(&q_ba);
        drop(q_ab);
        drop(q_ba);

        // Pre-split: both queues are alive (a and b each hold both).
        assert!(weak_ab.upgrade().is_some());
        assert!(weak_ba.upgrade().is_some());

        let a_send = a.into_send_half();
        // After splitting `a`, a's q_ba Arc binding is dropped; b
        // still holds q_ba so the Arc strong count > 0.
        assert!(
            weak_ab.upgrade().is_some(),
            "q_ab freed too early after split"
        );
        assert!(
            weak_ba.upgrade().is_some(),
            "q_ba freed too early after split"
        );

        let b_recv = b.into_recv_half();
        // After splitting `b`, b's q_ba Arc binding is dropped. Now
        // no one holds q_ba — it must be freed. q_ab is held by
        // both halves.
        assert!(
            weak_ab.upgrade().is_some(),
            "q_ab freed too early after both splits"
        );
        assert!(
            weak_ba.upgrade().is_none(),
            "q_ba should be freed once unused (leak: into_recv_half kept extra Arc ref)"
        );

        drop(a_send);
        drop(b_recv);

        // Every strong handle is gone. If into_send_half / into_recv_half
        // had leaked, q_ab would still upgrade here.
        assert!(
            weak_ab.upgrade().is_none(),
            "into_send_half / into_recv_half leaked Arc<Queue> refcount on q_ab"
        );
    }

    // ── Close-protocol ordering matrix ─────────────────────────────────────
    //
    // The unified Duplex and its two half aliases each carry one cap on
    // each of the two underlying queues. The total caps held on each
    // queue must net to zero exactly once across all orderings of
    // drop(unified), drop(send_half), drop(recv_half). Build each
    // permutation explicitly so a regression in `into_send_half` /
    // `into_recv_half` (which transfers a cap by moving an Arc out of
    // the unified handle) gets caught no matter which half drops first.
    //
    // For each ordering we verify, after the LAST drop:
    //   - both inner Queue allocations are freed (`Weak::upgrade` fails);
    //   - no panic occurred from a double-release in the Drop chain.
    //
    // Snapshot the inner Arcs via the test-only accessor, downgrade to
    // Weak refs, then drop the local strong handles so the only
    // remaining strong refs live in the unified/half handles.

    fn snapshot_weaks(d: &HewDuplex) -> (std::sync::Weak<Queue>, std::sync::Weak<Queue>) {
        let (s, r) = d.queue_arcs_for_test();
        let ws = Arc::downgrade(&s);
        let wr = Arc::downgrade(&r);
        (ws, wr)
    }

    #[test]
    fn close_ordering_unified_then_send_then_recv() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(a);
        drop(b_send);
        drop(b_recv);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 1");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 1");
    }

    #[test]
    fn close_ordering_unified_then_both_halves_concurrently() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(a);
        // "Simultaneous" drop expressed as a single tuple drop —
        // Rust drops fields in declaration order but the test asserts
        // only that after both halves go the queues are freed.
        let halves = (b_send, b_recv);
        drop(halves);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 2");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 2");
    }

    #[test]
    fn close_ordering_send_then_recv_then_unified() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(b_send);
        drop(b_recv);
        // Unified `a` still holds its caps; queues are alive.
        assert!(ws.upgrade().is_some(), "q_ab freed too early in order 3");
        assert!(wr.upgrade().is_some(), "q_ba freed too early in order 3");
        drop(a);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 3");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 3");
    }

    #[test]
    fn close_ordering_send_then_unified_then_recv() {
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(b_send);
        drop(a);
        // b_recv is still alive — q_ab still has b_recv as receiver.
        // q_ba may be freed depending on remaining caps, but at least
        // one queue is held.
        drop(b_recv);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 4");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 4");
    }

    #[test]
    fn close_ordering_recv_then_send_then_unified() {
        // Inverse of order 3: recv-half drops first, then send-half, then
        // the unified peer last. Verifies the close-protocol is order-symmetric
        // — recv-side closing first does not strand the q_ba queue or wedge
        // the still-live send-half.
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(b_recv);
        drop(b_send);
        // Unified `a` still holds its caps; both queues alive until a drops.
        assert!(ws.upgrade().is_some(), "q_ab freed too early in order 5");
        assert!(wr.upgrade().is_some(), "q_ba freed too early in order 5");
        drop(a);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 5");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 5");
    }

    #[test]
    fn close_ordering_recv_then_unified_then_send() {
        // Six-permutation closure: recv-first, unified-middle, send-last.
        let (a, b) = HewDuplex::new_pair(2, 2);
        let (ws, wr) = snapshot_weaks(&a);
        let b_send = b.clone_handle().into_send_half();
        let b_recv = b.into_recv_half();
        drop(b_recv);
        drop(a);
        // b_send is still alive — at least one queue is held.
        drop(b_send);
        assert!(ws.upgrade().is_none(), "q_ab leaked after order 6");
        assert!(wr.upgrade().is_none(), "q_ba leaked after order 6");
    }

    // ── Concurrent producer / close-during-block stress ────────────────────
    //
    // Stress the queue's locking discipline using a `Barrier` to launch
    // every thread simultaneously. No `thread::sleep`. Use `join` for
    // termination; close-during-blocking-recv synchronises through the
    // queue itself (the recv thread blocks; the producer thread drops
    // the send-side, which wakes the recv via the not_empty Condvar
    // signal in `release_sender`).

    #[test]
    fn n_concurrent_producers_deliver_every_message_exactly_once() {
        use std::sync::Barrier;
        const PRODUCERS: usize = 8;
        const PER_PRODUCER: usize = 64;
        let total = PRODUCERS * PER_PRODUCER;
        // Capacity matches total so the producer threads never block;
        // the test exercises locking under contention, not backpressure.
        let (sender, receiver) = HewDuplex::new_pair(total, 1);
        let sender = Arc::new(sender);
        let barrier = Arc::new(Barrier::new(PRODUCERS));
        let mut handles = Vec::new();
        for pid in 0..PRODUCERS {
            let s = Arc::clone(&sender);
            let b = Arc::clone(&barrier);
            handles.push(thread::spawn(move || {
                b.wait();
                for i in 0..PER_PRODUCER {
                    let payload = format!("p{pid}-{i}").into_bytes();
                    assert_eq!(s.send(payload), SendError::Ok);
                }
            }));
        }
        for h in handles {
            h.join().unwrap();
        }
        // Drain everything from the receiver side. Sort and compare to
        // the expected set: every message arrives exactly once.
        let mut got = Vec::with_capacity(total);
        for _ in 0..total {
            got.push(receiver.recv().expect("recv"));
        }
        got.sort();
        let mut expected = Vec::with_capacity(total);
        for pid in 0..PRODUCERS {
            for i in 0..PER_PRODUCER {
                expected.push(format!("p{pid}-{i}").into_bytes());
            }
        }
        expected.sort();
        assert_eq!(got, expected, "N-producer fan-in dropped or duplicated");
    }

    #[test]
    fn close_during_blocking_recv_wakes_recv_with_closed() {
        // The recv thread blocks on an empty queue; the main thread
        // drops the sole producer side; the recv thread must wake and
        // observe RecvError::Closed (NOT block indefinitely, NOT
        // deadlock).
        let (sender, receiver) = HewDuplex::new_pair(1, 1);
        let recv_thread = thread::spawn(move || receiver.recv());
        // Drop the sender side; release_sender's notify_all wakes the
        // blocked recv. The recv loop re-checks `senders == 0` under
        // the mutex and returns Closed.
        drop(sender);
        let result = recv_thread.join().expect("recv thread panicked");
        assert!(
            matches!(result, Err(RecvError::Closed)),
            "expected RecvError::Closed after sender drop, got {result:?}"
        );
    }

    #[test]
    fn send_after_both_endpoints_closed_returns_closed_without_panic() {
        // Once the receiver side has been dropped, every subsequent
        // send (blocking or non-blocking) from the remaining sender
        // must surface SendError::Closed. No panic, no UB.
        let (sender, receiver) = HewDuplex::new_pair(1, 1);
        drop(receiver);
        assert_eq!(sender.send(b"after-close".to_vec()), SendError::Closed);
        assert_eq!(sender.try_send(b"again".to_vec()), SendError::Closed);
        // Holding a half-handle, after extracting it, the same result.
        let (sender2, receiver2) = HewDuplex::new_pair(1, 1);
        let send_half = sender2.into_send_half();
        drop(receiver2);
        assert_eq!(send_half.send(b"half-after".to_vec()), SendError::Closed);
        assert_eq!(
            send_half.try_send(b"half-again".to_vec()),
            SendError::Closed
        );
    }

    // ── Double-close guard tests ───────────────────────────────────────────
    //
    // D4 requirement: every close / release function must return a typed
    // error on second call rather than invoking undefined behaviour.
    // Each test calls the C-ABI entry twice and asserts the second call
    // returns `SendError::DoubleClose` (= 4).
}
