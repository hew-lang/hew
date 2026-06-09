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
use std::mem::ManuallyDrop;
use std::ptr;

// ── Double-close guard wrappers ────────────────────────────────────────────
//
// Every heap allocation handed to the C-ABI caller is wrapped in one of
// these structs. The `released` flag is set atomically on the first
// close/release call. Subsequent calls detect the already-set flag and
// return `SendError::DoubleClose` (= 4) without touching `inner`.
//
// Why leak the outer wrapper on close (never call `Box::from_raw` on it):
//   A second close call must read `released` to know whether the handle
//   is still live. If the outer allocation were freed on first close, the
//   second call would read from freed memory — use-after-free UB. By
//   never freeing the wrapper, the flag remains valid for any subsequent
//   call. The allocation cost is bounded by the number of unique handles
//   ever created (a monotonically growing but practically small set in
//   the Hew object model).
//   Successful consume-split (`hew_duplex_{send,recv}_half`) is the
//   exception: its safety contract consumes the input pointer, so after
//   moving out the inner `HewDuplex` it reclaims the empty wrapper Box.
//
// Allocation protocol:
//   - Construct `Box::new(HewDuplexHandle::new(duplex))`; hand out
//     `Box::into_raw(...)` to the C caller.
//   - On close: read `released` through a raw-pointer projection
//     (`&*ptr::addr_of!((*d).released)`) and `swap` it to `true`. Only
//     materialise a `&mut HewDuplexHandle` AFTER winning the swap. If
//     the swap returned `true`, return `DoubleClose` immediately without
//     ever constructing a reference into the wrapper. Otherwise call
//     `ManuallyDrop::drop` on the inner value to run its existing `Drop`
//     impl (which releases Arc refcounts / queue caps) — but do NOT
//     reconstruct or drop the outer `Box<HewDuplexHandle>`.
//   - On successful consume-split: perform the same swap guard, take
//     the inner `HewDuplex` by value, then reconstruct and drop the
//     wrapper Box. Reusing the consumed pointer after success violates
//     the split entry point's safety contract.
//   - The raw-pointer flag-read phase is required: two concurrent
//     callers passing the same handle pointer would otherwise each hold
//     a `&mut *d` simultaneously, which is formal Rust aliasing UB even
//     though the inner field is not touched twice. The `#[repr(C)]`
//     guarantee on the wrappers, plus `released` being the leading
//     field, makes the projection well-defined.

/// C-ABI wrapper around [`HewDuplex`] with a double-close guard.
#[repr(C)]
#[derive(Debug)]
pub struct HewDuplexHandle {
    released: AtomicBool,
    inner: ManuallyDrop<HewDuplex>,
}

impl HewDuplexHandle {
    fn new(duplex: HewDuplex) -> Self {
        Self {
            released: AtomicBool::new(false),
            inner: ManuallyDrop::new(duplex),
        }
    }
}

/// C-ABI wrapper around [`HewSendHalf`] with a double-close guard.
#[repr(C)]
#[derive(Debug)]
pub struct HewSendHalfHandle {
    released: AtomicBool,
    inner: ManuallyDrop<HewSendHalf>,
}

impl HewSendHalfHandle {
    fn new(half: HewSendHalf) -> Self {
        Self {
            released: AtomicBool::new(false),
            inner: ManuallyDrop::new(half),
        }
    }
}

/// C-ABI wrapper around [`HewRecvHalf`] with a double-close guard.
#[repr(C)]
#[derive(Debug)]
pub struct HewRecvHalfHandle {
    released: AtomicBool,
    inner: ManuallyDrop<HewRecvHalf>,
}

impl HewRecvHalfHandle {
    fn new(half: HewRecvHalf) -> Self {
        Self {
            released: AtomicBool::new(false),
            inner: ManuallyDrop::new(half),
        }
    }
}

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
    out_a: *mut *mut HewDuplexHandle,
    out_b: *mut *mut HewDuplexHandle,
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
    let a_ptr = Box::into_raw(Box::new(HewDuplexHandle::new(a)));
    let b_ptr = Box::into_raw(Box::new(HewDuplexHandle::new(b)));
    // SAFETY:
    // - Provenance: caller-provided write targets verified non-null above.
    // - Type tag: target slot is `*mut HewDuplexHandle`, matches what we write.
    // - Lifetime owner: ownership of the Box allocation transfers via the raw
    //   pointer to the caller-supplied slot; no aliasing arises.
    // - Aliasing concurrency: caller-supplied pointers may not be shared with
    //   another thread; this is the caller's contract for an FFI out-param.
    // - Bounds: writing a single pointer-sized value into a non-null slot.
    // - Failure mode: precondition checked above; if upheld, write cannot fault.
    unsafe {
        ptr::write(out_a, a_ptr);
        ptr::write(out_b, b_ptr);
    }
    crate::tracing::record_channel_event(a_ptr as u64, crate::tracing::SPAN_DUPLEX_CREATED);
    SendError::Ok as i32
}

/// Send a payload on the unified Duplex's S-direction. Returns the
/// `SendError` discriminant as `i32` (0 = Ok, 1 = Closed, ...).
///
/// Blocks if the queue is full and at least one receiver remains.
///
/// Returns `SendError::DoubleClose` (4) if the handle has already been
/// closed (defence-in-depth: the Rust move-checker enforces this at the
/// type level; the flag guard catches sequential close-then-send from
/// native FFI callers without move-checker enforcement).
///
/// # Safety
///
/// `d` must point to a valid `HewDuplexHandle` returned by `hew_duplex_pair`.
/// `msg` must be valid for `len` bytes (may be null if `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_send(
    d: *mut HewDuplexHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if d.is_null() {
        crate::set_last_error("hew_duplex_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: load with Acquire to synchronise with the
    // AcqRel store in hew_duplex_close. Catches sequential close-then-send
    // from native FFI callers. A concurrent close-during-send is still a
    // caller contract violation — this guard does not serialise them.
    // SAFETY: `released` is the first field of #[repr(C)] HewDuplexHandle;
    // addr_of! projects without materialising &mut *d (which would alias a
    // concurrent close caller's exclusive borrow). The outer wrapper is
    // never freed (intentional leak), so the pointer remains valid.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_duplex_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
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
    // - Provenance: caller guarantees `d` came from hew_duplex_pair.
    // - Type tag: `*mut HewDuplexHandle` matches the originating type.
    // - Lifetime owner: caller still owns the wrapper; we take only a
    //   shared reference to the inner Duplex for the send call.
    // - Aliasing concurrency: `HewDuplex` is Sync (every field's mutation
    //   is guarded by an internal Mutex / atomic), so shared access from
    //   parallel threads is sound.
    // - Bounds: dereferencing a single non-null aligned pointer to a Sized type.
    // - Failure mode: dangling handle is the caller's contract violation;
    //   the runtime cannot detect this and behaviour is UB per FFI norms.
    let res = unsafe { (*d).inner.send(payload) };
    res as i32
}

/// Non-blocking variant of [`hew_duplex_send`]. Returns
/// `SendError::Full` if the queue is at capacity.
///
/// Returns `SendError::DoubleClose` (4) if the handle has already been closed.
///
/// # Safety
///
/// Same as `hew_duplex_send`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_try_send(
    d: *mut HewDuplexHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if d.is_null() {
        crate::set_last_error("hew_duplex_try_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_duplex_try_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
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
    // SAFETY: see hew_duplex_send for the six-axis breakdown of the
    // `*mut HewDuplexHandle` dereference. `try_send` does not block, but
    // the pointer-validity contract is identical.
    let res = unsafe { (*d).inner.try_send(payload) };
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
/// Returns `RecvError::Closed` (1) if the handle has already been closed,
/// with `*out_ptr = null` and `*out_len = 0`.
///
/// # Safety
///
/// `d`, `out_ptr`, and `out_len` must all be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_recv(
    d: *mut HewDuplexHandle,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if d.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_duplex_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_duplex_recv: handle already released".to_string());
        // SAFETY: out_ptr / out_len are non-null (checked above); write the
        // no-payload shape so the caller does not observe uninitialised output.
        unsafe {
            ptr::write(out_ptr, ptr::null_mut());
            ptr::write(out_len, 0usize);
        }
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_send dereference axis-by-axis; identical
    // (Sync handle, non-null aligned pointer, single shared borrow).
    let outcome = unsafe { (*d).inner.recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY:
            // - Provenance: `out_ptr` / `out_len` are caller-owned
            //   slots null-checked at function entry; the heap-allocated
            //   payload is fresh from `Box::into_raw` above and is not
            //   aliased elsewhere.
            // - Type tag: `*mut u8` written to `*mut *mut u8`; `usize`
            //   written to `*mut usize` — matched element types.
            // - Lifetime owner: payload ownership transfers from this
            //   frame to the caller, who must release it via
            //   `hew_duplex_payload_free`.
            // - Aliasing concurrency: caller-supplied out-pointers must
            //   not be shared with another thread for the duration of
            //   this call (standard FFI out-param contract).
            // - Bounds: two pointer-sized writes to non-null, aligned
            //   target slots.
            // - Failure mode: precondition (non-null) is checked above;
            //   if upheld, the writes cannot fault.
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: see the Ok arm above — identical out-param
            // contract. Writing null + 0 communicates the no-payload
            // shape on the error path; the caller must not pass the
            // null pointer to `hew_duplex_payload_free` (the free
            // entry short-circuits on null defensively).
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
/// Returns `RecvError::Closed` (1) if the handle has already been closed,
/// with `*out_ptr = null` and `*out_len = 0`.
///
/// # Safety
///
/// Same as `hew_duplex_recv`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_try_recv(
    d: *mut HewDuplexHandle,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if d.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_duplex_try_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_duplex_try_recv: handle already released".to_string());
        // SAFETY: out_ptr / out_len are non-null (checked above).
        unsafe {
            ptr::write(out_ptr, ptr::null_mut());
            ptr::write(out_len, 0usize);
        }
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_recv — identical six-axis profile for
    // the `*d` dereference (Sync handle via internal Arc<Queue>,
    // shared borrow only, single non-null aligned pointer).
    let outcome = unsafe { (*d).inner.try_recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: see the corresponding `hew_duplex_recv` Ok arm
            // for the full six-axis breakdown of the out-param
            // writes; identical (caller-owned non-null slots,
            // pointer-sized aligned writes of the fresh payload
            // pointer + its length).
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: see the corresponding `hew_duplex_recv` Err arm
            // — identical no-payload signalling shape.
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

/// Close-both-directions on a Duplex handle. Decrements both sender
/// and receiver caps on the underlying queues; if any cap was the last
/// alive, that direction's blocked peer wakes with `Closed`.
///
/// Returns `SendError::Ok` (0) on the first call.
/// Returns `SendError::DoubleClose` (4) on any subsequent call without
/// invoking undefined behaviour — per design principle D4 ("Hew is
/// designed for failure").
///
/// The outer handle allocation intentionally persists after close so
/// that the double-close guard flag remains readable for subsequent
/// calls. See the wrapper-struct comment block above `HewDuplexHandle`.
///
/// # Safety
///
/// `d` must have been returned by [`hew_duplex_pair`].
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_close(d: *mut HewDuplexHandle) -> i32 {
    if d.is_null() {
        return SendError::Ok as i32;
    }
    // SAFETY: (flag-read phase)
    // - Provenance: caller guarantees `d` came from hew_duplex_pair,
    //   which calls `Box::into_raw(Box::new(HewDuplexHandle::new(...)))`.
    // - Type tag: `*mut HewDuplexHandle` matches the originating Box type.
    //   `released` is the first field of `#[repr(C)]` HewDuplexHandle, so
    //   `addr_of!((*d).released)` yields a valid `*const AtomicBool` without
    //   materialising a `&mut HewDuplexHandle` (which would alias with a
    //   concurrent caller's `&mut` on the same pointer — formal aliasing
    //   UB even though the `inner` field is not actually touched twice).
    // - Lifetime owner: the outer HewDuplexHandle allocation is never
    //   freed (intentional leak — see wrapper-struct design comment).
    // - Aliasing concurrency: AtomicBool::swap with AcqRel ordering
    //   provides the acquire fence and linearizes all close/release
    //   callers; the loser observes `true` and returns before constructing
    //   any reference into the wrapper.
    // - Bounds: single in-bounds field read via projection on a non-null
    //   aligned pointer to a Sized `#[repr(C)]` type.
    // - Failure mode: double-close returns SendError::DoubleClose (4)
    //   via the atomic-flag guard; no UB on the second call.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.swap(true, Ordering::AcqRel) {
        return SendError::DoubleClose as i32;
    }
    // SAFETY: (drop phase) the swap above linearized close/release callers
    // and this thread won (old value was false). No other close/release
    // path can construct a reference into `*d` past its flag check, so the
    // `&mut *d` below is exclusive against all close/release entries.
    // (It is not claimed exclusive against in-flight `hew_duplex_send` /
    // `_recv` callers; those access `inner` through a shared `(*d).inner`
    // borrow internally synchronised by HewDuplex's Sync impl, and a
    // caller racing send-then-close on the same pointer is the documented
    // FFI contract violation.) ManuallyDrop::drop runs the inner Drop
    // without freeing the outer wrapper.
    let h = unsafe { &mut *d };
    // SAFETY: see the drop-phase block immediately above — this thread
    // owns the wrapper exclusively against all close/release callers.
    unsafe { ManuallyDrop::drop(&mut h.inner) };
    crate::tracing::record_channel_event(d as u64, crate::tracing::SPAN_DUPLEX_CLOSED);
    SendError::Ok as i32
}

/// Convert a unified Duplex into a `HewSendHalfHandle` (send-only alias).
/// Consumes the input handle (caller MUST NOT use `d` after this call).
/// The returned handle must be released via [`hew_duplex_close_half`]
/// with `Direction::Send`.
///
/// Returns null if another close/consume-split already released the handle
/// before this call wins the release swap.
///
/// # Safety
///
/// `d` must have been returned by `hew_duplex_pair`. On success `d`
/// is consumed and must not be used again.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_send_half(d: *mut HewDuplexHandle) -> *mut HewSendHalfHandle {
    if d.is_null() {
        crate::set_last_error("hew_duplex_send_half: null handle".to_string());
        return ptr::null_mut();
    }
    // SAFETY: (flag-read phase) see `hew_duplex_close` — `released` is read
    // via `addr_of!` to avoid materialising a `&mut HewDuplexHandle` that
    // would alias against a concurrent close/consume-split caller's
    // exclusive borrow. The atomic swap linearizes the contenders; the
    // loser returns null before constructing any reference into the wrapper.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.swap(true, Ordering::AcqRel) {
        crate::set_last_error("hew_duplex_send_half: handle already consumed".to_string());
        return ptr::null_mut();
    }
    // SAFETY: (take phase) the swap above linearized close/release callers
    // and this thread won; no other path can construct a reference into
    // `*d` past its flag check. ManuallyDrop::take moves the inner
    // HewDuplex out by value without running its Drop; into_send_half
    // takes ownership and handles the cap accounting itself.
    let h = unsafe { &mut *d };
    // SAFETY: see the take-phase block above — this thread owns the
    // wrapper exclusively against all close/release callers.
    let duplex = unsafe { ManuallyDrop::take(&mut h.inner) };
    let trace_id = d as u64;
    // SAFETY: the swap above won the consume/close race for callers that
    // honour the documented consume contract and do not use `d` after this
    // successful split. `ManuallyDrop::take` moved out the inner HewDuplex;
    // dropping the wrapper Box now only reclaims the released=true husk
    // allocation and does not drop the consumed inner value.
    let _ = unsafe { Box::from_raw(d) };
    let send_half = duplex.into_send_half();
    let result = Box::into_raw(Box::new(HewSendHalfHandle::new(send_half)));
    crate::tracing::record_channel_event(trace_id, crate::tracing::SPAN_DUPLEX_HALF_SPLIT);
    result
}

/// Convert a unified Duplex into a `HewRecvHalfHandle` (recv-only alias).
///
/// Returns null if another close/consume-split already released the handle
/// before this call wins the release swap.
///
/// # Safety
///
/// Same as `hew_duplex_send_half`.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_recv_half(d: *mut HewDuplexHandle) -> *mut HewRecvHalfHandle {
    if d.is_null() {
        crate::set_last_error("hew_duplex_recv_half: null handle".to_string());
        return ptr::null_mut();
    }
    // SAFETY: (flag-read phase) see `hew_duplex_close` — read via `addr_of!`
    // so no `&mut HewDuplexHandle` is materialised before the swap; that
    // avoids aliasing against a concurrent caller on the same pointer.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.swap(true, Ordering::AcqRel) {
        crate::set_last_error("hew_duplex_recv_half: handle already consumed".to_string());
        return ptr::null_mut();
    }
    // SAFETY: (take phase) the swap linearized close/release callers and
    // this thread won. ManuallyDrop::take moves the inner HewDuplex out
    // by value without running its Drop; into_recv_half takes ownership
    // and handles the cap accounting itself.
    let h = unsafe { &mut *d };
    // SAFETY: see the take-phase block above — this thread owns the
    // wrapper exclusively against all close/release callers.
    let duplex = unsafe { ManuallyDrop::take(&mut h.inner) };
    let trace_id = d as u64;
    // SAFETY: the swap above won the consume/close race for callers that
    // honour the documented consume contract and do not use `d` after this
    // successful split. `ManuallyDrop::take` moved out the inner HewDuplex;
    // dropping the wrapper Box now only reclaims the released=true husk
    // allocation and does not drop the consumed inner value.
    let _ = unsafe { Box::from_raw(d) };
    let recv_half = duplex.into_recv_half();
    let result = Box::into_raw(Box::new(HewRecvHalfHandle::new(recv_half)));
    crate::tracing::record_channel_event(trace_id, crate::tracing::SPAN_DUPLEX_HALF_SPLIT);
    result
}

/// Send a payload on a `HewSendHalfHandle`.
///
/// Returns `SendError::DoubleClose` (4) if the handle has already been closed.
///
/// # Safety
///
/// `half` must point to a valid `HewSendHalfHandle` returned by
/// [`hew_duplex_send_half`] (or its `*_clone` companion). `msg` must
/// be valid for `len` bytes (null if `len == 0`).
#[no_mangle]
pub unsafe extern "C" fn hew_send_half_send(
    half: *mut HewSendHalfHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if half.is_null() {
        crate::set_last_error("hew_send_half_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*half).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_send_half_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
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
    let res = unsafe { (*half).inner.send(payload) };
    res as i32
}

/// Non-blocking send on a `HewSendHalfHandle`. Returns `SendError::Full` if
/// the send queue is at capacity; returns `SendError::Closed` if the channel
/// is closed. Does not block waiting for space.
///
/// Returns `SendError::DoubleClose` (4) if the handle has already been closed.
///
/// # Safety
///
/// Same as `hew_send_half_send`.
#[no_mangle]
pub unsafe extern "C" fn hew_send_half_try_send(
    half: *mut HewSendHalfHandle,
    msg: *const u8,
    len: usize,
) -> i32 {
    if half.is_null() {
        crate::set_last_error("hew_send_half_try_send: null handle".to_string());
        return SendError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*half).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_send_half_try_send: handle already released".to_string());
        return SendError::DoubleClose as i32;
    }
    let payload = if len == 0 {
        Vec::new()
    } else if msg.is_null() {
        crate::set_last_error("hew_send_half_try_send: null msg with non-zero len".to_string());
        return SendError::Closed as i32;
    } else {
        // SAFETY: see hew_duplex_send dereference axis; identical contract.
        unsafe { std::slice::from_raw_parts(msg, len) }.to_vec()
    };
    // SAFETY: see hew_send_half_send — pointer-validity contract identical;
    // try_send does not block but the dereference safety profile is the same.
    let res = unsafe { (*half).inner.try_send(payload) };
    res as i32
}

/// Receive a payload from a `HewRecvHalfHandle`.
///
/// Returns `RecvError::Closed` (1) if the handle has already been closed,
/// with `*out_ptr = null` and `*out_len = 0`.
///
/// # Safety
///
/// `half`, `out_ptr`, and `out_len` must all be valid pointers; the
/// payload is owned by the caller and must be released with
/// [`hew_duplex_payload_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_recv_half_recv(
    half: *mut HewRecvHalfHandle,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if half.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_recv_half_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*half).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_recv_half_recv: handle already released".to_string());
        // SAFETY: out_ptr / out_len are non-null (checked above).
        unsafe {
            ptr::write(out_ptr, ptr::null_mut());
            ptr::write(out_len, 0usize);
        }
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_duplex_recv — symmetric profile.
    let outcome = unsafe { (*half).inner.recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: see `hew_duplex_recv` Ok arm for the full
            // six-axis breakdown of the out-param writes; identical.
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: see `hew_duplex_recv` Err arm — identical
            // no-payload signalling shape.
            unsafe {
                ptr::write(out_ptr, ptr::null_mut());
                ptr::write(out_len, 0usize);
            }
            e as i32
        }
    }
}

/// Non-blocking variant of [`hew_recv_half_recv`]. Returns
/// `RecvError::Empty` if no message is waiting; returns `RecvError::Closed`
/// if the channel is closed.
///
/// Returns `RecvError::Closed` (1) if the handle has already been closed,
/// with `*out_ptr = null` and `*out_len = 0`.
///
/// # Safety
///
/// Same as `hew_recv_half_recv`.
#[no_mangle]
pub unsafe extern "C" fn hew_recv_half_try_recv(
    half: *mut HewRecvHalfHandle,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if half.is_null() || out_ptr.is_null() || out_len.is_null() {
        crate::set_last_error("hew_recv_half_try_recv: null pointer argument".to_string());
        return RecvError::Closed as i32;
    }
    // Released-flag guard: see hew_duplex_send for rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*half).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_recv_half_try_recv: handle already released".to_string());
        // SAFETY: out_ptr / out_len are non-null (checked above).
        unsafe {
            ptr::write(out_ptr, ptr::null_mut());
            ptr::write(out_len, 0usize);
        }
        return RecvError::Closed as i32;
    }
    // SAFETY: see hew_recv_half_recv — identical six-axis profile for the
    // `*half` dereference; try_recv does not block but the pointer-validity
    // contract is the same.
    let outcome = unsafe { (*half).inner.try_recv() };
    match outcome {
        Ok(bytes) => {
            let len = bytes.len();
            let buf = bytes.into_boxed_slice();
            let ptr = Box::into_raw(buf).cast::<u8>();
            // SAFETY: see `hew_recv_half_recv` Ok arm — identical out-param
            // write contract (caller-owned non-null slots, fresh payload
            // pointer + length).
            unsafe {
                ptr::write(out_ptr, ptr);
                ptr::write(out_len, len);
            }
            RecvError::Ok as i32
        }
        Err(e) => {
            // SAFETY: see `hew_recv_half_recv` Err arm — identical
            // no-payload signalling shape.
            unsafe {
                ptr::write(out_ptr, ptr::null_mut());
                ptr::write(out_len, 0usize);
            }
            e as i32
        }
    }
}

/// Close one direction of a Duplex. Used by codegen for the MIR
/// `DropKind::DuplexHalfClose(Direction)` shape: the half-handle's
/// Drop fires this entry rather than [`hew_duplex_close`].
///
/// Behaviour:
///
/// - `direction == Send`: treats `half` as a `HewSendHalfHandle*`;
///   releases the `s_queue` sender-cap on the first call.
/// - `direction == Recv`: treats `half` as a `HewRecvHalfHandle*`;
///   releases the `r_queue` receiver-cap on the first call.
///
/// Returns `SendError::Ok` (0) on the first call.
/// Returns `SendError::DoubleClose` (4) if the handle was already closed,
/// without invoking undefined behaviour — per design principle D4.
///
/// The outer handle allocation intentionally persists after close so
/// that the double-close guard flag remains readable. See the
/// wrapper-struct comment block above `HewDuplexHandle`.
///
/// # Safety
///
/// `half` must point to the matching half-handle wrapper type for `direction`.
/// A mismatched direction is still UB.
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_close_half(half: *mut c_void, direction: i32) -> i32 {
    if half.is_null() {
        return SendError::Ok as i32;
    }
    match direction {
        x if x == HewDuplexDirection::Send as i32 => {
            let half_send = half.cast::<HewSendHalfHandle>();
            // SAFETY:
            // - Provenance: per the caller contract for `direction == Send`,
            //   `half` came from `hew_duplex_send_half` which produces a
            //   `Box::into_raw(Box::new(HewSendHalfHandle::new(...)))`.
            // - Type tag: the cast above matches the originating Box's
            //   element type; `released` is the first field of `#[repr(C)]`
            //   HewSendHalfHandle, so `addr_of!((*half_send).released)`
            //   yields a valid `*const AtomicBool` without materialising
            //   `&mut *half_send` — that would alias against a concurrent
            //   close/release caller on the same pointer (formal Rust UB
            //   even when the inner field is not actually touched twice).
            // - Lifetime owner: the outer wrapper allocation is never
            //   freed (intentional leak — see wrapper-struct design comment).
            // - Aliasing concurrency: AtomicBool::swap with AcqRel ordering
            //   provides the acquire fence and linearizes all close/release
            //   callers; the loser observes `true` and returns before
            //   constructing any reference into the wrapper.
            // - Bounds: single in-bounds field read via projection.
            // - Failure mode: double-close returns DoubleClose (4) instead
            //   of invoking UB on the freed inner allocation.
            let released = unsafe { &*ptr::addr_of!((*half_send).released) };
            if released.swap(true, Ordering::AcqRel) {
                return SendError::DoubleClose as i32;
            }
            // SAFETY: the swap linearized close/release callers and this
            // thread won; the `&mut` below is exclusive against all
            // other close/release entries.
            let h = unsafe { &mut *half_send };
            // SAFETY: first close — inner HewSendHalf is still valid.
            // ManuallyDrop::drop runs its Drop (sender-cap release)
            // without freeing the outer wrapper.
            unsafe { ManuallyDrop::drop(&mut h.inner) };
            SendError::Ok as i32
        }
        x if x == HewDuplexDirection::Recv as i32 => {
            let half_recv = half.cast::<HewRecvHalfHandle>();
            // SAFETY: see the Send arm — symmetric six-axis profile with
            // HewRecvHalfHandle and its receiver-cap release in Drop.
            let released = unsafe { &*ptr::addr_of!((*half_recv).released) };
            if released.swap(true, Ordering::AcqRel) {
                return SendError::DoubleClose as i32;
            }
            // SAFETY: the swap won — exclusive against all other
            // close/release callers.
            let h = unsafe { &mut *half_recv };
            // SAFETY: first close — inner HewRecvHalf is still valid.
            // ManuallyDrop::drop runs its Drop (receiver-cap release)
            // without freeing the outer wrapper.
            unsafe { ManuallyDrop::drop(&mut h.inner) };
            SendError::Ok as i32
        }
        _ => {
            crate::set_last_error(format!(
                "hew_duplex_close_half: invalid direction discriminant {direction}"
            ));
            SendError::Closed as i32
        }
    }
}

/// Refcount-bump clone of a unified Duplex handle.
///
/// Returns null if the handle has already been closed.
///
/// # Safety
///
/// `d` must be a valid, open `HewDuplexHandle` (not yet closed).
#[no_mangle]
pub unsafe extern "C" fn hew_duplex_clone(d: *mut HewDuplexHandle) -> *mut HewDuplexHandle {
    if d.is_null() {
        crate::set_last_error("hew_duplex_clone: null handle".to_string());
        return ptr::null_mut();
    }
    // Released-flag guard: cloning a closed handle would bump the Arc
    // refcount on a ManuallyDrop-dropped inner — UB. Consult the flag before
    // dereferencing inner. See hew_duplex_send for addr_of! rationale.
    // SAFETY: addr_of! projection on #[repr(C)] handle; outer wrapper never freed.
    let released = unsafe { &*ptr::addr_of!((*d).released) };
    if released.load(Ordering::Acquire) {
        crate::set_last_error("hew_duplex_clone: handle already released".to_string());
        return ptr::null_mut();
    }
    // SAFETY:
    // - Provenance: caller guarantees `d` came from hew_duplex_pair or a
    //   prior clone call.
    // - Type tag: `*mut HewDuplexHandle` matches the originating type.
    // - Lifetime owner: caller still owns the wrapper; we take a shared
    //   reference to the inner HewDuplex only for clone_handle.
    // - Aliasing concurrency: HewDuplex::clone_handle uses atomic refcount
    //   bumps (Arc) — safe from multiple threads simultaneously.
    // - Bounds: single non-null aligned pointer dereference.
    // - Failure mode: released flag guards against cloning a closed handle
    //   (the inner ManuallyDrop is in an invalid state after close).
    let cloned = unsafe { (*d).inner.clone_handle() };
    Box::into_raw(Box::new(HewDuplexHandle::new(cloned)))
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

    #[test]
    fn cabi_pair_cross_wires() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
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
            assert_eq!(hew_duplex_close(a), SendError::Ok as i32);
            assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
        }
    }

    #[test]
    fn cabi_close_both_dirs_on_last_drop() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
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
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
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
            assert_eq!(
                hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32),
                SendError::Ok as i32
            );
            // After a_send closes, b_recv must see Closed (no senders).
            assert_eq!(
                hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n),),
                RecvError::Closed as i32
            );
            assert_eq!(
                hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32),
                SendError::Ok as i32
            );
        }
    }

    #[test]
    fn cabi_pair_rejects_zero_capacity() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: out-slots valid stack variables.
        let rc1 =
            unsafe { hew_duplex_pair(0, 4, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        assert_eq!(rc1, SendError::Closed as i32);
        // SAFETY: out-slots valid stack variables.
        let rc2 =
            unsafe { hew_duplex_pair(4, 0, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        assert_eq!(rc2, SendError::Closed as i32);
    }

    #[test]
    fn cabi_pair_rejects_null_out_param() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: deliberately exercising the null-out-param branch.
        let rc = unsafe {
            hew_duplex_pair(
                2,
                2,
                std::ptr::addr_of_mut!(a),
                ptr::null_mut::<*mut HewDuplexHandle>(),
            )
        };
        assert_eq!(rc, SendError::Closed as i32);
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

    #[test]
    fn cabi_double_close_duplex_returns_already_closed() {
        // Verify that calling hew_duplex_close twice on the same handle
        // returns DoubleClose on the second call rather than invoking UB.
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: a / b are valid stack slots; all C-ABI calls use handles
        // produced by hew_duplex_pair within this block.
        unsafe {
            let rc = hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
            assert_eq!(rc, SendError::Ok as i32);
            // First close: normal.
            assert_eq!(hew_duplex_close(a), SendError::Ok as i32);
            // Second close: double-close guard fires.
            assert_eq!(hew_duplex_close(a), SendError::DoubleClose as i32);
            // b is still live; close it normally.
            assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
        }
    }

    #[test]
    fn cabi_double_close_send_half_returns_already_closed() {
        // Verify that calling hew_duplex_close_half (Send direction) twice
        // on the same half-handle returns DoubleClose on the second call.
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: a / b are valid stack slots; all handles produced by
        // hew_duplex_pair / hew_duplex_send_half / hew_duplex_recv_half.
        unsafe {
            hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
            let a_send = hew_duplex_send_half(a);
            assert!(!a_send.is_null());
            // First close: normal.
            assert_eq!(
                hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32),
                SendError::Ok as i32
            );
            // Second close: double-close guard fires.
            assert_eq!(
                hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32),
                SendError::DoubleClose as i32
            );
            // b was consumed into a recv half; close the original handle
            // wrapper (which was marked released by hew_duplex_recv_half).
            // We need to close b's half: get a recv half first.
            let b_recv = hew_duplex_recv_half(b);
            assert!(!b_recv.is_null());
            assert_eq!(
                hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32),
                SendError::Ok as i32
            );
        }
    }

    #[test]
    fn cabi_double_close_recv_half_returns_already_closed() {
        // Verify that calling hew_duplex_close_half (Recv direction) twice
        // on the same half-handle returns DoubleClose on the second call.
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: a / b are valid stack slots; all handles produced by
        // hew_duplex_pair / hew_duplex_recv_half / hew_duplex_send_half.
        unsafe {
            hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
            let b_recv = hew_duplex_recv_half(b);
            assert!(!b_recv.is_null());
            // First close: normal.
            assert_eq!(
                hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32),
                SendError::Ok as i32
            );
            // Second close: double-close guard fires.
            assert_eq!(
                hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32),
                SendError::DoubleClose as i32
            );
            // a is still live; close its send half.
            let a_send = hew_duplex_send_half(a);
            assert!(!a_send.is_null());
            assert_eq!(
                hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32),
                SendError::Ok as i32
            );
        }
    }

    // ── Concurrent close stress ────────────────────────────────────────────
    //
    // Race many threads calling `hew_duplex_close` on the same handle.
    // The raw-pointer flag-read phase in `hew_duplex_close` is what makes
    // this sound: each thread observes the AtomicBool through
    // `&*ptr::addr_of!((*d).released)` and only the swap winner builds a
    // `&mut HewDuplexHandle`. If a refactor regresses to taking
    // `&mut *d` before the swap, two concurrent winners would briefly
    // alias the same `&mut`, which is formal Rust UB.
    //
    // Asserts every iteration:
    //   - exactly one thread observes `SendError::Ok` (= 0)
    //   - every other thread observes `SendError::DoubleClose` (= 4)
    //
    // Repeated 100 iterations to widen the race window. Run under
    // `cargo test --release -p hew-runtime` for the best chance of
    // surfacing a regression.

    #[test]
    fn cabi_concurrent_close_duplex_returns_single_winner() {
        const THREADS: usize = 8;
        const ITERS: usize = 100;

        for iter in 0..ITERS {
            let mut a: *mut HewDuplexHandle = ptr::null_mut();
            let mut b: *mut HewDuplexHandle = ptr::null_mut();
            // SAFETY: stack out-slots are valid; capacities positive.
            unsafe {
                let rc =
                    hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b));
                assert_eq!(rc, SendError::Ok as i32);
            }
            // Race THREADS callers on the same pointer.
            let a_addr = a as usize;
            let barrier = Arc::new(std::sync::Barrier::new(THREADS));
            let mut handles = Vec::with_capacity(THREADS);
            for _ in 0..THREADS {
                let bar = Arc::clone(&barrier);
                handles.push(thread::spawn(move || {
                    bar.wait();
                    // SAFETY: every thread receives the same handle
                    // produced by hew_duplex_pair above; the
                    // implementation linearizes them via AtomicBool::swap
                    // and only the winner constructs a `&mut` into the
                    // wrapper.
                    unsafe { hew_duplex_close(a_addr as *mut HewDuplexHandle) }
                }));
            }
            let mut winners = 0;
            let mut losers = 0;
            for h in handles {
                match h.join().unwrap() {
                    rc if rc == SendError::Ok as i32 => winners += 1,
                    rc if rc == SendError::DoubleClose as i32 => losers += 1,
                    rc => panic!(
                        "iter {iter}: unexpected return {rc} from concurrent hew_duplex_close"
                    ),
                }
            }
            assert_eq!(
                winners, 1,
                "iter {iter}: expected exactly one winner, got {winners}"
            );
            assert_eq!(
                losers,
                THREADS - 1,
                "iter {iter}: expected {} losers, got {losers}",
                THREADS - 1
            );
            // Clean up the other side.
            // SAFETY: b is still live and untouched by the race above.
            unsafe {
                assert_eq!(hew_duplex_close(b), SendError::Ok as i32);
            }
        }
    }

    // ── Released-flag guard tests ──────────────────────────────────────────
    //
    // After hew_duplex_close / hew_duplex_close_half, every non-close entry
    // must return a typed error rather than dereferencing the freed inner
    // allocation. These tests prove the flag is consulted before the inner
    // borrow — the positive path (open handle → normal operation) is
    // exercised extensively in the round-trip tests above.

    #[test]
    fn cabi_send_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // SAFETY: handle came from hew_duplex_pair; first close returns Ok.
        assert_eq!(unsafe { hew_duplex_close(a) }, SendError::Ok as i32);
        // Send on the closed handle — must not SIGSEGV; must return DoubleClose.
        let msg = b"post-close";
        // SAFETY: `a` wrapper allocation persists (intentional leak); the
        // released flag is readable; inner is NOT dereferenced.
        let rc = unsafe { hew_duplex_send(a, msg.as_ptr(), msg.len()) };
        assert_eq!(
            rc,
            SendError::DoubleClose as i32,
            "send after close must be DoubleClose"
        );
        // SAFETY: b is still live.
        unsafe { hew_duplex_close(b) };
    }

    #[test]
    fn cabi_recv_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // Close `a`; then verify that recv on the closed `a` wrapper returns
        // Closed (released-flag path) with null out-params, not a SIGSEGV.
        // SAFETY: handle from hew_duplex_pair; wrapper persists after close.
        unsafe { hew_duplex_close(a) };
        let mut p: *mut u8 = ptr::null_mut();
        let mut n: usize = 0;
        // SAFETY: `a` wrapper persists; released flag is readable; inner NOT touched.
        let rc =
            unsafe { hew_duplex_recv(a, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)) };
        assert_eq!(
            rc,
            RecvError::Closed as i32,
            "recv on closed handle must return Closed"
        );
        assert!(p.is_null(), "out_ptr must be null on released-handle error");
        assert_eq!(n, 0, "out_len must be 0 on released-handle error");
        // SAFETY: b is still valid.
        unsafe { hew_duplex_close(b) };
    }

    #[test]
    fn cabi_try_send_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // SAFETY: handles from hew_duplex_pair.
        unsafe { hew_duplex_close(a) };
        let msg = b"x";
        // SAFETY: `a` wrapper persists; released flag is readable.
        let rc = unsafe { hew_duplex_try_send(a, msg.as_ptr(), msg.len()) };
        assert_eq!(
            rc,
            SendError::DoubleClose as i32,
            "try_send after close must be DoubleClose"
        );
        // SAFETY: b is still valid.
        unsafe { hew_duplex_close(b) };
    }

    #[test]
    fn cabi_try_recv_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // SAFETY: handles from hew_duplex_pair.
        unsafe { hew_duplex_close(a) };
        let mut p: *mut u8 = ptr::null_mut();
        let mut n: usize = 0;
        // SAFETY: `a` wrapper persists; released flag is readable.
        let rc =
            unsafe { hew_duplex_try_recv(a, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n)) };
        assert_eq!(
            rc,
            RecvError::Closed as i32,
            "try_recv after close must return Closed"
        );
        assert!(p.is_null());
        assert_eq!(n, 0);
        // SAFETY: b is still valid.
        unsafe { hew_duplex_close(b) };
    }

    #[test]
    fn cabi_send_half_send_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // SAFETY: handles from hew_duplex_pair.
        let a_send = unsafe { hew_duplex_send_half(a) };
        assert!(!a_send.is_null());
        // Close the send half.
        // SAFETY: a_send came from hew_duplex_send_half.
        unsafe { hew_duplex_close_half(a_send.cast(), HewDuplexDirection::Send as i32) };
        let msg = b"late";
        // SAFETY: wrapper persists; released flag is readable; inner NOT touched.
        let rc = unsafe { hew_send_half_send(a_send, msg.as_ptr(), msg.len()) };
        assert_eq!(
            rc,
            SendError::DoubleClose as i32,
            "send on closed half must be DoubleClose"
        );
        // SAFETY: b is still valid.
        unsafe { hew_duplex_close(b) };
    }

    #[test]
    fn cabi_recv_half_recv_after_close_returns_double_close() {
        let mut a: *mut HewDuplexHandle = ptr::null_mut();
        let mut b: *mut HewDuplexHandle = ptr::null_mut();
        // SAFETY: stack slots valid; positive capacities.
        unsafe { hew_duplex_pair(2, 2, std::ptr::addr_of_mut!(a), std::ptr::addr_of_mut!(b)) };
        // SAFETY: handles from hew_duplex_pair.
        let b_recv = unsafe { hew_duplex_recv_half(b) };
        assert!(!b_recv.is_null());
        // Close the recv half.
        // SAFETY: b_recv came from hew_duplex_recv_half.
        unsafe { hew_duplex_close_half(b_recv.cast(), HewDuplexDirection::Recv as i32) };
        let mut p: *mut u8 = ptr::null_mut();
        let mut n: usize = 0;
        // SAFETY: wrapper persists; released flag is readable; inner NOT touched.
        let rc = unsafe {
            hew_recv_half_recv(b_recv, std::ptr::addr_of_mut!(p), std::ptr::addr_of_mut!(n))
        };
        assert_eq!(
            rc,
            RecvError::Closed as i32,
            "recv on closed half must return Closed"
        );
        assert!(p.is_null());
        assert_eq!(n, 0);
        // SAFETY: a is still valid.
        unsafe { hew_duplex_close(a) };
    }

    // ── PartitionDetected variant tests ───────────────────────────────────

    /// `RecvError::PartitionDetected` has discriminant 3, does not alias
    /// `Ok`/`Closed`/`Empty`, and round-trips through `as i32`.
    #[test]
    fn recv_partition_variant_discriminant() {
        assert_eq!(RecvError::Ok as i32, 0);
        assert_eq!(RecvError::Closed as i32, 1);
        assert_eq!(RecvError::Empty as i32, 2);
        assert_eq!(RecvError::PartitionDetected as i32, 3);
    }

    /// `force_partition` on `HewDuplex` causes the next `recv`
    /// to return `RecvError::PartitionDetected` without blocking.
    #[test]
    fn recv_partition_detected_via_duplex_injection() {
        let (a, b) = HewDuplex::new_pair(4, 4);
        // Inject partition on b's recv side before any send.
        b.force_partition();
        assert!(
            matches!(b.recv(), Err(RecvError::PartitionDetected)),
            "expected PartitionDetected after injection on Duplex"
        );
        // a is unaffected — its recv side is a different queue.
        a.send(b"probe".to_vec());
        drop(a);
        // b cannot receive on its own inbound queue, but a can still be
        // observed from a fresh pair for symmetry; we simply verify b
        // keeps returning PartitionDetected.
        assert!(
            matches!(b.try_recv(), Err(RecvError::PartitionDetected)),
            "expected PartitionDetected from try_recv after injection"
        );
    }

    /// `force_partition` on `HewRecvHalf` causes the next `recv`
    /// to return `RecvError::PartitionDetected` and wakes a blocked receiver.
    #[test]
    fn recv_partition_detected_via_recv_half_injection() {
        let (a, b) = HewDuplex::new_pair(4, 4);
        let b_recv = b.into_recv_half();
        // Inject from another thread to simulate a peer-timeout wake.
        let b_recv_addr = (&raw const b_recv) as usize;
        let injector = std::thread::spawn(move || {
            // SAFETY: the test holds `b_recv` alive for the duration;
            // the pointer was cast from a live reference.
            let half = unsafe { &*(b_recv_addr as *const HewRecvHalf) };
            std::thread::sleep(std::time::Duration::from_millis(20));
            half.force_partition();
        });
        // Block on recv — injector will wake us with PartitionDetected.
        let result = b_recv.recv();
        injector.join().expect("injector thread panicked");
        assert!(
            matches!(result, Err(RecvError::PartitionDetected)),
            "expected PartitionDetected after injector woke blocked recv; got {result:?}"
        );
        drop(a);
    }
}
