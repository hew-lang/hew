//! Hew runtime: `mailbox` module.
//!
//! Dual-queue message passing primitive used by actors. Each mailbox has:
//!
//! - A **user message queue** (MPSC) for application-level messages.
//! - A **system message queue** (MPSC) for lifecycle events — always unbounded.
//!
//! Messages are deep-copied on send to ensure actor isolation. Bounded
//! mailboxes apply an overflow policy when capacity is exceeded.
//!
//! The user queue uses a lock-free Michael-Scott MPSC algorithm for the
//! fast path (unbounded, `DropNew`, `Fail`). Complex overflow policies
//! (`Block`, `DropOld`, `Coalesce`) fall back to a `Mutex`-protected
//! `VecDeque` since they require queue traversal or blocking.
//!
//! System messages use a separate lock-free MPSC queue.

use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::{AtomicI64, AtomicPtr, Ordering};
use std::sync::{Condvar, Mutex};

use crate::internal::types::{HewError, HewOverflowPolicy};
use crate::scheduler::{MESSAGES_RECEIVED, MESSAGES_SENT};

/// Re-export of [`HewOverflowPolicy`] for the public mailbox API.
pub use crate::internal::types::HewOverflowPolicy as OverflowPolicy;

/// Key extractor used by coalescing mailboxes.
pub type HewCoalesceKeyFn = unsafe extern "C" fn(i32, *mut c_void, usize) -> u64;

// ── Message node ────────────────────────────────────────────────────────

/// A single message in a mailbox queue.
///
/// Allocated with [`libc::malloc`] and freed by the caller (or by
/// [`hew_msg_node_free`]).
#[repr(C)]
#[derive(Debug)]
pub struct HewMsgNode {
    /// Intrusive MPSC next-pointer — must be the first field so that
    /// `*mut HewMsgNode` can be cast to/from `*mut MpscNode`.
    pub next: AtomicPtr<HewMsgNode>,
    /// Application-defined message type tag.
    pub msg_type: i32,
    /// Pointer to deep-copied message payload (malloc'd).
    pub data: *mut c_void,
    /// Size of `data` in bytes.
    pub data_size: usize,
    /// Optional reply channel for the ask pattern (unused by mailbox).
    pub reply_channel: *mut c_void,
}

/// Allocate a [`HewMsgNode`] via `libc::malloc`, deep-copying `data`.
///
/// # Safety
///
/// `data` must point to at least `data_size` readable bytes, or be null
/// when `data_size` is 0.
unsafe fn msg_node_alloc(msg_type: i32, data: *const c_void, data_size: usize) -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = unsafe { libc::malloc(std::mem::size_of::<HewMsgNode>()) }.cast::<HewMsgNode>();
    assert!(!node.is_null(), "OOM allocating message node");

    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
        (*node).reply_channel = ptr::null_mut();

        // Deep-copy message data for actor isolation.
        if data_size > 0 && !data.is_null() {
            let buf = libc::malloc(data_size);
            assert!(
                !buf.is_null(),
                "OOM allocating message data ({data_size} bytes)"
            );
            libc::memcpy(buf, data, data_size);
            (*node).data = buf;
        } else {
            (*node).data = ptr::null_mut();
        }
    }

    node
}

/// Free a [`HewMsgNode`] and its payload.
///
/// # Safety
///
/// `node` must have been allocated by [`msg_node_alloc`] (or
/// [`libc::malloc`] with the same layout) and must not be used after
/// this call.
#[no_mangle]
pub unsafe extern "C" fn hew_msg_node_free(node: *mut HewMsgNode) {
    if node.is_null() {
        return;
    }
    // SAFETY: Caller guarantees `node` was malloc'd and is exclusively owned.
    unsafe {
        libc::free((*node).data);
        libc::free(node.cast());
    }
}

// ── Lock-free MPSC queue ────────────────────────────────────────────────

/// Allocate a sentinel (dummy) node for an intrusive MPSC queue.
///
/// The sentinel has `msg_type = -1` and null data. It is never returned
/// to consumers — it exists only to simplify empty/non-empty transitions.
fn alloc_sentinel() -> *mut HewMsgNode {
    // SAFETY: malloc(sizeof HewMsgNode) — POD-like struct, no drop glue.
    let node = unsafe { libc::malloc(std::mem::size_of::<HewMsgNode>()) }.cast::<HewMsgNode>();
    assert!(!node.is_null(), "OOM allocating sentinel node");
    // SAFETY: `node` is non-null, properly aligned, and we own it exclusively.
    unsafe {
        ptr::write(&raw mut (*node).next, AtomicPtr::new(ptr::null_mut()));
        (*node).msg_type = -1;
        (*node).data = ptr::null_mut();
        (*node).data_size = 0;
        (*node).reply_channel = ptr::null_mut();
    }
    node
}

/// Lock-free MPSC queue using the Michael-Scott algorithm.
///
/// Multiple producers can enqueue concurrently via CAS on `tail`.
/// A single consumer dequeues from `head`. The queue always contains
/// at least a sentinel node, so `head` and `tail` are never null.
#[derive(Debug)]
struct MpscQueue {
    head: AtomicPtr<HewMsgNode>,
    tail: AtomicPtr<HewMsgNode>,
}

impl MpscQueue {
    fn new() -> Self {
        let sentinel = alloc_sentinel();
        Self {
            head: AtomicPtr::new(sentinel),
            tail: AtomicPtr::new(sentinel),
        }
    }

    /// Enqueue a node. Safe for concurrent producers.
    ///
    /// # Safety
    ///
    /// `node` must be a valid, exclusively-owned `HewMsgNode` with
    /// `node.next` set to null.
    unsafe fn enqueue(&self, node: *mut HewMsgNode) {
        // SAFETY: `node` is valid and exclusively owned. Set next to null
        // before publishing.
        unsafe { (*node).next.store(ptr::null_mut(), Ordering::Relaxed) };
        loop {
            let old_tail = self.tail.load(Ordering::Acquire);
            // SAFETY: `old_tail` is always a valid node (sentinel or previously
            // enqueued node) because we never free the tail without advancing it.
            let next = unsafe { (*old_tail).next.load(Ordering::Acquire) };
            if next.is_null() {
                // SAFETY: `old_tail` is valid; CAS its next from null to `node`.
                if unsafe {
                    (*old_tail).next.compare_exchange(
                        ptr::null_mut(),
                        node,
                        Ordering::Release,
                        Ordering::Relaxed,
                    )
                }
                .is_ok()
                {
                    // Try to advance tail. Failure is fine — a concurrent
                    // enqueue or the next enqueue will advance it.
                    let _ = self.tail.compare_exchange(
                        old_tail,
                        node,
                        Ordering::Release,
                        Ordering::Relaxed,
                    );
                    return;
                }
            } else {
                // Tail is lagging — help advance it.
                let _ = self.tail.compare_exchange(
                    old_tail,
                    next,
                    Ordering::Release,
                    Ordering::Relaxed,
                );
            }
        }
    }

    /// Try to dequeue a node. **Single-consumer only.**
    ///
    /// Returns a node containing the dequeued message's payload, or null
    /// if the queue is empty. The returned node is exclusively owned by
    /// the caller and must be freed with [`hew_msg_node_free`].
    ///
    /// Internally, the old sentinel is repurposed to carry the payload,
    /// while the first real node becomes the new sentinel.
    ///
    /// # Safety
    ///
    /// Only one thread may call this at a time (single-consumer invariant).
    unsafe fn try_dequeue(&self) -> *mut HewMsgNode {
        let dummy = self.head.load(Ordering::Acquire);
        // SAFETY: `dummy` (the sentinel) is always a valid node.
        let first = unsafe { (*dummy).next.load(Ordering::Acquire) };
        if first.is_null() {
            return ptr::null_mut();
        }
        // `first` is the actual message node. Make it the new sentinel
        // by promoting it to head.
        self.head.store(first, Ordering::Release);
        // Ensure tail doesn't point to the freed dummy.
        let _ = self
            .tail
            .compare_exchange(dummy, first, Ordering::Release, Ordering::Relaxed);
        // Transfer `first`'s payload into `dummy` so we can return `dummy`
        // to the caller. `first` stays as the new (empty) sentinel.
        // SAFETY: `dummy` and `first` are valid, non-aliased nodes.
        // `first` is now the head sentinel and won't be read by producers
        // (they only touch tail→next). The single-consumer invariant
        // ensures no concurrent dequeue.
        unsafe {
            (*dummy).msg_type = (*first).msg_type;
            (*dummy).data = (*first).data;
            (*dummy).data_size = (*first).data_size;
            (*dummy).reply_channel = (*first).reply_channel;
            (*dummy).next.store(ptr::null_mut(), Ordering::Relaxed);

            // Clear `first`'s payload so it's a clean sentinel.
            (*first).msg_type = -1;
            (*first).data = ptr::null_mut();
            (*first).data_size = 0;
            (*first).reply_channel = ptr::null_mut();
        }
        // Return `dummy` with the message payload. Caller frees it.
        dummy
    }

    /// Returns `true` if the queue has at least one real message.
    fn has_messages(&self) -> bool {
        let dummy = self.head.load(Ordering::Acquire);
        // SAFETY: `dummy` is always a valid sentinel node.
        let first = unsafe { (*dummy).next.load(Ordering::Acquire) };
        !first.is_null()
    }

    /// Drain and free all remaining nodes (including the sentinel).
    ///
    /// # Safety
    ///
    /// No concurrent access may occur. All nodes must have been allocated
    /// by `msg_node_alloc` (or `alloc_sentinel`).
    unsafe fn drain_and_free(&self) {
        let mut cur = self.head.load(Ordering::Acquire);
        while !cur.is_null() {
            // SAFETY: `cur` is a valid node in the queue.
            let next = unsafe { (*cur).next.load(Ordering::Relaxed) };
            // Sentinel has msg_type == -1 and null data, so freeing its
            // data is a no-op.
            // SAFETY: each node was malloc'd.
            unsafe {
                libc::free((*cur).data);
                libc::free(cur.cast());
            }
            cur = next;
        }
    }
}

// ── Mailbox ─────────────────────────────────────────────────────────────

/// Mutex-protected queue used by complex overflow policies that need
/// queue traversal or blocking.
#[derive(Debug)]
struct SlowPathQueue {
    user_queue: VecDeque<*mut HewMsgNode>,
}

// SAFETY: The raw pointers in the queue are only accessed while holding
// the mutex, and each pointer is exclusively owned by the mailbox.
unsafe impl Send for SlowPathQueue {}

/// Returns `true` if the given overflow policy requires the mutex slow path.
const fn needs_slow_path(policy: HewOverflowPolicy) -> bool {
    matches!(
        policy,
        HewOverflowPolicy::Block | HewOverflowPolicy::DropOld | HewOverflowPolicy::Coalesce
    )
}

/// Dual-queue actor mailbox.
///
/// Uses a lock-free MPSC queue for the fast path (unbounded, `DropNew`,
/// `Fail`) and a `Mutex`-protected `VecDeque` for complex policies
/// (`Block`, `DropOld`, `Coalesce`).
#[derive(Debug)]
pub struct HewMailbox {
    /// Lock-free user message queue (used when `!needs_slow_path`).
    user_fast: MpscQueue,
    /// Lock-free system message queue.
    sys_queue: MpscQueue,
    /// Mutex-protected user queue for Block/DropOld/Coalesce policies.
    slow_path: Mutex<SlowPathQueue>,
    /// Approximate message count for capacity checks.
    pub(crate) count: AtomicI64,
    /// Maximum user-queue capacity (`-1` = unbounded).
    capacity: i64,
    /// Policy applied when user-queue is at capacity.
    overflow: HewOverflowPolicy,
    /// Optional key extractor used by [`HewOverflowPolicy::Coalesce`].
    coalesce_key_fn: Option<HewCoalesceKeyFn>,
    /// Fallback policy used when coalesce finds no matching key.
    coalesce_fallback: HewOverflowPolicy,
    /// Whether the mailbox has been closed.
    closed: std::sync::atomic::AtomicBool,
    /// Condvar notified when a user message is consumed, waking blocked senders.
    not_full: Condvar,
    /// High-water mark: maximum `count` value observed.
    pub(crate) high_water_mark: AtomicI64,
    /// Whether this mailbox uses the slow (mutex) path for user messages.
    use_slow_path: bool,
}

/// Update the high-water mark after incrementing `count`.
fn update_high_water_mark(mb: &HewMailbox) {
    let current = mb.count.load(Ordering::Relaxed);
    let mut hwm = mb.high_water_mark.load(Ordering::Relaxed);
    while current > hwm {
        match mb.high_water_mark.compare_exchange_weak(
            hwm,
            current,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => break,
            Err(actual) => hwm = actual,
        }
    }
}

// ── Constructors ────────────────────────────────────────────────────────

/// Create an unbounded mailbox.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new() -> *mut HewMailbox {
    Box::into_raw(Box::new(HewMailbox {
        user_fast: MpscQueue::new(),
        sys_queue: MpscQueue::new(),
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        capacity: -1,
        overflow: HewOverflowPolicy::DropNew,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: false,
    }))
}

/// Create a bounded mailbox with the given capacity.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_bounded(capacity: i32) -> *mut HewMailbox {
    let policy = HewOverflowPolicy::DropNew;
    Box::into_raw(Box::new(HewMailbox {
        user_fast: MpscQueue::new(),
        sys_queue: MpscQueue::new(),
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        capacity: i64::from(capacity),
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: needs_slow_path(policy),
    }))
}

/// Create a bounded mailbox with the given capacity and overflow policy.
///
/// A `capacity` of `0` creates an unbounded mailbox.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_with_policy(
    capacity: usize,
    policy: OverflowPolicy,
) -> *mut HewMailbox {
    let cap = if capacity == 0 {
        -1
    } else {
        i64::try_from(capacity).unwrap_or(i64::MAX)
    };
    Box::into_raw(Box::new(HewMailbox {
        user_fast: MpscQueue::new(),
        sys_queue: MpscQueue::new(),
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        capacity: cap,
        overflow: policy,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: needs_slow_path(policy),
    }))
}

/// Create a bounded mailbox with the given capacity and the [`Coalesce`](HewOverflowPolicy::Coalesce)
/// overflow policy.
///
/// # Safety
///
/// Returned pointer must be freed with [`hew_mailbox_free`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_new_coalesce(capacity: u32) -> *mut HewMailbox {
    let cap = i64::from(capacity);
    Box::into_raw(Box::new(HewMailbox {
        user_fast: MpscQueue::new(),
        sys_queue: MpscQueue::new(),
        slow_path: Mutex::new(SlowPathQueue {
            user_queue: VecDeque::new(),
        }),
        count: AtomicI64::new(0),
        capacity: cap,
        overflow: HewOverflowPolicy::Coalesce,
        coalesce_key_fn: None,
        coalesce_fallback: HewOverflowPolicy::DropOld,
        closed: std::sync::atomic::AtomicBool::new(false),
        not_full: Condvar::new(),
        high_water_mark: AtomicI64::new(0),
        use_slow_path: true,
    }))
}

fn normalize_coalesce_fallback(policy: HewOverflowPolicy) -> HewOverflowPolicy {
    match policy {
        HewOverflowPolicy::Coalesce => HewOverflowPolicy::DropOld,
        other => other,
    }
}

unsafe fn coalesce_message_key(
    key_fn: Option<HewCoalesceKeyFn>,
    msg_type: i32,
    data: *mut c_void,
    data_size: usize,
) -> u64 {
    if let Some(key_fn) = key_fn {
        // SAFETY: caller guarantees key function and payload pointers are valid.
        unsafe { key_fn(msg_type, data, data_size) }
    } else {
        #[expect(
            clippy::cast_sign_loss,
            reason = "bit-pattern-preserving cast is fine for fallback msg_type keying"
        )]
        {
            msg_type as u64
        }
    }
}

unsafe fn replace_node_payload(
    node: *mut HewMsgNode,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
) {
    // SAFETY: `node` is a valid queue node owned while mailbox lock is held.
    unsafe {
        libc::free((*node).data);
        if data_size > 0 && !data.is_null() {
            let buf = libc::malloc(data_size);
            assert!(!buf.is_null(), "OOM allocating coalesced message data");
            libc::memcpy(buf, data, data_size);
            (*node).data = buf;
        } else {
            (*node).data = ptr::null_mut();
        }
        (*node).msg_type = msg_type;
        (*node).data_size = data_size;
    }
}

/// Configure coalescing behavior for a mailbox.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_set_coalesce_config(
    mb: *mut HewMailbox,
    key_fn: Option<HewCoalesceKeyFn>,
    fallback_policy: OverflowPolicy,
) {
    // SAFETY: caller guarantees `mb` is valid.
    let mb = unsafe { &mut *mb };
    mb.coalesce_key_fn = key_fn;
    mb.coalesce_fallback = normalize_coalesce_fallback(fallback_policy);
}

// ── Send (producer side) ────────────────────────────────────────────────

/// Send a message to the mailbox (user queue), deep-copying `data`.
///
/// Returns `0` ([`HewError::Ok`]) on success, `-1`
/// ([`HewError::ErrMailboxFull`]) if bounded and at capacity,
/// or `-2` ([`HewError::ErrActorStopped`]) if the mailbox is closed.
///
/// # Panics
///
/// Panics if memory allocation for the message node or coalesced data fails (OOM).
///
/// # Safety
///
/// - `mb` must be a valid pointer returned by [`hew_mailbox_new`] or
///   [`hew_mailbox_new_bounded`].
/// - `data` must point to at least `size` readable bytes, or be null
///   when `size` is 0.
#[no_mangle]
#[expect(
    clippy::too_many_lines,
    reason = "mailbox send with overflow policies is inherently complex"
)]
pub unsafe extern "C" fn hew_mailbox_send(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    if mb.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }

    // Bounded capacity check.
    if mb.capacity > 0 {
        let cur = mb.count.load(Ordering::Acquire);
        if cur >= mb.capacity {
            match mb.overflow {
                HewOverflowPolicy::DropNew | HewOverflowPolicy::Fail => {
                    return HewError::ErrMailboxFull as i32;
                }
                HewOverflowPolicy::Block => {
                    // Wait on condvar until space is available.
                    let mut q = match mb.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    loop {
                        if mb.closed.load(Ordering::Acquire) {
                            return HewError::ErrActorStopped as i32;
                        }
                        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                        if len < mb.capacity {
                            break;
                        }
                        q = match mb.not_full.wait(q) {
                            Ok(g) => g,
                            Err(e) => e.into_inner(),
                        };
                    }
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, size) };
                    q.user_queue.push_back(node);
                    drop(q);
                    mb.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return HewError::Ok as i32;
                }
                HewOverflowPolicy::Coalesce => {
                    let mut q = match mb.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    // Scan for an existing message with the same coalesce key.
                    // SAFETY: `data` validity guaranteed by caller.
                    let incoming_key =
                        unsafe { coalesce_message_key(mb.coalesce_key_fn, msg_type, data, size) };
                    let found = q
                        .user_queue
                        .iter()
                        .find(|&&n| {
                            // SAFETY: all nodes in the queue were allocated by msg_node_alloc.
                            unsafe {
                                coalesce_message_key(
                                    mb.coalesce_key_fn,
                                    (*n).msg_type,
                                    (*n).data,
                                    (*n).data_size,
                                ) == incoming_key
                            }
                        })
                        .copied();
                    if let Some(existing) = found {
                        // SAFETY: `existing` is valid; replace its payload.
                        unsafe {
                            replace_node_payload(existing, msg_type, data.cast_const(), size);
                        }
                        return HewError::Ok as i32;
                    }
                    // No matching key — use configured fallback policy.
                    match normalize_coalesce_fallback(mb.coalesce_fallback) {
                        HewOverflowPolicy::DropNew | HewOverflowPolicy::Fail => {
                            return HewError::ErrMailboxFull as i32;
                        }
                        HewOverflowPolicy::Block => {
                            loop {
                                if mb.closed.load(Ordering::Acquire) {
                                    return HewError::ErrActorStopped as i32;
                                }
                                let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                                if len < mb.capacity {
                                    break;
                                }
                                q = match mb.not_full.wait(q) {
                                    Ok(g) => g,
                                    Err(e) => e.into_inner(),
                                };
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node = unsafe { msg_node_alloc(msg_type, data, size) };
                            q.user_queue.push_back(node);
                            drop(q);
                            mb.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return HewError::Ok as i32;
                        }
                        HewOverflowPolicy::DropOld => {
                            if let Some(old) = q.user_queue.pop_front() {
                                // SAFETY: node was allocated by msg_node_alloc.
                                unsafe { hew_msg_node_free(old) };
                                mb.count.fetch_sub(1, Ordering::Release);
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node = unsafe { msg_node_alloc(msg_type, data, size) };
                            q.user_queue.push_back(node);
                            mb.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mb);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return HewError::Ok as i32;
                        }
                        HewOverflowPolicy::Coalesce => unreachable!(),
                    }
                }
                // DROP_OLD: dequeue the oldest message, then push the new one.
                HewOverflowPolicy::DropOld => {
                    let mut q = match mb.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    if let Some(old) = q.user_queue.pop_front() {
                        // SAFETY: node was allocated by msg_node_alloc.
                        unsafe { hew_msg_node_free(old) };
                        mb.count.fetch_sub(1, Ordering::Release);
                    }
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, size) };
                    q.user_queue.push_back(node);
                    mb.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mb);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return HewError::Ok as i32;
                }
            }
        }
    }

    // Fast path: no capacity issue (or unbounded).
    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size) };

    if mb.use_slow_path {
        let mut q = match mb.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        q.user_queue.push_back(node);
    } else {
        // SAFETY: `node` was just allocated with next == null.
        unsafe { mb.user_fast.enqueue(node) };
    }

    mb.count.fetch_add(1, Ordering::Release);
    update_high_water_mark(mb);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);

    HewError::Ok as i32
}

/// Non-blocking send that always fails immediately when at capacity.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_send(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    if mb.closed.load(Ordering::Acquire) {
        return HewError::ErrActorStopped as i32;
    }

    if mb.capacity > 0 {
        let cur = mb.count.load(Ordering::Acquire);
        if cur >= mb.capacity {
            return HewError::ErrMailboxFull as i32;
        }
    }

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size) };

    if mb.use_slow_path {
        let mut q = match mb.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        q.user_queue.push_back(node);
    } else {
        // SAFETY: `node` was just allocated with next == null.
        unsafe { mb.user_fast.enqueue(node) };
    }

    mb.count.fetch_add(1, Ordering::Release);
    update_high_water_mark(mb);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);

    HewError::Ok as i32
}

/// Send a system message, bypassing capacity limits.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_send`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_send_sys(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *mut c_void,
    size: usize,
) {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, size) };

    // SAFETY: `node` was just allocated with next == null.
    unsafe { mb.sys_queue.enqueue(node) };
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
}

/// Policy-aware push into the user queue.
///
/// Returns `0` on success, `1` if the message was dropped (`DropNew` policy),
/// `2` if the oldest message was dropped (`DropOld` policy), or `-1` on
/// failure (`Fail`/`Block` policy, or closed mailbox).
///
/// # Panics
///
/// Panics if memory allocation for the message node or coalesced data fails (OOM).
///
/// # Safety
///
/// - `mb` must be a valid mailbox pointer.
/// - `data` must point to at least `data_size` readable bytes, or be null
///   when `data_size` is 0.
#[no_mangle]
#[expect(
    clippy::too_many_lines,
    reason = "mailbox try_push with overflow policies is inherently complex"
)]
pub unsafe extern "C" fn hew_mailbox_try_push(
    mb: *mut HewMailbox,
    msg_type: i32,
    data: *const c_void,
    data_size: usize,
) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mbr = unsafe { &*mb };

    if mbr.closed.load(Ordering::Acquire) {
        return -1;
    }

    if mbr.capacity > 0 {
        let cur = mbr.count.load(Ordering::Acquire);
        if cur >= mbr.capacity {
            match mbr.overflow {
                HewOverflowPolicy::DropNew => return 1,
                HewOverflowPolicy::DropOld => {
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, data_size) };
                    let mut q = match mbr.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    if let Some(old) = q.user_queue.pop_front() {
                        // SAFETY: node was allocated by msg_node_alloc.
                        unsafe { hew_msg_node_free(old) };
                        mbr.count.fetch_sub(1, Ordering::Release);
                    }
                    q.user_queue.push_back(node);
                    mbr.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mbr);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return 2;
                }
                HewOverflowPolicy::Coalesce => {
                    let mut q = match mbr.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    // Scan for an existing message with the same coalesce key.
                    // SAFETY: `data` validity guaranteed by caller.
                    let incoming_key = unsafe {
                        coalesce_message_key(
                            mbr.coalesce_key_fn,
                            msg_type,
                            data.cast_mut(),
                            data_size,
                        )
                    };
                    let found = q
                        .user_queue
                        .iter()
                        .find(|&&n| {
                            // SAFETY: all nodes in the queue were allocated by msg_node_alloc
                            // and are valid while the lock is held.
                            unsafe {
                                coalesce_message_key(
                                    mbr.coalesce_key_fn,
                                    (*n).msg_type,
                                    (*n).data,
                                    (*n).data_size,
                                ) == incoming_key
                            }
                        })
                        .copied();
                    if let Some(existing) = found {
                        // SAFETY: `existing` is a valid node in the queue.
                        // Replace its data with the new payload.
                        unsafe { replace_node_payload(existing, msg_type, data, data_size) };
                        return 3; // coalesced
                    }
                    // No matching key — use configured fallback policy.
                    match normalize_coalesce_fallback(mbr.coalesce_fallback) {
                        HewOverflowPolicy::DropNew => return 1,
                        HewOverflowPolicy::Fail => return -1,
                        HewOverflowPolicy::DropOld => {
                            if let Some(old) = q.user_queue.pop_front() {
                                // SAFETY: node was allocated by msg_node_alloc.
                                unsafe { hew_msg_node_free(old) };
                                mbr.count.fetch_sub(1, Ordering::Release);
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node = unsafe { msg_node_alloc(msg_type, data, data_size) };
                            q.user_queue.push_back(node);
                            mbr.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mbr);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return 2;
                        }
                        HewOverflowPolicy::Block => {
                            loop {
                                if mbr.closed.load(Ordering::Acquire) {
                                    return -1;
                                }
                                let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                                if len < mbr.capacity {
                                    break;
                                }
                                q = match mbr.not_full.wait(q) {
                                    Ok(g) => g,
                                    Err(e) => e.into_inner(),
                                };
                            }
                            // SAFETY: `data` validity guaranteed by caller.
                            let node = unsafe { msg_node_alloc(msg_type, data, data_size) };
                            q.user_queue.push_back(node);
                            drop(q);
                            mbr.count.fetch_add(1, Ordering::Release);
                            update_high_water_mark(mbr);
                            MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                            return 0;
                        }
                        HewOverflowPolicy::Coalesce => unreachable!(),
                    }
                }
                HewOverflowPolicy::Fail => return -1,
                HewOverflowPolicy::Block => {
                    // Wait on condvar until space is available.
                    let mut q = match mbr.slow_path.lock() {
                        Ok(g) => g,
                        Err(e) => e.into_inner(),
                    };
                    loop {
                        if mbr.closed.load(Ordering::Acquire) {
                            return -1;
                        }
                        let len = i64::try_from(q.user_queue.len()).unwrap_or(i64::MAX);
                        if len < mbr.capacity {
                            break;
                        }
                        q = match mbr.not_full.wait(q) {
                            Ok(g) => g,
                            Err(e) => e.into_inner(),
                        };
                    }
                    // SAFETY: `data` validity guaranteed by caller.
                    let node = unsafe { msg_node_alloc(msg_type, data, data_size) };
                    q.user_queue.push_back(node);
                    drop(q);
                    mbr.count.fetch_add(1, Ordering::Release);
                    update_high_water_mark(mbr);
                    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
                    return 0;
                }
            }
        }
    }

    // SAFETY: `data` validity guaranteed by caller.
    let node = unsafe { msg_node_alloc(msg_type, data, data_size) };

    if mbr.use_slow_path {
        let mut q = match mbr.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        q.user_queue.push_back(node);
    } else {
        // SAFETY: `node` was just allocated with next == null.
        unsafe { mbr.user_fast.enqueue(node) };
    }

    mbr.count.fetch_add(1, Ordering::Release);
    update_high_water_mark(mbr);
    MESSAGES_SENT.fetch_add(1, Ordering::Relaxed);
    0
}

// ── Close ───────────────────────────────────────────────────────────────

/// Close a mailbox so that future sends are rejected.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
pub(crate) unsafe fn mailbox_close(mb: *mut HewMailbox) {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    mb.closed.store(true, Ordering::Release);
    // Wake any senders blocked on a full mailbox.
    mb.not_full.notify_all();
}

/// Returns `true` if the mailbox has been closed.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
pub(crate) unsafe fn mailbox_is_closed(mb: *mut HewMailbox) -> bool {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };
    mb.closed.load(Ordering::Acquire)
}

// ── Receive (consumer side) ─────────────────────────────────────────────

/// Try to receive a message. System messages have priority.
///
/// Returns a pointer to a [`HewMsgNode`] on success, or null if both
/// queues are empty. The caller owns the returned node and must free it
/// with [`hew_msg_node_free`].
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer. Only one thread may call recv
/// functions at a time (single-consumer invariant).
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_recv(mb: *mut HewMailbox) -> *mut HewMsgNode {
    // SAFETY: Caller guarantees `mb` is valid and single-consumer.
    let mb = unsafe { &*mb };

    // System messages have priority (lock-free dequeue).
    // SAFETY: single-consumer invariant satisfied by caller.
    let sys_node = unsafe { mb.sys_queue.try_dequeue() };
    if !sys_node.is_null() {
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        return sys_node;
    }

    // User messages: slow path uses mutex, fast path uses lock-free queue.
    if mb.use_slow_path {
        let mut q = match mb.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        if let Some(node) = q.user_queue.pop_front() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            drop(q);
            mb.not_full.notify_one();
            return node;
        }
    } else {
        // SAFETY: single-consumer invariant satisfied by caller.
        let node = unsafe { mb.user_fast.try_dequeue() };
        if !node.is_null() {
            mb.count.fetch_sub(1, Ordering::Release);
            MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
            return node;
        }
    }

    ptr::null_mut()
}

/// Try to receive a system message only.
///
/// # Safety
///
/// Same requirements as [`hew_mailbox_try_recv`].
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_try_recv_sys(mb: *mut HewMailbox) -> *mut HewMsgNode {
    // SAFETY: Caller guarantees `mb` is valid and single-consumer.
    let mb = unsafe { &*mb };

    // SAFETY: single-consumer invariant satisfied by caller.
    let node = unsafe { mb.sys_queue.try_dequeue() };
    if !node.is_null() {
        MESSAGES_RECEIVED.fetch_add(1, Ordering::Relaxed);
        return node;
    }

    ptr::null_mut()
}

// ── Queries ─────────────────────────────────────────────────────────────

/// Returns `1` if either queue has messages, `0` otherwise.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_has_messages(mb: *mut HewMailbox) -> i32 {
    // SAFETY: Caller guarantees `mb` is valid.
    let mb = unsafe { &*mb };

    if mb.sys_queue.has_messages() {
        return 1;
    }

    if mb.use_slow_path {
        let q = match mb.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        i32::from(!q.user_queue.is_empty())
    } else {
        i32::from(mb.user_fast.has_messages())
    }
}

/// Return the number of user messages in the mailbox.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_len(mb: *const HewMailbox) -> usize {
    // SAFETY: Caller guarantees `mb` is valid.
    let count = unsafe { &*mb }.count.load(Ordering::Acquire);
    usize::try_from(count).unwrap_or(0)
}

/// Return the mailbox capacity. Returns `0` for unbounded mailboxes.
///
/// # Safety
///
/// `mb` must be a valid mailbox pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_capacity(mb: *const HewMailbox) -> usize {
    // SAFETY: Caller guarantees `mb` is valid.
    let cap = unsafe { &*mb }.capacity;
    usize::try_from(cap).unwrap_or(0)
}

// ── Cleanup ─────────────────────────────────────────────────────────────

/// Free the mailbox, draining and freeing all remaining messages.
///
/// # Safety
///
/// `mb` must have been returned by [`hew_mailbox_new`] or
/// [`hew_mailbox_new_bounded`] and must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn hew_mailbox_free(mb: *mut HewMailbox) {
    if mb.is_null() {
        return;
    }

    // SAFETY: Caller guarantees `mb` was Box-allocated and is exclusively owned.
    let mailbox = unsafe { Box::from_raw(mb) };

    // Drain slow-path user queue (if used).
    {
        let mut q = match mailbox.slow_path.lock() {
            Ok(g) => g,
            Err(e) => e.into_inner(),
        };
        while let Some(node) = q.user_queue.pop_front() {
            // SAFETY: Each node was allocated by `msg_node_alloc`.
            unsafe { hew_msg_node_free(node) };
        }
    }

    // Drain lock-free user queue (sentinel + any remaining nodes).
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    unsafe { mailbox.user_fast.drain_and_free() };

    // Drain lock-free system queue (sentinel + any remaining nodes).
    // SAFETY: No concurrent access — mailbox is exclusively owned.
    unsafe { mailbox.sys_queue.drain_and_free() };
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[repr(C)]
    #[derive(Clone, Copy)]
    struct PriceUpdate {
        symbol: u64,
        price: i32,
    }

    unsafe extern "C" fn price_symbol_key(
        _msg_type: i32,
        data: *mut c_void,
        data_size: usize,
    ) -> u64 {
        if data.is_null() || data_size < size_of::<PriceUpdate>() {
            return 0;
        }
        // SAFETY: caller passes a valid PriceUpdate payload.
        unsafe { (*data.cast::<PriceUpdate>()).symbol }
    }

    #[test]
    fn new_mailbox_is_empty() {
        // SAFETY: test owns the mailbox exclusively.
        unsafe {
            let mb = hew_mailbox_new();
            assert_eq!(hew_mailbox_has_messages(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn send_and_recv_one() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 42;
            let rc = hew_mailbox_send(mb, 1, (&raw const val).cast_mut().cast(), size_of::<i32>());
            assert_eq!(rc, 0);
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 1);
            assert_eq!((*node).data_size, size_of::<i32>());
            assert_eq!(*((*node).data.cast::<i32>()), 42);
            hew_msg_node_free(node);

            assert_eq!(hew_mailbox_has_messages(mb), 0);
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn bounded_rejects_overflow() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(2);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(hew_mailbox_send(mb, 0, p, size_of::<i32>()), 0);
            // Third send should fail.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn try_send_bounded() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            let val: i32 = 7;
            let p = (&raw const val).cast_mut().cast();

            assert_eq!(hew_mailbox_try_send(mb, 0, p, size_of::<i32>()), 0);
            assert_eq!(
                hew_mailbox_try_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_messages_have_priority() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let u: i32 = 10;
            let s: i32 = 99;

            // Send a user message first, then a system message.
            hew_mailbox_send(mb, 1, (&raw const u).cast_mut().cast(), size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, (&raw const s).cast_mut().cast(), size_of::<i32>());

            // Recv should return the system message first.
            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn recv_sys_only() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let val: i32 = 5;
            let p = (&raw const val).cast_mut().cast();

            hew_mailbox_send(mb, 1, p, size_of::<i32>());
            hew_mailbox_send_sys(mb, 2, p, size_of::<i32>());

            // try_recv_sys should only return system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(!node.is_null());
            assert_eq!((*node).msg_type, 2);
            hew_msg_node_free(node);

            // No more system messages.
            let node = hew_mailbox_try_recv_sys(mb);
            assert!(node.is_null());

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn recv_empty_returns_null() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            assert!(hew_mailbox_try_recv(mb).is_null());
            assert!(hew_mailbox_try_recv_sys(mb).is_null());
            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn null_data_succeeds() {
        // SAFETY: test owns the mailbox exclusively; null data is a valid input.
        unsafe {
            let mb = hew_mailbox_new();
            let rc = hew_mailbox_send(mb, 0, ptr::null_mut(), 0);
            assert_eq!(rc, 0);

            let node = hew_mailbox_try_recv(mb);
            assert!(!node.is_null());
            assert!((*node).data.is_null());
            assert_eq!((*node).data_size, 0);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn sys_bypasses_capacity() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_bounded(1);
            let val: i32 = 1;
            let p = (&raw const val).cast_mut().cast();

            // Fill user queue.
            hew_mailbox_send(mb, 0, p, size_of::<i32>());
            // User queue is full.
            assert_eq!(
                hew_mailbox_send(mb, 0, p, size_of::<i32>()),
                HewError::ErrMailboxFull as i32
            );
            // System message should still succeed.
            hew_mailbox_send_sys(mb, 99, p, size_of::<i32>());
            assert_eq!(hew_mailbox_has_messages(mb), 1);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_uses_configured_key_fn() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(2);
            hew_mailbox_set_coalesce_config(mb, Some(price_symbol_key), HewOverflowPolicy::DropOld);

            let a = PriceUpdate {
                symbol: 7,
                price: 10,
            };
            let b = PriceUpdate {
                symbol: 9,
                price: 20,
            };
            let c = PriceUpdate {
                symbol: 7,
                price: 99,
            };

            assert_eq!(
                hew_mailbox_try_push(mb, 100, (&raw const a).cast(), size_of::<PriceUpdate>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 200, (&raw const b).cast(), size_of::<PriceUpdate>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 300, (&raw const c).cast(), size_of::<PriceUpdate>()),
                3
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 300);
            let payload = (*node).data.cast::<PriceUpdate>();
            assert_eq!((*payload).symbol, 7);
            assert_eq!((*payload).price, 99);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn coalesce_uses_configured_fallback_policy() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new_coalesce(1);
            hew_mailbox_set_coalesce_config(mb, None, HewOverflowPolicy::DropNew);

            let a: i32 = 10;
            let b: i32 = 20;
            assert_eq!(
                hew_mailbox_try_push(mb, 1, (&raw const a).cast(), size_of::<i32>()),
                0
            );
            assert_eq!(
                hew_mailbox_try_push(mb, 2, (&raw const b).cast(), size_of::<i32>()),
                1
            );

            let node = hew_mailbox_try_recv(mb);
            assert_eq!((*node).msg_type, 1);
            hew_msg_node_free(node);

            hew_mailbox_free(mb);
        }
    }

    #[test]
    fn deep_copy_isolation() {
        // SAFETY: test owns the mailbox exclusively; all pointers are valid.
        unsafe {
            let mb = hew_mailbox_new();
            let mut val: i32 = 100;
            hew_mailbox_send(mb, 0, (&raw mut val).cast(), size_of::<i32>());

            // Mutate original after send.
            val = 999;

            let node = hew_mailbox_try_recv(mb);
            // Message should have the original value.
            assert_eq!(*((*node).data.cast::<i32>()), 100);
            hew_msg_node_free(node);

            // Suppress unused-value warning.
            let _ = val;

            hew_mailbox_free(mb);
        }
    }
}
