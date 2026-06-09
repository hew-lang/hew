//! Suspending channel core for typed streams (NEW-7).
//!
//! The shared state behind an in-memory `(Sink<T>, Stream<T>)` pipe. Replaces
//! the old `mpsc::sync_channel` backing so a consumer that `await stream.recv()`
//! / a producer that `await sink.send(x)` can PARK its coroutine continuation
//! over the read-slot / `enqueue_resume` substrate (NEW-1) instead of blocking a
//! scheduler worker.
//!
//! ## Value routing
//!
//! Unlike the reactor read-slot, the channel slot carries NO bytes — it is a
//! pure readiness signal. Item ownership stays in the `queue` (or, for a parked
//! producer that hit a full ring, inside its `Waiter`). The woken consumer pops
//! the item from the queue ON ITS OWN execution edge (`pop`), so an item is
//! delivered exactly once even if the wake races the consumer's teardown: a
//! dead/cancelled consumer simply never pops, and the item remains queued for
//! the next consumer (or is dropped when the pipe is torn down).
//!
//! ## Wake discipline (`bg-thread-mailbox-delivery-toctou`)
//!
//! A parked peer is recorded as a `Waiter { actor, slot }`; the core holds an
//! independent in-flight ref on the slot (`read_slot_retain`) for as long as the
//! registration lives, released on EVERY exit (normal wake, detach, close). The
//! wake itself goes through [`crate::scheduler::enqueue_resume`], which
//! re-confirms the caller actor's liveness under the `LIVE_ACTORS` lock and
//! drops the wake with no deref if the actor was freed. The deposit checks the
//! slot's cancelled flag, so an abandon edge that won the race makes a late wake
//! a no-op. Wakes are always performed AFTER the core lock is released to avoid
//! re-entrancy into the scheduler under the channel lock.
//!
//! Native-only: parks a coroutine continuation woken by `enqueue_resume`, which
//! does not exist on `wasm32`.

use std::collections::VecDeque;
use std::sync::{Condvar, Mutex};

use crate::actor::HewActor;
use crate::read_slot::{
    hew_read_slot_free, read_slot_deposit_status, read_slot_retain, HewReadSlot, ReadStatus,
};

/// Codegen ABI: the await entry parked the continuation; the runtime will wake
/// it via `enqueue_resume`. The caller MUST `coro.suspend`.
pub const STREAM_AWAIT_SUSPEND: i32 = 0;
/// Codegen ABI: the await entry is already satisfiable (an item is queued, the
/// peer closed, or the ring had space). The caller MUST NOT suspend; it binds
/// the result on the immediate edge.
pub const STREAM_AWAIT_READY: i32 = 1;

/// One parked peer (a consumer awaiting an item, or a producer blocked on a full
/// ring). The core owns one in-flight ref on `slot`.
struct Waiter {
    /// The parked-continuation actor, woken via `enqueue_resume`. Raw and
    /// possibly-stale: `enqueue_resume` validates liveness, never this code.
    actor: *mut HewActor,
    /// The readiness slot; the core holds one retained ref while registered.
    slot: *mut HewReadSlot,
    /// A parked producer's pending item, enqueued by the drainer when space
    /// frees. `None` for a consumer waiter.
    item: Option<Vec<u8>>,
}

struct Inner {
    queue: VecDeque<Vec<u8>>,
    capacity: usize,
    /// The producer signalled EOF (`sink.close()` / sink drop).
    sink_closed: bool,
    /// The consumer cancelled (`stream.close()` / stream drop).
    stream_closed: bool,
    /// At most one parked consumer (a `Stream<T>` has a single owner).
    consumer: Option<Waiter>,
    /// Parked producers blocked on a full ring, woken FIFO as space frees.
    producers: VecDeque<Waiter>,
}

/// Shared in-memory pipe state, held by `Arc` from BOTH the stream backing and
/// the sink backing.
pub struct ChannelCore {
    inner: Mutex<Inner>,
    /// Notifies blocking (non-suspending) `recv`/`send` callers — the default
    /// callers (`main`, free fns) that run on a foreign thread with no parkable
    /// continuation. Suspending callers never touch this; they use slots.
    cv: Condvar,
}

// SAFETY: every field of `Inner` is accessed only under `inner`'s lock. The raw
// `*mut HewActor` / `*mut HewReadSlot` in a `Waiter` are never dereferenced
// here: the actor goes through `enqueue_resume`'s liveness-checked wake and the
// slot through the atomic read-slot API. The contained `Vec<u8>` items are owned.
unsafe impl Send for ChannelCore {}
// SAFETY: as above — all shared mutation is serialised by the `Mutex`.
unsafe impl Sync for ChannelCore {}

impl std::fmt::Debug for ChannelCore {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Do not lock in Debug (avoids a deadlock if formatted while held).
        f.debug_struct("ChannelCore").finish_non_exhaustive()
    }
}

impl ChannelCore {
    /// Build a fresh pipe core with the given bounded capacity (clamped to >= 1).
    #[must_use]
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: Mutex::new(Inner {
                queue: VecDeque::new(),
                capacity: capacity.max(1),
                sink_closed: false,
                stream_closed: false,
                consumer: None,
                producers: VecDeque::new(),
            }),
            cv: Condvar::new(),
        }
    }

    /// Lock the inner state, recovering from poisoning rather than panicking
    /// (a poisoned channel lock must not take down an unrelated worker).
    fn locked(&self) -> std::sync::MutexGuard<'_, Inner> {
        self.inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
    }

    /// Deposit a Data readiness signal and wake the parked peer. Runs OUTSIDE
    /// the core lock. The deposit is a no-op + no wake if the peer cancelled.
    ///
    /// # Safety
    ///
    /// `w.slot` must be a slot the core holds an in-flight ref to (released
    /// here); `w.actor` may be stale (`enqueue_resume` validates it).
    #[allow(
        clippy::needless_pass_by_value,
        reason = "takes the Waiter by value to consume it: the wake releases the \
                  in-flight slot ref and drops any pending producer item exactly once"
    )]
    unsafe fn wake(w: Waiter) {
        // SAFETY: the core holds an in-flight ref on the slot; depositing a
        // terminal status is the documented reactor-deposit contract.
        let do_wake = unsafe { read_slot_deposit_status(w.slot, ReadStatus::Data) };
        if do_wake {
            // SAFETY: `enqueue_resume` re-validates `w.actor` under the registry
            // lock; a freed actor drops the wake with no deref.
            unsafe { crate::scheduler::enqueue_resume(w.actor, std::ptr::null_mut()) };
        }
        // Release the core's in-flight ref (the single authority for it).
        // SAFETY: the core owned this ref; nothing else releases it.
        unsafe { hew_read_slot_free(w.slot) };
    }

    // ── Consumer side ────────────────────────────────────────────────────────

    /// Suspending consumer registration. Returns [`STREAM_AWAIT_READY`] when an
    /// item is queued or the producer has closed (bind via [`Self::pop`]), or
    /// [`STREAM_AWAIT_SUSPEND`] after parking the consumer on `slot`.
    ///
    /// # Safety
    ///
    /// `actor` is the awaiting actor (`hew_actor_self`); `slot` is a live read
    /// slot the caller created and holds the creator ref to.
    pub unsafe fn await_next(&self, actor: *mut HewActor, slot: *mut HewReadSlot) -> i32 {
        let mut inner = self.locked();
        if !inner.queue.is_empty() || inner.sink_closed {
            return STREAM_AWAIT_READY;
        }
        // Park: the core takes an in-flight ref so a producer deposit / close
        // cannot free the slot out from under the abandon edge.
        // SAFETY: caller holds the creator ref, so the slot is live to retain.
        unsafe { read_slot_retain(slot) };
        // Replace any prior (abandoned) consumer registration; a Stream<T> has a
        // single owner, so a live double-park cannot occur, but a stale slot from
        // a torn-down park must release its ref.
        if let Some(prev) = inner.consumer.replace(Waiter {
            actor,
            slot,
            item: None,
        }) {
            // SAFETY: `prev` was a registration the core owned an in-flight ref
            // to; release it (no wake — this is a replacement, not a delivery).
            unsafe { hew_read_slot_free(prev.slot) };
        }
        STREAM_AWAIT_SUSPEND
    }

    /// Pop one item (the consumer bind edge / a non-suspending blocking caller's
    /// fast path). Drains one parked producer into the freed ring slot and wakes
    /// it. Returns `None` on an empty-and-closed pipe (EOF) or an empty-and-open
    /// pipe (spurious — the caller maps both to "no item").
    #[must_use]
    pub fn pop(&self) -> Option<Vec<u8>> {
        let producer_wake;
        let item;
        {
            let mut inner = self.locked();
            item = inner.queue.pop_front();
            producer_wake = if item.is_some() {
                Self::drain_one_producer(&mut inner)
            } else {
                None
            };
        }
        if let Some(w) = producer_wake {
            // SAFETY: the waiter was removed from `producers` under the lock; we
            // own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        if item.is_some() {
            self.cv.notify_all();
        }
        item
    }

    /// Blocking consumer recv (default callers: `main`, free fns). Parks the
    /// FOREIGN thread on the condvar — never a scheduler worker's coroutine.
    #[must_use]
    pub fn blocking_recv(&self) -> Option<Vec<u8>> {
        let producer_wake;
        let item;
        {
            let mut inner = self.locked();
            loop {
                if let Some(v) = inner.queue.pop_front() {
                    item = Some(v);
                    break;
                }
                if inner.sink_closed {
                    item = None;
                    break;
                }
                inner = self
                    .cv
                    .wait(inner)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
            producer_wake = if item.is_some() {
                Self::drain_one_producer(&mut inner)
            } else {
                None
            };
        }
        if let Some(w) = producer_wake {
            // SAFETY: removed under the lock; we own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        if item.is_some() {
            self.cv.notify_all();
        }
        item
    }

    /// Non-blocking consumer poll (`try_recv`). Drains a producer on success.
    #[must_use]
    pub fn try_recv(&self) -> Option<Vec<u8>> {
        // Identical to `pop`: pop returns the queued item if any, else None.
        self.pop()
    }

    /// Block until the channel is readable (an item is queued or the sink
    /// closed) WITHOUT consuming, or until `cancelled` is observed. Returns
    /// `true` when readable, `false` when cancelled. Used by the `select{}`
    /// channel-recv arm's poll thread: the winner edge pops the item itself via
    /// the non-blocking `try_recv`, so this never consumes (a losing arm leaves
    /// its item queued for the next consumer). Foreign-thread blocking only —
    /// the select substrate itself is not worker-free (it mirrors the stream
    /// select arm's poll thread); the worker-free path is `await rx.recv()`.
    #[must_use]
    pub fn wait_ready(&self, cancelled: &std::sync::atomic::AtomicBool) -> bool {
        let mut inner = self.locked();
        loop {
            if cancelled.load(std::sync::atomic::Ordering::Acquire) {
                return false;
            }
            if !inner.queue.is_empty() || inner.sink_closed {
                return true;
            }
            inner = self
                .cv
                .wait(inner)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }

    /// Cancel a pending [`Self::wait_ready`] without a lost-wakeup window: set
    /// the caller's `cancelled` predicate AND notify the condvar while holding
    /// the SAME `inner` mutex that `wait_ready` checks the predicate under.
    ///
    /// `wait_ready` evaluates `cancelled` and the readiness predicate while
    /// holding `inner`, then atomically releases `inner` and parks on `cv`.
    /// Because this store + `notify_all` run under `inner`, they cannot
    /// interleave between the waiter's predicate check and its park: either we
    /// win the lock first (the waiter then observes `cancelled` before parking),
    /// or the waiter parks first (releasing `inner`) and our subsequent
    /// `notify_all` reaches the already-parked waiter. The cancel wake can never
    /// be lost.
    pub fn cancel_wait(&self, cancelled: &std::sync::atomic::AtomicBool) {
        let _inner = self.locked();
        cancelled.store(true, std::sync::atomic::Ordering::Release);
        self.cv.notify_all();
    }

    /// Detach an abandoned consumer registration (the codegen abandon edge).
    /// Releases the core's in-flight ref if `slot` is still the registered
    /// consumer; idempotent if a deposit already consumed it.
    ///
    /// # Safety
    ///
    /// `slot` must be the consumer's read slot.
    pub unsafe fn detach_consumer(&self, slot: *mut HewReadSlot) {
        let removed = {
            let mut inner = self.locked();
            match &inner.consumer {
                Some(w) if w.slot == slot => inner.consumer.take(),
                _ => None,
            }
        };
        if let Some(w) = removed {
            // SAFETY: removed under the lock; release the core's in-flight ref.
            unsafe { hew_read_slot_free(w.slot) };
        }
    }

    // ── Producer side ────────────────────────────────────────────────────────

    /// Suspending producer send. Pushes `item` and returns [`STREAM_AWAIT_READY`]
    /// when the ring has space (or the consumer has gone / the producer already
    /// closed — the write is then a silent discard, never an enqueue past EOF);
    /// parks the producer on `slot` and returns [`STREAM_AWAIT_SUSPEND`] when the
    /// ring is full.
    ///
    /// # Safety
    ///
    /// `actor` is the sending actor; `slot` is a live read slot owned by the
    /// caller.
    pub unsafe fn await_send(
        &self,
        actor: *mut HewActor,
        slot: *mut HewReadSlot,
        item: Vec<u8>,
    ) -> i32 {
        let consumer_wake;
        {
            let mut inner = self.locked();
            if inner.stream_closed || inner.sink_closed {
                // Consumer gone (mpsc-disconnect), or the producer already
                // signalled EOF (`close_sink`): fail closed — the send is a
                // no-op, never an enqueue past EOF, and never a park.
                return STREAM_AWAIT_READY;
            }
            if inner.queue.len() < inner.capacity {
                inner.queue.push_back(item);
                consumer_wake = inner.consumer.take();
            } else {
                // Full ring: park the producer; it owns `item` across the suspend.
                // SAFETY: caller holds the creator ref, so the slot is live.
                unsafe { read_slot_retain(slot) };
                inner.producers.push_back(Waiter {
                    actor,
                    slot,
                    item: Some(item),
                });
                return STREAM_AWAIT_SUSPEND;
            }
        }
        if let Some(w) = consumer_wake {
            // SAFETY: removed under the lock; we own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        self.cv.notify_all();
        STREAM_AWAIT_READY
    }

    /// Blocking producer send (default callers). Parks the FOREIGN thread on the
    /// condvar until the ring has space; a silent discard if the consumer is gone.
    pub fn blocking_send(&self, item: Vec<u8>) {
        let consumer_wake;
        {
            let mut inner = self.locked();
            loop {
                if inner.stream_closed {
                    return;
                }
                if inner.queue.len() < inner.capacity {
                    inner.queue.push_back(item);
                    consumer_wake = inner.consumer.take();
                    break;
                }
                inner = self
                    .cv
                    .wait(inner)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
        }
        if let Some(w) = consumer_wake {
            // SAFETY: removed under the lock; we own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        self.cv.notify_all();
    }

    /// Non-blocking producer send (`try_send`). Returns `true` if accepted,
    /// `false` if the ring is full.
    #[must_use]
    pub fn try_send(&self, item: Vec<u8>) -> bool {
        let consumer_wake;
        {
            let mut inner = self.locked();
            if inner.stream_closed {
                return true; // discard, not "full"
            }
            if inner.queue.len() >= inner.capacity {
                return false;
            }
            inner.queue.push_back(item);
            consumer_wake = inner.consumer.take();
        }
        if let Some(w) = consumer_wake {
            // SAFETY: removed under the lock; we own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        self.cv.notify_all();
        true
    }

    /// Detach an abandoned producer registration (the codegen abandon edge).
    /// Drops the parked item and releases the core's in-flight ref.
    ///
    /// # Safety
    ///
    /// `slot` must be the producer's read slot.
    pub unsafe fn detach_producer(&self, slot: *mut HewReadSlot) {
        let removed = {
            let mut inner = self.locked();
            if let Some(pos) = inner.producers.iter().position(|w| w.slot == slot) {
                inner.producers.remove(pos)
            } else {
                None
            }
        };
        if let Some(w) = removed {
            // SAFETY: removed under the lock; release the core's in-flight ref.
            unsafe { hew_read_slot_free(w.slot) };
        }
    }

    // ── Close edges ──────────────────────────────────────────────────────────

    /// The producer closed (EOF). Wakes a parked consumer so its `await_next`
    /// resume binds `None`.
    pub fn close_sink(&self) {
        let consumer_wake;
        {
            let mut inner = self.locked();
            inner.sink_closed = true;
            consumer_wake = inner.consumer.take();
        }
        if let Some(w) = consumer_wake {
            // SAFETY: removed under the lock; we own its in-flight ref.
            unsafe { Self::wake(w) };
        }
        self.cv.notify_all();
    }

    /// The consumer closed (local cancel/discard). Wakes every parked producer
    /// so their `await_send` resumes (the writes become no-ops) and drops their
    /// pending items.
    pub fn close_stream(&self) {
        let mut wakes: Vec<Waiter> = Vec::new();
        {
            let mut inner = self.locked();
            inner.stream_closed = true;
            while let Some(w) = inner.producers.pop_front() {
                wakes.push(w);
            }
        }
        for w in wakes {
            // SAFETY: removed under the lock; we own each in-flight ref. The
            // pending item is dropped with the `Waiter`.
            unsafe { Self::wake(w) };
        }
        self.cv.notify_all();
    }

    /// Pop one parked producer (if any) and re-enqueue its item, returning the
    /// waiter to wake. Caller holds the lock.
    fn drain_one_producer(inner: &mut Inner) -> Option<Waiter> {
        if inner.queue.len() >= inner.capacity {
            return None;
        }
        let mut w = inner.producers.pop_front()?;
        if let Some(item) = w.item.take() {
            inner.queue.push_back(item);
        }
        Some(w)
    }
}

#[cfg(test)]
#[allow(
    clippy::undocumented_unsafe_blocks,
    reason = "test-only: each unsafe call exercises the slot/await lifecycle the \
              test body describes; every pointer is a fresh local slot"
)]
mod tests {
    use super::*;
    use crate::read_slot::{hew_read_slot_new, hew_read_slot_status};

    #[test]
    fn fifo_push_pop_without_parking() {
        let core = ChannelCore::new(4);
        assert!(core.try_send(b"a".to_vec()));
        assert!(core.try_send(b"b".to_vec()));
        assert_eq!(core.pop(), Some(b"a".to_vec()));
        assert_eq!(core.pop(), Some(b"b".to_vec()));
        assert_eq!(core.pop(), None);
    }

    #[test]
    fn full_ring_rejects_try_send() {
        let core = ChannelCore::new(1);
        assert!(core.try_send(b"x".to_vec()));
        assert!(!core.try_send(b"y".to_vec()), "ring is full");
        assert_eq!(core.pop(), Some(b"x".to_vec()));
        assert!(core.try_send(b"y".to_vec()), "space freed by pop");
    }

    #[test]
    fn await_next_ready_when_item_queued() {
        let core = ChannelCore::new(2);
        assert!(core.try_send(b"hi".to_vec()));
        let slot = hew_read_slot_new();
        let rc = unsafe { core.await_next(std::ptr::null_mut(), slot) };
        assert_eq!(rc, STREAM_AWAIT_READY);
        // Not parked: detach is a no-op, free the creator ref.
        unsafe { core.detach_consumer(slot) };
        unsafe { hew_read_slot_free(slot) };
        assert_eq!(core.pop(), Some(b"hi".to_vec()));
    }

    #[test]
    fn await_next_parks_then_send_deposits_and_drops_ref() {
        let core = ChannelCore::new(2);
        let slot = hew_read_slot_new();
        // Park: empty + open.
        let rc = unsafe { core.await_next(std::ptr::null_mut(), slot) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        // A producer write deposits a Data signal + (null-actor) wake is dropped,
        // and releases the core's in-flight ref.
        assert!(core.try_send(b"z".to_vec()));
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Data as i32
        );
        // The consumer bind pops the actual item exactly once.
        assert_eq!(core.pop(), Some(b"z".to_vec()));
        // Release the creator ref (last ref → slot freed).
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn close_sink_wakes_parked_consumer_for_eof() {
        let core = ChannelCore::new(2);
        let slot = hew_read_slot_new();
        let rc = unsafe { core.await_next(std::ptr::null_mut(), slot) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        core.close_sink();
        // Woken; bind pops None (EOF).
        assert_eq!(core.pop(), None);
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn abandon_detach_releases_ref_and_suppresses_late_wake() {
        let core = ChannelCore::new(2);
        let slot = hew_read_slot_new();
        let rc = unsafe { core.await_next(std::ptr::null_mut(), slot) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        // Abandon edge: cancel the slot, then detach (releases core ref), then
        // free the creator ref. A racing producer deposit is dropped.
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
        unsafe { core.detach_consumer(slot) };
        unsafe { hew_read_slot_free(slot) };
        // The item the producer writes after abandon stays queued (exactly-once:
        // never lost), available to the next consumer.
        assert!(core.try_send(b"late".to_vec()));
        assert_eq!(core.pop(), Some(b"late".to_vec()));
    }

    #[test]
    fn parked_producer_drained_on_pop() {
        let core = ChannelCore::new(1);
        assert!(core.try_send(b"first".to_vec()));
        let slot = hew_read_slot_new();
        // Ring full → producer parks, owning its item.
        let rc = unsafe { core.await_send(std::ptr::null_mut(), slot, b"second".to_vec()) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        // Consumer pops "first"; the drainer re-enqueues "second" + wakes producer.
        assert_eq!(core.pop(), Some(b"first".to_vec()));
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Data as i32
        );
        // "second" now available exactly once.
        assert_eq!(core.pop(), Some(b"second".to_vec()));
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn abandoned_producer_detach_drops_item_and_releases_ref() {
        let core = ChannelCore::new(1);
        assert!(core.try_send(b"first".to_vec()));
        let slot = hew_read_slot_new();
        // Ring full → producer parks, owning "second".
        let rc = unsafe { core.await_send(std::ptr::null_mut(), slot, b"second".to_vec()) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        // Producer abandoned (continuation destroyed): cancel + detach + free.
        // The pending item is dropped (not delivered), and the core's in-flight
        // ref is released so the slot frees with the creator ref.
        unsafe { crate::read_slot::hew_read_slot_cancel(slot) };
        unsafe { core.detach_producer(slot) };
        unsafe { hew_read_slot_free(slot) };
        // Consumer pops "first"; no parked producer remains, so no second item
        // and no wake-after-free.
        assert_eq!(core.pop(), Some(b"first".to_vec()));
        assert_eq!(core.pop(), None);
    }

    #[test]
    fn close_sink_then_await_send_fails_closed_no_post_eof_enqueue() {
        let core = ChannelCore::new(2);
        // Producer signals EOF.
        core.close_sink();
        let slot = hew_read_slot_new();
        // A suspending send AFTER close must fail closed: it must not park (no
        // dangling registration) and must not enqueue past EOF.
        let rc = unsafe { core.await_send(std::ptr::null_mut(), slot, b"post-eof".to_vec()) };
        assert_eq!(rc, STREAM_AWAIT_READY, "post-close send must not park");
        // The item must NOT have been enqueued past EOF: the consumer sees EOF.
        assert_eq!(core.pop(), None, "no post-EOF enqueue after close_sink");
        unsafe { hew_read_slot_free(slot) };
    }

    #[test]
    fn close_stream_wakes_parked_producers_and_drops_items() {
        let core = ChannelCore::new(1);
        assert!(core.try_send(b"x".to_vec()));
        let slot = hew_read_slot_new();
        let rc = unsafe { core.await_send(std::ptr::null_mut(), slot, b"y".to_vec()) };
        assert_eq!(rc, STREAM_AWAIT_SUSPEND);
        // Consumer cancels: the parked producer is woken (its send becomes a
        // no-op) and its item dropped; the core in-flight ref is released.
        core.close_stream();
        assert_eq!(
            unsafe { hew_read_slot_status(slot) },
            ReadStatus::Data as i32
        );
        // Subsequent sends are silent discards (consumer gone).
        let rc2 = unsafe { core.await_send(std::ptr::null_mut(), slot, b"z".to_vec()) };
        assert_eq!(rc2, STREAM_AWAIT_READY);
        unsafe { hew_read_slot_free(slot) };
    }
}
