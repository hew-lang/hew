//! WASM channel groundwork: single-threaded bounded queue using [`VecDeque`].
//!
//! This module is **internal groundwork** for future WASM channel parity.
//! It is compiled only in `#[cfg(test)]` builds and is **not** wired into
//! the `hew_channel_*` C ABI surface. The `wasm_stubs` module in `lib.rs`
//! continues to `unreachable!()`-trap on all channel entry points, and the
//! type checker continues to reject `channel.new` / `Sender<T>` /
//! `Receiver<T>` on wasm32.
//!
//! ## What this module provides
//!
//! A tested, bounded, single-threaded channel queue with:
//! - `VecDeque<Vec<u8>>` storage matching the native `Vec<u8>` transport
//! - Capacity enforcement and sender/receiver lifecycle tracking
//! - `Rc<RefCell<...>>` sharing for multi-sender (clone) support
//! - Safe Rust API exercised by unit tests
//!
//! ## What is still needed to promote this to a real ABI surface
//!
//! Two contract-level gaps must be resolved before these queues can back
//! the `hew_channel_*` C symbols:
//!
//! 1. **`recv` must not conflate empty-with-live-senders and closed.**
//!    The stdlib contract (`std/channel/channel.hew`) specifies that `recv`
//!    returns `None` only when the channel is closed (all senders dropped).
//!    A correct WASM `recv` must either integrate with the cooperative
//!    scheduler to yield-and-resume when the queue is empty but senders
//!    are alive, or the ABI must be extended to distinguish "empty" from
//!    "closed" (e.g. a tri-state out-parameter).
//!
//! 2. **`send` must not silently drop messages at capacity.**
//!    The native `send` blocks on a full channel (backpressure). A correct
//!    WASM `send` must either integrate with the scheduler to yield until
//!    space is available, or the ABI must surface send failure so callers
//!    can observe and handle it.
//!
//! Both require cooperative scheduler integration (`scheduler_wasm`) or
//! an ABI extension — work that belongs in a dedicated follow-up.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

// ── Core queue ──────────────────────────────────────────────────────────

/// Shared inner state for a single-threaded bounded channel.
#[derive(Debug)]
struct ChannelInner {
    queue: VecDeque<Vec<u8>>,
    capacity: usize,
    /// Number of live sender handles. When this reaches 0 the channel is
    /// "closed" from the producer side.
    sender_count: usize,
    /// Set when the receiver handle is dropped.
    receiver_closed: bool,
}

/// Sender handle. Multiple senders may share the same inner queue via `Rc`.
#[derive(Debug)]
pub struct WasmChannelSender {
    inner: Rc<RefCell<ChannelInner>>,
}

/// Receiver handle. Exactly one receiver per channel.
#[derive(Debug)]
pub struct WasmChannelReceiver {
    inner: Rc<RefCell<ChannelInner>>,
}

/// Result of a `try_send` operation.
#[derive(Debug, PartialEq, Eq)]
pub enum TrySendError {
    /// The channel is at capacity; message was not enqueued.
    Full,
    /// The receiver has been closed; message was not enqueued.
    Closed,
}

/// Result of a `try_recv` operation.
#[derive(Debug, PartialEq, Eq)]
pub enum TryRecvError {
    /// The queue is empty but senders are still alive.
    Empty,
    /// The queue is empty and all senders have been dropped (closed).
    Closed,
}

// ── Constructor ─────────────────────────────────────────────────────────

/// Create a bounded single-threaded channel with the given capacity.
///
/// Returns `(sender, receiver)`. The capacity must be ≥ 1.
pub fn channel(capacity: usize) -> (WasmChannelSender, WasmChannelReceiver) {
    let cap = capacity.max(1);
    let inner = Rc::new(RefCell::new(ChannelInner {
        queue: VecDeque::with_capacity(cap),
        capacity: cap,
        sender_count: 1,
        receiver_closed: false,
    }));

    let sender = WasmChannelSender {
        inner: Rc::clone(&inner),
    };
    let receiver = WasmChannelReceiver { inner };

    (sender, receiver)
}

// ── Send ────────────────────────────────────────────────────────────────

impl WasmChannelSender {
    /// Try to send `data` through the channel.
    ///
    /// Returns `Ok(())` if the message was enqueued, or an error if the
    /// channel is full or the receiver is closed.
    pub fn try_send(&self, data: Vec<u8>) -> Result<(), TrySendError> {
        let mut inner = self.inner.borrow_mut();
        if inner.receiver_closed {
            return Err(TrySendError::Closed);
        }
        if inner.queue.len() >= inner.capacity {
            return Err(TrySendError::Full);
        }
        inner.queue.push_back(data);
        Ok(())
    }

    /// Returns `true` if the receiver has been dropped.
    ///
    /// A closed channel will reject all future sends with
    /// [`TrySendError::Closed`].
    #[allow(dead_code, reason = "groundwork API — will be used when wired to ABI")]
    pub fn is_closed(&self) -> bool {
        self.inner.borrow().receiver_closed
    }
}

impl Drop for WasmChannelSender {
    fn drop(&mut self) {
        let mut inner = self.inner.borrow_mut();
        inner.sender_count = inner.sender_count.saturating_sub(1);
    }
}

impl Clone for WasmChannelSender {
    fn clone(&self) -> Self {
        self.inner.borrow_mut().sender_count += 1;
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

// ── Receive ─────────────────────────────────────────────────────────────

impl WasmChannelReceiver {
    /// Try to receive a message without blocking.
    ///
    /// Returns the next queued message, or an error distinguishing
    /// "empty with live senders" from "closed."
    pub fn try_recv(&self) -> Result<Vec<u8>, TryRecvError> {
        let mut inner = self.inner.borrow_mut();
        if let Some(item) = inner.queue.pop_front() {
            return Ok(item);
        }
        if inner.sender_count == 0 {
            Err(TryRecvError::Closed)
        } else {
            Err(TryRecvError::Empty)
        }
    }

    /// Returns `true` if the channel is closed (all senders dropped and
    /// queue drained).
    #[allow(dead_code, reason = "groundwork API — will be used when wired to ABI")]
    pub fn is_closed(&self) -> bool {
        let inner = self.inner.borrow();
        inner.sender_count == 0 && inner.queue.is_empty()
    }
}

impl Drop for WasmChannelReceiver {
    fn drop(&mut self) {
        self.inner.borrow_mut().receiver_closed = true;
    }
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_and_drop() {
        let (tx, rx) = channel(4);
        drop(tx);
        drop(rx);
    }

    #[test]
    fn send_recv_basic() {
        let (tx, rx) = channel(4);
        assert!(tx.try_send(b"hello".to_vec()).is_ok());
        let msg = rx.try_recv().unwrap();
        assert_eq!(msg, b"hello");
        drop(tx);
        drop(rx);
    }

    #[test]
    fn try_recv_empty_with_live_sender() {
        let (tx, rx) = channel(4);
        // Empty but sender alive → Empty, not Closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        drop(tx);
        drop(rx);
    }

    #[test]
    fn try_recv_closed_after_sender_dropped() {
        let (tx, rx) = channel(4);
        drop(tx);
        // Sender dropped, queue empty → Closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Closed));
        drop(rx);
    }

    #[test]
    fn drain_then_closed() {
        let (tx, rx) = channel(4);
        assert!(tx.try_send(b"msg".to_vec()).is_ok());
        drop(tx);
        // Message available even though sender is dropped.
        let msg = rx.try_recv().unwrap();
        assert_eq!(msg, b"msg");
        // Now drained → Closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Closed));
        drop(rx);
    }

    #[test]
    fn bounded_capacity_returns_full() {
        let (tx, rx) = channel(2);
        assert!(tx.try_send(b"a".to_vec()).is_ok());
        assert!(tx.try_send(b"b".to_vec()).is_ok());
        // At capacity → Full.
        assert_eq!(tx.try_send(b"c".to_vec()), Err(TrySendError::Full));
        // Drain one, then send succeeds.
        assert_eq!(rx.try_recv().unwrap(), b"a");
        assert!(tx.try_send(b"c".to_vec()).is_ok());
        drop(tx);
        drop(rx);
    }

    #[test]
    fn send_after_receiver_closed() {
        let (tx, rx) = channel(4);
        drop(rx);
        assert_eq!(tx.try_send(b"dropped".to_vec()), Err(TrySendError::Closed));
        drop(tx);
    }

    #[test]
    fn sender_clone_shares_queue() {
        let (tx, rx) = channel(8);
        let tx2 = tx.clone();
        assert!(tx.try_send(b"from-1".to_vec()).is_ok());
        assert!(tx2.try_send(b"from-2".to_vec()).is_ok());
        assert_eq!(rx.try_recv().unwrap(), b"from-1");
        assert_eq!(rx.try_recv().unwrap(), b"from-2");
        drop(tx);
        drop(tx2);
        drop(rx);
    }

    #[test]
    fn clone_drop_does_not_close_channel() {
        let (tx, rx) = channel(4);
        let tx2 = tx.clone();
        drop(tx);
        // tx2 still alive → not closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        assert!(tx2.try_send(b"alive".to_vec()).is_ok());
        assert_eq!(rx.try_recv().unwrap(), b"alive");
        drop(tx2);
        // Now all senders dropped → Closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Closed));
        drop(rx);
    }

    #[test]
    fn fifo_ordering() {
        let (tx, rx) = channel(8);
        for i in 0u8..5 {
            assert!(tx.try_send(vec![i]).is_ok());
        }
        for i in 0u8..5 {
            assert_eq!(
                rx.try_recv().unwrap(),
                vec![i],
                "FIFO order violated at {i}"
            );
        }
        drop(tx);
        drop(rx);
    }

    #[test]
    fn int_round_trip() {
        let (tx, rx) = channel(4);
        let value: i64 = 42;
        assert!(tx.try_send(value.to_le_bytes().to_vec()).is_ok());
        let bytes = rx.try_recv().unwrap();
        let received = i64::from_le_bytes(bytes.try_into().unwrap());
        assert_eq!(received, 42);
        drop(tx);
        drop(rx);
    }

    #[test]
    fn zero_int_round_trip_not_confused_with_empty() {
        let (tx, rx) = channel(4);
        let value: i64 = 0;
        assert!(tx.try_send(value.to_le_bytes().to_vec()).is_ok());
        let bytes = rx.try_recv().unwrap();
        let received = i64::from_le_bytes(bytes.try_into().unwrap());
        assert_eq!(received, 0);
        // Now empty (but sender alive) → Empty, not Closed.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        drop(tx);
        drop(rx);
    }

    #[test]
    fn empty_string_round_trip() {
        let (tx, rx) = channel(4);
        assert!(tx.try_send(Vec::new()).is_ok());
        let msg = rx.try_recv().unwrap();
        assert!(msg.is_empty());
        // Queue drained, sender alive → Empty.
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        drop(tx);
        drop(rx);
    }

    // ── is_closed lifecycle ─────────────────────────────────────────────

    #[test]
    fn sender_is_closed_false_with_live_receiver() {
        let (tx, rx) = channel(4);
        // Receiver alive → not closed, regardless of sender count.
        assert!(!tx.is_closed());
        let tx2 = tx.clone();
        assert!(!tx.is_closed());
        assert!(!tx2.is_closed());
        drop(tx2);
        // Still not closed — receiver is alive.
        assert!(!tx.is_closed());
        drop(tx);
        drop(rx);
    }

    #[test]
    fn sender_is_closed_true_when_receiver_dropped() {
        let (tx, rx) = channel(4);
        assert!(!tx.is_closed());
        drop(rx);
        // Receiver dropped → closed.
        assert!(tx.is_closed());
        let tx2 = tx.clone();
        // All clones see the same closed state.
        assert!(tx2.is_closed());
        drop(tx);
        drop(tx2);
    }

    #[test]
    fn receiver_is_closed_lifecycle() {
        let (tx, rx) = channel(4);
        // Sender alive, queue empty → not closed.
        assert!(!rx.is_closed());
        assert!(tx.try_send(b"msg".to_vec()).is_ok());
        // Sender alive, queue non-empty → not closed.
        assert!(!rx.is_closed());
        drop(tx);
        // All senders dropped, but queue non-empty → not closed (drainable).
        assert!(!rx.is_closed());
        let _ = rx.try_recv().unwrap();
        // All senders dropped and queue drained → closed.
        assert!(rx.is_closed());
        drop(rx);
    }

    // ── Zero-capacity normalization ─────────────────────────────────────

    #[test]
    fn zero_capacity_normalized_to_one() {
        let (tx, rx) = channel(0);
        // Should behave as capacity-1: one message fits, second is Full.
        assert!(tx.try_send(b"first".to_vec()).is_ok());
        assert_eq!(tx.try_send(b"second".to_vec()), Err(TrySendError::Full));
        // Drain and retry.
        assert_eq!(rx.try_recv().unwrap(), b"first");
        assert!(tx.try_send(b"second".to_vec()).is_ok());
        assert_eq!(rx.try_recv().unwrap(), b"second");
        drop(tx);
        drop(rx);
    }
}
