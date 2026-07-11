//! WASM channel ABI: bounded single-threaded queue using [`VecDeque`].
//!
//! This is the wasm32 counterpart of [`crate::channel`]. It implements the
//! non-blocking, bounded slice that the cooperative scheduler can support today:
//!
//! - `channel.new`, sender clone/close, receiver close
//! - `send` via the layout-witness entry (`hew_channel_send_layout`) on the
//!   non-blocking queue path
//! - `try_recv` via the layout-witness entry (`hew_channel_try_recv_layout`)
//!
//! Blocking `recv` (`hew_channel_recv_layout`) remains deferred because it
//! still requires the cooperative scheduler to yield and resume when the
//! queue is empty but live senders remain.
#![allow(
    unsafe_op_in_unsafe_fn,
    reason = "FFI entry-point module; SAFETY documented at fn signature."
)]

use std::cell::RefCell;
use std::collections::VecDeque;
use std::ffi::c_void;
use std::ptr;
use std::rc::Rc;

use hew_cabi::vec::HewVecElemLayout;

use crate::channel_common::{
    decode_elem_envelope, elem_layout_witness, encode_elem_envelope, free_channel_pair,
};

#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};

#[cfg(test)]
static ACTIVE_HANDLES: AtomicUsize = AtomicUsize::new(0);

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

#[derive(Debug)]
#[repr(C)]
pub(crate) struct HewWasmChannelSender {
    inner: WasmChannelSender,
}

#[derive(Debug)]
#[repr(C)]
pub(crate) struct HewWasmChannelReceiver {
    inner: WasmChannelReceiver,
}

/// Temporary pair returned by [`hew_channel_new`].
#[derive(Debug)]
#[repr(C)]
pub struct HewWasmChannelPair {
    sender: *mut HewWasmChannelSender,
    receiver: *mut HewWasmChannelReceiver,
}

impl HewWasmChannelSender {
    fn new(inner: WasmChannelSender) -> Self {
        #[cfg(test)]
        ACTIVE_HANDLES.fetch_add(1, Ordering::Relaxed);
        Self { inner }
    }
}

impl HewWasmChannelReceiver {
    fn new(inner: WasmChannelReceiver) -> Self {
        #[cfg(test)]
        ACTIVE_HANDLES.fetch_add(1, Ordering::Relaxed);
        Self { inner }
    }
}

impl Drop for HewWasmChannelSender {
    fn drop(&mut self) {
        #[cfg(test)]
        ACTIVE_HANDLES.fetch_sub(1, Ordering::Relaxed);
    }
}

impl Drop for HewWasmChannelReceiver {
    fn drop(&mut self) {
        #[cfg(test)]
        ACTIVE_HANDLES.fetch_sub(1, Ordering::Relaxed);
    }
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

/// Create a bounded WASM channel and return the temporary pair handle.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub extern "C" fn hew_channel_new(capacity: i64) -> *mut HewWasmChannelPair {
    if capacity < 0 {
        crate::set_last_error(format!(
            "hew_channel_new: invalid capacity {capacity} (must be >= 0)"
        ));
        return ptr::null_mut();
    }
    let Some(cap) = usize::try_from(capacity.max(1)).ok() else {
        crate::set_last_error(format!(
            "hew_channel_new: capacity {capacity} exceeds platform maximum"
        ));
        return ptr::null_mut();
    };

    let (sender, receiver) = channel(cap);
    let sender = Box::into_raw(Box::new(HewWasmChannelSender::new(sender))); // ALLOCATOR-PAIRING: GlobalAlloc
    let receiver = Box::into_raw(Box::new(HewWasmChannelReceiver::new(receiver))); // ALLOCATOR-PAIRING: GlobalAlloc

    Box::into_raw(Box::new(HewWasmChannelPair { sender, receiver })) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Extract the sender from a channel pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
/// The sender must not be extracted more than once.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_pair_sender(
    pair: *mut HewWasmChannelPair,
) -> *mut HewWasmChannelSender {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `pair` is a valid channel-pair allocation.
    let sender = unsafe { (*pair).sender };
    // SAFETY: caller guarantees `pair` is valid; extracted handles are nulled.
    unsafe { (*pair).sender = ptr::null_mut() };
    sender
}

/// Extract the receiver from a channel pair.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
/// The receiver must not be extracted more than once.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_pair_receiver(
    pair: *mut HewWasmChannelPair,
) -> *mut HewWasmChannelReceiver {
    cabi_guard!(pair.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `pair` is a valid channel-pair allocation.
    let receiver = unsafe { (*pair).receiver };
    // SAFETY: caller guarantees `pair` is valid; extracted handles are nulled.
    unsafe { (*pair).receiver = ptr::null_mut() };
    receiver
}

/// Free the channel pair struct. Any handles not yet extracted are dropped.
///
/// # Safety
///
/// `pair` must be a valid pointer returned by [`hew_channel_new`].
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_pair_free(pair: *mut HewWasmChannelPair) {
    // SAFETY: caller guarantees `pair` came from `hew_channel_new`.
    unsafe { free_channel_pair(pair, |pair| (&mut pair.sender, &mut pair.receiver)) };
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
    #[allow(dead_code, reason = "queried by wasm channel lifecycle tests")]
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

fn send_bytes(sender: &HewWasmChannelSender, bytes: Vec<u8>, api_name: &str) {
    match sender.inner.try_send(bytes) {
        Ok(()) => {}
        Err(TrySendError::Full) => {
            panic!(
                "{api_name}: channel is full; blocking send is not available on wasm32 \
                 (cooperative scheduler does not yet support yield/resume for send parks)"
            );
        }
        Err(TrySendError::Closed) => {
            panic!("{api_name}: channel receiver is closed; message was not sent");
        }
    }
}

/// Send one element of any witness-describable type through the channel
/// (the wasm32 counterpart of the native `hew_channel_send_layout`). The
/// element is deep-copied into the envelope; the caller keeps its value.
/// Non-blocking: a full queue or closed receiver traps because this void ABI
/// cannot report a failed send. Blocking send needs cooperative yield/resume
/// parity.
///
/// # Safety
///
/// `sender` must be a valid pointer. `data` must point to one live element of
/// the witness's type. `layout` must point to a valid `HewVecElemLayout` for
/// the duration of the call (in practice a codegen static).
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_send_layout(
    sender: *mut HewWasmChannelSender,
    data: *const c_void,
    layout: *const HewVecElemLayout,
) {
    cabi_guard!(sender.is_null() || data.is_null());
    // SAFETY: caller guarantees the witness pointee lives for the call.
    let layout = unsafe { elem_layout_witness(layout, "hew_channel_send_layout") };
    // SAFETY: caller guarantees `data` points to one live element.
    let envelope = unsafe { encode_elem_envelope(data, layout, "hew_channel_send_layout") };
    // SAFETY: caller guarantees `sender` is a live ABI sender handle.
    send_bytes(unsafe { &*sender }, envelope, "hew_channel_send_layout");
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
    #[allow(dead_code, reason = "queried by wasm channel lifecycle tests")]
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

/// Try to receive one element without blocking (the wasm32 counterpart of
/// the native `hew_channel_try_recv_layout`). Decodes the envelope into the
/// consumer's out slot; ownership of the decoded element moves to the
/// consumer. Returns 1 when a value was bound, 0 when the queue is empty or
/// closed.
///
/// # Safety
///
/// `receiver` must be a valid pointer. `out` must point to one writable
/// element slot of the witness's type. `layout` must point to a valid
/// `HewVecElemLayout` for the duration of the call.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_try_recv_layout(
    receiver: *mut HewWasmChannelReceiver,
    out: *mut c_void,
    layout: *const HewVecElemLayout,
) -> i32 {
    cabi_guard!(receiver.is_null() || out.is_null(), 0);
    // SAFETY: caller guarantees the witness pointee lives for the call.
    let layout = unsafe { elem_layout_witness(layout, "hew_channel_try_recv_layout") };
    // SAFETY: caller guarantees `receiver` is a live ABI receiver handle.
    let item = unsafe { (*receiver).inner.try_recv() }.ok();
    // SAFETY: caller guarantees `out` is one writable element slot.
    unsafe { decode_elem_envelope(item, out, layout, "hew_channel_try_recv_layout") }
}

// ── Clone / Close ───────────────────────────────────────────────────────

/// Clone a sender handle for multi-producer use.
///
/// # Safety
///
/// `sender` must be a valid pointer.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_sender_clone(
    sender: *mut HewWasmChannelSender,
) -> *mut HewWasmChannelSender {
    cabi_guard!(sender.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `sender` is a live ABI sender handle.
    let cloned = unsafe { (*sender).inner.clone() };
    Box::into_raw(Box::new(HewWasmChannelSender::new(cloned))) // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Close and free a sender handle.
///
/// # Safety
///
/// `sender` must have been returned by [`hew_channel_pair_sender`] or
/// [`hew_channel_sender_clone`] and must not be used after this call.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_sender_close(sender: *mut HewWasmChannelSender) {
    if sender.is_null() {
        return;
    }
    // SAFETY: caller guarantees exclusive ownership of the Box allocation.
    unsafe { drop(Box::from_raw(sender)) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

/// Close and free a receiver handle.
///
/// # Safety
///
/// `receiver` must have been returned by [`hew_channel_pair_receiver`]
/// and must not be used after this call.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_receiver_close(receiver: *mut HewWasmChannelReceiver) {
    if receiver.is_null() {
        return;
    }
    // SAFETY: caller guarantees exclusive ownership of the Box allocation.
    unsafe { drop(Box::from_raw(receiver)) }; // ALLOCATOR-PAIRING: GlobalAlloc
}

#[cfg(test)]
fn active_handle_count() -> usize {
    ACTIVE_HANDLES.load(Ordering::Relaxed)
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::{c_char, CStr};

    use hew_cabi::vec::HewTypeOwnershipKind;

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
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        drop(tx);
        drop(rx);
    }

    #[test]
    fn try_recv_closed_after_sender_dropped() {
        let (tx, rx) = channel(4);
        drop(tx);
        assert_eq!(rx.try_recv(), Err(TryRecvError::Closed));
        drop(rx);
    }

    #[test]
    fn drain_then_closed() {
        let (tx, rx) = channel(4);
        assert!(tx.try_send(b"msg".to_vec()).is_ok());
        drop(tx);
        let msg = rx.try_recv().unwrap();
        assert_eq!(msg, b"msg");
        assert_eq!(rx.try_recv(), Err(TryRecvError::Closed));
        drop(rx);
    }

    #[test]
    fn bounded_capacity_returns_full() {
        let (tx, rx) = channel(2);
        assert!(tx.try_send(b"a".to_vec()).is_ok());
        assert!(tx.try_send(b"b".to_vec()).is_ok());
        assert_eq!(tx.try_send(b"c".to_vec()), Err(TrySendError::Full));
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
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        assert!(tx2.try_send(b"alive".to_vec()).is_ok());
        assert_eq!(rx.try_recv().unwrap(), b"alive");
        drop(tx2);
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
        assert_eq!(rx.try_recv(), Err(TryRecvError::Empty));
        drop(tx);
        drop(rx);
    }

    #[test]
    fn sender_is_closed_false_with_live_receiver() {
        let (tx, rx) = channel(4);
        assert!(!tx.is_closed());
        let tx2 = tx.clone();
        assert!(!tx.is_closed());
        assert!(!tx2.is_closed());
        drop(tx2);
        assert!(!tx.is_closed());
        drop(tx);
        drop(rx);
    }

    #[test]
    fn sender_is_closed_true_when_receiver_dropped() {
        let (tx, rx) = channel(4);
        assert!(!tx.is_closed());
        drop(rx);
        assert!(tx.is_closed());
        let tx2 = tx.clone();
        assert!(tx2.is_closed());
        drop(tx);
        drop(tx2);
    }

    #[test]
    fn receiver_is_closed_lifecycle() {
        let (tx, rx) = channel(4);
        assert!(!rx.is_closed());
        assert!(tx.try_send(b"msg".to_vec()).is_ok());
        assert!(!rx.is_closed());
        drop(tx);
        assert!(!rx.is_closed());
        let _ = rx.try_recv().unwrap();
        assert!(rx.is_closed());
        drop(rx);
    }

    #[test]
    fn zero_capacity_normalized_to_one() {
        let (tx, rx) = channel(0);
        assert!(tx.try_send(b"first".to_vec()).is_ok());
        assert_eq!(tx.try_send(b"second".to_vec()), Err(TrySendError::Full));
        assert_eq!(rx.try_recv().unwrap(), b"first");
        assert!(tx.try_send(b"second".to_vec()).is_ok());
        assert_eq!(rx.try_recv().unwrap(), b"second");
        drop(tx);
        drop(rx);
    }

    #[test]
    fn abi_pair_free_drops_unextracted_handles() {
        let _guard = crate::runtime_test_guard();
        assert_eq!(active_handle_count(), 0);
        let pair = hew_channel_new(2);
        assert!(!pair.is_null());
        assert_eq!(active_handle_count(), 2);
        // SAFETY: `pair` came from `hew_channel_new` and is still owned here.
        unsafe {
            hew_channel_pair_free(pair);
        }
        assert_eq!(active_handle_count(), 0);
    }

    #[test]
    fn abi_try_recv_and_lifecycle_match_native_contract() {
        let _guard = crate::runtime_test_guard();
        assert_eq!(active_handle_count(), 0);
        let pair = hew_channel_new(2);
        assert!(!pair.is_null());
        // SAFETY: extracted handles are used only within this test and are
        // closed exactly once before returning.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            assert_eq!(active_handle_count(), 2);
            hew_channel_pair_free(pair);

            let tx2 = hew_channel_sender_clone(tx);
            assert_eq!(active_handle_count(), 3);

            let string_witness = string_layout();
            let plain_witness = plain_layout(8, 8);

            let first: *const c_char = c"first".as_ptr();
            hew_channel_send_layout(
                tx,
                std::ptr::addr_of!(first).cast(),
                &raw const string_witness,
            );
            let seven: i64 = 7;
            hew_channel_send_layout(
                tx2,
                std::ptr::addr_of!(seven).cast(),
                &raw const plain_witness,
            );

            let mut msg: *mut c_char = ptr::null_mut();
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(msg).cast(),
                &raw const string_witness,
            );
            assert_eq!(rc, 1);
            assert_eq!(CStr::from_ptr(msg).to_str().unwrap(), "first");
            crate::cabi::free_cstring(msg); // CSTRING-FREE: str-open (test frees layout recv string output; header-aware)

            let mut value: i64 = -1;
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(value).cast(),
                &raw const plain_witness,
            );
            assert_eq!(rc, 1);
            assert_eq!(value, 7);

            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(value).cast(),
                &raw const plain_witness,
            );
            assert_eq!(rc, 0, "empty channel should bind no value");

            hew_channel_sender_close(tx);
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(value).cast(),
                &raw const plain_witness,
            );
            assert_eq!(rc, 0, "live clone keeps channel empty-not-closed as rc 0");
            hew_channel_sender_close(tx2);
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(value).cast(),
                &raw const plain_witness,
            );
            assert_eq!(rc, 0, "closed channel should also bind no value");

            hew_channel_receiver_close(rx);
        }
        assert_eq!(active_handle_count(), 0);
    }

    /// A Plain envelope of the wrong width is a malformed message: recv binds
    /// no value and records a diagnostic (mirrors the native layout path).
    #[test]
    fn abi_try_recv_layout_width_mismatch_binds_none_with_error() {
        crate::hew_clear_error();
        let pair = hew_channel_new(2);
        // SAFETY: test owns both channel handles and only sends stack-backed
        // test payloads.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Enqueue a 4-byte text envelope, then decode with an 8-byte
            // Plain witness — the width mismatch must bind no value.
            let string_witness = string_layout();
            let tiny: *const c_char = c"tiny".as_ptr();
            hew_channel_send_layout(
                tx,
                std::ptr::addr_of!(tiny).cast(),
                &raw const string_witness,
            );

            let plain_witness = plain_layout(8, 8);
            let mut value: i64 = -1;
            let rc = hew_channel_try_recv_layout(
                rx,
                std::ptr::addr_of_mut!(value).cast(),
                &raw const plain_witness,
            );
            assert_eq!(rc, 0);
            let err = CStr::from_ptr(crate::hew_last_error()).to_str().unwrap();
            assert!(err.contains("expected 8-byte element payload"));

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    #[should_panic(expected = "channel is full; blocking send is not available on wasm32")]
    fn send_bytes_full_traps() {
        crate::hew_clear_error();
        let _guard = crate::runtime_test_guard();
        let (tx, _rx) = channel(1);
        let tx = HewWasmChannelSender::new(tx);

        // Fill the channel to capacity.
        send_bytes(&tx, vec![1], "hew_channel_send_layout");

        // Send to a full channel — must trap rather than silently dropping.
        send_bytes(&tx, vec![2], "hew_channel_send_layout");
    }

    #[test]
    #[should_panic(expected = "channel receiver is closed; message was not sent")]
    fn send_bytes_closed_traps() {
        crate::hew_clear_error();
        let _guard = crate::runtime_test_guard();
        let (tx, rx) = channel(1);
        let tx = HewWasmChannelSender::new(tx);
        drop(rx);

        // A closed receiver must trap rather than silently dropping the envelope.
        send_bytes(&tx, vec![1], "hew_channel_send_layout");
    }
}
