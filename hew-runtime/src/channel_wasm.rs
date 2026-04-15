//! WASM channel ABI: bounded single-threaded queue using [`VecDeque`].
//!
//! This is the wasm32 counterpart of [`crate::channel`]. It implements the
//! non-blocking, bounded slice that the cooperative scheduler can support today:
//!
//! - `channel.new`, sender clone/close, receiver close
//! - `send` / `send_int` via the non-blocking queue path
//! - `try_recv` / `try_recv_int`
//!
//! Blocking `recv` / `recv_int` remain deferred because they still require the
//! cooperative scheduler to yield and resume when the queue is empty but live
//! senders remain.

use std::cell::RefCell;
use std::collections::VecDeque;
use std::ffi::{c_char, CStr};
use std::ptr;
use std::rc::Rc;

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
    let sender = Box::into_raw(Box::new(HewWasmChannelSender::new(sender)));
    let receiver = Box::into_raw(Box::new(HewWasmChannelReceiver::new(receiver)));

    Box::into_raw(Box::new(HewWasmChannelPair { sender, receiver }))
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
    if pair.is_null() {
        return;
    }
    // SAFETY: caller guarantees `pair` came from `hew_channel_new`.
    let pair = unsafe { Box::from_raw(pair) };
    if !pair.sender.is_null() {
        // SAFETY: unextracted sender handles are still Box-owned here.
        unsafe { drop(Box::from_raw(pair.sender)) };
    }
    if !pair.receiver.is_null() {
        // SAFETY: unextracted receiver handles are still Box-owned here.
        unsafe { drop(Box::from_raw(pair.receiver)) };
    }
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
        Ok(()) | Err(TrySendError::Closed) => {}
        Err(TrySendError::Full) => panic!(
            "{api_name}: wasm32 channels trap on full queues; blocking send \
             yield/resume parity is not implemented yet"
        ),
    }
}

/// Send a NUL-terminated string through the channel.
///
/// # Safety
///
/// `sender` must be a valid pointer. `data` must be a valid NUL-terminated
/// C string.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_send(sender: *mut HewWasmChannelSender, data: *const c_char) {
    cabi_guard!(sender.is_null() || data.is_null());
    // SAFETY: caller passes a valid NUL-terminated C string.
    let data = unsafe { CStr::from_ptr(data) };
    // SAFETY: caller guarantees `sender` is a live ABI sender handle.
    send_bytes(
        unsafe { &*sender },
        data.to_bytes().to_vec(),
        "hew_channel_send",
    );
}

/// Send a 64-bit integer through the channel.
///
/// # Safety
///
/// `sender` must be a valid pointer.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_send_int(sender: *mut HewWasmChannelSender, value: i64) {
    cabi_guard!(sender.is_null());
    // SAFETY: caller guarantees `sender` is a live ABI sender handle.
    send_bytes(
        unsafe { &*sender },
        value.to_le_bytes().to_vec(),
        "hew_channel_send_int",
    );
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

fn bytes_to_cstr(item: &[u8]) -> *mut c_char {
    let len = item.len();
    // SAFETY: malloc returns either a null pointer or a valid allocation.
    let buf = unsafe { libc::malloc(len + 1) };
    if buf.is_null() {
        return ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: `buf` has `len + 1` bytes and `item` has `len` readable bytes.
        unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
    }
    // SAFETY: the final byte of the allocation is reserved for the terminator.
    unsafe { *buf.cast::<u8>().add(len) = 0 };
    buf.cast::<c_char>()
}

/// Try to receive a message without blocking.
///
/// Returns a malloc-allocated NUL-terminated string if a message was
/// available, or NULL if the channel is empty or closed.
///
/// # Safety
///
/// `receiver` must be a valid pointer.
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_try_recv(
    receiver: *mut HewWasmChannelReceiver,
) -> *mut c_char {
    cabi_guard!(receiver.is_null(), ptr::null_mut());
    // SAFETY: caller guarantees `receiver` is a live ABI receiver handle.
    match unsafe { (*receiver).inner.try_recv() } {
        Ok(item) => bytes_to_cstr(&item),
        Err(TryRecvError::Empty | TryRecvError::Closed) => ptr::null_mut(),
    }
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
#[cfg_attr(target_arch = "wasm32", no_mangle)]
pub unsafe extern "C" fn hew_channel_try_recv_int(
    receiver: *mut HewWasmChannelReceiver,
    out_valid: *mut i32,
) -> i64 {
    cabi_guard!(receiver.is_null() || out_valid.is_null(), 0);
    // SAFETY: caller guarantees `receiver` is a live ABI receiver handle.
    match unsafe { (*receiver).inner.try_recv() } {
        Ok(item) => {
            // SAFETY: caller guarantees `out_valid` points to writable memory.
            unsafe { *out_valid = 1 };
            i64::from_le_bytes(item.try_into().unwrap_or([0; 8]))
        }
        Err(TryRecvError::Empty | TryRecvError::Closed) => {
            // SAFETY: caller guarantees `out_valid` points to writable memory.
            unsafe { *out_valid = 0 };
            0
        }
    }
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
    Box::into_raw(Box::new(HewWasmChannelSender::new(cloned)))
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
    unsafe { drop(Box::from_raw(sender)) };
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
    unsafe { drop(Box::from_raw(receiver)) };
}

#[cfg(test)]
fn active_handle_count() -> usize {
    ACTIVE_HANDLES.load(Ordering::Relaxed)
}

// ── Tests ───────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

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

            let first = CString::new("first").unwrap();
            hew_channel_send(tx, first.as_ptr());
            hew_channel_send_int(tx2, 7);

            let msg = hew_channel_try_recv(rx);
            assert!(!msg.is_null());
            assert_eq!(CStr::from_ptr(msg).to_str().unwrap(), "first");
            libc::free(msg.cast());

            let mut valid = -1;
            let value = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1);
            assert_eq!(value, 7);

            let empty = hew_channel_try_recv(rx);
            assert!(empty.is_null(), "empty channel should return NULL");

            hew_channel_sender_close(tx);
            let still_open = hew_channel_try_recv(rx);
            assert!(
                still_open.is_null(),
                "live clone keeps channel empty-not-closed ABI as NULL"
            );
            hew_channel_sender_close(tx2);
            let closed = hew_channel_try_recv(rx);
            assert!(closed.is_null(), "closed channel should also return NULL");

            valid = -1;
            let no_value = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0);
            assert_eq!(no_value, 0);

            hew_channel_receiver_close(rx);
        }
        assert_eq!(active_handle_count(), 0);
    }

    // On wasm32-wasip1 panics abort the test binary, so the explicit panic
    // wrapper must be asserted on host targets where unwinding is available.
    // The fail-closed queue-full path itself is still covered everywhere by
    // `bounded_capacity_returns_full`, which exercises the underlying
    // fallible send helper.
    #[test]
    #[cfg(not(target_arch = "wasm32"))]
    fn abi_send_traps_explicitly_when_queue_is_full() {
        use std::panic::{catch_unwind, AssertUnwindSafe};

        let _guard = crate::runtime_test_guard();
        let pair = hew_channel_new(1);
        // SAFETY: extracted handles are used only within this test and are
        // closed exactly once before returning.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let first = CString::new("first").unwrap();
            hew_channel_send(tx, first.as_ptr());

            let sender = &*tx;
            let panic = catch_unwind(AssertUnwindSafe(|| {
                send_bytes(sender, b"second".to_vec(), "hew_channel_send");
            }))
            .expect_err("full wasm channel should panic before silent drop");
            let message = if let Some(message) = panic.downcast_ref::<String>() {
                message.clone()
            } else if let Some(message) = panic.downcast_ref::<&str>() {
                (*message).to_string()
            } else {
                String::new()
            };
            assert!(message.contains("hew_channel_send"));
            assert!(message.contains("full queues"));

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }
}
