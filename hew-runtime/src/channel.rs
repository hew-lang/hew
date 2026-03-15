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
//! caller (must be freed with `free()`).

use std::ffi::{c_char, CStr};
use std::ptr;
use std::sync::mpsc::{self, Receiver, SyncSender, TryRecvError};

// ── Handle types ────────────────────────────────────────────────────────────

/// Opaque sender handle. Cloneable for multi-producer use.
#[derive(Debug)]
pub struct HewChannelSender {
    tx: SyncSender<Vec<u8>>,
}

/// Opaque receiver handle. Only one receiver per channel.
#[derive(Debug)]
pub struct HewChannelReceiver {
    rx: Receiver<Vec<u8>>,
}

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
/// # Safety
///
/// The returned pointer must be freed with [`hew_channel_pair_free`].
#[no_mangle]
pub extern "C" fn hew_channel_new(capacity: i64) -> *mut HewChannelPair {
    let cap = usize::try_from(capacity.max(1)).unwrap_or(1);
    let (tx, rx) = mpsc::sync_channel::<Vec<u8>>(cap);

    let sender = Box::into_raw(Box::new(HewChannelSender { tx }));
    let receiver = Box::into_raw(Box::new(HewChannelReceiver { rx }));

    Box::into_raw(Box::new(HewChannelPair { sender, receiver }))
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
    if pair.is_null() {
        return;
    }
    // SAFETY: caller guarantees pair was Box-allocated.
    let p = unsafe { Box::from_raw(pair) };
    if !p.sender.is_null() {
        // SAFETY: sender was Box-allocated in hew_channel_new.
        unsafe { drop(Box::from_raw(p.sender)) };
    }
    if !p.receiver.is_null() {
        // SAFETY: receiver was Box-allocated in hew_channel_new.
        unsafe { drop(Box::from_raw(p.receiver)) };
    }
}

// ── Send ────────────────────────────────────────────────────────────────────

/// Send a NUL-terminated string through the channel. Blocks if the channel
/// is full (backpressure).
///
/// # Safety
///
/// `sender` must be a valid pointer. `data` must be a valid NUL-terminated
/// C string.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_send(sender: *mut HewChannelSender, data: *const c_char) {
    cabi_guard!(sender.is_null() || data.is_null());
    // SAFETY: data is a valid C string per caller contract.
    let s = unsafe { CStr::from_ptr(data) };
    let bytes = s.to_bytes().to_vec();
    // SAFETY: sender is valid per caller contract.
    let _ = unsafe { (*sender).tx.send(bytes) };
}

// ── Receive ─────────────────────────────────────────────────────────────────

/// Allocate a NUL-terminated C string from a byte slice.
fn bytes_to_cstr(item: &[u8]) -> *mut c_char {
    let len = item.len();
    // SAFETY: libc::malloc returns a valid aligned pointer or null.
    let buf = unsafe { libc::malloc(len + 1) };
    if buf.is_null() {
        return ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: buf has len+1 bytes; item.as_ptr() has len bytes.
        unsafe { ptr::copy_nonoverlapping(item.as_ptr(), buf.cast::<u8>(), len) };
    }
    // SAFETY: writing NUL terminator at offset len within allocated len+1 bytes.
    unsafe { *buf.cast::<u8>().add(len) = 0 };
    buf.cast::<c_char>()
}

/// Block until a message is available and return it as a malloc-allocated
/// NUL-terminated string. Returns an empty string (valid pointer to NUL
/// byte) when the channel is closed (all senders dropped).
///
/// The caller must `free()` the returned pointer.
///
/// # Safety
///
/// `receiver` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv(receiver: *mut HewChannelReceiver) -> *mut c_char {
    cabi_guard!(receiver.is_null(), ptr::null_mut());
    // SAFETY: receiver is valid per caller contract.
    if let Ok(item) = unsafe { (*receiver).rx.recv() } {
        return bytes_to_cstr(&item);
    }
    // Channel closed — return a valid empty string so Hew string
    // comparison (`msg == ""`) works correctly.
    // SAFETY: libc::malloc returns a valid pointer or null.
    let buf = unsafe { libc::malloc(1) };
    if buf.is_null() {
        return ptr::null_mut();
    }
    // SAFETY: buf has 1 byte allocated; writing NUL terminator.
    unsafe { *buf.cast::<u8>() = 0 };
    buf.cast::<c_char>()
}

/// Try to receive a message without blocking.
///
/// Returns a malloc-allocated NUL-terminated string if a message was
/// available, or NULL if the channel is empty or closed.
///
/// The caller must `free()` the returned pointer when non-NULL.
///
/// # Safety
///
/// `receiver` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_try_recv(receiver: *mut HewChannelReceiver) -> *mut c_char {
    cabi_guard!(receiver.is_null(), ptr::null_mut());
    // SAFETY: receiver is valid per caller contract.
    match unsafe { (*receiver).rx.try_recv() } {
        Ok(item) => bytes_to_cstr(&item),
        Err(TryRecvError::Empty | TryRecvError::Disconnected) => ptr::null_mut(),
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
    let cloned_tx = unsafe { (*sender).tx.clone() };
    Box::into_raw(Box::new(HewChannelSender { tx: cloned_tx }))
}

// ── Close / Free ────────────────────────────────────────────────────────────

/// Close and free a sender handle.
///
/// When the last sender is closed, the receiver will observe channel closure
/// (recv returns an empty string).
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
    // SAFETY: caller guarantees sender was Box-allocated and is exclusively owned.
    unsafe { drop(Box::from_raw(sender)) };
}

/// Close and free a receiver handle.
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
    unsafe { drop(Box::from_raw(receiver)) };
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

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
    fn send_recv_basic() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let msg = b"hello\0";
            hew_channel_send(tx, msg.as_ptr().cast());

            let result = hew_channel_recv(rx);
            assert!(!result.is_null());
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_str().unwrap(), "hello");
            libc::free(result.cast());

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_returns_empty_on_closed() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_sender_close(tx);
            let result = hew_channel_recv(rx);
            // Closed channel returns a valid empty string, not NULL.
            assert!(!result.is_null());
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_str().unwrap(), "");
            libc::free(result.cast());

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_returns_null_when_empty() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let result = hew_channel_try_recv(rx);
            assert!(result.is_null());

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
                let msg = b"from-1\0";
                // SAFETY: tx is valid for the lifetime of this test.
                hew_channel_send(tx, msg.as_ptr().cast());
                hew_channel_sender_close(tx);
            });

            let t2 = thread::spawn(move || {
                let tx2 = tx2_addr as *mut HewChannelSender;
                let msg = b"from-2\0";
                // SAFETY: tx2 is valid for the lifetime of this test.
                hew_channel_send(tx2, msg.as_ptr().cast());
                hew_channel_sender_close(tx2);
            });

            t1.join().unwrap();
            t2.join().unwrap();

            let mut messages = Vec::new();
            loop {
                let result = hew_channel_recv(rx);
                if result.is_null() {
                    break;
                }
                let s = CStr::from_ptr(result).to_str().unwrap().to_owned();
                libc::free(result.cast());
                if s.is_empty() {
                    break;
                }
                messages.push(s);
            }

            messages.sort();
            assert_eq!(messages, vec!["from-1", "from-2"]);

            hew_channel_receiver_close(rx);
        }
    }
}
