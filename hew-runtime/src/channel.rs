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

use crate::channel_common::{bytes_to_cstr, free_channel_pair};

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

fn decode_i64_payload(payload: &[u8], context: &str) -> Result<i64, ()> {
    let bytes: [u8; 8] = payload.try_into().map_err(|_| {
        crate::set_last_error(format!(
            "{context}: expected 8-byte int payload, got {} bytes",
            payload.len()
        ));
    })?;
    Ok(i64::from_le_bytes(bytes))
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
/// Returns null on invalid capacity (sets last error).
///
/// # Safety
///
/// The returned pointer must be freed with [`hew_channel_pair_free`].
#[no_mangle]
pub extern "C" fn hew_channel_new(capacity: i64) -> *mut HewChannelPair {
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
    // SAFETY: caller guarantees `pair` came from `hew_channel_new`.
    unsafe { free_channel_pair(pair, |pair| (&mut pair.sender, &mut pair.receiver)) };
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

/// Send a 64-bit integer through the channel. Blocks if the channel is full.
///
/// The integer is serialised as 8 little-endian bytes for the internal
/// `Vec<u8>` transport.
///
/// # Safety
///
/// `sender` must be a valid pointer.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_send_int(sender: *mut HewChannelSender, value: i64) {
    cabi_guard!(sender.is_null());
    let bytes = value.to_le_bytes().to_vec();
    // SAFETY: sender is valid per caller contract.
    let _ = unsafe { (*sender).tx.send(bytes) };
}

// ── Receive ─────────────────────────────────────────────────────────────────

/// Block until a message is available and return it as a malloc-allocated
/// NUL-terminated string. Returns NULL when the channel is closed (all
/// senders dropped), letting codegen wrap the result as `Option<String>`.
///
/// The caller must `free()` the returned pointer when non-NULL.
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
    // Channel closed — return NULL so codegen wraps as None.
    ptr::null_mut()
}

/// Try to receive a message without blocking.
///
/// Returns a malloc-allocated NUL-terminated string if a message was
/// available, or NULL if the channel is empty or closed. This lets the
/// caller distinguish "received empty string" (`Some("")`) from
/// "nothing available" (`None`).
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

/// Block until an integer message is available.
///
/// Returns the integer value and sets `*out_valid` to 1 if a message was
/// received. Sets `*out_valid` to 0 and returns 0 when the channel is
/// closed (all senders dropped), letting codegen wrap as `Option<int>`.
///
/// # Safety
///
/// `receiver` and `out_valid` must be valid pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_channel_recv_int(
    receiver: *mut HewChannelReceiver,
    out_valid: *mut i32,
) -> i64 {
    cabi_guard!(receiver.is_null() || out_valid.is_null(), 0);
    // SAFETY: receiver and out_valid are valid per caller contract.
    if let Ok(item) = unsafe { (*receiver).rx.recv() } {
        if let Ok(value) = decode_i64_payload(&item, "hew_channel_recv_int") {
            // SAFETY: out_valid is valid per caller contract.
            unsafe { *out_valid = 1 };
            return value;
        }
        // SAFETY: out_valid is valid per caller contract.
        unsafe { *out_valid = 0 };
        return 0;
    }
    // SAFETY: out_valid is valid per caller contract.
    unsafe { *out_valid = 0 };
    0
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
#[no_mangle]
pub unsafe extern "C" fn hew_channel_try_recv_int(
    receiver: *mut HewChannelReceiver,
    out_valid: *mut i32,
) -> i64 {
    cabi_guard!(receiver.is_null() || out_valid.is_null(), 0);
    // SAFETY: receiver and out_valid are valid per caller contract.
    match unsafe { (*receiver).rx.try_recv() } {
        Ok(item) => {
            if let Ok(value) = decode_i64_payload(&item, "hew_channel_try_recv_int") {
                // SAFETY: out_valid is valid per caller contract.
                unsafe { *out_valid = 1 };
                value
            } else {
                // SAFETY: out_valid is valid per caller contract.
                unsafe { *out_valid = 0 };
                0
            }
        }
        Err(TryRecvError::Empty | TryRecvError::Disconnected) => {
            // SAFETY: out_valid is valid per caller contract.
            unsafe { *out_valid = 0 };
            0
        }
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
    fn recv_returns_null_on_closed() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_sender_close(tx);
            let result = hew_channel_recv(rx);
            // Closed channel returns NULL so codegen can wrap as None.
            assert!(result.is_null());

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
            assert!(result.is_null(), "empty channel should return NULL");

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_distinguishes_empty_string_from_no_message() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Send an actual empty string.
            let empty = b"\0";
            hew_channel_send(tx, empty.as_ptr().cast());

            // try_recv should return a valid (non-NULL) pointer to a NUL byte.
            let result = hew_channel_try_recv(rx);
            assert!(!result.is_null(), "empty string message should be non-NULL");
            let received = CStr::from_ptr(result);
            assert_eq!(received.to_str().unwrap(), "");
            libc::free(result.cast());

            // Now the channel is empty — try_recv should return NULL.
            let result2 = hew_channel_try_recv(rx);
            assert!(result2.is_null(), "drained channel should return NULL");

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_distinguishes_zero_from_no_message() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            // Send the value 0.
            hew_channel_send_int(tx, 0);

            let mut valid: i32 = -1;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1, "received 0 should set valid=1");
            assert_eq!(val, 0);

            // Now the channel is empty.
            let val2 = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "drained channel should set valid=0");
            assert_eq!(val2, 0);

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
                messages.push(s);
            }

            messages.sort();
            assert_eq!(messages, vec!["from-1", "from-2"]);

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn send_recv_int_basic() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_send_int(tx, 42);
            hew_channel_send_int(tx, -7);

            let mut valid: i32 = 0;
            let v1 = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1);
            assert_eq!(v1, 42);
            let v2 = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1);
            assert_eq!(v2, -7);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_int_returns_invalid_on_closed() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_sender_close(tx);
            let mut valid: i32 = -1;
            let val = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "closed channel should set out_valid to 0");
            assert_eq!(val, 0);

            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_returns_invalid_when_empty() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let mut valid: i32 = -1;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0, "empty channel should set valid=0");
            assert_eq!(val, 0);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_returns_value_when_available() {
        let pair = hew_channel_new(4);
        // SAFETY: pair was just created above.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            hew_channel_send_int(tx, 99);
            let mut valid: i32 = 0;
            let val = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 1, "message available should set valid=1");
            assert_eq!(val, 99);

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn try_recv_int_rejects_non_i64_payloads() {
        crate::hew_clear_error();
        let pair = hew_channel_new(4);
        // SAFETY: test owns both channel handles and only sends stack-backed test payloads.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let payload = b"oops\0";
            hew_channel_send(tx, payload.as_ptr().cast());

            let mut valid = -1;
            let value = hew_channel_try_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0);
            assert_eq!(value, 0);
            let err = std::ffi::CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected 8-byte int payload"),
                "unexpected error: {err}"
            );

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn recv_int_rejects_non_i64_payloads() {
        crate::hew_clear_error();
        let pair = hew_channel_new(4);
        // SAFETY: test owns both channel handles and only sends stack-backed test payloads.
        unsafe {
            let tx = hew_channel_pair_sender(pair);
            let rx = hew_channel_pair_receiver(pair);
            hew_channel_pair_free(pair);

            let payload = b"bad";
            hew_channel_send(tx, payload.as_ptr().cast());

            let mut valid = -1;
            let value = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
            assert_eq!(valid, 0);
            assert_eq!(value, 0);
            let err = std::ffi::CStr::from_ptr(crate::hew_last_error())
                .to_str()
                .unwrap()
                .to_string();
            assert!(
                err.contains("expected 8-byte int payload"),
                "unexpected error: {err}"
            );

            hew_channel_sender_close(tx);
            hew_channel_receiver_close(rx);
        }
    }

    #[test]
    fn int_multi_producer() {
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
                hew_channel_send_int(tx, 100);
                hew_channel_sender_close(tx);
            });

            let t2 = thread::spawn(move || {
                let tx2 = tx2_addr as *mut HewChannelSender;
                hew_channel_send_int(tx2, 200);
                hew_channel_sender_close(tx2);
            });

            t1.join().unwrap();
            t2.join().unwrap();

            let mut values = Vec::new();
            let mut valid: i32 = 0;
            loop {
                let v = hew_channel_recv_int(rx, std::ptr::addr_of_mut!(valid));
                if valid == 0 {
                    break;
                }
                values.push(v);
            }

            values.sort_unstable();
            assert_eq!(values, vec![100, 200]);

            hew_channel_receiver_close(rx);
        }
    }
}
