//! Hew runtime: `websocket` module.
//!
//! Provides synchronous WebSocket client functionality for compiled Hew programs.
//! All returned data pointers are allocated with `libc::malloc` so callers can
//! free them with the corresponding free function.

use std::ffi::CStr;
use std::net::TcpStream;
use std::os::raw::c_char;

use tungstenite::stream::MaybeTlsStream;
use tungstenite::{Message, WebSocket};

/// Opaque WebSocket connection handle.
///
/// Wraps a `tungstenite` [`WebSocket`] over a potentially-TLS TCP stream.
/// Must be closed with [`hew_ws_close`].
#[derive(Debug)]
pub struct HewWsConn {
    ws: WebSocket<MaybeTlsStream<TcpStream>>,
}

/// Message received from a WebSocket connection.
///
/// Must be freed with [`hew_ws_message_free`].
#[repr(C)]
#[derive(Debug)]
pub struct HewWsMessage {
    /// Message type: 0 = text, 1 = binary, 2 = ping, 3 = pong, 4 = close, −1 = error.
    pub msg_type: i32,
    /// Payload data allocated with `malloc`. Caller frees via [`hew_ws_message_free`].
    pub data: *mut u8,
    /// Length of `data` in bytes.
    pub data_len: usize,
}

/// Allocate `len` bytes via `libc::malloc`, copying from `src`.
/// Returns null on allocation failure.
///
/// # Safety
///
/// If `len > 0`, `src` must point to at least `len` readable bytes.
unsafe fn malloc_copy(src: *const u8, len: usize) -> *mut u8 {
    if len == 0 {
        return std::ptr::null_mut();
    }
    // SAFETY: We request `len` bytes from malloc; it returns a valid pointer or null.
    let ptr = unsafe { libc::malloc(len) }.cast::<u8>();
    if ptr.is_null() {
        return ptr;
    }
    // SAFETY: Caller guarantees `src` is valid for `len` bytes; `ptr` is freshly
    // allocated with `len` bytes, so both regions are valid and non-overlapping.
    unsafe { std::ptr::copy_nonoverlapping(src, ptr, len) };
    ptr
}

/// Build a heap-allocated [`HewWsMessage`] from a type tag and byte slice.
fn build_message(msg_type: i32, payload: &[u8]) -> *mut HewWsMessage {
    // SAFETY: payload.as_ptr() is valid for payload.len() bytes.
    let data = unsafe { malloc_copy(payload.as_ptr(), payload.len()) };
    Box::into_raw(Box::new(HewWsMessage {
        msg_type,
        data,
        data_len: payload.len(),
    }))
}

/// Connect to a WebSocket server.
///
/// Supports both `ws://` and `wss://` URLs. Returns a heap-allocated
/// [`HewWsConn`] on success, or null on error.
///
/// # Safety
///
/// `url` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_connect(url: *const c_char) -> *mut HewWsConn {
    if url.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `url` is a valid NUL-terminated C string per caller contract.
    let Ok(url_str) = unsafe { CStr::from_ptr(url) }.to_str() else {
        return std::ptr::null_mut();
    };

    match tungstenite::connect(url_str) {
        Ok((ws, _response)) => Box::into_raw(Box::new(HewWsConn { ws })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Send a text message over a WebSocket connection.
///
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// * `ws` must be a valid pointer returned by [`hew_ws_connect`].
/// * `msg` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_text(ws: *mut HewWsConn, msg: *const c_char) -> i32 {
    if ws.is_null() || msg.is_null() {
        return -1;
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let conn = unsafe { &mut *ws };
    // SAFETY: `msg` is a valid NUL-terminated C string per caller contract.
    let Ok(text) = unsafe { CStr::from_ptr(msg) }.to_str() else {
        return -1;
    };

    match conn.ws.send(Message::text(text)) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Send a binary message over a WebSocket connection.
///
/// Returns 0 on success, −1 on error.
///
/// # Safety
///
/// * `ws` must be a valid pointer returned by [`hew_ws_connect`].
/// * `data` must point to at least `len` readable bytes, or be null if `len` is 0.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_send_binary(
    ws: *mut HewWsConn,
    data: *const u8,
    len: usize,
) -> i32 {
    if ws.is_null() {
        return -1;
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let conn = unsafe { &mut *ws };

    let slice = if len == 0 {
        &[]
    } else {
        if data.is_null() {
            return -1;
        }
        // SAFETY: `data` is valid for `len` bytes per caller contract.
        unsafe { std::slice::from_raw_parts(data, len) }
    };

    match conn.ws.send(Message::binary(slice.to_vec())) {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Receive the next message from a WebSocket connection (blocking).
///
/// Returns a heap-allocated [`HewWsMessage`], or null on error.
/// The caller must free the result with [`hew_ws_message_free`].
///
/// # Safety
///
/// `ws` must be a valid pointer returned by [`hew_ws_connect`].
#[no_mangle]
pub unsafe extern "C" fn hew_ws_recv(ws: *mut HewWsConn) -> *mut HewWsMessage {
    if ws.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: `ws` is a valid HewWsConn pointer per caller contract.
    let conn = unsafe { &mut *ws };

    match conn.ws.read() {
        Ok(msg) => match msg {
            Message::Text(t) => {
                let bytes = t.as_bytes();
                build_message(0, bytes)
            }
            Message::Binary(b) => build_message(1, &b),
            Message::Ping(b) => build_message(2, &b),
            Message::Pong(b) => build_message(3, &b),
            Message::Close(_) => build_message(4, &[]),
            Message::Frame(_) => build_message(1, &[]),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Close a WebSocket connection and free its resources.
///
/// # Safety
///
/// `ws` must be a valid pointer returned by [`hew_ws_connect`], and must not
/// have been closed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_close(ws: *mut HewWsConn) {
    if ws.is_null() {
        return;
    }
    // SAFETY: `ws` was allocated with Box::into_raw in hew_ws_connect.
    let mut conn = unsafe { Box::from_raw(ws) };
    // Best-effort close; ignore errors (connection may already be closed).
    let _ = conn.ws.close(None);
    // Drain remaining frames so the close handshake completes.
    while conn.ws.read().is_ok() {}
    // Box is dropped here, freeing the HewWsConn struct.
}

/// Free a [`HewWsMessage`] previously returned by [`hew_ws_recv`].
///
/// # Safety
///
/// `msg` must be a pointer previously returned by [`hew_ws_recv`], and must
/// not have been freed already. Passing null is a no-op.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_free(msg: *mut HewWsMessage) {
    if msg.is_null() {
        return;
    }
    // SAFETY: `msg` was allocated with Box::into_raw in build_message.
    let message = unsafe { Box::from_raw(msg) };
    if !message.data.is_null() {
        // SAFETY: `data` was allocated with libc::malloc in malloc_copy.
        unsafe { libc::free(message.data.cast()) };
    }
    // Box is dropped here, freeing the HewWsMessage struct.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connect_returns_null_for_invalid_url() {
        let url = c"ws://127.0.0.1:1";
        // SAFETY: url is a valid C string literal.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "expected null for unreachable address");
    }

    #[test]
    fn connect_returns_null_for_null_url() {
        // SAFETY: Passing null is explicitly handled.
        let conn = unsafe { hew_ws_connect(std::ptr::null()) };
        assert!(conn.is_null());
    }

    #[test]
    fn message_struct_layout() {
        let msg = HewWsMessage {
            msg_type: 0,
            data: std::ptr::null_mut(),
            data_len: 42,
        };
        assert_eq!(msg.msg_type, 0);
        assert!(msg.data.is_null());
        assert_eq!(msg.data_len, 42);

        // Verify C-repr field ordering via pointer offsets.
        let base = &msg as *const HewWsMessage as usize;
        let type_offset = &msg.msg_type as *const i32 as usize - base;
        let data_offset = &msg.data as *const *mut u8 as usize - base;
        let len_offset = &msg.data_len as *const usize as usize - base;
        assert_eq!(type_offset, 0, "msg_type must be at offset 0");
        assert!(data_offset > type_offset, "data must come after msg_type");
        assert!(len_offset > data_offset, "data_len must come after data");
    }

    #[test]
    fn build_message_roundtrip() {
        let payload = b"hello websocket";
        let msg = build_message(0, payload);
        assert!(!msg.is_null());
        // SAFETY: msg was just allocated by build_message.
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 0);
        assert_eq!(msg_ref.data_len, payload.len());
        // SAFETY: data was allocated with malloc_copy from payload.
        let data_slice = unsafe { std::slice::from_raw_parts(msg_ref.data, msg_ref.data_len) };
        assert_eq!(data_slice, payload);
        // SAFETY: msg was allocated by build_message.
        unsafe { hew_ws_message_free(msg) };
    }

    #[test]
    fn message_free_null_is_noop() {
        // SAFETY: Passing null is explicitly handled.
        unsafe { hew_ws_message_free(std::ptr::null_mut()) };
    }
}
