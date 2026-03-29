//! Hew runtime: `websocket` module.
//!
//! Provides synchronous WebSocket client functionality for compiled Hew programs.
//! All returned data pointers are allocated with `libc::malloc` so callers can
//! free them with the corresponding free function.

use std::ffi::CStr;
use std::net::{TcpListener, TcpStream};
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

/// Get the message type tag from a [`HewWsMessage`].
///
/// Returns 0=text, 1=binary, 2=ping, 3=pong, 4=close, -1=error/null.
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_ws_recv`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_type(msg: *const HewWsMessage) -> i32 {
    if msg.is_null() {
        return -1;
    }
    (unsafe { &*msg }).msg_type
}

/// Extract the text content from a [`HewWsMessage`] as a NUL-terminated C string.
///
/// Returns a `malloc`-allocated string the caller must free, or null if the
/// message is null or has no data.
///
/// # Safety
///
/// `msg` must be a valid pointer returned by [`hew_ws_recv`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_message_text(msg: *const HewWsMessage) -> *mut c_char {
    if msg.is_null() {
        return std::ptr::null_mut();
    }
    let m = unsafe { &*msg };
    if m.data.is_null() || m.data_len == 0 {
        return std::ptr::null_mut();
    }
    // Allocate len+1 for NUL terminator.
    let ptr = unsafe { libc::malloc(m.data_len + 1) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        std::ptr::copy_nonoverlapping(m.data, ptr, m.data_len);
        *ptr.add(m.data_len) = 0; // NUL terminator
    }
    ptr.cast::<c_char>()
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

// ── WebSocket Attach (Erlang-style active mode) ────────────────────
//
// `hew_ws_attach` transfers a WebSocket connection to a background OS
// thread that reads frames and delivers them as actor messages. The
// actor never calls recv() — it just has receive fns that the runtime
// invokes. This is Erlang's "active mode" pattern.

/// Attach a WebSocket connection to an actor. Spawns a reader thread
/// that delivers frames as actor messages.
///
/// - `ws`: the WebSocket connection (ownership transferred — the conn
///   is consumed and must not be used after this call)
/// - `actor`: pointer to the target actor
/// - `on_message_type`: msg_type index for text frame delivery
/// - `on_close_type`: msg_type index for close/error notification
///
/// The reader thread calls `hew_actor_send(actor, on_message_type, text, len)`
/// for each text frame, and `hew_actor_send(actor, on_close_type, null, 0)`
/// when the connection closes or errors.
///
/// # Safety
///
/// - `ws` must be a valid pointer returned by `hew_ws_connect` or
///   `hew_ws_server_accept`.
/// - `actor` must be a valid actor pointer that outlives the connection.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_attach(
    ws: *mut HewWsConn,
    actor: *mut std::ffi::c_void,
    on_message_type: i32,
    on_close_type: i32,
) {
    if ws.is_null() || actor.is_null() {
        eprintln!("[attach] null pointer: ws={} actor={}", ws.is_null(), actor.is_null());
        return;
    }
    eprintln!("[attach] ws={:p} actor={:p} msg_type={} close_type={}", ws, actor, on_message_type, on_close_type);
    // Take ownership of the connection.
    let conn = unsafe { Box::from_raw(ws) };

    let actor_ptr = actor as usize;

    std::thread::spawn(move || {
        eprintln!("[attach-reader] thread started, reading...");
        let mut ws = conn;
        loop {
            match ws.ws.read() {
                Ok(msg) => match msg {
                    tungstenite::Message::Text(text) => {
                        let bytes = text.as_bytes();
                        let len = bytes.len();
                        // Allocate a NUL-terminated copy (Hew String = char*).
                        let str_ptr = unsafe { libc::malloc(len + 1) }.cast::<u8>();
                        if !str_ptr.is_null() {
                            unsafe {
                                std::ptr::copy_nonoverlapping(bytes.as_ptr(), str_ptr, len);
                                *str_ptr.add(len) = 0; // NUL terminator
                            }
                            // Pack the string pointer into an 8-byte buffer.
                            // The dispatch function reads arguments from the data
                            // buffer as pointer-sized values. For a String param,
                            // it reads one pointer (8 bytes) from the buffer.
                            let mut arg_buf = [0u8; 8];
                            let ptr_val = str_ptr as usize;
                            arg_buf.copy_from_slice(&ptr_val.to_ne_bytes());
                            unsafe {
                                hew_actor_send(
                                    actor_ptr as *mut std::ffi::c_void,
                                    on_message_type,
                                    arg_buf.as_mut_ptr().cast(),
                                    8, // sizeof(ptr)
                                );
                            }
                        }
                    }
                    tungstenite::Message::Ping(_) => {
                        // Auto-respond with pong.
                        let _ = ws.ws.send(tungstenite::Message::Pong(vec![].into()));
                    }
                    tungstenite::Message::Close(_) => {
                        // Notify actor and exit.
                        unsafe {
                            hew_actor_send(
                                actor_ptr as *mut std::ffi::c_void,
                                on_close_type,
                                std::ptr::null_mut(),
                                0,
                            );
                        }
                        break;
                    }
                    _ => {} // Ignore binary, pong, frame
                },
                Err(_) => {
                    // Connection error — notify actor and exit.
                    unsafe {
                        hew_actor_send(
                            actor_ptr as *mut std::ffi::c_void,
                            on_close_type,
                            std::ptr::null_mut(),
                            0,
                        );
                    }
                    break;
                }
            }
        }
        // Connection cleanup: drop ws (closes the socket).
    });
}

// Import the actor send function from the runtime.
extern "C" {
    fn hew_actor_send(
        actor: *mut std::ffi::c_void,
        msg_type: i32,
        data: *mut std::ffi::c_void,
        size: usize,
    );
}

// ── WebSocket Server ────────────────────────────────────────────────

/// Opaque WebSocket server handle.
///
/// Wraps a [`TcpListener`] that accepts incoming connections and upgrades
/// them to WebSocket via tungstenite. Must be closed with [`hew_ws_server_close`].
#[derive(Debug)]
pub struct HewWsServer {
    listener: TcpListener,
}

/// Create a WebSocket server listening on the given address (e.g. `"0.0.0.0:8080"`).
///
/// Returns a heap-allocated [`HewWsServer`] on success, or null on error.
///
/// # Safety
///
/// `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_new(addr: *const c_char) -> *mut HewWsServer {
    if addr.is_null() {
        return std::ptr::null_mut();
    }
    let Ok(addr_str) = (unsafe { CStr::from_ptr(addr) }).to_str() else {
        return std::ptr::null_mut();
    };
    match TcpListener::bind(addr_str) {
        Ok(listener) => Box::into_raw(Box::new(HewWsServer { listener })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get the port the server is listening on.
///
/// Returns -1 if `server` is null or the address cannot be determined.
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`], or null.
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_port(server: *const HewWsServer) -> i32 {
    if server.is_null() {
        return -1;
    }
    match (unsafe { &*server }).listener.local_addr() {
        Ok(addr) => addr.port() as i32,
        Err(_) => -1,
    }
}

/// Accept one WebSocket connection. Blocks until a client connects and
/// completes the WebSocket handshake.
///
/// Returns a [`HewWsConn`] (same type as client connections) on success,
/// or null on error. The returned connection works with [`hew_ws_send_text`],
/// [`hew_ws_recv`], and [`hew_ws_close`].
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_accept(server: *mut HewWsServer) -> *mut HewWsConn {
    eprintln!("[accept] server={:p} null={}", server, server.is_null());
    if server.is_null() {
        eprintln!("[accept] server is NULL, returning null");
        return std::ptr::null_mut();
    }
    let srv = unsafe { &*server };
    // Block until a valid WebSocket connection is established.
    // Reject non-WebSocket TCP connections (failed handshakes) by retrying.
    loop {
        let (stream, _addr) = match srv.listener.accept() {
            Ok(pair) => pair,
            Err(_) => return std::ptr::null_mut(),
        };
        // tungstenite::accept performs the HTTP upgrade handshake.
        // If the client is not a WebSocket client, the handshake fails
        // and we loop back to accept the next connection.
        // Wrap in MaybeTlsStream::Plain BEFORE the handshake so
        // tungstenite::accept produces WebSocket<MaybeTlsStream<TcpStream>>
        // directly — no rewrap needed, preserving internal buffers.
        let tls_stream = MaybeTlsStream::Plain(stream);
        match tungstenite::accept(tls_stream) {
            Ok(ws) => {
                let ptr = Box::into_raw(Box::new(HewWsConn { ws }));
                eprintln!("[accept] returning conn {:p}", ptr);
                return ptr;
            }
            Err(_) => {
                // Handshake failed (not a WebSocket client). Retry.
                continue;
            }
        }
    }
}

/// Close the server and stop listening.
///
/// # Safety
///
/// `server` must be a valid pointer returned by [`hew_ws_server_new`],
/// or null (no-op).
#[no_mangle]
pub unsafe extern "C" fn hew_ws_server_close(server: *mut HewWsServer) {
    if !server.is_null() {
        drop(unsafe { Box::from_raw(server) });
    }
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
        let base = &raw const msg as usize;
        let type_offset = &raw const msg.msg_type as usize - base;
        let data_offset = &raw const msg.data as usize - base;
        let len_offset = &raw const msg.data_len as usize - base;
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

    /// `hew_ws_send_text` with null ws returns -1.
    #[test]
    fn send_text_null_ws_returns_error() {
        let msg = c"hello";
        assert_eq!(
            // SAFETY: null ws is explicitly handled.
            unsafe { hew_ws_send_text(std::ptr::null_mut(), msg.as_ptr()) },
            -1
        );
    }

    /// `hew_ws_send_text` with null msg returns -1.
    #[test]
    fn send_text_null_msg_returns_error() {
        // We can't create a real ws connection without a server, so test the
        // null-msg guard by passing null for both — ws null check fires first.
        assert_eq!(
            // SAFETY: null pointers are explicitly handled.
            unsafe { hew_ws_send_text(std::ptr::null_mut(), std::ptr::null()) },
            -1
        );
    }

    /// `hew_ws_send_binary` with null ws returns -1.
    #[test]
    fn send_binary_null_ws_returns_error() {
        let data = [1u8, 2, 3];
        assert_eq!(
            // SAFETY: null ws is explicitly handled.
            unsafe { hew_ws_send_binary(std::ptr::null_mut(), data.as_ptr(), data.len()) },
            -1
        );
    }

    /// `hew_ws_recv` with null ws returns null.
    #[test]
    fn recv_null_ws_returns_null() {
        // SAFETY: null ws is explicitly handled.
        assert!(unsafe { hew_ws_recv(std::ptr::null_mut()) }.is_null());
    }

    /// `hew_ws_close` with null ws is a no-op.
    #[test]
    fn close_null_ws_is_noop() {
        // SAFETY: null ws is explicitly handled.
        unsafe { hew_ws_close(std::ptr::null_mut()) };
    }

    /// `build_message` with empty payload creates a valid message with null data.
    #[test]
    fn build_message_empty_payload() {
        let msg = build_message(4, &[]);
        assert!(!msg.is_null());
        // SAFETY: msg was just allocated by build_message.
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 4);
        assert_eq!(msg_ref.data_len, 0);
        assert!(
            msg_ref.data.is_null(),
            "empty payload should have null data"
        );
        // SAFETY: msg was allocated by build_message.
        unsafe { hew_ws_message_free(msg) };
    }

    /// connect with an HTTP URL (not ws://) returns null.
    #[test]
    fn connect_http_url_returns_null() {
        let url = c"http://127.0.0.1:1/path";
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "non-WebSocket URL should fail");
    }

    /// connect with an empty string returns null.
    #[test]
    fn connect_empty_url_returns_null() {
        let url = c"";
        // SAFETY: url is a valid C string.
        let conn = unsafe { hew_ws_connect(url.as_ptr()) };
        assert!(conn.is_null(), "empty URL should fail");
    }

    // ── Server tests ────────────────────────────────────────────────

    /// Server listens, client connects, exchanges a message, closes.
    #[test]
    fn server_accept_and_echo() {
        // Bind to port 0 for a random available port.
        let server = unsafe { hew_ws_server_new(c"127.0.0.1:0".as_ptr()) };
        assert!(!server.is_null(), "server should bind successfully");

        let port = unsafe { hew_ws_server_port(server) };
        assert!(port > 0, "port should be positive");

        let addr = format!("ws://127.0.0.1:{port}");
        let client_thread = std::thread::spawn(move || {
            let (mut ws, _) = tungstenite::connect(&addr).expect("client connect");
            ws.send(Message::text("hello from client"))
                .expect("client send");
            let reply = ws.read().expect("client read");
            assert_eq!(reply, Message::Text("echo: hello from client".into()));
            ws.close(None).ok();
            // Drain remaining frames so close handshake completes.
            while ws.read().is_ok() {}
        });

        // Accept one connection on the server side.
        let conn = unsafe { hew_ws_server_accept(server) };
        assert!(!conn.is_null(), "accept should succeed");

        // Receive the client's message.
        let msg = unsafe { hew_ws_recv(conn) };
        assert!(!msg.is_null(), "recv should succeed");
        let msg_ref = unsafe { &*msg };
        assert_eq!(msg_ref.msg_type, 0, "should be a text message");
        let text = unsafe {
            std::str::from_utf8(std::slice::from_raw_parts(msg_ref.data, msg_ref.data_len))
                .expect("valid utf8")
        };
        assert_eq!(text, "hello from client");

        // Echo back.
        let echo = std::ffi::CString::new(format!("echo: {text}")).unwrap();
        let rc = unsafe { hew_ws_send_text(conn, echo.as_ptr()) };
        assert_eq!(rc, 0, "send should succeed");

        unsafe { hew_ws_message_free(msg) };
        unsafe { hew_ws_close(conn) };
        unsafe { hew_ws_server_close(server) };

        client_thread.join().expect("client thread should finish");
    }

    /// Server with null addr returns null.
    #[test]
    fn server_null_addr_returns_null() {
        let server = unsafe { hew_ws_server_new(std::ptr::null()) };
        assert!(server.is_null());
    }

    /// Server port with null returns -1.
    #[test]
    fn server_port_null_returns_neg1() {
        assert_eq!(unsafe { hew_ws_server_port(std::ptr::null()) }, -1);
    }

    /// Server accept with null returns null.
    #[test]
    fn server_accept_null_returns_null() {
        assert!(unsafe { hew_ws_server_accept(std::ptr::null_mut()) }.is_null());
    }

    /// Server close with null is a no-op.
    #[test]
    fn server_close_null_is_noop() {
        unsafe { hew_ws_server_close(std::ptr::null_mut()) };
    }
}
