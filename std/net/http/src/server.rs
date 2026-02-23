//! Hew runtime: synchronous HTTP server.
//!
//! Provides an HTTP server built on [`tiny_http`] that can be driven from
//! compiled Hew programs via the C ABI functions below.

use hew_cabi::cabi::str_to_malloc;
use hew_cabi::sink::{into_sink_ptr, set_last_error, HewSink, SinkBacking};
use std::ffi::{c_char, CStr};
use std::io::Read;
use std::sync::mpsc;

const MAX_BODY_SIZE: usize = 10 * 1024 * 1024;

/// Opaque HTTP server handle.
pub struct HewHttpServer {
    inner: tiny_http::Server,
    max_body_size: usize,
}

impl std::fmt::Debug for HewHttpServer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewHttpServer").finish_non_exhaustive()
    }
}

/// Incoming HTTP request handle.
pub struct HewHttpRequest {
    inner: Option<tiny_http::Request>,
    max_body_size: usize,
}

impl std::fmt::Debug for HewHttpRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewHttpRequest").finish_non_exhaustive()
    }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// C ABI functions
// ---------------------------------------------------------------------------

/// Create an HTTP server bound to `addr` (e.g. `"0.0.0.0:8080"`).
///
/// Returns a heap-allocated [`HewHttpServer`], or null on error.
///
/// # Safety
///
/// `addr` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_server_new(addr: *const c_char) -> *mut HewHttpServer {
    if addr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: addr is a valid NUL-terminated C string per caller contract.
    let Ok(addr_str) = unsafe { CStr::from_ptr(addr) }.to_str() else {
        return std::ptr::null_mut();
    };

    match tiny_http::Server::http(addr_str) {
        Ok(server) => Box::into_raw(Box::new(HewHttpServer {
            inner: server,
            max_body_size: MAX_BODY_SIZE,
        })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Block until the next request arrives.
///
/// Returns a heap-allocated [`HewHttpRequest`], or null on error.
///
/// # Safety
///
/// `srv` must be a valid pointer to a [`HewHttpServer`] previously returned by
/// [`hew_http_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_http_server_recv(srv: *mut HewHttpServer) -> *mut HewHttpRequest {
    if srv.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: srv was allocated by hew_http_server_new and is valid.
    let server = unsafe { &*srv };

    match server.inner.recv() {
        Ok(req) => Box::into_raw(Box::new(HewHttpRequest {
            inner: Some(req),
            max_body_size: server.max_body_size,
        })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Set max request body size (in bytes) for future received requests.
///
/// Returns 0 on success, -1 on invalid input.
///
/// # Safety
///
/// `srv` must be a valid pointer to a [`HewHttpServer`] previously returned by
/// [`hew_http_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_http_server_set_max_body(
    srv: *mut HewHttpServer,
    max_bytes: i64,
) -> i32 {
    if srv.is_null() || max_bytes <= 0 {
        return -1;
    }
    let Ok(max_body_size) = usize::try_from(max_bytes) else {
        return -1;
    };
    // SAFETY: srv was allocated by hew_http_server_new and is valid.
    let server = unsafe { &mut *srv };
    server.max_body_size = max_body_size;
    0
}

/// Return the HTTP method of the request as a `malloc`-allocated C string.
///
/// # Safety
///
/// `req` must be a valid pointer to a [`HewHttpRequest`] whose `inner` is
/// `Some`.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_method(req: *const HewHttpRequest) -> *mut c_char {
    if req.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &*req };
    match request.inner.as_ref() {
        Some(r) => str_to_malloc(r.method().as_str()),
        None => std::ptr::null_mut(),
    }
}

/// Return the URL path of the request as a `malloc`-allocated C string.
///
/// # Safety
///
/// `req` must be a valid pointer to a [`HewHttpRequest`] whose `inner` is
/// `Some`.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_path(req: *const HewHttpRequest) -> *mut c_char {
    if req.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &*req };
    match request.inner.as_ref() {
        Some(r) => str_to_malloc(r.url()),
        None => std::ptr::null_mut(),
    }
}

/// Read the request body into a `malloc`-allocated buffer.
///
/// On success, `*out_len` is set to the number of bytes read and the return
/// value points to the buffer. On error, returns null.
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`] whose
///   `inner` is `Some`.
/// * `out_len` must be a valid pointer to a `usize`.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_body(
    req: *mut HewHttpRequest,
    out_len: *mut usize,
) -> *mut u8 {
    if req.is_null() || out_len.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &mut *req };
    let Some(mut inner) = request.inner.take() else {
        return std::ptr::null_mut();
    };

    let mut buf = Vec::new();
    let mut reader = inner.as_reader();
    if Read::take(&mut reader, request.max_body_size as u64 + 1)
        .read_to_end(&mut buf)
        .is_err()
    {
        request.inner = Some(inner);
        return std::ptr::null_mut();
    }
    if buf.len() > request.max_body_size {
        let response = tiny_http::Response::from_string("Payload Too Large")
            .with_status_code(tiny_http::StatusCode(413));
        let _ = inner.respond(response);
        return std::ptr::null_mut();
    }
    request.inner = Some(inner);

    let len = buf.len();
    // SAFETY: We allocate len bytes (or 1 if empty) via malloc.
    let ptr = unsafe { libc::malloc(if len == 0 { 1 } else { len }) }.cast::<u8>();
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    if len > 0 {
        // SAFETY: buf.as_ptr() is valid for len bytes; ptr is freshly
        // allocated with at least len bytes.
        unsafe { std::ptr::copy_nonoverlapping(buf.as_ptr(), ptr, len) };
    }
    // SAFETY: out_len is valid per caller contract.
    unsafe { *out_len = len };
    ptr
}

/// Return the value of the named HTTP header as a `malloc`-allocated C string.
///
/// Header name matching is case-insensitive. Returns null if the header is
/// absent or if any argument is invalid.
///
/// # Safety
///
/// * `req` must be a valid pointer to a [`HewHttpRequest`] whose `inner` is
///   `Some`.
/// * `name` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_header(
    req: *const HewHttpRequest,
    name: *const c_char,
) -> *mut c_char {
    if req.is_null() || name.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &*req };
    // SAFETY: name is a valid NUL-terminated C string per caller contract.
    let Ok(name_str) = unsafe { CStr::from_ptr(name) }.to_str() else {
        return std::ptr::null_mut();
    };

    let Some(inner) = request.inner.as_ref() else {
        return std::ptr::null_mut();
    };

    for header in inner.headers() {
        if header
            .field
            .as_str()
            .as_str()
            .eq_ignore_ascii_case(name_str)
        {
            return str_to_malloc(header.value.as_str());
        }
    }
    std::ptr::null_mut()
}

/// Send an HTTP response for the given request.
///
/// Takes ownership of the request's inner handle (sets it to `None`).
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`] whose
///   `inner` is `Some`.
/// * If `body_len > 0`, `body` must point to at least `body_len` readable
///   bytes.
/// * `content_type` must be a valid NUL-terminated C string (or null for no
///   `Content-Type` header).
#[no_mangle]
pub unsafe extern "C" fn hew_http_respond(
    req: *mut HewHttpRequest,
    status: i32,
    body: *const u8,
    body_len: usize,
    content_type: *const c_char,
) -> i32 {
    if req.is_null() {
        return -1;
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &mut *req };
    let Some(inner) = request.inner.take() else {
        return -1;
    };

    let body_vec = if body.is_null() || body_len == 0 {
        Vec::new()
    } else {
        // SAFETY: body is valid for body_len bytes per caller contract.
        unsafe { std::slice::from_raw_parts(body, body_len) }.to_vec()
    };

    #[expect(
        clippy::cast_possible_truncation,
        reason = "HTTP status codes fit in u16"
    )]
    #[expect(
        clippy::cast_sign_loss,
        reason = "status codes are always non-negative in practice"
    )]
    let status_code = tiny_http::StatusCode(status.max(0) as u16);
    let mut response = tiny_http::Response::from_data(body_vec).with_status_code(status_code);

    if !content_type.is_null() {
        // SAFETY: content_type is a valid NUL-terminated C string per caller contract.
        if let Ok(ct) = unsafe { CStr::from_ptr(content_type) }.to_str() {
            if let Ok(header) = tiny_http::Header::from_bytes("Content-Type", ct) {
                response = response.with_header(header);
            }
        }
    }

    if inner.respond(response).is_ok() {
        0
    } else {
        -1
    }
}

/// Send a `text/plain` response.
///
/// Convenience wrapper around [`hew_http_respond`].
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`].
/// * `text` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_respond_text(
    req: *mut HewHttpRequest,
    status: i32,
    text: *const c_char,
) -> i32 {
    if req.is_null() || text.is_null() {
        return -1;
    }
    // SAFETY: text is a valid NUL-terminated C string per caller contract.
    let text_bytes = unsafe { CStr::from_ptr(text) }.to_bytes();
    let ct = c"text/plain; charset=utf-8";
    // SAFETY: All pointers are valid; text_bytes.len() matches the buffer.
    unsafe {
        hew_http_respond(
            req,
            status,
            text_bytes.as_ptr(),
            text_bytes.len(),
            ct.as_ptr(),
        )
    }
}

/// Send an `application/json` response.
///
/// Convenience wrapper around [`hew_http_respond`].
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`].
/// * `json` must be a valid NUL-terminated C string containing JSON.
#[no_mangle]
pub unsafe extern "C" fn hew_http_respond_json(
    req: *mut HewHttpRequest,
    status: i32,
    json: *const c_char,
) -> i32 {
    if req.is_null() || json.is_null() {
        return -1;
    }
    // SAFETY: json is a valid NUL-terminated C string per caller contract.
    let json_bytes = unsafe { CStr::from_ptr(json) }.to_bytes();
    let ct = c"application/json";
    // SAFETY: All pointers are valid; json_bytes.len() matches the buffer.
    unsafe {
        hew_http_respond(
            req,
            status,
            json_bytes.as_ptr(),
            json_bytes.len(),
            ct.as_ptr(),
        )
    }
}

// ---------------------------------------------------------------------------
// Streaming response support
// ---------------------------------------------------------------------------

/// Adapter that bridges a `mpsc::Receiver<Vec<u8>>` into `std::io::Read`
/// so that `tiny_http` can stream the response body chunk by chunk.
struct ChannelReader {
    rx: mpsc::Receiver<Vec<u8>>,
    buf: Vec<u8>,
    offset: usize,
}

impl Read for ChannelReader {
    fn read(&mut self, out: &mut [u8]) -> std::io::Result<usize> {
        // Refill from channel when the current buffer is exhausted.
        while self.offset >= self.buf.len() {
            match self.rx.recv() {
                Ok(data) => {
                    self.buf = data;
                    self.offset = 0;
                }
                Err(_) => return Ok(0), // Sender dropped → EOF
            }
        }
        let avail = self.buf.len() - self.offset;
        let n = avail.min(out.len());
        out[..n].copy_from_slice(&self.buf[self.offset..self.offset + n]);
        self.offset += n;
        Ok(n)
    }
}

/// Sink backend that forwards writes to a channel for HTTP streaming.
#[derive(Debug)]
struct HttpResponseSink {
    tx: Option<mpsc::SyncSender<Vec<u8>>>,
}

impl SinkBacking for HttpResponseSink {
    fn write_item(&mut self, data: &[u8]) {
        if let Some(ref tx) = self.tx {
            let _ = tx.send(data.to_vec());
        }
    }
    fn flush(&mut self) {
        // tiny_http flushes on its own schedule; nothing to do here.
    }
    fn close(&mut self) {
        // Drop the sender to signal EOF to the ChannelReader.
        self.tx.take();
    }
}

/// Begin a streaming HTTP response, returning a `Sink` that writes directly
/// to the response body.
///
/// The response uses `Transfer-Encoding: chunked` so no `Content-Length` is
/// needed. Each `sink.write()` sends a chunk to the client immediately.
/// Call `sink.close()` (or drop the sink) to finish the response.
///
/// Returns a `*mut HewSink`, or null on error.
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`] whose
///   `inner` is `Some`.
/// * `content_type` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_http_respond_stream(
    req: *mut HewHttpRequest,
    status: i32,
    content_type: *const c_char,
) -> *mut HewSink {
    if req.is_null() {
        set_last_error("invalid request pointer".into());
        return std::ptr::null_mut();
    }
    // SAFETY: req was allocated by hew_http_server_recv and is valid.
    let request = unsafe { &mut *req };
    let Some(inner) = request.inner.take() else {
        set_last_error("request already responded to".into());
        return std::ptr::null_mut();
    };

    // Parse content type before spawning the thread.
    let ct_string = if content_type.is_null() {
        None
    } else {
        // SAFETY: content_type is a valid NUL-terminated C string per contract.
        unsafe { CStr::from_ptr(content_type) }
            .to_str()
            .ok()
            .map(String::from)
    };

    // Bounded channel — backpressure if the network is slower than the producer.
    let (tx, rx) = mpsc::sync_channel::<Vec<u8>>(64);

    let reader = ChannelReader {
        rx,
        buf: Vec::new(),
        offset: 0,
    };

    #[expect(
        clippy::cast_possible_truncation,
        reason = "HTTP status codes fit in u16"
    )]
    #[expect(
        clippy::cast_sign_loss,
        reason = "status codes are always non-negative in practice"
    )]
    let status_u16 = status.max(0) as u16;

    // Spawn a thread that drives tiny_http's response from the ChannelReader.
    // The thread lives until the Sink is closed (sender dropped → reader EOF).
    std::thread::spawn(move || {
        let mut response = tiny_http::Response::new(
            tiny_http::StatusCode(status_u16),
            Vec::new(),
            reader,
            None, // No Content-Length — tiny_http uses chunked encoding
            None,
        );
        if let Some(ct) = ct_string {
            if let Ok(header) = tiny_http::Header::from_bytes("Content-Type", ct) {
                response = response.with_header(header);
            }
        }
        let _ = inner.respond(response);
    });

    into_sink_ptr(HttpResponseSink { tx: Some(tx) })
}

/// Close and free the HTTP server.
///
/// # Safety
///
/// `srv` must be a valid pointer previously returned by [`hew_http_server_new`],
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_http_server_close(srv: *mut HewHttpServer) {
    if srv.is_null() {
        return;
    }
    // SAFETY: srv was allocated with Box::into_raw in hew_http_server_new.
    drop(unsafe { Box::from_raw(srv) });
}

/// Free a request handle without sending a response.
///
/// The underlying connection is dropped (client sees a connection reset).
///
/// # Safety
///
/// `req` must be a valid pointer previously returned by [`hew_http_server_recv`],
/// and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_free(req: *mut HewHttpRequest) {
    if req.is_null() {
        return;
    }
    // SAFETY: req was allocated with Box::into_raw in hew_http_server_recv.
    drop(unsafe { Box::from_raw(req) });
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_impls_compile() {
        // HewHttpRequest with inner = None is safe to construct without a port.
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let dbg = format!("{req:?}");
        assert!(dbg.contains("HewHttpRequest"));

        // We cannot construct a HewHttpServer without binding, but we can
        // verify the Debug impl exists via the type system.
        fn _assert_debug<T: std::fmt::Debug>() {}
        _assert_debug::<HewHttpServer>();
    }

    #[test]
    fn respond_on_consumed_request_returns_error() {
        // A request with inner = None simulates an already-responded request.
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        // SAFETY: req is a valid local struct; body is empty.
        let result = unsafe { hew_http_respond(&mut req, 200, std::ptr::null(), 0, ct.as_ptr()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_stream_on_consumed_request_sets_last_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        let sink = unsafe { hew_http_respond_stream(&mut req, 200, ct.as_ptr()) };
        assert!(sink.is_null());

        let err = hew_cabi::sink::hew_stream_last_error();
        assert!(!err.is_null());
        let err_msg = unsafe { CStr::from_ptr(err) }
            .to_str()
            .expect("error should be utf-8");
        assert_eq!(err_msg, "request already responded to");
        unsafe { libc::free(err.cast()) };
    }

    #[test]
    fn null_guards_return_safely() {
        // All functions should handle null pointers gracefully.
        // SAFETY: Passing null is the exact scenario we are testing.
        unsafe {
            assert!(hew_http_server_new(std::ptr::null()).is_null());
            assert!(hew_http_server_recv(std::ptr::null_mut()).is_null());
            assert!(hew_http_request_method(std::ptr::null()).is_null());
            assert!(hew_http_request_path(std::ptr::null()).is_null());
            assert!(hew_http_request_body(std::ptr::null_mut(), std::ptr::null_mut()).is_null());
            assert!(hew_http_request_header(std::ptr::null(), std::ptr::null()).is_null());
            assert_eq!(
                hew_http_respond(
                    std::ptr::null_mut(),
                    200,
                    std::ptr::null(),
                    0,
                    std::ptr::null()
                ),
                -1
            );
            assert_eq!(
                hew_http_respond_text(std::ptr::null_mut(), 200, std::ptr::null()),
                -1
            );
            assert_eq!(
                hew_http_respond_json(std::ptr::null_mut(), 200, std::ptr::null()),
                -1
            );
            hew_http_server_close(std::ptr::null_mut());
            hew_http_request_free(std::ptr::null_mut());
        }
    }
}
