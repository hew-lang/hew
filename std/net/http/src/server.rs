//! Hew runtime: synchronous HTTP server.
//!
//! Provides an HTTP server built on [`tiny_http`] that can be driven from
//! compiled Hew programs via the C ABI functions below.

use hew_cabi::cabi::{malloc_cstring, str_to_malloc};
use hew_cabi::sink::{into_write_sink_ptr, set_last_error, HewSink};
use std::ffi::{c_char, CStr};
use std::io::{Read, Write};
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

/// Read the request body and return it as a `malloc`-allocated, NUL-terminated
/// C string.
///
/// This is the bridge function that matches the Hew-side ABI
/// `body(req, encoding) -> String`. The `encoding` parameter is accepted for
/// forward-compatibility but currently only UTF-8 is produced.
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`] whose
///   `inner` is `Some`.
/// * `encoding` must be a valid NUL-terminated C string (or null).
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_body_string(
    req: *mut HewHttpRequest,
    _encoding: *const c_char,
) -> *mut c_char {
    let mut out_len: usize = 0;
    // SAFETY: req validity is the caller's responsibility; out_len is a
    // valid local variable.
    let ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: ptr is valid for out_len bytes per hew_http_request_body's contract.
    let result = unsafe { malloc_cstring(ptr, out_len) };
    // SAFETY: ptr was allocated via libc::malloc inside hew_http_request_body.
    unsafe { libc::free(ptr.cast()) };
    result
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

/// Bridge for the Hew-side `respond(req, status, content_type, body)` ABI.
///
/// Accepts arguments in the order emitted by codegen (matching the Hew API
/// surface) and forwards to [`hew_http_respond`] with the correct C ABI
/// argument order. The `Content-Length` header is derived from `body`
/// automatically; callers must not pass it separately.
///
/// # Safety
///
/// * `req` must be a valid, mutable pointer to a [`HewHttpRequest`] whose
///   `inner` is `Some`.
/// * `content_type` must be a valid NUL-terminated C string (or null).
/// * `body` must be a valid NUL-terminated C string (or null for empty body).
#[no_mangle]
pub unsafe extern "C" fn hew_http_respond_bridge(
    req: *mut HewHttpRequest,
    status: i32,
    content_type: *const c_char,
    body: *const c_char,
) -> i32 {
    let (body_ptr, body_len) = if body.is_null() {
        (std::ptr::null(), 0usize)
    } else {
        // SAFETY: body is a valid NUL-terminated C string per caller contract.
        let bytes = unsafe { CStr::from_ptr(body) }.to_bytes();
        (bytes.as_ptr(), bytes.len())
    };
    // SAFETY: All pointers are valid per caller contract.
    unsafe { hew_http_respond(req, status, body_ptr, body_len, content_type) }
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

impl Write for HttpResponseSink {
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        if let Some(ref tx) = self.tx {
            tx.send(data.to_vec()).map_err(|_| {
                std::io::Error::new(
                    std::io::ErrorKind::BrokenPipe,
                    "failed to send to HTTP response sink",
                )
            })?;
        }
        Ok(data.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        // tiny_http flushes on its own schedule; nothing to do here.
        Ok(())
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

    into_write_sink_ptr(HttpResponseSink { tx: Some(tx) })
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
        fn assert_debug<T: std::fmt::Debug>() {}

        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let dbg = format!("{req:?}");
        assert!(dbg.contains("HewHttpRequest"));

        assert_debug::<HewHttpServer>();
    }

    #[test]
    fn respond_on_consumed_request_returns_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        // SAFETY: req is a valid local struct; body is empty.
        let result =
            unsafe { hew_http_respond(&raw mut req, 200, std::ptr::null(), 0, ct.as_ptr()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_stream_on_consumed_request_sets_last_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        // SAFETY: req is a valid mutable pointer; ct is a valid C string literal.
        let sink = unsafe { hew_http_respond_stream(&raw mut req, 200, ct.as_ptr()) };
        assert!(sink.is_null());

        let err = hew_cabi::sink::hew_stream_last_error();
        assert!(!err.is_null());
        // SAFETY: err is a valid NUL-terminated C string from hew_stream_last_error.
        let err_msg = unsafe { CStr::from_ptr(err) }
            .to_str()
            .expect("error should be utf-8");
        assert_eq!(err_msg, "request already responded to");
        // SAFETY: err was allocated by hew_stream_last_error (via libc::malloc).
        unsafe { libc::free(err.cast()) };
    }

    #[test]
    fn null_guards_return_safely() {
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

    // -- Server construction ------------------------------------------

    #[test]
    fn server_new_loopback_returns_non_null() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null(), "binding to loopback:0 should succeed");
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn server_new_invalid_addr_returns_null() {
        let addr = c"not-a-valid-address";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(srv.is_null(), "invalid address should return null");
    }

    // -- set_max_body -------------------------------------------------

    #[test]
    fn set_max_body_null_server_returns_error() {
        // SAFETY: null pointer is the tested scenario.
        let result = unsafe { hew_http_server_set_max_body(std::ptr::null_mut(), 1024) };
        assert_eq!(result, -1);
    }

    #[test]
    fn set_max_body_zero_returns_error() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 0 is an invalid max body size.
        let result = unsafe { hew_http_server_set_max_body(srv, 0) };
        assert_eq!(result, -1, "zero max body should be rejected");
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn set_max_body_negative_returns_error() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; -1 is invalid.
        let result = unsafe { hew_http_server_set_max_body(srv, -1) };
        assert_eq!(result, -1, "negative max body should be rejected");
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn set_max_body_valid_returns_success() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 4096 is a valid size.
        let result = unsafe { hew_http_server_set_max_body(srv, 4096) };
        assert_eq!(result, 0);
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    // -- Request accessors on consumed request ------------------------

    #[test]
    fn request_method_consumed_returns_null() {
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: req is a valid local struct with inner = None.
        let result = unsafe { hew_http_request_method(&raw const req) };
        assert!(result.is_null());
    }

    #[test]
    fn request_path_consumed_returns_null() {
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: req is a valid local struct with inner = None.
        let result = unsafe { hew_http_request_path(&raw const req) };
        assert!(result.is_null());
    }

    #[test]
    fn request_body_consumed_returns_null() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let mut out_len: usize = 99;
        // SAFETY: req and out_len are valid local variables.
        let result = unsafe { hew_http_request_body(&raw mut req, &raw mut out_len) };
        assert!(result.is_null());
    }

    #[test]
    fn request_header_consumed_returns_null() {
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let name = c"content-type";
        // SAFETY: req is valid with inner = None; name is a valid C string.
        let result = unsafe { hew_http_request_header(&raw const req, name.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn request_header_null_name_returns_null() {
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: null name is the tested scenario.
        let result = unsafe { hew_http_request_header(&raw const req, std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn request_body_null_out_len_returns_null() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: out_len is null (tested scenario).
        let result = unsafe { hew_http_request_body(&raw mut req, std::ptr::null_mut()) };
        assert!(result.is_null());
    }

    // -- respond_text and respond_json null text ----------------------

    #[test]
    fn respond_text_null_text_returns_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: text is null (tested scenario); req is valid.
        let result = unsafe { hew_http_respond_text(&raw mut req, 200, std::ptr::null()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_json_null_json_returns_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: json is null (tested scenario); req is valid.
        let result = unsafe { hew_http_respond_json(&raw mut req, 200, std::ptr::null()) };
        assert_eq!(result, -1);
    }

    // -- respond_stream null request ----------------------------------

    #[test]
    fn respond_stream_null_request_returns_null() {
        let ct = c"text/plain";
        // SAFETY: null request is the tested scenario.
        let sink = unsafe { hew_http_respond_stream(std::ptr::null_mut(), 200, ct.as_ptr()) };
        assert!(sink.is_null());
    }

    // -- Loopback integration tests -----------------------------------

    /// Helper: read a malloc'd C string and free it.
    unsafe fn take_cstr(ptr: *mut c_char) -> String {
        assert!(!ptr.is_null());
        // SAFETY: ptr is a valid malloc'd C string.
        let s = unsafe { CStr::from_ptr(ptr) }.to_str().unwrap().to_owned();
        // SAFETY: ptr was allocated via libc::malloc in str_to_malloc.
        unsafe { libc::free(ptr.cast()) };
        s
    }

    /// Get the server's bound address as `http://127.0.0.1:<port>`.
    fn server_addr(srv: *mut HewHttpServer) -> String {
        // SAFETY: srv is valid and was just created.
        let server = unsafe { &*srv };
        format!("http://{}", server.inner.server_addr().to_ip().unwrap())
    }

    #[test]
    fn loopback_get_recv_respond_text() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle =
            std::thread::spawn(move || ureq::get(&format!("{base}/hello")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        // SAFETY: req is valid with inner = Some.
        let method = unsafe { take_cstr(hew_http_request_method(req)) };
        assert_eq!(method, "GET");

        // SAFETY: req is valid with inner = Some.
        let path = unsafe { take_cstr(hew_http_request_path(req)) };
        assert_eq!(path, "/hello");

        let text = c"world";
        // SAFETY: req is valid; text is a valid C string.
        let result = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);
        let body = resp.into_body().read_to_string().unwrap();
        assert_eq!(body, "world");

        // SAFETY: req was already responded to; free is safe.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_post_recv_read_body() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/submit"))
                .header("Content-Type", "application/json")
                .send(b"{\"key\":\"value\"}" as &[u8])
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let mut out_len: usize = 0;
        // SAFETY: req is valid, out_len is valid.
        let body_ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
        assert!(!body_ptr.is_null());
        assert!(out_len > 0);
        // SAFETY: body_ptr is valid for out_len bytes.
        let received = unsafe { std::slice::from_raw_parts(body_ptr, out_len) };
        let received_str = std::str::from_utf8(received).unwrap();
        assert_eq!(received_str, "{\"key\":\"value\"}");
        // SAFETY: body_ptr was malloc'd.
        unsafe { libc::free(body_ptr.cast()) };

        let ct_name = c"Content-Type";
        // SAFETY: req and ct_name are valid.
        let ct_val = unsafe { hew_http_request_header(req, ct_name.as_ptr()) };
        assert!(!ct_val.is_null());
        // SAFETY: ct_val is a valid malloc'd C string.
        let ct = unsafe { take_cstr(ct_val) };
        assert_eq!(ct, "application/json");

        let json = c"{\"status\":\"ok\"}";
        // SAFETY: req is valid; json is a valid C string.
        let result = unsafe { hew_http_respond_json(req, 200, json.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_respond_with_raw_body() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || ureq::get(&format!("{base}/raw")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let body = b"raw bytes here";
        let ct = c"application/octet-stream";
        // SAFETY: req is valid; body and ct are valid.
        let result = unsafe { hew_http_respond(req, 200, body.as_ptr(), body.len(), ct.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);
        let body_str = resp.into_body().read_to_string().unwrap();
        assert_eq!(body_str, "raw bytes here");

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_respond_empty_body() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle =
            std::thread::spawn(move || ureq::get(&format!("{base}/empty")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let ct = c"text/plain";
        // SAFETY: req is valid; null body with zero length is allowed.
        let result = unsafe { hew_http_respond(req, 204, std::ptr::null(), 0, ct.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 204);

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_respond_stream_sends_chunks() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle =
            std::thread::spawn(move || ureq::get(&format!("{base}/stream")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let ct = c"text/plain";
        // SAFETY: req and ct are valid.
        let sink = unsafe { hew_http_respond_stream(req, 200, ct.as_ptr()) };
        assert!(!sink.is_null(), "respond_stream should return a valid sink");

        // SAFETY: sink is a valid HewSink from into_sink_ptr.
        let sink_ref = unsafe { &mut *sink };
        sink_ref.write_item(b"chunk1");
        sink_ref.write_item(b"chunk2");
        sink_ref.close();

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);
        let body = resp.into_body().read_to_string().unwrap();
        assert_eq!(body, "chunk1chunk2");

        // SAFETY: sink was allocated by into_sink_ptr.
        drop(unsafe { Box::from_raw(sink) });

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_request_header_missing_returns_null() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || ureq::get(&format!("{base}/hdr")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let name = c"X-Nonexistent";
        // SAFETY: req and name are valid.
        let val = unsafe { hew_http_request_header(req, name.as_ptr()) };
        assert!(val.is_null(), "missing header should return null");

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let _ = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        handle.join().unwrap();

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_request_header_case_insensitive() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::get(&format!("{base}/ci"))
                .header("X-My-Header", "found-it")
                .call()
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let name = c"x-my-header";
        // SAFETY: req and name are valid.
        let val = unsafe { hew_http_request_header(req, name.as_ptr()) };
        assert!(!val.is_null());
        // SAFETY: val is a valid malloc'd C string.
        let s = unsafe { take_cstr(val) };
        assert_eq!(s, "found-it");

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let _ = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        handle.join().unwrap();

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_empty_post_body() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/empty"))
                .header("Content-Type", "text/plain")
                .send(b"" as &[u8])
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let mut out_len: usize = 99;
        // SAFETY: req and out_len are valid.
        let body_ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
        assert!(!body_ptr.is_null());
        assert_eq!(out_len, 0);
        // SAFETY: body_ptr was malloc'd.
        unsafe { libc::free(body_ptr.cast()) };

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let _ = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        handle.join().unwrap();

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_max_body_exceeded_returns_413() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 5 is a valid max body size.
        let result = unsafe { hew_http_server_set_max_body(srv, 5) };
        assert_eq!(result, 0);
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/big"))
                .header("Content-Type", "text/plain")
                .send(b"this body is longer than 5 bytes" as &[u8])
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let mut out_len: usize = 0;
        // SAFETY: req and out_len are valid.
        let body_ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
        assert!(body_ptr.is_null(), "oversized body should be rejected");

        let client_result = handle.join().unwrap();
        match client_result {
            Ok(resp) => assert_eq!(resp.status().as_u16(), 413),
            Err(ureq::Error::StatusCode(code)) => assert_eq!(code, 413),
            Err(e) => panic!("unexpected error: {e}"),
        }

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_request_free_drops_connection() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || ureq::get(&format!("{base}/dropped")).call());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        // SAFETY: req was allocated by hew_http_server_recv.
        unsafe { hew_http_request_free(req) };

        let result = handle.join().unwrap();
        assert!(result.is_err(), "dropped request should cause client error");

        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    // -- Bridge function unit tests -----------------------------------

    #[test]
    fn body_string_null_request_returns_null() {
        let encoding = c"utf-8";
        // SAFETY: null request is the tested scenario.
        let result =
            unsafe { hew_http_request_body_string(std::ptr::null_mut(), encoding.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn body_string_consumed_request_returns_null() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let encoding = c"utf-8";
        // SAFETY: req is a valid local struct with inner = None.
        let result = unsafe { hew_http_request_body_string(&raw mut req, encoding.as_ptr()) };
        assert!(result.is_null());
    }

    #[test]
    fn body_string_null_encoding_accepted() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        // SAFETY: null encoding should not crash; consumed request returns null.
        let result = unsafe { hew_http_request_body_string(&raw mut req, std::ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn respond_bridge_null_request_returns_error() {
        let ct = c"text/plain";
        let body = c"hello";
        // SAFETY: null request is the tested scenario.
        let result = unsafe {
            hew_http_respond_bridge(std::ptr::null_mut(), 200, ct.as_ptr(), body.as_ptr())
        };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_bridge_consumed_request_returns_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        let body = c"hello";
        // SAFETY: req is valid with inner = None; all C strings are valid.
        let result =
            unsafe { hew_http_respond_bridge(&raw mut req, 200, ct.as_ptr(), body.as_ptr()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_bridge_null_body_accepted() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let ct = c"text/plain";
        // SAFETY: null body is valid (empty response); consumed request returns -1.
        let result =
            unsafe { hew_http_respond_bridge(&raw mut req, 200, ct.as_ptr(), std::ptr::null()) };
        assert_eq!(result, -1);
    }

    #[test]
    fn respond_bridge_null_content_type_accepted() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
        };
        let body = c"hello";
        // SAFETY: null content_type is valid; consumed request returns -1.
        let result =
            unsafe { hew_http_respond_bridge(&raw mut req, 200, std::ptr::null(), body.as_ptr()) };
        assert_eq!(result, -1);
    }

    // -- Bridge function loopback integration tests -------------------

    #[test]
    fn loopback_body_string_reads_post() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/body-string"))
                .header("Content-Type", "text/plain")
                .send(b"hello bridge" as &[u8])
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let encoding = c"utf-8";
        // SAFETY: req is valid; encoding is a valid C string.
        let body_cstr = unsafe { hew_http_request_body_string(req, encoding.as_ptr()) };
        assert!(!body_cstr.is_null());
        // SAFETY: body_cstr is a valid malloc'd C string.
        let body = unsafe { take_cstr(body_cstr) };
        assert_eq!(body, "hello bridge");

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let result = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_body_string_empty_body() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/body-string-empty"))
                .header("Content-Type", "text/plain")
                .send(b"" as &[u8])
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let encoding = c"utf-8";
        // SAFETY: req is valid; encoding is a valid C string.
        let body_cstr = unsafe { hew_http_request_body_string(req, encoding.as_ptr()) };
        assert!(!body_cstr.is_null());
        // SAFETY: body_cstr is a valid malloc'd C string.
        let body = unsafe { take_cstr(body_cstr) };
        assert_eq!(body, "");

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let result = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_respond_bridge_full_response() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::get(&format!("{base}/bridge-respond")).call().unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let ct = c"text/html";
        let body = c"<h1>Hello</h1>";
        // SAFETY: req, ct, and body are all valid.
        let result = unsafe { hew_http_respond_bridge(req, 200, ct.as_ptr(), body.as_ptr()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 200);
        let ct_header = resp
            .headers()
            .get("Content-Type")
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        assert_eq!(ct_header, "text/html");
        let resp_body = resp.into_body().read_to_string().unwrap();
        assert_eq!(resp_body, "<h1>Hello</h1>");

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_respond_bridge_null_body_sends_empty() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle =
            std::thread::spawn(move || ureq::get(&format!("{base}/bridge-empty")).call().unwrap());

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let ct = c"text/plain";
        // SAFETY: req and ct are valid; null body means empty response.
        let result = unsafe { hew_http_respond_bridge(req, 204, ct.as_ptr(), std::ptr::null()) };
        assert_eq!(result, 0);

        let resp = handle.join().unwrap();
        assert_eq!(resp.status().as_u16(), 204);

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }
}
