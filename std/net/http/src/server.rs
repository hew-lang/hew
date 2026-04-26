//! Hew runtime: synchronous HTTP server.
//!
//! Provides an HTTP server built on [`tiny_http`] that can be driven from
//! compiled Hew programs via the C ABI functions below.
//!
//! Request bodies are fail-closed by a conservative 30s read deadline by
//! default so slow trickle-feed clients cannot hold the server indefinitely.
//! Use [`hew_http_server_set_request_timeout_ms`] to tune that deadline.

use hew_cabi::cabi::{malloc_cstring, str_to_malloc};
use hew_cabi::sink::{into_write_sink_ptr, set_last_error, HewSink};
use hew_cabi::vec::HewVec;
use std::ffi::{c_char, c_void, CStr};
use std::io::{self, Read, Write};
use std::sync::{
    atomic::{AtomicBool, AtomicUsize, Ordering},
    mpsc, Arc, Mutex, PoisonError,
};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

const MAX_BODY_SIZE: usize = 10 * 1024 * 1024;
const DEFAULT_REQUEST_BODY_TIMEOUT: Duration = Duration::from_secs(30);
const RESPONSE_THREAD_POLL_INTERVAL: Duration = Duration::from_millis(50);
const RESPONSE_THREAD_JOIN_TIMEOUT: Duration = Duration::from_secs(2);

/// Opaque HTTP server handle.
pub struct HewHttpServer {
    inner: tiny_http::Server,
    max_body_size: usize,
    request_body_timeout: Duration,
    response_threads: ResponseThreadTracker,
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
    request_body_timeout: Duration,
    response_threads: Option<ResponseThreadTracker>,
}

impl std::fmt::Debug for HewHttpRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewHttpRequest").finish_non_exhaustive()
    }
}

struct TrackedResponseThread {
    done_rx: mpsc::Receiver<()>,
    handle: JoinHandle<()>,
}

#[derive(Clone)]
struct ResponseThreadTracker {
    active_count: Arc<AtomicUsize>,
    cancel: Arc<AtomicBool>,
    threads: Arc<Mutex<Vec<TrackedResponseThread>>>,
}

impl ResponseThreadTracker {
    fn new() -> Self {
        Self {
            active_count: Arc::new(AtomicUsize::new(0)),
            cancel: Arc::new(AtomicBool::new(false)),
            threads: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn cancel_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.cancel)
    }

    fn spawn_response(
        &self,
        request: tiny_http::Request,
        status: u16,
        content_type: Option<String>,
        reader: ChannelReader,
    ) {
        self.reap_completed();
        let (done_tx, done_rx) = mpsc::channel();
        let active_count = Arc::clone(&self.active_count);
        active_count.fetch_add(1, Ordering::AcqRel);
        let handle = thread::spawn(move || {
            struct ActiveCountGuard(Arc<AtomicUsize>);

            impl Drop for ActiveCountGuard {
                fn drop(&mut self) {
                    self.0.fetch_sub(1, Ordering::AcqRel);
                }
            }

            let _active_count = ActiveCountGuard(active_count);
            let mut response = tiny_http::Response::new(
                tiny_http::StatusCode(status),
                Vec::new(),
                reader,
                None, // No Content-Length — tiny_http uses chunked encoding
                None,
            );
            if let Some(ct) = content_type {
                match tiny_http::Header::from_bytes("Content-Type", ct) {
                    Ok(header) => {
                        response = response.with_header(header);
                    }
                    Err(err) => {
                        eprintln!("hew_http: ignoring invalid Content-Type header: {err:?}");
                    }
                }
            }
            if let Err(err) = request.respond(response) {
                eprintln!("hew_http: streaming response thread failed to respond: {err}");
            }
            if done_tx.send(()).is_err() {
                eprintln!("hew_http: response thread completion receiver dropped before join");
            }
        });
        self.threads
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .push(TrackedResponseThread { done_rx, handle });
    }

    fn reap_completed(&self) {
        let completed = {
            let mut threads = self.threads.lock().unwrap_or_else(PoisonError::into_inner);
            let mut completed = Vec::new();
            let mut idx = 0;
            while idx < threads.len() {
                let is_done = match threads[idx].done_rx.try_recv() {
                    Ok(()) | Err(mpsc::TryRecvError::Disconnected) => true,
                    Err(mpsc::TryRecvError::Empty) => false,
                };
                if is_done {
                    completed.push(threads.swap_remove(idx));
                } else {
                    idx += 1;
                }
            }
            completed
        };
        for thread in completed {
            join_response_thread(thread.handle);
        }
    }

    fn cancel_and_join_all(&self) {
        self.cancel.store(true, Ordering::Release);
        let threads =
            std::mem::take(&mut *self.threads.lock().unwrap_or_else(PoisonError::into_inner));
        for thread in threads {
            match thread.done_rx.recv_timeout(RESPONSE_THREAD_JOIN_TIMEOUT) {
                Ok(()) | Err(mpsc::RecvTimeoutError::Disconnected) => {
                    join_response_thread(thread.handle);
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    eprintln!(
                        "hew_http: response thread exceeded {RESPONSE_THREAD_JOIN_TIMEOUT:?} shutdown timeout; detaching"
                    );
                }
            }
        }
    }

    #[cfg(test)]
    fn active_response_count(&self) -> usize {
        self.active_count.load(Ordering::Acquire)
    }

    #[cfg(test)]
    fn tracked_thread_count(&self) -> usize {
        self.threads
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .len()
    }

    #[cfg(test)]
    fn push_tracked_thread(&self, thread: TrackedResponseThread) {
        self.threads
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .push(thread);
    }
}

fn join_response_thread(handle: JoinHandle<()>) {
    if let Err(err) = handle.join() {
        eprintln!("hew_http: response thread panicked: {err:?}");
    }
}

struct DeadlineReader<R> {
    inner: R,
    started_at: Instant,
    timeout: Duration,
}

impl<R> DeadlineReader<R> {
    fn new(inner: R, timeout: Duration) -> Self {
        Self {
            inner,
            started_at: Instant::now(),
            timeout,
        }
    }
}

impl<R: Read> Read for DeadlineReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if self.started_at.elapsed() >= self.timeout {
            return Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "request body read deadline exceeded",
            ));
        }
        self.inner.read(buf)
    }
}

impl Drop for HewHttpServer {
    fn drop(&mut self) {
        self.response_threads.cancel_and_join_all();
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: ResponseThreadTracker::new(),
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
    server.response_threads.reap_completed();

    match server.inner.recv() {
        Ok(req) => Box::into_raw(Box::new(HewHttpRequest {
            inner: Some(req),
            max_body_size: server.max_body_size,
            request_body_timeout: server.request_body_timeout,
            response_threads: Some(server.response_threads.clone()),
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

/// Set the per-request body read deadline (in milliseconds) for future
/// received requests.
///
/// Returns 0 on success, -1 on invalid input.
///
/// # Safety
///
/// `srv` must be a valid pointer to a [`HewHttpServer`] previously returned by
/// [`hew_http_server_new`].
#[no_mangle]
pub unsafe extern "C" fn hew_http_server_set_request_timeout_ms(
    srv: *mut HewHttpServer,
    timeout_ms: i64,
) -> i32 {
    if srv.is_null() || timeout_ms <= 0 {
        return -1;
    }
    let Ok(timeout_ms) = u64::try_from(timeout_ms) else {
        return -1;
    };
    // SAFETY: srv was allocated by hew_http_server_new and is valid.
    let server = unsafe { &mut *srv };
    server.request_body_timeout = Duration::from_millis(timeout_ms);
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
    let mut reader = DeadlineReader::new(inner.as_reader(), request.request_body_timeout);
    if let Err(err) =
        Read::take(&mut reader, request.max_body_size as u64 + 1).read_to_end(&mut buf)
    {
        if err.kind() == io::ErrorKind::TimedOut {
            drop(buf);
            let response = tiny_http::Response::from_string("Request Timeout")
                .with_status_code(tiny_http::StatusCode(408));
            let _ = inner.respond(response);
            return std::ptr::null_mut();
        }
        request.inner = Some(inner);
        return std::ptr::null_mut();
    }
    if buf.len() > request.max_body_size {
        drop(buf);
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
    cancel: Arc<AtomicBool>,
    offset: usize,
}

impl Read for ChannelReader {
    fn read(&mut self, out: &mut [u8]) -> std::io::Result<usize> {
        // Refill from channel when the current buffer is exhausted.
        while self.offset >= self.buf.len() {
            if self.cancel.load(Ordering::Acquire) {
                return Ok(0);
            }
            match self.rx.recv_timeout(RESPONSE_THREAD_POLL_INTERVAL) {
                Ok(data) => {
                    self.buf = data;
                    self.offset = 0;
                }
                Err(mpsc::RecvTimeoutError::Timeout) => {}
                Err(mpsc::RecvTimeoutError::Disconnected) => return Ok(0), // Sender dropped → EOF
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
    cancel: Arc<AtomicBool>,
    tx: Option<mpsc::SyncSender<Vec<u8>>>,
}

impl Write for HttpResponseSink {
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        if self.cancel.load(Ordering::Acquire) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::BrokenPipe,
                "HTTP response sink cancelled during server shutdown",
            ));
        }
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

    let Some(response_threads) = request.response_threads.clone() else {
        request.inner = Some(inner);
        set_last_error("request is missing response thread tracker".into());
        return std::ptr::null_mut();
    };

    // Bounded channel — backpressure if the network is slower than the producer.
    let (tx, rx) = mpsc::sync_channel::<Vec<u8>>(64);
    let cancel = response_threads.cancel_flag();
    let reader = ChannelReader {
        rx,
        buf: Vec::new(),
        cancel: Arc::clone(&cancel),
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

    response_threads.spawn_response(inner, status_u16, ct_string, reader);
    into_write_sink_ptr(HttpResponseSink {
        cancel,
        tx: Some(tx),
    })
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
// Bulk header accessor
// ---------------------------------------------------------------------------

/// ABI layout for a `(String, String)` tuple element in a Hew `Vec<(String, String)>`.
///
/// Both pointers are `malloc`-allocated and must be freed by the owner. Hew's
/// compiled destructor handles this automatically; Rust callers must free them
/// manually before calling `hew_vec_free`.
#[repr(C)]
struct HewStringPair {
    name: *mut c_char,
    value: *mut c_char,
}

/// Return a new `Vec<(String, String)>` containing all headers from `req`.
///
/// Each element is a `(name, value)` pair of `malloc`-allocated C strings.
/// The caller owns the returned vector and its element strings. Hew's compiled
/// destructor frees the string fields when the `Vec<(String, String)>` goes
/// out of scope. Returns an empty vector if `req` is null or has no inner
/// request; never returns null.
///
/// # Safety
///
/// `req` must be a valid [`HewHttpRequest`] pointer, or null.
///
/// # Panics
///
/// In practice never panics. The internal conversion of
/// `size_of::<*mut c_char>() * 2` to `i64` is infallible on any supported
/// platform (pointer sizes are always a small fraction of `i64::MAX`).
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_headers(req: *const HewHttpRequest) -> *mut HewVec {
    let elem_size = i64::try_from(2 * std::mem::size_of::<*mut c_char>())
        .expect("pointer-pair elem_size always fits i64");
    // SAFETY: allocates a new HewVec with elem_size=16 (two pointers), Plain kind.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_generic(elem_size, 0) };
    if req.is_null() {
        return vec;
    }
    // SAFETY: req is a valid HewHttpRequest per caller contract.
    let request = unsafe { &*req };
    let Some(inner) = request.inner.as_ref() else {
        return vec;
    };
    for header in inner.headers() {
        let pair = HewStringPair {
            name: str_to_malloc(header.field.as_str().as_str()),
            value: str_to_malloc(header.value.as_str()),
        };
        // SAFETY: vec is a valid HewVec; &pair is a valid elem_size-byte region.
        unsafe {
            hew_cabi::vec::hew_vec_push_generic(vec, std::ptr::addr_of!(pair).cast::<c_void>());
        }
    }
    vec
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{Shutdown, SocketAddr, TcpStream};

    fn wait_for_condition(
        description: &str,
        timeout: Duration,
        mut condition: impl FnMut() -> bool,
    ) {
        let start = Instant::now();
        while start.elapsed() < timeout {
            if condition() {
                return;
            }
            std::thread::sleep(Duration::from_millis(5));
        }
        assert!(condition(), "{description} within {timeout:?}");
    }

    #[test]
    fn debug_impls_compile() {
        fn assert_debug<T: std::fmt::Debug>() {}

        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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

    // -- set_request_timeout_ms ---------------------------------------

    #[test]
    fn set_request_timeout_ms_null_server_returns_error() {
        // SAFETY: null pointer is the tested scenario.
        let result = unsafe { hew_http_server_set_request_timeout_ms(std::ptr::null_mut(), 1000) };
        assert_eq!(result, -1);
    }

    #[test]
    fn set_request_timeout_ms_zero_returns_error() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 0 is an invalid timeout.
        let result = unsafe { hew_http_server_set_request_timeout_ms(srv, 0) };
        assert_eq!(result, -1, "zero timeout should be rejected");
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn set_request_timeout_ms_negative_returns_error() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; -1 is invalid.
        let result = unsafe { hew_http_server_set_request_timeout_ms(srv, -1) };
        assert_eq!(result, -1, "negative timeout should be rejected");
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn set_request_timeout_ms_valid_returns_success() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 250 is a valid timeout.
        let result = unsafe { hew_http_server_set_request_timeout_ms(srv, 250) };
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
        };
        // SAFETY: out_len is null (tested scenario).
        let result = unsafe { hew_http_request_body(&raw mut req, std::ptr::null_mut()) };
        assert!(result.is_null());
    }

    // -- Request headers() accessor -----------------------------------

    #[test]
    fn request_headers_null_req_returns_empty_vec() {
        // SAFETY: null is explicitly handled; returns a valid empty vec.
        let vec = unsafe { hew_http_request_headers(std::ptr::null()) };
        assert!(!vec.is_null());
        // SAFETY: vec was just allocated by hew_http_request_headers.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(len, 0);
        // SAFETY: vec is a valid HewVec; no string elements to free (ElemKind::Plain, len=0).
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn request_headers_consumed_req_returns_empty_vec() {
        let req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
        };
        // SAFETY: req is a valid local struct with inner = None.
        let vec = unsafe { hew_http_request_headers(&raw const req) };
        assert!(!vec.is_null());
        // SAFETY: vec was allocated by hew_http_request_headers.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(len, 0);
        // SAFETY: vec is a valid HewVec; no elements to free.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn loopback_request_headers_single_pair_roundtrip() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::get(&format!("{base}/headers-single"))
                .header("X-Custom", "hello")
                .call()
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        // SAFETY: req is valid.
        let vec = unsafe { hew_http_request_headers(req) };
        assert!(!vec.is_null());
        // SAFETY: vec is valid.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert!(len >= 1, "expected at least the X-Custom header");

        // Find the X-Custom header among the returned pairs.
        let mut found = false;
        for i in 0..len {
            // SAFETY: vec is valid; i is in bounds.
            let elem_ptr = unsafe { hew_cabi::vec::hew_vec_get_generic(vec, i) };
            assert!(!elem_ptr.is_null());
            // SAFETY: elem_ptr points to a HewStringPair (two consecutive *mut c_char).
            let pair = unsafe { &*(elem_ptr.cast::<HewStringPair>()) };
            assert!(!pair.name.is_null());
            assert!(!pair.value.is_null());
            // SAFETY: pair.name is a valid malloc'd C string from hew_http_request_headers.
            let name = unsafe { CStr::from_ptr(pair.name) }
                .to_str()
                .unwrap()
                .to_owned();
            // SAFETY: pair.value is a valid malloc'd C string from hew_http_request_headers.
            let value = unsafe { CStr::from_ptr(pair.value) }
                .to_str()
                .unwrap()
                .to_owned();
            if name.eq_ignore_ascii_case("x-custom") {
                assert_eq!(value, "hello");
                found = true;
            }
            // SAFETY: pair.name was malloc'd by hew_http_request_headers.
            unsafe { libc::free(pair.name.cast()) };
            // SAFETY: pair.value was malloc'd by hew_http_request_headers.
            unsafe { libc::free(pair.value.cast()) };
        }
        assert!(found, "X-Custom header not found in request headers");

        // SAFETY: vec elements have been freed; vec itself is still valid.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let result = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        assert_eq!(result, 0);
        handle.join().unwrap();

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_request_headers_multiple_pairs_order_preserved() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string literal.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let base = server_addr(srv);

        let handle = std::thread::spawn(move || {
            ureq::get(&format!("{base}/headers-multi"))
                .header("X-First", "alpha")
                .header("X-Second", "beta")
                .call()
                .unwrap()
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        // SAFETY: req is valid.
        let vec = unsafe { hew_http_request_headers(req) };
        assert!(!vec.is_null());
        // SAFETY: vec is valid.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert!(len >= 2, "expected at least X-First and X-Second headers");

        let mut pairs: Vec<(String, String)> = Vec::new();
        for i in 0..len {
            // SAFETY: vec is valid; i is in bounds.
            let elem_ptr = unsafe { hew_cabi::vec::hew_vec_get_generic(vec, i) };
            assert!(!elem_ptr.is_null());
            // SAFETY: elem_ptr points to a HewStringPair.
            let pair = unsafe { &*(elem_ptr.cast::<HewStringPair>()) };
            assert!(!pair.name.is_null());
            assert!(!pair.value.is_null());
            // SAFETY: pair.name is a valid malloc'd C string from hew_http_request_headers.
            let name = unsafe { CStr::from_ptr(pair.name) }
                .to_str()
                .unwrap()
                .to_owned();
            // SAFETY: pair.value is a valid malloc'd C string from hew_http_request_headers.
            let value = unsafe { CStr::from_ptr(pair.value) }
                .to_str()
                .unwrap()
                .to_owned();
            pairs.push((name, value));
            // SAFETY: pair.name was malloc'd by hew_http_request_headers.
            unsafe { libc::free(pair.name.cast()) };
            // SAFETY: pair.value was malloc'd by hew_http_request_headers.
            unsafe { libc::free(pair.value.cast()) };
        }

        let first_pos = pairs
            .iter()
            .position(|(k, _)| k.eq_ignore_ascii_case("x-first"));
        let second_pos = pairs
            .iter()
            .position(|(k, _)| k.eq_ignore_ascii_case("x-second"));
        assert!(first_pos.is_some(), "X-First header not found");
        assert!(second_pos.is_some(), "X-Second header not found");
        assert_eq!(
            pairs
                .iter()
                .find(|(k, _)| k.eq_ignore_ascii_case("x-first"))
                .unwrap()
                .1,
            "alpha"
        );
        assert_eq!(
            pairs
                .iter()
                .find(|(k, _)| k.eq_ignore_ascii_case("x-second"))
                .unwrap()
                .1,
            "beta"
        );
        assert!(
            first_pos.unwrap() < second_pos.unwrap(),
            "X-First must appear before X-Second"
        );

        // SAFETY: vec elements have been freed; vec itself is still valid.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let result = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        assert_eq!(result, 0);
        handle.join().unwrap();

        // SAFETY: req was already responded to.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    // -- respond_text and respond_json null text ----------------------

    #[test]
    fn respond_text_null_text_returns_error() {
        let mut req = HewHttpRequest {
            inner: None,
            max_body_size: MAX_BODY_SIZE,
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
        format!("http://{}", server_socket_addr(srv))
    }

    fn server_socket_addr(srv: *mut HewHttpServer) -> SocketAddr {
        // SAFETY: srv is valid and was just created.
        let server = unsafe { &*srv };
        server.inner.server_addr().to_ip().unwrap()
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
    fn server_close_cancels_streaming_responses_and_drains_threads() {
        const CANCEL_SLO: Duration = Duration::from_secs(1);

        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let tracker = {
            // SAFETY: srv is valid until hew_http_server_close consumes it below.
            let server = unsafe { &*srv };
            server.response_threads.clone()
        };
        let base = server_addr(srv);
        let (done_tx, done_rx) = mpsc::channel();

        let client = std::thread::spawn(move || {
            let started = std::time::Instant::now();
            let result = ureq::get(&format!("{base}/stream-close")).call().unwrap();
            let body = result.into_body().read_to_string();
            done_tx
                .send((started.elapsed(), body))
                .expect("server-close test receiver must still be present");
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let ct = c"text/plain";
        // SAFETY: req and ct are valid.
        let sink = unsafe { hew_http_respond_stream(req, 200, ct.as_ptr()) };
        assert!(!sink.is_null(), "respond_stream should return a valid sink");
        assert_eq!(
            tracker.active_response_count(),
            1,
            "streaming response thread should be tracked while active"
        );

        // SAFETY: req is valid after respond_stream consumes its inner request.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };

        let (elapsed, body_result) = done_rx
            .recv_timeout(CANCEL_SLO)
            .expect("client should observe server-close cancellation within the SLO");
        assert!(
            elapsed <= CANCEL_SLO,
            "streaming response should end within {CANCEL_SLO:?}, got {elapsed:?}"
        );
        let body = body_result.expect("cancelled stream should still finish cleanly");
        assert_eq!(body, "");
        assert_eq!(
            tracker.active_response_count(),
            0,
            "all tracked streaming response threads must be gone after server drop"
        );

        // SAFETY: sink was allocated by into_sink_ptr.
        drop(unsafe { Box::from_raw(sink) });
        client.join().expect("client thread should not panic");
    }

    #[test]
    fn server_close_cancels_concurrent_streaming_responses_and_reaps_completed_threads() {
        const STREAM_COUNT: usize = 4;
        const CANCEL_SLO: Duration = Duration::from_secs(2);

        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let tracker = {
            // SAFETY: srv is valid until hew_http_server_close consumes it below.
            let server = unsafe { &*srv };
            server.response_threads.clone()
        };
        let base = server_addr(srv);

        let clients: Vec<_> = (0..STREAM_COUNT)
            .map(|idx| {
                let base = base.clone();
                std::thread::spawn(move || {
                    ureq::get(&format!("{base}/stream-load/{idx}"))
                        .call()
                        .unwrap()
                        .into_body()
                        .read_to_string()
                        .unwrap()
                })
            })
            .collect();

        let mut sinks = Vec::with_capacity(STREAM_COUNT);
        for _ in 0..STREAM_COUNT {
            // SAFETY: srv is valid until hew_http_server_close consumes it below.
            let req = unsafe { hew_http_server_recv(srv) };
            assert!(!req.is_null());
            let ct = c"text/plain";
            // SAFETY: req and ct are valid.
            let sink = unsafe { hew_http_respond_stream(req, 200, ct.as_ptr()) };
            assert!(!sink.is_null(), "respond_stream should return a valid sink");
            sinks.push(sink);
            // SAFETY: req is valid after respond_stream consumes its inner request.
            unsafe { hew_http_request_free(req) };
        }

        wait_for_condition("all response threads tracked", CANCEL_SLO, || {
            tracker.active_response_count() == STREAM_COUNT
        });
        assert_eq!(
            tracker.tracked_thread_count(),
            STREAM_COUNT,
            "all streaming response threads should be tracked before shutdown"
        );

        let completed_sink = sinks.pop().expect("one sink should be available");
        // SAFETY: completed_sink was allocated by into_sink_ptr.
        drop(unsafe { Box::from_raw(completed_sink) });

        wait_for_condition("one stream completes before reap", CANCEL_SLO, || {
            tracker.active_response_count() == STREAM_COUNT - 1
        });
        assert_eq!(
            tracker.tracked_thread_count(),
            STREAM_COUNT,
            "completed threads stay queued until reap_completed runs"
        );

        tracker.reap_completed();
        assert_eq!(
            tracker.tracked_thread_count(),
            STREAM_COUNT - 1,
            "reap_completed should remove finished response threads while others are still active"
        );

        let started = Instant::now();
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
        let close_elapsed = started.elapsed();
        assert!(
            close_elapsed <= CANCEL_SLO,
            "server close should finish within {CANCEL_SLO:?}, got {close_elapsed:?}"
        );

        wait_for_condition("remaining response threads cancel", CANCEL_SLO, || {
            tracker.active_response_count() == 0
        });
        assert_eq!(
            tracker.tracked_thread_count(),
            0,
            "cancel_and_join_all should drain the tracked thread list"
        );

        for sink in sinks {
            // SAFETY: sink was allocated by into_sink_ptr.
            drop(unsafe { Box::from_raw(sink) });
        }
        for client in clients {
            assert_eq!(client.join().unwrap(), "");
        }
    }

    #[test]
    fn server_close_detaches_hung_response_thread_after_timeout() {
        const DETACH_EPSILON: Duration = Duration::from_millis(400);

        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        let tracker = {
            // SAFETY: srv is valid until hew_http_server_close consumes it below.
            let server = unsafe { &*srv };
            server.response_threads.clone()
        };

        let release = Arc::new(AtomicBool::new(false));
        let release_for_thread = Arc::clone(&release);
        let active_count = Arc::clone(&tracker.active_count);
        let (done_tx, done_rx) = mpsc::channel();
        active_count.fetch_add(1, Ordering::AcqRel);
        let handle = std::thread::spawn(move || {
            struct ActiveCountGuard(Arc<AtomicUsize>);

            impl Drop for ActiveCountGuard {
                fn drop(&mut self) {
                    self.0.fetch_sub(1, Ordering::AcqRel);
                }
            }

            let _active_count = ActiveCountGuard(active_count);
            while !release_for_thread.load(Ordering::Acquire) {
                std::thread::park_timeout(Duration::from_millis(5));
            }
            let _ = done_tx.send(());
        });
        tracker.push_tracked_thread(TrackedResponseThread { done_rx, handle });

        let started = Instant::now();
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
        let close_elapsed = started.elapsed();
        let close_limit = RESPONSE_THREAD_JOIN_TIMEOUT + DETACH_EPSILON;
        assert!(
            close_elapsed <= close_limit,
            "server close should return within {close_limit:?}, got {close_elapsed:?}"
        );
        assert_eq!(
            tracker.tracked_thread_count(),
            0,
            "detach path should empty the tracked thread list even when a thread outlives close"
        );
        assert_eq!(
            tracker.active_response_count(),
            1,
            "detached response thread should still be running immediately after close"
        );

        release.store(true, Ordering::Release);
        wait_for_condition(
            "detached response thread exits after release",
            Duration::from_secs(1),
            || tracker.active_response_count() == 0,
        );
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
    fn loopback_request_body_timeout_returns_408() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 100ms is a valid timeout.
        let result = unsafe { hew_http_server_set_request_timeout_ms(srv, 100) };
        assert_eq!(result, 0);
        let socket_addr = server_socket_addr(srv);

        let handle = std::thread::spawn(move || {
            let mut stream = TcpStream::connect(socket_addr).unwrap();
            stream
                .set_read_timeout(Some(Duration::from_secs(1)))
                .unwrap();
            stream
                .write_all(
                    b"POST /slow HTTP/1.1\r\nHost: localhost\r\nContent-Length: 2048\r\n\r\nx",
                )
                .unwrap();
            std::thread::sleep(Duration::from_millis(150));
            let _ = stream.write_all(b"y");
            let _ = stream.shutdown(Shutdown::Write);

            let mut response = String::new();
            let _ = stream.read_to_string(&mut response);
            response
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let mut out_len: usize = usize::MAX;
        let start = Instant::now();
        // SAFETY: req and out_len are valid.
        let body_ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
        let elapsed = start.elapsed();
        if !body_ptr.is_null() {
            // SAFETY: body_ptr was malloc'd.
            unsafe { libc::free(body_ptr.cast()) };
            let text = c"late";
            // SAFETY: req is valid; text is a valid C string.
            let _ = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };
        }

        let response = handle.join().unwrap();
        assert!(body_ptr.is_null(), "timed out body should be rejected");
        assert!(
            response.starts_with("HTTP/1.1 408") || response.starts_with("HTTP/1.0 408"),
            "expected 408 response, got {response:?}"
        );
        assert!(response.contains("Request Timeout"));
        assert!(
            elapsed < Duration::from_millis(350),
            "deadline should stop slow body reads early, got {elapsed:?}"
        );

        // SAFETY: req and srv are valid.
        unsafe { hew_http_request_free(req) };
        // SAFETY: srv was allocated by hew_http_server_new.
        unsafe { hew_http_server_close(srv) };
    }

    #[test]
    fn loopback_request_body_within_timeout_succeeds() {
        let addr = c"127.0.0.1:0";
        // SAFETY: addr is a valid C string.
        let srv = unsafe { hew_http_server_new(addr.as_ptr()) };
        assert!(!srv.is_null());
        // SAFETY: srv is valid; 250ms is a valid timeout.
        let result = unsafe { hew_http_server_set_request_timeout_ms(srv, 250) };
        assert_eq!(result, 0);
        let base = server_addr(srv);
        let body = vec![b'a'; 2048];

        let handle = std::thread::spawn(move || {
            ureq::post(&format!("{base}/timely"))
                .header("Content-Type", "text/plain")
                .send(body.as_slice())
        });

        // SAFETY: srv is valid.
        let req = unsafe { hew_http_server_recv(srv) };
        assert!(!req.is_null());

        let mut out_len: usize = 0;
        // SAFETY: req and out_len are valid.
        let body_ptr = unsafe { hew_http_request_body(req, &raw mut out_len) };
        assert!(!body_ptr.is_null());
        assert_eq!(out_len, 2048);
        // SAFETY: body_ptr points to `out_len` bytes allocated by libc::malloc.
        let body = unsafe { std::slice::from_raw_parts(body_ptr, out_len) };
        assert_eq!(body, vec![b'a'; 2048].as_slice());
        // SAFETY: body_ptr was malloc'd.
        unsafe { libc::free(body_ptr.cast()) };

        let text = c"ok";
        // SAFETY: req is valid; text is a valid C string.
        let _ = unsafe { hew_http_respond_text(req, 200, text.as_ptr()) };

        let client_result = handle.join().unwrap();
        match client_result {
            Ok(resp) => assert_eq!(resp.status().as_u16(), 200),
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
            request_body_timeout: DEFAULT_REQUEST_BODY_TIMEOUT,
            response_threads: None,
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
