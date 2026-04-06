//! Hew runtime: `http_client` module.
//!
//! Provides basic HTTP client functionality for compiled Hew programs.
//! All returned strings and response structs are allocated with `libc::malloc`
//! / `Box` so callers can free them with the corresponding free function.

// WASM-TODO: std::net::http outbound client requests remain native-only until
// Hew has a browser/WASM networking bridge.

use hew_cabi::{
    cabi::{cstr_to_str, str_to_malloc},
    vec::{ElemKind, HewVec},
};
use std::ffi::{c_void, CStr};
use std::os::raw::c_char;
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::Duration;

/// Global timeout for all HTTP requests, in milliseconds. Default: 30 000 ms.
static HTTP_TIMEOUT_MS: AtomicI32 = AtomicI32::new(30_000);

/// Response from an HTTP request.
///
/// Returned by [`hew_http_get`], [`hew_http_post`], and the request helpers.
/// Must be freed with [`hew_http_response_free`].
#[repr(C)]
#[derive(Debug)]
pub struct HewHttpResponse {
    /// HTTP status code, or -1 on network/transport error.
    pub status_code: i32,
    /// Response body (NUL-terminated, allocated with `malloc`). Caller frees
    /// the entire struct via `hew_http_response_free`.
    pub body: *mut c_char,
    /// Length of body in bytes (not counting NUL terminator).
    pub body_len: usize,
    /// Captured response headers (heap-allocated `Box<Vec<(String, String)>>`).
    /// May be null if no headers were captured.
    pub headers: *mut Vec<(String, String)>,
}

/// Collect all response headers into a heap-allocated `Vec<(String, String)>`.
fn capture_headers(map: &ureq::http::HeaderMap) -> *mut Vec<(String, String)> {
    let pairs: Vec<(String, String)> = map
        .iter()
        .filter_map(|(name, value)| {
            value
                .to_str()
                .ok()
                .map(|v| (name.to_string(), v.to_string()))
        })
        .collect();
    Box::into_raw(Box::new(pairs))
}

/// Build a [`HewHttpResponse`] from a Rust string body and captured headers.
fn build_response(
    status_code: i32,
    body: &str,
    headers: *mut Vec<(String, String)>,
) -> *mut HewHttpResponse {
    let body_len = body.len();
    let body_ptr = str_to_malloc(body);
    Box::into_raw(Box::new(HewHttpResponse {
        status_code,
        body: body_ptr,
        body_len,
        headers,
    }))
}

/// Build an error response with `status_code = -1` and no headers.
fn error_response(msg: &str) -> *mut HewHttpResponse {
    build_response(-1, msg, std::ptr::null_mut())
}

/// Free a C string allocated by `strdup`/`malloc` if present.
unsafe fn free_c_string(ptr: *const c_char) {
    if !ptr.is_null() {
        // SAFETY: ptr was allocated by strdup/malloc and is owned by the caller.
        unsafe { libc::free(ptr.cast_mut().cast()) };
    }
}

/// Parse a `"Header-Name: value"` line into an owned `(name, value)` pair.
fn parse_header_line(header: &str) -> Option<(String, String)> {
    let (name, value) = header.split_once(':')?;
    Some((name.trim().to_string(), value.trim().to_string()))
}

/// Parse a null-terminated C string array of header lines into owned header pairs.
unsafe fn parse_headers_from_c_array(
    headers: *const *const c_char,
    header_count: i32,
) -> Vec<(String, String)> {
    let mut parsed_headers: Vec<(String, String)> = Vec::new();
    if headers.is_null() || header_count <= 0 {
        return parsed_headers;
    }

    for i in 0..usize::try_from(header_count).unwrap_or(0) {
        // SAFETY: headers is a valid array of header_count C string pointers per
        // caller contract; i < header_count so the pointer arithmetic is in bounds.
        let header_ptr = unsafe { *headers.add(i) };
        if header_ptr.is_null() {
            continue;
        }
        // SAFETY: each non-null entry is a valid NUL-terminated C string per caller.
        let Ok(header_text) = unsafe { CStr::from_ptr(header_ptr) }.to_str() else {
            continue;
        };
        if let Some((name, value)) = parse_header_line(header_text) {
            parsed_headers.push((name, value));
        }
    }

    parsed_headers
}

/// Parse a Hew `Vec<(String, String)>` of header pairs into owned `(name, value)` pairs.
///
/// Fails closed: returns an error if `headers` is not a Plain-kind vector with the
/// expected element size for a `(String, String)` pair, or if any name or value
/// pointer is null or contains invalid UTF-8.
unsafe fn parse_headers_from_hew_tuple_vec(
    headers: *mut HewVec,
) -> Result<Vec<(String, String)>, String> {
    if headers.is_null() {
        return Ok(Vec::new());
    }

    // SAFETY: caller guarantees headers is a valid HewVec pointer.
    let headers_ref = unsafe { &*headers };
    if headers_ref.elem_kind != ElemKind::Plain {
        return Err("request headers must be Vec<(String, String)>".to_string());
    }
    let expected_elem_size = 2 * std::mem::size_of::<*mut c_char>();
    if headers_ref.elem_size as usize != expected_elem_size {
        return Err(format!(
            "request headers Vec element size mismatch: expected {expected_elem_size}, got {}",
            headers_ref.elem_size
        ));
    }

    let mut parsed_headers = Vec::with_capacity(headers_ref.len);
    for index in 0..headers_ref.len {
        let Ok(index_i64) = i64::try_from(index) else {
            return Err("headers length exceeds Hew index range".to_string());
        };
        // SAFETY: index_i64 is a valid in-bounds index for this HewVec.
        let elem_ptr =
            unsafe { hew_cabi::vec::hew_vec_get_generic(headers as *const HewVec, index_i64) };
        if elem_ptr.is_null() {
            continue;
        }
        // SAFETY: elem_ptr points to a HewStringPair within the vector's element buffer.
        // The pair's string pointers are owned by the vector; we read but do not free them.
        let pair = unsafe { &*(elem_ptr.cast::<HewStringPair>()) };
        if pair.name.is_null() || pair.value.is_null() {
            return Err("malformed header pair: null name or value pointer".to_string());
        }
        // SAFETY: pair.name is a valid NUL-terminated C string owned by the vector.
        let name = unsafe { cstr_to_str(pair.name) }
            .map(str::to_owned)
            .ok_or_else(|| "malformed header pair: invalid UTF-8 in name".to_string())?;
        // SAFETY: pair.value is a valid NUL-terminated C string owned by the vector.
        let value = unsafe { cstr_to_str(pair.value) }
            .map(str::to_owned)
            .ok_or_else(|| "malformed header pair: invalid UTF-8 in value".to_string())?;
        parsed_headers.push((name, value));
    }

    Ok(parsed_headers)
}

/// Set the global timeout applied to all subsequent HTTP requests.
///
/// Pass `timeout_ms = 0` to disable the timeout. The default is 30 000 ms.
///
/// # Safety
///
/// This function is safe to call from any thread.
#[no_mangle]
pub unsafe extern "C" fn hew_http_set_timeout(timeout_ms: i32) {
    HTTP_TIMEOUT_MS.store(timeout_ms, Ordering::Relaxed);
}

/// Build a configured [`ureq::Agent`] using the current global timeout.
fn make_agent() -> ureq::Agent {
    let raw = HTTP_TIMEOUT_MS.load(Ordering::Relaxed).max(0);
    // Casting i32 → u32 is safe here because .max(0) guarantees non-negative.
    let ms = u64::from(raw.cast_unsigned());
    let config = ureq::Agent::config_builder()
        .timeout_global(if ms > 0 {
            Some(Duration::from_millis(ms))
        } else {
            None
        })
        .build();
    ureq::Agent::new_with_config(config)
}

/// Parse a response, capturing headers and mapping body-read errors to an
/// error response.
fn finish_response(mut resp: ureq::http::Response<ureq::Body>) -> *mut HewHttpResponse {
    let status = resp.status().as_u16();
    let headers = capture_headers(resp.headers());
    match resp.body_mut().read_to_string() {
        Ok(body) => build_response(i32::from(status), &body, headers),
        Err(e) => {
            if !headers.is_null() {
                // SAFETY: headers was just allocated with Box::into_raw.
                drop(unsafe { Box::from_raw(headers) });
            }
            error_response(&format!("failed to read response body: {e}"))
        }
    }
}

/// Execute an outbound HTTP request with the shared timeout/configuration path.
fn send_request(
    method: &str,
    url: &str,
    body: Option<&[u8]>,
    headers: &[(String, String)],
) -> *mut HewHttpResponse {
    let agent = make_agent();
    let method_upper = method.to_uppercase();

    match method_upper.as_str() {
        "GET" | "HEAD" | "DELETE" => {
            let req = match method_upper.as_str() {
                "HEAD" => agent.head(url),
                "DELETE" => agent.delete(url),
                _ => agent.get(url),
            };
            let req = headers
                .iter()
                .fold(req, |request, (name, value)| request.header(name, value));
            match req.call() {
                Ok(resp) => finish_response(resp),
                Err(ureq::Error::StatusCode(code)) => {
                    build_response(i32::from(code), "", std::ptr::null_mut())
                }
                Err(e) => error_response(&e.to_string()),
            }
        }
        "POST" | "PUT" | "PATCH" => {
            let req = match method_upper.as_str() {
                "PUT" => agent.put(url),
                "PATCH" => agent.patch(url),
                _ => agent.post(url),
            };
            let req = headers
                .iter()
                .fold(req, |request, (name, value)| request.header(name, value));
            match req.send(body.unwrap_or(b"")) {
                Ok(resp) => finish_response(resp),
                Err(ureq::Error::StatusCode(code)) => {
                    build_response(i32::from(code), "", std::ptr::null_mut())
                }
                Err(e) => error_response(&e.to_string()),
            }
        }
        _ => error_response(&format!("unsupported HTTP method: {method}")),
    }
}

/// Parse C ABI request inputs and execute them through [`send_request`].
unsafe fn request_from_c_parts(
    method: *const c_char,
    url: *const c_char,
    body: *const c_char,
    headers: &[(String, String)],
) -> *mut HewHttpResponse {
    if method.is_null() || url.is_null() {
        return error_response("invalid argument");
    }
    // SAFETY: method and url are valid NUL-terminated C strings per caller contract.
    let method_str = match unsafe { CStr::from_ptr(method) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in method: {e}")),
    };
    // SAFETY: url is a valid NUL-terminated C string per caller contract.
    let url_str = match unsafe { CStr::from_ptr(url) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in URL: {e}")),
    };
    let body_bytes = if body.is_null() {
        None
    } else {
        // SAFETY: body is a valid NUL-terminated C string per caller contract.
        Some(unsafe { CStr::from_ptr(body) }.to_bytes())
    };

    send_request(method_str, url_str, body_bytes, headers)
}

/// Make an HTTP GET request.
///
/// Returns a heap-allocated [`HewHttpResponse`]. The caller must free it with
/// [`hew_http_response_free`].
///
/// # Safety
///
/// `url` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_get(url: *const c_char) -> *mut HewHttpResponse {
    if url.is_null() {
        return error_response("invalid argument");
    }
    // SAFETY: url is a valid NUL-terminated C string per caller contract.
    let url_str = match unsafe { CStr::from_ptr(url) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in URL: {e}")),
    };

    send_request("GET", url_str, None, &[])
}

/// Make an HTTP POST request with a body.
///
/// Returns a heap-allocated [`HewHttpResponse`]. The caller must free it with
/// [`hew_http_response_free`].
///
/// # Safety
///
/// `url`, `content_type`, and `body` must all be valid NUL-terminated C strings
/// (or null, which is treated as an invalid argument).
#[no_mangle]
pub unsafe extern "C" fn hew_http_post(
    url: *const c_char,
    content_type: *const c_char,
    body: *const c_char,
) -> *mut HewHttpResponse {
    if url.is_null() || content_type.is_null() || body.is_null() {
        return error_response("invalid argument");
    }
    // SAFETY: url is a valid NUL-terminated C string per caller contract.
    let url_str = match unsafe { CStr::from_ptr(url) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in URL: {e}")),
    };
    // SAFETY: content_type is a valid NUL-terminated C string per caller contract.
    let content_type_str = match unsafe { CStr::from_ptr(content_type) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in content-type: {e}")),
    };
    // SAFETY: body is a valid NUL-terminated C string per caller contract.
    let body_bytes = unsafe { CStr::from_ptr(body) }.to_bytes();
    let headers = vec![("Content-Type".to_string(), content_type_str.to_string())];

    send_request("POST", url_str, Some(body_bytes), &headers)
}

/// Perform an HTTP request with a configurable method, URL, optional body,
/// and optional headers.
///
/// - `method` — `"GET"`, `"POST"`, `"PUT"`, `"DELETE"`, `"PATCH"`, or
///   `"HEAD"` (case-insensitive).
/// - `body` — may be null; ignored for GET / HEAD / DELETE.
/// - `headers` — null-terminated array of `"Key: Value"` strings, or null.
/// - `header_count` — number of entries in `headers`; ignored when `headers`
///   is null.
///
/// Returns a heap-allocated [`HewHttpResponse`]. The caller must free it with
/// [`hew_http_response_free`].
///
/// # Safety
///
/// - `method` and `url` must be valid NUL-terminated C strings.
/// - If `body` is non-null it must be a valid NUL-terminated C string.
/// - If `headers` is non-null it must point to at least `header_count` valid
///   NUL-terminated C string pointers.
#[no_mangle]
pub unsafe extern "C" fn hew_http_request(
    method: *const c_char,
    url: *const c_char,
    body: *const c_char,
    headers: *const *const c_char,
    header_count: i32,
) -> *mut HewHttpResponse {
    // SAFETY: headers/header_count follow this function's documented raw-array ABI.
    let parsed_headers = unsafe { parse_headers_from_c_array(headers, header_count) };
    // SAFETY: method/url/body follow the existing C ABI contract for this function.
    unsafe { request_from_c_parts(method, url, body, &parsed_headers) }
}

/// Hew-facing wrapper for [`hew_http_request`] that accepts `Vec<(String, String)>` headers.
///
/// # Safety
///
/// `method`, `url`, and `body` follow the same string rules as
/// [`hew_http_request`]. `headers` must be null or a valid `Vec<(String, String)>` handle
/// (Plain-kind HewVec with element size matching two pointer-width fields).
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_hew(
    method: *const c_char,
    url: *const c_char,
    body: *const c_char,
    headers: *mut HewVec,
) -> *mut HewHttpResponse {
    // SAFETY: headers follows this function's documented Vec<(String, String)> ABI contract.
    let parsed_headers = match unsafe { parse_headers_from_hew_tuple_vec(headers) } {
        Ok(parsed_headers) => parsed_headers,
        Err(message) => return error_response(&message),
    };
    // SAFETY: method/url/body follow the same string ABI contract here.
    unsafe { request_from_c_parts(method, url, body, &parsed_headers) }
}

/// Free a [`HewHttpResponse`] previously returned by [`hew_http_get`],
/// [`hew_http_post`], or [`hew_http_request`].
///
/// # Safety
///
/// `resp` must be a pointer previously returned by `hew_http_get`,
/// `hew_http_post`, or `hew_http_request`, and must not have been freed already.
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_free(resp: *mut HewHttpResponse) {
    if resp.is_null() {
        return;
    }
    // SAFETY: resp was allocated with Box::into_raw in build_response.
    let response = unsafe { Box::from_raw(resp) };
    if !response.body.is_null() {
        // SAFETY: body was allocated with libc::malloc in str_to_malloc.
        unsafe { libc::free(response.body.cast()) };
    }
    if !response.headers.is_null() {
        // SAFETY: headers was allocated with Box::into_raw in capture_headers.
        drop(unsafe { Box::from_raw(response.headers) });
    }
    // Box is dropped here, freeing the HewHttpResponse struct.
}

// ── Response accessor functions ───────────────────────────────────────

/// Get the HTTP status code from a response.
///
/// Returns -1 if `resp` is null.
///
/// # Safety
///
/// `resp` must be a valid [`HewHttpResponse`] pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_status(resp: *const HewHttpResponse) -> i32 {
    if resp.is_null() {
        return -1;
    }
    // SAFETY: resp is a valid HewHttpResponse per caller contract.
    unsafe { (*resp).status_code }
}

/// Get a copy of the response body as a malloc-allocated C string.
///
/// The caller must free the returned string with `libc::free`. Returns null if
/// `resp` is null.
///
/// # Safety
///
/// `resp` must be a valid [`HewHttpResponse`] pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_body(resp: *const HewHttpResponse) -> *mut c_char {
    if resp.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: resp is a valid HewHttpResponse per caller contract.
    let r = unsafe { &*resp };
    if r.body.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: body is a valid NUL-terminated C string from str_to_malloc.
    unsafe { libc::strdup(r.body) }
}

/// Look up a response header by name (case-insensitive).
///
/// Returns a malloc-allocated C string. If the header is not present the
/// returned string is empty (not null). Returns null if `resp` or `name` is
/// null.
///
/// # Safety
///
/// `resp` must be a valid [`HewHttpResponse`] pointer, or null.
/// `name` must be a valid NUL-terminated C string, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_header(
    resp: *const HewHttpResponse,
    name: *const c_char,
) -> *mut c_char {
    if resp.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: resp is a valid HewHttpResponse per caller contract.
    let r = unsafe { &*resp };
    // SAFETY: If non-null, name is a valid NUL-terminated C string per caller contract.
    let Some(name_str) = (unsafe { cstr_to_str(name) }) else {
        return str_to_malloc("");
    };
    let name_lower = name_str.to_lowercase();
    if r.headers.is_null() {
        return str_to_malloc("");
    }
    // SAFETY: headers was allocated with Box::into_raw in capture_headers.
    let headers = unsafe { &*r.headers };
    for (k, v) in headers {
        if k.to_lowercase() == name_lower {
            return str_to_malloc(v);
        }
    }
    str_to_malloc("")
}

/// Get the `content-type` response header.
///
/// Convenience shorthand for `hew_http_response_header(resp, "content-type")`.
/// Returns a malloc-allocated C string (empty if not present). Returns null if
/// `resp` is null.
///
/// # Safety
///
/// `resp` must be a valid [`HewHttpResponse`] pointer, or null.
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_content_type(
    resp: *const HewHttpResponse,
) -> *mut c_char {
    // SAFETY: name is a valid static NUL-terminated C string.
    unsafe { hew_http_response_header(resp, c"content-type".as_ptr()) }
}

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

/// Return a new `Vec<(String, String)>` containing all captured response headers.
///
/// Each element is a `(name, value)` pair of `malloc`-allocated C strings.
/// The caller owns the returned vector and its element strings. Hew's compiled
/// destructor frees the string fields when the `Vec<(String, String)>` goes
/// out of scope. Returns an empty vector if `resp` is null or no headers were
/// captured; never returns null.
///
/// # Safety
///
/// `resp` must be a valid [`HewHttpResponse`] pointer, or null.
///
/// # Panics
///
/// In practice never panics. The internal conversion of
/// `size_of::<*mut c_char>() * 2` to `i64` is infallible on any supported
/// platform (pointer sizes are always a small fraction of `i64::MAX`).
#[no_mangle]
pub unsafe extern "C" fn hew_http_response_headers(resp: *const HewHttpResponse) -> *mut HewVec {
    let elem_size = i64::try_from(2 * std::mem::size_of::<*mut c_char>())
        .expect("pointer-pair elem_size always fits i64");
    // SAFETY: allocates a new HewVec with elem_size=16 (two pointers), Plain kind.
    let vec = unsafe { hew_cabi::vec::hew_vec_new_generic(elem_size, 0) };
    if resp.is_null() {
        return vec;
    }
    // SAFETY: resp is a valid HewHttpResponse per caller contract.
    let r = unsafe { &*resp };
    if r.headers.is_null() {
        return vec;
    }
    // SAFETY: headers was allocated with Box::into_raw in capture_headers.
    let headers = unsafe { &*r.headers };
    for (name, value) in headers {
        let pair = HewStringPair {
            name: str_to_malloc(name),
            value: str_to_malloc(value),
        };
        // SAFETY: vec is a valid HewVec; &pair is a valid elem_size-byte region.
        unsafe {
            hew_cabi::vec::hew_vec_push_generic(vec, std::ptr::addr_of!(pair).cast::<c_void>());
        }
    }
    vec
}

/// Extract a response body string and release the response handle.
///
/// Returns null when the response pointer is null or represents a transport or
/// validation error (`status_code < 0`).
unsafe fn take_body_string(resp: *mut HewHttpResponse) -> *mut c_char {
    if resp.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: resp was just allocated by one of the hew_http_* request functions.
    let resp_ref = unsafe { &mut *resp };

    if resp_ref.status_code < 0 {
        // SAFETY: resp is a valid HewHttpResponse from one of the constructors.
        unsafe { hew_http_response_free(resp) };
        return std::ptr::null_mut();
    }

    let body = resp_ref.body;
    resp_ref.body = std::ptr::null_mut();

    // SAFETY: resp is valid and body ownership was transferred to the caller.
    unsafe { hew_http_response_free(resp) };

    body
}

/// Convenience wrapper: make an HTTP GET request and return just the body
/// string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on transport or network failure.
/// Non-2xx HTTP responses still return a non-null string (the body may be
/// empty); use [`hew_http_get`] to inspect the status code.
///
/// # Safety
///
/// `url` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_get_string(url: *const c_char) -> *mut c_char {
    // SAFETY: url is forwarded with the same contract to hew_http_get.
    let resp = unsafe { hew_http_get(url) };
    // SAFETY: resp originates from hew_http_get.
    unsafe { take_body_string(resp) }
}

/// Convenience wrapper: make an HTTP POST request and return just the response
/// body string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on transport or network failure
/// (reported with a negative status code). Non-2xx HTTP responses still
/// return a non-null string (the body may be empty); use [`hew_http_post`]
/// to inspect the status code.
///
/// # Safety
///
/// `url`, `content_type`, and `body` must all be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_http_post_string(
    url: *const c_char,
    content_type: *const c_char,
    body: *const c_char,
) -> *mut c_char {
    // SAFETY: all pointers forwarded with the same contract to hew_http_post.
    let resp = unsafe { hew_http_post(url, content_type, body) };
    // SAFETY: resp originates from hew_http_post.
    unsafe { take_body_string(resp) }
}

/// Convenience wrapper: make an HTTP request and return just the body string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on transport or network failure.
/// Non-2xx HTTP responses still return a non-null string (the body may be
/// empty); use [`hew_http_request`] to inspect the status code.
///
/// # Safety
///
/// This shares the same pointer contracts as [`hew_http_request`].
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_string(
    method: *const c_char,
    url: *const c_char,
    body: *const c_char,
    headers: *const *const c_char,
    header_count: i32,
) -> *mut c_char {
    // SAFETY: all pointers are forwarded with the same contract to hew_http_request.
    let resp = unsafe { hew_http_request(method, url, body, headers, header_count) };
    // SAFETY: resp originates from hew_http_request.
    unsafe { take_body_string(resp) }
}

/// Hew-facing wrapper for [`hew_http_request_string`] that accepts `Vec<(String, String)>`
/// headers.
///
/// # Safety
///
/// This shares the same pointer contracts as [`hew_http_request_hew`].
#[no_mangle]
pub unsafe extern "C" fn hew_http_request_string_hew(
    method: *const c_char,
    url: *const c_char,
    body: *const c_char,
    headers: *mut HewVec,
) -> *mut c_char {
    // SAFETY: all pointers are forwarded with the same contract to hew_http_request_hew.
    let resp = unsafe { hew_http_request_hew(method, url, body, headers) };
    // SAFETY: resp originates from hew_http_request_hew.
    unsafe { take_body_string(resp) }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::ptr;

    /// Read a response and free it; returns `(status_code, body_string)`.
    ///
    /// # Safety
    /// `resp` must be a valid non-null `*mut HewHttpResponse`.
    unsafe fn take_response(resp: *mut HewHttpResponse) -> (i32, String) {
        assert!(!resp.is_null());
        // SAFETY: resp is valid and non-null.
        let r = unsafe { &*resp };
        let status = r.status_code;
        // SAFETY: body is a valid NUL-terminated C string from str_to_malloc.
        let body = unsafe { CStr::from_ptr(r.body) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: resp was returned by one of the hew_http_* constructors.
        unsafe { hew_http_response_free(resp) };
        (status, body)
    }

    /// Build a Hew `Vec<String>` populated with the provided header lines.
    unsafe fn make_string_vec(values: &[&str]) -> *mut HewVec {
        // SAFETY: hew_vec_new_str allocates a valid Vec<String> handle.
        let vec = unsafe { hew_cabi::vec::hew_vec_new_str() };
        for value in values {
            let value = CString::new(*value).unwrap();
            // SAFETY: vec is a valid string vec and value is a valid C string.
            unsafe { hew_cabi::vec::hew_vec_push_str(vec, value.as_ptr()) };
        }
        vec
    }

    // -- Existing tests (request null/unsupported) --------------------

    #[test]
    fn request_null_method_returns_error() {
        let url = CString::new("http://example.com").unwrap();
        // SAFETY: method is null (invalid), url is valid.
        let resp =
            unsafe { hew_http_request(ptr::null(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        // SAFETY: resp is a valid error response.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
        assert!(!body.is_empty());
    }

    #[test]
    fn request_null_url_returns_error() {
        let method = CString::new("GET").unwrap();
        // SAFETY: url is null (invalid), method is valid.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), ptr::null(), ptr::null(), ptr::null(), 0) };
        // SAFETY: resp is a valid error response.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
        assert!(!body.is_empty());
    }

    #[test]
    fn request_unsupported_method_returns_error() {
        let method = CString::new("TRACE").unwrap();
        let url = CString::new("http://example.com").unwrap();
        // SAFETY: both are valid C strings.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        // SAFETY: resp is a valid error response.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
        assert!(body.contains("unsupported"));
    }

    #[test]
    fn set_timeout_stores_value() {
        // Test several timeout scenarios in one test to avoid racing with
        // parallel tests that also mutate the global HTTP_TIMEOUT_MS.
        let original = HTTP_TIMEOUT_MS.load(Ordering::Relaxed);

        // SAFETY: no pointer arguments; just writes to an atomic.
        unsafe { hew_http_set_timeout(5_000) };
        assert_eq!(HTTP_TIMEOUT_MS.load(Ordering::Relaxed), 5_000);

        // Zero disables timeout.
        // SAFETY: no pointer arguments.
        unsafe { hew_http_set_timeout(0) };
        assert_eq!(HTTP_TIMEOUT_MS.load(Ordering::Relaxed), 0);

        // Negative values are stored; make_agent clamps via .max(0).
        // SAFETY: no pointer arguments.
        unsafe { hew_http_set_timeout(-1) };
        assert_eq!(HTTP_TIMEOUT_MS.load(Ordering::Relaxed), -1);

        // Restore original so other tests are unaffected.
        // SAFETY: no pointer arguments; just writes to an atomic.
        unsafe { hew_http_set_timeout(original) };
    }

    #[test]
    fn request_header_count_zero_is_accepted() {
        let method = CString::new("GET").unwrap();
        let url = CString::new("http://localhost:0/").unwrap();
        // SAFETY: method and url are valid C strings; headers is null.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        // SAFETY: resp is a valid error response.
        let (status, _body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn response_status_accessor_returns_code() {
        let resp = build_response(201, "created", std::ptr::null_mut());
        // SAFETY: resp is a valid HewHttpResponse from build_response.
        assert_eq!(unsafe { hew_http_response_status(resp) }, 201);
        // SAFETY: resp is a valid HewHttpResponse.
        unsafe { hew_http_response_free(resp) };
    }

    #[test]
    fn response_status_accessor_null_returns_minus_one() {
        // SAFETY: Null pointer is explicitly handled.
        assert_eq!(unsafe { hew_http_response_status(ptr::null()) }, -1);
    }

    #[test]
    fn response_body_accessor_returns_copy() {
        let resp = build_response(200, "hello", std::ptr::null_mut());
        // SAFETY: resp is a valid HewHttpResponse.
        let body_ptr = unsafe { hew_http_response_body(resp) };
        assert!(!body_ptr.is_null());
        // SAFETY: body_ptr is a valid malloc'd C string.
        let body = unsafe { CStr::from_ptr(body_ptr) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: body_ptr was malloc'd by hew_http_response_body (strdup).
        unsafe { libc::free(body_ptr.cast()) };
        // SAFETY: resp is still valid (body_ptr is a copy).
        unsafe { hew_http_response_free(resp) };
        assert_eq!(body, "hello");
    }

    #[test]
    fn response_header_lookup_case_insensitive() {
        let headers = Box::into_raw(Box::new(vec![(
            "Content-Type".to_string(),
            "application/json".to_string(),
        )]));
        let resp = build_response(200, "", headers);
        let name = CString::new("content-type").unwrap();
        // SAFETY: resp and name are valid.
        let h = unsafe { hew_http_response_header(resp, name.as_ptr()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        assert_eq!(val, "application/json");
    }

    #[test]
    fn response_header_missing_returns_empty_string() {
        let resp = build_response(200, "", std::ptr::null_mut());
        let name = CString::new("x-missing").unwrap();
        // SAFETY: resp and name are valid.
        let h = unsafe { hew_http_response_header(resp, name.as_ptr()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        assert_eq!(val, "");
    }

    #[test]
    fn response_content_type_returns_header() {
        let headers = Box::into_raw(Box::new(vec![(
            "content-type".to_string(),
            "text/plain".to_string(),
        )]));
        let resp = build_response(200, "", headers);
        // SAFETY: resp is a valid HewHttpResponse.
        let ct = unsafe { hew_http_response_content_type(resp) };
        assert!(!ct.is_null());
        // SAFETY: ct is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(ct) }.to_str().unwrap().to_owned();
        // SAFETY: ct was malloc'd.
        unsafe { libc::free(ct.cast()) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        assert_eq!(val, "text/plain");
    }

    // -- Null/empty input guards --------------------------------------

    #[test]
    fn get_null_url_returns_error_response() {
        // SAFETY: passing null is the tested scenario.
        let resp = unsafe { hew_http_get(ptr::null()) };
        assert!(
            !resp.is_null(),
            "null URL should return error response, not null"
        );
        // SAFETY: resp is a valid error response.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
        assert!(body.contains("invalid argument"));
    }

    #[test]
    fn post_null_url_returns_error_response() {
        let ct = CString::new("text/plain").unwrap();
        let body = CString::new("hello").unwrap();
        // SAFETY: url is null (tested scenario); ct and body are valid.
        let resp = unsafe { hew_http_post(ptr::null(), ct.as_ptr(), body.as_ptr()) };
        // SAFETY: resp is a valid error response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn post_null_content_type_returns_error_response() {
        let url = CString::new("http://localhost:1/test").unwrap();
        let body = CString::new("hello").unwrap();
        // SAFETY: content_type is null (tested scenario); url and body valid.
        let resp = unsafe { hew_http_post(url.as_ptr(), ptr::null(), body.as_ptr()) };
        // SAFETY: resp is a valid error response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn post_null_body_returns_error_response() {
        let url = CString::new("http://localhost:1/test").unwrap();
        let ct = CString::new("text/plain").unwrap();
        // SAFETY: body is null (tested scenario); url and ct are valid.
        let resp = unsafe { hew_http_post(url.as_ptr(), ct.as_ptr(), ptr::null()) };
        // SAFETY: resp is a valid error response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn get_string_null_url_returns_null() {
        // SAFETY: passing null is the tested scenario.
        let result = unsafe { hew_http_get_string(ptr::null()) };
        assert!(result.is_null());
    }

    #[test]
    fn post_string_null_url_returns_null() {
        let ct = CString::new("text/plain").unwrap();
        let body = CString::new("data").unwrap();
        // SAFETY: url is null (tested scenario); ct and body are valid.
        let result = unsafe { hew_http_post_string(ptr::null(), ct.as_ptr(), body.as_ptr()) };
        assert!(result.is_null());
    }

    // -- Response free ------------------------------------------------

    #[test]
    fn response_free_null_is_noop() {
        // SAFETY: null is explicitly handled by hew_http_response_free.
        unsafe { hew_http_response_free(ptr::null_mut()) };
    }

    // -- Response body edge cases -------------------------------------

    #[test]
    fn response_body_null_resp_returns_null() {
        // SAFETY: null pointer is the tested scenario.
        let body = unsafe { hew_http_response_body(ptr::null()) };
        assert!(body.is_null());
    }

    #[test]
    fn response_body_empty_returns_empty_string() {
        let resp = build_response(204, "", ptr::null_mut());
        // SAFETY: resp is a valid HewHttpResponse.
        let body_ptr = unsafe { hew_http_response_body(resp) };
        assert!(!body_ptr.is_null());
        // SAFETY: body_ptr is a valid malloc'd C string.
        let body = unsafe { CStr::from_ptr(body_ptr) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: body_ptr was malloc'd by strdup / str_to_malloc.
        unsafe { libc::free(body_ptr.cast()) };
        // SAFETY: resp is still valid (body_ptr was a copy).
        unsafe { hew_http_response_free(resp) };
        assert_eq!(body, "");
    }

    // -- Response header edge cases -----------------------------------

    #[test]
    fn response_header_null_name_returns_empty() {
        let resp = build_response(200, "", ptr::null_mut());
        // SAFETY: name is null (tested scenario); resp is valid.
        let h = unsafe { hew_http_response_header(resp, ptr::null()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        assert_eq!(val, "");
    }

    #[test]
    fn response_header_null_resp_returns_empty() {
        let name = CString::new("x-test").unwrap();
        // SAFETY: resp is null (tested scenario); name is valid.
        let h = unsafe { hew_http_response_header(ptr::null(), name.as_ptr()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        assert_eq!(val, "");
    }

    #[test]
    fn response_content_type_null_resp_returns_empty() {
        // SAFETY: resp is null (tested scenario).
        let ct = unsafe { hew_http_response_content_type(ptr::null()) };
        assert!(!ct.is_null());
        // SAFETY: ct is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(ct) }.to_str().unwrap().to_owned();
        // SAFETY: ct was malloc'd.
        unsafe { libc::free(ct.cast()) };
        assert_eq!(val, "");
    }

    #[test]
    fn response_multiple_headers_finds_correct_one() {
        let headers = Box::into_raw(Box::new(vec![
            ("X-Request-Id".to_string(), "abc-123".to_string()),
            ("Content-Type".to_string(), "application/json".to_string()),
            ("X-Rate-Limit".to_string(), "100".to_string()),
        ]));
        let resp = build_response(200, "", headers);
        let name = CString::new("X-RATE-LIMIT").unwrap();
        // SAFETY: resp and name are valid.
        let h = unsafe { hew_http_response_header(resp, name.as_ptr()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        assert_eq!(val, "100");
    }

    // -- Response headers() accessor ----------------------------------

    #[test]
    fn response_headers_null_resp_returns_empty_vec() {
        // SAFETY: null is explicitly handled; returns a valid empty vec.
        let vec = unsafe { hew_http_response_headers(ptr::null()) };
        assert!(!vec.is_null());
        // SAFETY: vec was just allocated by hew_http_response_headers.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(len, 0);
        // SAFETY: vec is a valid HewVec; no string elements to free (ElemKind::Plain, len=0).
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
    }

    #[test]
    fn response_headers_no_headers_field_returns_empty_vec() {
        let resp = build_response(200, "", ptr::null_mut());
        // SAFETY: resp is a valid HewHttpResponse with null headers.
        let vec = unsafe { hew_http_response_headers(resp) };
        assert!(!vec.is_null());
        // SAFETY: vec was allocated by hew_http_response_headers.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(len, 0);
        // SAFETY: vec is a valid HewVec; no elements to free.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
    }

    #[test]
    fn response_headers_returns_all_pairs_in_order() {
        let headers = Box::into_raw(Box::new(vec![
            ("content-type".to_string(), "application/json".to_string()),
            ("x-request-id".to_string(), "abc-123".to_string()),
        ]));
        let resp = build_response(200, "", headers);
        // SAFETY: resp is a valid HewHttpResponse.
        let vec = unsafe { hew_http_response_headers(resp) };
        assert!(!vec.is_null());
        // SAFETY: vec is valid.
        let len = unsafe { hew_cabi::vec::hew_vec_len(vec) };
        assert_eq!(len, 2);

        // Read and verify each (name, value) pair.
        // Elements must be freed manually: ElemKind::Plain means hew_vec_free
        // only releases the backing buffer, not the embedded string pointers.
        for i in 0..2i64 {
            // SAFETY: vec is valid and i is in range.
            let elem_ptr = unsafe { hew_cabi::vec::hew_vec_get_generic(vec, i) };
            assert!(!elem_ptr.is_null());
            // SAFETY: elem_ptr points to a HewStringPair (two consecutive *mut c_char).
            let pair = unsafe { &*(elem_ptr.cast::<HewStringPair>()) };
            assert!(!pair.name.is_null());
            assert!(!pair.value.is_null());
            // SAFETY: name and value are valid malloc'd C strings from hew_http_response_headers.
            let name = unsafe { CStr::from_ptr(pair.name) }
                .to_str()
                .unwrap()
                .to_owned();
            // SAFETY: pair.value is a valid malloc'd C string from hew_http_response_headers.
            let value = unsafe { CStr::from_ptr(pair.value) }
                .to_str()
                .unwrap()
                .to_owned();
            if i == 0 {
                assert_eq!(name, "content-type");
                assert_eq!(value, "application/json");
            } else {
                assert_eq!(name, "x-request-id");
                assert_eq!(value, "abc-123");
            }
            // Free the strings before freeing the backing buffer.
            // SAFETY: pair.name was malloc'd by hew_http_response_headers.
            unsafe { libc::free(pair.name.cast()) };
            // SAFETY: pair.value was malloc'd by hew_http_response_headers.
            unsafe { libc::free(pair.value.cast()) };
        }

        // SAFETY: string elements already freed above; just releases the buffer.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
    }

    #[test]
    fn response_headers_single_pair_roundtrip() {
        let headers = Box::into_raw(Box::new(vec![(
            "x-custom".to_string(),
            "my-value".to_string(),
        )]));
        let resp = build_response(200, "", headers);
        // SAFETY: resp is a valid HewHttpResponse.
        let vec = unsafe { hew_http_response_headers(resp) };
        assert!(!vec.is_null());
        // SAFETY: vec is valid.
        assert_eq!(unsafe { hew_cabi::vec::hew_vec_len(vec) }, 1);

        // SAFETY: index 0 is in range.
        let elem_ptr = unsafe { hew_cabi::vec::hew_vec_get_generic(vec, 0) };
        // SAFETY: elem_ptr points to a HewStringPair.
        let pair = unsafe { &*(elem_ptr.cast::<HewStringPair>()) };
        // SAFETY: pointers are valid malloc'd C strings from hew_http_response_headers.
        let name = unsafe { CStr::from_ptr(pair.name) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: pair.value is a valid malloc'd C string from hew_http_response_headers.
        let value = unsafe { CStr::from_ptr(pair.value) }
            .to_str()
            .unwrap()
            .to_owned();
        assert_eq!(name, "x-custom");
        assert_eq!(value, "my-value");
        // SAFETY: pair.name was malloc'd by hew_http_response_headers.
        unsafe { libc::free(pair.name.cast()) };
        // SAFETY: pair.value was malloc'd by hew_http_response_headers.
        unsafe { libc::free(pair.value.cast()) };

        // SAFETY: elements freed; safe to release the buffer.
        unsafe { hew_cabi::vec::hew_vec_free(vec) };
        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
    }

    // -- Timeout ------------------------------------------------------
    // (consolidated into set_timeout_stores_value above to avoid
    // parallel test races on the global HTTP_TIMEOUT_MS atomic)

    // -- build_response -----------------------------------------------

    #[test]
    fn build_response_preserves_body_length() {
        let resp = build_response(200, "hello world", ptr::null_mut());
        // SAFETY: resp is a valid HewHttpResponse from build_response.
        let r = unsafe { &*resp };
        assert_eq!(r.body_len, 11);
        assert_eq!(r.status_code, 200);
        // SAFETY: resp was allocated by build_response.
        unsafe { hew_http_response_free(resp) };
    }

    #[test]
    fn error_response_has_status_minus_one_and_null_headers() {
        let resp = error_response("test error");
        assert!(!resp.is_null());
        // SAFETY: resp is a valid HewHttpResponse from error_response.
        let r = unsafe { &*resp };
        assert_eq!(r.status_code, -1);
        assert!(r.headers.is_null());
        // SAFETY: body is a valid C string from str_to_malloc.
        let body = unsafe { CStr::from_ptr(r.body) }
            .to_str()
            .unwrap()
            .to_owned();
        assert_eq!(body, "test error");
        // SAFETY: resp was allocated by error_response.
        unsafe { hew_http_response_free(resp) };
    }

    // -- make_agent ---------------------------------------------------

    #[test]
    fn make_agent_does_not_panic_with_default_timeout() {
        let _agent = make_agent();
    }

    // -- Invalid URL (actual network) ---------------------------------

    #[test]
    fn get_invalid_url_returns_transport_error() {
        let url = CString::new("http://[::1]:0/this-will-fail").unwrap();
        // SAFETY: url is a valid C string.
        let resp = unsafe { hew_http_get(url.as_ptr()) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, _body) = unsafe { take_response(resp) };
        assert_eq!(status, -1, "unreachable URL should yield transport error");
    }

    #[test]
    fn post_invalid_url_returns_transport_error() {
        let url = CString::new("http://[::1]:0/this-will-fail").unwrap();
        let ct = CString::new("text/plain").unwrap();
        let body = CString::new("test").unwrap();
        // SAFETY: all pointers are valid C strings.
        let resp = unsafe { hew_http_post(url.as_ptr(), ct.as_ptr(), body.as_ptr()) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn request_get_invalid_url_returns_transport_error() {
        let method = CString::new("GET").unwrap();
        let url = CString::new("http://[::1]:0/nope").unwrap();
        // SAFETY: method and url are valid C strings.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    // -- hew_http_request with headers --------------------------------

    #[test]
    fn request_with_null_header_entries_skipped() {
        let method = CString::new("GET").unwrap();
        let url = CString::new("http://[::1]:0/test").unwrap();
        let headers: [*const c_char; 2] = [ptr::null(), ptr::null()];
        // SAFETY: method and url valid; headers has 2 null entries (skipped).
        let resp = unsafe {
            hew_http_request(
                method.as_ptr(),
                url.as_ptr(),
                ptr::null(),
                headers.as_ptr(),
                2,
            )
        };
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn request_with_malformed_header_skipped() {
        let method = CString::new("GET").unwrap();
        let url = CString::new("http://[::1]:0/test").unwrap();
        let bad_header = CString::new("no-colon-here").unwrap();
        let headers: [*const c_char; 1] = [bad_header.as_ptr()];
        // SAFETY: all pointers are valid.
        let resp = unsafe {
            hew_http_request(
                method.as_ptr(),
                url.as_ptr(),
                ptr::null(),
                headers.as_ptr(),
                1,
            )
        };
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
    }

    #[test]
    fn request_hew_rejects_non_string_header_vec() {
        let method = CString::new("GET").unwrap();
        let url = CString::new("http://example.com").unwrap();
        // SAFETY: hew_vec_new allocates a valid non-string HewVec for the tested failure path.
        let headers = unsafe { hew_cabi::vec::hew_vec_new() };
        // SAFETY: method/url are valid C strings and headers is a live HewVec handle.
        let resp =
            unsafe { hew_http_request_hew(method.as_ptr(), url.as_ptr(), ptr::null(), headers) };
        // SAFETY: resp is a valid error response and headers is still owned by the test.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, -1);
        assert!(body.contains("Vec<String>"));
        // SAFETY: headers was allocated by hew_vec_new and has not been freed yet.
        unsafe { hew_cabi::vec::hew_vec_free(headers) };
    }

    // -- Loopback integration tests -----------------------------------

    use std::thread;

    fn start_echo_server(
        response_status: u16,
        response_body: &str,
    ) -> (String, thread::JoinHandle<()>) {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind to loopback");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());
        let body = response_body.to_string();
        let handle = thread::spawn(move || {
            if let Ok(req) = server.recv() {
                let response = tiny_http::Response::from_string(&body)
                    .with_status_code(tiny_http::StatusCode(response_status));
                let _ = req.respond(response);
            }
        });
        (addr, handle)
    }

    #[test]
    fn loopback_get_200_returns_body() {
        let (addr, handle) = start_echo_server(200, "hello from server");
        let url = CString::new(format!("{addr}/test")).unwrap();
        // SAFETY: url is a valid C string.
        let resp = unsafe { hew_http_get(url.as_ptr()) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, 200);
        assert_eq!(body, "hello from server");
        handle.join().unwrap();
    }

    #[test]
    fn loopback_get_404_returns_status_with_empty_body() {
        let (addr, handle) = start_echo_server(404, "not found");
        let url = CString::new(format!("{addr}/missing")).unwrap();
        // SAFETY: url is a valid C string.
        let resp = unsafe { hew_http_get(url.as_ptr()) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, body) = unsafe { take_response(resp) };
        // ureq treats 4xx as Error::StatusCode, so hew_http_get builds
        // a response with the status code but empty body.
        assert_eq!(status, 404);
        assert!(body.is_empty());
        handle.join().unwrap();
    }

    #[test]
    fn loopback_post_sends_body() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let mut req = server.recv().expect("receive request");
            let mut body = String::new();
            req.as_reader().read_to_string(&mut body).unwrap();
            assert_eq!(body, "posted data");
            let response = tiny_http::Response::from_string("accepted")
                .with_status_code(tiny_http::StatusCode(201));
            let _ = req.respond(response);
        });

        let url = CString::new(format!("{addr}/submit")).unwrap();
        let ct = CString::new("text/plain").unwrap();
        let body = CString::new("posted data").unwrap();
        // SAFETY: all pointers are valid C strings.
        let resp = unsafe { hew_http_post(url.as_ptr(), ct.as_ptr(), body.as_ptr()) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, resp_body) = unsafe { take_response(resp) };
        assert_eq!(status, 201);
        assert_eq!(resp_body, "accepted");
        handle.join().unwrap();
    }

    #[test]
    fn loopback_get_string_returns_body_only() {
        let (addr, handle) = start_echo_server(200, "body only");
        let url = CString::new(format!("{addr}/string")).unwrap();
        // SAFETY: url is a valid C string.
        let result = unsafe { hew_http_get_string(url.as_ptr()) };
        assert!(!result.is_null());
        // SAFETY: result is a valid malloc'd C string.
        let body = unsafe { CStr::from_ptr(result) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: result was malloc'd by hew_http_get_string.
        unsafe { libc::free(result.cast()) };
        assert_eq!(body, "body only");
        handle.join().unwrap();
    }

    #[test]
    fn loopback_post_string_returns_body_only() {
        let (addr, handle) = start_echo_server(200, "post body");
        let url = CString::new(format!("{addr}/pstring")).unwrap();
        let ct = CString::new("text/plain").unwrap();
        let body = CString::new("data").unwrap();
        // SAFETY: all pointers are valid C strings.
        let result = unsafe { hew_http_post_string(url.as_ptr(), ct.as_ptr(), body.as_ptr()) };
        assert!(!result.is_null());
        // SAFETY: result is a valid malloc'd C string.
        let s = unsafe { CStr::from_ptr(result) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: result was malloc'd.
        unsafe { libc::free(result.cast()) };
        assert_eq!(s, "post body");
        handle.join().unwrap();
    }

    #[test]
    fn loopback_get_string_error_returns_null() {
        // hew_http_get_string returns null when the underlying request fails.
        let url = CString::new("http://[::1]:0/will-fail").unwrap();
        // SAFETY: url is a valid C string.
        let result = unsafe { hew_http_get_string(url.as_ptr()) };
        assert!(result.is_null(), "transport error should return null");
    }

    #[test]
    fn loopback_request_put_with_body() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let mut req = server.recv().expect("receive request");
            assert_eq!(req.method().as_str(), "PUT");
            let mut body = String::new();
            req.as_reader().read_to_string(&mut body).unwrap();
            assert_eq!(body, "update payload");
            let response = tiny_http::Response::from_string("updated")
                .with_status_code(tiny_http::StatusCode(200));
            let _ = req.respond(response);
        });

        let method = CString::new("PUT").unwrap();
        let url = CString::new(format!("{addr}/resource")).unwrap();
        let body = CString::new("update payload").unwrap();
        // SAFETY: all pointers are valid C strings.
        let resp = unsafe {
            hew_http_request(method.as_ptr(), url.as_ptr(), body.as_ptr(), ptr::null(), 0)
        };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, resp_body) = unsafe { take_response(resp) };
        assert_eq!(status, 200);
        assert_eq!(resp_body, "updated");
        handle.join().unwrap();
    }

    #[test]
    fn loopback_request_delete() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let req = server.recv().expect("receive request");
            assert_eq!(req.method().as_str(), "DELETE");
            let response =
                tiny_http::Response::from_string("").with_status_code(tiny_http::StatusCode(204));
            let _ = req.respond(response);
        });

        let method = CString::new("DELETE").unwrap();
        let url = CString::new(format!("{addr}/item/42")).unwrap();
        // SAFETY: all pointers are valid C strings.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, 204);
        handle.join().unwrap();
    }

    #[test]
    fn loopback_request_with_custom_headers() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let req = server.recv().expect("receive request");
            let mut found = false;
            for h in req.headers() {
                if h.field.as_str().as_str().eq_ignore_ascii_case("x-custom") {
                    assert_eq!(h.value.as_str(), "test-value");
                    found = true;
                }
            }
            assert!(found, "custom header not found on server side");
            let response =
                tiny_http::Response::from_string("ok").with_status_code(tiny_http::StatusCode(200));
            let _ = req.respond(response);
        });

        let method = CString::new("GET").unwrap();
        let url = CString::new(format!("{addr}/headers")).unwrap();
        let h1 = CString::new("X-Custom: test-value").unwrap();
        let headers: [*const c_char; 1] = [h1.as_ptr()];
        // SAFETY: all pointers are valid C strings.
        let resp = unsafe {
            hew_http_request(
                method.as_ptr(),
                url.as_ptr(),
                ptr::null(),
                headers.as_ptr(),
                1,
            )
        };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response.
        let (status, _) = unsafe { take_response(resp) };
        assert_eq!(status, 200);
        handle.join().unwrap();
    }

    #[test]
    fn loopback_request_hew_with_custom_headers() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let req = server.recv().expect("receive request");
            let mut found = false;
            for h in req.headers() {
                if h.field
                    .as_str()
                    .as_str()
                    .eq_ignore_ascii_case("x-hew-surface")
                {
                    assert_eq!(h.value.as_str(), "client");
                    found = true;
                }
            }
            assert!(found, "hew surface header not found on server side");
            let response =
                tiny_http::Response::from_string("ok").with_status_code(tiny_http::StatusCode(200));
            let _ = req.respond(response);
        });

        let method = CString::new("GET").unwrap();
        let url = CString::new(format!("{addr}/hew-surface")).unwrap();
        // SAFETY: make_string_vec returns a live Vec<String> handle for this request.
        let headers = unsafe { make_string_vec(&["X-Hew-Surface: client"]) };
        // SAFETY: method/url are valid C strings and headers is a valid string vec.
        let resp =
            unsafe { hew_http_request_hew(method.as_ptr(), url.as_ptr(), ptr::null(), headers) };
        assert!(!resp.is_null());
        // SAFETY: resp is a valid response handle.
        let (status, body) = unsafe { take_response(resp) };
        assert_eq!(status, 200);
        assert_eq!(body, "ok");
        // SAFETY: headers was allocated by make_string_vec and remains owned here.
        unsafe { hew_cabi::vec::hew_vec_free(headers) };
        handle.join().unwrap();
    }

    #[test]
    fn loopback_response_captures_headers() {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let addr = format!("http://{}", server.server_addr().to_ip().unwrap());

        let handle = thread::spawn(move || {
            let req = server.recv().expect("receive request");
            let header = tiny_http::Header::from_bytes("X-Server-Id", "srv-42").unwrap();
            let response = tiny_http::Response::from_string("ok")
                .with_status_code(tiny_http::StatusCode(200))
                .with_header(header);
            let _ = req.respond(response);
        });

        let url = CString::new(format!("{addr}/capture")).unwrap();
        // SAFETY: url is a valid C string.
        let resp = unsafe { hew_http_get(url.as_ptr()) };
        assert!(!resp.is_null());

        let name = CString::new("X-Server-Id").unwrap();
        // SAFETY: resp and name are valid.
        let h = unsafe { hew_http_response_header(resp, name.as_ptr()) };
        assert!(!h.is_null());
        // SAFETY: h is a valid malloc'd C string.
        let val = unsafe { CStr::from_ptr(h) }.to_str().unwrap().to_owned();
        // SAFETY: h was malloc'd.
        unsafe { libc::free(h.cast()) };
        assert_eq!(val, "srv-42");

        // SAFETY: resp is still valid.
        unsafe { hew_http_response_free(resp) };
        handle.join().unwrap();
    }

    #[test]
    fn loopback_request_string_hew_returns_body_only() {
        let (addr, handle) = start_echo_server(200, "hew request string");
        let method = CString::new("GET").unwrap();
        let url = CString::new(format!("{addr}/hew-string")).unwrap();
        // SAFETY: make_string_vec returns a live, empty Vec<String> handle.
        let headers = unsafe { make_string_vec(&[]) };
        // SAFETY: method/url are valid C strings and headers is a valid string vec.
        let result = unsafe {
            hew_http_request_string_hew(method.as_ptr(), url.as_ptr(), ptr::null(), headers)
        };
        assert!(!result.is_null());
        // SAFETY: headers was allocated by make_string_vec and remains owned here.
        unsafe { hew_cabi::vec::hew_vec_free(headers) };
        // SAFETY: result is a valid malloc'd C string.
        let body = unsafe { CStr::from_ptr(result) }
            .to_str()
            .unwrap()
            .to_owned();
        // SAFETY: result was malloc'd by hew_http_request_string_hew.
        unsafe { libc::free(result.cast()) };
        assert_eq!(body, "hew request string");
        handle.join().unwrap();
    }
}
