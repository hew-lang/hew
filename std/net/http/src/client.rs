//! Hew runtime: `http_client` module.
//!
//! Provides basic HTTP client functionality for compiled Hew programs.
//! All returned strings and response structs are allocated with `libc::malloc`
//! / `Box` so callers can free them with the corresponding free function.

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::ffi::CStr;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicI32, Ordering};
use std::time::Duration;

/// Global timeout for all HTTP requests, in milliseconds. Default: 30 000 ms.
static HTTP_TIMEOUT_MS: AtomicI32 = AtomicI32::new(30_000);

/// Response from an HTTP request.
///
/// Returned by [`hew_http_get`] and [`hew_http_post`].
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

    match ureq::get(url_str).call() {
        Ok(mut resp) => {
            let status = resp.status().as_u16();
            let headers = capture_headers(resp.headers());
            match resp.body_mut().read_to_string() {
                Ok(body) => build_response(i32::from(status), &body, headers),
                Err(e) => {
                    // Free headers before returning error.
                    if !headers.is_null() {
                        // SAFETY: headers was just allocated with Box::into_raw.
                        drop(unsafe { Box::from_raw(headers) });
                    }
                    error_response(&format!("failed to read response body: {e}"))
                }
            }
        }
        Err(ureq::Error::StatusCode(code)) => {
            build_response(i32::from(code), "", std::ptr::null_mut())
        }
        Err(e) => error_response(&e.to_string()),
    }
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
    // SAFETY: All pointers are valid NUL-terminated C strings per caller contract.
    let url_str = match unsafe { CStr::from_ptr(url) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in URL: {e}")),
    };
    // SAFETY: content_type is a valid NUL-terminated C string per caller contract.
    let ct_str = match unsafe { CStr::from_ptr(content_type) }.to_str() {
        Ok(s) => s,
        Err(e) => return error_response(&format!("invalid UTF-8 in content-type: {e}")),
    };
    // SAFETY: body is a valid NUL-terminated C string per caller contract.
    let body_bytes = unsafe { CStr::from_ptr(body) }.to_bytes();

    match ureq::post(url_str)
        .header("Content-Type", ct_str)
        .send(body_bytes)
    {
        Ok(mut resp) => {
            let status = resp.status().as_u16();
            let headers = capture_headers(resp.headers());
            match resp.body_mut().read_to_string() {
                Ok(resp_body) => build_response(i32::from(status), &resp_body, headers),
                Err(e) => {
                    if !headers.is_null() {
                        // SAFETY: headers was just allocated with Box::into_raw.
                        drop(unsafe { Box::from_raw(headers) });
                    }
                    error_response(&format!("failed to read response body: {e}"))
                }
            }
        }
        Err(ureq::Error::StatusCode(code)) => {
            build_response(i32::from(code), "", std::ptr::null_mut())
        }
        Err(e) => error_response(&e.to_string()),
    }
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

    // Collect headers from the "Key: Value" C string array.
    let mut parsed_headers: Vec<(&str, &str)> = Vec::new();
    if !headers.is_null() && header_count > 0 {
        for i in 0..usize::try_from(header_count).unwrap_or(0) {
            // SAFETY: headers is a valid array of header_count C string pointers per
            // caller contract; i < header_count so the pointer arithmetic is in bounds.
            let hdr_ptr = unsafe { *headers.add(i) };
            if hdr_ptr.is_null() {
                continue;
            }
            // SAFETY: each non-null entry is a valid NUL-terminated C string per caller.
            let Ok(hdr) = unsafe { CStr::from_ptr(hdr_ptr) }.to_str() else {
                continue;
            };
            if let Some((key, value)) = hdr.split_once(':') {
                parsed_headers.push((key.trim(), value.trim()));
            }
        }
    }

    let agent = make_agent();
    let method_upper = method_str.to_uppercase();

    match method_upper.as_str() {
        "GET" | "HEAD" | "DELETE" => {
            let req = match method_upper.as_str() {
                "HEAD" => agent.head(url_str),
                "DELETE" => agent.delete(url_str),
                _ => agent.get(url_str),
            };
            let req = parsed_headers
                .iter()
                .fold(req, |r, (k, v)| r.header(*k, *v));
            match req.call() {
                Ok(resp) => finish_response(resp),
                Err(ureq::Error::StatusCode(code)) => {
                    build_response(i32::from(code), "", std::ptr::null_mut())
                }
                Err(e) => error_response(&e.to_string()),
            }
        }
        "POST" | "PUT" | "PATCH" => {
            let body_bytes: &[u8] = if body.is_null() {
                b""
            } else {
                // SAFETY: body is non-null, and per caller contract is a valid NUL-terminated C string.
                unsafe { CStr::from_ptr(body) }.to_bytes()
            };
            let req = match method_upper.as_str() {
                "PUT" => agent.put(url_str),
                "PATCH" => agent.patch(url_str),
                _ => agent.post(url_str),
            };
            let req = parsed_headers
                .iter()
                .fold(req, |r, (k, v)| r.header(*k, *v));
            match req.send(body_bytes) {
                Ok(resp) => finish_response(resp),
                Err(ureq::Error::StatusCode(code)) => {
                    build_response(i32::from(code), "", std::ptr::null_mut())
                }
                Err(e) => error_response(&e.to_string()),
            }
        }
        _ => error_response(&format!("unsupported HTTP method: {method_str}")),
    }
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

/// Convenience wrapper: make an HTTP GET request and return just the body
/// string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on error.
///
/// # Safety
///
/// `url` must be a valid NUL-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn hew_http_get_string(url: *const c_char) -> *mut c_char {
    // SAFETY: url is forwarded with the same contract to hew_http_get.
    let resp = unsafe { hew_http_get(url) };
    if resp.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: resp was just allocated by hew_http_get and is valid.
    let resp_ref = unsafe { &mut *resp };

    if resp_ref.status_code < 0 {
        // Error case: free everything and return null.
        // SAFETY: resp is a valid HewHttpResponse from hew_http_get.
        unsafe { hew_http_response_free(resp) };
        return std::ptr::null_mut();
    }

    // Extract body pointer, then null it out so hew_http_response_free
    // won't free it (we're returning it to the caller).
    let body = resp_ref.body;
    resp_ref.body = std::ptr::null_mut();

    // SAFETY: resp is a valid HewHttpResponse; body was nulled so it won't be freed.
    unsafe { hew_http_response_free(resp) };

    body
}

/// Convenience wrapper: make an HTTP POST request and return just the response
/// body string.
///
/// Returns a `malloc`-allocated, NUL-terminated C string. The caller must free
/// it with `libc::free`. Returns null on error or non-2xx status.
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
    if resp.is_null() {
        return std::ptr::null_mut();
    }
    // SAFETY: resp was just allocated by hew_http_post and is valid.
    let resp_ref = unsafe { &mut *resp };

    if resp_ref.status_code < 0 {
        // SAFETY: resp is a valid HewHttpResponse from hew_http_post.
        unsafe { hew_http_response_free(resp) };
        return std::ptr::null_mut();
    }

    // Extract body, null it so hew_http_response_free won't double-free.
    let body_ptr = resp_ref.body;
    resp_ref.body = std::ptr::null_mut();

    // SAFETY: resp is valid; body was nulled.
    unsafe { hew_http_response_free(resp) };

    body_ptr
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
        // SAFETY: no pointer arguments; just writes to an atomic.
        unsafe { hew_http_set_timeout(5_000) };
        assert_eq!(HTTP_TIMEOUT_MS.load(Ordering::Relaxed), 5_000);
        // Restore default so other tests are unaffected.
        // SAFETY: no pointer arguments; just writes to an atomic.
        unsafe { hew_http_set_timeout(30_000) };
    }

    #[test]
    fn request_header_count_zero_is_accepted() {
        let method = CString::new("GET").unwrap();
        // Use an invalid host so the connection fails quickly without a live server.
        let url = CString::new("http://localhost:0/").unwrap();
        // SAFETY: method and url are valid C strings; headers is null.
        let resp =
            unsafe { hew_http_request(method.as_ptr(), url.as_ptr(), ptr::null(), ptr::null(), 0) };
        // Expect a transport error (status -1) since no server is running, but no panic.
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
}
