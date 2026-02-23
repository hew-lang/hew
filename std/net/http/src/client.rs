//! Hew runtime: `http_client` module.
//!
//! Provides basic HTTP client functionality for compiled Hew programs.
//! All returned strings and response structs are allocated with `libc::malloc`
//! / `Box` so callers can free them with the corresponding free function.

use hew_cabi::cabi::str_to_malloc;
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
    /// Response body (NUL-terminated, allocated with `malloc`). Caller frees.
    pub body: *mut c_char,
    /// Length of body in bytes (not counting NUL terminator).
    pub body_len: usize,
}

/// Build a [`HewHttpResponse`] from a Rust string body.
fn build_response(status_code: i32, body: &str) -> *mut HewHttpResponse {
    let body_len = body.len();
    let body_ptr = str_to_malloc(body);
    Box::into_raw(Box::new(HewHttpResponse {
        status_code,
        body: body_ptr,
        body_len,
    }))
}

/// Build an error response with `status_code = -1`.
fn error_response(msg: &str) -> *mut HewHttpResponse {
    build_response(-1, msg)
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
            match resp.body_mut().read_to_string() {
                Ok(body) => build_response(i32::from(status), &body),
                Err(e) => error_response(&format!("failed to read response body: {e}")),
            }
        }
        Err(ureq::Error::StatusCode(code)) => build_response(i32::from(code), ""),
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
            match resp.body_mut().read_to_string() {
                Ok(resp_body) => build_response(i32::from(status), &resp_body),
                Err(e) => error_response(&format!("failed to read response body: {e}")),
            }
        }
        Err(ureq::Error::StatusCode(code)) => build_response(i32::from(code), ""),
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

/// Parse a response, mapping body-read errors to an error response.
fn finish_response(mut resp: ureq::http::Response<ureq::Body>) -> *mut HewHttpResponse {
    let status = resp.status().as_u16();
    match resp.body_mut().read_to_string() {
        Ok(body) => build_response(i32::from(status), &body),
        Err(e) => error_response(&format!("failed to read response body: {e}")),
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
                Err(ureq::Error::StatusCode(code)) => build_response(i32::from(code), ""),
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
                Err(ureq::Error::StatusCode(code)) => build_response(i32::from(code), ""),
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
        // SAFETY: body was allocated with libc::malloc in malloc_cstring.
        unsafe { libc::free(response.body.cast()) };
    }
    // Box is dropped here, freeing the HewHttpResponse struct.
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
        // SAFETY: body is a valid NUL-terminated C string from malloc_cstring.
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
}
