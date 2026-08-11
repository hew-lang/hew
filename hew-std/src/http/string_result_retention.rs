//! Measured result-retention proofs for HTTP request/response strings.
//!
//! The request half uses one real loopback request because
//! [`server::HewHttpRequest`] intentionally hides the `tiny_http::Request`
//! that owns its method, path, headers, and body. The response half uses the
//! exported C-layout response object directly. Every admitted symbol has its
//! own R1/R2/R3 call site:
//!
//! - R1: two results held live simultaneously occupy distinct allocations.
//! - R2: `cstring_ensure_unique(result) == result`, proving refcount one at
//!   handoff.
//! - R3: after the caller releases both results, the same request/response can
//!   produce another result.
//!
//! Null, missing-header, and allocated-empty paths are audited separately.
//! They do not borrow a static sentinel and do not weaken the positive-path
//! transfer proof.

use std::ffi::{c_char, CStr, CString};
use std::io::{Read, Write};
use std::net::TcpStream;
use std::thread::JoinHandle;
use std::time::Duration;

use hew_cabi::cabi::{cstring_ensure_unique, free_cstring, str_to_malloc};

use super::{client, server};

fn assert_transferred(
    symbol: &str,
    mut call: impl FnMut() -> *mut c_char,
    validate: impl Fn(&CStr),
) {
    let first = call();
    let second = call();
    assert!(
        !first.is_null() && !second.is_null(),
        "{symbol}: expected two live results"
    );
    assert_ne!(
        first, second,
        "{symbol}: R1 failed: two live results share an address"
    );

    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: `ptr` is a live header-aware result from the named producer.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "{symbol}: R2 failed: the {label} result was shared at handoff"
        );
        // SAFETY: the uniqueness probe returned the live pointer unchanged.
        validate(unsafe { CStr::from_ptr(ptr) });
    }

    // SAFETY: R1/R2 establish two distinct, solely-owned live results.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }

    let third = call();
    assert!(
        !third.is_null(),
        "{symbol}: R3 failed after caller releases"
    );
    // SAFETY: `third` is a fresh live result.
    validate(unsafe { CStr::from_ptr(third) });
    // SAFETY: the third result is solely owned by this caller.
    unsafe { free_cstring(third) };
}

struct RequestFixture {
    server: *mut server::HewHttpServer,
    request: *mut server::HewHttpRequest,
    client: Option<JoinHandle<Vec<u8>>>,
}

impl RequestFixture {
    fn new() -> Self {
        let addr = CString::new("127.0.0.1:0").unwrap();
        // SAFETY: `addr` is a live NUL-terminated bind address.
        let server = unsafe { server::hew_http_server_new(addr.as_ptr()) };
        assert!(!server.is_null(), "loopback HTTP server must bind");
        // SAFETY: `server` remains live for the fixture.
        let port = unsafe { server::hew_http_server_port(server) };
        assert!(port > 0, "ephemeral HTTP server must report its port");

        let client = std::thread::spawn(move || {
            let mut stream = TcpStream::connect(("127.0.0.1", u16::try_from(port).unwrap()))
                .expect("loopback client must connect");
            stream
                .set_read_timeout(Some(Duration::from_secs(5)))
                .expect("set client read timeout");
            stream
                .set_write_timeout(Some(Duration::from_secs(5)))
                .expect("set client write timeout");
            let body = b"request-body-owner";
            write!(
                stream,
                "POST /retention/probe?round=1 HTTP/1.1\r\n\
                 Host: 127.0.0.1\r\n\
                 X-Retention: request-owner\r\n\
                 Content-Length: {}\r\n\
                 Connection: close\r\n\
                 \r\n",
                body.len()
            )
            .expect("write loopback request head");
            stream.write_all(body).expect("write loopback request body");
            stream.flush().expect("flush loopback request");

            let mut response = Vec::new();
            stream
                .read_to_end(&mut response)
                .expect("read loopback response");
            response
        });

        // SAFETY: `server` is live and the client has sent one request.
        let request = unsafe { server::hew_http_server_recv(server) };
        assert!(
            !request.is_null(),
            "loopback server must receive the request"
        );
        Self {
            server,
            request,
            client: Some(client),
        }
    }
}

impl Drop for RequestFixture {
    fn drop(&mut self) {
        let response = CString::new("retention-complete").unwrap();
        // SAFETY: the fixture owns a live request. A body read restores the
        // inner request, so this response also proves the request remains
        // operational after caller-side result releases.
        let status = unsafe { server::hew_http_respond_text(self.request, 200, response.as_ptr()) };
        assert_eq!(status, 0, "request must remain usable for a response");
        // SAFETY: the fixture owns both handles and closes each exactly once.
        unsafe {
            server::hew_http_request_free(self.request);
            server::hew_http_server_close(self.server);
        }
        let response = self
            .client
            .take()
            .expect("client handle present")
            .join()
            .expect("loopback client thread must finish");
        let response = String::from_utf8_lossy(&response);
        assert!(
            response.starts_with("HTTP/1.1 200"),
            "loopback response must succeed: {response}"
        );
        assert!(
            response.contains("retention-complete"),
            "loopback response body must arrive: {response}"
        );
    }
}

#[test]
fn request_method_path_and_header_results_are_transferred() {
    let fixture = RequestFixture::new();

    assert_transferred(
        "hew_http_request_method",
        // SAFETY: `fixture.request` remains live for the measurement.
        || unsafe { server::hew_http_request_method(fixture.request) },
        |text| assert_eq!(text.to_str().unwrap(), "POST"),
    );
    assert_transferred(
        "hew_http_request_path",
        // SAFETY: `fixture.request` remains live for the measurement.
        || unsafe { server::hew_http_request_path(fixture.request) },
        |text| assert_eq!(text.to_str().unwrap(), "/retention/probe?round=1"),
    );

    let header = CString::new("x-retention").unwrap();
    assert_transferred(
        "hew_http_request_header",
        // SAFETY: the request and NUL-terminated header name remain live.
        || unsafe { server::hew_http_request_header(fixture.request, header.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "request-owner"),
    );
}

#[test]
fn request_body_result_is_transferred_and_request_survives_release() {
    let fixture = RequestFixture::new();
    let encoding = CString::new("utf-8").unwrap();

    // A request body is a stream: the first read has the payload and the
    // second/third reads are allocated empty strings. That producer-specific
    // state transition still admits the same ownership proof.
    // SAFETY: request/encoding remain live through all calls.
    let first = unsafe { server::hew_http_request_body_string(fixture.request, encoding.as_ptr()) };
    // SAFETY: as above; the request's body reader is now at EOF.
    let second =
        unsafe { server::hew_http_request_body_string(fixture.request, encoding.as_ptr()) };
    assert!(
        !first.is_null() && !second.is_null(),
        "hew_http_request_body_string: expected two live results"
    );
    assert_ne!(
        first, second,
        "hew_http_request_body_string: R1 failed: live payload/EOF results alias"
    );
    for (label, ptr) in [("first", first), ("second", second)] {
        // SAFETY: `ptr` is a live header-aware body result.
        let unique = unsafe { cstring_ensure_unique(ptr) };
        assert_eq!(
            unique, ptr,
            "hew_http_request_body_string: R2 failed for {label} result"
        );
    }
    // SAFETY: both pointers remain live and solely owned.
    let first_text = unsafe { CStr::from_ptr(first) };
    assert_eq!(first_text.to_str().unwrap(), "request-body-owner");
    // SAFETY: the second pointer is a live allocated-empty result.
    assert!(unsafe { CStr::from_ptr(second) }.to_bytes().is_empty());
    // SAFETY: R1/R2 prove two independent caller-owned results.
    unsafe {
        free_cstring(first);
        free_cstring(second);
    }

    // SAFETY: caller releases do not consume the request; another body result
    // is still produced from the live EOF state.
    let third = unsafe { server::hew_http_request_body_string(fixture.request, encoding.as_ptr()) };
    assert!(
        !third.is_null(),
        "hew_http_request_body_string: R3 failed after caller releases"
    );
    // SAFETY: `third` is live and solely owned.
    assert!(unsafe { CStr::from_ptr(third) }.to_bytes().is_empty());
    // SAFETY: balancing release for the third result.
    unsafe { free_cstring(third) };

    // SAFETY: the request itself must still be readable after the body
    // result's lifecycle, independently of Drop's successful response check.
    let method = unsafe { server::hew_http_request_method(fixture.request) };
    assert!(!method.is_null());
    // SAFETY: `method` is the caller-owned result just returned.
    unsafe { free_cstring(method) };
}

#[test]
fn request_null_and_missing_header_paths_are_not_static_results() {
    // SAFETY: each API explicitly accepts null and returns null.
    unsafe {
        assert!(server::hew_http_request_method(std::ptr::null()).is_null());
        assert!(server::hew_http_request_path(std::ptr::null()).is_null());
        assert!(
            server::hew_http_request_body_string(std::ptr::null_mut(), std::ptr::null()).is_null()
        );
        assert!(server::hew_http_request_header(std::ptr::null(), std::ptr::null()).is_null());
    }

    let fixture = RequestFixture::new();
    let missing = CString::new("x-definitely-missing").unwrap();
    // SAFETY: request/name are live; absence is represented by null, not a
    // borrowed static empty string.
    let absent = unsafe { server::hew_http_request_header(fixture.request, missing.as_ptr()) };
    assert!(absent.is_null());

    // SAFETY: a missing lookup does not disturb the request producer state.
    let path = unsafe { server::hew_http_request_path(fixture.request) };
    assert!(!path.is_null());
    // SAFETY: `path` is a fresh caller-owned result.
    unsafe { free_cstring(path) };
}

fn response_fixture() -> *mut client::HewHttpResponse {
    let body = "response-body-owner";
    let headers = vec![
        (
            "Content-Type".to_owned(),
            "application/retention".to_owned(),
        ),
        ("X-Retention".to_owned(), "response-owner".to_owned()),
    ];
    Box::into_raw(Box::new(client::HewHttpResponse {
        status_code: 207,
        body: str_to_malloc(body),
        body_len: body.len(),
        headers: Box::into_raw(Box::new(headers)),
        body_allocation_failed: false,
    }))
}

#[test]
fn response_body_content_type_and_header_results_are_transferred() {
    let response = response_fixture();
    assert_transferred(
        "hew_http_response_body",
        // SAFETY: `response` remains live for the measurement.
        || unsafe { client::hew_http_response_body(response) },
        |text| assert_eq!(text.to_str().unwrap(), "response-body-owner"),
    );
    assert_eq!(
        // SAFETY: accessor calls and caller releases only borrow the response.
        unsafe { client::hew_http_response_status(response) },
        207,
        "response state must survive body result releases"
    );

    assert_transferred(
        "hew_http_response_content_type",
        // SAFETY: `response` remains live for the measurement.
        || unsafe { client::hew_http_response_content_type(response) },
        |text| assert_eq!(text.to_str().unwrap(), "application/retention"),
    );

    let name = CString::new("x-retention").unwrap();
    assert_transferred(
        "hew_http_response_header",
        // SAFETY: response/name remain live through the measurement.
        || unsafe { client::hew_http_response_header(response, name.as_ptr()) },
        |text| assert_eq!(text.to_str().unwrap(), "response-owner"),
    );

    // SAFETY: every accessor borrowed `response`; this is its sole release.
    unsafe { client::hew_http_response_free(response) };
}

#[test]
fn response_null_missing_and_empty_paths_allocate_or_return_null_as_documented() {
    // Body represents an invalid/null response with null.
    // SAFETY: null is explicitly accepted by this accessor.
    assert!(unsafe { client::hew_http_response_body(std::ptr::null()) }.is_null());

    // Header/content-type represent a null response with a fresh allocated
    // empty result, never with a static sentinel.
    assert_transferred(
        "hew_http_response_header(null-response)",
        // SAFETY: null is explicitly accepted.
        || unsafe { client::hew_http_response_header(std::ptr::null(), std::ptr::null()) },
        |text| assert!(text.to_bytes().is_empty()),
    );
    assert_transferred(
        "hew_http_response_content_type(null-response)",
        // SAFETY: null is explicitly accepted.
        || unsafe { client::hew_http_response_content_type(std::ptr::null()) },
        |text| assert!(text.to_bytes().is_empty()),
    );

    let response = response_fixture();
    let missing = CString::new("x-definitely-missing").unwrap();
    assert_transferred(
        "hew_http_response_header(missing)",
        // SAFETY: response/name remain live; a miss returns allocated empty.
        || unsafe { client::hew_http_response_header(response, missing.as_ptr()) },
        |text| assert!(text.to_bytes().is_empty()),
    );
    assert_transferred(
        "hew_http_response_header(null-name)",
        // SAFETY: null name is explicitly mapped to allocated empty.
        || unsafe { client::hew_http_response_header(response, std::ptr::null()) },
        |text| assert!(text.to_bytes().is_empty()),
    );
    // SAFETY: all empty-path calls borrowed the response.
    unsafe { client::hew_http_response_free(response) };
}
