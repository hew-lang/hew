//! Managed string results survive sibling releases and leave their producer usable.
//! Raw protocol objects retain their own independent storage and release path.

use crate::test_string::ManagedString;
use std::io::{Read, Write};
use std::net::TcpStream;
use std::thread::JoinHandle;
use std::time::Duration;

use hew_cabi::cabi::str_to_malloc;
use hew_cabi::string::{string_as_str, string_release, string_retain, HewString};

use super::{client, server};

fn assert_owned_results(
    symbol: &str,
    mut call: impl FnMut() -> *mut HewString,
    validate: impl Fn(&str),
) {
    let first = call();
    let second = call();
    if !first.is_null() {
        assert_ne!(first, second, "{symbol}: live copies must be independent");
    }
    // SAFETY: both results are owned managed strings, including canonical empty.
    unsafe {
        validate(string_as_str(first));
        validate(string_as_str(second));
        let retained = string_retain(first);
        string_release(first);
        string_release(second);
        validate(string_as_str(retained));
        string_release(retained);
    }
    let third = call();
    // SAFETY: the producer remains usable after releasing earlier results.
    unsafe {
        validate(string_as_str(third));
        string_release(third);
    }
}

struct RequestFixture {
    server: *mut server::HewHttpServer,
    request: *mut server::HewHttpRequest,
    client: Option<JoinHandle<Vec<u8>>>,
}

impl RequestFixture {
    fn new() -> Self {
        let addr = ManagedString::new("127.0.0.1:0");
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
        let response = ManagedString::new("retention-complete");
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

    assert_owned_results(
        "hew_http_request_method",
        // SAFETY: `fixture.request` remains live for the measurement.
        || unsafe { server::hew_http_request_method(fixture.request) },
        |text| assert_eq!(text, "POST"),
    );
    assert_owned_results(
        "hew_http_request_path",
        // SAFETY: `fixture.request` remains live for the measurement.
        || unsafe { server::hew_http_request_path(fixture.request) },
        |text| assert_eq!(text, "/retention/probe?round=1"),
    );

    let header = ManagedString::new("x-retention");
    assert_owned_results(
        "hew_http_request_header",
        // SAFETY: the request and NUL-terminated header name remain live.
        || unsafe { server::hew_http_request_header(fixture.request, header.as_ptr()) },
        |text| assert_eq!(text, "request-owner"),
    );
}

#[test]
fn request_body_result_is_transferred_and_request_survives_release() {
    let fixture = RequestFixture::new();
    let encoding = ManagedString::new("utf-8");

    // SAFETY: the fixture lends its live request and encoding strings.
    let first = unsafe { server::hew_http_request_body_string(fixture.request, encoding.as_ptr()) };
    // SAFETY: the request's body reader is now at EOF.
    let second =
        unsafe { server::hew_http_request_body_string(fixture.request, encoding.as_ptr()) };
    drop(fixture);
    // SAFETY: both returned values own their strings independently of the request.
    unsafe {
        assert_eq!(string_as_str(first), "request-body-owner");
        assert!(string_as_str(second).is_empty());
        string_release(first);
        string_release(second);
    }
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
    let missing = ManagedString::new("x-definitely-missing");
    // SAFETY: request/name are live; absence is represented by null, not a
    // borrowed static empty string.
    let absent = unsafe { server::hew_http_request_header(fixture.request, missing.as_ptr()) };
    assert!(absent.is_null());

    // SAFETY: a missing lookup does not disturb the request producer state.
    let path = unsafe { server::hew_http_request_path(fixture.request) };
    assert!(!path.is_null());
    // SAFETY: `path` is a fresh caller-owned result.
    unsafe { string_release(path) };
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
    assert_owned_results(
        "hew_http_response_body",
        // SAFETY: `response` remains live for the measurement.
        || unsafe { client::hew_http_response_body(response) },
        |text| assert_eq!(text, "response-body-owner"),
    );
    assert_eq!(
        // SAFETY: accessor calls and caller releases only borrow the response.
        unsafe { client::hew_http_response_status(response) },
        207,
        "response state must survive body result releases"
    );

    assert_owned_results(
        "hew_http_response_content_type",
        // SAFETY: `response` remains live for the measurement.
        || unsafe { client::hew_http_response_content_type(response) },
        |text| assert_eq!(text, "application/retention"),
    );

    let name = ManagedString::new("x-retention");
    assert_owned_results(
        "hew_http_response_header",
        // SAFETY: response/name remain live through the measurement.
        || unsafe { client::hew_http_response_header(response, name.as_ptr()) },
        |text| assert_eq!(text, "response-owner"),
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
    assert_owned_results(
        "hew_http_response_header(null-response)",
        // SAFETY: null is explicitly accepted.
        || unsafe { client::hew_http_response_header(std::ptr::null(), std::ptr::null()) },
        |text| assert!(text.is_empty()),
    );
    assert_owned_results(
        "hew_http_response_content_type(null-response)",
        // SAFETY: null is explicitly accepted.
        || unsafe { client::hew_http_response_content_type(std::ptr::null()) },
        |text| assert!(text.is_empty()),
    );

    let response = response_fixture();
    let missing = ManagedString::new("x-definitely-missing");
    assert_owned_results(
        "hew_http_response_header(missing)",
        // SAFETY: response/name remain live; a miss returns canonical empty.
        || unsafe { client::hew_http_response_header(response, missing.as_ptr()) },
        |text| assert!(text.is_empty()),
    );
    assert_owned_results(
        "hew_http_response_header(null-name)",
        // SAFETY: null name is explicitly mapped to canonical empty.
        || unsafe { client::hew_http_response_header(response, std::ptr::null()) },
        |text| assert!(text.is_empty()),
    );
    // SAFETY: all empty-path calls borrowed the response.
    unsafe { client::hew_http_response_free(response) };
}
