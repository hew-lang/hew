//! A single-response HTTP server for registry-facing integration tests.
//!
//! Bound to `127.0.0.1` on an ephemeral port so a test can point a **named**
//! registry at it and prove a client path end to end without addressing a real
//! registry.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::mpsc::{self, Receiver};
use std::time::Duration;

#[derive(Debug)]
pub struct CannedRequest {
    pub method: String,
    pub path: String,
    pub body: Vec<u8>,
}

fn read_http_request(stream: &mut TcpStream) -> CannedRequest {
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("set read timeout");
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    let (header_end, content_length) = loop {
        let n = stream.read(&mut chunk).expect("read HTTP request");
        assert!(n != 0, "connection closed before complete HTTP request");
        buf.extend_from_slice(&chunk[..n]);
        let Some(header_end) = find_double_crlf(&buf) else {
            continue;
        };
        let header_text = String::from_utf8_lossy(&buf[..header_end]);
        let content_length: usize = header_text
            .lines()
            .find_map(|line| {
                let (key, value) = line.split_once(':')?;
                key.trim()
                    .eq_ignore_ascii_case("content-length")
                    .then(|| value.trim().parse().ok())
                    .flatten()
            })
            .unwrap_or(0);
        if buf.len() >= header_end + 4 + content_length {
            break (header_end, content_length);
        }
    };

    let header_text = String::from_utf8_lossy(&buf[..header_end]);
    let request_line = header_text.lines().next().expect("HTTP request line");
    let mut request_parts = request_line.split_whitespace();
    let method = request_parts
        .next()
        .expect("HTTP request method")
        .to_owned();
    let path = request_parts.next().expect("HTTP request path").to_owned();
    let body_start = header_end + 4;
    CannedRequest {
        method,
        path,
        body: buf[body_start..body_start + content_length].to_vec(),
    }
}

/// Read and discard one complete HTTP request.
pub fn drain_http_request(stream: &mut TcpStream) {
    let _ = read_http_request(stream);
}

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
    buf.windows(4).position(|w| w == b"\r\n\r\n")
}

/// Bind an ephemeral port, accept exactly one connection, drain its request,
/// and reply with a canned `status_line`/`body` response, then stop.
///
/// Returns the bound port; the caller points a named registry's `api` at it.
pub fn spawn_canned_response_server(status_line: &'static str, body: &'static str) -> u16 {
    spawn_recording_canned_response_server(status_line, body).0
}

/// Spawn a canned server and return the request received by it.
pub fn spawn_recording_canned_response_server(
    status_line: &'static str,
    body: &'static str,
) -> (u16, Receiver<CannedRequest>) {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
    let port = listener.local_addr().expect("local_addr").port();
    let (request_tx, request_rx) = mpsc::channel();
    std::thread::spawn(move || {
        if let Ok((mut stream, _addr)) = listener.accept() {
            let request = read_http_request(&mut stream);
            let _ = request_tx.send(request);
            let response = format!(
                "HTTP/1.1 {status_line}\r\n\
                 Content-Type: application/json\r\n\
                 Content-Length: {}\r\n\
                 Connection: close\r\n\
                 \r\n\
                 {body}",
                body.len()
            );
            let _ = stream.write_all(response.as_bytes());
        }
    });
    (port, request_rx)
}
