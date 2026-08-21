//! A single-response HTTP server for registry-facing integration tests.
//!
//! Bound to `127.0.0.1` on an ephemeral port so a test can point a **named**
//! registry at it and prove a client path end to end without addressing a real
//! registry.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::time::Duration;

/// Read an HTTP request's headers and drain its `Content-Length` body so the
/// client's write completes before the server responds — otherwise the
/// client can fail on the write side instead of on the response status.
pub fn drain_http_request(stream: &mut TcpStream) {
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("set read timeout");
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    while let Ok(n) = stream.read(&mut chunk) {
        if n == 0 {
            break;
        }
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
        let body_so_far = buf.len() - (header_end + 4);
        let mut remaining = content_length.saturating_sub(body_so_far);
        while remaining > 0 {
            let Ok(n) = stream.read(&mut chunk) else {
                break;
            };
            if n == 0 {
                break;
            }
            remaining = remaining.saturating_sub(n);
        }
        break;
    }
}

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
    buf.windows(4).position(|w| w == b"\r\n\r\n")
}

/// Bind an ephemeral port, accept exactly one connection, drain its request,
/// and reply with a canned `status_line`/`body` response, then stop.
///
/// Returns the bound port; the caller points a named registry's `api` at it.
pub fn spawn_canned_response_server(status_line: &'static str, body: &'static str) -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
    let port = listener.local_addr().expect("local_addr").port();
    std::thread::spawn(move || {
        if let Ok((mut stream, _addr)) = listener.accept() {
            drain_http_request(&mut stream);
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
    port
}
