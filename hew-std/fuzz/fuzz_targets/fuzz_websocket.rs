//! WebSocket handshakes and frames from an untrusted peer, with Hew's caps.
//!
//! The first byte picks the side. As a server, the input is the client's
//! upgrade request followed by frames; as a client, it is the server's
//! upgrade response (including redirects and subprotocol selection)
//! followed by frames. Both sides run tungstenite with the exact
//! `WebSocketConfig` every Hew attach site uses, over an in-memory stream.
//! Properties: no panic; no message exceeds Hew's message cap; reading ends
//! at the end of input.
#![no_main]

use std::io::{self, Read, Write};

use hew_std::websocket::fuzzing;
use libfuzzer_sys::fuzz_target;
use tungstenite::client::{client_with_config, uri_mode, IntoClientRequest};
use tungstenite::handshake::HandshakeError;
use tungstenite::{accept_with_config, Message, WebSocket};

const KEY: &str = "dGhlIHNhbXBsZSBub25jZQ==";

/// Reads the fuzz input, discards writes.
struct Peer<'a> {
    input: &'a [u8],
}

impl Read for Peer<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.input.read(buf)
    }
}

impl Write for Peer<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn drain(mut ws: WebSocket<Peer<'_>>) {
    // Each read consumes input or fails, so the input length bounds the loop;
    // the cap guards against a reader that stops consuming.
    for _ in 0..100_000 {
        match ws.read() {
            Ok(message) => {
                let len = match &message {
                    Message::Text(text) => text.len(),
                    Message::Binary(data) | Message::Ping(data) | Message::Pong(data) => data.len(),
                    Message::Close(_) | Message::Frame(_) => 0,
                };
                assert!(
                    len <= fuzzing::MAX_MESSAGE_SIZE_BYTES,
                    "a {len}-byte message passed the cap"
                );
            }
            Err(_) => return,
        }
    }
    panic!("reading did not reach the end of the input");
}

fn server(input: &[u8]) {
    match accept_with_config(Peer { input }, Some(fuzzing::default_config())) {
        Ok(ws) => drain(ws),
        Err(HandshakeError::Interrupted(_) | HandshakeError::Failure(_)) => {}
    }
}

fn client(input: &[u8]) {
    let mut request = "ws://example.test/chat"
        .into_client_request()
        .expect("a fixed URL is a request");
    let headers = request.headers_mut();
    headers.insert(
        "Sec-WebSocket-Protocol",
        "chat, superchat".parse().expect("header"),
    );
    // A fixed key keeps runs reproducible; its accept value is RFC 6455's
    // example, `s3pPLMBiTxaQ9kYGzzhZRbK+xOo=`.
    headers.insert("Sec-WebSocket-Key", KEY.parse().expect("header"));
    match client_with_config(request, Peer { input }, Some(fuzzing::default_config())) {
        Ok((ws, response)) => {
            let selected = response
                .headers()
                .get("Sec-WebSocket-Protocol")
                .and_then(|value| value.to_str().ok());
            if let Some(selected) = selected {
                assert!(
                    matches!(selected, "chat" | "superchat"),
                    "accepted an unoffered subprotocol {selected:?}"
                );
            }
            drain(ws);
        }
        // The redirect path Hew follows: Location becomes the next request.
        Err(HandshakeError::Failure(tungstenite::Error::Http(response)))
            if response.status().is_redirection() =>
        {
            if let Some(location) = response
                .headers()
                .get("Location")
                .and_then(|v| v.to_str().ok())
            {
                if let Ok(next) = location.into_client_request() {
                    let _ = uri_mode(next.uri());
                    let _ = next.uri().host();
                    let _ = next.uri().port_u16();
                }
            }
        }
        Err(_) => {}
    }
}

fuzz_target!(|data: &[u8]| {
    let Some((&side, input)) = data.split_first() else {
        return;
    };
    if side % 2 == 0 {
        server(input);
    } else {
        client(input);
    }
});
