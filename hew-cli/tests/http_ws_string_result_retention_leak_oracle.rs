//! Darwin leak slopes for shipped HTTP/WebSocket string wrappers.
//!
//! The four fixtures keep materially different producer states separate:
//! HTTP request accessors, HTTP response accessors, WebSocket message text,
//! and the WebSocket error-slot string. The network fixtures use real loopback
//! traffic. Every iteration prints one witness line, so the shared harness
//! proves both low/high probes completed all requested work before trusting
//! their leak counts.

#![cfg(unix)]

mod support;

use support::leak_slope::assert_frame_slope_below_tolerance_exact_lines;

fn http_request_source(frames: usize) -> String {
    format!(
        r#"
import std.net.http.http_client;
import std.net.http;

actor Client {{
    let url: string;
    var complete: i64;

    receive fn fetch(unused: i64) {{
        http_client.set_timeout(5000);
        let response = http_client.get(url);
        if response.status() != 200 {{
            panic("HTTP request retention response failed");
        }}
        response.close();
        complete = 1;
    }}

    receive fn finished() -> i64 {{
        complete
    }}
}}

fn main() {{
    match http.listen("127.0.0.1:0") {{
        Ok(server) => {{
            let port = http.server_port(server);
            let client = spawn Client(
                url: f"http://127.0.0.1:{{port}}/retention",
                complete: 0,
            );
            client.fetch(0);
            let request = server.accept();
            for _ in 0..{frames} {{
                let checksum = request.method().len()
                    + request.path().len()
                    + request.header("Host").len()
                    + request.body("utf-8").len();
                println(checksum);
            }}
            match request.try_respond_text(200, "ok") {{
                Ok(_) => {{}},
                Err(_) => panic("HTTP retention response failed"),
            }}
            request.close();
            match await client.finished() {{
                Ok(done) => {{
                    if done != 1 {{
                        panic("HTTP retention client stopped early");
                    }}
                }},
                Err(_) => panic("HTTP retention client failed"),
            }}
            server.close();
        }},
        Err(_) => panic("HTTP retention listener failed"),
    }}
}}
"#
    )
}

fn http_response_source(frames: usize) -> String {
    format!(
        r#"
import std.net.http.http_client;
import std.net.http;

actor Client {{
    let url: string;
    let frames: i64;
    var complete: i64;

    receive fn fetch(unused: i64) {{
        http_client.set_timeout(5000);
        let response = http_client.get(url);
        for _ in 0..frames {{
            let checksum = response.body().len()
                + response.content_type().len()
                + response.header("Content-Type").len();
            println(checksum);
        }}
        response.close();
        complete = 1;
    }}

    receive fn finished() -> i64 {{
        complete
    }}
}}

fn main() {{
    match http.listen("127.0.0.1:0") {{
        Ok(server) => {{
            let port = http.server_port(server);
            let client = spawn Client(
                url: f"http://127.0.0.1:{{port}}/retention",
                frames: {frames},
                complete: 0,
            );
            client.fetch(0);
            let request = server.accept();
            match request.try_respond(
                200,
                "application/retention",
                "response-owner",
            ) {{
                Ok(_) => {{}},
                Err(_) => panic("HTTP retention response failed"),
            }}
            request.close();
            match await client.finished() {{
                Ok(done) => {{
                    if done != 1 {{
                        panic("HTTP retention client stopped early");
                    }}
                }},
                Err(_) => panic("HTTP retention client failed"),
            }}
            server.close();
        }},
        Err(_) => panic("HTTP retention listener failed"),
    }}
}}
"#
    )
}

fn websocket_message_source(frames: usize) -> String {
    format!(
        r#"
import std.net.websocket;

actor Client {{
    let url: string;
    let frames: i64;
    var complete: i64;

    receive fn send_frames(unused: i64) {{
        match websocket.connect(url) {{
            Ok(connection) => {{
                for _ in 0..frames {{
                    if connection.send_text("message-owner") != 0 {{
                        panic("WebSocket retention send failed");
                    }}
                }}
                connection.close();
                complete = 1;
            }},
            Err(_) => panic("WebSocket retention connect failed"),
        }}
    }}

    receive fn finished() -> i64 {{
        complete
    }}
}}

fn main() {{
    match websocket.listen("127.0.0.1:0") {{
        Ok(server) => {{
            let client = spawn Client(
                url: f"ws://127.0.0.1:{{server.port()}}/retention",
                frames: {frames},
                complete: 0,
            );
            client.send_frames(0);
            let connection = server.accept();
            for _ in 0..{frames} {{
                let message = connection.recv();
                println(message.text().len());
                message.close();
            }}
            match await client.finished() {{
                Ok(done) => {{
                    if done != 1 {{
                        panic("WebSocket retention client stopped early");
                    }}
                }},
                Err(_) => panic("WebSocket retention client failed"),
            }}
            connection.close();
            server.close();
        }},
        Err(_) => panic("WebSocket retention listener failed"),
    }}
}}
"#
    )
}

fn websocket_error_source(frames: usize) -> String {
    format!(
        r#"
import std.net.websocket;

fn main() {{
    match websocket.listen("not an address") {{
        Ok(server) => {{
            server.close();
            panic("invalid WebSocket address unexpectedly bound");
        }},
        Err(_) => {{
            for _ in 0..{frames} {{
                println(websocket.last_error().len());
            }}
        }},
    }}
}}
"#
    )
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_http_request_string_wrappers_have_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "http_request_strings",
        http_request_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_http_response_string_wrappers_have_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "http_response_strings",
        http_response_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_websocket_message_text_wrapper_has_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "websocket_message_text",
        websocket_message_source,
        std::convert::identity,
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_websocket_error_string_wrapper_has_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "websocket_error_string",
        websocket_error_source,
        std::convert::identity,
    );
}
