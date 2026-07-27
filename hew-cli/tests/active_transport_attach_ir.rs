//! Typed TLS/WebSocket attach must use the concrete actor protocol IDs.
//!
//! Numeric `0, 1` attach arguments can compile and then trap asynchronously
//! when actor dispatch uses hashed protocol IDs. This oracle compiles both
//! typed method surfaces and proves each emitted runtime call carries the same
//! two IDs present in the concrete actor's dispatch switch.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r"
import std::net::tls;
import std::net::websocket;

actor TlsProbe {
    receive fn on_data(data: bytes) {}
    receive fn on_close() {}
}

actor WebSocketProbe {
    receive fn on_message(text: string) {}
    receive fn on_close() {}
}

fn attach_tls(stream: tls.TlsStream, handler: LocalPid<TlsProbe>) {
    stream.attach(handler);
}

fn attach_websocket(conn: websocket.Conn, handler: LocalPid<WebSocketProbe>) {
    conn.attach(handler);
}

fn main() {}
";

fn function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let start = ir
        .find(symbol)
        .unwrap_or_else(|| panic!("missing {symbol} in emitted IR"));
    let body = &ir[start..];
    let end = body
        .find("\n}")
        .map_or(body.len(), |closing_brace| closing_brace + 2);
    &body[..end]
}

fn attach_ids(body: &str, callee: &str) -> [i64; 2] {
    let call = body
        .lines()
        .find(|line| line.contains(&format!("call i32 @{callee}(")))
        .unwrap_or_else(|| panic!("missing {callee} call:\n{body}"));
    let mut ids = call.split(", i64 ").skip(1).map(|field| {
        field
            .trim_end_matches(')')
            .trim()
            .parse::<i64>()
            .unwrap_or_else(|err| panic!("invalid protocol ID in `{call}`: {err}"))
    });
    let first = ids.next().expect("attach call must carry data/message ID");
    let second = ids.next().expect("attach call must carry close ID");
    assert!(
        ids.next().is_none(),
        "attach call carries excess IDs: {call}"
    );
    [first, second]
}

fn assert_dispatch_contains(dispatch: &str, ids: [i64; 2]) {
    for id in ids {
        assert!(
            dispatch.contains(&format!("i32 {id}, label %msg_")),
            "runtime attach ID {id} is absent from concrete actor dispatch:\n{dispatch}"
        );
    }
}

#[test]
fn typed_tls_and_websocket_attach_emit_concrete_dispatch_ids() {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let source = dir.path().join("active_transport_attach.hew");
    std::fs::write(&source, SOURCE).expect("write Hew source");

    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.path().to_str().expect("emit directory is UTF-8"),
            source.to_str().expect("source path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("run hew compile");
    assert!(
        output.status.success(),
        "typed attach fixture must compile:\n{}",
        describe_output(&output)
    );

    let ir = std::fs::read_to_string(dir.path().join("active_transport_attach.ll"))
        .expect("read emitted LLVM IR");
    let tls_ids = attach_ids(function_body(&ir, "@attach_tls("), "hew_tls_attach");
    let websocket_ids = attach_ids(function_body(&ir, "@attach_websocket("), "hew_ws_attach");

    assert_ne!(tls_ids, [0, 1], "TLS attach must not use ordinal IDs");
    assert_ne!(
        websocket_ids,
        [0, 1],
        "WebSocket attach must not use ordinal IDs"
    );
    assert_dispatch_contains(
        function_body(&ir, "@__hew_actor_dispatch_TlsProbe("),
        tls_ids,
    );
    assert_dispatch_contains(
        function_body(&ir, "@__hew_actor_dispatch_WebSocketProbe("),
        websocket_ids,
    );
}
