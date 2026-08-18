//! Inferred TCP/TLS/WebSocket attach must use concrete actor protocol IDs.
//!
//! Numeric `0, 1` attach arguments can compile and then trap asynchronously
//! when actor dispatch uses hashed protocol IDs. Imported method return types
//! also retain their defining module's bare handle spelling. This oracle
//! compiles all three inferred-return surfaces and proves each emitted runtime
//! call carries the same two IDs present in the concrete actor's dispatch
//! switch.

mod support;

use std::process::Command;

use tempfile::tempdir;

use support::{describe_output, hew_binary, repo_root, require_codegen};

const SOURCE: &str = r#"
import std.net;
import std.net.tls;
import std.net.websocket;

actor TcpProbe {
    receive fn on_data(data: bytes) {}
    receive fn on_close() {}
}

actor TlsProbe {
    receive fn on_data(data: bytes) {}
    receive fn on_close() {}
}

actor WebSocketProbe {
    receive fn on_message(text: string) {}
    receive fn on_close() {}
}

fn _observe_borrowed_bytes(data: bytes) {}

fn attach_tcp(listener: net.Listener, handler: LocalPid<TcpProbe>) {
    let conn = listener.accept();
    conn.attach(handler);
}

fn attach_tls(handler: LocalPid<TlsProbe>) {
    let stream = tls.connect("127.0.0.1", 443);
    stream.attach(handler);
}

fn attach_websocket(server: websocket.Server, handler: LocalPid<WebSocketProbe>) {
    let conn = server.accept();
    conn.attach(handler);
}

fn main() {}
"#;

/// `attach` transfers the connection to the runtime reactor.  A second close
/// from the original local would overlap that ownership and must be refused
/// before code generation; accepting it would schedule a stale scope-drop
/// against a handle now owned by active-mode shutdown.
const TCP_ATTACH_USE_AFTER_TRANSFER_SOURCE: &str = r"
import std.net;

actor TcpProbe {
    receive fn on_data(data: bytes) {}
    receive fn on_close() {}
}

fn attach_then_close(listener: net.Listener, handler: LocalPid<TcpProbe>) {
    let conn = listener.accept();
    conn.attach(handler);
    conn.close();
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

fn attach_ids(body: &str, callee: &str, id_ty: &str) -> [i64; 2] {
    let call = body
        .lines()
        .find(|line| line.contains(&format!("call i32 @{callee}(")))
        .unwrap_or_else(|| panic!("missing {callee} call:\n{body}"));
    let id_separator = format!(", {id_ty} ");
    let mut ids = call.split(&id_separator).skip(1).map(|field| {
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
fn inferred_transport_attach_emits_concrete_dispatch_ids() {
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
    let tcp_ids = attach_ids(
        function_body(&ir, "@attach_tcp("),
        "hew_tcp_attach_local",
        "i32",
    );
    let tls_ids = attach_ids(function_body(&ir, "@attach_tls("), "hew_tls_attach", "i64");
    let websocket_ids = attach_ids(
        function_body(&ir, "@attach_websocket("),
        "hew_ws_attach",
        "i64",
    );

    assert_ne!(tcp_ids, [0, 1], "TCP attach must not use ordinal IDs");
    assert_ne!(tls_ids, [0, 1], "TLS attach must not use ordinal IDs");
    assert_ne!(
        websocket_ids,
        [0, 1],
        "WebSocket attach must not use ordinal IDs"
    );
    assert_dispatch_contains(
        function_body(&ir, "@__hew_actor_dispatch_TcpProbe("),
        tcp_ids,
    );
    assert_dispatch_contains(
        function_body(&ir, "@__hew_actor_dispatch_TlsProbe("),
        tls_ids,
    );
    assert_dispatch_contains(
        function_body(&ir, "@__hew_actor_dispatch_WebSocketProbe("),
        websocket_ids,
    );

    let tcp_on_data = function_body(&ir, "@TcpProbe__recv__on_data(");
    assert!(
        tcp_on_data.contains("call void @hew_bytes_drop("),
        "copy-mode active transport delivery transfers the sole bytes owner \
         into the receive handler, whose cleanup must release it:\n{tcp_on_data}"
    );
    let ordinary_borrow = function_body(&ir, "@_observe_borrowed_bytes(");
    assert!(
        !ordinary_borrow.contains("call void @hew_bytes_drop("),
        "ordinary by-value bytes calls remain caller-owned borrows; the \
         active-handler ingress rule must not widen:\n{ordinary_borrow}"
    );
}

#[test]
fn tcp_attach_transfers_the_connection_and_refuses_a_second_close() {
    require_codegen();
    let dir = tempdir().expect("temporary emit directory");
    let source = dir.path().join("tcp_attach_use_after_transfer.hew");
    std::fs::write(&source, TCP_ATTACH_USE_AFTER_TRANSFER_SOURCE).expect("write Hew source");

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
        !output.status.success(),
        "conn.close() after conn.attach() must be rejected; emitted output:\n{}",
        describe_output(&output)
    );
    let diagnostic = describe_output(&output);
    assert!(
        diagnostic.contains("moved") || diagnostic.contains("consumed"),
        "failure must be ownership-based rather than an unrelated parse/type error:\n{diagnostic}"
    );
}
