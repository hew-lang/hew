//! End-to-end protocol smoke test for `hew-lsp`.
//!
//! Spawns the language server as a child process and drives a real LSP session
//! over stdio: `initialize` → `initialized` → `textDocument/didOpen`, then waits
//! for the `textDocument/publishDiagnostics` notification. The session is a
//! complete client/server handshake, so a regression that breaks the JSON-RPC
//! framing, the capability advertisement, or the diagnostics pipeline fails here
//! instead of slipping past the `--version` check.
//!
//! The binary defaults to the freshly built `hew-lsp` (`CARGO_BIN_EXE_hew-lsp`),
//! but CI points `HEW_LSP_BIN` at the release artifact so the shipped binary is
//! exercised directly.

use std::io::{BufRead, BufReader, Read, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::time::{Duration, Instant};

use serde_json::{json, Value};

/// Overall budget for the handshake + first diagnostics publish. Generous so the
/// 100ms analysis debounce and a cold child process never make CI flaky.
const SESSION_BUDGET: Duration = Duration::from_secs(30);

/// A source file that always produces a type-checker diagnostic: `undefined_var`
/// is unresolved, so the analysis pipeline must emit at least one error.
const BAD_SOURCE: &str = "fn main() -> i32 { undefined_var }\n";

fn server_binary() -> String {
    std::env::var("HEW_LSP_BIN").unwrap_or_else(|_| env!("CARGO_BIN_EXE_hew-lsp").to_string())
}

/// Frame and send a single JSON-RPC message with the LSP `Content-Length` header.
fn send(stdin: &mut ChildStdin, message: &Value) {
    let body = serde_json::to_vec(message).expect("serialize message");
    write!(stdin, "Content-Length: {}\r\n\r\n", body.len()).expect("write header");
    stdin.write_all(&body).expect("write body");
    stdin.flush().expect("flush stdin");
}

/// Read one framed JSON-RPC message; returns `None` on clean EOF.
fn read_message(reader: &mut BufReader<ChildStdout>) -> Option<Value> {
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        if reader.read_line(&mut line).ok()? == 0 {
            return None; // EOF
        }
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            break; // blank line terminates headers
        }
        if let Some(value) = trimmed.strip_prefix("Content-Length:") {
            content_length = value.trim().parse().ok();
        }
    }
    let len = content_length?;
    let mut body = vec![0u8; len];
    reader.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

/// Spawn a background reader so the main thread can apply a wall-clock deadline.
fn spawn_reader(stdout: ChildStdout) -> Receiver<Value> {
    let (tx, rx) = mpsc::channel();
    std::thread::spawn(move || {
        let mut reader = BufReader::new(stdout);
        while let Some(message) = read_message(&mut reader) {
            if tx.send(message).is_err() {
                break;
            }
        }
    });
    rx
}

fn recv_until<F: Fn(&Value) -> bool>(rx: &Receiver<Value>, deadline: Instant, pred: F) -> Value {
    loop {
        let remaining = deadline
            .checked_duration_since(Instant::now())
            .unwrap_or_default();
        match rx.recv_timeout(remaining) {
            Ok(message) if pred(&message) => return message,
            Ok(_) => {}
            Err(RecvTimeoutError::Timeout) => panic!("timed out waiting for expected LSP message"),
            Err(RecvTimeoutError::Disconnected) => panic!("hew-lsp exited before expected message"),
        }
    }
}

fn shutdown(child: &mut Child, stdin: &mut ChildStdin) {
    send(stdin, &json!({"jsonrpc":"2.0","id":99,"method":"shutdown"}));
    send(stdin, &json!({"jsonrpc":"2.0","method":"exit"}));
    // Don't depend on graceful exit: reap deterministically so the test can't hang.
    let _ = child.kill();
    let _ = child.wait();
}

#[test]
fn lsp_initialize_didopen_diagnostics_roundtrip() {
    let mut child = Command::new(server_binary())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn hew-lsp");

    let mut stdin = child.stdin.take().expect("child stdin");
    let rx = spawn_reader(child.stdout.take().expect("child stdout"));
    let deadline = Instant::now() + SESSION_BUDGET;
    let uri = "file:///protocol_smoke/main.hew";

    // 1. initialize → expect a valid handshake advertising capabilities.
    send(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": { "processId": null, "capabilities": {}, "rootUri": null }
        }),
    );
    let init = recv_until(&rx, deadline, |m| m.get("id") == Some(&json!(1)));
    let caps = &init["result"]["capabilities"];
    assert!(
        !caps["textDocumentSync"].is_null(),
        "initialize must advertise textDocumentSync, got: {init}"
    );
    assert!(
        !caps["definitionProvider"].is_null(),
        "initialize must advertise definitionProvider, got: {init}"
    );

    // 2. initialized + didOpen of a file with a known type error.
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","method":"initialized","params":{}}),
    );
    send(
        &mut stdin,
        &json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": { "textDocument": {
                "uri": uri, "languageId": "hew", "version": 1, "text": BAD_SOURCE
            }}
        }),
    );

    // 3. expect publishDiagnostics for our document with at least one error.
    let publish = recv_until(&rx, deadline, |m| {
        m.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && m["params"]["uri"] == json!(uri)
            && m["params"]["diagnostics"]
                .as_array()
                .is_some_and(|d| !d.is_empty())
    });
    let diags = publish["params"]["diagnostics"].as_array().unwrap();
    assert!(
        diags.iter().any(|d| d["source"] == json!("hew-types")),
        "expected a hew-types diagnostic, got: {diags:?}"
    );

    shutdown(&mut child, &mut stdin);
}
