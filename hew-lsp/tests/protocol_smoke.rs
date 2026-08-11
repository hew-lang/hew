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

/// RAII guard that kills and reaps the spawned server on every exit path —
/// including a panicking assertion or `recv_until` timeout that fires before the
/// polite `shutdown()` runs. Without it, a failing run would leak an orphaned
/// `hew-lsp` child. (#2298)
struct ServerProcess {
    child: Child,
}

impl Drop for ServerProcess {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Send the polite shutdown/exit pair; the final kill+reap is the guard's `Drop`.
fn shutdown(stdin: &mut ChildStdin) {
    send(stdin, &json!({"jsonrpc":"2.0","id":99,"method":"shutdown"}));
    send(stdin, &json!({"jsonrpc":"2.0","method":"exit"}));
}

#[test]
fn lsp_initialize_didopen_diagnostics_roundtrip() {
    let mut server = ServerProcess {
        child: Command::new(server_binary())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn hew-lsp"),
    };

    let mut stdin = server.child.stdin.take().expect("child stdin");
    let rx = spawn_reader(server.child.stdout.take().expect("child stdout"));
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

    shutdown(&mut stdin);
}

// ── MIR-stage lint surfacing (#2176) ─────────────────────────────────
//
// `dead_store` rides the MIR liveness pass, not the HIR checker sweep, so
// before #2176 it reached the CLI only. These tests drive a real LSP session
// and assert the lint arrives over the wire.
//
// Presence alone would be a weak claim: a pass that flagged every buffer would
// also satisfy it. Each positive is therefore paired with a CONTROL that
// differs only in the property under test, and the controls assert the ABSENCE
// of a `dead_store` diagnostic while a plain type error still publishes — so a
// silent control cannot pass by the pipeline being broken.

/// Fixtures mirror `hew-cli/tests/lint_pass_e2e.rs` so the two surfaces are
/// held to one definition of the lint.
const LSP_DEAD_STORE: &str =
    "fn f() -> i64 {\n    var x = 5;\n    x = 6;\n    x\n}\nfn main() {\n    let _ = f();\n}\n";

/// CONTROL 1 — identical but for the in-source allow directive.
const LSP_DEAD_STORE_SUPPRESSED: &str =
    "fn f() -> i64 {\n    // hew:allow(dead_store)\n    var x = 5;\n    x = 6;\n    x\n}\nfn main() {\n    let _ = f();\n}\n";

/// CONTROL 2 — a normal accumulator loop where every store IS read. Proves the
/// pass discriminates on liveness rather than firing on any `var`.
const LSP_FOR_RANGE_CLEAN: &str =
    "fn sum(n: i64) -> i64 {\n    var total = 0;\n    for i in 0..n {\n        total = total + i;\n    }\n    total\n}\nfn main() {\n    let _ = sum(5);\n}\n";

/// Drive one `initialize` → `didOpen` session and return the published
/// diagnostics for the document.
fn diagnostics_for(source: &str, uri: &str) -> Vec<Value> {
    let mut server = ServerProcess {
        child: Command::new(server_binary())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .expect("spawn hew-lsp"),
    };
    let mut stdin = server.child.stdin.take().expect("child stdin");
    let rx = spawn_reader(server.child.stdout.take().expect("child stdout"));
    let deadline = Instant::now() + SESSION_BUDGET;

    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","id":1,"method":"initialize",
                "params":{"processId":null,"capabilities":{},"rootUri":null}}),
    );
    recv_until(&rx, deadline, |m| m.get("id") == Some(&json!(1)));
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","method":"initialized","params":{}}),
    );
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","method":"textDocument/didOpen",
                "params":{"textDocument":{"uri":uri,"languageId":"hew","version":1,"text":source}}}),
    );
    let publish = recv_until(&rx, deadline, |m| {
        m.get("method") == Some(&json!("textDocument/publishDiagnostics"))
            && m["params"]["uri"] == json!(uri)
    });
    let diags = publish["params"]["diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    shutdown(&mut stdin);
    diags
}

fn dead_store_diagnostics(diags: &[Value]) -> Vec<&Value> {
    diags
        .iter()
        .filter(|d| d["code"] == json!("dead_store"))
        .collect()
}

#[test]
fn lsp_publishes_mir_dead_store_as_warning() {
    let diags = diagnostics_for(LSP_DEAD_STORE, "file:///mir_lint/dead_store.hew");
    let found = dead_store_diagnostics(&diags);
    assert_eq!(
        found.len(),
        1,
        "expected exactly one dead_store diagnostic, got: {diags:?}"
    );
    let d = found[0];
    // Severity is the crux of #2176: the HIR path maps everything to ERROR
    // unconditionally, and a lint must not fail a buffer the compiler accepts.
    assert_eq!(
        d["severity"],
        json!(2),
        "dead_store must publish as WARNING (2), got: {d}"
    );
    assert_eq!(d["source"], json!("hew-mir"), "unexpected source: {d}");
    // The span must point at the dead store on line 1 (0-based), not line 0.
    assert_eq!(
        d["range"]["start"]["line"],
        json!(1),
        "dead_store span should cover `var x = 5;`, got: {d}"
    );
}

#[test]
fn lsp_mir_lint_respects_in_source_allow_directive() {
    let diags = diagnostics_for(LSP_DEAD_STORE_SUPPRESSED, "file:///mir_lint/suppressed.hew");
    assert!(
        dead_store_diagnostics(&diags).is_empty(),
        "// hew:allow(dead_store) must suppress the LSP diagnostic, got: {diags:?}"
    );
}

#[test]
fn lsp_mir_lint_stays_silent_on_live_stores() {
    let diags = diagnostics_for(LSP_FOR_RANGE_CLEAN, "file:///mir_lint/clean.hew");
    assert!(
        dead_store_diagnostics(&diags).is_empty(),
        "a normal accumulator loop must not trip dead_store, got: {diags:?}"
    );
}

/// Guards the controls above: proves this harness DOES publish for these URIs,
/// so an empty control is discrimination and not a dead pipeline.
#[test]
fn lsp_control_harness_still_publishes_diagnostics() {
    let diags = diagnostics_for(BAD_SOURCE, "file:///mir_lint/harness_control.hew");
    assert!(
        !diags.is_empty(),
        "harness must still publish for a known-bad source"
    );
}
