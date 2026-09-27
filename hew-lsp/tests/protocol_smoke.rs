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
use std::path::PathBuf;
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

struct TestProject(PathBuf);

impl Drop for TestProject {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn lsp_test_failure_diagnostic_and_seeded_rerun_roundtrip() {
    let unique = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let root = std::env::temp_dir().join(format!("hew-lsp-test-{}-{unique}", std::process::id()));
    std::fs::create_dir(&root).expect("create test project");
    let _project = TestProject(root.clone());
    let file = root.join("main.hew");
    let failing = "#[test]\nfn fails() { assert(1 == 2); }\n";
    std::fs::write(&file, failing).expect("write test source");
    // Exercise an editor URI that names the same file differently from the
    // canonical selector returned by test discovery.
    #[cfg(unix)]
    let editor_file = {
        let alias = root.join("editor-alias");
        std::os::unix::fs::symlink(&root, &alias).expect("create editor path alias");
        alias.join("main.hew")
    };
    #[cfg(not(unix))]
    let editor_file = file.clone();
    let uri = url::Url::from_file_path(&editor_file)
        .expect("test file URI")
        .to_string();
    let root_uri = url::Url::from_file_path(&root)
        .expect("test root URI")
        .to_string();

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
    let deadline = Instant::now() + Duration::from_mins(1);
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","id":1,"method":"initialize",
        "params":{"processId":null,"capabilities":{},"rootUri":root_uri}}),
    );
    recv_until(&rx, deadline, |m| m.get("id") == Some(&json!(1)));
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","method":"initialized","params":{}}),
    );
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","method":"textDocument/didOpen",
        "params":{"textDocument":{"uri":uri,"languageId":"hew","version":1,"text":failing}}}),
    );
    recv_until(&rx, deadline, |m| {
        m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri
    });

    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","id":2,"method":"hew/tests",
        "params":{"textDocument":{"uri":uri}}}),
    );
    let inventory = recv_until(&rx, deadline, |m| m.get("id") == Some(&json!(2)));
    let selector = inventory["result"][0]["selector"]
        .as_str()
        .expect("discovered test selector");
    send(
        &mut stdin,
        &json!({"jsonrpc":"2.0","id":3,"method":"workspace/executeCommand",
        "params":{"command":"hew.runTest","arguments":[selector]}}),
    );
    let failed = recv_until(&rx, deadline, |m| {
        m["method"] == "textDocument/publishDiagnostics"
            && m["params"]["uri"] == uri
            && m["params"]["diagnostics"]
                .as_array()
                .is_some_and(|diagnostics| diagnostics.iter().any(|d| d["source"] == "hew test"))
    });
    let diagnostic = failed["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|diagnostic| diagnostic["source"] == "hew test")
        .unwrap();
    assert_eq!(diagnostic["range"]["start"]["line"], 1);
    assert_eq!(diagnostic["range"]["start"]["character"], 13);
    assert!(diagnostic["message"].as_str().unwrap().contains("left: 1"));
    assert!(diagnostic["message"].as_str().unwrap().contains("right: 2"));

    assert_seeded_rerun_and_edit_clear(&mut stdin, &rx, deadline, &uri);
    shutdown(&mut stdin);
}

fn assert_seeded_rerun_and_edit_clear(
    stdin: &mut ChildStdin,
    rx: &Receiver<serde_json::Value>,
    deadline: Instant,
    uri: &str,
) {
    send(
        stdin,
        &json!({"jsonrpc":"2.0","id":4,"method":"textDocument/codeLens",
        "params":{"textDocument":{"uri":uri}}}),
    );
    let lenses = recv_until(rx, deadline, |m| m.get("id") == Some(&json!(4)));
    let rerun = lenses["result"]
        .as_array()
        .expect("code lenses")
        .iter()
        .find(|lens| {
            lens["command"]["title"]
                .as_str()
                .is_some_and(|title| title.contains("Rerun with seed"))
        })
        .expect("seeded rerun lens");
    assert!(rerun["command"]["arguments"][0]["seed"]
        .as_str()
        .and_then(|seed| seed.parse::<u64>().ok())
        .is_some());

    let passing = "#[test]\nfn fails() { assert(1 == 1); }\n";
    send(
        stdin,
        &json!({"jsonrpc":"2.0","method":"textDocument/didChange",
        "params":{"textDocument":{"uri":uri,"version":2},"contentChanges":[{"text":passing}]}}),
    );
    let cleared = recv_until(rx, deadline, |m| {
        m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri
    });
    assert!(cleared["params"]["diagnostics"]
        .as_array()
        .is_some_and(|diagnostics| diagnostics.iter().all(|d| d["source"] != "hew test")));
}

// The MIR dead-store lint went with the legacy MIR pipeline, so there is no
// lint to surface over the wire. The harness control below stays: it proves a
// real LSP session still publishes diagnostics for these URIs.

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

/// Proves a real LSP session publishes diagnostics for a known-bad source.
#[test]
fn lsp_control_harness_still_publishes_diagnostics() {
    let diags = diagnostics_for(BAD_SOURCE, "file:///lsp/harness_control.hew");
    assert!(
        !diags.is_empty(),
        "harness must still publish for a known-bad source"
    );
}
