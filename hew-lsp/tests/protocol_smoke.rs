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

use std::collections::{BTreeMap, BTreeSet};
use std::io::{BufRead, BufReader, Read, Write};
use std::path::{Path, PathBuf};
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

impl TestProject {
    fn new(label: &str) -> Self {
        let unique = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let root =
            std::env::temp_dir().join(format!("hew-lsp-{label}-{}-{unique}", std::process::id()));
        std::fs::create_dir(&root).expect("create test project");
        Self(root)
    }

    fn write(&self, relative: &str, source: &str) -> PathBuf {
        let path = self.0.join(relative);
        std::fs::create_dir_all(path.parent().expect("source parent"))
            .expect("create source directory");
        std::fs::write(&path, source).expect("write project source");
        path
    }
}

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

/// Rename scenarios use the same framing, reader and process guard as the smoke
/// tests above. Keep the client's unsaved buffers separate from disk, as an
/// editor does when it applies a `WorkspaceEdit` without saving the documents.
struct RenameSession {
    _server: ServerProcess,
    stdin: ChildStdin,
    rx: Receiver<Value>,
    next_id: u64,
    buffers: BTreeMap<String, (String, i32)>,
    responses: Vec<Value>,
}

fn file_uri(path: &Path) -> String {
    url::Url::from_file_path(path)
        .expect("file URI")
        .to_string()
}

impl RenameSession {
    fn new(root: Option<&TestProject>, folders: &[&TestProject]) -> Self {
        let mut server = ServerProcess {
            child: Command::new(server_binary())
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::null())
                .spawn()
                .expect("spawn hew-lsp"),
        };
        let stdin = server.child.stdin.take().expect("child stdin");
        let rx = spawn_reader(server.child.stdout.take().expect("child stdout"));
        let mut session = Self {
            _server: server,
            stdin,
            rx,
            next_id: 1,
            buffers: BTreeMap::new(),
            responses: Vec::new(),
        };
        let workspace_folders: Vec<Value> = folders
            .iter()
            .enumerate()
            .map(|(index, project)| {
                json!({"uri":file_uri(&project.0),"name":format!("project-{index}")})
            })
            .collect();
        session.request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": root.map(|project| file_uri(&project.0)),
                "workspaceFolders": if folders.is_empty() { Value::Null } else { json!(workspace_folders) },
                "capabilities": {"workspace":{"workspaceEdit":{"documentChanges":true}}}
            }),
        );
        send(
            &mut session.stdin,
            &json!({"jsonrpc":"2.0","method":"initialized","params":{}}),
        );
        session
    }

    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.start_request(method, params);
        self.wait_for_responses(&[id], SESSION_BUDGET)
    }

    fn start_request(&mut self, method: &str, params: Value) -> u64 {
        let id = self.next_id;
        self.next_id += 1;
        let mut request = json!({"jsonrpc":"2.0","id":id,"method":method});
        request["params"] = params;
        send(&mut self.stdin, &request);
        id
    }

    fn wait_for_responses(&mut self, ids: &[u64], budget: Duration) -> Value {
        let matches = |message: &Value| message["id"].as_u64().is_some_and(|id| ids.contains(&id));
        if let Some(index) = self.responses.iter().position(matches) {
            return self.responses.remove(index);
        }
        let deadline = Instant::now() + budget;
        loop {
            let remaining = deadline
                .checked_duration_since(Instant::now())
                .unwrap_or_default();
            match self.rx.recv_timeout(remaining) {
                Ok(message) if matches(&message) => return message,
                Ok(message) if message["id"].as_u64().is_some() => self.responses.push(message),
                Ok(_) => {}
                Err(RecvTimeoutError::Timeout) => {
                    panic!("timed out waiting for response IDs {ids:?}")
                }
                Err(RecvTimeoutError::Disconnected) => {
                    panic!("hew-lsp exited before response IDs {ids:?}")
                }
            }
        }
    }

    fn open(&mut self, path: &Path, source: &str, version: i32) -> Value {
        let uri = file_uri(path);
        self.open_uri(&uri, source, version)
    }

    fn open_uri(&mut self, uri: &str, source: &str, version: i32) -> Value {
        self.buffers
            .insert(uri.to_string(), (source.to_string(), version));
        send(
            &mut self.stdin,
            &json!({"jsonrpc":"2.0","method":"textDocument/didOpen",
                "params":{"textDocument":{"uri":uri,"languageId":"hew",
                    "version":version,"text":source}}}),
        );
        recv_until(&self.rx, Instant::now() + SESSION_BUDGET, |message| {
            message["method"] == "textDocument/publishDiagnostics"
                && message["params"]["uri"] == uri
        })
    }

    fn change(&mut self, path: &Path) -> Value {
        let uri = file_uri(path);
        let (source, version) = self.buffers.get(&uri).expect("open document");
        send(
            &mut self.stdin,
            &json!({"jsonrpc":"2.0","method":"textDocument/didChange",
                "params":{"textDocument":{"uri":uri,"version":version},
                    "contentChanges":[{"text":source}]}}),
        );
        recv_until(&self.rx, Instant::now() + SESSION_BUDGET, |message| {
            message["method"] == "textDocument/publishDiagnostics"
                && message["params"]["uri"] == uri
        })
    }

    fn change_without_waiting(&mut self, path: &Path, source: &str, version: i32) {
        let uri = file_uri(path);
        self.change_uri_without_waiting(&uri, source, version);
    }

    fn change_uri_without_waiting(&mut self, uri: &str, source: &str, version: i32) {
        self.buffers
            .insert(uri.to_string(), (source.to_string(), version));
        send(
            &mut self.stdin,
            &json!({"jsonrpc":"2.0","method":"textDocument/didChange",
                "params":{"textDocument":{"uri":uri,"version":version},
                    "contentChanges":[{"text":source}]}}),
        );
    }

    fn prepare(&mut self, path: &Path, source: &str, needle: &str) -> Value {
        self.request(
            "textDocument/prepareRename",
            json!({"textDocument":{"uri":file_uri(path)},
                "position":position_of(source, needle)}),
        )
    }

    fn rename(&mut self, path: &Path, source: &str, needle: &str, new_name: &str) -> Value {
        self.request(
            "textDocument/rename",
            json!({"textDocument":{"uri":file_uri(path)},
                "position":position_of(source, needle),"newName":new_name}),
        )
    }

    fn start_rename(&mut self, path: &Path, source: &str, needle: &str, new_name: &str) -> u64 {
        self.start_request(
            "textDocument/rename",
            json!({"textDocument":{"uri":file_uri(path)},
                "position":position_of(source, needle),"newName":new_name}),
        )
    }

    fn source(&self, path: &Path) -> String {
        self.buffers.get(&file_uri(path)).map_or_else(
            || std::fs::read_to_string(path).expect("read closed source"),
            |(source, _)| source.clone(),
        )
    }

    fn apply(&mut self, response: &Value) -> BTreeSet<String> {
        assert!(response["error"].is_null(), "rename failed: {response}");
        let edit = &response["result"];
        assert!(!edit.is_null(), "rename must return edits: {response}");
        let mut changes = BTreeMap::<String, Vec<Value>>::new();
        if let Some(documents) = edit["changes"].as_object() {
            for (uri, edits) in documents {
                changes.insert(uri.clone(), edits.as_array().expect("text edits").clone());
            }
        }
        if let Some(documents) = edit["documentChanges"].as_array() {
            for document in documents {
                let uri = document["textDocument"]["uri"]
                    .as_str()
                    .expect("text document edit URI");
                if let Some((_, version)) = self.buffers.get(uri) {
                    assert_eq!(document["textDocument"]["version"], *version);
                } else {
                    assert!(
                        document["textDocument"]["version"].is_null(),
                        "closed document has no client version: {document}"
                    );
                }
                assert!(
                    changes
                        .insert(
                            uri.to_string(),
                            document["edits"].as_array().expect("text edits").clone(),
                        )
                        .is_none(),
                    "duplicate document edit: {uri}"
                );
            }
        }
        assert!(
            !changes.is_empty(),
            "rename must return a nonempty edit: {response}"
        );
        let touched = changes.keys().cloned().collect();
        for (uri, edits) in changes {
            if let Some((source, version)) = self.buffers.get_mut(&uri) {
                *source = apply_text_edits(source, &edits);
                *version += 1;
            } else {
                let path = url::Url::parse(&uri)
                    .expect("edit URI")
                    .to_file_path()
                    .expect("closed edit file path");
                let updated = apply_text_edits(&self.source(&path), &edits);
                std::fs::write(path, updated).expect("apply closed-file edits");
            }
        }
        touched
    }
}

impl Drop for RenameSession {
    fn drop(&mut self) {
        shutdown(&mut self.stdin);
    }
}

fn position_of(source: &str, needle: &str) -> Value {
    let offset = source.find(needle).expect("rename cursor substring");
    let prefix = &source[..offset];
    let line = prefix.bytes().filter(|byte| *byte == b'\n').count();
    let column = prefix.rsplit('\n').next().unwrap().encode_utf16().count();
    json!({"line":line,"character":column})
}

fn position_offset(source: &str, position: &Value) -> usize {
    let line = usize::try_from(position["line"].as_u64().expect("line")).unwrap();
    let character = usize::try_from(position["character"].as_u64().expect("character")).unwrap();
    let mut line_start = 0;
    for _ in 0..line {
        line_start += source[line_start..].find('\n').expect("line exists") + 1;
    }
    let mut utf16 = 0;
    for (offset, ch) in source[line_start..].char_indices() {
        if utf16 == character {
            return line_start + offset;
        }
        assert_ne!(ch, '\n', "character lies beyond line");
        utf16 += ch.len_utf16();
        assert!(utf16 <= character, "edit splits a UTF-16 code point");
    }
    assert_eq!(utf16, character, "character lies beyond document");
    source.len()
}

fn apply_text_edits(source: &str, edits: &[Value]) -> String {
    let mut replacements: Vec<_> = edits
        .iter()
        .map(|edit| {
            (
                position_offset(source, &edit["range"]["start"]),
                position_offset(source, &edit["range"]["end"]),
                edit["newText"].as_str().expect("replacement text"),
            )
        })
        .collect();
    replacements.sort_by_key(|replacement| std::cmp::Reverse(replacement.0));
    for pair in replacements.windows(2) {
        assert!(pair[1].1 <= pair[0].0, "overlapping rename edits: {pair:?}");
    }
    let mut updated = source.to_string();
    for (start, end, replacement) in replacements {
        assert!(start <= end, "reversed rename range");
        updated.replace_range(start..end, replacement);
    }
    updated
}

fn assert_clean(publish: &Value) {
    let diagnostics = publish["params"]["diagnostics"]
        .as_array()
        .expect("published diagnostics");
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic["severity"] != 1),
        "renamed source must type-check: {diagnostics:?}"
    );
}

fn assert_rename_refused(response: &Value) {
    assert_eq!(
        response["error"]["code"], -32803,
        "atomic refusal: {response}"
    );
    assert!(
        response.get("result").is_none(),
        "refusal must not return partial edits: {response}"
    );
    assert!(response["error"]["message"]
        .as_str()
        .is_some_and(|message| !message.is_empty()));
}

#[cfg(unix)]
fn link_workspace_boundaries(project: &TestProject, outside: &TestProject) {
    std::os::unix::fs::symlink(&outside.0, project.0.join("linked"))
        .expect("create outside directory symlink");
    std::os::unix::fs::symlink(&project.0, project.0.join("cycle"))
        .expect("create workspace cycle");
}

#[test]
fn lsp_exported_function_rename_updates_closed_import_forms_by_identity() {
    let project = TestProject::new("rename-imports");
    let util_source = "pub fn greet() -> i32 { 1 }\nfn wrapper() -> i32 { greet() }\n";
    let util = project.write("util.hew", util_source);
    let whole = project.write(
        "whole.hew",
        "import util;\nfn main() { println(util.greet()); }\n",
    );
    let module_alias = project.write("module_alias.hew", "import util as library;\ntype Holder { greet: i32; }\nfn main() { let welcome = 7; println(library.greet()); println(welcome); }\nfn shadowed(library: Holder) -> i32 { library.greet }\n");
    let named = project.write("named.hew", "import util.{ greet };\nfn main() { println(greet()); }\nfn shadowed() -> i32 { let greet = 8; greet }\n");
    let named_alias = project.write(
        "named_alias.hew",
        "import util.{ greet as call };\nfn main() { println(call()); }\n",
    );
    let self_alias = project.write(
        "self_alias.hew",
        "import util.{ greet as greet };\nfn main() { println(greet()); }\n",
    );
    // Equal declaration offsets and spellings in another physical module must
    // not merge symbol identities across separate analyses.
    let other_source = "pub fn greet() -> i32 { 2 }\n";
    let other = project.write("other.hew", other_source);
    let other_consumer_source = "import other;\nfn main() { println(other.greet()); }\n";
    let other_consumer = project.write("other_consumer.hew", other_consumer_source);
    let unrelated_source = "fn broken() -> i32 { missing }\n";
    let unrelated = project.write("unrelated.hew", unrelated_source);
    let excluded_source = "import util.{ greet };\nfn welcome() -> i32 { greet() }\n";
    let worktree = project.write("worktrees/other/main.hew", excluded_source);
    let outside = TestProject::new("rename-outside");
    let outside_source = "pub fn greet() -> i32 { 3 }\n";
    let outside_file = outside.write("util.hew", outside_source);
    #[cfg(unix)]
    link_workspace_boundaries(&project, &outside);

    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&util, util_source, 1));
    let bad = session.open(&unrelated, unrelated_source, 1);
    assert!(bad["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .any(|d| d["severity"] == 1));
    assert_clean(&session.open(&outside_file, outside_source, 1));
    let response = session.rename(&util, util_source, "greet", "welcome");
    let touched = session.apply(&response);
    assert_eq!(
        touched,
        [
            &util,
            &whole,
            &module_alias,
            &named,
            &named_alias,
            &self_alias
        ]
        .into_iter()
        .map(|path| file_uri(path))
        .collect()
    );
    assert_eq!(
        session.source(&util),
        "pub fn welcome() -> i32 { 1 }\nfn wrapper() -> i32 { welcome() }\n"
    );
    assert_eq!(
        session.source(&whole),
        "import util;\nfn main() { println(util.welcome()); }\n"
    );
    assert_eq!(
        session.source(&module_alias),
        "import util as library;\ntype Holder { greet: i32; }\nfn main() { let welcome = 7; println(library.welcome()); println(welcome); }\nfn shadowed(library: Holder) -> i32 { library.greet }\n"
    );
    assert_eq!(
        session.source(&named),
        "import util.{ welcome };\nfn main() { println(welcome()); }\nfn shadowed() -> i32 { let greet = 8; greet }\n"
    );
    assert_eq!(
        session.source(&named_alias),
        "import util.{ welcome as call };\nfn main() { println(call()); }\n"
    );
    assert_eq!(
        session.source(&self_alias),
        "import util.{ welcome as greet };\nfn main() { println(greet()); }\n"
    );
    assert_eq!(session.source(&other), other_source);
    assert_eq!(session.source(&other_consumer), other_consumer_source);
    assert_eq!(session.source(&unrelated), unrelated_source);
    assert_eq!(session.source(&worktree), excluded_source);
    assert_eq!(session.source(&outside_file), outside_source);

    // Apply the returned edits, then analyse the resulting programs through a
    // fresh protocol session with the definition's unsaved buffer first.
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&util, &session.source(&util), 2));
    for path in [
        &whole,
        &module_alias,
        &named,
        &named_alias,
        &self_alias,
        &other_consumer,
    ] {
        assert_clean(&verification.open(path, &session.source(path), 1));
    }
}

#[test]
fn lsp_exported_function_rename_from_qualified_call_uses_unsaved_workspace_buffers() {
    let project = TestProject::new("rename-unsaved");
    let sibling = TestProject::new("rename-second-root");
    let legacy_root = TestProject::new("rename-root-uri-fallback");
    let saved_util = "pub fn greet() -> i32 { 1 }\n";
    let unsaved_util =
        "// unsaved definition\nfn helper() -> i32 { 7 }\npub fn greet() -> i32 { helper() }\n";
    let util = project.write("util.hew", saved_util);
    let saved_main = "import util;\nfn main() { println(util.greet()); }\n";
    let unsaved_main = "// unsaved consumer\nimport util as library;\nfn main() { let salute = 9; println(library.greet()); println(library.greet()); println(salute); }\n";
    let main = project.write("main.hew", saved_main);
    let closed = project.write(
        "closed.hew",
        "import util.{ greet as call };\nfn closed() -> i32 { call() }\n",
    );
    let sibling_util = sibling.write("util.hew", saved_util);
    let sibling_source = "import util;\nfn main() { println(util.greet()); }\n";
    let sibling_main = sibling.write("main.hew", sibling_source);
    let outside_source = "pub fn greet() -> i32 { 4 }\n";
    let outside = legacy_root.write("util.hew", outside_source);
    let mut session = RenameSession::new(Some(&legacy_root), &[&project, &sibling]);
    assert_clean(&session.open(&util, saved_util, 1));
    assert_clean(&session.open(&main, saved_main, 1));
    assert_clean(&session.open(&outside, outside_source, 1));
    let prepared = session.prepare(&main, saved_main, "greet");
    assert!(
        prepared["error"].is_null() && !prepared["result"].is_null(),
        "qualified exported function must be renameable: {prepared}"
    );
    // Request immediately after didChange: waiting for publishDiagnostics here
    // would hide a stale debounced-analysis snapshot and permit wrong ranges.
    session.change_without_waiting(&util, unsaved_util, 7);
    session.change_without_waiting(&main, unsaved_main, 12);
    let response = session.rename(&main, unsaved_main, "greet", "salute");
    let touched = session.apply(&response);
    assert_eq!(
        touched,
        [&util, &main, &closed]
            .into_iter()
            .map(|path| file_uri(path))
            .collect()
    );
    assert_eq!(
        session.source(&util),
        "// unsaved definition\nfn helper() -> i32 { 7 }\npub fn salute() -> i32 { helper() }\n"
    );
    assert_eq!(
        session.source(&main),
        "// unsaved consumer\nimport util as library;\nfn main() { let salute = 9; println(library.salute()); println(library.salute()); println(salute); }\n"
    );
    assert_eq!(
        session.source(&closed),
        "import util.{ salute as call };\nfn closed() -> i32 { call() }\n"
    );
    assert_eq!(
        std::fs::read_to_string(&util).unwrap(),
        saved_util,
        "open definition stays unsaved"
    );
    assert_eq!(
        std::fs::read_to_string(&main).unwrap(),
        saved_main,
        "open consumer stays unsaved"
    );
    assert_eq!(session.source(&sibling_util), saved_util);
    assert_eq!(session.source(&sibling_main), sibling_source);
    assert_eq!(session.source(&outside), outside_source);
    let mut verification = RenameSession::new(Some(&legacy_root), &[&project, &sibling]);
    assert_clean(&verification.open(&util, &session.source(&util), 8));
    assert_clean(&verification.open(&main, &session.source(&main), 13));
    assert_clean(&verification.open(&closed, &session.source(&closed), 1));
    assert_clean(&verification.open(&sibling_main, sibling_source, 1));
}

#[test]
fn lsp_named_import_alias_and_original_token_have_distinct_rename_roles() {
    let project = TestProject::new("rename-local-alias");
    let util_source = "pub fn greet() -> i32 { 1 }\n";
    let util = project.write("util.hew", util_source);
    let main_source = "import util.{ greet as greet };\nfn main() { println(greet()); }\n";
    let main = project.write("main.hew", main_source);
    let closed_source = "import util;\nfn main() { println(util.greet()); }\n";
    let closed = project.write("closed.hew", closed_source);
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&main, main_source, 3));
    let response = session.rename(&main, main_source, "greet());", "invoke");
    assert_eq!(
        session.apply(&response),
        [file_uri(&main)].into_iter().collect()
    );
    assert_eq!(
        session.source(&main),
        "import util.{ greet as invoke };\nfn main() { println(invoke()); }\n"
    );
    assert_eq!(session.source(&util), util_source);
    assert_eq!(session.source(&closed), closed_source);
    assert_clean(&session.change(&main));
    let aliased_source = session.source(&main);
    let exported = session.rename(&main, &aliased_source, "greet as", "salute");
    assert_eq!(
        session.apply(&exported),
        [&util, &main, &closed]
            .into_iter()
            .map(|path| file_uri(path))
            .collect()
    );
    assert_eq!(
        session.source(&main),
        "import util.{ salute as invoke };\nfn main() { println(invoke()); }\n"
    );
    assert_eq!(session.source(&util), "pub fn salute() -> i32 { 1 }\n");
    assert_eq!(
        session.source(&closed),
        "import util;\nfn main() { println(util.salute()); }\n"
    );
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&main, &session.source(&main), 5));
    assert_clean(&verification.open(&closed, &session.source(&closed), 1));
}

#[test]
fn lsp_exported_function_rename_refuses_incomplete_importers_and_capture_atomically() {
    let project = TestProject::new("rename-refusal");
    let util_source = "pub fn greet() -> i32 { 1 }\n";
    let util = project.write("util.hew", util_source);
    let main_source = "import util;\nfn main() { println(util.greet()); }\n";
    let main = project.write("main.hew", main_source);
    let incomplete_source = "import util;\nfn broken( { util.greet() }\n";
    let incomplete = project.write("incomplete.hew", incomplete_source);
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&util, util_source, 1));
    let refused = session.rename(&util, util_source, "greet", "salute");
    assert_rename_refused(&refused);
    assert_eq!(session.source(&util), util_source);
    assert_eq!(session.source(&main), main_source);
    assert_eq!(session.source(&incomplete), incomplete_source);

    let unresolved_source = "import util;\nfn broken() -> i32 { missing + util.greet() }\n";
    std::fs::write(&incomplete, unresolved_source).expect("repair parse error");
    let unresolved = session.rename(&util, util_source, "greet", "salute");
    assert_rename_refused(&unresolved);
    assert_eq!(session.source(&util), util_source);
    assert_eq!(session.source(&main), main_source);
    assert_eq!(session.source(&incomplete), unresolved_source);

    // Repairing the unresolved source uncovers a semantic capture: an importer
    // has a local binding that would take over the renamed unqualified call.
    let capture_source = "import util.{ greet };\nfn broken() -> i32 { let salute = 3; greet() }\n";
    std::fs::write(&incomplete, capture_source).expect("repair importer");
    let capture = session.rename(&util, util_source, "greet", "salute");
    assert_rename_refused(&capture);
    assert_eq!(session.source(&util), util_source);
    assert_eq!(session.source(&main), main_source);
    assert_eq!(session.source(&incomplete), capture_source);

    // The same spelling beside a qualified call is harmless. Recovery must
    // return the complete edit set, with no stale refusal or partial cache.
    let recovered_source = "import util;\nfn broken() -> i32 { let salute = 3; util.greet() }\n";
    std::fs::write(&incomplete, recovered_source).expect("repair capture");
    let recovered = session.rename(&util, util_source, "greet", "salute");
    assert_eq!(
        session.apply(&recovered),
        [&util, &main, &incomplete]
            .into_iter()
            .map(|path| file_uri(path))
            .collect()
    );
    assert_eq!(
        session.source(&incomplete),
        "import util;\nfn broken() -> i32 { let salute = 3; util.salute() }\n"
    );
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&util, &session.source(&util), 2));
    assert_clean(&verification.open(&main, &session.source(&main), 1));
    assert_clean(&verification.open(&incomplete, &session.source(&incomplete), 1));
}

#[test]
fn lsp_untitled_local_rename_preserves_fresh_buffers_without_project_roots() {
    let uri = "untitled:Untitled-1";
    let source = "fn main() { let answer = 7; println(answer); }\n";
    let mut session = RenameSession::new(None, &[]);
    assert_clean(&session.open_uri(uri, source, 1));
    let prepared = session.request(
        "textDocument/prepareRename",
        json!({"textDocument":{"uri":uri},"position":position_of(source, "answer")}),
    );
    assert!(
        prepared["error"].is_null() && !prepared["result"].is_null(),
        "untitled local binding must be renameable: {prepared}"
    );
    let response = session.request(
        "textDocument/rename",
        json!({"textDocument":{"uri":uri},"position":position_of(source, "answer"),"newName":"result"}),
    );
    assert_eq!(
        session.apply(&response),
        [uri.to_string()].into_iter().collect()
    );
    assert_eq!(
        session.buffers[uri].0,
        "fn main() { let result = 7; println(result); }\n"
    );

    // Local rename remains useful in an incomplete scratch buffer. Neither the
    // absent filesystem path nor this separate unresolved function should make
    // it fall through to project-wide exported-function analysis.
    let unsaved = "// latest scratch buffer\nfn main() { let answer = 7; println(answer); }\nfn unrelated() -> i32 { missing }\n";
    session.change_uri_without_waiting(uri, unsaved, 9);
    let fresh = session.request(
        "textDocument/rename",
        json!({"textDocument":{"uri":uri},"position":position_of(unsaved, "answer"),"newName":"result"}),
    );
    assert_eq!(
        session.apply(&fresh),
        [uri.to_string()].into_iter().collect()
    );
    let expected = "// latest scratch buffer\nfn main() { let result = 7; println(result); }\nfn unrelated() -> i32 { missing }\n";
    assert_eq!(session.buffers[uri].0, expected);
    let mut verification = RenameSession::new(None, &[]);
    let publish = verification.open_uri(uri, expected, 10);
    let errors: Vec<_> = publish["params"]["diagnostics"]
        .as_array()
        .expect("scratch diagnostics")
        .iter()
        .filter(|diagnostic| diagnostic["severity"] == 1)
        .collect();
    assert!(!errors.is_empty(), "unrelated source error is retained");
    assert!(
        errors.iter().all(|diagnostic| diagnostic["message"]
            .as_str()
            .is_some_and(|message| message.contains("missing"))),
        "rename must not introduce a local binding error: {errors:?}"
    );
}

#[test]
fn lsp_directory_module_external_function_rename_keeps_import_roles_and_file_ranges() {
    let project = TestProject::new("rename-directory-imports");
    project.write(
        "hew.toml",
        "[package]\nname = \"app\"\nedition = \"2026\"\n",
    );
    let util_source = "pub fn greet() -> i32 { 1 }\n";
    let util = project.write("util.hew", util_source);
    let entry_source = "import app.util.{ greet };\npub fn other() -> i32 { 2 }\npub fn direct() -> i32 { greet() }\n";
    let entry = project.write("greeting/greeting.hew", entry_source);
    let peer_source = "import app.util as utility;\nimport app.util.{ greet as call };\npub fn aliased() -> i32 { call() }\npub fn qualified() -> i32 { utility.greet() }\n";
    let peer = project.write("greeting/peer.hew", peer_source);
    let expected_files: BTreeSet<_> = [&util, &entry, &peer]
        .into_iter()
        .map(|path| file_uri(path))
        .collect();

    // Both unopened directory sources are assembled through a synthetic root.
    // Their import tokens and occurrences must still map to physical files.
    let mut declaration_session = RenameSession::new(Some(&project), &[]);
    assert_clean(&declaration_session.open(&util, util_source, 1));
    let declaration = declaration_session.rename(&util, util_source, "greet", "go");
    assert_eq!(declaration_session.apply(&declaration), expected_files);
    assert_eq!(
        declaration_session.source(&util),
        "pub fn go() -> i32 { 1 }\n"
    );
    assert_eq!(
        declaration_session.source(&entry),
        "import app.util.{ go };\npub fn other() -> i32 { 2 }\npub fn direct() -> i32 { go() }\n"
    );
    let renamed_peer = "import app.util as utility;\nimport app.util.{ go as call };\npub fn aliased() -> i32 { call() }\npub fn qualified() -> i32 { utility.go() }\n";
    assert_eq!(declaration_session.source(&peer), renamed_peer);
    // Save only the client-owned definition before starting the next session;
    // WorkspaceEdit application already wrote the closed entry and peer files.
    std::fs::write(&util, declaration_session.source(&util)).expect("save renamed definition");
    drop(declaration_session);

    // An original imported token in a directory peer is an export rename, even
    // when its local alias has another spelling and stays local to this file.
    let mut imported_session = RenameSession::new(Some(&project), &[]);
    assert_clean(&imported_session.open(&peer, renamed_peer, 5));
    let prepared = imported_session.prepare(&peer, renamed_peer, "go as");
    assert!(
        prepared["error"].is_null() && !prepared["result"].is_null(),
        "directory peer imported token must be renameable: {prepared}"
    );
    let imported = imported_session.rename(&peer, renamed_peer, "go as", "hi");
    assert_eq!(imported_session.apply(&imported), expected_files);
    assert_eq!(imported_session.source(&util), "pub fn hi() -> i32 { 1 }\n");
    assert_eq!(
        imported_session.source(&entry),
        "import app.util.{ hi };\npub fn other() -> i32 { 2 }\npub fn direct() -> i32 { hi() }\n"
    );
    let final_peer = "import app.util as utility;\nimport app.util.{ hi as call };\npub fn aliased() -> i32 { call() }\npub fn qualified() -> i32 { utility.hi() }\n";
    assert_eq!(imported_session.source(&peer), final_peer);
    assert_eq!(
        std::fs::read_to_string(&peer).unwrap(),
        renamed_peer,
        "open directory peer stays unsaved"
    );
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&peer, final_peer, 6));
    assert_clean(&verification.open(&entry, &imported_session.source(&entry), 1));
}

#[test]
fn lsp_non_function_declaration_rename_survives_an_unrelated_missing_import() {
    let project = TestProject::new("rename-incomplete-declarations");
    let source = "import missing;\ntype Thing { value: i32; }\nfn main() {}\n";
    let file = project.write("main.hew", source);
    let fresh_source = "// latest unsaved declarations\nimport missing;\ntype Thing { value: i32; }\nfn main() {}\n";
    for (needle, new_name, expected) in [
        (
            "Thing",
            "Other",
            "// latest unsaved declarations\nimport missing;\ntype Other { value: i32; }\nfn main() {}\n",
        ),
        (
            "value",
            "other",
            "// latest unsaved declarations\nimport missing;\ntype Thing { other: i32; }\nfn main() {}\n",
        ),
    ] {
        let mut session = RenameSession::new(Some(&project), &[]);
        let original = session.open(&file, source, 1);
        assert!(
            original["params"]["diagnostics"]
                .as_array()
                .expect("original import diagnostics")
                .iter()
                .any(|diagnostic| diagnostic["code"] == "E_MODULE_NOT_FOUND")
        );
        session.change_without_waiting(&file, fresh_source, 7);
        let prepared = session.prepare(&file, fresh_source, needle);
        assert!(
            prepared["error"].is_null() && !prepared["result"].is_null(),
            "unrelated missing import must not disable {needle} declaration rename: {prepared}"
        );
        let response = session.rename(&file, fresh_source, needle, new_name);
        assert_eq!(
            session.apply(&response),
            [file_uri(&file)].into_iter().collect()
        );
        assert_eq!(session.source(&file), expected);
        assert_eq!(
            std::fs::read_to_string(&file).unwrap(),
            source,
            "declaration edits stay unsaved"
        );
        let mut verification = RenameSession::new(Some(&project), &[]);
        let publish = verification.open(&file, expected, 8);
        let errors: Vec<_> = publish["params"]["diagnostics"]
            .as_array()
            .expect("renamed import diagnostics")
            .iter()
            .filter(|diagnostic| diagnostic["severity"] == 1)
            .collect();
        assert!(!errors.is_empty(), "missing import diagnostic must remain");
        assert!(
            errors
                .iter()
                .all(|diagnostic| diagnostic["code"] == "E_MODULE_NOT_FOUND"),
            "declaration rename must preserve only the original import error: {errors:?}"
        );
    }
}

#[test]
fn lsp_incomplete_import_rename_cannot_borrow_an_unrelated_field_identity() {
    let project = TestProject::new("rename-import-field-collision");
    let util_source = "pub fn greet() -> i32 { 1 }\n";
    let util = project.write("util.hew", util_source);
    let closed_source = "import util;\nfn main() { println(util.greet()); }\n";
    let closed = project.write("closed.hew", closed_source);
    let source = "import util.{ greet };\nimport missing;\ntype Holder { greet: i32; }\nfn main() { println(greet()); }\n";
    let main = project.write("main.hew", source);
    let mut session = RenameSession::new(Some(&project), &[]);
    let diagnostics = session.open(&main, source, 9);
    assert!(diagnostics["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .any(|diagnostic| diagnostic["code"] == "E_MODULE_NOT_FOUND"));
    let response = session.rename(&main, source, "greet", "salute");
    assert_rename_refused(&response);
    assert_eq!(session.source(&main), source);
    assert_eq!(std::fs::read_to_string(&main).unwrap(), source);
    assert_eq!(std::fs::read_to_string(&util).unwrap(), util_source);
    assert_eq!(std::fs::read_to_string(&closed).unwrap(), closed_source);
}

#[cfg(unix)]
#[test]
fn lsp_unsaved_export_rename_resolves_symlink_boundaries_before_missing_leaf() {
    let project = TestProject::new("rename-unsaved-boundary");
    let outside = TestProject::new("rename-unsaved-outside");
    let sentinel_source = "pub fn sentinel() -> i32 { 2 }\n";
    let sentinel = outside.write("preserved.hew", sentinel_source);
    std::os::unix::fs::symlink(&outside.0, project.0.join("linked"))
        .expect("create outside directory link");
    let source = "pub fn greet() -> i32 { 1 }\n";
    let escaped = project.0.join("linked/new.hew");
    assert!(!escaped.exists());
    let mut outside_session = RenameSession::new(Some(&project), &[]);
    assert_clean(&outside_session.open(&escaped, source, 5));
    let refused = outside_session.rename(&escaped, source, "greet", "salute");
    assert_rename_refused(&refused);
    assert_eq!(outside_session.source(&escaped), source);
    assert!(
        !escaped.exists(),
        "refusal must not create the escaped file"
    );
    assert!(!outside.0.join("new.hew").exists());
    assert_eq!(std::fs::read_to_string(&sentinel).unwrap(), sentinel_source);
    drop(outside_session);

    // A nonexistent leaf is still a valid unsaved document when its nearest
    // existing physical ancestor belongs to the configured project root.
    let inside = project.0.join("new.hew");
    assert!(!inside.exists());
    let mut inside_session = RenameSession::new(Some(&project), &[]);
    assert_clean(&inside_session.open(&inside, source, 11));
    let response = inside_session.rename(&inside, source, "greet", "salute");
    assert!(
        response["result"]["documentChanges"].is_array(),
        "advertised documentChanges must protect the unsaved client version: {response}"
    );
    assert_eq!(
        inside_session.apply(&response),
        [file_uri(&inside)].into_iter().collect()
    );
    let expected = "pub fn salute() -> i32 { 1 }\n";
    assert_eq!(inside_session.source(&inside), expected);
    assert!(!inside.exists(), "successful buffer rename remains unsaved");
    assert!(!outside.0.join("new.hew").exists());
    assert_eq!(std::fs::read_to_string(&sentinel).unwrap(), sentinel_source);
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&inside, expected, 12));
}

#[test]
fn lsp_local_non_function_reference_rename_survives_missing_import() {
    let source =
        "import missing;\ntype Thing { value: i32; }\nfn read(item: Thing) -> i32 { item.value }\n";
    for (needle, replacement) in [("Thing)", "Other"), ("value }", "other")] {
        let project = TestProject::new("rename-nonfunction-reference");
        let main = project.write("main.hew", source);
        let mut session = RenameSession::new(Some(&project), &[]);
        session.open(&main, source, 1);
        let response = session.rename(&main, source, needle, replacement);
        assert!(
            response["error"].is_null(),
            "local reference remains eligible: {response}"
        );
        assert_eq!(
            session.apply(&response),
            [file_uri(&main)].into_iter().collect()
        );
        assert!(session.source(&main).contains(replacement));
        if replacement == "other" {
            assert_eq!(session.source(&main), "import missing;\ntype Thing { other: i32; }\nfn read(item: Thing) -> i32 { item.other }\n");
        }
    }
}

#[test]
fn lsp_local_field_cannot_enable_partial_imported_function_rename() {
    for (import, call) in [
        ("import util.{ greet };", "greet()"),
        ("import util as utility;", "(utility.greet)()"),
    ] {
        let project = TestProject::new("rename-field-function-ambiguity");
        let util_source = "pub fn greet() -> i32 { 1 }\n";
        let util = project.write("util.hew", util_source);
        let closed_source = "import util;\nfn main() { println(util.greet()); }\n";
        let closed = project.write("closed.hew", closed_source);
        let source = format!("{import}\nimport missing;\ntype Holder {{ greet: i32; }}\nfn read(item: Holder) -> i32 {{ item.greet }}\nfn main() {{ println({call}); }}\n");
        let main = project.write("main.hew", &source);
        let mut session = RenameSession::new(Some(&project), &[]);
        session.open(&main, &source, 1);
        let response = session.rename(&main, &source, "greet }", "salute");
        assert_rename_refused(&response);
        assert_eq!(session.source(&main), source);
        assert_eq!(std::fs::read_to_string(&util).unwrap(), util_source);
        assert_eq!(std::fs::read_to_string(&closed).unwrap(), closed_source);
    }
}

#[cfg(unix)]
#[test]
fn lsp_unsaved_symlink_export_rename_updates_closed_importer() {
    let project = TestProject::new("rename-unsaved-symlink-import");
    let physical = project.0.join("actual");
    std::fs::create_dir(&physical).expect("create physical source directory");
    std::os::unix::fs::symlink(&physical, project.0.join("linked"))
        .expect("create inside directory link");
    let fresh = project.0.join("linked/fresh.hew");
    let source = "pub fn greet() -> i32 { 1 }\n";
    let main_source = "import linked.fresh as utility;\nfn main() { println(utility.greet()); }\n";
    let main = project.write("main.hew", main_source);
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&fresh, source, 11));
    let response = session.rename(&fresh, source, "greet", "salute");
    assert_eq!(
        session.apply(&response),
        [file_uri(&fresh), file_uri(&main)].into_iter().collect()
    );
    let expected = "pub fn salute() -> i32 { 1 }\n";
    assert_eq!(session.source(&fresh), expected);
    assert_eq!(
        session.source(&main),
        "import linked.fresh as utility;\nfn main() { println(utility.salute()); }\n"
    );
    assert!(!fresh.exists(), "buffer rename remains unsaved");
    assert!(!physical.join("fresh.hew").exists());
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&fresh, expected, 12));
    assert_clean(&verification.open(&main, &session.source(&main), 1));
}

#[test]
fn lsp_shared_dependency_rename_keeps_manifest_root_scopes_distinct() {
    let alpha = TestProject::new("rename-reuse-alpha");
    let beta = TestProject::new("rename-reuse-beta");
    let manifest = "[package]\nname = \"app\"\nedition = \"2026\"\n";
    alpha.write("hew.toml", manifest);
    beta.write("hew.toml", manifest);
    let saved_alpha = "pub fn greet() -> i32 { 1 }\n";
    let alpha_util = alpha.write("util.hew", saved_alpha);
    let alpha_entry = alpha.write(
        "greeting/greeting.hew",
        "import app.util.{ greet };\npub fn entry() -> i32 { greet() }\n",
    );
    let alpha_peer = alpha.write("greeting/peer.hew", "import app.util as library;\nimport app.util.{ greet as call };\npub fn peer() -> i32 { call() + library.greet() }\n");
    let beta_source = "pub fn greet() -> string { \"beta\" }\n";
    let beta_util = beta.write("util.hew", beta_source);
    let beta_entry_source = "import app.util.{ greet };\npub fn entry() -> string { greet() }\n";
    let beta_entry = beta.write("greeting/greeting.hew", beta_entry_source);
    let beta_peer_source = "import app.util as library;\nimport app.util.{ greet as call };\npub fn peer() -> string { call() + library.greet() }\n";
    let beta_peer = beta.write("greeting/peer.hew", beta_peer_source);
    let fresh_alpha = "// latest shared dependency\nfn helper() -> i32 { 7 }\npub fn greet() -> i32 { helper() }\n";
    let mut session = RenameSession::new(None, &[&alpha, &beta]);
    assert_clean(&session.open(&alpha_util, saved_alpha, 1));
    assert_clean(&session.open(&beta_util, beta_source, 1));
    session.change_without_waiting(&alpha_util, fresh_alpha, 7);
    let response = session.rename(&alpha_util, fresh_alpha, "greet", "salute");
    assert_eq!(
        session.apply(&response),
        [&alpha_util, &alpha_entry, &alpha_peer]
            .into_iter()
            .map(|path| file_uri(path))
            .collect()
    );
    let expected_alpha = "// latest shared dependency\nfn helper() -> i32 { 7 }\npub fn salute() -> i32 { helper() }\n";
    assert_eq!(session.source(&alpha_util), expected_alpha);
    assert_eq!(
        session.source(&alpha_entry),
        "import app.util.{ salute };\npub fn entry() -> i32 { salute() }\n"
    );
    assert_eq!(session.source(&alpha_peer), "import app.util as library;\nimport app.util.{ salute as call };\npub fn peer() -> i32 { call() + library.salute() }\n");
    assert_eq!(session.source(&beta_util), beta_source);
    assert_eq!(session.source(&beta_entry), beta_entry_source);
    assert_eq!(session.source(&beta_peer), beta_peer_source);
    assert_eq!(
        std::fs::read_to_string(&alpha_util).unwrap(),
        saved_alpha,
        "shared dependency remains unsaved"
    );

    // The same package/import spelling is valid in both roots with different
    // signatures; dependency facts from either real root cannot serve the other.
    let mut verification = RenameSession::new(None, &[&alpha, &beta]);
    assert_clean(&verification.open(&alpha_util, expected_alpha, 8));
    assert_clean(&verification.open(&alpha_peer, &session.source(&alpha_peer), 1));
    assert_clean(&verification.open(&alpha_entry, &session.source(&alpha_entry), 1));
    assert_clean(&verification.open(&beta_peer, beta_peer_source, 1));
    assert_clean(&verification.open(&beta_entry, beta_entry_source, 1));
}

#[test]
fn lsp_shared_dependency_rename_rechecks_capture_errors_and_changed_consumers() {
    let project = TestProject::new("rename-reuse-requests");
    project.write(
        "hew.toml",
        "[package]\nname = \"app\"\nedition = \"2026\"\n",
    );
    let saved_util = "pub fn greet() -> i32 { 1 }\n";
    let util = project.write("util.hew", saved_util);
    let capture_source =
        "import app.util.{ greet };\npub fn use_named(salute: fn() -> i32) -> i32 { greet() }\n";
    let named = project.write("named.hew", capture_source);
    let alias_source = "import app.util as library;\nimport app.util.{ greet as call };\npub fn use_alias() -> i32 { call() + library.greet() }\n";
    let alias = project.write("alias.hew", alias_source);
    let expected_files: BTreeSet<_> = [&util, &named, &alias]
        .into_iter()
        .map(|path| file_uri(path))
        .collect();
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&util, saved_util, 1));

    // This capture still type-checks: the parameter and export have the same
    // callable type. Proposed analysis must check identity, not just errors.
    let capture = session.rename(&util, saved_util, "greet", "salute");
    assert_rename_refused(&capture);
    assert!(
        capture["error"]["message"]
            .as_str()
            .is_some_and(|message| message.contains("capture")),
        "same-typed binding capture must be diagnosed: {capture}"
    );
    assert_eq!(session.source(&util), saved_util);
    assert_eq!(session.source(&named), capture_source);
    assert_eq!(session.source(&alias), alias_source);

    let repaired_named = "import app.util.{ greet as original };\npub fn use_named(salute: fn() -> i32) -> i32 { original() }\n";
    std::fs::write(&named, repaired_named).expect("repair same-typed capture");
    let broken_util = "// changed shared dependency\npub fn greet() -> i32 { missing }\n";
    session.change_without_waiting(&util, broken_util, 5);
    let broken = session.rename(&util, broken_util, "greet", "salute");
    assert_rename_refused(&broken);
    assert_eq!(session.source(&util), broken_util);
    assert_eq!(session.source(&named), repaired_named);
    assert_eq!(session.source(&alias), alias_source);

    let repaired_util = "// repaired shared dependency\npub fn greet() -> i32 { 4 }\n";
    session.change_without_waiting(&util, repaired_util, 6);
    let recovered = session.rename(&util, repaired_util, "greet", "salute");
    assert_eq!(session.apply(&recovered), expected_files);
    let renamed_util = "// repaired shared dependency\npub fn salute() -> i32 { 4 }\n";
    assert_eq!(session.source(&util), renamed_util);
    assert_eq!(session.source(&named), "import app.util.{ salute as original };\npub fn use_named(salute: fn() -> i32) -> i32 { original() }\n");
    assert_eq!(session.source(&alias), "import app.util as library;\nimport app.util.{ salute as call };\npub fn use_alias() -> i32 { call() + library.salute() }\n");
    assert_clean(&session.change(&util));

    // A later request sees new closed-consumer text and its different aliases,
    // even though this server has already analysed the same dependency paths.
    let changed_alias = "// edited unopened consumer\nimport app.util as utility;\nimport app.util.{ salute as invoke };\npub fn use_alias() -> i32 { invoke() + utility.salute() }\n";
    std::fs::write(&alias, changed_alias).expect("change closed consumer aliases");
    let subsequent = session.rename(&util, renamed_util, "salute", "done");
    assert_eq!(session.apply(&subsequent), expected_files);
    let final_util = "// repaired shared dependency\npub fn done() -> i32 { 4 }\n";
    assert_eq!(session.source(&util), final_util);
    assert_eq!(session.source(&named), "import app.util.{ done as original };\npub fn use_named(salute: fn() -> i32) -> i32 { original() }\n");
    assert_eq!(session.source(&alias), "// edited unopened consumer\nimport app.util as utility;\nimport app.util.{ done as invoke };\npub fn use_alias() -> i32 { invoke() + utility.done() }\n");
    assert_eq!(
        std::fs::read_to_string(&util).unwrap(),
        saved_util,
        "successive dependency edits remain unsaved"
    );
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&util, final_util, 8));
    assert_clean(&verification.open(&named, &session.source(&named), 1));
    assert_clean(&verification.open(&alias, &session.source(&alias), 1));
}

const RESPONSIVE_RENAME_BUDGET: Duration = Duration::from_mins(2);
const RESPONSIVE_CONSUMERS: usize = 100;
const RESPONSIVE_CONTROL: &str = "fn main() { let marker = 7; println(marker); }\n";
const RESPONSIVE_UTIL: &str = "pub fn greet() -> i32 { 1 }\n";

fn responsive_rename_fixture(label: &str) -> (TestProject, PathBuf, PathBuf, Vec<PathBuf>) {
    let project = TestProject::new(label);
    let util = project.write("util.hew", RESPONSIVE_UTIL);
    let control = project.write("control.hew", RESPONSIVE_CONTROL);
    let consumers = (0..RESPONSIVE_CONSUMERS)
        .map(|index| {
            project.write(
                &format!("consumer_{index:03}.hew"),
                &format!("import util;\npub fn read_{index}() -> i32 {{ util.greet() }}\n"),
            )
        })
        .collect();
    (project, util, control, consumers)
}

fn assert_hover_before_rename(session: &mut RenameSession, control: &Path, rename_id: u64) {
    let hover_id = session.start_request("textDocument/hover",
        json!({"textDocument":{"uri":file_uri(control)},"position":position_of(RESPONSIVE_CONTROL, "marker")}));
    let response = session.wait_for_responses(&[hover_id, rename_id], RESPONSIVE_RENAME_BUDGET);
    assert_eq!(
        response["id"], hover_id,
        "hover must be served while project rename is pending: {response}"
    );
    assert!(
        response["error"].is_null() && !response["result"].is_null(),
        "control hover must succeed: {response}"
    );
}

#[test]
fn lsp_pending_project_rename_serves_hover_and_rejects_stale_open_and_disk_sources() {
    let (project, util, control, consumers) = responsive_rename_fixture("rename-responsive-edits");
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&control, RESPONSIVE_CONTROL, 1));
    assert_clean(&session.open(&util, RESPONSIVE_UTIL, 1));
    let active = session.start_rename(&util, RESPONSIVE_UTIL, "greet", "salute");
    assert_hover_before_rename(&mut session, &control, active);

    let fresh_util = "// current unsaved definition\npub fn greet() -> i32 { 2 }\n";
    let changed_consumer =
        "import util as library;\npub fn read_0() -> i32 { library.greet() + library.greet() }\n";
    session.change_without_waiting(&util, fresh_util, 2);
    std::fs::write(&consumers[0], changed_consumer).expect("edit closed consumer during rename");
    let stale = session.wait_for_responses(&[active], RESPONSIVE_RENAME_BUDGET);
    assert_rename_refused(&stale);
    assert_eq!(session.source(&util), fresh_util);
    assert_eq!(session.source(&consumers[0]), changed_consumer);

    let retry = session.start_rename(&util, fresh_util, "greet", "salute");
    let response = session.wait_for_responses(&[retry], RESPONSIVE_RENAME_BUDGET);
    let expected_files: BTreeSet<_> = std::iter::once(&util)
        .chain(consumers.iter())
        .map(|path| file_uri(path))
        .collect();
    assert_eq!(session.apply(&response), expected_files);
    let renamed_util = "// current unsaved definition\npub fn salute() -> i32 { 2 }\n";
    assert_eq!(session.source(&util), renamed_util);
    assert_eq!(
        session.source(&consumers[0]),
        "import util as library;\npub fn read_0() -> i32 { library.salute() + library.salute() }\n"
    );
    for (index, consumer) in consumers.iter().enumerate().skip(1) {
        assert_eq!(
            session.source(consumer),
            format!("import util;\npub fn read_{index}() -> i32 {{ util.salute() }}\n")
        );
    }
    assert_clean(&session.change(&util));

    // A disk-only change can land before the worker's initial disk capture.
    // Both outcomes are safe: refuse the stale snapshot, or return a complete
    // edit for the newly captured source. Applying stale ranges is never safe.
    let disk_active = session.start_rename(&util, renamed_util, "salute", "done");
    assert_hover_before_rename(&mut session, &control, disk_active);
    let disk_source = "// newly edited closed consumer\nimport util as current;\npub fn read_1() -> i32 { current.salute() + current.salute() }\n";
    std::fs::write(&consumers[1], disk_source).expect("edit disk-only consumer during rename");
    let disk_response = session.wait_for_responses(&[disk_active], RESPONSIVE_RENAME_BUDGET);
    let current = if disk_response["error"].is_null() {
        disk_response
    } else {
        assert_rename_refused(&disk_response);
        assert_eq!(session.source(&consumers[1]), disk_source);
        let retry = session.start_rename(&util, renamed_util, "salute", "done");
        session.wait_for_responses(&[retry], RESPONSIVE_RENAME_BUDGET)
    };
    assert_eq!(session.apply(&current), expected_files);
    let final_util = "// current unsaved definition\npub fn done() -> i32 { 2 }\n";
    assert_eq!(session.source(&util), final_util);
    assert_eq!(
        session.source(&consumers[0]),
        "import util as library;\npub fn read_0() -> i32 { library.done() + library.done() }\n"
    );
    assert_eq!(session.source(&consumers[1]), "// newly edited closed consumer\nimport util as current;\npub fn read_1() -> i32 { current.done() + current.done() }\n");
    for (index, consumer) in consumers.iter().enumerate().skip(2) {
        assert_eq!(
            session.source(consumer),
            format!("import util;\npub fn read_{index}() -> i32 {{ util.done() }}\n")
        );
    }
    assert_eq!(
        std::fs::read_to_string(&util).unwrap(),
        RESPONSIVE_UTIL,
        "responsive rename edits remain unsaved"
    );
    assert_eq!(session.source(&control), RESPONSIVE_CONTROL);
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&util, final_util, 4));
    assert_clean(&verification.open(&consumers[0], &session.source(&consumers[0]), 1));
    assert_clean(&verification.open(&consumers[1], &session.source(&consumers[1]), 1));
}

#[test]
fn lsp_cancelled_project_rename_and_queued_snapshot_cannot_deliver_stale_edits() {
    let (project, util, control, consumers) = responsive_rename_fixture("rename-responsive-cancel");
    let mut session = RenameSession::new(Some(&project), &[]);
    assert_clean(&session.open(&control, RESPONSIVE_CONTROL, 1));
    assert_clean(&session.open(&util, RESPONSIVE_UTIL, 1));
    let active = session.start_rename(&util, RESPONSIVE_UTIL, "greet", "discarded");
    assert_hover_before_rename(&mut session, &control, active);
    send(
        &mut session.stdin,
        &json!({"jsonrpc":"2.0","method":"$/cancelRequest","params":{"id":active}}),
    );

    // The queued request owns the original version before it waits for the
    // cancelled request's still-running CPU work to release its permit.
    let queued = session.start_rename(&util, RESPONSIVE_UTIL, "greet", "salute");
    assert_hover_before_rename(&mut session, &control, queued);
    let fresh_util = "// changed after queueing\npub fn greet() -> i32 { 9 }\n";
    session.change_without_waiting(&util, fresh_util, 7);
    let cancelled = session.wait_for_responses(&[active], RESPONSIVE_RENAME_BUDGET);
    assert!(
        !cancelled["error"].is_null() && cancelled.get("result").is_none(),
        "cancelled request must never return a WorkspaceEdit: {cancelled}"
    );
    let stale_queued = session.wait_for_responses(&[queued], RESPONSIVE_RENAME_BUDGET);
    assert_rename_refused(&stale_queued);
    assert_eq!(session.source(&util), fresh_util);
    for (index, consumer) in consumers.iter().enumerate() {
        assert_eq!(
            session.source(consumer),
            format!("import util;\npub fn read_{index}() -> i32 {{ util.greet() }}\n")
        );
    }

    let retry = session.start_rename(&util, fresh_util, "greet", "salute");
    let response = session.wait_for_responses(&[retry], RESPONSIVE_RENAME_BUDGET);
    assert_eq!(
        session.apply(&response),
        std::iter::once(&util)
            .chain(consumers.iter())
            .map(|path| file_uri(path))
            .collect()
    );
    let expected = "// changed after queueing\npub fn salute() -> i32 { 9 }\n";
    assert_eq!(session.source(&util), expected);
    for (index, consumer) in consumers.iter().enumerate() {
        assert_eq!(
            session.source(consumer),
            format!("import util;\npub fn read_{index}() -> i32 {{ util.salute() }}\n")
        );
    }
    assert_eq!(std::fs::read_to_string(&util).unwrap(), RESPONSIVE_UTIL);
    let mut verification = RenameSession::new(Some(&project), &[]);
    assert_clean(&verification.open(&util, expected, 8));
    assert_clean(&verification.open(&consumers[0], &session.source(&consumers[0]), 1));
}
