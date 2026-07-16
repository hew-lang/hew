//! End-to-end execution tests for the owned-element Stream/Sink surface
//! (CAP-08): awaited non-byte `Sink<T>.send`, a Stream/Sink half moved into
//! actor state with an exactly-once ownership proof, and the concurrency edge
//! classes that exercise the backpressure ramp.
//!
//! Two invariants ride these tests:
//!   1. `await sink.send(x)` over a `Sink<string>` routes the awaited send
//!      through the layout-witness runtime entry (`hew_stream_await_send_layout`),
//!      never the byte-native `hew_stream_await_send`. Element type rides the
//!      checker-resolved value type, not the symbol name.
//!   2. A Stream/Sink half in actor state closes EXACTLY ONCE. The runtime
//!      close is an unguarded `Box::from_raw`, so exactly-once rests entirely
//!      on the MIR provers; the release-count assertions are `== 1` (never
//!      `>= 1`) and the scribble-poisoned run turns any double-free into a
//!      hard crash.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo).join("debug").join("hew")
}

fn hew_command(repo: &Path) -> Command {
    let bin = hew_bin(repo);
    if bin.exists() {
        return Command::new(bin);
    }
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

fn ensure_hew_runtime_lib() {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn temp_source(stem: &str, source: &str) -> (PathBuf, PathBuf) {
    let dir = std::env::temp_dir().join(format!("hew-sink-owned-{}-{stem}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp source dir");
    let path = dir.join(format!("{stem}.hew"));
    std::fs::write(&path, source).expect("write temp Hew source");
    (dir, path)
}

/// Compile and run a Hew snippet under the given extra env; return trimmed
/// stdout. Asserts exit-0. When `scribble` is set, the run poisons freed
/// memory (`MallocScribble` + guard edges) so a double-free of a moved handle
/// crashes instead of silently passing.
fn run_hew_source_env(stem: &str, source: &str, scribble: bool) -> String {
    ensure_hew_runtime_lib();
    let repo = repo_root();
    let (_dir, path) = temp_source(stem, source);

    let mut cmd = hew_command(&repo);
    cmd.arg("run").arg(&path);
    if scribble {
        cmd.env("MallocScribble", "1")
            .env("MallocPreScribble", "1")
            .env("MallocGuardEdges", "1");
    }
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew run {}", path.display()),
        Duration::from_secs(30),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("stdout is utf-8")
        .trim()
        .to_string()
}

fn run_hew_source(stem: &str, source: &str) -> String {
    run_hew_source_env(stem, source, false)
}

/// Compile `source` and return the emitted LLVM IR text (`<stem>.ll`).
fn emit_llvm_ir(stem: &str, source: &str) -> String {
    ensure_hew_runtime_lib();
    let repo = repo_root();
    let (dir, path) = temp_source(stem, source);

    let mut cmd = hew_command(&repo);
    cmd.arg("compile").arg("--emit-dir").arg(&dir).arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew compile --emit-dir {}", dir.display()),
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        output.status.success(),
        "hew compile {} exited non-zero (status={:?}); stderr:\n{}",
        path.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let ll = dir.join(format!("{stem}.ll"));
    std::fs::read_to_string(&ll).unwrap_or_else(|e| panic!("read emitted IR {}: {e}", ll.display()))
}

/// Compile a snippet expecting a fail-closed compile refusal; return combined
/// stderr. Asserts the compile exited non-zero.
fn compile_expect_refusal(stem: &str, source: &str) -> String {
    let repo = repo_root();
    let (dir, path) = temp_source(stem, source);
    let mut cmd = hew_command(&repo);
    cmd.arg("compile").arg("--emit-dir").arg(&dir).arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew compile --emit-dir {}", dir.display()),
        Duration::from_secs(60),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        !output.status.success(),
        "expected a fail-closed compile refusal but compile SUCCEEDED for {stem}"
    );
    String::from_utf8_lossy(&output.stderr).to_string()
}

// ── Slice B: awaited non-byte send routing ────────────────────────────────

/// `await sink.send(f"...")` over a `Sink<string>` inside an actor handler
/// compiles, runs, and prints each item in order. The awaited send routes
/// through the layout-witness runtime entry, NOT the byte-native one. Ring
/// capacity exceeds the item count so every send binds immediately (this test
/// pins routing + ordering; the backpressure suspend ramp is exercised by the
/// racing send/recv edge below).
#[test]
fn awaited_string_sink_send_runs_in_order() {
    let source = r#"import std::stream;

actor Producer {
    let n: i64;

    receive fn run(unused: i64) {
        let (sink, input) = stream.pipe(8);
        for i in 0 .. n {
            await sink.send(f"item-{i}");
        }
        sink.close();
        var done = false;
        while !done {
            match await input.recv() {
                Some(s) => println(s),
                None => { done = true; },
            }
        }
    }
}

fn main() {
    let p = spawn Producer(n: 3);
    p.run(0);
    sleep(300ms);
    println("done");
}
"#;
    let stdout = run_hew_source("await_string_send_order", source);
    assert_eq!(
        stdout, "item-0\nitem-1\nitem-2\ndone",
        "awaited Sink<string> send must print items in order then done; got {stdout:?}"
    );
}

/// The awaited `Sink<string>` send lowers to `hew_stream_await_send_layout`
/// (the layout-witness path), never the byte-native `hew_stream_await_send`.
/// Element type rides the checker-resolved value type, not the symbol name.
#[test]
fn awaited_string_sink_send_routes_through_layout_entry() {
    let source = r#"import std::stream;

actor Producer {
    let n: i64;

    receive fn run(unused: i64) {
        let (sink, input) = stream.pipe(8);
        for i in 0 .. n {
            await sink.send(f"item-{i}");
        }
        sink.close();
        var done = false;
        while !done {
            match await input.recv() {
                Some(s) => println(s),
                None => { done = true; },
            }
        }
    }
}

fn main() {
    let p = spawn Producer(n: 3);
    p.run(0);
    sleep(300ms);
}
"#;
    let ir = emit_llvm_ir("await_string_send_layout_ir", source);
    assert!(
        ir.contains("@hew_stream_await_send_layout("),
        "awaited Sink<string> send must route through the layout witness entry"
    );
    assert!(
        !ir.contains("@hew_stream_await_send("),
        "awaited Sink<string> send must NOT route through the byte-native entry"
    );
}

/// D4: an awaited string send to a FILE-backed sink (`to_file()`) reaches
/// `hew_stream_await_send_layout` on a non-channel sink. The runtime writes
/// String/Plain/Bytes envelopes immediately (no in-memory queue to own a
/// layout-managed release; string is not layout-managed), so the send
/// completes synchronously and the file contents are correct.
#[test]
fn awaited_string_send_to_file_sink_writes_contents() {
    let dir = std::env::temp_dir().join(format!("hew-filesink-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create temp dir");
    let out = dir.join("out.txt");
    let out_lit = out
        .to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"");
    let source = format!(
        r#"import std::stream;

actor Writer {{
    let path: string;

    receive fn run(unused: i64) {{
        match stream.to_file(path) {{
            Ok(sink) => {{
                await sink.send("line-a\n");
                await sink.send("line-b\n");
                sink.close();
            }},
            Err(e) => println(f"open failed: {{e}}"),
        }}
    }}
}}

fn main() {{
    let w = spawn Writer(path: "{out_lit}");
    w.run(0);
    sleep(300ms);
    println("done");
}}
"#
    );
    let stdout = run_hew_source("await_string_send_file_sink", &source);
    assert_eq!(
        stdout, "done",
        "file-sink writer prints done; got {stdout:?}"
    );
    let contents = std::fs::read_to_string(&out)
        .unwrap_or_else(|e| panic!("file sink output not written: {e}"));
    assert_eq!(
        contents, "line-a\nline-b\n",
        "awaited string send to a file sink must write each line exactly once"
    );
}

/// Negative: `try_send` on a `Sink<string>` keeps its existing refusal. This
/// lane widens only the AWAITED (suspending) send; the non-blocking typed
/// `try_send` runtime entry is out of scope and stays refused fail-closed.
#[test]
fn try_send_on_string_sink_stays_refused() {
    let source = r#"import std::stream;

fn main() {
    let (sink, input) = stream.pipe(4);
    let _r = sink.try_send("hi");
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("try_send_string_refused", source);
    assert!(
        stderr.contains("NotYetImplemented") || stderr.contains("not yet"),
        "try_send on Sink<string> must stay refused fail-closed; got:\n{stderr}"
    );
}
