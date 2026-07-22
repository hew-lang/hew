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

const COVERAGE_DEADLINE_FACTOR: u32 = 4;

fn subprocess_deadline(normal: Duration, coverage: bool) -> Duration {
    if coverage {
        normal.saturating_mul(COVERAGE_DEADLINE_FACTOR)
    } else {
        normal
    }
}

fn active_subprocess_deadline(normal: Duration) -> Duration {
    subprocess_deadline(normal, cfg!(coverage))
}

#[test]
fn coverage_subprocess_deadlines_are_bounded_without_changing_normal_deadlines() {
    let run_deadline = Duration::from_secs(30);
    let compile_deadline = Duration::from_secs(60);

    assert_eq!(subprocess_deadline(run_deadline, false), run_deadline);
    assert_eq!(
        subprocess_deadline(compile_deadline, false),
        compile_deadline
    );
    assert_eq!(
        subprocess_deadline(run_deadline, true),
        Duration::from_secs(120)
    );
    assert_eq!(
        subprocess_deadline(compile_deadline, true),
        Duration::from_secs(240)
    );
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
        active_subprocess_deadline(Duration::from_secs(30)),
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
        active_subprocess_deadline(Duration::from_secs(60)),
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
        active_subprocess_deadline(Duration::from_secs(60)),
    )
    .unwrap_or_else(|e| panic!("{e}"));
    assert!(
        !output.status.success(),
        "expected a fail-closed compile refusal but compile SUCCEEDED for {stem}"
    );
    String::from_utf8_lossy(&output.stderr).to_string()
}

/// Count direct `call ... @<symbol>(` sites across the WHOLE emitted module.
/// The moved-handle close fires through the actor's `state_drop_fn`; the
/// module-wide count must be EXACTLY ONE. A presence oracle (`contains`) is
/// blind to a double-free masked by the runtime's null-guard — only the count
/// distinguishes exactly-once.
fn count_calls_in_module(ir: &str, symbol: &str) -> usize {
    let needle = format!("@{symbol}(");
    ir.lines()
        .filter(|line| line.contains(&needle) && line.contains("call "))
        .count()
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

// ── Slice E: a Stream/Sink half in actor state closes exactly once ─────────

/// Probe A: a `Sink<string>` half moves into actor state (`spawn Writer(sink:
/// sink)`); the actor's `state_drop_fn` is the single free site. Runs clean
/// under `MallocScribble` (a double-free would crash), and the emitted IR
/// contains EXACTLY ONE `hew_sink_close` for the moved half.
#[test]
fn sink_half_in_actor_state_closes_exactly_once() {
    let source = r#"import std::stream;

actor Writer {
    let sink: stream.Sink<string>;

    receive fn put(item: string) {
        sink.send(item);
    }
}

fn main() {
    let (sink, input) = stream.pipe(4);
    let w = spawn Writer(sink: sink);
    w.put("hello");
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
}
"#;
    let stdout = run_hew_source_env("sink_half_scribble", source, true);
    assert_eq!(
        stdout, "hello",
        "sink half in actor state: expected 'hello', got {stdout:?}"
    );
    let ir = emit_llvm_ir("sink_half_ir", source);
    assert_eq!(
        count_calls_in_module(&ir, "hew_sink_close"),
        1,
        "the moved Sink half must be closed EXACTLY once (state_drop_fn); a \
         second close is a double-free"
    );
}

/// Probe A twin: a `Stream<string>` half moves into a consumer actor's state;
/// the actor awaits `input.recv()` and drains to EOF when the main-side (local)
/// sink closes. The Stream half is closed exactly once by the consumer's
/// `state_drop_fn`, and the main-side sink exactly once by its local drop —
/// both `== 1`, scribble-clean.
#[test]
fn stream_half_in_actor_state_closes_exactly_once() {
    let source = r#"import std::stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var count = 0;
        var done = false;
        while !done {
            match await input.recv() {
                Some(_s) => { count = count + 1; },
                None => { done = true; },
            }
        }
        println(f"count={count}");
    }
}

fn main() {
    let (sink, input) = stream.pipe(2);
    let c = spawn Consumer(input: input);
    c.drain();
    for i in 0 .. 4 {
        sink.send(f"item-{i}");
    }
    sink.close();
    sleep(400ms);
    println("done");
}
"#;
    let stdout = run_hew_source_env("stream_half_scribble", source, true);
    assert_eq!(
        stdout, "count=4\ndone",
        "stream half in actor state: expected 'count=4' then 'done', got {stdout:?}"
    );
    let ir = emit_llvm_ir("stream_half_ir", source);
    assert_eq!(
        count_calls_in_module(&ir, "hew_stream_close"),
        1,
        "the moved Stream half must be closed EXACTLY once (state_drop_fn)"
    );
    assert_eq!(
        count_calls_in_module(&ir, "hew_sink_close"),
        1,
        "the main-side local sink must be closed EXACTLY once"
    );
}

// ── Slice E: concurrency edge classes ─────────────────────────────────────

/// Edge 1 — racing send/recv across actors: a consumer actor awaits recv on a
/// Stream half it owns in state while `main` concurrently sends N items into
/// the local sink half. Every item is delivered exactly once, in order,
/// followed by a single EOF. Scribble-clean.
#[test]
fn racing_send_recv_delivers_in_order() {
    let source = r#"import std::stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var done = false;
        while !done {
            match await input.recv() {
                Some(s) => println(s),
                None => { done = true; },
            }
        }
        println("eof");
    }
}

fn main() {
    let (sink, input) = stream.pipe(4);
    let c = spawn Consumer(input: input);
    c.drain();
    for i in 0 .. 6 {
        sink.send(f"item-{i}");
    }
    sink.close();
    sleep(400ms);
}
"#;
    let stdout = run_hew_source_env("edge_racing", source, true);
    assert_eq!(
        stdout, "item-0\nitem-1\nitem-2\nitem-3\nitem-4\nitem-5\neof",
        "racing send/recv must deliver each item once in order then one EOF; got {stdout:?}"
    );
}

/// Edge 2 — drop-during-await: a producer actor owns a Sink half in state and
/// awaits sends into a capacity-1 ring. `main` drains a single item, leaving
/// the producer parked on the full ring, then exits — tearing the producer down
/// mid-await. The abandon edge (`hew_read_slot_cancel` + `hew_sink_detach_await`
/// + `hew_read_slot_free`) must run cleanly: exit 0, scribble-clean, no hang.
#[test]
fn producer_parked_on_full_ring_teardown_is_clean() {
    let source = r#"import std::stream;

actor Producer {
    let sink: stream.Sink<string>;

    receive fn run() {
        var i = 0;
        while i < 100 {
            await sink.send(f"v{i}");
            i = i + 1;
        }
    }
}

fn main() {
    let (sink, input) = stream.pipe(1);
    let p = spawn Producer(sink: sink);
    p.run();
    sleep(200ms);
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
    println("main-exit");
}
"#;
    let stdout = run_hew_source_env("edge_drop_during_await", source, true);
    assert_eq!(
        stdout, "v0\nmain-exit",
        "producer parked on a full ring must be torn down cleanly; got {stdout:?}"
    );
}

/// Edge 3 — close-during-suspend: a consumer actor parks in `await input.recv()`
/// on an empty ring; `main` then closes the (local) sink. The recv resumes with
/// `None` exactly once and the program exits 0.
#[test]
fn close_during_suspended_recv_yields_none_once() {
    let source = r#"import std::stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var nones = 0;
        match await input.recv() {
            Some(s) => println(s),
            None => { nones = nones + 1; },
        }
        println(f"nones={nones}");
    }
}

fn main() {
    let (sink, input) = stream.pipe(2);
    let c = spawn Consumer(input: input);
    c.drain();
    sleep(150ms);
    sink.close();
    sleep(200ms);
    println("done");
}
"#;
    let stdout = run_hew_source_env("edge_close_during_suspend", source, true);
    assert_eq!(
        stdout, "nones=1\ndone",
        "a suspended recv must resume with None exactly once when the sink closes; got {stdout:?}"
    );
}

/// Edge 4 — backpressure exactness: a capacity-1 ring with more items than
/// capacity. Every item is delivered exactly once and EOF (`None`) exactly once
/// after `close()`. Asserts exact counts (`== 5`, `== 1`), never `> 0`.
#[test]
fn backpressure_capacity_one_delivers_each_item_once() {
    let source = r#"import std::stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var count = 0;
        var eofs = 0;
        var done = false;
        while !done {
            match await input.recv() {
                Some(_v) => { count = count + 1; },
                None => { eofs = eofs + 1; done = true; },
            }
        }
        println(f"count={count} eofs={eofs}");
    }
}

fn main() {
    let (sink, input) = stream.pipe(1);
    let c = spawn Consumer(input: input);
    c.drain();
    for i in 0 .. 5 {
        sink.send(f"v{i}");
    }
    sink.close();
    sleep(400ms);
    println("done");
}
"#;
    let stdout = run_hew_source_env("edge_backpressure", source, true);
    assert_eq!(
        stdout, "count=5 eofs=1\ndone",
        "capacity-1 backpressure must deliver each item once and exactly one EOF; got {stdout:?}"
    );
}

// ── Slice D: negative fixtures (fail-closed refusals) ──────────────────────

/// D5 (consume): explicitly closing an owned handle held in ACTOR STATE
/// (`sink.close()` on the bare state field) is refused fail-closed — the actor's
/// `state_drop_fn` is the single owner, so an explicit close would double-free.
#[test]
fn explicit_close_of_state_field_handle_is_refused() {
    let source = r#"import std::stream;

actor Producer {
    let sink: stream.Sink<string>;

    receive fn go() {
        sink.send("x");
        sink.close();
    }
}

fn main() {
    let (sink, input) = stream.pipe(2);
    let p = spawn Producer(sink: sink);
    p.go();
    sleep(100ms);
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("state_field_close_refused", source);
    assert!(
        stderr.contains("closing an owned handle held in actor state") && stderr.contains("Sink"),
        "explicit close of a state-held handle must be refused fail-closed; got:\n{stderr}"
    );
}

/// D5 (overwrite): reassigning an owned handle actor-state field (`var sink`;
/// `sink = <fresh>`) is refused fail-closed — the previous handle would leak and
/// the new one be double-owned by teardown.
#[test]
fn overwrite_of_state_field_handle_is_refused() {
    let source = r#"import std::stream;

actor Writer {
    var sink: stream.Sink<string>;

    receive fn reset() {
        let (s, _i) = stream.pipe(4);
        sink = s;
    }
}

fn main() {
    let (sink, input) = stream.pipe(4);
    let w = spawn Writer(sink: sink);
    w.reset();
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("state_field_overwrite_refused", source);
    assert!(
        stderr.contains("overwriting an owned handle in actor state"),
        "overwriting a state-held handle must be refused fail-closed; got:\n{stderr}"
    );
}

/// D3 (fail-closed boundary): reusing a handle AFTER it was moved into a spawn
/// stays a hard move-checker error — the spawn consumed it.
#[test]
fn reuse_of_handle_after_spawn_is_refused() {
    let source = r#"import std::stream;

actor Writer {
    let sink: stream.Sink<string>;
    receive fn put(item: string) { sink.send(item); }
}

fn main() {
    let (sink, input) = stream.pipe(4);
    let w = spawn Writer(sink: sink);
    sink.send("after-move");
    match input.recv() {
        Some(s) => println(s),
        None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("reuse_after_spawn_refused", source);
    assert!(
        stderr.contains("moved value") || stderr.contains("consumed"),
        "reusing a handle after spawn must stay a move-checker error; got:\n{stderr}"
    );
}
