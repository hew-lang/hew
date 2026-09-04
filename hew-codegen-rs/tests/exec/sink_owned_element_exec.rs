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

use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;
use std::time::Duration;

fn ensure_codegen_artifacts() -> (PathBuf, PathBuf) {
    static BUILT: OnceLock<(PathBuf, PathBuf)> = OnceLock::new();
    BUILT
        .get_or_init(|| {
            let hew = hew_testutil::ensure_hew_bin_built().expect("build hew compiler");
            let libhew = hew_testutil::ensure_hew_lib_built().expect("build Hew runtime archive");
            assert_eq!(
                hew.parent(),
                libhew.parent(),
                "hew and the Hew runtime archive must share one target/profile authority"
            );
            (hew, libhew)
        })
        .clone()
}

fn hew_command() -> Command {
    let (hew, _) = ensure_codegen_artifacts();
    Command::new(hew)
}

#[test]
fn sink_harness_artifacts_follow_running_test_target_profile() {
    let (hew, libhew) = ensure_codegen_artifacts();
    let current_exe = std::env::current_exe().expect("resolve running test executable");
    let expected_dir = current_exe
        .parent()
        .and_then(std::path::Path::parent)
        .expect("test executable must use <target>/<profile>/deps layout");

    assert_eq!(hew.parent(), Some(expected_dir));
    assert_eq!(libhew.parent(), Some(expected_dir));
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
    let (_dir, path) = temp_source(stem, source);

    let mut cmd = hew_command();
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
    let (dir, path) = temp_source(stem, source);

    let mut cmd = hew_command();
    cmd.arg("compile")
        .arg("--emit-llvm")
        .arg("--emit-dir")
        .arg(&dir)
        .arg(&path);
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
    let (dir, path) = temp_source(stem, source);
    let mut cmd = hew_command();
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

/// Return one LLVM function body by symbol. Keeping an ownership assertion
/// within the function that owns the handle prevents unrelated stdlib helpers
/// from changing its count.
fn llvm_function_body<'a>(ir: &'a str, function: &str) -> &'a str {
    let needle = format!("@{function}(");
    let start = ir
        .find(&needle)
        .unwrap_or_else(|| panic!("LLVM function `{function}` not found\n{ir}"));
    let body = &ir[start..];
    let end = body
        .find("\n}\n")
        .unwrap_or_else(|| panic!("LLVM function `{function}` has no closing brace\n{body}"));
    &body[..end]
}

fn is_call_instruction(line: &str) -> bool {
    line.contains("call ") || line.contains("invoke ")
}

/// Count direct `call` or `invoke` sites for `@<symbol>(` in the one LLVM function that owns
/// the relevant handle. A presence oracle (`contains`) is blind to a
/// double-free masked by the runtime's null-guard — this exact local count is
/// not.
fn count_calls_in_function(ir: &str, function: &str, symbol: &str) -> usize {
    let body = llvm_function_body(ir, function);
    let needle = format!("@{symbol}(");
    body.lines()
        .filter(|line| line.contains(&needle) && is_call_instruction(line))
        .count()
}

/// Count source-call sites, excluding elaborated drop-path calls whose loaded
/// operand is deliberately named `<symbol> drop`. A function may carry one
/// close on each mutually-exclusive cancellation/trap/return exit while still
/// having exactly one source `sink.close()` call on the success path.
fn count_source_calls_in_function(ir: &str, function: &str, symbol: &str) -> usize {
    let body = llvm_function_body(ir, function);
    let needle = format!("@{symbol}(");
    let drop_label = format!("\"{symbol} drop");
    body.lines()
        .filter(|line| {
            line.contains(&needle) && is_call_instruction(line) && !line.contains(&drop_label)
        })
        .count()
}

#[test]
fn close_count_is_scoped_to_its_owner_function() {
    let ir = "define void @owner() personality ptr @rust_eh_personality {\n  invoke void @hew_sink_close(ptr null) to label %done unwind label %cleanup\n}\n\
              \ndefine void @unrelated() {\n  call void @hew_sink_close(ptr null)\n  invoke void @hew_sink_close(ptr null) to label %done unwind label %cleanup\n}\n";
    assert_eq!(count_calls_in_function(ir, "owner", "hew_sink_close"), 1);
    assert_eq!(
        count_calls_in_function(ir, "unrelated", "hew_sink_close"),
        2,
        "counterfactual: a second close in the owner must remain visible"
    );
}

#[test]
fn contextual_file_sink_binders_close_once_per_exit() {
    let dir = std::env::temp_dir().join(format!("hew-contextual-file-sink-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create contextual sink temp dir");
    let output_path = dir.join("out.txt");
    let early_path = dir.join("early.txt");
    let missing_path = dir.join("missing").join("out.txt");
    let source = r#"import std.stream;

fn if_implicit(path: string) {
    if let .Ok(sink) = stream.to_file(path) {
        sink.write("if implicit");
    }
}

fn if_explicit(path: string) {
    if let .Ok(sink) = stream.to_file(path) {
        sink.write("if explicit");
        sink.close();
    }
}

fn match_implicit(path: string) {
    match stream.to_file(path) {
        .Ok(sink) => sink.write("match implicit"),
        _ => {},
    }
}

fn match_explicit(path: string) {
    match stream.to_file(path) {
        .Ok(sink) => {
            sink.write("match explicit");
            sink.close();
        },
        _ => {},
    }
}

fn while_implicit(path: string) {
    while let .Ok(sink) = stream.to_file(path) {
        sink.write("while implicit");
        break;
    }
}

fn while_explicit(path: string) {
    while let .Ok(sink) = stream.to_file(path) {
        sink.write("while explicit");
        sink.close();
        break;
    }
}

fn let_else_implicit(path: string) {
    let .Ok(sink) = stream.to_file(path) else { return; };
    sink.write("let else implicit");
}

fn let_else_explicit(path: string) {
    let .Ok(sink) = stream.to_file(path) else { return; };
    sink.write("let else explicit");
    sink.close();
}

fn if_explicit_early(path: string, before: bool, after: bool) {
    if let .Ok(sink) = stream.to_file(path) {
        if before { return; }
        sink.close();
        if after { return; }
    }
}

fn main() {
    let ok = "__OK_PATH__";
    let early = "__EARLY_PATH__";
    let bad = "__BAD_PATH__";
    if_implicit(ok);
    if_implicit(bad);
    if_explicit(ok);
    if_explicit(bad);
    match_implicit(ok);
    match_implicit(bad);
    match_explicit(ok);
    match_explicit(bad);
    while_implicit(ok);
    while_implicit(bad);
    while_explicit(ok);
    while_explicit(bad);
    let_else_implicit(ok);
    let_else_implicit(bad);
    let_else_explicit(ok);
    let_else_explicit(bad);
    if_explicit_early(early, true, false);
    if_explicit_early(early, false, true);
    if_explicit_early(early, false, false);
    println("matrix-ok");
}
"#
    .replace("__OK_PATH__", &output_path.to_string_lossy())
    .replace("__EARLY_PATH__", &early_path.to_string_lossy())
    .replace("__BAD_PATH__", &missing_path.to_string_lossy());

    let stdout = run_hew_source_env("contextual_file_sink_matrix", &source, true);
    assert_eq!(stdout, "matrix-ok");
    assert_eq!(
        std::fs::read_to_string(&output_path).expect("read final file-sink output"),
        "let else explicit"
    );

    let ir = emit_llvm_ir("contextual_file_sink_matrix_ir", &source);
    let expected_sites = [
        ("if_implicit", 3, 0),
        ("if_explicit", 1, 1),
        ("match_implicit", 3, 0),
        ("match_explicit", 1, 1),
        ("while_implicit", 3, 0),
        ("while_explicit", 1, 1),
        ("let_else_implicit", 2, 0),
        ("let_else_explicit", 1, 1),
        ("if_explicit_early", 4, 1),
    ];
    for (function, total, source_calls) in expected_sites {
        assert_eq!(
            count_calls_in_function(&ir, function, "hew_sink_close"),
            total,
            "{function} must retain exactly its mutually exclusive exit close sites"
        );
        assert_eq!(
            count_source_calls_in_function(&ir, function, "hew_sink_close"),
            source_calls,
            "{function} must emit the expected explicit close count"
        );
    }
}

#[test]
fn guarded_match_arm_hands_the_live_sink_to_the_selected_arm() {
    let dir = std::env::temp_dir().join(format!("hew-guarded-file-sink-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create guarded sink temp dir");
    let taken_path = dir.join("taken.txt");
    let fallthrough_path = dir.join("fallthrough.txt");
    let source = r#"import std.stream;

fn pick(flag: bool) -> bool {
    return flag;
}

fn guarded(path: string, flag: bool) {
    match stream.to_file(path) {
        .Ok(sink) if pick(flag) => {
            sink.write("guard taken");
        },
        .Ok(sink) => {
            sink.write("guard fallthrough");
        },
        .Err(e) => {
            println(f"open failed: {e}");
        },
    }
}

fn main() {
    guarded("__TAKEN_PATH__", true);
    guarded("__FALLTHROUGH_PATH__", false);
    println("guarded-ok");
}
"#
    .replace("__TAKEN_PATH__", &taken_path.to_string_lossy())
    .replace("__FALLTHROUGH_PATH__", &fallthrough_path.to_string_lossy());

    let stdout = run_hew_source_env("guarded_file_sink_arm", &source, true);
    assert_eq!(stdout, "guarded-ok");

    // The rejected arm destructured the carrier before its guard answered. If
    // that arm keeps the handle, the selected arm writes through a null Sink
    // and the file is never closed.
    assert_eq!(
        std::fs::read_to_string(&taken_path).expect("read guard-taken output"),
        "guard taken"
    );
    assert_eq!(
        std::fs::read_to_string(&fallthrough_path).expect("read guard-fallthrough output"),
        "guard fallthrough"
    );

    let ir = emit_llvm_ir("guarded_file_sink_arm_ir", &source);
    assert_eq!(
        count_source_calls_in_function(&ir, "guarded", "hew_sink_close"),
        0,
        "neither arm closes explicitly"
    );
    assert_eq!(
        count_calls_in_function(&ir, "guarded", "hew_sink_close"),
        6,
        "each arm binder gets its own mutually exclusive exit closes"
    );
}

/// The guard-shape matrix over a contextual `Sink`: a guard that BORROWS the
/// binder, two consecutive guarded `.Ok` arms, a guarded `.Ok` ahead of `.Err`
/// and a wildcard, and a nested `match` inside a guarded arm. Every path is
/// asserted for exact file contents under a scribble-poisoned run, for its
/// exact close-site count, and — via a bounded repeat of the paths that never
/// bind the payload to a body — for the absence of an fd leak.
#[test]
fn guard_shapes_over_a_contextual_sink_write_and_close_on_every_path() {
    let dir = std::env::temp_dir().join(format!("hew-guard-shapes-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create guard-shape temp dir");
    let path_for = |stem: &str| dir.join(format!("{stem}.txt"));
    let expected = [
        // (a) the guard borrows the binder: its write lands on BOTH paths, then
        // the selected arm appends its own.
        ("a_taken", "probe taken"),
        ("a_fallthrough", "probe fallthrough"),
        // (b) two consecutive guarded `.Ok` arms ahead of an unguarded one.
        ("b_first", "first"),
        ("b_second", "second"),
        ("b_third", "third"),
        // (c) guarded `.Ok`, then `.Err`, then `_`. The wildcard path never
        // binds the payload, so the carrier stays its close authority.
        ("c_guarded", "guarded ok"),
        ("c_wildcard", ""),
        // (d) a nested `match` inside the guarded arm.
        ("d_positive", "nested positive"),
        ("d_negative", "nested non-positive"),
        ("d_fallthrough", "outer fallthrough"),
    ];

    let mut source = r#"import std.stream;

fn classify(n: i64) -> Result<string, string> {
    if n > 0 {
        return Ok("positive");
    }
    return Err("non-positive");
}

fn borrow_guard(path: string, flag: bool) {
    match stream.to_file(path) {
        .Ok(sink) if { sink.write("probe "); flag } => {
            sink.write("taken");
        },
        .Ok(sink) => {
            sink.write("fallthrough");
        },
        .Err(e) => println(f"open failed: {e}"),
    }
}

fn two_guards(path: string, first: bool, second: bool) {
    match stream.to_file(path) {
        .Ok(sink) if first => {
            sink.write("first");
        },
        .Ok(sink) if second => {
            sink.write("second");
        },
        .Ok(sink) => {
            sink.write("third");
        },
        .Err(e) => println(f"open failed: {e}"),
    }
}

fn guarded_then_wildcard(path: string, flag: bool) {
    match stream.to_file(path) {
        .Ok(sink) if flag => {
            sink.write("guarded ok");
        },
        .Err(e) => println(f"open failed: {e}"),
        _ => {},
    }
}

fn nested_arm(path: string, flag: bool, n: i64) {
    match stream.to_file(path) {
        .Ok(sink) if flag => {
            match classify(n) {
                .Ok(tag) => sink.write(f"nested {tag}"),
                .Err(e) => sink.write(f"nested {e}"),
            }
        },
        .Ok(sink) => {
            sink.write("outer fallthrough");
        },
        .Err(e) => println(f"open failed: {e}"),
    }
}

fn main() {
    borrow_guard("__A_TAKEN__", true);
    borrow_guard("__A_FALLTHROUGH__", false);
    two_guards("__B_FIRST__", true, false);
    two_guards("__B_SECOND__", false, true);
    two_guards("__B_THIRD__", false, false);
    guarded_then_wildcard("__C_GUARDED__", true);
    guarded_then_wildcard("__C_WILDCARD__", false);
    nested_arm("__D_POSITIVE__", true, 1);
    nested_arm("__D_NEGATIVE__", true, -1);
    nested_arm("__D_FALLTHROUGH__", false, 1);
    // A path that never binds the payload to an arm body leaves the carrier as
    // the close authority. Repeat it past any plausible fd table so a missing
    // close surfaces as `open failed`, not as a silent leak.
    for i in 0..2000 {
        guarded_then_wildcard("__C_WILDCARD__", false);
        two_guards("__B_THIRD__", false, false);
    }
    println("guard-shapes-ok");
}
"#
    .to_string();
    for (stem, _) in expected {
        source = source.replace(
            &format!("__{}__", stem.to_uppercase()),
            &path_for(stem).to_string_lossy(),
        );
    }

    let stdout = run_hew_source_env("guard_shapes_sink", &source, true);
    assert_eq!(stdout, "guard-shapes-ok");
    for (stem, contents) in expected {
        let path = path_for(stem);
        assert_eq!(
            std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("read {}: {e}", path.display())),
            contents,
            "{stem} must hold exactly the selected arm's output"
        );
    }

    let ir = emit_llvm_ir("guard_shapes_sink_ir", &source);
    for (function, total) in [
        ("borrow_guard", 6),
        ("two_guards", 9),
        ("guarded_then_wildcard", 3),
        ("nested_arm", 9),
    ] {
        assert_eq!(
            count_calls_in_function(&ir, function, "hew_sink_close"),
            total,
            "{function} must keep exactly its mutually exclusive exit close sites"
        );
        assert_eq!(
            count_source_calls_in_function(&ir, function, "hew_sink_close"),
            0,
            "{function} closes implicitly on every path"
        );
    }
}

/// A guard that CONSUMES the handed-off `Sink` runs its close before the guard
/// result is known: a false guard would fall through to a later arm holding a
/// closed handle, then close it again. The handoff does not exempt the binder
/// from the projected-payload consume hook, so this is refused fail-closed.
#[test]
fn consuming_the_sink_inside_a_fallthrough_guard_is_refused() {
    let source = r#"import std.stream;

fn guarded(path: string, flag: bool) {
    match stream.to_file(path) {
        .Ok(sink) if { sink.close(); flag } => {
            println("guard taken");
        },
        .Ok(sink) => {
            sink.write("guard fallthrough");
        },
        .Err(e) => println(f"open failed: {e}"),
    }
}

fn main() {
    guarded("/dev/null", false);
}
"#;
    let stderr = compile_expect_refusal("guarded_sink_consume_refusal", source);
    assert!(
        stderr.contains("cannot move the heap-owning payload `sink` out of a `match`-arm guard"),
        "refusal must name the guarded consume; stderr was:\n{stderr}"
    );
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
    let source = r#"import std.stream;

actor Producer {
    let n: i64;

    receive fn run(unused: i64) {
        let (sink, input) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
        for i in 0 .. n {
            await sink.send(f"item-{i}");
        }
        sink.close();
        var done = false;
        while !done {
            match await input.recv() {
                .Some(s) => println(s),
                .None => { done = true; },
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
    let source = r#"import std.stream;

actor Producer {
    let n: i64;

    receive fn run(unused: i64) {
        let (sink, input) = match stream.pipe(8) { .Ok(pair) => pair, .Err(error) => panic(error), };
        for i in 0 .. n {
            await sink.send(f"item-{i}");
        }
        sink.close();
        var done = false;
        while !done {
            match await input.recv() {
                .Some(s) => println(s),
                .None => { done = true; },
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
        r#"import std.stream;

actor Writer {{
    let path: string;

    receive fn run(unused: i64) {{
        match stream.to_file(path) {{
            .Ok(sink) => {{
                await sink.send("line-a\n");
                await sink.send("line-b\n");
                sink.close();
            }},
            .Err(e) => println(f"open failed: {{e}}"),
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
    let source = r#"import std.stream;

fn main() {
    let (sink, input) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let _r = sink.try_send("hi");
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
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
    let source = r#"import std.stream;

actor Writer {
    let sink: stream.Sink<string>;

    receive fn put(item: string) {
        sink.send(item);
    }
}

fn main() {
    let (sink, input) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let w = spawn Writer(sink: sink);
    w.put("hello");
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
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
        count_calls_in_function(&ir, "__hew_state_drop_Writer", "hew_sink_close"),
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
    let source = r#"import std.stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var count = 0;
        var done = false;
        while !done {
            match await input.recv() {
                .Some(_s) => { count = count + 1; },
                .None => { done = true; },
            }
        }
        println(f"count={count}");
    }
}

fn main() {
    let (sink, input) = match stream.pipe(2) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
        count_calls_in_function(&ir, "__hew_state_drop_Consumer", "hew_stream_close"),
        1,
        "the moved Stream half must be closed EXACTLY once (state_drop_fn)"
    );
    assert_eq!(
        count_source_calls_in_function(
            &ir,
            crate::ir_assertions::entry_body_symbol(&ir),
            "hew_sink_close"
        ),
        1,
        "main must contain exactly one source sink.close() call; elaborated \
         cleanup exits are mutually exclusive and covered by the scribble run"
    );
}

// ── Slice E: concurrency edge classes ─────────────────────────────────────

/// Edge 1 — racing send/recv across actors: a consumer actor awaits recv on a
/// Stream half it owns in state while `main` concurrently sends N items into
/// the local sink half. Every item is delivered exactly once, in order,
/// followed by a single EOF. Scribble-clean.
#[test]
fn racing_send_recv_delivers_in_order() {
    let source = r#"import std.stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var done = false;
        while !done {
            match await input.recv() {
                .Some(s) => println(s),
                .None => { done = true; },
            }
        }
        println("eof");
    }
}

fn main() {
    let (sink, input) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
    let source = r#"import std.stream;

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
    let (sink, input) = match stream.pipe(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let p = spawn Producer(sink: sink);
    p.run();
    sleep(200ms);
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
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
    let source = r#"import std.stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var nones = 0;
        match await input.recv() {
            .Some(s) => println(s),
            .None => { nones = nones + 1; },
        }
        println(f"nones={nones}");
    }
}

fn main() {
    let (sink, input) = match stream.pipe(2) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
    let source = r#"import std.stream;

actor Consumer {
    let input: stream.Stream<string>;

    receive fn drain() {
        var count = 0;
        var eofs = 0;
        var done = false;
        while !done {
            match await input.recv() {
                .Some(_v) => { count = count + 1; },
                .None => { eofs = eofs + 1; done = true; },
            }
        }
        println(f"count={count} eofs={eofs}");
    }
}

fn main() {
    let (sink, input) = match stream.pipe(1) { .Ok(pair) => pair, .Err(error) => panic(error), };
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
///
/// The refusal now arrives from the checker instead of MIR: consuming an actor
/// state field without re-initialising it is refused for every affine state
/// field, and that rule reaches this program first. The property under test is
/// unchanged — a state-held handle cannot be explicitly closed — only the layer
/// that says so. The MIR authority keeps its own pin in
/// `explicit_close_of_state_field_handle_is_refused_by_mir_after_reinit`, whose
/// shape the checker rule admits, so the MIR refusal cannot rot behind the
/// earlier one.
#[test]
fn explicit_close_of_state_field_handle_is_refused() {
    let source = r#"import std.stream;

actor Producer {
    let sink: stream.Sink<string>;

    receive fn go() {
        sink.send("x");
        sink.close();
    }
}

fn main() {
    let (sink, input) = match stream.pipe(2) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let p = spawn Producer(sink: sink);
    p.go();
    sleep(100ms);
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("state_field_close_refused", source);
    assert!(
        stderr.contains("actor state `sink` is consumed here and never re-initialised")
            && stderr.contains("actor state outlives the handler that consumed it"),
        "explicit close of a state-held handle must be refused fail-closed; got:\n{stderr}"
    );
}

/// D5 (consume), MIR authority: the same refusal, reached through a shape the
/// checker's state-hole rule admits.
///
/// Re-initialising `sink` plugs the hole the checker cares about, so the
/// program gets past it and MIR still refuses the close on its own grounds —
/// the actor's `state_drop_fn` is the single owner. Without this pin the MIR
/// refusal would be shadowed by the checker's and could regress unobserved.
#[test]
fn explicit_close_of_state_field_handle_is_refused_by_mir_after_reinit() {
    let source = r#"import std.stream;

actor Producer {
    var sink: stream.Sink<string>;

    receive fn go() {
        sink.send("x");
        sink.close();
        let (fresh, _unused) = match stream.pipe(2) { .Ok(pair) => pair, .Err(error) => panic(error), };
        sink = fresh;
    }
}

fn main() {
    let (sink, input) = match stream.pipe(2) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let p = spawn Producer(sink: sink);
    p.go();
    sleep(100ms);
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("state_field_close_reinit_refused", source);
    assert!(
        stderr.contains("closing an owned handle held in actor state") && stderr.contains("Sink"),
        "the MIR refusal for a state-held handle close must still fire; got:\n{stderr}"
    );
}

/// D5 (overwrite): reassigning an owned handle actor-state field (`var sink`;
/// `sink = <fresh>`) is refused fail-closed — the previous handle would leak and
/// the new one be double-owned by teardown.
#[test]
fn overwrite_of_state_field_handle_is_refused() {
    let source = r#"import std.stream;

actor Writer {
    var sink: stream.Sink<string>;

    receive fn reset() {
        let (s, _i) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
        sink = s;
    }
}

fn main() {
    let (sink, input) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let w = spawn Writer(sink: sink);
    w.reset();
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
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
    let source = r#"import std.stream;

actor Writer {
    let sink: stream.Sink<string>;
    receive fn put(item: string) { sink.send(item); }
}

fn main() {
    let (sink, input) = match stream.pipe(4) { .Ok(pair) => pair, .Err(error) => panic(error), };
    let w = spawn Writer(sink: sink);
    sink.send("after-move");
    match input.recv() {
        .Some(s) => println(s),
        .None => println("eof"),
    }
}
"#;
    let stderr = compile_expect_refusal("reuse_after_spawn_refused", source);
    assert!(
        stderr.contains("moved value") || stderr.contains("consumed"),
        "reusing a handle after spawn must stay a move-checker error; got:\n{stderr}"
    );
}
