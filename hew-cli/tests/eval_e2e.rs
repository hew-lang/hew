mod support;

use std::io::{BufRead, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen, strip_ansi};

fn run_eval_with_stdin_in_dir(args: &[&str], input: &str, cwd: &Path) -> Output {
    let mut child = Command::new(hew_binary())
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    {
        let mut stdin = child.stdin.take().expect("stdin should be piped");
        stdin.write_all(input.as_bytes()).unwrap();
    }

    child.wait_with_output().unwrap()
}

fn run_eval_with_stdin(args: &[&str], input: &str) -> Output {
    run_eval_with_stdin_in_dir(args, input, repo_root())
}

fn assert_no_monomorphic_overload_error(stderr: &str) {
    assert!(
        !stderr.contains("no registered monomorphic overload"),
        "stderr still contains the old auto-print overload error: {stderr}"
    );
}

fn surface_fixture(name: &str) -> PathBuf {
    repo_root()
        .join("examples")
        .join("v05")
        .join("surfaces")
        .join(name)
}

fn run_for_await_surface_fixture(name: &str) {
    require_codegen();

    let source = surface_fixture(&format!("{name}.hew"));
    let expected = std::fs::read_to_string(surface_fixture(&format!("{name}.expected")))
        .expect("for-await fixture expected stdout should be readable");
    assert!(
        source.is_file(),
        "for-await fixture missing: {}",
        source.display()
    );

    let mut command = Command::new(hew_binary());
    command
        .arg("run")
        .arg(&source)
        .current_dir(repo_root())
        .env("HEW_WORKERS", "1");
    let output = support::run_bounded_command(
        command,
        format!("hew run {} (HEW_WORKERS=1)", source.display()),
    );

    assert!(
        output.status.success(),
        "for-await fixture {name} should exit 0; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected,
        "for-await fixture {name} produced unexpected stdout",
    );
}

fn for_await_mir_checked_dump(name: &str) -> String {
    require_codegen();

    let source = surface_fixture(&format!("{name}.hew"));
    let mut command = Command::new(hew_binary());
    command
        .args(["compile", "--dump-mir", "checked"])
        .arg(&source)
        .current_dir(repo_root())
        .env("HEW_WORKERS", "1");
    let output = support::run_bounded_command(
        command,
        format!("hew compile --dump-mir checked {}", source.display()),
    );
    assert!(
        output.status.success(),
        "MIR dump for {name} should succeed; stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn for_await_receiver_string_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("for_await_recv_string");
}

#[test]
fn for_await_receiver_int_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("for_await_recv_int");
}

#[test]
fn for_await_stream_bytes_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("for_await_stream_bytes");
}

#[test]
fn for_await_stream_string_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("for_await_stream_string");
}

#[test]
fn typed_streams_string_single_await_recv_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("typed_streams_string");
}

#[test]
fn try_recv_stream_string_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("try_recv_stream_string");
}

#[test]
fn for_await_recv_f64_drains_to_completion_under_single_worker() {
    run_for_await_surface_fixture("for_await_recv_f64");
}

#[test]
fn channel_record_elements_roundtrip_and_early_exit_under_single_worker() {
    run_for_await_surface_fixture("channel_record_elements");
}

#[test]
fn for_await_mir_dump_contains_suspending_recv_terminators() {
    for name in ["for_await_recv_string", "for_await_recv_int"] {
        let dump = for_await_mir_checked_dump(name);
        assert!(
            dump.contains("SuspendingChannelRecv"),
            "{name} must lower to a SuspendingChannelRecv terminator:\n{dump}",
        );
    }

    // Bytes element: `SuspendingStreamNext { elem_ty: Bytes, .. }` — the
    // checker-resolved element type selects the Bytes content-encoding
    // witness for the layout pop in codegen.
    let bytes_dump = for_await_mir_checked_dump("for_await_stream_bytes");
    assert!(
        bytes_dump.contains("SuspendingStreamNext"),
        "for_await_stream_bytes must lower to a SuspendingStreamNext terminator:\n{bytes_dump}",
    );
    assert!(
        bytes_dump.contains("elem_ty: Bytes"),
        "for_await_stream_bytes's SuspendingStreamNext must carry \
         elem_ty: Bytes (the Bytes element witness):\n{bytes_dump}",
    );
    assert!(
        !bytes_dump.contains("elem_ty: String"),
        "for_await_stream_bytes must NOT carry elem_ty: String \
         (string element type leaking into the bytes path):\n{bytes_dump}",
    );

    // String element: `SuspendingStreamNext { elem_ty: String, .. }` — the
    // String element witness keeps the header-aware cstring decode and binds
    // `Option<string>`.
    for name in ["for_await_stream_string", "typed_streams_string"] {
        let dump = for_await_mir_checked_dump(name);
        assert!(
            dump.contains("SuspendingStreamNext"),
            "{name} must lower to a SuspendingStreamNext terminator:\n{dump}",
        );
        assert!(
            dump.contains("elem_ty: String"),
            "{name}'s SuspendingStreamNext must carry elem_ty: String \
             (the String element witness):\n{dump}",
        );
        assert!(
            !dump.contains("elem_ty: Bytes"),
            "{name} must NOT carry elem_ty: Bytes \
             (bytes element type leaking into the string path):\n{dump}",
        );
    }
}

enum WaitOutcome {
    Found,
    Timeout,
    ChildClosed,
}

fn wait_for_line(rx: &mpsc::Receiver<String>, needle: &str, budget: Duration) -> WaitOutcome {
    let start = Instant::now();
    while start.elapsed() < budget {
        match rx.recv_timeout(budget.saturating_sub(start.elapsed())) {
            Ok(line) if line.contains(needle) => return WaitOutcome::Found,
            Ok(_) => {} // banner-adjacent line or blank line — keep waiting
            Err(RecvTimeoutError::Timeout) => return WaitOutcome::Timeout,
            Err(RecvTimeoutError::Disconnected) => return WaitOutcome::ChildClosed,
        }
    }
    WaitOutcome::Timeout
}

#[test]
fn timeout_zero_is_rejected() {
    let output = Command::new(hew_binary())
        .args(["eval", "--timeout", "0", "1 + 1"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[test]
fn eval_inline_expression_succeeds() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "1 + 2"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn eval_std_observe_reads_runtime_metric() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("observe_eval.hew");
    std::fs::write(
        &path,
        "import std::observe;\n\nobserve.read(\"heap.live_bytes\") >= 0\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "true\n");
}

#[test]
fn eval_std_observe_scrape_and_series_include_actor_attribution() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("observe_attribution_eval.hew");
    std::fs::write(
        &path,
        r#"import std::observe;

actor Counter {
    var count: i64;

    receive fn increment(n: i64) {
        count = count + n;
    }

    receive fn total() -> i64 {
        count
    }
}

// The awaited reply posts BEFORE the worker records the turn's
// attribution, so the attributed series may lag the ask by a beat on a
// loaded host. Poll the eventual invariant with a bounded deadline
// (2s) instead of asserting a racy instantaneous read. (A function
// body, not top-level statements: the eval session splits top-level
// control flow away from its bindings.)
fn wait_for_attribution() -> i64 {
    var tries = 0;
    while tries < 200 {
        let snapshot = observe.series();
        if snapshot.contains("actors.attributed_turns_by_handler_total") {
            tries = 200;
        } else {
            sleep_ms(10);
            tries = tries + 1;
        }
    }
    0
}

let counter = spawn Counter(count: 0);
counter.increment(1);
counter.increment(2);
let _total = await counter.total();
let _waited = wait_for_attribution();
println(observe.series());
observe.scrape()
"#,
    )
    .unwrap();

    let mut command = Command::new(hew_binary());
    command
        .args(["eval", "--timeout", "30", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .env("HEW_OBSERVE", "hot");

    // `run_bounded_command` contains the compiled-Hew process tree with a
    // wall-clock timeout; Darwin has no portable per-child data cap here, so
    // this test uses the timeout-only fallback requested for macOS.
    let output = support::run_bounded_command(command, format!("hew eval {}", path.display()));

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("actors.attributed_turns_by_handler_total"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("actors_attributed_turns_by_handler_total"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("actors_attributed_turn_duration_ns_by_handler_total"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("Counter::total"), "stdout: {stdout}");
    assert!(
        stdout.lines().any(
            |line| line.starts_with("actors_attributed_turns_by_handler_total{")
                && !line.ends_with(" 0")
        ),
        "stdout: {stdout}"
    );
    assert!(
        stdout.lines().any(|line| line
            .starts_with("actors_attributed_turn_duration_ns_by_handler_total{")
            && !line.ends_with(" 0")),
        "stdout: {stdout}"
    );
}

#[test]
fn eval_inline_unit_expression_is_not_auto_printed() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", r#"print("unit side-effect\n")"#])
        .current_dir(repo_root())
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "stderr: {stderr}");
    assert_no_monomorphic_overload_error(&stderr);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "unit side-effect\n"
    );
}

#[test]
fn eval_file_in_repl_context_succeeds() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("positive_eval.hew");
    std::fs::write(
        &path,
        "fn identity<T>(x: T) -> T {\n    x\n}\n\nidentity(42)\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_file_resolves_sibling_imports_relative_to_file_path() {
    require_codegen();

    let dir = support::tempdir();
    let nested = dir.path().join("nested");
    std::fs::create_dir_all(&nested).unwrap();
    std::fs::write(
        nested.join("lib.hew"),
        "pub fn answer() -> i64 {\n    42\n}\n",
    )
    .unwrap();
    let path = nested.join("main.hew");
    std::fs::write(&path, "import \"lib.hew\";\n\nanswer()\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_stdin_in_repl_context_succeeds() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval", "-f", "-"],
        "fn identity<T>(x: T) -> T {\n    x\n}\n\nidentity(42)\n",
    );

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_stdin_file_mode_resolves_imports_from_cwd() {
    require_codegen();

    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("lib.hew"),
        "pub fn answer() -> i64 {\n    42\n}\n",
    )
    .unwrap();

    let output = run_eval_with_stdin_in_dir(
        &["eval", "-f", "-"],
        "import \"lib.hew\";\n\nanswer()\n",
        dir.path(),
    );

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn statement_replay_stdin_does_not_repeat_one_shot_statement() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval", "-f", "-"], "println(\"once\");\n1 + 1\n");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "once\n2\n");
}

#[test]
fn statement_replay_stdin_keeps_explicit_binding() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval", "-f", "-"], "let x = 41;\nx + 1\n");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn statement_replay_repl_does_not_repeat_one_shot_statement() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval"], "println(\"once\");\n1 + 1\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.matches("once\n").count(), 1, "stdout: {stdout}");
    assert!(stdout.contains("2\n"), "stdout: {stdout}");
}

#[test]
fn repl_loads_and_runs_hello_world_file() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval"],
        ":load examples/playground/basics/hello_world.hew\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Hello, World!\n"), "stdout: {stdout}");
    assert!(
        stdout.contains("Loaded examples/playground/basics/hello_world.hew"),
        "stdout: {stdout}"
    );
}

#[test]
fn repl_accepts_multiline_function_and_keeps_it_in_session() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval"],
        "fn foo() -> i64 {\n    41\n}\nfoo() + 1\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("42\n"), "stdout: {stdout}");
}

#[test]
fn repl_type_command_reads_interactive_session_binding() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval"], "var x = 10;\n:type x\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("i64\n"), "stdout: {stdout}");
}

#[test]
fn repl_displays_typed_generator_description() {
    let output = run_eval_with_stdin(&["eval"], "gen { yield 1; yield 2; }\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("<generator yielding i64, returning ()>\n"),
        "stdout: {stdout}"
    );
    assert!(
        !stdout.contains("<generator>\n"),
        "stdout must include typed generator details, not a placeholder: {stdout}"
    );
}

#[test]
fn repl_type_command_surfaces_diagnostic_when_no_type_info() {
    let output = run_eval_with_stdin(&["eval"], ":type None\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("could not determine type: no type information for expression"),
        "stderr: {stderr}"
    );
    assert!(
        !stdout.contains("could not determine type"),
        "diagnostic must be emitted on stderr, not stdout: {stdout}"
    );
    assert!(!stdout.contains("unknown"), "stdout: {stdout}");
    assert!(!stderr.contains("unknown"), "stderr: {stderr}");
}

#[test]
fn eval_timeout_exit_code_is_non_zero() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("timeout_eval.hew");
    std::fs::write(
        &path,
        "fn spin_forever() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}\n\nspin_forever()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("1")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: evaluation timed out after 1s"));
}

#[test]
fn eval_large_stdout_completes_before_timeout() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("large_stdout_eval.hew");
    std::fs::write(
        &path,
        "scope {\n    var i = 0;\n    while i < 20000 {\n        println(\"line\");\n        i = i + 1;\n    }\n}\n",
    )
    .unwrap();

    // The --timeout here is a hang watchdog, not a performance gate.  20 000
    // println calls complete in well under a second on any reasonable host.
    // This is a hang watchdog, not a perf gate: 300 s is large enough that
    // only a genuinely stuck program (not load/scheduler jitter, even under
    // heavy concurrent build+test load) can ever trip it.
    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("300")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout.lines().count(), 20_000);
    assert!(stdout.starts_with("line\n"), "stdout: {stdout}");
    assert!(stdout.ends_with("line\n"), "stdout: {stdout}");
}

#[test]
fn eval_large_stderr_completes_before_timeout() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("large_stderr_eval.hew");
    std::fs::write(
        &path,
        "import std::io;\n\nfn spam_err() {\n    var i = 0;\n    while i < 20000 {\n        io.write_err(\"line\\n\");\n        i = i + 1;\n    }\n}\n\nspam_err()\n42\n",
    )
    .unwrap();

    // The --timeout here is a hang watchdog, not a performance gate.  20 000
    // write_err calls complete in well under a second on any reasonable host.
    // This is a hang watchdog, not a perf gate: 300 s is large enough that
    // only a genuinely stuck program (not load/scheduler jitter, even under
    // heavy concurrent build+test load) can ever trip it.
    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("300")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

#[test]
fn eval_repl_timeout_is_reported_and_quit_still_works() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval", "--timeout", "1"],
        "scope {\n    loop {\n        println(\"spin\");\n    }\n}\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("evaluation timed out after 1s"),
        "stderr: {stderr}"
    );
}

#[test]
fn eval_repl_continues_balanced_incomplete_expression() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval"], "1 +\n2\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("3\n"), "stdout: {stdout}");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(!stderr.contains("expected expression"), "stderr: {stderr}");
}

#[test]
fn eval_repl_reports_balanced_invalid_input_without_waiting() {
    let output = run_eval_with_stdin(&["eval"], "1 + )\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<repl>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | 1 + )"), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
}

#[test]
fn eval_stdin_continues_balanced_incomplete_expression() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval", "-f", "-"], "1 +\n2\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn eval_stdin_reports_balanced_invalid_input() {
    let output = run_eval_with_stdin(&["eval", "-f", "-"], "1 + *\n");

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<stdin>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("expected expression"), "stderr: {stderr}");
    assert!(stderr.contains("1 | 1 + *"), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_file_continues_balanced_incomplete_expression() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("balanced_incomplete.hew");
    std::fs::write(&path, "1 +\n2\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

#[test]
fn eval_file_reports_balanced_invalid_input() {
    let dir = support::tempdir();
    let path = dir.path().join("balanced_invalid.hew");
    std::fs::write(&path, "1 + *\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:1:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("expected expression"), "stderr: {stderr}");
    assert!(stderr.contains("1 | 1 + *"), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_inline_parse_errors_render_cli_diagnostics() {
    let output = Command::new(hew_binary())
        .args(["eval", "1 +"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<eval>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | 1 +"), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_inline_statement_type_errors_render_user_input() {
    let output = Command::new(hew_binary())
        .args(["eval", "let x: i64 = \"oops\";"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<eval>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("type mismatch: expected `i64`, found `string`"),
        "stderr: {stderr}"
    );
    assert!(
        stderr.contains("1 | let x: i64 = \"oops\";"),
        "stderr: {stderr}"
    );
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_inline_expression_type_errors_render_user_input() {
    let output = Command::new(hew_binary())
        .args(["eval", "1 + \"x\""])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<eval>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("cannot apply `+` to `i64` and `string`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("1 | 1 + \"x\""), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_repl_statement_type_errors_render_user_input() {
    let output = run_eval_with_stdin(&["eval"], "let x: i64 = \"oops\";\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<repl>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("type mismatch: expected `i64`, found `string`"),
        "stderr: {stderr}"
    );
    assert!(
        stderr.contains("1 | let x: i64 = \"oops\";"),
        "stderr: {stderr}"
    );
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
}

#[test]
fn eval_repl_expression_type_errors_render_user_input() {
    let output = run_eval_with_stdin(&["eval"], "1 + \"x\"\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<repl>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("cannot apply `+` to `i64` and `string`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("1 | 1 + \"x\""), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
}

#[test]
fn eval_stdin_statement_type_errors_render_user_input() {
    let output = run_eval_with_stdin(&["eval", "-f", "-"], "let x: i64 = \"oops\";\n");

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<stdin>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("type mismatch: expected `i64`, found `string`"),
        "stderr: {stderr}"
    );
    assert!(
        stderr.contains("1 | let x: i64 = \"oops\";"),
        "stderr: {stderr}"
    );
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_stdin_expression_type_errors_render_user_input() {
    let output = run_eval_with_stdin(&["eval", "-f", "-"], "1 + \"x\"\n");

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<stdin>:1:"), "stderr: {stderr}");
    assert!(
        stderr.contains("cannot apply `+` to `i64` and `string`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("1 | 1 + \"x\""), "stderr: {stderr}");
    assert!(!stderr.contains("println("), "stderr: {stderr}");
    assert!(!stderr.contains("2 |"), "stderr: {stderr}");
    assert!(!stderr.contains("fn main()"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

#[test]
fn eval_repl_load_parse_errors_render_cli_diagnostics() {
    let dir = support::tempdir();
    let path = dir.path().join("bad_load.hew");
    std::fs::write(&path, "fn broken(\n").unwrap();

    let output = run_eval_with_stdin(&["eval"], &format!(":load {}\n:quit\n", path.display()));

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:1:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("1 | fn broken("), "stderr: {stderr}");
}

#[test]
fn eval_repl_load_non_root_type_errors_render_imported_filename() {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    let dep_path = dir.path().join("dep.hew");
    std::fs::write(&path, "import \"dep.hew\";\n\nfn main() {}\n").unwrap();
    std::fs::write(&dep_path, "pub fn mistyped() -> i64 { true }\n").unwrap();

    let output = run_eval_with_stdin(&["eval"], &format!(":load {}\n:quit\n", path.display()));

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    // The import resolver canonicalizes the dep path, so the rendered
    // filename uses the canonical form — on Windows runners that expands
    // 8.3 short names (RUNNER~1) the raw tempdir path would never match.
    let dep_canonical = dep_path.canonicalize().unwrap_or_else(|_| dep_path.clone());
    let dep_header = format!("{}:1:", dep_canonical.display());
    assert!(stderr.contains(&dep_header), "stderr: {stderr}");
    assert!(
        stderr.contains("pub fn mistyped() -> i64 { true }"),
        "stderr: {stderr}"
    );
    assert!(
        !stderr.contains("diagnostics already rendered"),
        "stderr: {stderr}"
    );
}

#[test]
fn eval_repl_load_valid_file_succeeds() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("load_ok.hew");
    std::fs::write(&path, "fn answer() -> i64 {\n    42\n}\n").unwrap();

    let output = run_eval_with_stdin(
        &["eval"],
        &format!(":load {}\nanswer()\n:quit\n", path.display()),
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&format!("Loaded {}", path.display())),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("added 1 item"), "stdout: {stdout}");
    assert!(stdout.contains("42\n"), "stdout: {stdout}");
}

#[test]
fn eval_repl_load_resolves_sibling_imports_relative_to_file_path() {
    require_codegen();

    let dir = support::tempdir();
    let nested = dir.path().join("nested");
    std::fs::create_dir_all(&nested).unwrap();
    std::fs::write(
        nested.join("lib.hew"),
        "pub fn answer() -> i64 {\n    42\n}\n",
    )
    .unwrap();

    let path = nested.join("load_main.hew");
    std::fs::write(&path, "import \"lib.hew\";\n\nanswer()\n").unwrap();

    let output = run_eval_with_stdin(&["eval"], &format!(":load {}\n:quit\n", path.display()));

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&format!("Loaded {}", path.display())),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("42\n"), "stdout: {stdout}");
}

/// `:clear` must emit "Session cleared." to stdout regardless of whether the
/// codegen backend is available — it is a pure session-state operation.
#[test]
fn eval_repl_clear_emits_confirmation() {
    let output = run_eval_with_stdin(&["eval"], ":clear\n:quit\n");

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Session cleared."), "stdout: {stdout}");
}

/// `:clear` must drop all accumulated items and bindings so that previously
/// defined names are no longer visible and can be safely redefined.
#[test]
fn eval_repl_clear_resets_session_state() {
    require_codegen();

    // Round 1: define a function, call it (get 42).
    // Round 2: :clear, redefine the *same* function with 99, call it again.
    // If :clear didn't reset the session the duplicate definition would cause
    // a compile error or the old value would leak through.
    let output = run_eval_with_stdin(
        &["eval"],
        "fn answer() -> i64 { 42 }\nanswer()\n:clear\nfn answer() -> i64 { 99 }\nanswer()\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("42\n"), "stdout: {stdout}");
    assert!(stdout.contains("Session cleared."), "stdout: {stdout}");
    assert!(stdout.contains("99\n"), "stdout: {stdout}");
}

#[test]
fn eval_repl_session_commands_introspect_state() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval"],
        "let base = 41;\nfn answer(x: i64) -> i64 { x + 1 }\n:session\n:items\n:bindings\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Session state:"), "stdout: {stdout}");
    assert!(stdout.contains("1 remembered item"), "stdout: {stdout}");
    assert!(stdout.contains("1 persistent binding"), "stdout: {stdout}");
    assert!(stdout.contains("Remembered items (1):"), "stdout: {stdout}");
    assert!(stdout.contains("fn answer"), "stdout: {stdout}");
    assert!(
        stdout.contains("Persistent bindings (1):"),
        "stdout: {stdout}"
    );
    assert!(stdout.contains("let base"), "stdout: {stdout}");
}

#[test]
fn eval_repl_load_reports_session_delta() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("load_session_delta.hew");
    std::fs::write(
        &path,
        "let base = 41;\nfn answer(x: i64) -> i64 {\n    x + 1\n}\n",
    )
    .unwrap();

    let output = run_eval_with_stdin(&["eval"], &format!(":load {}\n:quit\n", path.display()));

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&format!(
            "Loaded {} (added 1 item, 1 binding)",
            path.display()
        )),
        "stdout: {stdout}"
    );
}

#[test]
fn eval_repl_clear_reports_removed_session_delta() {
    require_codegen();

    let output = run_eval_with_stdin(
        &["eval"],
        "let base = 41;\nfn answer(x: i64) -> i64 { x + 1 }\n:clear\n:quit\n",
    );

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Session cleared."), "stdout: {stdout}");
    assert!(
        stdout.contains("Removed 1 item, 1 binding."),
        "stdout: {stdout}"
    );
}

#[test]
fn eval_file_type_errors_render_cli_diagnostics() {
    let dir = support::tempdir();
    let path = dir.path().join("negative_eval.hew");
    std::fs::write(&path, "fn broken() -> i64 {\n    \"oops\"\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let header = format!("{}:2:", path.display());
    assert!(stderr.contains(&header), "stderr: {stderr}");
    assert!(
        stderr.contains("type mismatch: expected `i64`, found `string`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("2 |     \"oops\""), "stderr: {stderr}");
    assert!(stderr.contains("|     ^^^^^^"), "stderr: {stderr}");
    assert!(!stderr.contains("Error:"), "stderr: {stderr}");
}

// ---------------------------------------------------------------------------
// WASM target tests
// ---------------------------------------------------------------------------

/// `hew eval --target wasm32-wasi <expr>` compiles and runs a simple inline
/// expression through wasmtime, capturing stdout.
// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_inline_expression_succeeds() {
    require_codegen();
    support::require_wasi_runner();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "1 + 2"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "3\n");
}

/// `hew eval --target wasm32-wasi -f <file>` evaluates a .hew file via WASM.
// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_file_succeeds() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_eval_file.hew");
    std::fs::write(&path, "fn double(x: i64) -> i64 { x * 2 }\ndouble(21)\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n");
}

fn assert_wasm_eval_file_output(test_name: &str, source: &str, expected_stdout: &str) {
    require_codegen();
    if !support::try_require_wasi_runner() {
        return;
    }

    let dir = support::tempdir();
    let path = dir.path().join(format!("{test_name}.hew"));
    std::fs::write(&path, source).expect("write wasm eval oracle source");

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "{test_name}: stderr:\n{}\nstdout:\n{}",
        String::from_utf8_lossy(&output.stderr),
        String::from_utf8_lossy(&output.stdout)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_stdout,
        "{test_name}: stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn eval_wasm_hashmap_string_i64_values_are_correct() {
    assert_wasm_eval_file_output(
        "wasm_hashmap_string_i64",
        r#"
{
    let m: HashMap<string, i64> = HashMap::new();
    m.insert("alpha", 17);
    m.insert("beta", 25);

    let alpha = match m.get("alpha") {
        Some(v) => v,
        None => panic("alpha: missing value"),
    };
    let beta = match m.get("beta") {
        Some(v) => v,
        None => panic("beta: missing value"),
    };
    let sum = alpha + beta;

    if alpha != 17 { panic(f"alpha: expected 17, got {alpha}"); }
    if beta != 25 { panic(f"beta: expected 25, got {beta}"); }
    if !m.contains_key("alpha") { panic("contains alpha: expected true"); }
    if m.contains_key("missing") { panic("contains missing: expected false"); }
    if m.len() != 2 { panic(f"len: expected 2, got {m.len()}"); }
    if sum != 42 { panic(f"sum: expected 42, got {sum}"); }

    print("hashmap-string:");
    print(alpha);
    print(":");
    print(beta);
    print(":");
    print(sum);
    print(":");
    println(m.len());
    sum
}
"#,
        "hashmap-string:17:25:42:2\n42\n",
    );
}

#[test]
fn eval_wasm_hashmap_record_key_uses_thunk_values_correctly() {
    assert_wasm_eval_file_output(
        "wasm_hashmap_point_i64",
        r#"
record Point {
    x: i64,
    y: i64
}

{
    let m: HashMap<Point, i64> = HashMap::new();
    m.insert(Point { x: 3, y: 4 }, 88);
    m.insert(Point { x: 5, y: 6 }, 99);

    let first = match m.get(Point { x: 3, y: 4 }) {
        Some(v) => v,
        None => panic("first point: missing value"),
    };
    let second = match m.get(Point { x: 5, y: 6 }) {
        Some(v) => v,
        None => panic("second point: missing value"),
    };

    if first != 88 { panic(f"first point: expected 88, got {first}"); }
    if second != 99 { panic(f"second point: expected 99, got {second}"); }
    if !m.contains_key(Point { x: 3, y: 4 }) { panic("contains first point: expected true"); }
    if m.contains_key(Point { x: 0, y: 0 }) { panic("contains missing point: expected false"); }
    if m.len() != 2 { panic(f"len: expected 2, got {m.len()}"); }

    print("hashmap-point:");
    print(first);
    print(":");
    print(second);
    print(":");
    println(m.len());
    first
}
"#,
        "hashmap-point:88:99:2\n88\n",
    );
}

#[test]
fn eval_wasm_hashset_string_values_are_correct() {
    assert_wasm_eval_file_output(
        "wasm_hashset_string",
        r#"
{
    let s: HashSet<string> = HashSet::new();
    let inserted_alpha = s.insert("alpha");
    let inserted_beta = s.insert("beta");
    let duplicate_alpha = s.insert("alpha");
    let has_alpha_before_remove = s.contains("alpha");
    let has_missing = s.contains("missing");
    let len_before_remove = s.len();
    let removed_alpha = s.remove("alpha");
    let has_alpha_after_remove = s.contains("alpha");
    let len_after_remove = s.len();

    if !inserted_alpha { panic("insert alpha: expected true"); }
    if !inserted_beta { panic("insert beta: expected true"); }
    if duplicate_alpha { panic("duplicate alpha: expected false"); }
    if !has_alpha_before_remove { panic("contains alpha before remove: expected true"); }
    if has_missing { panic("contains missing: expected false"); }
    if len_before_remove != 2 { panic(f"len before remove: expected 2, got {len_before_remove}"); }
    if !removed_alpha { panic("remove alpha: expected true"); }
    if has_alpha_after_remove { panic("contains alpha after remove: expected false"); }
    if len_after_remove != 1 { panic(f"len after remove: expected 1, got {len_after_remove}"); }

    print("hashset-string:");
    print(inserted_alpha);
    print(":");
    print(duplicate_alpha);
    print(":");
    print(has_alpha_before_remove);
    print(":");
    print(removed_alpha);
    print(":");
    println(len_after_remove);
    len_before_remove + len_after_remove
}
"#,
        "hashset-string:true:false:true:true:1\n3\n",
    );
}

/// A WASM eval that runs longer than the timeout exits with a timeout error.
// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_timeout_is_reported() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_eval_timeout.hew");
    // A function that loops forever — compiles fine under WASM32 (no
    // structured-concurrency APIs), but wasmtime will spin until the timeout.
    std::fs::write(
        &path,
        "fn spin_forever() -> i64 {\n    var i: i64 = 0;\n    while true { i = i + 1; }\n    i\n}\nspin_forever()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "--timeout", "1", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("timed out after 1s"),
        "expected timeout message, stderr: {stderr}"
    );
}

/// Source that uses a feature unsupported on WASM32 (structured-concurrency
/// `scope`) should surface the expected unsupported diagnostic and fail.
#[test]
fn eval_wasm_unsupported_feature_reports_diagnostic() {
    require_codegen();
    if !support::try_require_wasi_runner() {
        return;
    }

    let dir = support::tempdir();
    let path = dir.path().join("wasm_eval_unsupported.hew");
    // `scope { }` uses OS-thread-backed structured concurrency — not available
    // on WASM32.  The compiler should emit an "not supported on WASM32"
    // diagnostic and exit non-zero before wasmtime is ever invoked.
    std::fs::write(&path, "scope {\n    println(\"hello\");\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "expected failure for unsupported WASM feature"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not supported on WASM32") || stderr.contains("WASM32"),
        "expected unsupported WASM diagnostic, stderr: {stderr}"
    );
}

/// `hew eval --target wasm32-wasi` without an expression or file is rejected
/// with a clear diagnostic — interactive REPL is not supported for WASI targets.
#[test]
fn eval_wasm_interactive_mode_rejected() {
    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi"])
        .current_dir(repo_root())
        .stdin(Stdio::null())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "expected non-zero exit for WASI interactive REPL attempt"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("interactive REPL is not supported"),
        "expected rejection diagnostic in stderr, got: {stderr}"
    );
    assert!(
        stderr.contains("wasm32-wasi"),
        "expected target name in rejection message, got: {stderr}"
    );
}

/// `hew eval --target wasm32-wasi "scope { }"` — `scope` is not supported on
/// WASM32. The fast typecheck pass (before codegen) should surface this
/// diagnostic, so the process fails quickly without invoking the compiler.
#[test]
fn eval_wasm_fast_typecheck_rejects_wasm_unsupported_ops() {
    // No codegen required: the fast typecheck should catch this.
    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "scope { }"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "expected failure for scope {{ }} on WASM target"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("WASM32") || stderr.contains("not supported"),
        "expected WASM diagnostic from fast typecheck, stderr: {stderr}"
    );
}

/// A wasm32 compile must reject `for await item in rx` over a channel receiver
/// before link/runtime discovery. The `for await` HIR desugar now reaches the
/// suspending recv carrier on native targets, so this pins the wasm fail-closed
/// gate directly against the compile path rather than relying on REPL chunking.
#[test]
fn compile_wasm_rejects_for_await_receiver_before_link() {
    let dir = support::tempdir();
    let path = dir.path().join("for_await_receiver_wasm.hew");
    std::fs::write(
        &path,
        concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        ),
    )
    .expect("write wasm for-await receiver fixture");

    let output = Command::new(hew_binary())
        .args(["compile", "--target", "wasm32-wasi"])
        .arg(&path)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "expected failure for `for await` over Receiver<T> on WASM target"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Blocking channel receive")
            || stderr.contains("UnknownType { name: \"Receiver\" }"),
        "expected pre-codegen channel receiver diagnostic, stderr: {stderr}"
    );
}

// ── Runtime-failure output contract ──────────────────────────────────────────
//
// When a compiled Hew program exits non-zero, `hew eval` must:
//   1. Print any stdout the program produced before failure to its own stdout.
//   2. Exit with the child's exact exit code (not always 1).
//
// `panic()` is the Hew builtin that exits non-zero; it uses exit code 101
// (Rust's panic convention), which is distinct from the CLI's own error exit
// code (1) and therefore makes propagation detectable.

#[test]
fn eval_inline_runtime_failure_exits_with_child_exit_code() {
    require_codegen();

    // `panic` exits the child with code 101.  Without the fix `hew eval`
    // would always return 1 regardless of the child's code.
    let output = Command::new(hew_binary())
        .args(["eval", r#"panic("deliberate failure")"#])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101 (Hew panic), got {:?}",
        output.status.code()
    );
}

#[test]
fn eval_inline_runtime_failure_surfaces_child_stderr() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", r#"panic("deliberate failure")"#])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_no_monomorphic_overload_error(&stderr);
    assert!(
        stderr.contains("deliberate failure"),
        "expected runtime stderr on the terminal path, got: {stderr:?}"
    );
}

#[test]
fn eval_file_runtime_failure_exits_with_child_exit_code() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("failing_eval.hew");
    // A single expression that unconditionally panics.  `panic` exits with
    // code 101 (Hew's convention), which is distinct from the CLI's own
    // error exit code (1) and makes propagation detectable.
    std::fs::write(&path, "panic(\"deliberate failure\")\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101 (Hew panic), got {:?}",
        output.status.code()
    );
}

#[test]
fn eval_file_runtime_failure_preserves_pre_failure_stdout() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("partial_output_eval.hew");
    // A helper that prints before panicking, called as a single expression so
    // both print and panic run inside the same compiled binary.  Stdout
    // produced before the panic must not be silently discarded.
    std::fs::write(
        &path,
        "fn do_and_fail() {\n    print(\"partial\\n\");\n    panic(\"deliberate failure\");\n}\ndo_and_fail()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("partial"),
        "pre-failure stdout was discarded; got stdout: {stdout:?}"
    );
}

// ── WASM runtime-failure output contract (parity with native path) ────────────
//
// `hew eval --target wasm32-wasi` must honour the same contract as native eval:
//   1. Stdout produced before failure must not be discarded.
//   2. The child's exit code must be propagated, not hard-coded to 1.

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_inline_runtime_failure_exits_with_child_exit_code() {
    require_codegen();
    support::require_wasi_runner();

    // `panic` exits the WASM module with code 101.  Without parity the WASM
    // path would swallow the code and always return 1.
    let output = Command::new(hew_binary())
        .args([
            "eval",
            "--target",
            "wasm32-wasi",
            r#"panic("deliberate failure")"#,
        ])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101 (Hew panic via WASM), got {:?}",
        output.status.code()
    );
}

// Follow-on eval work owns unignoring the broader WASI eval/stdout suite. This ignored
// ratchet pins the narrower attributed-trap contract added before that cutover:
// codegen still emits `hew_trap_with_code` followed by `llvm.trap`, and the
// wasm32 runtime maps canonical non-actor trap code 201 to the child exit code.
// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_integer_overflow_exits_with_trap_code_201() {
    require_codegen();
    support::require_wasi_runner();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "9223372036854775807 + 1"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(201),
        "expected child exit code 201 (Hew integer overflow via WASM), got {:?}",
        output.status.code()
    );
}

// Follow-on eval work owns unignoring the broader WASI eval/stdout suite. This ignored
// ratchet proves attributed traps do not collapse to Hew panic code 101.
#[ignore = "blocked on WASM divide-by-zero trap exit code collapsing to 1 instead of 202"]
#[test]
fn eval_wasm_divide_by_zero_exits_with_trap_code_202() {
    require_codegen();
    support::require_wasi_runner();

    let output = Command::new(hew_binary())
        .args([
            "eval",
            "--target",
            "wasm32-wasi",
            "let a: i64 = 10; let b: i64 = 0; a / b",
        ])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(202),
        "expected child exit code 202 (Hew divide by zero via WASM), got {:?}",
        output.status.code()
    );
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_file_runtime_failure_exits_with_child_exit_code() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_failing_eval.hew");
    std::fs::write(
        &path,
        "fn fail() -> i64 {\n    panic(\"deliberate failure\")\n}\nfail()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101 (Hew panic via WASM), got {:?}",
        output.status.code()
    );
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_file_runtime_failure_preserves_pre_failure_stdout() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_partial_output_eval.hew");
    std::fs::write(
        &path,
        "fn do_and_fail() -> i64 {\n    print(\"wasm-partial\\n\");\n    panic(\"deliberate failure\")\n}\ndo_and_fail()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("wasm-partial"),
        "WASM pre-failure stdout was discarded; got stdout: {stdout:?}"
    );
}

// ── JSON run contract ─────────────────────────────────────────────────────────
//
// `hew eval --json` must emit a single JSON object on stdout (exit 0) whose
// `status` field is one of "ok", "compile_error", or "runtime_failure".
//
// Contract invariants (all three outcomes):
//   - Exactly one JSON object on stdout; nothing else on stdout.
//   - Process exits 0 regardless of outcome.
//   - `status`      distinguishes the three outcome categories.
//   - `stdout`      contains program output (may be empty).
//   - `stderr`      contains captured runtime stderr (empty otherwise).
//   - `exit_code`   is the child exit code (0 for ok/compile_error).
//   - `diagnostics` is non-empty exactly when status == "compile_error".

#[test]
fn eval_json_ok_inline_expression() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "1 + 2"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 with --json, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "ok", "unexpected status: {v}");
    assert_eq!(v["stdout"], "3\n", "unexpected stdout: {v}");
    assert_eq!(v["stderr"], "", "stderr must be empty on ok: {v}");
    assert_eq!(v["exit_code"], 0, "unexpected exit_code: {v}");
    assert_eq!(v["diagnostics"], "", "diagnostics must be empty on ok: {v}");
}

#[test]
fn eval_json_runtime_failure() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", r#"panic("deliberate")"#])
        .current_dir(repo_root())
        .output()
        .unwrap();

    // --json must exit 0 even on runtime failure.
    assert!(
        output.status.success(),
        "expected exit 0 with --json, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let parent_stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !parent_stderr.contains("deliberate"),
        "runtime stderr leaked to the parent stderr stream: {parent_stderr:?}"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "runtime_failure", "unexpected status: {v}");
    assert_eq!(
        v["exit_code"], 101,
        "expected child exit code 101 (Hew panic): {v}"
    );
    assert!(
        v["stderr"].as_str().unwrap_or("").contains("deliberate"),
        "runtime stderr missing from JSON contract: {v}"
    );
    assert_eq!(
        v["diagnostics"], "",
        "diagnostics must be empty on runtime_failure: {v}"
    );
}

#[test]
fn eval_json_runtime_failure_preserves_stdout() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("partial.hew");
    std::fs::write(
        &path,
        "fn do_it() {\n    print(\"before-fail\\n\");\n    panic(\"deliberate\");\n}\ndo_it()\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(output.status.success(), "expected exit 0 with --json");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "runtime_failure", "unexpected status: {v}");
    assert!(
        v["stdout"].as_str().unwrap_or("").contains("before-fail"),
        "pre-failure stdout was not preserved in JSON: {v}"
    );
    assert!(
        v["stderr"].as_str().unwrap_or("").contains("deliberate"),
        "runtime stderr missing from JSON contract: {v}"
    );
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_json_ok_inline_expression() {
    require_codegen();
    support::require_wasi_runner();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "--target", "wasm32-wasi", "1 + 2"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 with --json, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "ok", "unexpected status: {v}");
    assert_eq!(v["stdout"], "3\n", "unexpected stdout: {v}");
    assert_eq!(v["stderr"], "", "stderr must be empty on ok: {v}");
    assert_eq!(v["exit_code"], 0, "unexpected exit_code: {v}");
    assert_eq!(v["diagnostics"], "", "diagnostics must be empty on ok: {v}");
}

// WINDOWS-TODO: requires wasmtime runtime which is not configured on Windows.
#[cfg_attr(windows, ignore)]
#[test]
fn eval_wasm_json_runtime_failure_captures_stderr_without_leaking() {
    require_codegen();
    support::require_wasi_runner();

    let output = Command::new(hew_binary())
        .args([
            "eval",
            "--json",
            "--target",
            "wasm32-wasi",
            r#"if true { panic("deliberate wasm failure") } else { 0 }"#,
        ])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 with --json, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let parent_stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !parent_stderr.contains("deliberate wasm failure"),
        "WASM runtime stderr leaked to parent stderr: {parent_stderr:?}"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "runtime_failure", "unexpected status: {v}");
    assert_eq!(v["exit_code"], 101, "expected child exit code 101: {v}");
    assert!(
        v["stderr"]
            .as_str()
            .unwrap_or("")
            .contains("deliberate wasm failure"),
        "WASM runtime stderr missing from JSON contract: {v}"
    );
}

#[test]
fn eval_json_compile_error() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "this_does_not_exist_at_all()"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    // --json must exit 0 even on compile error.
    assert!(
        output.status.success(),
        "expected exit 0 with --json on compile error, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "compile_error", "unexpected status: {v}");
    assert_eq!(v["exit_code"], 0, "unexpected exit_code: {v}");
    assert_eq!(
        v["stdout"], "",
        "stdout must be empty on compile error: {v}"
    );
    assert_eq!(
        v["stderr"], "",
        "stderr must be empty on compile error: {v}"
    );
    assert!(
        !v["diagnostics"].as_str().unwrap_or("").is_empty(),
        "diagnostics must be non-empty on compile_error: {v}"
    );
}

#[test]
fn eval_json_compile_error_contains_diagnostic_text() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "this_does_not_exist_at_all()"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    let diagnostics = v["diagnostics"].as_str().unwrap_or("");
    let parent_stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        parent_stderr.is_empty(),
        "compile diagnostics leaked to parent stderr instead of JSON: {parent_stderr:?}"
    );
    assert!(
        !diagnostics.contains("\u{1b}["),
        "compile diagnostics in JSON must not contain ANSI escapes: {diagnostics:?}"
    );
    // The diagnostic must mention the unknown name so tooling can surface it.
    assert!(
        diagnostics.contains("this_does_not_exist_at_all")
            || diagnostics.contains("error")
            || diagnostics.contains("not found"),
        "diagnostics text appears empty or unhelpful: {diagnostics:?}"
    );
}

#[test]
fn eval_json_manifest_message_diagnostic_stays_in_json() {
    let dir = support::tempdir();
    std::fs::write(dir.path().join("hew.toml"), "[package]\nname = \"myapp\"\n").unwrap();

    let path = dir.path().join("manifest_error.hew");
    std::fs::write(&path, "import math;\n\n1 + 2\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected exit 0 with --json on manifest diagnostic, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let parent_stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        parent_stderr.is_empty(),
        "message diagnostic leaked to parent stderr: {parent_stderr:?}"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "compile_error", "unexpected status: {v}");
    let diagnostics = v["diagnostics"].as_str().unwrap_or("");
    assert!(
        diagnostics.contains("module `math` is not declared in hew.toml"),
        "expected manifest diagnostic in JSON payload: {diagnostics:?}"
    );
    assert!(
        diagnostics.contains("adze add math"),
        "expected manifest hint in JSON payload: {diagnostics:?}"
    );
}

#[test]
fn eval_json_requires_non_interactive() {
    // --json without -f and without an expression (interactive mode) is rejected.
    let output = Command::new(hew_binary())
        .args(["eval", "--json"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "expected failure for --json in interactive mode"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("--json"),
        "expected --json in error message: {stderr}"
    );
}

#[test]
fn eval_json_file_ok() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("json_ok.hew");
    std::fs::write(&path, "42\n").unwrap();

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(output.status.success(), "expected exit 0 with --json");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nstdout: {stdout}"));

    assert_eq!(v["status"], "ok", "unexpected status: {v}");
    assert_eq!(v["stdout"], "42\n", "unexpected stdout: {v}");
}

// ── Cross-chunk output-preservation regression ────────────────────────────────
//
// When a file contains multiple top-level chunks (separated by a complete
// statement boundary) and a later chunk fails at runtime, the stdout produced
// by earlier successful chunks must not be dropped.
//
// This is a regression guard for the buffered-accumulation path introduced in
// `eval_source_file_cli`: the `?` short-circuit on RuntimeFailure used to
// silently drop `collected` from prior chunks.  The fix prepends `collected`
// into the RuntimeFailure stdout before propagating.
//
// File shape that actually exercises the prepend path:
//   chunk 1 (bare expression): print("prior-chunk\n")   ← emits stdout, succeeds
//   chunk 2 (bare expression): panic("boom")            ← fails, exit 101
//
// Chunk 1 runs in its own compiled binary and writes to stdout.  That output
// is captured into `collected`.  When chunk 2's binary panics, the fix prepends
// `collected` into the RuntimeFailure.stdout before re-raising, so the caller
// (non-JSON print path, JSON stdout field) sees "prior-chunk\n".
//
// Without the fix the `?` would propagate RuntimeFailure { stdout: "", … }
// and "prior-chunk\n" would be silently dropped — the tests would FAIL.
//
// `:load` coverage: `load_file` delegates directly to `eval_source_file_cli`
// and forwards the RuntimeFailure stdout field unchanged.  The file-eval tests
// below are a sufficient proxy; no separate interactive-REPL harness is needed.

/// Write a two-chunk .hew file:
///   chunk 1 — bare `print("prior-chunk\n")` (complete expression, emits stdout)
///   chunk 2 — bare `panic("boom")`           (complete expression, exits 101)
///
/// The blank line between them ensures the chunk-splitter in
/// `eval_source_file_cli` finishes chunk 1 before starting chunk 2.
fn write_cross_chunk_failure_file(dir: &std::path::Path) -> std::path::PathBuf {
    let path = dir.join("cross_chunk_failure.hew");
    std::fs::write(&path, "print(\"prior-chunk\\n\")\n\npanic(\"boom\")\n").unwrap();
    path
}

/// Return a multi-line diagnostic string for a cross-chunk failure assertion.
///
/// Embeds exit code (or "signal" when the process was killed by a signal),
/// full stderr, full stdout, and the input file path so that a failing
/// assertion message is immediately classifiable — e.g. tempdir setup
/// error, compile diagnostic, linker failure, or a real exit-code
/// regression — without needing to re-run or re-trigger the job.
fn cross_chunk_failure_ctx(output: &Output, path: &Path) -> String {
    let code = match output.status.code() {
        Some(c) => format!("{c}"),
        None => "signal".to_owned(),
    };
    format!(
        "\n  file:      {}\n  exit_code: {code}\n  stderr:    {:?}\n  stdout:    {:?}",
        path.display(),
        String::from_utf8_lossy(&output.stderr),
        String::from_utf8_lossy(&output.stdout),
    )
}

#[test]
fn eval_file_cross_chunk_failure_preserves_prior_chunk_stdout() {
    require_codegen();

    let dir = support::tempdir();
    let path = write_cross_chunk_failure_file(dir.path());

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("-f")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    let ctx = cross_chunk_failure_ctx(&output, &path);
    assert!(
        !output.status.success(),
        "expected non-zero exit on runtime failure{ctx}"
    );
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101{ctx}"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("prior-chunk"),
        "stdout from earlier chunk was dropped{ctx}"
    );
}

#[test]
fn eval_json_file_cross_chunk_failure_preserves_prior_chunk_stdout() {
    require_codegen();

    let dir = support::tempdir();
    let path = write_cross_chunk_failure_file(dir.path());

    let output = Command::new(hew_binary())
        .args(["eval", "--json", "-f"])
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    let ctx = cross_chunk_failure_ctx(&output, &path);
    assert!(output.status.success(), "expected exit 0 with --json{ctx}");

    let raw = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&raw)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nraw: {raw}\n{ctx}"));

    assert_eq!(
        v["status"], "runtime_failure",
        "unexpected status: {v}{ctx}"
    );
    assert_eq!(
        v["exit_code"], 101,
        "expected child exit code 101: {v}{ctx}"
    );
    let captured = v["stdout"].as_str().unwrap_or("");
    assert!(
        captured.contains("prior-chunk"),
        "stdout from earlier chunk was absent in JSON contract: {v}{ctx}"
    );
}

/// Assert that per-submission output appears in the pipe *before* the process exits.
///
/// When `hew eval`'s stdout is piped, libc uses full block-buffering by
/// default.  Without an explicit `stdout().flush()` after each `print!`,
/// output is held in the buffer until exit — the reported symptom.
///
/// The test waits for the REPL banner before it starts the per-submission
/// flush clock, then sends one statement and requires the resulting output to
/// become observable in the parent before any process-exit boundary.  The
/// deadline is a bounded budget for that observation, not the invariant.
///
/// `--timeout 120` is passed to the child so the child's internal
/// compile+codegen+link budget matches the harness's load-invariant
/// expectation.  Without it the child defaults to 30 s, which on a
/// cold or heavily loaded machine can expire before the first statement
/// compiles — causing `hew eval` to self-abort at the FLUSH path, not
/// the STARTUP path, making the flake invisible to `STARTUP_BUDGET`.
#[test]
fn eval_repl_piped_stdout_flushes_per_submission() {
    const STARTUP_BUDGET: Duration = Duration::from_mins(1);
    const FLUSH_BUDGET: Duration = Duration::from_mins(1);

    require_codegen();

    let mut child = Command::new(hew_binary())
        .args(["eval", "--timeout", "120"])
        .current_dir(repo_root())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn hew eval");

    let mut stdin = child.stdin.take().expect("stdin piped");
    let stdout = child.stdout.take().expect("stdout piped");
    let stderr = child.stderr.take().expect("stderr piped");

    // Read lines from child stdout on a background thread, forwarding each
    // line to the channel so the main thread can time-bound the wait.
    let (tx, rx) = mpsc::channel::<String>();
    let reader_thread = std::thread::spawn(move || {
        let buf = std::io::BufReader::new(stdout);
        for line in buf.lines() {
            match line {
                Ok(l) => {
                    if tx.send(l).is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });
    let stderr_thread = std::thread::spawn(move || {
        let mut buf = String::new();
        let mut reader = std::io::BufReader::new(stderr);
        let _ = reader.read_to_string(&mut buf);
        buf
    });

    let startup_outcome = wait_for_line(&rx, "Hew REPL v", STARTUP_BUDGET);

    let mut wait_outcome = None;
    if matches!(startup_outcome, WaitOutcome::Found) {
        // Send one statement that produces output.
        stdin
            .write_all(b"println(\"flush-check\");\n")
            .expect("write to hew eval stdin");
        // Ensure the line reaches the child's stdin buffer.
        stdin.flush().expect("flush hew eval stdin");

        wait_outcome = Some(wait_for_line(&rx, "flush-check", FLUSH_BUDGET));
    }

    // Gracefully terminate the process regardless of outcome.
    let _ = stdin.write_all(b":quit\n");
    let _ = stdin.flush();
    drop(stdin);
    let _ = reader_thread.join();
    let child_status = child.wait().expect("wait on hew eval");
    let stderr_output = stderr_thread
        .join()
        .unwrap_or_else(|_| String::from("<stderr reader panicked>"));
    let stderr_suffix = if stderr_output.trim().is_empty() {
        String::new()
    } else {
        format!("\nstderr: {stderr_output}")
    };

    match startup_outcome {
        WaitOutcome::Found => {}
        WaitOutcome::Timeout => panic!(
            "REPL banner not observed within {STARTUP_BUDGET:?}; child did not reach readline \
             before the startup budget expired\nstatus: {child_status}{stderr_suffix}"
        ),
        WaitOutcome::ChildClosed => panic!(
            "child stdout closed before the REPL banner was observed; child likely exited or \
             crashed before reaching readline\nstatus: {child_status}{stderr_suffix}"
        ),
    }

    match wait_outcome.expect("wait outcome should be set after startup readiness") {
        WaitOutcome::Found => {}
        WaitOutcome::Timeout => panic!(
            "'flush-check' did not arrive in the pipe within {FLUSH_BUDGET:?} after the REPL \
             banner was observed, while the child process was still alive — per-submission \
             stdout flush is missing or stalled\nstatus: {child_status}{stderr_suffix}"
        ),
        WaitOutcome::ChildClosed => panic!(
            "child stdout closed before 'flush-check' was observed; child likely exited or \
             crashed — this is not a flush invariant failure, investigate the child process\n\
             status: {child_status}{stderr_suffix}"
        ),
    }
}

/// Clap-level smoke test: `--jit=worker` is a recognised flag and the
/// REPL exits successfully when it is supplied.
///
/// NOTE: This test does NOT verify that the flag is wired through to
/// `ReplSession::set_jit_mode` — both the pre-fix and post-fix code paths
/// exit 0 for this input.  The wiring invariant is covered by the unit test
/// `eval::repl::tests::set_jit_mode_stores_mode_on_session` in `repl.rs`.
///
/// `--jit=worker` (AOT+spawn) is used here rather than `--jit=inprocess` or
/// `--jit=auto` because both of those route to `run_inprocess_jit`, which
/// SIGSEGVs on Linux (#1523).  `--jit=worker` exercises the same clap
/// flag-routing path without triggering the in-process JIT crash.
#[test]
fn eval_repl_jit_worker_flag_accepted_by_clap() {
    require_codegen();

    let output = run_eval_with_stdin(&["eval", "--jit=worker"], "1 + 1\n:quit\n");

    assert!(
        output.status.success(),
        "hew eval --jit=worker exited non-zero\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

/// End-to-end: a user function that returns `Result<i64, string>` constructs
/// `Ok` / `Err`, and a match-statement on the result binds each variant's
/// payload and dispatches per-arm bodies. Guards against the substrate gap
/// that produced `UnresolvedSymbol("Ok")` at HIR resolution and `match arm
/// variant not registered in machine/enum ctor registry` for the arms.
///
/// The fix registered built-in `Option<T>` / `Result<T, E>` in HIR's
/// `machine_ctor_registry` + `enum_variants_by_name` pre-pass, and added a
/// `Stmt::Match` lowering arm so statement-position `match` reaches the
/// match builder. We run the program end-to-end (not just inspect IR) and
/// assert the byte-level stdout so codegen runs the right branch per match.
///
/// NOTE: This test originally used `Result<i64, string>` to exercise both
/// Ok/Err arms. After the composite-return fail-closed boundary landed
/// (W3.028), `Result<i64, string>` is rejected because `string` is
/// heap-owning. The test now uses `Result<i64, i64>` and exercises the same
/// ctor + match dispatch logic without hitting the boundary.
#[test]
fn run_result_ok_err_ctors_and_match_arms_dispatch_per_variant() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("result_match.hew");
    std::fs::write(
        &hew_src,
        "fn parse_positive(n: i64) -> Result<i64, i64> {\n\
         \x20   if n == 42 {\n\
         \x20       Ok(42)\n\
         \x20   } else {\n\
         \x20       Err(0)\n\
         \x20   }\n\
         }\n\
         \n\
         fn main() {\n\
         \x20   let r1 = parse_positive(42);\n\
         \x20   let r2 = parse_positive(7);\n\
         \x20   match r1 {\n\
         \x20       Ok(n) => println(n),\n\
         \x20       Err(e) => println(e),\n\
         \x20   }\n\
         \x20   match r2 {\n\
         \x20       Ok(n) => println(n),\n\
         \x20       Err(e) => println(e),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "42\n0\n",);
}

/// Negative probe: matching an `Option<T>` pattern (`Some` / `None`) against
/// a `Result<T, E>` scrutinee must be rejected at type-check time, not
/// silently miscompiled. Guards against the failure mode where the HIR
/// match-arm lowering would have happily accepted any registered variant
/// name regardless of the scrutinee's enum type.
///
/// Asserts a non-zero exit code with a stderr message that mentions the
/// offending variant and the scrutinee enum. The check is by substring to
/// avoid coupling to the exact diagnostic phrasing.
#[test]
fn match_wrong_variant_against_result_scrutinee_is_compile_time_type_error() {
    let dir = support::tempdir();
    let hew_src = dir.path().join("result_wrong_variant.hew");
    std::fs::write(
        &hew_src,
        "fn main() {\n\
         \x20   let r: Result<i64, string> = Ok(1);\n\
         \x20   match r {\n\
         \x20       Some(x) => println(x),\n\
         \x20       None => println(0),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "hew run should fail on wrong-variant match; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("Some") && stderr.contains("Result"),
        "stderr should name the offending variant `Some` and the scrutinee enum `Result`; got: {stderr}"
    );
}

/// Positive probe: a `.len()` / `.starts_with(...)` / `.to_uppercase()` chain
/// on a string runs end-to-end via the `impl string` block in
/// std/builtins.hew rather than the legacy per-symbol FFI dispatch table
/// in `check_string_method`.  Asserts byte-level stdout so codegen actually
/// executes the runtime FFI symbols selected by method dispatch.
#[test]
fn run_string_methods_dispatch_through_impl_block() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("string_impl_methods.hew");
    std::fs::write(
        &hew_src,
        "fn main() {\n\
         \x20   let s: string = \"hello world\";\n\
         \x20   println(s.len());\n\
         \x20   if s.starts_with(\"hello\") {\n\
         \x20       println(\"matched\");\n\
         \x20   }\n\
         \x20   let upper = s.to_uppercase();\n\
         \x20   println(upper);\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "11\nmatched\nHELLO WORLD\n",
    );
}

/// Negative probe: calling a nonexistent string method must error at check
/// time with an undefined-method diagnostic, not silently miscompile or
/// fail at runtime.  Guards against the impl-block raise accidentally
/// admitting any identifier as a method on `Ty::String`.
#[test]
fn unknown_string_method_errors_at_check_time() {
    let dir = support::tempdir();
    let hew_src = dir.path().join("string_unknown_method.hew");
    std::fs::write(
        &hew_src,
        "fn main() {\n\
         \x20   let s: string = \"hello\";\n\
         \x20   let _ = s.no_such_method();\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "hew run should fail on unknown string method; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("no_such_method"),
        "stderr should name the missing method `no_such_method`; got: {stderr}"
    );
}

/// Slice ε probe 1 — generic machine with a `<T: Display>` where-clause runs
/// end-to-end.  The machine `Tagger<T: Display>` has states `Empty` and
/// `Tagged { value: T }` plus a `Tag { value: T }` event.  The main function
/// drives a `Tagger<i64>` from `Empty` → `Tagged` via one `step()` call and
/// then prints the current state name.  Expected output: `Tagged`.
#[test]
fn trait_bound_probe1_bounded_machine_runs() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("tagger.hew");
    std::fs::write(
        &hew_src,
        "machine Tagger<T: Display> {\n\
         \x20   events {\n\
         \x20       Tag { value: T; }\n\
         \x20   }\n\
         \x20   state Empty;\n\
         \x20   state Tagged { value: T; }\n\
         \x20   on Tag: Empty => Tagged { Tagged { value: event.value } }\n\
         \x20   on Tag: Tagged => Tagged reenter { Tagged { value: event.value } }\n\
         }\n\
         fn main() {\n\
         \x20   var t: Tagger<i64> = Empty;\n\
         \x20   t.step(Tag { value: 42 });\n\
         \x20   println(t.state_name());\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed for bounded machine; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "Tagged\n",
        "state_name() should return \"Tagged\" after stepping through Tag event",
    );
}

/// Slice ε probe 2 — machine with two independent bounded type parameters
/// `<A: Display, B: Display>` runs end-to-end.  Instantiated as
/// `Pair<i64, string>`, this exercises the multi-type-param generics surface
/// on machines: state fields, event fields, and state-name dispatch all work
/// across different concrete monomorphisations.  Uses only machine dispatch
/// (`.step()` / `.state_name()`) so the probe is not blocked by the L1
/// generic monomorphisation gap in MIR codegen.
#[test]
fn trait_bound_probe2_multi_bound_machine_runs() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("pair_machine.hew");
    std::fs::write(
        &hew_src,
        "machine Pair<A: Display, B: Display> {\n\
         \x20   events {\n\
         \x20       Load { first: A; second: B; }\n\
         \x20       Clear;\n\
         \x20   }\n\
         \x20   state Empty;\n\
         \x20   state Full { first: A; second: B; }\n\
         \x20   on Load: Empty => Full { Full { first: event.first, second: event.second } }\n\
         \x20   on Load: Full => Full reenter { Full { first: event.first, second: event.second } }\n\
         \x20   on Clear: Empty => Empty reenter { Empty }\n\
         \x20   on Clear: Full => Empty { Empty }\n\
         }\n\
         fn main() {\n\
         \x20   var p: Pair<i64, i64> = Empty;\n\
         \x20   p.step(Load { first: 42, second: 99 });\n\
         \x20   println(p.state_name());\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed for multi-param bounded machine; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "Full\n",
        "state_name() should return \"Full\" after loading both fields",
    );
}

/// Slice ε probe 3 — `impl<T> Trait for UserType<T> where T: Bound` method
/// dispatched by dot-syntax on a concrete generic receiver runs end-to-end.
/// `p.next()` on a `Pair<i64>` resolves through the inherent-method table to
/// the qualified `Pair::next` symbol, registers the `Pair::next$$i64`
/// monomorphisation, and dispatches to a concrete MIR body. The `where T:
/// Display` predicate on the impl's own type parameter is admitted; the
/// `type Item = T` associated-type binding is checker-only metadata and does
/// not block this projection-free method (the method's return type is the
/// concrete `Option<T>`, not a `Self::Item` projection).
#[test]
fn trait_bound_probe3_where_clause_impl_dispatch_runs() {
    require_codegen();

    let dir = support::tempdir();
    let hew_src = dir.path().join("pair_iter.hew");
    std::fs::write(
        &hew_src,
        "pub type Pair<T> { left: T; right: T; }\n\
         impl<T> Iterator for Pair<T> where T: Display {\n\
         \x20   type Item = T;\n\
         \x20   fn next(p: Pair<T>) -> Option<T> {\n\
         \x20       Some(p.left)\n\
         \x20   }\n\
         }\n\
         fn main() {\n\
         \x20   let p = Pair { left: 77, right: 88 };\n\
         \x20   match p.next() {\n\
         \x20       Some(x) => println(x),\n\
         \x20       None => println(-1),\n\
         \x20   }\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed for where-clause impl dispatch; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "77\n",
        "Pair::next under where T: Display must return Some(left) and print 77",
    );
}

// ---------------------------------------------------------------------------
// W4.047 P1.3: end-to-end totality probes for the high-risk Unit-coercion
// seams that the HIR shadow asserts instrument (actor-ask reply, dyn-method
// return, static trait-method dispatch). Each program returns a *concrete
// non-Unit* type across the seam, so in a debug build (debug_assertions on)
// the shadow asserts execute against real check_program output. A pass proves
// both halves of the Phase-1 contract: (a) the typed handoff is total at these
// seams (no assert fires) and (b) behaviour is unchanged (correct output).
// ---------------------------------------------------------------------------

fn run_hew_source(file_name: &str, src: &str) -> Output {
    let dir = support::tempdir();
    let hew_src = dir.path().join(file_name);
    std::fs::write(&hew_src, src).unwrap();
    Command::new(hew_binary())
        .arg("run")
        .arg(&hew_src)
        .current_dir(repo_root())
        .output()
        .unwrap()
}

/// Actor-ask reply seam: a `select { reply from w.twice(..) => reply }` ask
/// returns a concrete `i64` reply, surfaced as the process exit code. This is
/// the codegen-supported ask form (cf. `examples/v05/actor_ask_race.hew`); the
/// ask-reply totality assert runs during lowering of the reply binding.
#[test]
fn w4_047_actor_ask_reply_concrete_type_totality() {
    require_codegen();

    let output = run_hew_source(
        "ask_reply.hew",
        "actor Doubler {\n\
         \x20   receive fn twice(n: i64) -> i64 { n * 2 }\n\
         }\n\
         fn main() -> i64 {\n\
         \x20   let w = spawn Doubler;\n\
         \x20   let r = select {\n\
         \x20       reply from w.twice(21) => reply,\n\
         \x20       after 1000ms => 0,\n\
         \x20   };\n\
         \x20   r\n\
         }\n",
    );

    // 21*2 = 42, returned as the process exit code.
    assert_eq!(
        output.status.code(),
        Some(42),
        "actor-ask reply probe should exit 42; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
}

#[test]
fn w4_047_static_trait_dispatch_concrete_return_totality() {
    require_codegen();

    let output = run_hew_source(
        "static_valued.hew",
        "trait Valued {\n\
         \x20   fn value(val: Self) -> i64;\n\
         }\n\
         type Token { id: i64; }\n\
         impl Valued for Token {\n\
         \x20   fn value(t: Token) -> i64 { t.id }\n\
         }\n\
         fn show<T: Valued>(item: T) -> i64 {\n\
         \x20   item.value()\n\
         }\n\
         fn main() {\n\
         \x20   let t = Token { id: 42 };\n\
         \x20   println(f\"{show(t)}\");\n\
         }\n",
    );

    assert!(
        output.status.success(),
        "static trait-dispatch probe should run; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(strip_ansi(&String::from_utf8_lossy(&output.stdout)), "42\n");
}
