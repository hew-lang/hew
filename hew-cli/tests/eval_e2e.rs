mod support;

use std::io::Write;
use std::path::Path;
use std::process::{Command, Output, Stdio};

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

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("5")
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

    let output = Command::new(hew_binary())
        .arg("eval")
        .arg("--timeout")
        .arg("5")
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
    let output = run_eval_with_stdin(&["eval"], "1 + *\n:quit\n");

    assert!(
        output.status.success(),
        "stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(stderr.contains("<repl>:1:"), "stderr: {stderr}");
    assert!(stderr.contains("error:"), "stderr: {stderr}");
    assert!(stderr.contains("expected expression"), "stderr: {stderr}");
    assert!(stderr.contains("1 | 1 + *"), "stderr: {stderr}");
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
        stderr.contains("type mismatch: expected `int`, found `String`"),
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
        stderr.contains("cannot apply `+` to `int` and `String`"),
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
        stderr.contains("type mismatch: expected `int`, found `String`"),
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
        stderr.contains("cannot apply `+` to `int` and `String`"),
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
        stderr.contains("type mismatch: expected `int`, found `String`"),
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
        stderr.contains("cannot apply `+` to `int` and `String`"),
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
    let dep_header = format!("{}:1:", dep_path.display());
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
        stderr.contains("type mismatch: expected `int`, found `String`"),
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

/// A WASM eval that runs longer than the timeout exits with a timeout error.
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
        "fn spin_forever() {\n    var i: i64 = 0;\n    loop { i = i + 1; }\n}\nspin_forever()\n",
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
    support::require_wasi_runner();

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

/// `hew eval --target wasm32-wasi -f -` must reject `for await item in rx`
/// over a channel receiver during the fast typecheck pass, before codegen can
/// lower it to the blocking runtime recv that traps on wasm32.
#[test]
fn eval_wasm_fast_typecheck_rejects_for_await_receiver() {
    let output = run_eval_with_stdin(
        &["eval", "--target", "wasm32-wasi", "-f", "-"],
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
    );

    assert!(
        !output.status.success(),
        "expected failure for `for await` over Receiver<T> on WASM target"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Blocking channel receive"),
        "expected blocking channel receive diagnostic from fast typecheck, stderr: {stderr}"
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

#[test]
fn eval_wasm_file_runtime_failure_exits_with_child_exit_code() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_failing_eval.hew");
    std::fs::write(&path, "panic(\"deliberate failure\")\n").unwrap();

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

#[test]
fn eval_wasm_file_runtime_failure_preserves_pre_failure_stdout() {
    require_codegen();
    support::require_wasi_runner();

    let dir = support::tempdir();
    let path = dir.path().join("wasm_partial_output_eval.hew");
    std::fs::write(
        &path,
        "fn do_and_fail() {\n    print(\"wasm-partial\\n\");\n    panic(\"deliberate failure\");\n}\ndo_and_fail()\n",
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
            r#"panic("deliberate wasm failure")"#,
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

    assert!(
        !output.status.success(),
        "expected non-zero exit on runtime failure"
    );
    assert_eq!(
        output.status.code(),
        Some(101),
        "expected child exit code 101"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("prior-chunk"),
        "stdout from earlier chunk was dropped; got: {stdout:?}"
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

    assert!(output.status.success(), "expected exit 0 with --json");

    let raw = String::from_utf8_lossy(&output.stdout);
    let v: serde_json::Value = serde_json::from_str(&raw)
        .unwrap_or_else(|e| panic!("stdout is not valid JSON: {e}\nraw: {raw}"));

    assert_eq!(v["status"], "runtime_failure", "unexpected status: {v}");
    assert_eq!(v["exit_code"], 101, "expected child exit code 101: {v}");
    let captured = v["stdout"].as_str().unwrap_or("");
    assert!(
        captured.contains("prior-chunk"),
        "stdout from earlier chunk was absent in JSON contract: {v}"
    );
}
