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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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
    let dir = tempfile::tempdir().unwrap();
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
    let dir = tempfile::tempdir().unwrap();
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
    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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
    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

    let dir = tempfile::tempdir().unwrap();
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

/// `hew eval --target wasm32-wasi` without an expression starts the interactive
/// REPL with WASM mode. Sending EOF immediately should exit cleanly (exit 0).
#[test]
fn eval_wasm_interactive_mode_exits_on_eof() {
    // No codegen needed — we just send EOF immediately.
    let output = Command::new(hew_binary())
        .args(["eval", "--target", "wasm32-wasi"])
        .current_dir(repo_root())
        // Provide EOF on stdin so the REPL exits after printing the banner.
        .stdin(Stdio::null())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "expected REPL to exit 0 on EOF with --target, stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Hew REPL"),
        "expected REPL banner, stdout: {stdout}"
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
