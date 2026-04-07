mod support;

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::sync::OnceLock;

use support::strip_ansi;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn require_codegen() -> bool {
    static BUILD_OK: OnceLock<bool> = OnceLock::new();
    *BUILD_OK.get_or_init(|| {
        Command::new("make")
            .args(["runtime", "stdlib"])
            .current_dir(repo_root())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
    if !require_codegen() {
        return;
    }

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
