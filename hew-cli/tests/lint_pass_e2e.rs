//! End-to-end coverage for the M1 compiler lint pass surfaced through
//! `hew check`: the `needless_range_loop` lint renders as a warning by default,
//! the `--allow` / `--warn` / `--deny` flags re-level it, an in-source
//! `// hew:allow(...)` directive suppresses it, and an unknown lint name fails
//! closed at the CLI boundary.

mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root, strip_ansi, tempdir};

/// A program whose only diagnostic is the `needless_range_loop` lint: the loop
/// indexes `xs` with `i` and does nothing else with `i`, so iterating the
/// collection directly is exactly equivalent.
const NEEDLESS: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     for i in 0..xs.len() {\n\
     let _ = xs[i];\n\
     }\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const NEEDLESS_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     // hew:allow(needless_range_loop)\n\
     for i in 0..xs.len() {\n\
     let _ = xs[i];\n\
     }\n\
     }\n";

const LINT_MESSAGE: &str = "the loop variable `i` is only used to index `xs`";

/// Run `hew check <fixture>` with `source` written to a temp file plus any
/// extra args (e.g. `--deny needless_range_loop`) inserted before the path.
fn run_check(source: &str, extra_args: &[&str]) -> Output {
    let dir = tempdir();
    let path = dir.path().join("prog.hew");
    std::fs::write(&path, source).unwrap();

    let mut command = Command::new(hew_binary());
    command.arg("check");
    command.args(extra_args);
    command.arg(&path);
    command
        .current_dir(repo_root())
        .output()
        .expect("failed to spawn hew check")
}

fn stderr_of(output: &Output) -> String {
    strip_ansi(&String::from_utf8_lossy(&output.stderr))
}

#[test]
fn lint_warning_renders_by_default() {
    let output = run_check(NEEDLESS, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LINT_MESSAGE),
        "expected the needless_range_loop warning to render:\n{stderr}"
    );
}

#[test]
fn allow_flag_suppresses_lint() {
    let output = run_check(NEEDLESS, &["--allow", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "--allow must suppress the lint:\n{stderr}"
    );
}

#[test]
fn allow_all_wildcard_suppresses_lint() {
    let output = run_check(NEEDLESS, &["-A", "all"]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "`-A all` must suppress every lint:\n{stderr}"
    );
}

#[test]
fn warn_flag_keeps_lint_as_warning() {
    let output = run_check(NEEDLESS, &["--warn", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a warning must not fail:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LINT_MESSAGE),
        "--warn must keep the lint at warning severity:\n{stderr}"
    );
}

#[test]
fn deny_flag_promotes_lint_to_error() {
    let output = run_check(NEEDLESS, &["--deny", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "--deny must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(LINT_MESSAGE),
        "--deny must render the lint as an error:\n{stderr}"
    );
}

#[test]
fn inline_directive_suppresses_lint() {
    let output = run_check(NEEDLESS_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

#[test]
fn inline_directive_overrides_deny() {
    // A local allow wins even over a command-line --deny: the lint is dropped,
    // so the build still succeeds.
    let output = run_check(NEEDLESS_SUPPRESSED, &["--deny", "needless_range_loop"]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "an in-source allow must override --deny:\n{stderr}"
    );
    assert!(
        !stderr.contains(LINT_MESSAGE),
        "the suppressed lint must not surface under --deny:\n{stderr}"
    );
}

#[test]
fn unknown_lint_name_is_rejected() {
    let output = run_check(NEEDLESS, &["--allow", "no_such_lint"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "an unknown lint name must fail closed:\n{stderr}"
    );
    assert!(
        stderr.contains("unknown lint `no_such_lint`"),
        "expected a clear unknown-lint error:\n{stderr}"
    );
}
