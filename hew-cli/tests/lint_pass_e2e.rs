//! End-to-end coverage for the compiler lint pass surfaced through `hew check`:
//! lints render as warnings by default, the `--allow` / `--warn` / `--deny`
//! flags re-level them, an in-source `// hew:allow(...)` directive suppresses
//! them, and an unknown lint name fails closed at the CLI boundary. Covers the
//! `needless_range_loop` and `len_zero_comparison` checker lints plus the
//! `dead_code` warning now routed through the same registry.

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

// ── checker-stage lint: len_zero_comparison ──────────────────────────

/// A program whose only diagnostic is `len_zero_comparison`: `xs.len() == 0`
/// is exactly `xs.is_empty()`.
const LEN_ZERO: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     let _ = xs.len() == 0;\n\
     }\n";

/// The same program with an in-source allow directive on the line above.
const LEN_ZERO_SUPPRESSED: &str = "fn main() {\n\
     let xs: Vec<i64> = Vec::new();\n\
     // hew:allow(len_zero_comparison)\n\
     let _ = xs.len() == 0;\n\
     }\n";

const LEN_ZERO_MESSAGE: &str = "is exactly `is_empty()`";

#[test]
fn len_zero_comparison_renders_by_default() {
    let output = run_check(LEN_ZERO, &[]);
    let stderr = stderr_of(&output);
    assert!(
        output.status.success(),
        "a lint warning must not fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:") && stderr.contains(LEN_ZERO_MESSAGE),
        "expected the len_zero_comparison warning to render:\n{stderr}"
    );
}

#[test]
fn len_zero_comparison_deny_promotes_to_error() {
    let output = run_check(LEN_ZERO, &["-D", "len_zero_comparison"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(LEN_ZERO_MESSAGE),
        "-D must render the lint as an error:\n{stderr}"
    );
}

#[test]
fn len_zero_comparison_inline_directive_suppresses() {
    let output = run_check(LEN_ZERO_SUPPRESSED, &[]);
    let stderr = stderr_of(&output);
    assert!(output.status.success(), "check should pass:\n{stderr}");
    assert!(
        !stderr.contains(LEN_ZERO_MESSAGE),
        "an in-source `// hew:allow(...)` directive must suppress the lint:\n{stderr}"
    );
}

// ── migrated warning: dead_code is now registry-controlled ───────────

/// `helper` is never called, so the migrated `dead_code` lint flags it.
const DEAD_CODE: &str = "fn helper() {}\nfn main() {}\n";

const DEAD_CODE_MESSAGE: &str = "function `helper` is never called";

#[test]
fn dead_code_warns_by_default_but_is_allowable() {
    // The migration preserves the default warning while making it suppressible
    // through the same `--allow` path as every other registry lint.
    let default = run_check(DEAD_CODE, &[]);
    let default_stderr = stderr_of(&default);
    assert!(
        default_stderr.contains("warning:") && default_stderr.contains(DEAD_CODE_MESSAGE),
        "dead_code must still warn by default:\n{default_stderr}"
    );

    let allowed = run_check(DEAD_CODE, &["--allow", "dead_code"]);
    let allowed_stderr = stderr_of(&allowed);
    assert!(
        !allowed_stderr.contains(DEAD_CODE_MESSAGE),
        "--allow dead_code must suppress the migrated warning:\n{allowed_stderr}"
    );
}

#[test]
fn dead_code_deny_promotes_to_error() {
    let output = run_check(DEAD_CODE, &["-D", "dead_code"]);
    let stderr = stderr_of(&output);
    assert!(
        !output.status.success(),
        "-D dead_code must fail the build:\n{stderr}"
    );
    assert!(
        stderr.contains("error:") && stderr.contains(DEAD_CODE_MESSAGE),
        "-D must render the migrated lint as an error:\n{stderr}"
    );
}

/// A `re"..."` literal triggers implicit injection of `import std::text::regex`,
/// whose body compares `values.len() == 0` internally. The lint sweep runs over
/// user-authored bodies only, so that standard-library comparison must NOT
/// surface as a `len_zero_comparison` warning against the user's program. (The
/// regex literal itself is not yet lowered past the frontend, so `hew check`
/// still exits non-zero on the `E_NOT_YET_IMPLEMENTED` MIR gate — that error is
/// the proof the module was pulled in; the lint leak is what we guard against.)
const REGEX_LITERAL: &str = "fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n";

#[test]
fn stdlib_lints_do_not_leak_through_implicit_import() {
    let output = run_check(REGEX_LITERAL, &[]);
    let stderr = stderr_of(&output);
    // Sanity: the implicit `std::text::regex` import was actually resolved and
    // compiled (otherwise this test would pass vacuously). The regex literal is
    // not yet lowered, so its MIR gate error is the witness.
    assert!(
        stderr.contains("RegexLiteralRef") || stderr.contains("regex"),
        "expected the implicit std::text::regex import to be exercised:\n{stderr}"
    );
    // The actual guard: no lint finding from the library's own source.
    assert!(
        !stderr.contains(LEN_ZERO_MESSAGE),
        "a len_zero_comparison finding must not leak from stdlib bodies:\n{stderr}"
    );
}
