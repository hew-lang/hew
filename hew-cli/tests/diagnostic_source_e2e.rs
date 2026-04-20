//! End-to-end tests for diagnostic source routing.
//!
//! These tests run the `hew check` subcommand (which does not require the
//! native codegen infrastructure) and assert that type errors originating in
//! non-root module bodies name the correct source file in their rendered
//! output rather than the root compilation unit.
//!
//! The tests create temporary multi-file fixtures on disk, invoke the `hew`
//! binary, and inspect the captured stderr.

mod support;

use std::process::Command;
use support::{hew_binary, strip_ansi};

/// Write a set of `(filename, content)` pairs into a temporary directory and
/// return the directory handle (kept alive for the duration of the test).
fn write_fixture(files: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = support::tempdir();
    for (name, content) in files {
        std::fs::write(dir.path().join(name), content).expect("cannot write fixture file");
    }
    dir
}

// ── rendered-output tests ────────────────────────────────────────────────────

/// A type error in `dep.hew` must be rendered with `dep.hew` as the filename
/// header (not the root `main.hew`), and the underline must point at source
/// text from `dep.hew`.
///
/// Regression guard for the full CLI rendering path:
/// checker → `source_module` tag → `typecheck_program` routing →
/// `render_diagnostic_with_raw_notes` → stderr.
#[test]
fn non_root_module_error_rendered_with_correct_filename() {
    let fixture = write_fixture(&[
        ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
        // dep.hew: the return type says i64 but the body returns `true` (bool).
        // The underline should point at `true` inside dep.hew.
        ("dep.hew", "pub fn mistyped() -> i64 { true }\n"),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must exit non-zero when dep.hew has a type error"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The error header must name dep.hew, not main.hew.
    assert!(
        stderr.contains("dep.hew:"),
        "stderr must contain 'dep.hew:' but got:\n{stderr}"
    );
    assert!(
        !stderr.contains("main.hew:"),
        "stderr must NOT contain 'main.hew:' for a dep-only error; got:\n{stderr}"
    );

    // The underlined source text must come from dep.hew (contains `true`).
    assert!(
        stderr.contains("true"),
        "rendered source underline must show dep.hew content ('true'); got:\n{stderr}"
    );
}

/// A duplicate function definition in `dep.hew` (registration-phase error)
/// must name `dep.hew` in the rendered output for the dep-level error.
///
/// Note: importing a duplicate may also produce a secondary error at the
/// import site in `main.hew`; the key invariant is that the *first*
/// duplicate-definition error (the one about `dep.dup`) is attributed to
/// `dep.hew`, not `main.hew`.
///
/// Regression guard for `collect_functions` → slice-tag → CLI rendering path.
#[test]
fn registration_phase_error_rendered_with_correct_filename() {
    let fixture = write_fixture(&[
        ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
        // dep.hew has two identical function names — duplicate_definition emitted
        // during collect_functions (registration phase, before body check).
        (
            "dep.hew",
            "pub fn dup() -> i64 { 1 }\npub fn dup() -> i64 { 2 }\n",
        ),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must exit non-zero when dep.hew has a duplicate definition"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The dep-level duplicate error must be attributed to dep.hew.
    // (A secondary import-collision error may legitimately mention main.hew,
    // but the primary `dep.dup` error must point at dep.hew.)
    assert!(
        stderr.contains("dep.hew:"),
        "registration-phase stderr must contain 'dep.hew:' but got:\n{stderr}"
    );

    // Verify the dep.hew line specifically mentions the dup function name.
    let has_dep_dup_error = stderr
        .lines()
        .any(|line| line.contains("dep.hew:") && (line.contains("dup") || line.contains("error:")));
    assert!(
        has_dep_dup_error,
        "a 'dep.hew:' error line about the duplicate must appear; got:\n{stderr}"
    );
}

// ── warning source-routing e2e tests ────────────────────────────────────────

/// An unused-variable warning in `dep.hew` must be rendered with `dep.hew` as
/// the filename header, not `main.hew`.
///
/// The warning is attributed via the `source_module` tag set when `dep.hew` is
/// body-checked: `emit_scope_warnings` copies `self.current_module` into each
/// warning, then `typecheck_program` routes the warning to the dep source file
/// using `type_diagnostic_to_frontend`.
///
/// This test guards the full CLI warning rendering path end-to-end.
#[test]
fn non_root_unused_variable_warning_rendered_with_dep_filename() {
    let fixture = write_fixture(&[
        ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
        // dep.hew: `x` is bound but never used — triggers UnusedVariable warning.
        ("dep.hew", "pub fn helper() -> i64 { let x = 42; 100 }\n"),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    // Warnings do not cause a non-zero exit code — hew check exits 0.
    assert!(
        output.status.success(),
        "hew check must exit zero when dep.hew has only a warning; got stderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The warning header must name dep.hew, not main.hew.
    assert!(
        stderr.contains("dep.hew:"),
        "unused-variable warning stderr must contain 'dep.hew:' but got:\n{stderr}"
    );
    // Only check that no *diagnostic* line is attributed to main.hew.
    // The success line "{path}/main.hew: OK" is expected and must not be matched.
    let main_is_diagnostic = stderr.lines().any(|line| {
        line.contains("main.hew:") && (line.contains("warning:") || line.contains("error:"))
    });
    assert!(
        !main_is_diagnostic,
        "unused-variable warning must NOT be attributed to main.hew as a diagnostic; got:\n{stderr}"
    );

    // The rendered output must mention the variable name or "warning".
    let mentions_warning = stderr.contains("warning") || stderr.contains("unused");
    assert!(
        mentions_warning,
        "stderr must mention the warning; got:\n{stderr}"
    );
}

/// `hew check --Werror` must fail on the same dep warning that plain
/// `hew check` reports non-fatally.
#[test]
fn non_root_unused_variable_warning_becomes_error_with_werror() {
    let fixture = write_fixture(&[
        ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
        ("dep.hew", "pub fn helper() -> i64 { let x = 42; 100 }\n"),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", "--Werror", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check --Werror must exit non-zero when dep.hew has only a warning; got stderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("dep.hew:"),
        "--Werror warning output must still be attributed to dep.hew; got:\n{stderr}"
    );
    assert!(
        stderr.contains("warning:"),
        "--Werror should still render the original warning; got:\n{stderr}"
    );
    assert!(
        stderr.contains("warnings treated as errors"),
        "--Werror failure should explain why the command failed; got:\n{stderr}"
    );
    assert!(
        !stderr.contains(": OK"),
        "--Werror failure must not print the success footer; got:\n{stderr}"
    );
}

/// An unreachable-code warning in `dep.hew` must be rendered with `dep.hew`
/// as the filename header.
///
/// `dep.hew` contains code after a `return` statement; the body checker
/// emits `UnreachableCode` with `source_module` set to the dep module name.
#[test]
fn non_root_unreachable_code_warning_rendered_with_dep_filename() {
    let fixture = write_fixture(&[
        ("main.hew", "import \"dep.hew\";\n\nfn main() {}\n"),
        // dep.hew: literal after `return` is unreachable — triggers UnreachableCode.
        ("dep.hew", "pub fn compute() -> i64 { return 42; 0 }\n"),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    // Warnings produce a zero exit code.
    assert!(
        output.status.success(),
        "hew check must exit zero for unreachable-code warning only; got stderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // Warning must name dep.hew.
    assert!(
        stderr.contains("dep.hew:"),
        "unreachable-code warning must mention 'dep.hew:' but got:\n{stderr}"
    );
    // Only check that no *diagnostic* line is attributed to main.hew.
    let main_is_diagnostic = stderr.lines().any(|line| {
        line.contains("main.hew:") && (line.contains("warning:") || line.contains("error:"))
    });
    assert!(
        !main_is_diagnostic,
        "unreachable-code warning must NOT be attributed to main.hew as a diagnostic; got:\n{stderr}"
    );

    let mentions_warning = stderr.contains("warning") || stderr.contains("unreachable");
    assert!(
        mentions_warning,
        "stderr must mention the warning; got:\n{stderr}"
    );
}

/// Mixed case: `dep.hew` produces a warning; `main.hew` is otherwise clean.
///
/// Invariants:
/// - `hew check` exits **successfully** (warnings are not fatal)
/// - The warning is attributed to `dep.hew`, not `main.hew`
/// - No error lines mention `main.hew` as the source of a diagnostic
#[test]
fn mixed_dep_warning_main_clean_exits_success_warning_attributed_to_dep() {
    let fixture = write_fixture(&[
        // main.hew calls dep_value() which is clean.
        (
            "main.hew",
            "import \"dep.hew\";\n\nfn main() {\n    let v = dep_value();\n    println(v);\n}\n",
        ),
        // dep.hew exports dep_value(); the internal helper has an unused variable.
        (
            "dep.hew",
            "pub fn dep_value() -> i64 { let ignored = 0; 42 }\n",
        ),
    ]);

    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    // Warnings must not cause a non-zero exit.
    assert!(
        output.status.success(),
        "hew check must exit zero (dep warning only, main is clean); got stderr:\n{}",
        strip_ansi(&String::from_utf8_lossy(&output.stderr))
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The warning must be attributed to dep.hew.
    assert!(
        stderr.contains("dep.hew:"),
        "warning must be attributed to dep.hew; got:\n{stderr}"
    );

    // No diagnostic must blame main.hew as the source of the warning.
    let main_hew_diag_line = stderr.lines().any(|line| {
        line.contains("main.hew:") && (line.contains("warning") || line.contains("error:"))
    });
    assert!(
        !main_hew_diag_line,
        "no warning/error line should point at main.hew; got:\n{stderr}"
    );
}
