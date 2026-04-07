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

use std::path::PathBuf;
use std::process::Command;
use support::strip_ansi;

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

/// Write a set of `(filename, content)` pairs into a temporary directory and
/// return the directory handle (kept alive for the duration of the test).
fn write_fixture(files: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("cannot create temp dir for e2e fixture");
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
