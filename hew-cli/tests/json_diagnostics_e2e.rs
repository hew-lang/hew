//! End-to-end tests for `--format=json` structured diagnostics.
//!
//! These exercise the CLI from a consumer's perspective: an agent runs
//! `hew check --format=json` and parses the JSON array of diagnostics, reads
//! the stable `code`/`severity`/`span`/`message`, and applies the
//! machine-checked `fixes`. They also guard the exit-code contract and the
//! Debug-payload leak fix.

mod support;

use std::fs;
use std::process::Command;

use serde_json::Value;
use support::{describe_output, hew_binary, strip_ansi};

fn write_fixture(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("main.hew");
    fs::write(&path, source).expect("write fixture");
    (dir, path)
}

fn run(args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .output()
        .expect("hew binary must run")
}

/// Parse stdout as a JSON array of diagnostics, panicking with context on
/// failure so a malformed payload surfaces the actual bytes.
fn parse_json_array(output: &std::process::Output) -> Vec<Value> {
    let stdout = String::from_utf8_lossy(&output.stdout);
    let value: Value = serde_json::from_str(&stdout).unwrap_or_else(|error| {
        panic!(
            "stdout must be a parseable JSON array; parse error: {error}\nstdout:\n{stdout}\n{}",
            describe_output(output)
        )
    });
    value
        .as_array()
        .unwrap_or_else(|| panic!("top-level JSON must be an array; got:\n{stdout}"))
        .clone()
}

/// A type error emits a parseable JSON array carrying at least
/// `{code, severity, file, span, message}` and exits 1.
#[test]
fn check_and_compile_type_errors_emit_structured_json_and_exit_1() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"not a number\";\n}\n");
    for verb in ["check", "compile"] {
        let output = run(&[verb, "--format=json", path.to_str().unwrap()]);

        assert_eq!(
            output.status.code(),
            Some(1),
            "type error must exit 1\n{}",
            describe_output(&output),
        );

        let diagnostics = parse_json_array(&output);
        let mismatch = diagnostics
            .iter()
            .find(|d| d["code"] == "Mismatch")
            .expect("expected a Mismatch diagnostic");

        assert_eq!(mismatch["severity"], "error");
        assert!(
            mismatch["file"].as_str().unwrap().ends_with("main.hew"),
            "file field must name the source: {mismatch}",
        );
        let span = &mismatch["span"];
        assert_eq!(span["start_line"], 2, "span carries 1-based line: {span}");
        assert!(span["start_byte"].is_number(), "span carries byte offsets");
        assert!(
            mismatch["message"]
                .as_str()
                .unwrap()
                .contains("type mismatch"),
            "message carries human-readable prose: {mismatch}",
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        for forbidden in [
            "MirDiagnostic",
            "HirDiagnostic",
            "SiteId(",
            "owning_pass",
            "construct:",
        ] {
            assert!(
                !stdout.contains(forbidden),
                "{verb} JSON contains compiler internals `{forbidden}`: {stdout}",
            );
        }
    }
}

/// A clean program emits an empty JSON array and exits 0.
#[test]
fn check_clean_program_emits_empty_array_and_exits_0() {
    let (_dir, path) = write_fixture("fn main() {\n    println(\"hello\")\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(0),
        "clean program must exit 0\n{}",
        describe_output(&output),
    );
    let diagnostics = parse_json_array(&output);
    assert!(
        diagnostics.is_empty(),
        "clean program must emit an empty diagnostic array; got: {diagnostics:?}",
    );
}

/// The code-action engine must surface a machine-checked fix in the JSON
/// `fixes` array — the core value for agentic consumers.
#[test]
fn check_json_carries_machine_checked_fix() {
    // An unused variable yields a "Prefix with `_`" quick-fix.
    let (_dir, path) = write_fixture("fn main() {\n    let unused = 1;\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

    let diagnostics = parse_json_array(&output);
    let unused = diagnostics
        .iter()
        .find(|d| d["code"] == "UnusedVariable")
        .expect("expected an UnusedVariable diagnostic");

    let fixes = unused["fixes"].as_array().expect("fixes is an array");
    assert!(
        !fixes.is_empty(),
        "UnusedVariable must carry a machine-checked fix: {unused}",
    );
    let edit = &fixes[0]["edits"][0];
    assert_eq!(
        edit["new_text"], "_unused",
        "fix edit must prefix the binding with `_`: {edit}",
    );
    assert!(
        edit["span"]["start_byte"].is_number(),
        "fix edit span must carry byte offsets so it can be applied: {edit}",
    );
}

/// `--format json` (space-separated) must be equivalent to `--format=json`.
#[test]
fn check_accepts_space_separated_format_value() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"oops\";\n}\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);
    assert_eq!(output.status.code(), Some(1));
    let diagnostics = parse_json_array(&output);
    assert!(
        diagnostics.iter().any(|d| d["code"] == "Mismatch"),
        "space-separated `--format json` must produce the same JSON",
    );
}

/// The text path must remain unchanged (no Debug payloads) and must not emit
/// JSON — guards that the default format is still human text.
#[test]
fn check_default_text_format_is_unchanged() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"oops\";\n}\n");
    let output = run(&["check", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert_eq!(output.status.code(), Some(1));
    assert!(
        stderr.contains("type mismatch"),
        "text diagnostics still render on stderr; got:\n{stderr}",
    );
    assert!(
        stdout.trim().is_empty(),
        "default text mode must not emit JSON on stdout; got:\n{stdout}",
    );
}

/// `E_MODULE_NOT_FOUND` locates itself at the offending `import` (not a zero
/// span) and, for a std-module typo close to a real module, names the fix.
#[test]
fn module_not_found_has_span_and_suggestion() {
    let (_dir, path) = write_fixture("import std.htp;\nfn main() { }\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "module-not-found must exit 1\n{}",
        describe_output(&output),
    );

    let diagnostics = parse_json_array(&output);
    let not_found = diagnostics
        .iter()
        .find(|d| d["code"] == "E_MODULE_NOT_FOUND")
        .unwrap_or_else(|| panic!("expected an E_MODULE_NOT_FOUND diagnostic: {diagnostics:?}"));

    let span = &not_found["span"];
    assert!(
        span["start_line"].as_u64().unwrap_or(0) > 0,
        "module-not-found must carry the import's own span, not a zero span: {span}",
    );
    assert_eq!(
        span["start_line"], 1,
        "the bad `import` is on line 1 of the fixture: {span}",
    );
    assert!(
        not_found["message"]
            .as_str()
            .unwrap()
            .contains("std.net.http"),
        "a std-module typo close to `std.net.http` must suggest it: {not_found}",
    );
    assert!(
        not_found["file"].as_str().unwrap().ends_with("main.hew"),
        "file field must name the source: {not_found}",
    );
}

/// The right module name at the wrong nesting (`std.http` for `std.net.http`)
/// is the single likeliest real mistake — not a typo `find_similar` would
/// catch (the leaf spelling is exactly right), so it needs its own exact-leaf
/// match ahead of the fuzzy fallback.
#[test]
fn module_not_found_suggests_an_exact_leaf_match_at_the_wrong_nesting() {
    let (_dir, path) = write_fixture("import std.http;\nfn main() { }\n");
    let output = run(&["check", "--format", "json", path.to_str().unwrap()]);

    let diagnostics = parse_json_array(&output);
    let not_found = diagnostics
        .iter()
        .find(|d| d["code"] == "E_MODULE_NOT_FOUND")
        .unwrap_or_else(|| panic!("expected an E_MODULE_NOT_FOUND diagnostic: {diagnostics:?}"));
    assert!(
        not_found["message"]
            .as_str()
            .unwrap()
            .contains("std.net.http"),
        "the right leaf name at the wrong nesting must still suggest `std.net.http`: {not_found}",
    );
}
