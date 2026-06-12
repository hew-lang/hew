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

/// `--format=json` must be rejected as unrecognized before this lane wired it
/// in — this test now asserts it is *accepted* (the inverse guard), so a
/// regression that drops the flag fails loudly.
#[test]
fn format_json_flag_is_recognized_on_check() {
    let (_dir, path) = write_fixture("fn main() {\n    println(\"ok\")\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);
    assert!(
        output.status.success(),
        "`--format=json` must be a recognized flag on `hew check`\n{}",
        describe_output(&output),
    );
}

/// A type error emits a parseable JSON array carrying at least
/// `{code, severity, file, span, message}` and exits 1.
#[test]
fn check_type_error_emits_structured_json_and_exits_1() {
    let (_dir, path) = write_fixture("fn main() {\n    let x: i64 = \"not a number\";\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

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

/// A not-yet-implemented program's JSON message must contain no Rust `{...}`
/// Debug payload and no `0x` hex — it reads as a human limitation.
#[test]
fn check_not_yet_implemented_json_has_no_debug_payload() {
    // A standalone regex literal triggers a MIR `NotYetImplemented` gate.
    let (_dir, path) =
        write_fixture("fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "NYI program must exit 1\n{}",
        describe_output(&output),
    );
    let diagnostics = parse_json_array(&output);
    let nyi = diagnostics
        .iter()
        .find(|d| d["code"] == "NotYetImplemented")
        .expect("expected a NotYetImplemented diagnostic");

    let message = nyi["message"].as_str().unwrap();
    assert!(
        !message.contains('{') && !message.contains('}'),
        "NYI message must not contain a Rust Debug struct payload; got: {message}",
    );
    assert!(
        !message.contains("0x"),
        "NYI message must not contain hex pointers; got: {message}",
    );
    assert!(
        !message.contains("owning_pass") && !message.contains("SiteId"),
        "NYI message must not leak Rust field names; got: {message}",
    );
}

/// The whole JSON payload — across every diagnostic and note — must be free of
/// Rust Debug struct payloads. This is the strongest guard for the leak fix.
#[test]
fn check_json_payload_is_free_of_debug_structs() {
    let (_dir, path) =
        write_fixture("fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n");
    let output = run(&["check", "--format=json", path.to_str().unwrap()]);
    let stdout = String::from_utf8_lossy(&output.stdout);

    // The serde-rendered JSON object braces are expected; a Rust Debug payload
    // shows up as field-name fragments that never belong in user output.
    for forbidden in [
        "MirDiagnostic",
        "HirDiagnostic",
        "SiteId(",
        "owning_pass",
        "construct:",
    ] {
        assert!(
            !stdout.contains(forbidden),
            "JSON payload must not contain Rust Debug fragment `{forbidden}`; got:\n{stdout}",
        );
    }
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

/// `hew compile --format=json` routes a MIR gate failure through the JSON sink
/// rather than leaking a `MirDiagnostic { .. }` Debug payload (the bundled leak
/// fix, observed from the compile path the audit pinpointed).
#[test]
fn compile_mir_gate_failure_json_has_no_debug_payload() {
    let (_dir, path) =
        write_fixture("fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n");
    let output = run(&["compile", "--format=json", path.to_str().unwrap()]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "compile MIR gate failure must exit 1\n{}",
        describe_output(&output),
    );
    let diagnostics = parse_json_array(&output);
    let nyi = diagnostics
        .iter()
        .find(|d| d["code"] == "NotYetImplemented")
        .expect("expected a NotYetImplemented diagnostic from the compile path");
    let message = nyi["message"].as_str().unwrap();
    assert!(
        !message.contains('{') && !message.contains("SiteId"),
        "compile NYI message must not leak a Debug payload; got: {message}",
    );
}

/// `hew compile` (text) must no longer leak the raw `MirDiagnostic { .. }`
/// Debug payload — this is the before/after guard for the `main.rs` leak site.
#[test]
fn compile_mir_gate_failure_text_has_no_debug_payload() {
    let (_dir, path) =
        write_fixture("fn main() -> i64 {\n    let _r = re\"hello\";\n    return 0;\n}\n");
    let output = run(&["compile", path.to_str().unwrap()]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert_eq!(output.status.code(), Some(1));
    assert!(
        !stderr.contains("MirDiagnostic")
            && !stderr.contains("NotYetImplemented {")
            && !stderr.contains("SiteId("),
        "compile must not emit raw MIR Debug payloads; got:\n{stderr}",
    );
    assert!(
        stderr.contains("not implemented yet"),
        "compile should render the user-readable MIR message; got:\n{stderr}",
    );
    assert!(
        stderr.contains("main.hew:2:"),
        "compile MIR diagnostic should be source-attributed; got:\n{stderr}",
    );
}
