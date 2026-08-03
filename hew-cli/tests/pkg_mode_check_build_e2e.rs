/// No-input `hew check` / `hew build` (package mode) coverage.
///
/// With no `.hew` input file, `hew check` validates the manifest and `hew
/// build` runs the package's `[native]` build. Both modes cannot honor
/// compile-only flags (there is no source file to apply them to), so every
/// such flag must be rejected loudly rather than silently ignored, and
/// `--format json` — which has no structured-diagnostic output to format in
/// package mode — must be rejected rather than emitting invalid mixed
/// text+JSON output.
mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, hew_binary};

const VALID_MANIFEST: &str = "[package]\nname = \"pkgmode\"\nversion = \"1.0.0\"\n";

#[test]
fn check_no_input_default_output_validates_manifest() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .arg("check")
        .current_dir(dir.path())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "hew check with no input should validate a clean manifest\n{}",
        describe_output(&output),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("OK: manifest is valid"),
        "expected manifest-valid message, got:\n{}",
        describe_output(&output),
    );
}

#[test]
fn check_no_input_format_json_is_rejected() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .args(["check", "--format", "json"])
        .current_dir(dir.path())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "hew check --format json with no input should be rejected, not silently mixed\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("--format json"),
        "rejection should name the flag:\n{}",
        describe_output(&output),
    );
    // stdout still carries the single JsonDiagnosticFlush chokepoint's output
    // (an empty diagnostic array, since json mode was already active before
    // the rejection) — the same convention every other usage error under
    // `--format json` follows. It must not ALSO carry the human manifest text.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("OK: manifest is valid") && !stdout.contains("Issues found"),
        "rejected package-mode json request should not print manifest text on stdout:\n{}",
        describe_output(&output),
    );
}

#[test]
fn check_no_input_rejects_compile_only_project_dir_flag() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .args(["check", "--project-dir", "somewhere"])
        .current_dir(dir.path())
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "hew check --project-dir with no input should be rejected, not silently ignored\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("require an input .hew file"),
        "rejection should explain that the flag needs an input file:\n{}",
        describe_output(&output),
    );
}

#[test]
fn build_no_input_default_output_reports_native_build() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("invoke hew build");

    assert!(
        output.status.success(),
        "hew build with no input and no [native] section should no-op cleanly\n{}",
        describe_output(&output),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("nothing to build") || stdout.contains("Built native lib"),
        "expected a native-build status message, got:\n{}",
        describe_output(&output),
    );
}

#[test]
fn build_no_input_format_json_is_rejected() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .args(["build", "--format", "json"])
        .current_dir(dir.path())
        .output()
        .expect("invoke hew build");

    assert!(
        !output.status.success(),
        "hew build --format json with no input should be rejected, not silently mixed\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("--format json"),
        "rejection should name the flag:\n{}",
        describe_output(&output),
    );
    // See the matching comment in `check_no_input_format_json_is_rejected`:
    // stdout still carries the single JsonDiagnosticFlush chokepoint's empty
    // array, but must not ALSO carry the human build-status text.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("nothing to build") && !stdout.contains("Built native lib"),
        "rejected package-mode json request should not print build text on stdout:\n{}",
        describe_output(&output),
    );
}

#[test]
fn build_no_input_rejects_compile_only_target_flag() {
    let dir = support::tempdir();
    fs::write(dir.path().join("hew.toml"), VALID_MANIFEST).expect("write hew.toml");

    let output = Command::new(hew_binary())
        .args(["build", "--target", "wasm32-unknown-unknown"])
        .current_dir(dir.path())
        .output()
        .expect("invoke hew build");

    assert!(
        !output.status.success(),
        "hew build --target with no input should be rejected, not silently ignored\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("require an input .hew file"),
        "rejection should explain that the flag needs an input file:\n{}",
        describe_output(&output),
    );
}
