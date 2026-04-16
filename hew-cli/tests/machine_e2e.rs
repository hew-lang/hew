mod support;

use std::process::Command;

use support::hew_binary;

fn machine_fixture() -> &'static str {
    "machine Light {\n    state Off;\n    state On;\n    event Toggle;\n    on Toggle: Off -> On { On }\n    on Toggle: On -> Off { Off }\n}\n"
}

#[test]
fn machine_diagram_emits_mermaid_on_stdout() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("light.hew");
    std::fs::write(&input, machine_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    assert!(output.stderr.is_empty());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("stateDiagram-v2"), "stdout: {stdout}");
    assert!(stdout.contains("[*] --> Off"), "stdout: {stdout}");
    assert!(stdout.contains("Off --> On : Toggle"), "stdout: {stdout}");
    assert!(stdout.contains("On --> Off : Toggle"), "stdout: {stdout}");
}

#[test]
fn machine_diagram_dot_emits_graphviz_on_stdout() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("light.hew");
    std::fs::write(&input, machine_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .arg("--dot")
        .output()
        .unwrap();

    assert!(output.status.success());
    assert!(output.stderr.is_empty());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("digraph Light {"), "stdout: {stdout}");
    assert!(stdout.contains("__start -> Off;"), "stdout: {stdout}");
    assert!(
        stdout.contains("Off -> On [label=\"Toggle\"]"),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("On -> Off [label=\"Toggle\"]"),
        "stdout: {stdout}"
    );
}

#[test]
fn machine_list_prints_summary_on_stdout() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("light.hew");
    std::fs::write(&input, machine_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("list")
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    assert!(output.stderr.is_empty());

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("machine Light {"), "stdout: {stdout}");
    assert!(stdout.contains("  States:"), "stdout: {stdout}");
    assert!(stdout.contains("    Off"), "stdout: {stdout}");
    assert!(stdout.contains("    On"), "stdout: {stdout}");
    assert!(stdout.contains("  Events:"), "stdout: {stdout}");
    assert!(stdout.contains("    Toggle"), "stdout: {stdout}");
    assert!(stdout.contains("  Transitions: 2"), "stdout: {stdout}");
}

#[test]
fn machine_diagram_missing_file_exits_non_zero() {
    let dir = tempfile::tempdir().unwrap();
    let missing = dir.path().join("missing.hew");

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&missing)
        .output()
        .unwrap();

    assert!(!output.status.success());

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error reading"), "stderr: {stderr}");
    assert!(stderr.contains("missing.hew"), "stderr: {stderr}");
}

#[test]
fn machine_diagram_no_machines_exits_non_zero() {
    let dir = tempfile::tempdir().unwrap();
    let input = dir.path().join("no_machine.hew");
    std::fs::write(&input, "fn main() -> int {\n    0\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .output()
        .unwrap();

    assert!(!output.status.success());
    assert!(
        output.stdout.is_empty(),
        "stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("No machines found in"), "stderr: {stderr}");
    assert!(stderr.contains("no_machine.hew"), "stderr: {stderr}");
}
