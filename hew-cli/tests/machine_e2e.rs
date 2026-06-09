mod support;

use std::process::Command;

use support::hew_binary;

fn machine_fixture() -> &'static str {
    "machine Light {\n    events {\n        Toggle;\n    }\n    state Off;\n    state On;\n    on Toggle: Off => On { On }\n    on Toggle: On => Off { Off }\n}\n"
}

#[test]
fn machine_diagram_emits_mermaid_on_stdout() {
    let dir = support::tempdir();
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
    let dir = support::tempdir();
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

fn composite_fixture() -> &'static str {
    "machine Conn {\n\
     \x20   events {\n\
     \x20       Connect;\n\
     \x20       Disconnect;\n\
     \x20   }\n\
     \x20   state Disconnected;\n\
     \x20   state Connected {\n\
     \x20       initial state Authenticating;\n\
     \x20       state Active;\n\
     \x20       on Disconnect: _ => Disconnected;\n\
     \x20   }\n\
     \x20   on Connect: Disconnected => Authenticating;\n\
     \x20   on Connect: _ => _ { state }\n\
     \x20   on Disconnect: _ => _ { state }\n\
     }\n"
}

#[test]
fn machine_diagram_renders_composite_nesting_in_mermaid() {
    // The diagram threads the AST composite grouping over the flat HIR (HIR has
    // no composite concept), so a depth-1 composite draws a nested
    // `state Connected { … }` block with the initial-substate marker.
    let dir = support::tempdir();
    let input = dir.path().join("conn.hew");
    std::fs::write(&input, composite_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("state Connected {"),
        "mermaid must nest the composite block; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("[*] --> Authenticating"),
        "nested block must mark the initial substate; stdout:\n{stdout}"
    );
    // The parent Disconnect rule expanded to concrete-source transitions.
    assert!(
        stdout.contains("Authenticating --> Disconnected : Disconnect"),
        "stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_composite_json_carries_composites_array() {
    let dir = support::tempdir();
    let input = dir.path().join("conn.hew");
    std::fs::write(&input, composite_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .arg("--format")
        .arg("json")
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"composites\":[{\"name\":\"Connected\""),
        "json must carry the composites array; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("\"initial\":\"Authenticating\""),
        "stdout:\n{stdout}"
    );
}

#[test]
fn machine_list_prints_summary_on_stdout() {
    let dir = support::tempdir();
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
    let dir = support::tempdir();
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
    let dir = support::tempdir();
    let input = dir.path().join("no_machine.hew");
    std::fs::write(&input, "fn main() -> i64 {\n    0\n}\n").unwrap();

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
