mod support;

use std::process::Command;

use support::hew_binary;

#[test]
fn ordinary_machine_diagram_retains_typed_outputs_and_dynamic_targets() {
    let dir = support::tempdir();
    let input = dir.path().join("gate.hew");
    std::fs::write(
        &input,
        include_str!("../../tests/core-acceptance/cases/machine-wildcard-hooks.hew"),
    )
    .unwrap();
    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(input)
        .args(["--format", "json"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let diagram: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(diagram["outputs"][0]["name"], "Trace");
    assert_eq!(diagram["outputs"][0]["fields"][0], "text");
    assert_eq!(diagram["transitions"][0]["to"], "_");
    assert_eq!(diagram["transitions"][0]["external"], true);
}

fn machine_fixture() -> &'static str {
    "machine Light {\n    events {\n        Toggle,\n    }\n    state Off,\n    state On,\n    on Toggle: Off => On,\n    on Toggle: On => Off,\n}\n"
}

fn missing_import_fixture() -> &'static str {
    "machine TrafficLight {\n\
     \x20   events { Tick, }\n\
     \x20   state Red,\n\
     \x20   state Green,\n\
     \x20   state Yellow,\n\
     \x20   on Tick: Red => Green,\n\
     \x20   on Tick: Green => Yellow,\n\
     \x20   on Tick: Yellow => Red,\n\
     }\n\
     fn main() {\n\
     \x20   let _ = fs.read(\"test.txt\");\n\
     }\n"
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
     \x20       Connect,\n\
     \x20       Disconnect,\n\
     \x20   }\n\
     \x20   state Disconnected,\n\
     \x20   state Connected {\n\
     \x20       initial state Authenticating,\n\
     \x20       state Active,\n\
     \x20       on Disconnect: _ => .Disconnected,\n\
     \x20   }\n\
     \x20   ,on Connect: Disconnected => .Authenticating,\n\
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

    // The native evaluator does not admit composite states (HEW-SPEC-2026
    // §3.11.2), so the checker refuses this fixture and the renderer only sees
    // it behind `--no-check`.
    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .args(["--format", "json", "--no-check"])
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let diagram: serde_json::Value = serde_json::from_str(&stdout).expect("diagram JSON");
    assert_eq!(
        diagram["composites"],
        serde_json::json!([{
            "name": "Connected",
            "initial": "Authenticating",
            "members": ["Authenticating", "Active"],
        }]),
        "json must carry the composite group with its initial substate; stdout:\n{stdout}"
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
    assert!(stderr.contains("cannot read"), "stderr: {stderr}");
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

#[test]
fn machine_diagram_fails_closed_on_missing_import() {
    let dir = support::tempdir();
    let input = dir.path().join("missing_import.hew");
    std::fs::write(&input, missing_import_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("stateDiagram-v2"),
        "must not emit a fabricated diagram; stdout: {stdout}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("undefined variable `fs`"),
        "stderr must surface the checker's missing-import diagnostic; stderr: {stderr}"
    );
}

#[test]
fn machine_list_fails_closed_on_missing_import() {
    let dir = support::tempdir();
    let input = dir.path().join("missing_import.hew");
    std::fs::write(&input, missing_import_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("list")
        .arg(&input)
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("machine TrafficLight"),
        "must not emit a fabricated inventory; stdout: {stdout}"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("undefined variable `fs`"),
        "stderr must surface the checker's missing-import diagnostic; stderr: {stderr}"
    );
}

#[test]
fn machine_list_fails_closed_on_parse_error() {
    let dir = support::tempdir();
    let input = dir.path().join("parse_err.hew");
    std::fs::write(&input, "machine Broken {\n  state A\n  on A + => \n").unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("list")
        .arg(&input)
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    // Located diagnostics, anchored on the offending file: the machine command
    // fails closed rather than listing a half-parsed machine.
    assert!(
        stderr.contains("parse_err.hew:2:10: error:"),
        "stderr: {stderr}"
    );
    assert!(
        stderr.contains("expected `,` between structural members"),
        "stderr: {stderr}"
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).is_empty(),
        "must not emit a fabricated inventory; stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
}

#[test]
fn machine_diagram_no_check_still_renders() {
    let dir = support::tempdir();
    let input = dir.path().join("missing_import.hew");
    std::fs::write(&input, missing_import_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("diagram")
        .arg(&input)
        .arg("--no-check")
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("stateDiagram-v2"), "stdout: {stdout}");
}

#[test]
fn machine_list_fails_closed_on_zero_machines() {
    let dir = support::tempdir();
    let input = dir.path().join("no_machine.hew");
    std::fs::write(&input, "fn main() -> i64 {\n    0\n}\n").unwrap();

    let output = Command::new(hew_binary())
        .arg("machine")
        .arg("list")
        .arg(&input)
        .output()
        .unwrap();

    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("No machines found in"), "stderr: {stderr}");
}

// ── Fixtures for new gap-filling tests ───────────────────────────────────────

/// Machine with `default { state }` — unhandled events stay in current state.
fn default_fixture() -> &'static str {
    "machine Tank {\n\
     \x20   events {\n\
     \x20       Fill,\n\
     \x20       Drain,\n\
     \x20   }\n\
     \x20   state Filling,\n\
     \x20   state Draining,\n\
     \x20   on Drain: Filling => Draining,\n\
     \x20   default { state }\n\
     }\n"
}

/// Machine with `reenter` self-transitions.
fn reenter_fixture() -> &'static str {
    "machine Counter {\n\
     \x20   events {\n\
     \x20       Inc,\n\
     \x20       Reset,\n\
     \x20   }\n\
     \x20   state Zero,\n\
     \x20   state NonZero { value: i64, }\n\
     \x20   ,on Inc: Zero => NonZero { value: 1 }\n\
     \x20   on Inc: NonZero => NonZero reenter { value: state.value + 1 }\n\
     \x20   on Reset: NonZero => Zero,\n\
     \x20   on Reset: Zero => Zero reenter,\n\
     }\n"
}

/// Machine with an `emits { … }` manifest.
/// Uses `default { state }` to satisfy exhaustiveness so the HIR check path works.
fn emits_fixture() -> &'static str {
    "machine Relay {\n\
     \x20   events {\n\
     \x20       Trigger,\n\
     \x20       Signal,\n\
     \x20   }\n\
     \x20   emits {\n\
     \x20       Signal,\n\
     \x20   }\n\
     \x20   state Idle,\n\
     \x20   state Active,\n\
     \x20   on Trigger: Idle => Active { emit Signal {}; .Active }\n\
     \x20   on Trigger: Active => Idle,\n\
     \x20   default { state }\n\
     }\n"
}

/// Generic machine — the ordinary HIR check path admits it. `default { state }`
/// satisfies exhaustiveness, which generic machines are checked for like any
/// other.
fn generic_fixture() -> &'static str {
    "machine Box<T> {\n\
     \x20   events {\n\
     \x20       Put { value: T, }\n\
     \x20       ,Take,\n\
     \x20   }\n\
     \x20   state Empty,\n\
     \x20   state Full { value: T, }\n\
     \x20   ,on Put(value): Empty => Full { value: value }\n\
     \x20   on Take: Full => Empty,\n\
     }\n"
}

// ── fix 1: `has_default` arm visible in all renderers ────────────────────────

#[test]
fn machine_diagram_default_mermaid_emits_note() {
    let dir = support::tempdir();
    let input = dir.path().join("tank.hew");
    std::fs::write(&input, default_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .arg("--no-check")
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("unhandled events stay in current state"),
        "mermaid must note stay-on-unhandled semantics; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_default_dot_emits_label() {
    let dir = support::tempdir();
    let input = dir.path().join("tank.hew");
    std::fs::write(&input, default_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--dot", "--no-check"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("unhandled events stay in current state"),
        "DOT must label stay-on-unhandled machines; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_default_json_has_default_true() {
    let dir = support::tempdir();
    let input = dir.path().join("tank.hew");
    std::fs::write(&input, default_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json", "--no-check"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"hasDefault\":true"),
        "JSON must carry hasDefault:true; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_no_default_json_has_default_false() {
    let dir = support::tempdir();
    let input = dir.path().join("light.hew");
    std::fs::write(&input, machine_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"hasDefault\":false"),
        "JSON must carry hasDefault:false for trapping machines; stdout:\n{stdout}"
    );
}

// ── fix 2: `reenter` self-loops labelled in all renderers ────────────────────

#[test]
fn machine_diagram_reenter_mermaid_labels_edge() {
    let dir = support::tempdir();
    let input = dir.path().join("counter.hew");
    std::fs::write(&input, reenter_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Inc [reenter]"),
        "mermaid must suffix [reenter] on reenter self-transition; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("Reset [reenter]"),
        "mermaid must suffix [reenter] on reenter self-transition; stdout:\n{stdout}"
    );
    // A non-reenter transition must NOT carry the suffix.
    assert!(
        stdout.contains("Zero --> NonZero : Inc\n") || stdout.contains("Zero --> NonZero : Inc\r"),
        "non-reenter transition must not have [reenter]; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_reenter_dot_labels_edge() {
    let dir = support::tempdir();
    let input = dir.path().join("counter.hew");
    std::fs::write(&input, reenter_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .arg("--dot")
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"Inc [reenter]\""),
        "DOT must include [reenter] in edge label; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_reenter_json_field() {
    let dir = support::tempdir();
    let input = dir.path().join("counter.hew");
    std::fs::write(&input, reenter_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"reenter\":true"),
        "JSON must carry reenter:true for reenter transitions; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("\"reenter\":false"),
        "JSON must carry reenter:false for normal transitions; stdout:\n{stdout}"
    );
}

// ── fix 3: `emits {}` manifest surfaced in all renderers ─────────────────────

#[test]
fn machine_diagram_emits_mermaid_shows_note() {
    let dir = support::tempdir();
    let input = dir.path().join("relay.hew");
    std::fs::write(&input, emits_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Emits: Signal"),
        "mermaid must show Emits note; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_emits_dot_tooltip() {
    let dir = support::tempdir();
    let input = dir.path().join("relay.hew");
    std::fs::write(&input, emits_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .arg("--dot")
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Emits: Signal"),
        "DOT must include Emits tooltip; stdout:\n{stdout}"
    );
}

#[test]
fn machine_diagram_emits_json_root_field() {
    let dir = support::tempdir();
    let input = dir.path().join("relay.hew");
    std::fs::write(&input, emits_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"emits\":[\"Signal\"]"),
        "JSON must carry emits array at root; stdout:\n{stdout}"
    );
}

#[test]
fn machine_list_shows_emits_section() {
    let dir = support::tempdir();
    let input = dir.path().join("relay.hew");
    std::fs::write(&input, emits_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "list"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Emits: Signal"),
        "cmd_list must show Emits section; stdout:\n{stdout}"
    );
}

// ── fix 4: generic machines go through the ordinary HIR check path ───────────

#[test]
fn machine_diagram_generic_renders_through_the_check_path() {
    let dir = support::tempdir();
    let input = dir.path().join("box.hew");
    std::fs::write(&input, generic_fixture()).unwrap();

    // Default (check) path — a generic machine is checked like any other and
    // renders with nothing on stderr. The old AST fallback and its
    // "skipping HIR checks" warning are retired.
    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "generic machine must render; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.stderr.is_empty(),
        "the check path must not warn about generic machines; stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("stateDiagram-v2"),
        "must produce diagram output; stdout:\n{stdout}"
    );
}

#[test]
fn machine_list_generic_lists_through_the_check_path() {
    let dir = support::tempdir();
    let input = dir.path().join("box.hew");
    std::fs::write(&input, generic_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "list"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "generic machine list must not fail; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("machine Box {"), "stdout:\n{stdout}");
    assert!(stdout.contains("  States:"), "stdout:\n{stdout}");
    assert!(stdout.contains("    Empty"), "stdout:\n{stdout}");
    assert!(stdout.contains("    Full { value }"), "stdout:\n{stdout}");
    assert!(stdout.contains("  Events:"), "stdout:\n{stdout}");
    assert!(stdout.contains("    Put { value }"), "stdout:\n{stdout}");
    assert!(stdout.contains("  Transitions: 2"), "stdout:\n{stdout}");

    assert!(
        output.stderr.is_empty(),
        "the check path must not warn about generic machines; stderr:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn machine_diagram_generic_json_carries_type_params() {
    let dir = support::tempdir();
    let input = dir.path().join("box.hew");
    std::fs::write(&input, generic_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"typeParams\":[\"T\"]"),
        "JSON must carry typeParams for generic machine; stdout:\n{stdout}"
    );
}

// ── fix 5: JSON wildcard rows expanded, not raw `_` ──────────────────────────

#[test]
fn machine_diagram_json_no_wildcard_rows() {
    // A wildcard source must reach the JSON as concrete states, never a raw
    // "_". The wildcard-derived `A => A` row is NOT suppressed: it is emitted
    // flagged `selfTransition: true, external: false`, matching the mermaid
    // renderer's `A --> A : Reset` edge. Whether a vacuous self-loop belongs in
    // a diagram at all is an open question; this pins what both renderers
    // actually agree on today so a change to either is deliberate.
    let dir = support::tempdir();
    // Minimal wildcard machine: one wildcard source, one named target.
    // `default { state }` satisfies exhaustiveness so the HIR path works.
    let source = "machine Toggle {\n\
                  \x20   events { Flip, Reset, }\n\
                  \x20   state A,\n\
                  \x20   state B,\n\
                  \x20   on Flip: A => B,\n\
                  \x20   on Reset: _ => A,\n\
                  \x20   default { state }\n\
                  }\n";
    let input = dir.path().join("toggle.hew");
    std::fs::write(&input, source).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let diagram: serde_json::Value = serde_json::from_str(&stdout).expect("diagram JSON");
    let transitions = diagram["transitions"]
        .as_array()
        .expect("transitions array")
        .clone();
    assert!(
        transitions
            .iter()
            .all(|row| row["from"] != "_" && row["to"] != "_"),
        "JSON must not carry a raw wildcard state; stdout:\n{stdout}"
    );
    assert_eq!(
        transitions,
        vec![
            serde_json::json!({"event": "Flip", "from": "A", "to": "B",
                "selfTransition": false, "guarded": false, "reenter": false, "external": true}),
            serde_json::json!({"event": "Reset", "from": "A", "to": "A",
                "selfTransition": true, "guarded": false, "reenter": false, "external": false}),
            serde_json::json!({"event": "Reset", "from": "B", "to": "A",
                "selfTransition": false, "guarded": false, "reenter": false, "external": true}),
        ],
        "wildcard Reset must expand to both concrete sources; stdout:\n{stdout}"
    );

    // The mermaid renderer draws the same self-loop, so the two agree.
    let mermaid = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();
    assert!(mermaid.status.success());
    let mermaid = String::from_utf8_lossy(&mermaid.stdout).into_owned();
    assert!(
        mermaid.contains("A --> A : Reset"),
        "mermaid must draw the same wildcard-derived self-loop; stdout:\n{mermaid}"
    );
}

#[test]
fn machine_diagram_json_event_fields_present() {
    // Events with payload fields must carry their field names in the JSON.
    let dir = support::tempdir();
    // `default { state }` satisfies exhaustiveness so the HIR check path works.
    let source = "machine Sender {\n\
                  \x20   events { Send { payload: i64, }, Ack, }\n\
                  \x20   state Idle,\n\
                  \x20   state Waiting,\n\
                  \x20   on Send: Idle => Waiting,\n\
                  \x20   on Ack: Waiting => Idle,\n\
                  \x20   default { state }\n\
                  }\n";
    let input = dir.path().join("sender.hew");
    std::fs::write(&input, source).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--format", "json"])
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"fields\":[\"payload\"]"),
        "JSON must include event payload field names; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("\"fields\":[]"),
        "JSON must include empty fields array for payload-free events; stdout:\n{stdout}"
    );
}

// ── machines reached through an import (#3229) ───────────────────────────────

/// A file whose only machine arrives through an import: the machine lives in
/// `std/machines/toggle.hew` and the root declares none of its own.
fn imported_only_fixture() -> &'static str {
    "import std.machines.toggle.{Toggle, ToggleEvent};\n\
     fn main() {\n\
     \x20   var t: Toggle = .Off;\n\
     \x20   t.step(ToggleEvent.Flip);\n\
     \x20   println(t.state_name());\n\
     }\n"
}

/// A root machine beside an imported one, so the command has to render both.
fn local_and_imported_fixture() -> &'static str {
    "import std.machines.toggle.{Toggle, ToggleEvent};\n\
     machine Door {\n\
     \x20   events { Push, }\n\
     \x20   state Closed,\n\
     \x20   state Open,\n\
     \x20   on Push: Closed => Open,\n\
     \x20   on Push: Open => Closed,\n\
     }\n\
     fn main() {\n\
     \x20   var d: Door = .Closed;\n\
     \x20   d.step(.Push);\n\
     \x20   var t: Toggle = .Off;\n\
     \x20   t.step(ToggleEvent.Flip);\n\
     \x20   println(f\"{d.state_name()} {t.state_name()}\");\n\
     }\n"
}

#[test]
fn machine_diagram_renders_a_machine_reached_only_through_an_import() {
    let dir = support::tempdir();
    let input = dir.path().join("driver.hew");
    std::fs::write(&input, imported_only_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "a file that only drives an imported machine must diagram it; stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("stateDiagram-v2"), "stdout:\n{stdout}");
    assert!(stdout.contains("[*] --> Off"), "stdout:\n{stdout}");
    assert!(stdout.contains("Off --> On : Flip"), "stdout:\n{stdout}");
    assert!(stdout.contains("On --> Off : Flip"), "stdout:\n{stdout}");
}

#[test]
fn machine_list_renders_a_machine_reached_only_through_an_import() {
    let dir = support::tempdir();
    let input = dir.path().join("driver.hew");
    std::fs::write(&input, imported_only_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "list"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("machine Toggle {"), "stdout:\n{stdout}");
    assert!(stdout.contains("    Off"), "stdout:\n{stdout}");
    assert!(stdout.contains("    On"), "stdout:\n{stdout}");
    assert!(stdout.contains("    Flip"), "stdout:\n{stdout}");
    assert!(stdout.contains("  Transitions: 2"), "stdout:\n{stdout}");
}

#[test]
fn machine_diagram_renders_the_local_and_the_imported_machine() {
    // Counterpart to the import-only case: rendering imported machines must not
    // cost the file its own, and `--machine` still selects one of the two.
    let dir = support::tempdir();
    let input = dir.path().join("both.hew");
    std::fs::write(&input, local_and_imported_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.matches("stateDiagram-v2").count(),
        2,
        "one diagram for the local machine and one for the imported; stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("Closed --> Open : Push"),
        "stdout:\n{stdout}"
    );
    assert!(stdout.contains("Off --> On : Flip"), "stdout:\n{stdout}");

    let selected = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .args(["--machine", "Toggle"])
        .output()
        .unwrap();
    assert!(selected.status.success());
    let selected = String::from_utf8_lossy(&selected.stdout);
    assert_eq!(
        selected.matches("stateDiagram-v2").count(),
        1,
        "--machine must select one; stdout:\n{selected}"
    );
    assert!(
        !selected.contains("Closed --> Open : Push"),
        "--machine Toggle must not render Door; stdout:\n{selected}"
    );
}

#[test]
fn machine_diagram_renders_no_machine_a_file_does_not_reach() {
    // Negative control for the import path: the standard library declares
    // machines, and a file that imports none of them must diagram none.
    let dir = support::tempdir();
    let input = dir.path().join("light.hew");
    std::fs::write(&input, machine_fixture()).unwrap();

    let output = Command::new(hew_binary())
        .args(["machine", "diagram"])
        .arg(&input)
        .output()
        .unwrap();

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(
        stdout.matches("stateDiagram-v2").count(),
        1,
        "only the file's own machine may render; stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("Flip"),
        "no unimported standard-library machine may appear; stdout:\n{stdout}"
    );
}

#[test]
fn machine_commands_succeed_on_every_compiling_machine_example() {
    // The shipped examples are the front door: every one that compiles must
    // diagram and list. The `reject_*` fixtures are deliberate compile errors
    // and are this test's negative control — they must keep failing, not be
    // skipped. `select_on_transition.hew` carries an imported machine beside an
    // f-string interpolation, so the pairing is covered here too.
    let examples = support::repo_root().join("examples/machine");
    let mut sources: Vec<std::path::PathBuf> = std::fs::read_dir(&examples)
        .unwrap_or_else(|e| panic!("read {}: {e}", examples.display()))
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "hew"))
        .collect();
    sources.sort();
    assert!(
        sources.len() > 1,
        "expected machine examples under {}",
        examples.display()
    );

    let mut rejected = 0usize;
    for source in &sources {
        let name = source.file_name().unwrap().to_string_lossy().into_owned();
        let reject_fixture = name.starts_with("reject_");
        if reject_fixture {
            rejected += 1;
        }

        for (subcommand, marker) in [("diagram", "stateDiagram-v2"), ("list", "machine ")] {
            let output = Command::new(hew_binary())
                .args(["machine", subcommand])
                .arg(source)
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);

            if reject_fixture {
                assert!(
                    !output.status.success(),
                    "{name} is a reject fixture; `machine {subcommand}` must refuse it; \
                     stdout:\n{stdout}"
                );
                continue;
            }

            assert!(
                output.status.success(),
                "{name}: `hew machine {subcommand}` must succeed; stderr:\n{stderr}"
            );
            assert!(
                stdout.contains(marker),
                "{name}: `machine {subcommand}` must render a machine; stdout:\n{stdout}"
            );
        }
    }
    assert!(
        rejected > 0,
        "the reject fixtures are this test's negative control; none were found"
    );
}
