mod support;

use support::{hew_binary, repo_root, require_codegen};

fn run_fixture(name: &str) -> std::process::Output {
    let fixture = repo_root()
        .join("hew-cli")
        .join("tests")
        .join("fixtures")
        .join(name);
    std::process::Command::new(hew_binary())
        .arg("run")
        .arg(&fixture)
        .current_dir(repo_root())
        .output()
        .unwrap_or_else(|e| panic!("failed to run hew binary: {e}"))
}

/// Or-patterns: `1 | 2 | 3 => "small"` routes correctly for each alternative
/// and falls through to `_` for values outside the set.
#[test]
fn or_pattern_routes_all_alternatives() {
    require_codegen();

    let output = run_fixture("pattern_or.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Expects: small small small big big (one per line)
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["small", "small", "small", "big", "big"],
        "or-pattern output mismatch; stdout: {stdout}"
    );
}

/// Pattern guards: `x if x > 10` and `x if x > 0` are tried in order;
/// falling through on a false guard reaches the next arm.
#[test]
fn pattern_guard_ordered_dispatch() {
    require_codegen();

    let output = run_fixture("pattern_guard.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["big", "small", "non-positive", "non-positive"],
        "guard dispatch output mismatch; stdout: {stdout}"
    );
}

/// Guard over a constructor-payload binding: `MyOpt::Some(n) if n > 0` guards
/// on the binding introduced by the arm's own constructor pattern. The payload
/// binding must be in scope when the guard is evaluated.
#[test]
fn guard_over_payload_binding_resolves_correctly() {
    require_codegen();

    let output = run_fixture("pattern_guard_payload.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["positive", "non-positive", "non-positive", "absent"],
        "guard-over-payload output mismatch; stdout: {stdout}"
    );
}

/// Literal payload patterns: `MyOpt::Some(0) => "zero"` matches only when the
/// payload equals the literal; `MyOpt::Some(n) => "nonzero"` catches the rest.
#[test]
fn literal_payload_discriminates_specific_value() {
    require_codegen();

    let output = run_fixture("pattern_literal_payload.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["zero", "nonzero", "nothing"],
        "literal payload output mismatch; stdout: {stdout}"
    );
}

/// Payload or-patterns with leading-dot variants:
/// `.UnclosedBlock(m) | .UnknownDirective(m) => m` binds the same payload name
/// across two tuple-payload variants written in the implicit-enum (leading-dot)
/// form. The bound payload must flow to the body for each alternative; the unit
/// arm covers exhaustiveness.
#[test]
fn payload_or_pattern_binds_across_variants() {
    require_codegen();

    let output = run_fixture("pattern_payload_or.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["block-msg", "dir-msg", "empty"],
        "payload or-pattern output mismatch; stdout: {stdout}"
    );
}

/// Payload or-patterns with wildcard sub-patterns:
/// `.Click(_) | .Scroll(_)` discards the payload across both variants. Confirms
/// a wildcard payload slot lowers without emitting a binding and routes to the
/// shared arm body for each alternative.
#[test]
fn payload_or_pattern_wildcard_discards_payload() {
    require_codegen();

    let output = run_fixture("pattern_payload_or_wildcard.hew");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let lines: Vec<&str> = stdout.trim().lines().collect();
    assert_eq!(
        lines,
        ["active", "active", "idle"],
        "wildcard payload or-pattern output mismatch; stdout: {stdout}"
    );
}
