//! End-to-end regressions for diagnostics whose suggested source must compile.

mod support;

use std::process::Command;

use support::{hew_binary, repo_root, strip_ansi};

fn check_fixture(name: &str) -> std::process::Output {
    let fixture = repo_root()
        .join("hew-cli/tests/fixtures/diagnostic_suggestions")
        .join(name);
    Command::new(hew_binary())
        .args([
            "check",
            fixture.to_str().expect("fixture path must be UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("hew check must run")
}

#[test]
fn contextual_machine_transition_suggestion_compiles() {
    let output = check_fixture("contextual_machine_transition.hew");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "contextual machine transition states must compile:\n{stderr}"
    );
    assert!(
        !stderr.contains("E_BARE_VARIANT_EXPR"),
        "contextual transition states must not retain bare-variant warnings:\n{stderr}"
    );
}

#[test]
fn bare_machine_transition_compiles_without_variant_suggestion() {
    let output = check_fixture("bare_machine_transition.hew");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "bare machine transition states must compile:\n{stderr}"
    );
    assert!(
        !stderr.contains("E_BARE_VARIANT_EXPR"),
        "machine state targets must not be treated as bare variants:\n{stderr}"
    );
}
