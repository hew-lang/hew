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
fn bare_machine_transition_is_rejected_with_contextual_fix_it() {
    let output = check_fixture("bare_machine_transition_reject.hew");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "bare machine transition states must be rejected:\n{stderr}"
    );
    assert!(
        stderr.contains("E_BARE_VARIANT_EXPR"),
        "bare transition target must report the bare-variant error:\n{stderr}"
    );
    assert!(
        stderr.contains("replace `On` with `.On`"),
        "bare transition target must suggest contextual syntax:\n{stderr}"
    );
    // The fix-it is only appliable if it is anchored on the text it asks the
    // author to replace. In `bare_machine_transition_reject.hew`, `On` is the
    // target of the line-9 transition at column 23 and `Off` the target of
    // line 10 at column 22 — not the following `on` keyword or the closing
    // brace.
    assert!(
        stderr.contains(
            "bare_machine_transition_reject.hew:9:23: error: E_BARE_VARIANT_EXPR: bare variant `On`"
        ),
        "the `On` fix-it must be anchored on the target state token:\n{stderr}"
    );
    assert!(
        stderr.contains(
            "bare_machine_transition_reject.hew:10:22: error: E_BARE_VARIANT_EXPR: bare variant `Off`"
        ),
        "the `Off` fix-it must be anchored on the target state token:\n{stderr}"
    );
}
