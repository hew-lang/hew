mod support;

use std::process::Command;

use support::{hew_binary, require_codegen};

/// `hew eval --jit auto "1 + 1"` must print `2` and exit 0.
///
/// `auto` selects the best-available backend; with the in-process LLJIT bridge
/// still unimplemented (#1227/#1235) it falls back to the AOT path rather than
/// failing closed, so this scenario now works end to end.
#[test]
fn jit_auto_simple_expression_succeeds() {
    require_codegen();

    let output = Command::new(hew_binary())
        .args(["eval", "--jit", "auto", "1 + 1"])
        .output()
        .expect("failed to spawn hew binary");

    assert!(
        output.status.success(),
        "hew eval --jit auto exited with non-zero status\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "2\n",
        "expected stdout to be '2\\n'"
    );
}
