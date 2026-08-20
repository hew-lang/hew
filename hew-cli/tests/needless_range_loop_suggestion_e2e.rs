//! End-to-end regression: the `needless_range_loop` rewrite must compile.

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
fn local_pid_range_loop_has_no_uncompilable_direct_iteration_suggestion() {
    let output = check_fixture("local_pid_indexed_broadcast.hew");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "indexed LocalPid broadcast must compile:\n{stderr}"
    );
    assert!(
        !stderr.contains("iterate the collection directly"),
        "the lint must not suggest unsupported VecIter ownership semantics:\n{stderr}"
    );
}
