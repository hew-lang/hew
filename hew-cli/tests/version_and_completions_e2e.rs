mod support;

use std::process::{Command, Output};

use support::hew_binary;

fn run_hew(args: &[&str]) -> Output {
    Command::new(hew_binary()).args(args).output().unwrap()
}

fn assert_version_shape(stdout: &str) {
    let line = stdout.trim_end();
    assert!(line.starts_with("hew "), "stdout: {stdout}");
    assert_eq!(
        line,
        format!("hew {}", env!("HEW_VERSION")),
        "stdout: {stdout}"
    );
}

#[test]
fn version_shape_matches_build_identity() {
    assert_version_shape(&format!("hew {}\n", env!("HEW_VERSION")));
}

#[test]
fn version_shape_rejects_malformed_git_metadata() {
    for detail in [
        "unknown-unknown",
        "git-unknown",
        "git-unavailable-dirty",
        "not-a-hash",
        "a1b2c3d-unknown",
        "A1B2C3D",
        "A1B2C3D-dirty",
    ] {
        let output = format!("hew {} (debug, {detail})\n", env!("CARGO_PKG_VERSION"));
        assert!(
            std::panic::catch_unwind(|| assert_version_shape(&output)).is_err(),
            "malformed version detail was accepted: {detail}"
        );
    }
}

#[test]
fn hew_version_subcommand_exits_zero_and_emits_version_on_stdout() {
    let output = run_hew(&["version"]);

    assert!(
        output.status.success(),
        "hew version failed:
stdout: {}
stderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_version_shape(&stdout);
}

#[test]
fn non_tag_build_version_contains_dev_identity() {
    let output = run_hew(&["--version"]);
    assert!(
        output.status.success(),
        "hew --version failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("-dev."),
        "non-tag build should include a dev identity: {stdout}"
    );
}

fn assert_completions_output(shell: &str, marker: &str) {
    let output = run_hew(&["completions", shell]);

    assert!(
        output.status.success(),
        "hew completions {shell} failed:
stdout: {}
stderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert!(
        output.stderr.is_empty(),
        "unexpected stderr for {shell}: {}",
        String::from_utf8_lossy(&output.stderr),
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("hew"), "stdout for {shell}: {stdout}");
    assert!(stdout.contains(marker), "stdout for {shell}: {stdout}");
}

#[test]
fn completions_bash_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("bash", "complete -F _hew");
}

#[test]
fn completions_zsh_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("zsh", "#compdef hew");
}

#[test]
fn completions_fish_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output("fish", "complete -c hew");
}

#[test]
fn completions_powershell_exits_zero_and_emits_script_to_stdout() {
    assert_completions_output(
        "powershell",
        "Register-ArgumentCompleter -Native -CommandName 'hew'",
    );
}
