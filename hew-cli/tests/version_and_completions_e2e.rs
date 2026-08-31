mod support;

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use support::hew_binary;

#[allow(
    dead_code,
    reason = "the included build script exposes helpers that this test does not call"
)]
#[path = "../build.rs"]
mod build_script;

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

fn run_git(repo: &Path, args: &[&str]) {
    let output = Command::new("git")
        .current_dir(repo)
        .args(args)
        .env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_SYSTEM", "/dev/null")
        .output()
        .expect("run git");
    assert!(
        output.status.success(),
        "git {args:?} failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn temporary_git_repo() -> tempfile::TempDir {
    let repo = tempfile::tempdir().expect("create temporary repository");
    run_git(repo.path(), &["init", "--quiet"]);
    run_git(repo.path(), &["config", "user.name", "Version Test"]);
    run_git(
        repo.path(),
        &["config", "user.email", "version-test@example.invalid"],
    );
    fs::write(repo.path().join("tracked.txt"), "clean\n").expect("write tracked file");
    run_git(repo.path(), &["add", "tracked.txt"]);
    run_git(
        repo.path(),
        &["commit", "--quiet", "-m", "test: seed version repository"],
    );
    repo
}

#[test]
fn dirty_exact_tag_build_reports_dirty_identity() {
    let repo = temporary_git_repo();
    run_git(repo.path(), &["tag", "v0.6.0-rc3"]);
    fs::write(repo.path().join("tracked.txt"), "dirty\n").expect("make repository dirty");
    assert_eq!(
        build_script::git_version(repo.path(), env!("CARGO_PKG_VERSION")),
        "0.6.0-rc3+dirty"
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
