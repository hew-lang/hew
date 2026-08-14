mod support;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::sync::Mutex;

use support::hew_binary;

static GIT_WORKTREE_MUTATION: Mutex<()> = Mutex::new(());

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

struct TaggedDirtyCheckout {
    repo: PathBuf,
    tracked_file: PathBuf,
    original_contents: Vec<u8>,
}

impl TaggedDirtyCheckout {
    fn create(repo: &Path) -> Self {
        let tracked_file = repo.join("hew-cli/build.rs");
        let original_contents = fs::read(&tracked_file).expect("read tracked build script");
        run_git(repo, &["tag", "v0.6.0-rc2"]);
        fs::write(
            &tracked_file,
            [original_contents.as_slice(), b"\n"].concat(),
        )
        .expect("make tracked file dirty");
        Self {
            repo: repo.to_path_buf(),
            tracked_file,
            original_contents,
        }
    }
}

impl Drop for TaggedDirtyCheckout {
    fn drop(&mut self) {
        fs::write(&self.tracked_file, &self.original_contents)
            .expect("restore tracked build script");
        run_git(&self.repo, &["tag", "-d", "v0.6.0-rc2"]);
    }
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

fn run_git_with_input(repo: &Path, args: &[&str], input: &[u8]) {
    let mut child = Command::new("git")
        .current_dir(repo)
        .args(args)
        .env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_SYSTEM", "/dev/null")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("run git with input");
    std::io::Write::write_all(child.stdin.as_mut().expect("git stdin"), input)
        .expect("write git input");
    let output = child.wait_with_output().expect("wait for git");
    assert!(
        output.status.success(),
        "git {args:?} failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn temporary_repo_from_worktree(repo: &Path) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create temporary repository directory");
    let archive = dir.path().join("source.tar");
    let repo_dir = dir.path().join("repo");
    fs::create_dir(&repo_dir).expect("create temporary repository root");
    let output = Command::new("git")
        .current_dir(repo)
        .args(["archive", "--format=tar", "HEAD", "--output"])
        .arg(&archive)
        .output()
        .expect("archive repository");
    assert!(output.status.success(), "git archive failed: {output:?}");
    let output = Command::new("tar")
        .current_dir(&repo_dir)
        .args(["-xf"])
        .arg(&archive)
        .output()
        .expect("extract repository archive");
    assert!(output.status.success(), "tar extract failed: {output:?}");

    let diff = Command::new("git")
        .current_dir(repo)
        .args(["diff", "--binary", "HEAD"])
        .output()
        .expect("capture worktree diff");
    assert!(diff.status.success(), "git diff failed: {diff:?}");
    run_git_with_input(&repo_dir, &["init", "--quiet"], &[]);
    run_git(&repo_dir, &["config", "user.name", "Version Test"]);
    run_git(
        &repo_dir,
        &["config", "user.email", "version-test@example.invalid"],
    );
    run_git_with_input(
        &repo_dir,
        &["apply", "--whitespace=nowarn", "-"],
        &diff.stdout,
    );
    run_git(&repo_dir, &["add", "."]);
    run_git(
        &repo_dir,
        &["commit", "--quiet", "-m", "test: seed version repository"],
    );
    dir
}

#[test]
fn dirty_exact_tag_build_reports_dirty_identity() {
    let _lock = GIT_WORKTREE_MUTATION.lock().expect("lock git worktree");
    let source_repo = support::repo_root();
    let temp_repo = temporary_repo_from_worktree(source_repo);
    let repo = temp_repo.path().join("repo");
    let checkout = TaggedDirtyCheckout::create(&repo);
    let target_dir = tempfile::tempdir().expect("create isolated cargo target directory");

    let output = Command::new("cargo")
        .current_dir(&repo)
        .args(["build", "--quiet", "-p", "hew-cli", "--bin", "hew"])
        .arg("--manifest-path")
        .arg(repo.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target_dir.path())
        .output()
        .expect("build tagged dirty hew-cli");
    assert!(
        output.status.success(),
        "cargo build failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let probe = Command::new(target_dir.path().join("debug/hew"))
        .arg("--version")
        .output()
        .expect("probe tagged dirty hew binary");
    assert!(
        probe.status.success(),
        "hew --version failed: {}",
        String::from_utf8_lossy(&probe.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&probe.stdout).trim_end(),
        "hew 0.6.0-rc2+dirty"
    );

    drop(checkout);
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
