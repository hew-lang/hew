use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

#[test]
fn actor_e2e_counter_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter", 42);
}

#[test]
fn actor_init_on_start_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter_init", 42);
}

#[test]
fn actor_on_stop_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_on_stop", 42);
}

/// Run a compiled fixture, killing it if it does not exit within `secs`.
///
/// The actor fixtures can deadlock at runtime; a bare `Command::output()`
/// would hang the test harness forever and orphan the child process. This
/// bounds the wait and reaps the child so a hang surfaces as a test failure
/// instead of a leaked process.
fn run_bounded(binary: &Path, secs: u64) -> std::process::ExitStatus {
    let mut command = Command::new(binary);
    hew_testutil::run_command_bounded(
        &mut command,
        format!("actor fixture {}", binary.display()),
        Duration::from_secs(secs),
    )
    .unwrap_or_else(|error| panic!("{error}"))
    .status
}

fn compile_and_run_actor_fixture(fixture_name: &str, expected_exit_code: i32) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let repo = manifest_dir.parent().expect("workspace root");
    let binary = repo.join(format!(".tmp/compile-out/{fixture_name}"));
    let _ = std::fs::remove_file(&binary);
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());

    if !repo.join("target/debug/libhew.a").exists() {
        let stdlib = Command::new(&cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .output()
            .expect("build libhew.a");
        assert!(
            stdlib.status.success(),
            "cargo build -p hew-lib failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&stdlib.stdout),
            String::from_utf8_lossy(&stdlib.stderr)
        );
    }

    let compile = Command::new(&cargo)
        .current_dir(repo)
        .args([
            "run",
            "--quiet",
            "-p",
            "hew-cli",
            "--bin",
            "hew",
            "--",
            "compile",
            &format!("examples/v05/{fixture_name}.hew"),
        ])
        .output()
        .expect("run hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let status = run_bounded(&binary, 30);
    assert_eq!(status.code(), Some(expected_exit_code));

    let _ = std::fs::remove_file(binary);
}
