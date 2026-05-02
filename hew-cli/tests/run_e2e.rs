mod support;

use std::process::Command;

use support::{hew_binary, repo_root, require_codegen};

fn hew_std() -> std::path::PathBuf {
    repo_root().join("std")
}

/// Verify that `hew run --timeout` kills the entire process tree spawned by
/// the compiled Hew program, not just the root binary.
///
/// The Hew program uses `process.run()` to start a grandchild `sleep 98765`
/// via the shell and write its PID to a marker file, then spins forever.
/// After the timeout fires, both the compiled binary and the grandchild sleep
/// must be dead — proving that `killpg` is used rather than a bare `kill`.
///
/// Grandchild sleep inherits the compiled binary's process group (the Hew
/// runtime's `hew_process_run` spawns sh without `setpgid`, so background
/// jobs in non-interactive sh retain the parent's PGID). `BoundedChild` then
/// targets that entire group with `killpg(SIGKILL)` on timeout.
#[cfg(unix)]
#[test]
fn run_timeout_kills_grandchild_process_tree() {
    require_codegen();

    let dir = support::tempdir();
    let pid_file = dir.path().join("grandchild.pid");
    let hew_src = dir.path().join("grandchild_spinner.hew");

    // Write a Hew program that:
    //   1. Spawns a grandchild `sleep 98765` via the shell, writes its PID to
    //      the marker file (the shell exits immediately after; sleep stays in
    //      the compiled binary's process group).
    //   2. Loops forever so the timeout is guaranteed to fire.
    std::fs::write(
        &hew_src,
        format!(
            "import std::process;\n\
             fn main() {{\n\
             \x20   process.run(\"sh -c 'sleep 98765 & echo $! > {pid_file}'\");\n\
             \x20   var i = 0;\n\
             \x20   loop {{\n\
             \x20       i = i + 1;\n\
             \x20   }}\n\
             }}\n",
            pid_file = pid_file.display(),
        ),
    )
    .unwrap();

    // The --timeout is a hang watchdog, not a precision timer.  30 s gives the
    // compiled binary generous startup time (compilation + link + spawn) even
    // on a heavily loaded CI runner.  The program loops forever, so the
    // timeout always fires; the assertion is that the whole process group is
    // dead, not how long it took.
    let output = Command::new(hew_binary())
        .arg("run")
        .arg("--timeout")
        .arg("30")
        .arg(&hew_src)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        !output.status.success(),
        "hew run --timeout should fail; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Error: program timed out after"),
        "expected timeout error in stderr, got: {stderr}",
    );

    // Give the OS a brief window to finish reaping processes.
    // 50 ms is sufficient on contemporary OSes; 300 ms was conservative overhead.
    std::thread::sleep(std::time::Duration::from_millis(50));

    // Poll for the PID file to exist rather than assuming it is present.
    // Under load during the test run, the Hew program's startup and shell
    // invocation may take longer than expected.
    let pid_file_exists = {
        let mut retries = 0;
        loop {
            if pid_file.exists() {
                break true;
            }
            if retries >= 20 {
                break false;
            }
            retries += 1;
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    };
    assert!(
        pid_file_exists,
        "grandchild PID file should have been written before the timeout fired"
    );

    let pid_str = std::fs::read_to_string(&pid_file)
        .expect("grandchild PID file should have been written before the timeout fired");
    let grandchild_pid: u32 = pid_str
        .trim()
        .parse()
        .expect("grandchild PID file should contain a numeric PID");

    #[allow(
        clippy::cast_possible_wrap,
        reason = "PIDs fit in i32 on all supported Unix platforms"
    )]
    // SAFETY: `kill(pid, 0)` is a POSIX liveness probe — no signal is sent.
    let alive = unsafe { libc::kill(grandchild_pid as libc::pid_t, 0) } == 0;
    assert!(
        !alive,
        "grandchild PID {grandchild_pid} should be dead after process-group kill on timeout",
    );
}

#[test]
fn timeout_zero_is_rejected() {
    let output = Command::new(hew_binary())
        .args(["run", "--timeout", "0", "placeholder.hew"])
        .current_dir(repo_root())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: --timeout must be at least 1 second"));
}

#[test]
fn run_timeout_exit_code_is_non_zero() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("timeout_run.hew");
    std::fs::write(
        &path,
        "fn main() {\n    var i = 0;\n    loop {\n        i = i + 1;\n    }\n}\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg("--timeout")
        .arg("1")
        .arg(&path)
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error: program timed out after 1s"));
}

#[test]
fn run_program_with_std_path_exists_succeeds() {
    require_codegen();

    let dir = support::tempdir();
    let path = dir.path().join("path_exists_run.hew");
    std::fs::write(dir.path().join("present.txt"), "ok").unwrap();
    std::fs::create_dir(dir.path().join("present-dir")).unwrap();
    std::fs::write(
        &path,
        "import std::path;\n\
         fn main() {\n\
         \x20   println(path.exists(\"present.txt\"));\n\
         \x20   println(path.exists(\"present-dir\"));\n\
         \x20   println(path.exists(\"missing.txt\"));\n\
         }\n",
    )
    .unwrap();

    let output = Command::new(hew_binary())
        .arg("run")
        .arg(&path)
        .env("HEW_STD", hew_std())
        .current_dir(dir.path())
        .output()
        .unwrap();

    assert!(
        output.status.success(),
        "hew run should succeed; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "true\ntrue\nfalse\n"
    );
}
