//! Multi-process proof that test runs consume a prebuilt `libhew.a` without
//! invoking Cargo.
//!
//! The test gate builds and certifies the archive before this binary starts.
//! Every child then shares one nextest run id, verifies that archive through
//! `ensure_hew_lib_built`, and performs a real `hew compile` link. `CARGO`
//! points at a fail-closed spy, so any attempted bootstrap is both recorded and
//! rejected. The load-bearing verdict is zero spy records plus every real link
//! succeeding.

#![cfg(unix)]

use std::env;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::time::{Duration, Instant};

const CHILD_ENV: &str = "HEW_VERIFY_ONLY_CHILD";
const READY_ENV: &str = "HEW_VERIFY_ONLY_READY";
const RELEASE_ENV: &str = "HEW_VERIFY_ONLY_RELEASE";
const CHILD_COUNT: usize = 12;
const READY_DEADLINE: Duration = Duration::from_secs(30);
const CHILD_DEADLINE: Duration = Duration::from_mins(1);
const POLL_INTERVAL: Duration = Duration::from_millis(5);

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-testutil has a workspace parent")
        .to_path_buf()
}

fn profile_dir() -> PathBuf {
    env::current_exe()
        .expect("resolve current test binary")
        .parent()
        .and_then(Path::parent)
        .expect("test binary has target/<profile>/deps ancestry")
        .to_path_buf()
}

fn hew_binary() -> PathBuf {
    profile_dir().join("hew")
}

fn wait_for(path: &Path, deadline: Duration, label: &str) {
    let started = Instant::now();
    while !path.exists() {
        assert!(
            started.elapsed() < deadline,
            "timed out waiting for {label}"
        );
        std::thread::sleep(POLL_INTERVAL);
    }
}

fn run_child() {
    let ready = PathBuf::from(env::var_os(READY_ENV).expect("child ready path is set"));
    let release = PathBuf::from(env::var_os(RELEASE_ENV).expect("child release path is set"));
    fs::write(&ready, b"ready").expect("publish child readiness");
    wait_for(&release, READY_DEADLINE, "parent release");

    hew_testutil::ensure_hew_lib_built()
        .expect("test gate must provide a present, certified libhew archive");

    let scratch = tempfile::tempdir().expect("create link scratch directory");
    let source = scratch.path().join("probe.hew");
    let emit = scratch.path().join("out");
    fs::write(&source, "fn main() {}\n").expect("write link fixture");

    let mut command = Command::new(hew_binary());
    command
        .arg("compile")
        .arg(&source)
        .arg("--emit-dir")
        .arg(&emit);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        "verify-only hew compile",
        Duration::from_secs(30),
    )
    .expect("real hew compile must complete");
    assert!(
        output.status.success(),
        "real hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn write_cargo_spy(path: &Path, records: &Path) {
    fs::create_dir_all(records).expect("create cargo-spy record directory");
    fs::write(
        path,
        format!(
            "#!/bin/sh\nset -eu\ntouch '{}/invoked-'\"$$\"\necho 'cargo is forbidden during a test run' >&2\nexit 97\n",
            records.display()
        ),
    )
    .expect("write cargo spy");
    let mut permissions = fs::metadata(path).expect("stat cargo spy").permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(path, permissions).expect("make cargo spy executable");
}

fn wait_for_children_ready(paths: &[PathBuf]) {
    let started = Instant::now();
    while !paths.iter().all(|path| path.exists()) {
        assert!(
            started.elapsed() < READY_DEADLINE,
            "timed out waiting for all verify-only children"
        );
        std::thread::sleep(POLL_INTERVAL);
    }
}

fn reap_child(child: &mut Child, index: usize) {
    let started = Instant::now();
    loop {
        match child.try_wait().expect("poll verify-only child") {
            Some(status) => {
                assert!(
                    status.success(),
                    "verify-only child {index} failed: {status}"
                );
                return;
            }
            None if started.elapsed() >= CHILD_DEADLINE => {
                hew_testutil::terminate_process_group(child, "verify-only child")
                    .expect("terminate timed-out verify-only child");
                panic!("verify-only child {index} exceeded {CHILD_DEADLINE:?}");
            }
            None => std::thread::sleep(POLL_INTERVAL),
        }
    }
}

#[test]
#[ignore = "run by `make libhew-link-race-test` after building the compiler and libhew archive"]
fn concurrent_test_links_never_invoke_cargo() {
    if env::var_os(CHILD_ENV).is_some() {
        run_child();
        return;
    }

    assert!(
        hew_binary().is_file(),
        "test gate must prebuild the hew binary"
    );
    let scratch = tempfile::tempdir().expect("create parent scratch directory");
    let records = scratch.path().join("cargo-records");
    let cargo_spy = scratch.path().join("cargo-spy.sh");
    let release = scratch.path().join("release");
    write_cargo_spy(&cargo_spy, &records);

    let exe = env::current_exe().expect("resolve current test binary");
    let run_id = format!("verify-only-{}", std::process::id());
    let mut children = Vec::new();
    let mut ready_paths = Vec::new();
    for index in 0..CHILD_COUNT {
        let ready = scratch.path().join(format!("ready-{index}"));
        let mut command = Command::new(&exe);
        command
            .args([
                "--exact",
                "concurrent_test_links_never_invoke_cargo",
                "--ignored",
                "--nocapture",
                "--test-threads=1",
            ])
            .env(CHILD_ENV, "1")
            .env(READY_ENV, &ready)
            .env(RELEASE_ENV, &release)
            .env("NEXTEST_RUN_ID", &run_id)
            .env("HEW_TEST_NO_BUILD", "1")
            .env("CARGO", &cargo_spy);
        hew_testutil::own_process_group(&mut command);
        children.push(command.spawn().expect("spawn verify-only child"));
        ready_paths.push(ready);
    }

    wait_for_children_ready(&ready_paths);
    fs::write(&release, b"go").expect("release verify-only children");
    for (index, child) in children.iter_mut().enumerate() {
        reap_child(child, index);
    }

    let invocations = fs::read_dir(&records)
        .expect("read cargo-spy records")
        .count();
    assert_eq!(
        invocations, 0,
        "test-run archive verification or linking attempted to invoke Cargo"
    );

    let certificate = Command::new(repo_root().join("scripts/check-libhew-fresh.sh"))
        .args(["--debug-dir"])
        .arg(profile_dir())
        .output()
        .expect("run libhew certificate verifier");
    assert!(
        certificate.status.success(),
        "test gate did not leave a valid libhew certificate\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&certificate.stdout),
        String::from_utf8_lossy(&certificate.stderr)
    );
}
