//! End-to-end structural equality exec tests for records and payload enums.

#![cfg(not(target_arch = "wasm32"))]
#![cfg(unix)]

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn hew_command(repo: &Path) -> Command {
    let bin = target_dir(repo).join("debug").join("hew");
    if bin.exists() {
        return Command::new(bin);
    }
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn run_fixture(repo: &Path, group: &str, name: &str) {
    ensure_hew_runtime_lib(repo);
    let fixture = repo
        .join("examples")
        .join(group)
        .join(format!("{name}.hew"));
    let expected =
        std::fs::read_to_string(fixture.with_extension("expected")).expect("read .expected");

    let output = hew_command(repo)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run {} exited non-zero (status={:?}); stderr:\n{}",
        fixture.display(),
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout is utf-8");
    assert_eq!(stdout, expected, "{name} stdout mismatch");
}

#[test]
fn run_record_eq_executes() {
    run_fixture(&repo_root(), "records", "run_record_eq");
}

#[test]
fn run_float_eq_executes() {
    // Structural equality over f64 fields is bitwise/total: reflexive (NaN==NaN
    // when bits match) and +0.0 != -0.0. The .expected output pins both teeth.
    run_fixture(&repo_root(), "records", "run_float_eq");
}

#[test]
fn run_payload_enum_eq_executes() {
    run_fixture(&repo_root(), "enums", "run_payload_enum_eq");
}
