//! End-to-end execution test for the stdlib `Lifecycle<T>` machine fixture.

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
    let bin = target_dir(repo)
        .join("debug")
        .join(format!("hew{}", std::env::consts::EXE_SUFFIX));
    if bin.exists() {
        return Command::new(bin);
    }

    // Cold `target/`: build `hew` once under the shared serialized build
    // lock, OUTSIDE any per-test deadline, so a concurrent build-lock holder
    // cannot make a `cargo run` fallback burn the bounded budget and produce
    // a false timeout (hew-lang/hew#1887).
    Command::new(hew_testutil::ensure_hew_bin_built().expect("build hew binary"))
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

#[test]
fn run_lifecycle_fixture_compiles_and_matches_expected_stdout() {
    let repo = repo_root();
    ensure_hew_runtime_lib(&repo);

    let fixture = repo
        .join("examples")
        .join("machine")
        .join("run_lifecycle.hew");
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));
    let out_dir =
        std::env::temp_dir().join(format!("hew-machine-lifecycle-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&out_dir);
    std::fs::create_dir_all(&out_dir).expect("create lifecycle compile output dir");

    let compile = hew_command(&repo)
        .arg("compile")
        .arg(&fixture)
        .arg("--emit-dir")
        .arg(&out_dir)
        .output()
        .expect("spawn hew compile");
    assert!(
        compile.status.success(),
        "hew compile run_lifecycle exited non-zero (status={:?}); stderr:\n{}",
        compile.status,
        String::from_utf8_lossy(&compile.stderr)
    );

    let mut command = hew_command(&repo);
    command.arg("run").arg(&fixture);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        format!("hew run {}", fixture.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
    .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run run_lifecycle exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(stdout, expected, "run_lifecycle stdout mismatch");
}
