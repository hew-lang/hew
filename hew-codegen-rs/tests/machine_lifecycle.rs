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
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let lib = target_dir(repo).join("debug").join("libhew.a");
        if lib.exists() {
            return;
        }

        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
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
