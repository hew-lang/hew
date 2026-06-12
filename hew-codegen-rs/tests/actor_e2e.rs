use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
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

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo)
        .join("debug")
        .join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn hew_lib(repo: &Path) -> PathBuf {
    let lib_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
    target_dir(repo).join("debug").join(lib_name)
}

fn ensure_codegen_artifacts(repo: &Path) {
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        let hew = hew_bin(repo);
        let lib = hew_lib(repo);
        if hew.is_file() && lib.is_file() {
            return;
        }

        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let output = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-cli", "-p", "hew-lib"])
            .output()
            .expect("spawn cargo build -p hew-cli -p hew-lib");
        assert!(
            output.status.success(),
            "cargo build -p hew-cli -p hew-lib failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        assert!(
            hew.is_file(),
            "codegen bootstrap succeeded but hew binary was not created at {}",
            hew.display()
        );
        assert!(
            lib.is_file(),
            "codegen bootstrap succeeded but Hew library was not created at {}",
            lib.display()
        );
    });
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
    let repo = repo_root();
    ensure_codegen_artifacts(&repo);

    let emit_dir = tempfile::Builder::new()
        .prefix(&format!("hew-actor-e2e-{fixture_name}-"))
        .tempdir()
        .expect("create actor fixture emit dir");

    let compile = Command::new(hew_bin(&repo))
        .current_dir(&repo)
        .args([
            "compile",
            "--emit-dir",
            emit_dir
                .path()
                .to_str()
                .expect("emit dir path is valid UTF-8"),
            &format!("examples/v05/{fixture_name}.hew"),
        ])
        .output()
        .expect("run built hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let stdout = String::from_utf8_lossy(&compile.stdout);
    let binary = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            panic!("hew compile stdout did not contain a `native:` line:\n{stdout}")
        });
    assert!(
        binary.is_file(),
        "hew compile reported native binary {}, but it does not exist",
        binary.display()
    );

    let status = run_bounded(&binary, 30);
    assert_eq!(status.code(), Some(expected_exit_code));
}
