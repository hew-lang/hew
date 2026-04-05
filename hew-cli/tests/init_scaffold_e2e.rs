use std::path::PathBuf;
use std::process::Command;

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

/// Run `hew init <name>` in `dir` and return the output.
fn run_init(dir: &std::path::Path, name: &str) -> std::process::Output {
    Command::new(hew_binary())
        .args(["init", name])
        .current_dir(dir)
        .output()
        .unwrap()
}

/// Run `hew check <file>` in `dir` and return the output.
fn run_check(dir: &std::path::Path, file: &str) -> std::process::Output {
    Command::new(hew_binary())
        .args(["check", file])
        .current_dir(dir)
        .output()
        .unwrap()
}

#[test]
fn init_creates_main_hew() {
    let tmp = tempfile::tempdir().unwrap();
    let out = run_init(tmp.path(), "hello_world");

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let project_dir = tmp.path().join("hello_world");
    assert!(project_dir.exists(), "project directory was not created");

    let main_hew = project_dir.join("main.hew");
    assert!(main_hew.exists(), "main.hew was not created");
}

#[test]
fn init_scaffold_has_no_typed_return_on_main() {
    let tmp = tempfile::tempdir().unwrap();
    run_init(tmp.path(), "typed_check");

    let src = std::fs::read_to_string(tmp.path().join("typed_check").join("main.hew")).unwrap();

    // The starter must not declare `-> i32` (or any return type) on main —
    // a bare integer literal defaults to i64 which would cause a type mismatch.
    assert!(
        !src.contains("-> i32"),
        "init scaffold must not use `-> i32` on fn main; got:\n{src}"
    );
    assert!(
        !src.contains("-> i64"),
        "init scaffold must not use `-> i64` on fn main; got:\n{src}"
    );
}

#[test]
fn init_scaffold_passes_hew_check() {
    let tmp = tempfile::tempdir().unwrap();
    run_init(tmp.path(), "check_test");

    let project_dir = tmp.path().join("check_test");
    let out = run_check(&project_dir, "main.hew");

    assert!(
        out.status.success(),
        "`hew check main.hew` failed on freshly-generated scaffold:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}

#[test]
fn init_scaffold_stays_source_only_and_points_to_adze_init() {
    let tmp = tempfile::tempdir().unwrap();
    let out = run_init(tmp.path(), "docs_test");

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let project_dir = tmp.path().join("docs_test");
    assert!(
        !project_dir.join("hew.toml").exists(),
        "hew init must remain source-only and not create hew.toml"
    );

    let readme = std::fs::read_to_string(project_dir.join("README.md")).unwrap();
    assert!(
        readme.contains("It does not create `hew.toml`"),
        "README should explain that hew init is source-only; got:\n{readme}"
    );
    assert!(
        readme.contains("adze init"),
        "README should point manifest-first onboarding at adze init; got:\n{readme}"
    );
    assert!(
        readme.contains("hew check main.hew"),
        "README should recommend `hew check main.hew`; got:\n{readme}"
    );
    assert!(
        readme.contains("hew run main.hew"),
        "README should recommend `hew run main.hew`; got:\n{readme}"
    );

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("Created source-only project"),
        "stdout should describe the source-only scaffold; got:\n{stdout}"
    );
    assert!(
        stdout.contains("No hew.toml was created"),
        "stdout should explain the missing manifest and point to adze init; got:\n{stdout}"
    );
}
