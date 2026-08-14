mod support;

use std::fs;
use std::process::Command;

use support::hew_binary;

/// Run `hew` in `dir` with an isolated HOME so a developer's real
/// package-manager config cannot leak defaults into the scaffold.
fn run_hew(dir: &std::path::Path, args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .current_dir(dir)
        .env("HOME", dir)
        .output()
        .unwrap()
}

/// Run `hew init <name>` in `dir` and return the output.
fn run_init(dir: &std::path::Path, name: &str) -> std::process::Output {
    run_hew(dir, &["init", name])
}

#[test]
fn init_creates_manifest_first_project() {
    let tmp = support::tempdir();
    let out = run_init(tmp.path(), "hello_world");

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let project_dir = tmp.path().join("hello_world");
    assert!(project_dir.exists(), "project directory was not created");
    assert!(
        project_dir.join("main.hew").exists(),
        "main.hew was not created"
    );

    let manifest = fs::read_to_string(project_dir.join("hew.toml")).unwrap();
    assert!(
        manifest.contains("name = \"hello_world\""),
        "hew.toml should carry the project name; got:\n{manifest}"
    );

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("Created hew.toml"),
        "stdout should announce the manifest; got:\n{stdout}"
    );
}

#[test]
fn init_scaffold_has_no_typed_return_on_main() {
    let tmp = support::tempdir();
    run_init(tmp.path(), "typed_check");

    let src = fs::read_to_string(tmp.path().join("typed_check").join("main.hew")).unwrap();

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
    let tmp = support::tempdir();
    run_init(tmp.path(), "check_test");

    let project_dir = tmp.path().join("check_test");
    let out = run_hew(&project_dir, &["check", "main.hew"]);

    assert!(
        out.status.success(),
        "`hew check main.hew` failed on freshly-generated scaffold:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}

#[test]
fn init_without_name_creates_project_in_cwd() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init"]);

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        tmp.path().join("hew.toml").exists(),
        "hew init should create hew.toml in the current directory"
    );
    assert!(
        tmp.path().join("main.hew").exists(),
        "hew init should create main.hew in the current directory"
    );

    let project_name = tmp
        .path()
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap();
    let manifest = fs::read_to_string(tmp.path().join("hew.toml")).unwrap();
    assert!(
        manifest.contains(&format!("name = \"{project_name}\"")),
        "hew.toml should name the cwd project; got:\n{manifest}"
    );
}

#[test]
fn init_dot_and_no_arg_produce_the_same_project_name() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "."]);

    assert!(
        out.status.success(),
        "hew init . failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let project_name = tmp
        .path()
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap();
    let manifest = fs::read_to_string(tmp.path().join("hew.toml")).unwrap();
    assert!(
        manifest.contains(&format!("name = \"{project_name}\"")),
        "hew init . should name the project after the target directory, matching `hew init` \
         with no argument; got:\n{manifest}"
    );
}

#[test]
fn init_dot_normalizes_hyphenated_package_name_without_renaming_directory() {
    let tmp = support::tempdir();
    let project_dir = tmp.path().join("config-telemetry");
    fs::create_dir(&project_dir).unwrap();

    let out = run_hew(&project_dir, &["init", "."]);

    assert!(
        out.status.success(),
        "hew init . failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    let manifest = fs::read_to_string(project_dir.join("hew.toml")).unwrap();
    assert!(
        manifest.contains("name = \"config_telemetry\""),
        "hew.toml should use an importable package identifier; got:\n{manifest}"
    );
    assert!(
        project_dir.exists(),
        "hew init must keep the hyphenated directory name"
    );
    assert!(
        !tmp.path().join("config_telemetry").exists(),
        "hew init must not create a renamed sibling directory"
    );
}

#[test]
fn init_refuses_existing_manifest_untouched() {
    let tmp = support::tempdir();
    fs::write(tmp.path().join("hew.toml"), "sentinel manifest").unwrap();

    let out = run_hew(tmp.path(), &["init"]);

    assert_eq!(
        out.status.code(),
        Some(1),
        "hew init should exit 1 when hew.toml already exists:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("hew.toml already exists"),
        "stderr should name the conflicting manifest; got:\n{stderr}"
    );
    assert_eq!(
        fs::read_to_string(tmp.path().join("hew.toml")).unwrap(),
        "sentinel manifest",
        "hew init must leave the pre-existing hew.toml untouched"
    );
}

#[test]
fn init_preserves_existing_source_files() {
    let tmp = support::tempdir();
    fs::write(tmp.path().join("main.hew"), "// existing\n").unwrap();

    let out = run_hew(tmp.path(), &["init"]);

    assert!(
        out.status.success(),
        "hew init should succeed beside an existing main.hew:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    assert_eq!(
        fs::read_to_string(tmp.path().join("main.hew")).unwrap(),
        "// existing\n",
        "hew init must never overwrite an existing source file"
    );
    assert!(
        tmp.path().join("hew.toml").exists(),
        "hew init should still create the manifest beside the preserved source"
    );
}

#[test]
fn init_merges_existing_gitignore() {
    let tmp = support::tempdir();
    fs::write(tmp.path().join(".gitignore"), "*.o\n").unwrap();

    let out = run_hew(tmp.path(), &["init"]);

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let gi = fs::read_to_string(tmp.path().join(".gitignore")).unwrap();
    assert!(
        gi.contains("*.o"),
        "existing entries must be kept; got:\n{gi}"
    );
    assert!(
        gi.contains("target/"),
        ".gitignore should gain the build-dir entry; got:\n{gi}"
    );
}

#[test]
fn init_lib_scaffolds_lib_hew() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "mylib", "--lib"]);

    assert!(
        out.status.success(),
        "hew init --lib failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let project_dir = tmp.path().join("mylib");
    assert!(
        project_dir.join("lib.hew").exists(),
        "lib.hew was not created"
    );
    assert!(
        !project_dir.join("main.hew").exists(),
        "a library scaffold must not create main.hew"
    );
}

#[test]
fn init_actor_scaffolds_actor_main() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "myactor", "--actor"]);

    assert!(
        out.status.success(),
        "hew init --actor failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let src = fs::read_to_string(tmp.path().join("myactor").join("main.hew")).unwrap();
    assert!(
        src.contains("actor Counter"),
        "actor scaffold should contain an actor definition; got:\n{src}"
    );
}

#[test]
fn init_lib_and_actor_conflict_is_rejected() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "--lib", "--actor"]);

    assert_eq!(
        out.status.code(),
        Some(2),
        "conflicting template flags should be a usage error:\nstderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );
}

#[test]
fn init_force_flag_is_rejected() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "--force"]);

    assert_eq!(
        out.status.code(),
        Some(2),
        "hew init must not accept a destructive --force flag:\nstderr: {}",
        String::from_utf8_lossy(&out.stderr),
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--force"),
        "stderr should name the rejected flag; got:\n{stderr}"
    );
}
