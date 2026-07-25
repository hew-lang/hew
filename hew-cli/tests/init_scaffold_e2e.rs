mod support;

use std::fs;
use std::process::Command;

use support::hew_binary;

fn run_hew(dir: &std::path::Path, args: &[&str]) -> std::process::Output {
    Command::new(hew_binary())
        .args(args)
        .current_dir(dir)
        .output()
        .unwrap()
}

/// Run `hew init <name>` in `dir` and return the output.
fn run_init(dir: &std::path::Path, name: &str) -> std::process::Output {
    run_hew(dir, &["init", name])
}

/// Run `hew check <file>` in `dir` and return the output.
fn run_check(dir: &std::path::Path, file: &str) -> std::process::Output {
    run_hew(dir, &["check", file])
}

#[test]
fn init_creates_main_hew() {
    let tmp = support::tempdir();
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
    let tmp = support::tempdir();
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
    let tmp = support::tempdir();
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
    let tmp = support::tempdir();
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

#[test]
fn init_without_name_creates_files_in_cwd() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init"]);

    assert!(
        out.status.success(),
        "hew init failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    assert!(
        tmp.path().join("main.hew").exists(),
        "hew init should create main.hew in the current directory"
    );
    assert!(
        tmp.path().join("README.md").exists(),
        "hew init should create README.md in the current directory"
    );

    let project_name = tmp
        .path()
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains(&format!("Created source-only project \"{project_name}\"")),
        "stdout should name the cwd project; got:\n{stdout}"
    );
}

#[test]
fn init_existing_empty_directory_without_force_succeeds() {
    let tmp = support::tempdir();
    let existing = tmp.path().join("existing");
    fs::create_dir(&existing).unwrap();

    let out = run_init(tmp.path(), "existing");

    assert!(
        out.status.success(),
        "hew init should succeed when the target directory exists but is empty:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    assert!(
        existing.join("main.hew").exists(),
        "hew init should write main.hew into the pre-existing empty directory"
    );
    assert!(
        existing.join("README.md").exists(),
        "hew init should write README.md into the pre-existing empty directory"
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
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains(&format!("Created source-only project \"{project_name}\"")),
        "hew init . should name the project after the target directory, matching `hew init` \
         with no argument; got:\n{stdout}"
    );
}

#[test]
fn init_existing_directory_with_real_main_hew_exits_one_naming_the_file() {
    let tmp = support::tempdir();
    let existing = tmp.path().join("existing");
    fs::create_dir(&existing).unwrap();
    fs::write(existing.join("main.hew"), "fn main() {}\n").unwrap();

    let out = run_init(tmp.path(), "existing");

    assert_eq!(
        out.status.code(),
        Some(1),
        "hew init should exit 1 when main.hew already exists in the target directory:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("main.hew"),
        "stderr should name the conflicting file, not the directory; got:\n{stderr}"
    );
    assert_eq!(
        fs::read_to_string(existing.join("main.hew")).unwrap(),
        "fn main() {}\n",
        "hew init should leave the pre-existing main.hew untouched when refusing to overwrite"
    );
}

#[test]
fn init_existing_scaffold_files_without_force_exit_one() {
    for file_name in ["main.hew", "README.md"] {
        let tmp = support::tempdir();
        let existing_path = tmp.path().join(file_name);
        fs::write(&existing_path, "sentinel").unwrap();

        let out = run_hew(tmp.path(), &["init"]);

        assert_eq!(
            out.status.code(),
            Some(1),
            "hew init should exit 1 when {file_name} already exists:\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );

        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains(&format!(
                "{file_name}' already exists (use --force to overwrite)"
            )),
            "stderr should explain how to recover for {file_name}; got:\n{stderr}"
        );
        assert!(
            !stderr.contains("already exist (use --force to overwrite)"),
            "a single conflict should use the singular verb; got:\n{stderr}"
        );
        assert_eq!(
            fs::read_to_string(&existing_path).unwrap(),
            "sentinel",
            "hew init should leave {file_name} untouched when refusing to overwrite"
        );
    }
}

#[test]
fn init_existing_scaffold_files_names_every_conflict_in_one_message() {
    let tmp = support::tempdir();
    fs::write(tmp.path().join("main.hew"), "sentinel main").unwrap();
    fs::write(tmp.path().join("README.md"), "sentinel readme").unwrap();

    let out = run_hew(tmp.path(), &["init"]);

    assert_eq!(
        out.status.code(),
        Some(1),
        "hew init should exit 1 when both scaffold files already exist:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("main.hew"),
        "stderr should name main.hew; got:\n{stderr}"
    );
    assert!(
        stderr.contains("README.md"),
        "stderr should name README.md; got:\n{stderr}"
    );
    assert!(
        stderr.contains("README.md' already exist (use --force to overwrite)"),
        "stderr should explain how to recover, with the verb agreeing with the two \
         conflicting files; got:\n{stderr}"
    );
    assert!(
        !stderr.contains("already exists"),
        "two conflicts should use the plural verb; got:\n{stderr}"
    );
}

#[test]
fn init_force_overwrites_existing_scaffold() {
    let tmp = support::tempdir();
    let main_hew = tmp.path().join("main.hew");
    let readme = tmp.path().join("README.md");
    fs::write(&main_hew, "old main").unwrap();
    fs::write(&readme, "old readme").unwrap();

    let out = run_hew(tmp.path(), &["init", "--force"]);

    assert!(
        out.status.success(),
        "hew init --force failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let main_src = fs::read_to_string(&main_hew).unwrap();
    assert_ne!(
        main_src, "old main",
        "main.hew should be overwritten by --force"
    );
    assert!(
        main_src.contains("fn main()"),
        "forced scaffold should restore the starter main.hew; got:\n{main_src}"
    );

    let readme_src = fs::read_to_string(&readme).unwrap();
    assert_ne!(
        readme_src, "old readme",
        "README.md should be overwritten by --force"
    );
    assert!(
        readme_src.contains("It does not create `hew.toml`"),
        "forced scaffold should restore the starter README; got:\n{readme_src}"
    );

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("--force replaced existing: main.hew, README.md"),
        "stdout should report which pre-existing files --force replaced; got:\n{stdout}"
    );
}

#[test]
fn init_force_into_empty_directory_prints_no_replacement_notice() {
    let tmp = support::tempdir();
    let out = run_hew(tmp.path(), &["init", "--force"]);

    assert!(
        out.status.success(),
        "hew init --force failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );

    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        !stdout.contains("--force replaced existing"),
        "stdout should not claim a replacement when no scaffold file pre-existed; got:\n{stdout}"
    );
}
