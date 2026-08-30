mod support;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::hew_binary;

fn run_hew(dir: &Path, args: &[&str]) -> Output {
    Command::new(hew_binary())
        .args(args)
        .current_dir(dir)
        .env("HOME", dir)
        .env("USERPROFILE", dir)
        .output()
        .unwrap()
}

fn init_project(parent: &Path) -> PathBuf {
    let output = run_hew(parent, &["init", "fmt_project"]);
    assert!(
        output.status.success(),
        "hew init failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    parent.join("fmt_project")
}

fn assert_output(output: &Output, code: i32, stdout: &str, stderr: &str) {
    assert_eq!(
        output.status.code(),
        Some(code),
        "unexpected exit status\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), stdout);
    assert_eq!(String::from_utf8_lossy(&output.stderr), stderr);
}

#[test]
fn fmt_check_dot_accepts_a_fresh_init_project_silently() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());

    let output = run_hew(&project, &["fmt", "--check", "."]);

    assert_output(&output, 0, "", "");
}

#[test]
fn fmt_check_directory_reports_drift_in_deterministic_order_without_writing() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());
    fs::create_dir(project.join("nested")).unwrap();

    let main_source = "fn main() { let value = 1; }\n";
    let alpha_source = "fn alpha() { let value = 2; }\n";
    let zeta_source = "fn zeta() { let value = 3; }\n";
    fs::write(project.join("main.hew"), main_source).unwrap();
    fs::write(project.join("nested/alpha.hew"), alpha_source).unwrap();
    fs::write(project.join("nested/zeta.hew"), zeta_source).unwrap();

    let output = run_hew(&project, &["fmt", "--check", "."]);

    let expected = [
        Path::new(".").join("main.hew"),
        Path::new(".").join("nested").join("alpha.hew"),
        Path::new(".").join("nested").join("zeta.hew"),
    ]
    .map(|path| format!("{}: needs formatting\n", path.display()))
    .concat();
    assert_output(&output, 1, "", &expected);
    assert_eq!(
        fs::read_to_string(project.join("main.hew")).unwrap(),
        main_source
    );
    assert_eq!(
        fs::read_to_string(project.join("nested/alpha.hew")).unwrap(),
        alpha_source
    );
    assert_eq!(
        fs::read_to_string(project.join("nested/zeta.hew")).unwrap(),
        zeta_source
    );
}

#[test]
fn fmt_directory_repairs_every_source_then_check_passes() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());
    fs::create_dir(project.join("nested")).unwrap();
    fs::write(
        project.join("nested/zeta.hew"),
        "fn zeta() { let value = 3; }\n",
    )
    .unwrap();
    fs::write(
        project.join("nested/alpha.hew"),
        "fn alpha() { let value = 2; }\n",
    )
    .unwrap();

    let output = run_hew(&project, &["fmt", "."]);

    let expected = [
        Path::new(".").join("nested").join("alpha.hew"),
        Path::new(".").join("nested").join("zeta.hew"),
    ]
    .map(|path| format!("Formatted {}\n", path.display()))
    .concat();
    assert_output(&output, 0, "", &expected);
    assert_eq!(
        fs::read_to_string(project.join("nested/alpha.hew")).unwrap(),
        "fn alpha() {\n    let value = 2;\n}\n"
    );
    assert_eq!(
        fs::read_to_string(project.join("nested/zeta.hew")).unwrap(),
        "fn zeta() {\n    let value = 3;\n}\n"
    );

    let check = run_hew(&project, &["fmt", "--check", "."]);
    assert_output(&check, 0, "", "");
}

#[test]
fn fmt_directory_excludes_metadata_and_build_dirs_but_direct_file_still_works() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());
    for dir in [".git", ".hew", "target"] {
        fs::create_dir(project.join(dir)).unwrap();
    }
    let broken = "fn broken( {\n";
    fs::write(project.join(".git/ignored.hew"), broken).unwrap();
    fs::write(project.join(".hew/cached.hew"), broken).unwrap();
    let generated = "fn generated() { let value = 1; }\n";
    fs::write(project.join("target/generated.hew"), generated).unwrap();

    let directory_check = run_hew(&project, &["fmt", "--check", "."]);
    assert_output(&directory_check, 0, "", "");
    assert_eq!(
        fs::read_to_string(project.join("target/generated.hew")).unwrap(),
        generated
    );

    let direct = run_hew(&project, &["fmt", "target/generated.hew"]);
    assert_output(&direct, 0, "", "Formatted target/generated.hew\n");
    assert_eq!(
        fs::read_to_string(project.join("target/generated.hew")).unwrap(),
        "fn generated() {\n    let value = 1;\n}\n"
    );
    assert_eq!(
        fs::read_to_string(project.join(".git/ignored.hew")).unwrap(),
        broken
    );
    assert_eq!(
        fs::read_to_string(project.join(".hew/cached.hew")).unwrap(),
        broken
    );
}

#[test]
fn fmt_directory_deduplicates_overlapping_and_aliased_inputs() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());
    fs::create_dir(project.join("nested")).unwrap();
    let source = "fn alpha() { let value = 1; }\n";
    fs::write(project.join("nested/alpha.hew"), source).unwrap();

    let output = run_hew(
        &project,
        &[
            "fmt",
            "--check",
            ".",
            "nested",
            "nested/alpha.hew",
            "./nested/alpha.hew",
        ],
    );

    let expected = format!(
        "{}: needs formatting\n",
        Path::new(".").join("nested").join("alpha.hew").display()
    );
    assert_output(&output, 1, "", &expected);
    assert_eq!(
        fs::read_to_string(project.join("nested/alpha.hew")).unwrap(),
        source
    );
}

#[test]
fn fmt_missing_and_empty_directory_inputs_are_actionable() {
    let tmp = support::tempdir();

    let missing = run_hew(tmp.path(), &["fmt", "missing.hew"]);
    assert_output(
        &missing,
        1,
        "",
        "Error: format input `missing.hew` does not exist\n",
    );

    fs::create_dir(tmp.path().join("empty")).unwrap();
    let empty = run_hew(tmp.path(), &["fmt", "empty"]);
    assert_output(
        &empty,
        1,
        "",
        "Error: format directory `empty` contains no .hew source files\n",
    );
}

#[test]
fn fmt_migrate_directory_input_points_to_root_mode() {
    let tmp = support::tempdir();
    let project = init_project(tmp.path());

    let output = run_hew(&project, &["fmt", "--migrate", "."]);

    assert_output(
        &output,
        1,
        "",
        "Error: directory migration input `.` requires `--root`; run `hew fmt --migrate --root .`\n",
    );
}
