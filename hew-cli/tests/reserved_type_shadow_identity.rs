mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, tempdir};

const SOURCE: &str = r#"
type Task<T> { value: T }
type Unit<T> { value: T }

fn task_value(task: Task<i64>) -> i64 {
    task.value
}

fn unit_value(unit: Unit<i64>) -> i64 {
    unit.value
}

fn main() {
    let task = Task<i64> { value: 17 };
    let unit = Unit<i64> { value: 25 };
    if task_value(task) + unit_value(unit) != 42 {
        panic("reserved-name source identities were not preserved");
    }
}
"#;

fn source_path() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempdir();
    let path = dir.path().join("reserved_type_shadows.hew");
    std::fs::write(&path, SOURCE).expect("write reserved-name shadow source");
    (dir, path)
}

fn package_imported_source_path() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempdir();
    let module_source = r"
pub type Task<T> { value: T }
pub type Unit<T> { value: T }

pub fn shadow_sum(task: Task<i64>, unit: Unit<i64>) -> i64 {
    task.value + unit.value
}
";
    let package = dir.path().join("hew/foo");
    std::fs::create_dir_all(&package).expect("create package");
    std::fs::write(
        package.join("hew.toml"),
        "[package]\nname = \"hew::foo\"\nversion = \"0.1.0\"\n",
    )
    .expect("write package manifest");
    std::fs::write(package.join("foo.hew"), module_source).expect("write package source");
    let path = dir.path().join("main.hew");
    std::fs::write(
        &path,
        r#"import hew.foo.{ Task, Unit, shadow_sum };

fn main() {
    let task = Task<i64> { value: 17 };
    let unit = Unit<i64> { value: 25 };
    if shadow_sum(task, unit) != 42 {
        panic("imported reserved-name source identities were not preserved");
    }
}
"#,
    )
    .expect("write imported reserved-name source");
    (dir, path)
}

fn assert_package_imported_shadows_check_and_run() {
    require_codegen();

    let (_dir, path) = package_imported_source_path();
    let output = Command::new(hew_binary())
        .args(["check", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("check imported reserved-name source");
    assert!(
        output.status.success(),
        "package-imported Task<T>/Unit<T> source shadows must pass checking:\n{}",
        describe_output(&output)
    );

    let output = Command::new(hew_binary())
        .args(["run", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("run imported reserved-name source");
    assert!(
        output.status.success(),
        "package-imported Task<T>/Unit<T> identities must survive native execution:\n{}",
        describe_output(&output)
    );
}

#[test]
fn generic_task_and_unit_source_shadows_pass_check() {
    let (_dir, path) = source_path();
    let output = Command::new(hew_binary())
        .args(["check", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("run hew check");
    assert!(
        output.status.success(),
        "generic source Task<T>/Unit<T> declarations must outrank compiler early arms:\n{}",
        describe_output(&output)
    );
}

#[test]
fn generic_task_and_unit_source_shadows_run() {
    require_codegen();

    let (_dir, path) = source_path();
    let output = Command::new(hew_binary())
        .args(["run", path.to_str().expect("utf-8 source path")])
        .current_dir(repo_root())
        .output()
        .expect("run reserved-name shadow source");
    assert!(
        output.status.success(),
        "generic source Task<T>/Unit<T> identities must survive HIR, MIR, and native execution:\n{}",
        describe_output(&output)
    );
}

#[test]
fn package_named_imported_task_and_unit_shadows_check_and_run() {
    assert_package_imported_shadows_check_and_run();
}
