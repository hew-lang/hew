mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

fn write_manifest(root: &Path, dependencies: &str) {
    std::fs::write(
        root.join("hew.toml"),
        format!(
            "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\n[dependencies]\n{dependencies}"
        ),
    )
    .expect("write project manifest");
}

fn run_pkg(home: &Path, project: &Path, args: &[&str]) -> Output {
    Command::new(hew_binary())
        .args(args)
        .current_dir(project)
        .env("HOME", home)
        .env_remove("USERPROFILE")
        .output()
        .expect("run hew package command")
}

#[test]
fn pkg_path_dependency_adds_installs_and_builds() {
    require_codegen();
    let workspace = tempfile::Builder::new()
        .prefix("pkg-path-e2e-")
        .tempdir_in(repo_root())
        .expect("create package workspace");
    let home = workspace.path().join("home");
    let project = workspace.path().join("app");
    let dependency = workspace.path().join("foo");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&project).unwrap();
    std::fs::create_dir_all(&dependency).unwrap();
    write_manifest(&project, "");
    std::fs::write(
        project.join("main.hew"),
        "import foo;\nfn main() -> i64 { foo.answer() }\n",
    )
    .unwrap();
    std::fs::write(
        dependency.join("hew.toml"),
        "[package]\nname = \"foo\"\nversion = \"1.4.0\"\nedition = \"2026\"\n",
    )
    .unwrap();
    std::fs::write(
        dependency.join("foo.hew"),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .unwrap();

    let add = run_pkg(&home, &project, &["add", "foo", "--path", "../foo"]);
    assert!(
        add.status.success(),
        "hew add --path failed\n{}",
        describe_output(&add)
    );
    let install = run_pkg(&home, &project, &["install"]);
    assert!(
        install.status.success(),
        "path install failed\n{}",
        describe_output(&install)
    );
    let lock = std::fs::read_to_string(project.join("hew.lock")).unwrap();
    assert!(lock.contains("source = \"path\""), "{lock}");
    assert!(lock.contains("path = \"../foo\""), "{lock}");

    let mut check = Command::new(hew_binary());
    check
        .arg("check")
        .current_dir(&project)
        .env("HOME", &home)
        .env_remove("USERPROFILE");
    let output = run_bounded_command(check, "hew check path dependency");
    assert!(
        output.status.success(),
        "path dependency check failed\n{}",
        describe_output(&output)
    );

    let mut build = Command::new(hew_binary());
    build
        .arg("build")
        .current_dir(&project)
        .env("HOME", &home)
        .env_remove("USERPROFILE");
    let output = run_bounded_command(build, "hew build path dependency");
    assert!(
        output.status.success(),
        "path dependency build failed\n{}",
        describe_output(&output)
    );
}

#[test]
fn pkg_missing_path_refuses_install() {
    let root = support::tempdir();
    let home = root.path().join("home");
    let project = root.path().join("app");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&project).unwrap();
    write_manifest(&project, "missing = { path = \"../missing\" }\n");

    let output = run_pkg(&home, &project, &["install"]);
    assert!(
        !output.status.success(),
        "missing path must fail\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("missing"), "{stderr}");
    assert!(
        stderr.contains("../missing") || stderr.contains("/missing"),
        "{stderr}"
    );
}
