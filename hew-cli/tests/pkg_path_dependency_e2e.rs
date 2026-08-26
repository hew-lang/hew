mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

fn output_text(output: &Output) -> (String, String) {
    (
        String::from_utf8(output.stdout.clone()).expect("stdout is UTF-8"),
        String::from_utf8(output.stderr.clone()).expect("stderr is UTF-8"),
    )
}

fn json_diagnostics(output: &Output) -> Vec<serde_json::Value> {
    serde_json::from_slice(&output.stdout).unwrap_or_else(|error| {
        panic!(
            "stdout must be a JSON diagnostics array: {error}\n{}",
            describe_output(output)
        )
    })
}

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

#[expect(
    clippy::too_many_lines,
    reason = "one linear first-run transcript keeps every command and full-output assertion in user order"
)]
fn assert_init_lib_path_dependency_first_run(package: &str, root_source: &str) {
    let workspace = support::tempdir();
    let home = workspace.path().join("home");
    std::fs::create_dir_all(&home).unwrap();

    let init_lib = run_pkg(&home, workspace.path(), &["init", package, "--lib"]);
    assert!(init_lib.status.success(), "{}", describe_output(&init_lib));
    let (stdout, stderr) = output_text(&init_lib);
    assert_eq!(
        stdout,
        format!(
            "Created hew.toml: {package} v0.1.0: A Hew library\n\
             Scaffolded {root_source} and .gitignore\n\
             Next: `hew check {root_source}`\n"
        )
    );
    assert_eq!(stderr, "");

    let library = workspace.path().join(package);
    let manifest = std::fs::read_to_string(library.join("hew.toml")).unwrap();
    assert!(
        manifest.contains(&format!("main = \"{root_source}\"")),
        "manifest/root mismatch:\n{manifest}"
    );
    let library_check = run_pkg(&home, &library, &["check"]);
    assert!(
        library_check.status.success(),
        "{}",
        describe_output(&library_check)
    );
    let (stdout, stderr) = output_text(&library_check);
    assert_eq!(stdout, "");
    let separator = std::path::MAIN_SEPARATOR;
    assert!(
        stderr.ends_with(&format!("{package}{separator}{root_source}: OK\n")),
        "{stderr}"
    );

    let init_consumer = run_pkg(&home, workspace.path(), &["init", "consumer"]);
    assert!(
        init_consumer.status.success(),
        "{}",
        describe_output(&init_consumer)
    );
    let consumer = workspace.path().join("consumer");
    let module_alias = package.rsplit('.').next().unwrap();
    std::fs::write(
        consumer.join("main.hew"),
        format!("import {package};\n\nfn main() {{ println({module_alias}.add(2, 3)); }}\n"),
    )
    .unwrap();

    let dependency_path = format!("../{package}");
    let add = run_pkg(
        &home,
        &consumer,
        &["add", package, "--path", &dependency_path],
    );
    assert!(add.status.success(), "{}", describe_output(&add));
    assert_eq!(
        output_text(&add),
        (
            format!("Added {package} from {dependency_path} to hew.toml\n"),
            String::new()
        )
    );

    let install = run_pkg(&home, &consumer, &["install"]);
    assert!(install.status.success(), "{}", describe_output(&install));
    let materialized = if cfg!(unix) { "linked" } else { "copied" };
    assert_eq!(
        output_text(&install),
        (
            format!("  {materialized} {package}@0.1.0\nWrote hew.lock\n"),
            String::new()
        )
    );
    let installed = package
        .split('.')
        .fold(consumer.join(".hew/packages"), |path, segment| {
            path.join(segment)
        });
    assert!(
        installed.is_dir(),
        "missing installed package at {}",
        installed.display()
    );
    assert!(installed.join(root_source).is_file());

    let text_check = run_pkg(&home, &consumer, &["check"]);
    assert!(
        text_check.status.success(),
        "{}",
        describe_output(&text_check)
    );
    let (stdout, stderr) = output_text(&text_check);
    assert_eq!(stdout, "");
    assert!(
        stderr.ends_with(&format!("consumer{separator}main.hew: OK\n")),
        "{stderr}"
    );

    let json_check = run_pkg(&home, &consumer, &["check", "--format=json"]);
    assert!(
        json_check.status.success(),
        "{}",
        describe_output(&json_check)
    );
    assert_eq!(
        output_text(&json_check),
        ("[]\n".to_string(), String::new())
    );

    let locked = run_pkg(&home, &consumer, &["install", "--locked", "--offline"]);
    assert!(locked.status.success(), "{}", describe_output(&locked));
    assert_eq!(
        output_text(&locked),
        (
            format!(
                "  {materialized} {package}@0.1.0\nUsed locked dependency graph\n"
            ),
            format!(
                "Offline mode: using cached packages from {}; the registry will not be contacted.\n",
                home.join(".hew/packages").display()
            )
        )
    );

    let reused = run_pkg(&home, &consumer, &["check", "--format=json"]);
    assert!(reused.status.success(), "{}", describe_output(&reused));
    assert_eq!(output_text(&reused), ("[]\n".to_string(), String::new()));
}

#[test]
fn init_lib_path_dependency_first_run_and_locked_offline_reuse() {
    assert_init_lib_path_dependency_first_run("local_dep", "local_dep.hew");
}

#[test]
fn dotted_init_lib_path_dependency_first_run_and_locked_offline_reuse() {
    assert_init_lib_path_dependency_first_run("hew.selfqualtype", "selfqualtype.hew");
}

fn install_invalid_library_root(workspace: &Path, home: &Path) -> std::path::PathBuf {
    let dependency = workspace.join("local_dep");
    let consumer = workspace.join("consumer");
    std::fs::create_dir_all(&dependency).unwrap();
    std::fs::create_dir_all(&consumer).unwrap();
    std::fs::write(
        dependency.join("hew.toml"),
        "[package]\nname = \"local_dep\"\nversion = \"0.1.0\"\nedition = \"2026\"\nmain = \"local_dep.hew\"\n",
    )
    .unwrap();
    // Deliberately use the obsolete scaffold spelling. The package has a
    // manifest and installs successfully, but no canonical import root.
    std::fs::write(
        dependency.join("lib.hew"),
        "pub fn answer() -> i64 { 42 }\n",
    )
    .unwrap();
    write_manifest(&consumer, "");
    std::fs::write(
        consumer.join("main.hew"),
        "import local_dep;\nfn main() {}\n",
    )
    .unwrap();
    let add = run_pkg(
        home,
        &consumer,
        &["add", "local_dep", "--path", "../local_dep"],
    );
    assert!(add.status.success(), "{}", describe_output(&add));
    let install = run_pkg(home, &consumer, &["install"]);
    assert!(install.status.success(), "{}", describe_output(&install));
    consumer
}

#[test]
fn missing_canonical_library_root_has_actionable_text_json_parity() {
    let workspace = support::tempdir();
    let home = workspace.path().join("home");
    std::fs::create_dir_all(&home).unwrap();
    let consumer = install_invalid_library_root(workspace.path(), &home);

    let text = run_pkg(&home, &consumer, &["check"]);
    assert_eq!(text.status.code(), Some(1), "{}", describe_output(&text));
    let (text_stdout, text_stderr) = output_text(&text);
    assert_eq!(text_stdout, "");
    assert!(text_stderr.contains("has no canonical root module `local_dep.hew`"));
    assert!(text_stderr.contains("a library package exposes `<package-name>.hew`"));
    assert!(text_stderr.contains("hew init --lib local_dep"));

    let json = run_pkg(&home, &consumer, &["check", "--format=json"]);
    assert_eq!(json.status.code(), Some(1), "{}", describe_output(&json));
    assert_eq!(String::from_utf8(json.stderr.clone()).unwrap(), "");
    let diagnostics = json_diagnostics(&json);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
    assert_eq!(diagnostics[0]["code"], "E_PACKAGE_ROOT_MISSING");
    assert_eq!(diagnostics[0]["severity"], "error");
    assert_eq!(
        diagnostics[0]["message"].as_str().unwrap(),
        text_stderr.trim_end(),
        "text and JSON must carry the same actionable diagnostic"
    );
}

#[test]
fn local_and_installed_package_roots_are_ambiguous_in_text_and_json() {
    let workspace = support::tempdir();
    let home = workspace.path().join("home");
    std::fs::create_dir_all(&home).unwrap();
    let init_lib = run_pkg(&home, workspace.path(), &["init", "local_dep", "--lib"]);
    assert!(init_lib.status.success(), "{}", describe_output(&init_lib));
    let consumer = workspace.path().join("consumer");
    std::fs::create_dir_all(&consumer).unwrap();
    write_manifest(&consumer, "");
    std::fs::write(
        consumer.join("main.hew"),
        "import local_dep;\nfn main() {}\n",
    )
    .unwrap();
    std::fs::write(
        consumer.join("local_dep.hew"),
        "pub fn local_answer() -> i64 { 1 }\n",
    )
    .unwrap();
    let add = run_pkg(
        &home,
        &consumer,
        &["add", "local_dep", "--path", "../local_dep"],
    );
    assert!(add.status.success(), "{}", describe_output(&add));
    let install = run_pkg(&home, &consumer, &["install"]);
    assert!(install.status.success(), "{}", describe_output(&install));

    let text = run_pkg(&home, &consumer, &["check"]);
    assert_eq!(text.status.code(), Some(1), "{}", describe_output(&text));
    let (text_stdout, text_stderr) = output_text(&text);
    assert_eq!(text_stdout, "");
    assert!(text_stderr.contains("import `local_dep` is ambiguous"));
    assert!(text_stderr.contains("Rename or remove one"));

    let json = run_pkg(&home, &consumer, &["check", "--format=json"]);
    assert_eq!(json.status.code(), Some(1), "{}", describe_output(&json));
    assert_eq!(String::from_utf8(json.stderr.clone()).unwrap(), "");
    let diagnostics = json_diagnostics(&json);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:#?}");
    assert_eq!(diagnostics[0]["code"], "E_IMPORT_AMBIGUOUS");
    assert_eq!(
        diagnostics[0]["message"].as_str().unwrap(),
        text_stderr.trim_end()
    );
}
