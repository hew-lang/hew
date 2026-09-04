//! Package-mode `hew build` / `hew run` / `hew check`.
//!
//! With no argument, or with a directory argument, all three resolve the
//! enclosing package through `hew.toml`: `[package] main` names the entry
//! point, `[native]` is built first as a prerequisite, and the binary is named
//! after the package and written to `<root>/target/<profile>/`, cargo-style.
//! The explicit-file form is unchanged: no package root means no `target/` to
//! own, so the binary lands beside the source. A directory is resolved before
//! any path reaches the compiler, so no raw OS error can escape.

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

/// Package builds resolve `libhew.a` by walking up from the working directory,
/// so fixtures live under the repo root like every other linking test.
fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("package-mode-hew-")
        .tempdir_in(repo_root())
        .expect("temp dir")
}

const GREETING: &str = "package-mode-ok";

fn manifest(name: &str, extra: &str) -> String {
    format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\nedition = \"2026\"\n{extra}")
}

/// Write a package rooted at `dir` whose entry point prints [`GREETING`].
fn write_package(dir: &Path, name: &str, entry: &str, extra: &str) {
    std::fs::write(dir.join("hew.toml"), manifest(name, extra)).expect("write hew.toml");
    let entry_path = dir.join(entry);
    if let Some(parent) = entry_path.parent() {
        std::fs::create_dir_all(parent).expect("create entry directory");
    }
    std::fs::write(
        &entry_path,
        format!("fn main() {{\n    println(\"{GREETING}\")\n}}\n"),
    )
    .expect("write entry");
}

fn binary_in(dir: &Path, name: &str) -> PathBuf {
    dir.join(format!("{name}{}", std::env::consts::EXE_SUFFIX))
}

/// Where a package build writes its binary by default: `<root>/target/<profile>/<name>`.
fn package_binary_in(root: &Path, profile: &str, name: &str) -> PathBuf {
    root.join("target")
        .join(profile)
        .join(format!("{name}{}", std::env::consts::EXE_SUFFIX))
}

fn assert_prints_greeting(binary: &Path) {
    let run = run_bounded_command(Command::new(binary), format!("run {}", binary.display()));
    assert!(
        run.status.success(),
        "package binary exited non-zero: {:?}\nstderr: {}",
        run.status,
        String::from_utf8_lossy(&run.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout).trim(),
        GREETING,
        "package binary printed the wrong output",
    );
}

/// `hew build` with no argument, inside a package, compiles the manifest's
/// entry point and names the binary after the package.
#[test]
fn build_no_argument_produces_the_package_named_binary() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "greeter", "main.hew", "");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "package build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "greeter"));
}

/// `hew build .` names the current directory's package explicitly and behaves
/// identically to the no-argument form — `.` is never read as source.
#[test]
fn build_dot_builds_the_current_package() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "dotpkg", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "."])
        .current_dir(dir.path())
        .output()
        .expect("run hew build .");

    assert!(
        output.status.success(),
        "`hew build .` failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "dotpkg"));
}

/// `hew build <dir>` builds that package and writes its binary into the package
/// root, not the invoking directory.
#[test]
fn build_directory_argument_writes_into_that_package_root() {
    require_codegen();
    let dir = workspace();
    let pkg = dir.path().join("nested");
    std::fs::create_dir(&pkg).expect("create package dir");
    write_package(&pkg, "nested", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "nested"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build nested");

    assert!(
        output.status.success(),
        "directory build failed\n{}",
        describe_output(&output),
    );
    assert!(
        !binary_in(dir.path(), "nested").is_file(),
        "binary should live in the package root, not the invoking directory",
    );
    assert_prints_greeting(&package_binary_in(&pkg, "debug", "nested"));
}

/// `hew run` with no argument compiles and runs the package's entry point.
#[test]
fn run_no_argument_runs_the_package_entry_point() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "runner", "main.hew", "");

    let mut command = Command::new(hew_binary());
    command.arg("run").current_dir(dir.path());
    let output = run_bounded_command(command, "hew run".to_string());

    assert!(
        output.status.success(),
        "`hew run` in a package failed\n{}",
        describe_output(&output),
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        GREETING,
        "`hew run` printed the wrong output\n{}",
        describe_output(&output),
    );
}

/// `[package] main` overrides the `main.hew` convention.
#[test]
fn package_main_field_selects_the_entry_point() {
    require_codegen();
    let dir = workspace();
    write_package(
        dir.path(),
        "custom",
        "src/app.hew",
        "main = \"src/app.hew\"\n",
    );

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "build with a declared entry point failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "custom"));
}

/// Resolution walks up from the working directory, so a subdirectory of a
/// package still builds that package.
#[test]
fn build_from_a_subdirectory_resolves_the_enclosing_package() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "walkup", "main.hew", "");
    let sub = dir.path().join("util");
    std::fs::create_dir(&sub).expect("create subdirectory");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(&sub)
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "build from a subdirectory failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "walkup"));
}

/// A relative directory argument resolves from the invocation directory, then
/// walks through it to reach the enclosing package.
#[test]
fn build_relative_directory_from_a_subdirectory_resolves_the_enclosing_package() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "relativewalkup", "main.hew", "");
    let invocation = dir.path().join("util").join("deep");
    let argument = invocation.join("sub");
    std::fs::create_dir_all(&argument).expect("create invocation subdirectory");

    let output = Command::new(hew_binary())
        .args(["build", "sub"])
        .current_dir(&invocation)
        .output()
        .expect("run hew build sub");

    assert!(
        output.status.success(),
        "relative directory build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "relativewalkup"));
}

/// The package's `[native]` crate is a prerequisite: when it cannot build, the
/// package build stops there and never produces a binary.
#[test]
fn native_prerequisite_failure_stops_the_build() {
    let dir = workspace();
    write_package(
        dir.path(),
        "withnative",
        "main.hew",
        "\n[native]\nlib = \"withnative\"\ncrate = \"native\"\n",
    );
    // If Hew compilation ran first, this malformed entry would win. Seeing the
    // missing native crate instead pins the prerequisite ordering.
    std::fs::write(dir.path().join("main.hew"), "fn main( {\n").expect("break entry point");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        !output.status.success(),
        "a package whose [native] crate cannot build must fail\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("[native]"),
        "failure should name the native prerequisite:\n{}",
        describe_output(&output),
    );
    assert!(
        !package_binary_in(dir.path(), "debug", "withnative").exists(),
        "no binary may be produced when the native prerequisite fails",
    );
}

#[test]
fn native_prerequisite_with_matching_toolchain_builds_and_links() {
    require_codegen();
    let dir = workspace();
    write_package(
        dir.path(),
        "withnativeok",
        "main.hew",
        "\n[native]\nlib = \"withnativeok_native\"\ncrate = \"native\"\n",
    );
    let native_dir = dir.path().join("native");
    std::fs::create_dir_all(native_dir.join("src")).expect("create native crate dir");
    // Make the fixture its own Cargo workspace.
    std::fs::write(
        native_dir.join("Cargo.toml"),
        "[workspace]\n\n\
         [package]\nname = \"withnativeok_native\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n\
         [lib]\ncrate-type = [\"staticlib\"]\n",
    )
    .expect("write native Cargo.toml");
    std::fs::write(native_dir.join("src/lib.rs"), "").expect("write native lib.rs");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "a package whose [native] crate matches the runtime's rustc must build\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "withnativeok"));
}

/// Outside any package, the file-less form is a usage error naming the search
/// origin and the way out.
#[test]
fn relative_directory_without_a_manifest_reports_a_usage_error() {
    let dir = support::tempdir();
    let argument = dir.path().join("empty");
    std::fs::create_dir(&argument).expect("create empty directory");

    let output = Command::new(hew_binary())
        .args(["build", "empty"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build empty");

    assert_eq!(
        output.status.code(),
        Some(2),
        "missing manifest is a usage error\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("no hew.toml in ")
            && stderr.contains("empty")
            && stderr.contains("hew init"),
        "error should name the missing manifest and the way out:\n{}",
        describe_output(&output),
    );
}

/// A package whose declared entry point is absent points at the manifest field
/// rather than failing inside the compiler.
#[test]
fn missing_entry_point_points_at_the_manifest_field() {
    let dir = workspace();
    std::fs::write(
        dir.path().join("hew.toml"),
        manifest("gone", "main = \"src/app.hew\"\n"),
    )
    .expect("write hew.toml");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        !output.status.success(),
        "a missing entry point must fail\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("[package] main") && stderr.contains("src/app.hew"),
        "error should name the manifest field and the file:\n{}",
        describe_output(&output),
    );
}

/// The explicit-file form is untouched: the binary is named after the file stem
/// and lands in the working directory.
#[test]
fn explicit_file_form_still_builds_to_the_stem() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "explicit", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "main.hew"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build main.hew");

    assert!(
        output.status.success(),
        "explicit-file build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&binary_in(dir.path(), "main"));
}

/// `hew build` with no `--release` writes into `target/debug/` by default, and
/// the status line names the artefact path.
#[test]
fn build_writes_target_debug_by_default() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "debugpkg", "main.hew", "");

    let output = Command::new(hew_binary())
        .arg("build")
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "debug build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "debug", "debugpkg"));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Built target/debug/debugpkg"),
        "status line should name the debug artefact path:\n{}",
        describe_output(&output),
    );
}

/// `hew build --release` writes into `target/release/`, not `target/debug/`,
/// and the status line names the release artefact path.
#[test]
fn build_release_writes_target_release() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "relpkg", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "--release"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build --release");

    assert!(
        output.status.success(),
        "release build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&package_binary_in(dir.path(), "release", "relpkg"));
    assert!(
        !package_binary_in(dir.path(), "debug", "relpkg").exists(),
        "a --release build must not also write target/debug/",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Built target/release/relpkg"),
        "status line should name the release artefact path:\n{}",
        describe_output(&output),
    );
}

/// `-o` names the linked binary explicitly and wins over the
/// `target/<profile>/` default, even inside a package.
#[test]
fn dash_o_wins_in_package_mode() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "customout", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "-o", "bin/renamed"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build -o bin/renamed");

    assert!(
        output.status.success(),
        "-o build failed\n{}",
        describe_output(&output),
    );
    assert_prints_greeting(&dir.path().join("bin/renamed"));
    assert!(
        !dir.path().join("target").exists(),
        "-o must skip the target/ default entirely",
    );
}

/// `--emit-obj -o` names the object explicitly in file mode, replacing the
/// default stem-named object entirely.
#[test]
fn emit_obj_honours_dash_o_in_file_mode() {
    require_codegen();
    let dir = support::tempdir();
    std::fs::write(dir.path().join("obj.hew"), "fn main() {}\n").expect("write obj.hew");

    let output = Command::new(hew_binary())
        .args(["build", "obj.hew", "--emit-obj", "-o", "custom.o"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build obj.hew --emit-obj -o custom.o");

    assert!(
        output.status.success(),
        "--emit-obj -o (file mode) failed\n{}",
        describe_output(&output),
    );
    assert!(
        dir.path().join("custom.o").is_file(),
        "named object not written"
    );
    assert!(
        !dir.path().join("obj.o").exists(),
        "the default stem-named object must not also be written",
    );
}

/// `--emit-obj -o` names the object explicitly in package mode too, winning
/// over the `target/<profile>/` default.
#[test]
fn emit_obj_honours_dash_o_in_package_mode() {
    require_codegen();
    let dir = workspace();
    write_package(dir.path(), "objpkg", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["build", "--emit-obj", "-o", "custom.o"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build --emit-obj -o custom.o");

    assert!(
        output.status.success(),
        "--emit-obj -o (package mode) failed\n{}",
        describe_output(&output),
    );
    assert!(
        dir.path().join("custom.o").is_file(),
        "named object not written"
    );
    assert!(
        !dir.path().join("target").exists(),
        "-o must skip the target/ default entirely",
    );
}

/// `hew check` with no argument type-checks the package's entry point.
#[test]
fn check_no_argument_type_checks_the_entry_point() {
    let dir = workspace();
    write_package(dir.path(), "checked", "main.hew", "");

    let output = Command::new(hew_binary())
        .arg("check")
        .current_dir(dir.path())
        .output()
        .expect("run hew check");

    assert!(
        output.status.success(),
        "package check failed\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("main.hew: OK"),
        "check should report on the entry point:\n{}",
        describe_output(&output),
    );
}

/// A malformed manifest stops `hew check` before the entry point is compiled,
/// with the issue on stderr so `--format json` keeps stdout clean.
#[test]
fn check_reports_manifest_issues_before_type_checking() {
    let dir = workspace();
    write_package(dir.path(), "checked", "main.hew", "");
    std::fs::write(
        dir.path().join("hew.toml"),
        manifest("Not A Valid Name", ""),
    )
    .expect("rewrite hew.toml");

    let output = Command::new(hew_binary())
        .arg("check")
        .current_dir(dir.path())
        .output()
        .expect("run hew check");

    assert!(
        !output.status.success(),
        "an invalid package name must fail the check\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("invalid package name"),
        "check should name the manifest issue:\n{}",
        describe_output(&output),
    );
    assert!(
        !stderr.contains("main.hew: OK"),
        "type-checking must not run once the manifest is rejected:\n{}",
        describe_output(&output),
    );
}

/// A directory handed to a command that takes only source files gets a real
/// diagnostic — never the raw `Is a directory` OS error.
#[test]
fn directory_reaching_the_compiler_gets_a_diagnostic() {
    let dir = workspace();
    write_package(dir.path(), "diag", "main.hew", "");

    let output = Command::new(hew_binary())
        .args(["compile", "."])
        .current_dir(dir.path())
        .output()
        .expect("run hew compile .");

    assert!(
        !output.status.success(),
        "compiling a directory must fail\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("is a directory, not a .hew source file"),
        "error should explain the directory/file distinction:\n{}",
        describe_output(&output),
    );
    assert!(
        !stderr.contains("os error"),
        "no raw OS error may escape:\n{}",
        describe_output(&output),
    );
}

/// `hew new <name>` creates the directory, scaffolds it like `hew init`,
/// and — like `cargo new` — initializes a git repository of its own, since
/// this fixture (a bare `tempfile::tempdir()`, unlike `workspace()`) sits
/// outside any existing repository.
#[test]
fn hew_new_scaffolds_and_inits_git() {
    let outside_any_repo = tempfile::tempdir().expect("temp dir outside the checkout");

    let output = Command::new(hew_binary())
        .args(["new", "svc"])
        .current_dir(outside_any_repo.path())
        .output()
        .expect("run hew new");

    assert!(
        output.status.success(),
        "hew new failed\n{}",
        describe_output(&output),
    );

    let project_dir = outside_any_repo.path().join("svc");
    assert!(
        project_dir.join("hew.toml").exists(),
        "hew new should scaffold hew.toml"
    );
    assert!(
        project_dir.join("main.hew").exists(),
        "hew new should scaffold main.hew"
    );
    assert!(
        project_dir.join(".git").is_dir(),
        "hew new should initialize a git repository of its own"
    );
    let gitignore = std::fs::read_to_string(project_dir.join(".gitignore")).unwrap();
    assert!(
        gitignore.lines().any(|l| l.trim() == "target/"),
        "scaffold .gitignore should ignore target/:\n{gitignore}"
    );
}

/// Negative control for the test above: `hew new` inside an existing
/// repository must not nest a second `.git` (matching `cargo new`).
#[test]
fn hew_new_inside_a_repo_does_not_nest_git() {
    let dir = workspace();

    let output = Command::new(hew_binary())
        .args(["new", "svc"])
        .current_dir(dir.path())
        .output()
        .expect("run hew new");

    assert!(
        output.status.success(),
        "hew new failed\n{}",
        describe_output(&output),
    );
    assert!(
        !dir.path().join("svc").join(".git").exists(),
        "hew new must not nest a repository inside an existing one"
    );
}

/// `hew add` refuses an unknown package with exit 1 before writing the
/// manifest — proven against a hermetic canned registry (a 404 for every
/// package name), never the real default registry.
#[test]
fn hew_add_unknown_package_exits_1() {
    let dir = workspace();
    write_package(dir.path(), "consumer", "main.hew", "");

    let port = support::http_canned::spawn_canned_response_server(
        "404 Not Found",
        r#"{"message":"package not found"}"#,
    );
    let manifest_before = std::fs::read_to_string(dir.path().join("hew.toml")).unwrap();

    let output = Command::new(hew_binary())
        .args(["add", "hew.does.not.exist"])
        .current_dir(dir.path())
        // A fresh HEW_HOME keeps a developer's real ~/.hew/config.toml (a
        // configured fallback-api, stored credentials) from leaking in.
        .env("HEW_HOME", dir.path().join(".fresh-hew-home"))
        .env("HEW_REGISTRY", format!("http://127.0.0.1:{port}"))
        .output()
        .expect("run hew add");

    assert_eq!(
        output.status.code(),
        Some(1),
        "an unknown package must exit 1\n{}",
        describe_output(&output),
    );
    let manifest_after = std::fs::read_to_string(dir.path().join("hew.toml")).unwrap();
    assert_eq!(
        manifest_before, manifest_after,
        "hew.toml must be unchanged when the package is refused"
    );
}

/// `hew add --dry-run` prints the manifest change and writes nothing. Uses
/// a local path dependency so the test doubles as proof of the invariant
/// that local-path dependencies never contact a registry.
#[test]
fn hew_add_dry_run_writes_nothing() {
    let dir = workspace();
    write_package(dir.path(), "consumer", "main.hew", "");
    let dep_dir = dir.path().join("localdep");
    std::fs::create_dir_all(&dep_dir).expect("create dependency dir");
    write_package(&dep_dir, "localdep", "localdep.hew", "");

    let manifest_before = std::fs::read_to_string(dir.path().join("hew.toml")).unwrap();

    let output = Command::new(hew_binary())
        .args(["add", "localdep", "--path", "localdep", "--dry-run"])
        .current_dir(dir.path())
        .output()
        .expect("run hew add --dry-run");

    assert!(
        output.status.success(),
        "hew add --dry-run failed\n{}",
        describe_output(&output),
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Would add"),
        "dry-run should report the change it would make:\n{stdout}"
    );
    let manifest_after = std::fs::read_to_string(dir.path().join("hew.toml")).unwrap();
    assert_eq!(
        manifest_before, manifest_after,
        "hew add --dry-run must not write hew.toml"
    );
}
