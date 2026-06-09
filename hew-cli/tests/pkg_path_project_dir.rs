/// Integration test for `.adze/packages` resolution using `ctx.project_dir`.
///
/// Prior to this fix, `.adze/packages` candidates were built from `cwd` (the
/// process working directory at the time of compilation). This meant that
/// `hew check /abs/path/to/main.hew` invoked from an unrelated directory would
/// fail to find packages installed in `/abs/path/to/.adze/packages/`.
///
/// The fix anchors the `.adze/packages` search to `ctx.project_dir` (derived
/// from the input file's parent), so package resolution is cwd-independent.
mod support;

use std::fs;

/// Verify that `hew check` resolves a package from `.adze/packages` relative
/// to the source file's directory, regardless of the process working directory.
///
/// Layout:
/// ```text
/// <temp>/
/// ├── main.hew               ← import foo; fn main() { ... }
/// └── .adze/packages/
///     └── foo/
///         └── foo.hew        ← pub fn answer() -> i64 { 42 }
/// ```
///
/// `hew check` is invoked from an unrelated directory (`/tmp`) using the
/// absolute path to `<temp>/main.hew`. The test asserts exit 0, proving
/// that package resolution uses the source file's parent, not the cwd.
#[test]
fn pkg_path_resolves_relative_to_project_dir_not_cwd() {
    let dir = support::tempdir();
    let pkg_dir = dir.path().join(".adze/packages/foo");
    fs::create_dir_all(&pkg_dir).expect("create .adze/packages/foo");

    fs::write(pkg_dir.join("foo.hew"), "pub fn answer() -> i64 { 42 }\n").expect("write foo.hew");

    fs::write(
        dir.path().join("main.hew"),
        "import foo;\nfn main() -> i64 { foo.answer() }\n",
    )
    .expect("write main.hew");

    let main_path = dir.path().join("main.hew");

    // Invoke from /tmp — a directory that has no .adze/packages/foo.
    // Resolution must use the source file's parent, not the cwd.
    let output = std::process::Command::new(support::hew_binary())
        .arg("check")
        .arg(&main_path)
        .current_dir(std::env::temp_dir())
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "hew check failed — package not found when cwd differs from project dir\n{}",
        support::describe_output(&output),
    );
}
