/// Acceptance test for the `directory_module_demo` example.
///
/// Verifies that `hew check` succeeds on the directory-form module example.
/// The example exercises:
///   - `import greeting;` resolving to a directory-form module
///   - peer-file merging (`greeting.hew` + `greeting_helpers.hew`)
///   - qualified function calls across merged peer files (`greeting.hello()`,
///     `greeting.target()`)
///
/// `hew check` (parse + typecheck) is the right gate here because the demo
/// uses string concatenation, which is not yet lowered in the v0.5 codegen.
/// The directory-module resolution and call-lowering paths are covered by
/// the vertical-slice acceptance suite (`tests/v05-vertical-slice/accept/`).
mod support;

/// `hew check` on the `directory_module_demo` example exits 0.
#[test]
fn directory_module_demo_checks_clean() {
    let demo = support::repo_root().join("examples/directory_module_demo/main.hew");

    assert!(demo.exists(), "example not found: {}", demo.display());

    let output = std::process::Command::new(support::hew_binary())
        .arg("check")
        .arg(&demo)
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "hew check failed on `directory_module_demo`\n{}",
        support::describe_output(&output),
    );
}
