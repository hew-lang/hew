/// Acceptance test for the `directory_module_demo` example.
///
/// Verifies that `hew check` accepts the directory-form module example after
/// directory-module resolution succeeds.
/// The example exercises:
///   - `import greeting;` resolving to a directory-form module
///   - peer-file merging (`greeting.hew` + `greeting_helpers.hew`)
///   - qualified function calls across merged peer files (`greeting.hello()`,
///     `greeting.target()`)
///
mod support;

/// `hew check` on the `directory_module_demo` example succeeds.
#[test]
fn directory_module_demo_checks_successfully() {
    let demo = support::repo_root().join("examples/directory_module_demo/main.hew");

    assert!(demo.exists(), "example not found: {}", demo.display());

    let output = std::process::Command::new(support::hew_binary())
        .arg("check")
        .arg(&demo)
        .output()
        .expect("invoke hew check");

    assert!(
        output.status.success(),
        "`directory_module_demo` should check successfully\n{}",
        support::describe_output(&output),
    );
}
