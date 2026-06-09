/// Acceptance test for the `directory_module_demo` example.
///
/// Verifies that `hew check` reaches the Stage 2 MIR gate on the directory-form
/// module example after directory-module resolution succeeds.
/// The example exercises:
///   - `import greeting;` resolving to a directory-form module
///   - peer-file merging (`greeting.hew` + `greeting_helpers.hew`)
///   - qualified function calls across merged peer files (`greeting.hello()`,
///     `greeting.target()`)
///
/// Stage 2 routes `hew check` through HIR/MIR gates. The demo uses string
/// concatenation, which is still not in the current MIR spine, so success is no
/// longer the expected contract; reaching the MIR diagnostic is.
mod support;

/// `hew check` on the `directory_module_demo` example reaches MIR.
#[test]
fn directory_module_demo_reaches_mir_gate() {
    let demo = support::repo_root().join("examples/directory_module_demo/main.hew");

    assert!(demo.exists(), "example not found: {}", demo.display());

    let output = std::process::Command::new(support::hew_binary())
        .arg("check")
        .arg(&demo)
        .output()
        .expect("invoke hew check");

    assert!(
        !output.status.success(),
        "Stage 2 check should reject `directory_module_demo` at MIR for unsupported string concat\n{}",
        support::describe_output(&output),
    );
    let stderr = support::strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        stderr.contains("E_NOT_YET_IMPLEMENTED") || stderr.contains("E_MIR"),
        "directory demo should reach a deep gate diagnostic; got:\n{stderr}",
    );
    assert!(
        stderr.contains("MIR lowering") && !stderr.contains("MirDiagnostic"),
        "directory demo should render a user-facing MIR diagnostic; got:\n{stderr}",
    );
}
