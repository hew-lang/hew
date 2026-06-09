/// Integration tests for `hew compile` native linking.
///
/// These tests verify that `cmd_compile` routes the native link step
/// through `link::link_executable`, which resolves `libhew.a` (the combined
/// runtime + stdlib staticlib) and applies the per-platform link plan.
///
/// The fixture used here (`01-arith.hew`) does not exercise the duplex
/// substrate — that path requires the HIR→MIR surface for `duplex_pair` to
/// be wired through the typecheck pass (E3 prerequisite). A duplex-specific
/// end-to-end test is in scope for the E5c lane once E3 lands.
///
/// Linux coverage for `-lpthread -ldl -lm -lrt` requires Docker validation;
/// see `feedback_test_linux_docker`. The link plan is codified in
/// `target::NativeLinkPlan` and applied unconditionally by
/// `link::link_executable` — the test here exercises the macOS path.
mod support;

use std::process::Command;

use support::{describe_output, hew_binary, repo_root, require_codegen};

/// `hew compile` on a trivial arithmetic program links without
/// error and produces a binary that runs to completion.
///
/// This test exercises the full path:
/// - Codegen emits a native `.o` object via `hew-emit`.
/// - `cmd_compile` calls `link::link_executable` with the host
///   `TargetSpec`, which resolves `libhew.a` and applies the platform
///   link plan.
/// - The produced binary runs and exits with the expected non-zero code
///   (3 + 4 = 7 from the fixture).
///
/// Before this change, `cmd_compile` used a raw `cc <obj> -o <bin>`
/// invocation with no `-l` flags, so any program referencing runtime
/// symbols would have produced undefined-symbol linker errors.
#[test]
fn compile_native_link_produces_runnable_binary() {
    require_codegen();

    let fixture = repo_root().join("tests/vertical-slice/accept/01-arith.hew");
    assert!(fixture.exists(), "fixture not found: {}", fixture.display());

    let emit_dir = tempfile::Builder::new()
        .prefix("compile-link-test-")
        .tempdir()
        .expect("create temp dir for compile output");

    let fixture_str = fixture.to_str().expect("fixture path is valid UTF-8");
    let emit_dir_str = emit_dir.path().to_str().expect("emit dir is valid UTF-8");

    // Compile the fixture to a native binary. Native is the default target.
    let compile_output = Command::new(hew_binary())
        .args(["compile", "--emit-dir", emit_dir_str, fixture_str])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");

    assert!(
        compile_output.status.success(),
        "hew compile failed:\n{}",
        describe_output(&compile_output),
    );

    // Locate the binary path from the `native: <path>` line on stdout.
    let stdout = String::from_utf8_lossy(&compile_output.stdout);
    let binary_path = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .unwrap_or_else(|| {
            panic!("hew compile stdout did not contain a `native:` line:\n{stdout}")
        });

    // Verify the binary exists and runs to completion. The arith fixture
    // computes `3 + 4 = 7` and returns it as the exit code.
    assert!(
        std::path::Path::new(binary_path).exists(),
        "binary not found at {binary_path}"
    );

    let run_output = Command::new(binary_path)
        .output()
        .unwrap_or_else(|e| panic!("failed to run compiled binary {binary_path}: {e}"));

    // The fixture returns `a + b` (3 + 4 = 7). The exit code confirms the
    // arithmetic ran correctly inside the binary.
    assert_eq!(
        run_output.status.code(),
        Some(7),
        "compiled binary exited with unexpected code; expected 7 (3+4)\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run_output.stdout),
        String::from_utf8_lossy(&run_output.stderr),
    );
}
