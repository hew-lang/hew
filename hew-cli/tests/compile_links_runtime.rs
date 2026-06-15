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

/// Compile `source` to a native binary and return its path (kept alive by the
/// returned `TempDir`). Mirrors the `hew compile --emit-dir` + `native:` stdout
/// parsing used by `compile_native_link_produces_runnable_binary`.
fn compile_to_native(source: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let emit_dir = tempfile::Builder::new()
        .prefix("wire-decode-trap-")
        .tempdir()
        .expect("create temp dir for compile output");
    let src_path = emit_dir.path().join("prog.hew");
    std::fs::write(&src_path, source).expect("write source");

    let compile_output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            emit_dir.path().to_str().expect("emit dir is valid UTF-8"),
            src_path.to_str().expect("source path is valid UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        compile_output.status.success(),
        "hew compile failed:\n{}",
        describe_output(&compile_output),
    );
    let stdout = String::from_utf8_lossy(&compile_output.stdout);
    let binary_path = stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .unwrap_or_else(|| panic!("hew compile stdout had no `native:` line:\n{stdout}"));
    (emit_dir, std::path::PathBuf::from(binary_path))
}

/// `hew compile` on a trivial arithmetic program links without
/// error and produces a binary that runs to completion.
///
/// This test exercises the full path:
/// - Codegen emits a native `.o` object in-process through LLVM.
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

/// A wire-type `Type.decode(bytes)` on a valid encoding round-trips: the
/// fields reconstruct correctly and the program runs to a clean exit. Guards
/// the non-null (success) arm of the decode call-site lowering against the
/// null-branch fix regressing the happy path.
#[test]
fn wire_decode_valid_bytes_round_trips() {
    require_codegen();
    let (_emit_dir, binary_path) = compile_to_native(
        "#[wire]\n\
         struct Point { x: i64 @1, y: i64 @2 }\n\
         fn main() -> i64 {\n\
         \x20   let p = Point { x: 7, y: 35 };\n\
         \x20   let b = p.encode();\n\
         \x20   let p2 = Point.decode(b);\n\
         \x20   return p2.x + p2.y;\n\
         }\n",
    );
    let run_output = Command::new(&binary_path)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {}: {e}", binary_path.display()));
    // 7 + 35 = 42 confirms both fields survived the round-trip.
    assert_eq!(
        run_output.status.code(),
        Some(42),
        "valid wire decode must round-trip to 42 (7+35)\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run_output.stdout),
        String::from_utf8_lossy(&run_output.stderr),
    );
}

/// A wire-type `Type.decode(bytes)` on MALFORMED bytes (an empty buffer that is
/// not a valid encoding) must FAIL CLOSED with a clean runtime trap — never a
/// SIGSEGV. The deserialize thunk frees its partial reconstruction and returns
/// null; before the null-branch fix the call site loaded that null pointer and
/// segfaulted (exit 139 / signal 11). Now it traps `HEW_TRAP_WIRE_DECODE_FAILED`
/// and aborts via `llvm.trap` (SIGTRAP/SIGILL), the fail-closed boundary.
#[cfg(unix)]
#[test]
fn wire_decode_malformed_bytes_fails_closed_not_segfault() {
    use std::os::unix::process::ExitStatusExt;

    require_codegen();
    let (_emit_dir, binary_path) = compile_to_native(
        "#[wire]\n\
         struct Point { x: i64 @1, y: i64 @2 }\n\
         fn main() -> i64 {\n\
         \x20   let b: bytes = bytes::new();\n\
         \x20   let p = Point.decode(b);\n\
         \x20   return p.x + p.y;\n\
         }\n",
    );
    let run_output = Command::new(&binary_path)
        .output()
        .unwrap_or_else(|e| panic!("failed to run {}: {e}", binary_path.display()));

    // The program must not succeed on malformed input.
    assert!(
        !run_output.status.success(),
        "decoding malformed bytes must not succeed\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run_output.stdout),
        String::from_utf8_lossy(&run_output.stderr),
    );
    // The crash MUST be a trap signal (SIGILL=4 / SIGTRAP=5 from `llvm.trap`),
    // never SIGSEGV (11) — the segfault this fix closed.
    let signal = run_output.status.signal();
    assert_ne!(
        signal,
        Some(libc::SIGSEGV),
        "malformed wire decode segfaulted (SIGSEGV) instead of trapping closed\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&run_output.stdout),
        String::from_utf8_lossy(&run_output.stderr),
    );
    assert!(
        matches!(signal, Some(libc::SIGILL | libc::SIGTRAP)),
        "expected a clean trap signal (SIGILL/SIGTRAP) on malformed wire decode, got signal {signal:?} / code {:?}\nstdout: {}\nstderr: {}",
        run_output.status.code(),
        String::from_utf8_lossy(&run_output.stdout),
        String::from_utf8_lossy(&run_output.stderr),
    );
}
