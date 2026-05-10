//! End-to-end tests for the `--show-stack-hints` flag on `hew check`.
//!
//! Phase A.0 ships the diagnostic-only stack-allocation hint scaffold: the
//! checker's escape-analysis pass walks every function body and emits a
//! `HEW-PERF-001` info-severity line on stderr for each binding whose RHS
//! resolves to a known heap-allocation class (`Vec`, `String`, `HashMap`,
//! `HashSet`, `Rc`, `ClosureEnv`).
//!
//! These tests pin three contracts that are stable from A.0 onward:
//!
//! 1. The flag is accepted on `hew check` and never affects exit code.
//! 2. The scaffold emits at least one `HEW-PERF-001` line for a program that
//!    contains both a Vec binding and a closure binding.
//! 3. Each emitted line uses the documented format
//!    `<file>:<line>:<col>: info[HEW-PERF-001]: binding `<name>` (<class>) ...`.
//!
//! Hint *accuracy* (which bindings should fire, which should be silent under
//! escape conditions) is the responsibility of subsequent slices A.1/A.2/A.3
//! and is not asserted here. A.0 is intentionally noisy.

mod support;

use std::process::Command;
use support::{hew_binary, strip_ansi};

fn write_fixture(content: &str) -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let path = dir.path().join("scaffold.hew");
    std::fs::write(&path, content).expect("cannot write scaffold fixture");
    (dir, path)
}

/// `hew check --show-stack-hints` on a program that allocates a Vec and binds
/// a lambda must exit zero, must surface at least one HEW-PERF-001 line on
/// stderr, and the rendered lines must point at the fixture file using the
/// documented `file:line:col: info[HEW-PERF-001]: binding ...` shape.
#[test]
fn show_stack_hints_flag_emits_perf_001_on_known_allocations() {
    // Two known-heap-class bindings:
    //   - `v` resolves to `Vec<int>`        -> AllocationClass::Vec
    //   - `f` is a closure literal           -> AllocationClass::ClosureEnv
    // Plus one stack-shaped binding (`r`) that must NOT produce a hint —
    // a primitive int return does not allocate on the heap.
    let source = "fn main() {\n\
        \x20\x20\x20\x20let v: Vec<int> = Vec::new();\n\
        \x20\x20\x20\x20let f = (x: int) -> int => x * 2;\n\
        \x20\x20\x20\x20let r: int = f(3);\n\
        \x20\x20\x20\x20println(f\"v.len = {v.len()}, r = {r}\");\n\
        }\n";

    let (_fixture, scaffold_path) = write_fixture(source);

    let output = Command::new(hew_binary())
        .args([
            "check",
            "--show-stack-hints",
            scaffold_path.to_str().unwrap(),
        ])
        .output()
        .expect("hew binary must run");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // Severity is info — the flag must never affect exit code.
    assert!(
        output.status.success(),
        "hew check --show-stack-hints must exit 0 on a well-typed program; \
         got exit={:?}\nstderr:\n{stderr}",
        output.status.code(),
    );

    // At least one HEW-PERF-001 line must be present (intentionally noisy
    // baseline — the exact count is not constrained until the heuristic
    // slices land).
    let perf_lines: Vec<&str> = stderr
        .lines()
        .filter(|line| line.contains("HEW-PERF-001"))
        .collect();
    assert!(
        !perf_lines.is_empty(),
        "expected at least one HEW-PERF-001 line on stderr; got:\n{stderr}",
    );

    // Each line must reference the scaffold fixture's path (not a placeholder
    // like <unknown> or the binary name) — diagnostic source attribution.
    let scaffold_str = scaffold_path.to_str().expect("scaffold path must be UTF-8");
    for line in &perf_lines {
        assert!(
            line.contains(scaffold_str),
            "HEW-PERF-001 line must cite the fixture path `{scaffold_str}`; \
             got line:\n{line}\nfull stderr:\n{stderr}",
        );
        assert!(
            line.contains("info[HEW-PERF-001]: binding `"),
            "HEW-PERF-001 line must use the documented format; got:\n{line}",
        );
        assert!(
            line.contains(") could be stack-allocated"),
            "HEW-PERF-001 line must end with the documented suffix; got:\n{line}",
        );
    }

    // Without the flag, the same program must produce zero HEW-PERF-001
    // lines — the scaffold is gated on opt-in, never always-on.
    let output_no_flag = Command::new(hew_binary())
        .args(["check", scaffold_path.to_str().unwrap()])
        .output()
        .expect("hew binary must run without flag");
    let stderr_no_flag = strip_ansi(&String::from_utf8_lossy(&output_no_flag.stderr));
    assert!(
        !stderr_no_flag.contains("HEW-PERF-001"),
        "no HEW-PERF-001 lines should appear without --show-stack-hints; got:\n{stderr_no_flag}",
    );
}
