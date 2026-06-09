//! End-to-end tests for the `--show-stack-hints` flag on `hew check` and
//! `hew run`.
//!
//! Phase A.0 ships the diagnostic-only stack-allocation hint scaffold: the
//! checker's escape-analysis pass walks every function body and emits a
//! `HEW-PERF-001` info-severity line on stderr for each binding whose RHS
//! resolves to a known heap-allocation class (`Vec`, `String`, `HashMap`,
//! `HashSet`, `Rc`, `ClosureEnv`).
//!
//! These tests pin contracts that are stable from A.0 onward:
//!
//! 1. The flag is accepted on `hew check` and never affects exit code.
//! 2. The scaffold emits at least one `HEW-PERF-001` line for a program that
//!    contains both a Vec binding and a closure binding.
//! 3. Each emitted line uses the documented format
//!    `<file>:<line>:<col>: info[HEW-PERF-001]: binding `<name>` (<class>) ...`.
//! 4. A type-erroring program passed to `hew check --show-stack-hints` still
//!    renders source-span diagnostics (underline lines), not just the bare
//!    failure summary — the flag path must not swallow span attribution.
//! 5. `hew run --show-stack-hints` emits hints to stderr before the program
//!    executes, and the program's own stdout is unaffected.
//!
//! Hint *accuracy* (which bindings should fire, which should be silent under
//! escape conditions) is the responsibility of subsequent slices A.1/A.2/A.3
//! and is not asserted here. A.0 is intentionally noisy.

mod support;

use std::process::Command;
use support::{hew_binary, require_codegen, strip_ansi};

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
    //   - `v` resolves to `Vec<i64>`        -> AllocationClass::Vec
    //   - `f` is a closure literal           -> AllocationClass::ClosureEnv
    // Plus one stack-shaped binding (`r`) that must NOT produce a hint —
    // a primitive i64 return does not allocate on the heap.
    let source = "fn main() {\n\
        \x20\x20\x20\x20let v: Vec<i64> = Vec::new();\n\
        \x20\x20\x20\x20let f = (x: i64) -> i64 => x * 2;\n\
        \x20\x20\x20\x20let r: i64 = f(3);\n\
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

/// `hew check --show-stack-hints` on a type-erroring program must still render
/// source-span diagnostics (with `^^^` underlines and `file:line:col` attribution)
/// rather than silently swallowing them and emitting only the bare failure
/// summary.
///
/// Regression guard for the `cmd_check` failure branch: before the fix, the
/// `--show-stack-hints` path discarded `failure.diagnostics` and only printed
/// `failure.message`, so the user never saw which line caused the error.
#[test]
fn show_stack_hints_check_failure_renders_span_diagnostics() {
    // A program with a deliberate type error: the Vec<[i64; 2]> element type
    // is unsupported, producing a type error with a source span. The span
    // underline (`^^^`) must appear in stderr when --show-stack-hints is set.
    let source = "fn main() {\n\
        \x20\x20\x20\x20let v: Vec<[i64; 2]> = Vec::new();\n\
        \x20\x20\x20\x20println(v.len());\n\
        }\n";

    let (_fixture, scaffold_path) = write_fixture(source);
    let scaffold_str = scaffold_path.to_str().expect("scaffold path must be UTF-8");

    let output = Command::new(hew_binary())
        .args([
            "check",
            "--show-stack-hints",
            scaffold_path.to_str().unwrap(),
        ])
        .output()
        .expect("hew binary must run");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The program has a type error — must exit non-zero.
    assert!(
        !output.status.success(),
        "hew check --show-stack-hints must exit non-zero on a type-erroring program; \
         stderr:\n{stderr}",
    );

    // The rendered output must include the fixture's path as a diagnostic
    // location header (e.g. `<path>:2:12: error: ...`).
    assert!(
        stderr.contains(scaffold_str),
        "stderr must cite the fixture path `{scaffold_str}` for span attribution; \
         got:\n{stderr}",
    );

    // A source-span underline (`^^^` or `^`) must be present — this proves the
    // full diagnostic rendering path ran, not just `eprintln!(failure.message)`.
    assert!(
        stderr.contains('^'),
        "stderr must contain a span underline (`^`) from the diagnostic renderer; \
         got:\n{stderr}",
    );
}

/// `hew run --show-stack-hints` must emit HEW-PERF-001 hints to stderr for
/// a program with known heap-class bindings, and the program's own stdout
/// output must be unaffected.
///
/// This exercises the `cmd_run` code path, which is distinct from `cmd_check`:
/// it calls `hew_compile::check_file` for hints first, then separately compiles
/// and runs the artifact. Both steps must succeed and the two output streams
/// must not interfere.
// Disabled during v0.5 cutover: command execution is not yet routed through the Rust MIR/codegen-rs substrate.
#[ignore = "v0.5: execution awaits Rust MIR/codegen-rs routing"]
#[test]
fn show_stack_hints_run_emits_hints_and_executes_program() {
    require_codegen();

    // A program with a known heap-class binding (`v: Vec<i64>`) that also
    // prints a sentinel value so we can verify execution succeeded.
    let source = "fn main() {\n\
        \x20\x20\x20\x20let v: Vec<i64> = Vec::new();\n\
        \x20\x20\x20\x20println(f\"len={v.len()}\");\n\
        }\n";

    let (_fixture, scaffold_path) = write_fixture(source);
    let scaffold_str = scaffold_path.to_str().expect("scaffold path must be UTF-8");

    let output = Command::new(hew_binary())
        .args(["run", "--show-stack-hints", scaffold_path.to_str().unwrap()])
        .output()
        .expect("hew binary must run");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The program must run successfully.
    assert!(
        output.status.success(),
        "hew run --show-stack-hints must exit 0 on a well-typed program; \
         got exit={:?}\nstdout:\n{stdout}\nstderr:\n{stderr}",
        output.status.code(),
    );

    // The program's stdout must contain the sentinel value — execution completed.
    assert!(
        stdout.contains("len=0"),
        "program stdout must contain the sentinel `len=0`; got:\n{stdout}",
    );

    // At least one HEW-PERF-001 hint must be present on stderr.
    let perf_lines: Vec<&str> = stderr
        .lines()
        .filter(|line| line.contains("HEW-PERF-001"))
        .collect();
    assert!(
        !perf_lines.is_empty(),
        "expected at least one HEW-PERF-001 line on stderr; got:\n{stderr}",
    );

    // Each hint line must cite the fixture path for source attribution.
    for line in &perf_lines {
        assert!(
            line.contains(scaffold_str),
            "HEW-PERF-001 line must cite `{scaffold_str}`; got:\n{line}\nfull stderr:\n{stderr}",
        );
    }
}
