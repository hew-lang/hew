//! #2437 permanent regression coverage: composite-returning `fork`/`await`
//! through the `TaskEntry` adapter, on the standard (non-cancelled) path.
//!
//! The dynamic proof that the cancel-exit fix itself (`emit_cancel_trap_or_
//! return`'s composite arm, hew-codegen-rs/src/llvm.rs) actually corrects the
//! forced-cancellation value lives in `scripts/forced-cancel-composite-check.sh`
//! (requires `hew-runtime`'s `forced-cancel-test` feature — not linkable
//! against the standard `libhew.a` these e2e tests use). The IR-shape
//! regression lives in `hew-codegen-rs/tests/emission/
//! task_entry_cancel_composite_emission.rs`.
//!
//! These three tests are Validation candidates #2 (happy-path non-
//! regression), #4 (owned-field composite), and #5 (`LoopBackEdge`
//! generalization) from the #2437 plan: they never trigger cancellation, so
//! they exercise the composite arm's SIBLING code paths, proving the fix
//! does not disturb the ordinary `return`ed-value path for any of these
//! composite shapes.

mod support;

use support::{repo_root, require_codegen, run_bounded_hew_run, strip_ansi};

fn run_fixture_and_assert_expected(fixture: &str) {
    require_codegen();

    let source = repo_root().join(format!("tests/vertical-slice/accept/{fixture}.hew"));
    let expected = std::fs::read_to_string(
        repo_root().join(format!("tests/vertical-slice/accept/{fixture}.expected")),
    )
    .unwrap_or_else(|e| panic!("read {fixture}.expected: {e}"));

    let output = run_bounded_hew_run(&source, repo_root());

    assert!(
        output.status.success(),
        "{fixture} should run cleanly; stdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    );
    let actual = strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert_eq!(actual, expected, "stdout mismatch for {fixture}");
}

/// Validation candidate #2: `fork`+`await` over a plain scalar-field
/// composite (`record Point { x: i64, y: i64 }`), never cancelled. Must
/// remain byte-identical to pre-fix behaviour (`x=11 y=22`).
#[test]
fn task_entry_composite_cancel_happy_path_unchanged() {
    run_fixture_and_assert_expected("task_entry_composite_cancel_happy_path");
}

/// Validation candidate #4: `fork`+`await` over a composite with an owned
/// heap field (`record Wrapped { s: string, n: i64 }`). The zero-initialized
/// cancel-path composite never reaches this happy path — this proves the
/// fix does not disturb normal owned-field round-tripping.
#[test]
fn task_entry_composite_cancel_owned_field_round_trips() {
    run_fixture_and_assert_expected("task_entry_composite_cancel_owned_field");
}

/// Validation candidate #5: `compute()` contains a loop, so its `TaskEntry`
/// adapter also carries a `LoopBackEdge` cooperate site alongside
/// `FunctionEntry` (hew-mir/src/dataflow.rs). The composite-arm fix is
/// arm-level, not cooperate-site-level, so it must generalize here too.
#[test]
fn task_entry_composite_cancel_loop_back_edge_generalizes() {
    run_fixture_and_assert_expected("task_entry_composite_cancel_loop_back_edge");
}
