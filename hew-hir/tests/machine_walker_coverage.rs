//! W4.002 — coverage tests for the FC-P0 sibling Machine-walker fix in
//! `hew-hir/src/lower.rs`.
//!
//! Two HIR pre-pass walkers previously skipped `Item::Machine`:
//!   - `check_wasm_blocking_recv_gate` (wasm32-only `.recv()` rejection)
//!   - `check_task_gates` (task/fork/deadline shape validation)
//!
//! Both now visit the canonical four Machine positions per the A242
//! invariant: each state's `entry` block, each state's `exit` block,
//! each transition's `guard` expression, and each transition's `body`
//! expression. These tests assert one positive case per position per
//! walker (8 positives), plus one negative per walker (2 negatives).

use hew_hir::{lower_program, HirDiagnosticKind, TargetArch};
use hew_parser::parser;
use hew_types::TypeCheckOutput;

// ── helpers ────────────────────────────────────────────────────────────────

fn lower_wasm(source: &str) -> hew_hir::LowerOutput {
    let parsed = parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::Wasm32,
    )
}

fn lower_host(source: &str) -> hew_hir::LowerOutput {
    let parsed = parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    )
}

fn has_blocking_recv(out: &hew_hir::LowerOutput) -> bool {
    out.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::BlockingChannelRecvUnsupportedOnWasm { .. }
        )
    })
}

fn has_task_signature_unsupported(out: &hew_hir::LowerOutput) -> bool {
    out.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::TaskSpawnSignatureUnsupported { .. }
        )
    })
}

// ── wasm_blocking_recv: 4 positions + 1 negative ───────────────────────────

#[test]
fn wasm_blocking_recv_in_machine_state_entry() {
    // Position 1: state.entry block.
    let source = r"
        machine M {
            state Idle {
                entry { let _ = ch.recv(); }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_wasm(source);
    assert!(
        has_blocking_recv(&out),
        ".recv() in state.entry must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn wasm_blocking_recv_in_machine_state_exit() {
    // Position 2: state.exit block.
    let source = r"
        machine M {
            state Idle {
                exit { let _ = ch.recv(); }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_wasm(source);
    assert!(
        has_blocking_recv(&out),
        ".recv() in state.exit must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn wasm_blocking_recv_in_machine_transition_guard() {
    // Position 3: transition.guard expression.
    let source = r"
        machine M {
            state Idle;
            event Tick;
            on Tick: Idle -> Idle @reenter when ch.recv() { Idle }
        }
    ";
    let out = lower_wasm(source);
    assert!(
        has_blocking_recv(&out),
        ".recv() in transition.guard must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn wasm_blocking_recv_in_machine_transition_body() {
    // Position 4: transition.body expression (Expr::Block here).
    let source = r"
        machine M {
            state Idle;
            event Tick;
            on Tick: Idle -> Idle @reenter { let _ = ch.recv(); Idle }
        }
    ";
    let out = lower_wasm(source);
    assert!(
        has_blocking_recv(&out),
        ".recv() in transition.body must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn wasm_machine_without_recv_no_diagnostic() {
    // Negative: a benign machine on wasm32 must not trigger the recv gate.
    let source = r"
        machine M {
            state Idle {
                entry { let _ = 1; }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_wasm(source);
    assert!(
        !has_blocking_recv(&out),
        "benign machine must not trigger recv gate; got: {:#?}",
        out.diagnostics
    );
}

// ── task_gates: 4 positions + 1 negative ───────────────────────────────────
//
// Plant `scope { fork child = worker(42); }` — the fork-child-with-args
// shape that triggers `TaskSpawnSignatureUnsupported` per
// `check_fork_child_shape` (hew-hir/src/lower.rs:11688). Target is host
// (task gate is target-agnostic).

#[test]
fn task_gate_in_machine_state_entry() {
    let source = r"
        fn worker(x: int) {}
        machine M {
            state Idle {
                entry { scope { fork child = worker(42); } }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_host(source);
    assert!(
        has_task_signature_unsupported(&out),
        "fork-child-with-args in state.entry must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn task_gate_in_machine_state_exit() {
    let source = r"
        fn worker(x: int) {}
        machine M {
            state Idle {
                exit { scope { fork child = worker(42); } }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_host(source);
    assert!(
        has_task_signature_unsupported(&out),
        "fork-child-with-args in state.exit must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn task_gate_in_machine_transition_guard() {
    // The guard is a plain expression; planting a Block expression with the
    // fork inside lets the walker descend into it. Guards must be boolean at
    // type-check time but the HIR pre-pass walker runs before the checker
    // narrows types.
    let source = r"
        fn worker(x: int) {}
        machine M {
            state Idle;
            event Tick;
            on Tick: Idle -> Idle @reenter
                when { scope { fork child = worker(42); } true }
                { Idle }
        }
    ";
    let out = lower_host(source);
    assert!(
        has_task_signature_unsupported(&out),
        "fork-child-with-args in transition.guard must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn task_gate_in_machine_transition_body() {
    let source = r"
        fn worker(x: int) {}
        machine M {
            state Idle;
            event Tick;
            on Tick: Idle -> Idle @reenter {
                scope { fork child = worker(42); }
                Idle
            }
        }
    ";
    let out = lower_host(source);
    assert!(
        has_task_signature_unsupported(&out),
        "fork-child-with-args in transition.body must be detected; got: {:#?}",
        out.diagnostics
    );
}

#[test]
fn task_gate_machine_without_fork_no_diagnostic() {
    // Negative: a benign machine with no task/fork constructs must not trip
    // the task-gate walker.
    let source = r"
        machine M {
            state Idle {
                entry { let _ = 1; }
            }
            event Tick;
            on Tick: Idle -> Idle @reenter { Idle }
        }
    ";
    let out = lower_host(source);
    assert!(
        !has_task_signature_unsupported(&out),
        "benign machine must not trigger task gate; got: {:#?}",
        out.diagnostics
    );
}
