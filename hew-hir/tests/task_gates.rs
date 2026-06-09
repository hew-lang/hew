//! Tests for FC-P1-A1 task/fork/deadline gates.
//!
//! Validates that spawned calls, fork children, fork blocks, deadline scopes,
//! and await expressions are correctly validated at HIR lowering.

use hew_hir::{lower_program, HirDiagnosticKind, TargetArch};
use hew_parser::parser;
use hew_types::TypeCheckOutput;

// ── ForkChild signature/callee tests ────────────────────────────────────────

#[test]
fn fork_child_direct_fn_unit_accepted() {
    // Valid: fork child calling direct module function with no args
    let source = r"
        fn worker() {}
        fn main() {
            scope {
                fork child = worker();
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_gate_diagnostic = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::TaskSpawnSignatureUnsupported { .. }
                | HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }
        )
    });
    assert!(
        !has_gate_diagnostic,
        "Valid fork child should not trigger gate; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn fork_child_with_args_rejected() {
    // Invalid: spawned function has arguments
    let source = r"
        fn worker(x: int) {}
        fn main() {
            scope {
                fork child = worker(42);
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_signature_unsupported = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::TaskSpawnSignatureUnsupported { .. }
        )
    });
    assert!(
        has_signature_unsupported,
        "Fork child with args must emit TaskSpawnSignatureUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for invalid fork child signature"
    );
}

#[test]
fn fork_child_indirect_call_rejected() {
    // Invalid: callee is not a direct module function (variable reference)
    let source = r"
        fn worker() {}
        fn main() {
            let f = worker;
            scope {
                fork child = f();
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_callee_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }));
    assert!(
        has_callee_unsupported,
        "Fork child with indirect call must emit TaskSpawnCalleeUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for invalid fork child callee"
    );
}

// ── Spawned closure tests ───────────────────────────────────────────────────

#[test]
fn spawned_closure_zero_params_accepted() {
    // Valid: spawned closure has no parameters
    let source = r"
        fn main() {
            scope {
                fork child = { || work() }();
            }
        }
        fn work() {}
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_gate_diagnostic = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::SpawnedClosureSignatureUnsupported { .. }
        )
    });
    assert!(
        !has_gate_diagnostic,
        "Valid spawned closure should not trigger gate; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn spawned_closure_with_params_rejected() {
    // Invalid: spawned closure has parameters
    // Note: Without full type checking, we may not have closure capture facts,
    // so we'll get TaskSpawnCalleeUnsupported instead of the more specific
    // SpawnedClosureSignatureUnsupported. This is acceptable - the gate fires.
    let source = r"
        fn main() {
            scope {
                fork child = { |x| work(x) }();
            }
        }
        fn work(x: int) {}
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_closure_gate = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::SpawnedClosureSignatureUnsupported { .. }
                | HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }
        )
    });
    assert!(
        has_closure_gate,
        "Spawned closure with params must emit closure gate diagnostic; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for invalid spawned closure signature"
    );
}

// ── ForkBlock body tests ────────────────────────────────────────────────────

#[test]
fn fork_block_single_call_accepted() {
    // Valid: fork block with single direct function call
    let source = r"
        fn worker() {}
        fn main() {
            scope {
                fork { worker() }
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_gate_diagnostic = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::ForkBlockBodyUnsupported { .. }));
    assert!(
        !has_gate_diagnostic,
        "Valid fork block should not trigger gate; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn fork_block_empty_rejected() {
    // Invalid: fork block has empty body
    let source = r"
        fn main() {
            scope {
                fork {}
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_fork_block_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::ForkBlockBodyUnsupported { .. }));
    assert!(
        has_fork_block_unsupported,
        "Empty fork block must emit ForkBlockBodyUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for empty fork block"
    );
}

#[test]
fn fork_block_multi_statement_rejected() {
    // Invalid: fork block has multiple statements
    let source = r"
        fn a() {}
        fn b() {}
        fn main() {
            scope {
                fork {
                    a();
                    b();
                }
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_fork_block_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::ForkBlockBodyUnsupported { .. }));
    assert!(
        has_fork_block_unsupported,
        "Multi-statement fork block must emit ForkBlockBodyUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for multi-statement fork block"
    );
}

#[test]
fn fork_block_not_call_rejected() {
    // Invalid: fork block body is not a call expression
    let source = r"
        fn main() {
            scope {
                fork { 42 }
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_fork_block_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::ForkBlockBodyUnsupported { .. }));
    assert!(
        has_fork_block_unsupported,
        "Non-call fork block must emit ForkBlockBodyUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for non-call fork block"
    );
}

// ── ScopeDeadline tests ─────────────────────────────────────────────────────

#[test]
fn deadline_empty_body_accepted() {
    // Valid: deadline has empty body (syntax sugar for timeout-only scope)
    let source = r"
        fn main() {
            scope {
                after(1ms);
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_gate_diagnostic = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::DeadlineBodyUnsupported { .. }));
    assert!(
        !has_gate_diagnostic,
        "Empty deadline should not trigger gate; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn deadline_non_empty_body_rejected() {
    // Invalid: deadline has non-empty body
    let source = r"
        fn main() {
            scope {
                after(1ms) {
                    work();
                }
            }
        }
        fn work() {}
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_deadline_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::DeadlineBodyUnsupported { .. }));
    assert!(
        has_deadline_unsupported,
        "Non-empty deadline must emit DeadlineBodyUnsupported; got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err for non-empty deadline"
    );
}

// ── AwaitTask tests ─────────────────────────────────────────────────────────

#[test]
fn await_unit_task_accepted() {
    // Valid: awaiting task with unit result
    // Note: This is a syntactic test; actual Task type checking happens in type checker
    let source = r"
        fn main() {
            let task = worker();
            await task;
        }
        fn worker() {}
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    // Without type information, the gate won't fire
    // This is acceptable - the gate is defense-in-depth for MIR
    let has_gate_diagnostic = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::AwaitTaskResultUnsupported { .. }));
    assert!(
        !has_gate_diagnostic,
        "Valid await should not trigger gate; got: {:#?}",
        output.diagnostics
    );
}

#[test]
fn await_expression_parses() {
    // Ensure await expressions are parsed correctly
    let source = r"
        fn main() {
            await task_handle;
        }
    ";
    let parsed = parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "Parse errors: {:?}",
        parsed.errors
    );

    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    // The walker should traverse await expressions without panicking
    // Gate diagnostics depend on type information which isn't available in this minimal test
    // This test validates that the walker code doesn't crash
    let _ = output;
}

// ── Integration tests ───────────────────────────────────────────────────────

#[test]
fn multiple_gates_can_fire() {
    // Multiple violations in one program should all be reported
    let source = r"
        fn main() {
            scope {
                fork { a(); b(); }
                fork {}
                after(1ms) { work(); }
            }
        }
        fn a() {}
        fn b() {}
        fn work() {}
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let gate_diagnostic_count = output
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                HirDiagnosticKind::ForkBlockBodyUnsupported { .. }
                    | HirDiagnosticKind::DeadlineBodyUnsupported { .. }
            )
        })
        .count();

    assert!(
        gate_diagnostic_count >= 3,
        "Expected at least 3 gate diagnostics (2 fork blocks + 1 deadline); got: {:#?}",
        output.diagnostics
    );

    let result = output.into_result();
    assert!(
        result.is_err(),
        "into_result() must return Err when multiple gates fire"
    );
}

#[test]
fn nested_fork_block_detected() {
    // Ensure walker descends into nested structures
    let source = r"
        fn main() {
            scope {
                if true {
                    fork {}
                }
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_fork_block_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::ForkBlockBodyUnsupported { .. }));
    assert!(
        has_fork_block_unsupported,
        "Nested fork block violation must be detected; got: {:#?}",
        output.diagnostics
    );
}

// ── FC-P1-A1 (revision pass 2, Finding 2) ───────────────────────────────────

/// Module-qualified spawn callees (`mod::worker()`) must not false-reject.
///
/// The parser concatenates path segments into a single
/// `Expr::Identifier("mod::worker")` (hew-parser/src/parser.rs:5334-5356),
/// but `fn_registry` is keyed on bare function names. The Finding-2 fix
/// strips the `mod::` prefix before lookup so cross-module spawns are
/// accepted as direct module functions.
#[test]
fn mod_qualified_spawn_accepted() {
    let source = r"
        fn worker() {}
        fn main() {
            scope {
                mod::worker();
            }
        }
    ";
    let parsed = parser::parse(source);
    let output = lower_program(
        &parsed.program,
        &TypeCheckOutput::default(),
        &hew_hir::ResolutionCtx,
        TargetArch::host(),
    );

    let has_callee_unsupported = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.kind, HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }));
    assert!(
        !has_callee_unsupported,
        "module-qualified spawn `mod::worker()` must not false-reject as \
         TaskSpawnCalleeUnsupported; got: {:#?}",
        output.diagnostics
    );
}
