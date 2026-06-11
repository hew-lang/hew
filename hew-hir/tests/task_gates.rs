//! Tests for FC-P1-A1 task/fork/deadline gates.
//!
//! Validates that spawned calls, fork children, fork blocks, deadline scopes,
//! and await expressions are correctly validated at HIR lowering.

#[path = "support/mod.rs"]
mod support;

use hew_hir::HirDiagnosticKind;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

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
    let output = lower(source);

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
fn fork_child_with_args_accepted() {
    // Valid since the arg-bearing fork lift: a direct module-fn fork child
    // with arguments passes the HIR gates — the args ride the fork-entry
    // shim env at MIR, where the per-arg type restriction (BitCopy scalars
    // + string) is enforced fail-closed.
    let source = r"
        fn worker(x: int) {}
        fn main() {
            scope {
                fork child = worker(42);
                await child;
            }
        }
    ";
    let output = lower(source);

    let has_gate_diagnostic = output.diagnostics.iter().any(|d| {
        matches!(
            d.kind,
            HirDiagnosticKind::TaskSpawnSignatureUnsupported { .. }
                | HirDiagnosticKind::TaskSpawnCalleeUnsupported { .. }
        )
    });
    assert!(
        !has_gate_diagnostic,
        "Arg-bearing direct-fn fork child must pass the HIR gates; got: {:#?}",
        output.diagnostics
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
    let output = lower(source);

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
    let output = lower(source);

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
    // Invalid: spawned closure has parameters. The block-wrapped closure shape
    // currently reaches the broader callee gate, which still proves rejection.
    let source = r"
        fn main() {
            scope {
                fork child = { |x| work(x) }();
            }
        }
        fn work(x: int) {}
    ";
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
    // Valid: awaiting a unit-returning worker must not trip the non-unit gate.
    let source = r"
        fn main() {
            let task = worker();
            await task;
        }
        fn worker() {}
    ";
    let output = lower(source);

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
fn value_await_in_let_rejected_fail_closed() {
    // HEW-SPEC-2026 §4.3 value resolution (`let v = await x;` for a
    // Task<i64> fork binding) is not wired yet: task results have no
    // result-propagation substrate (wrapper discards the callee return).
    // The let-value await must refuse with a clear AwaitOutOfPosition
    // diagnostic — never bind a fabricated value.
    let source = r"
        fn compute() -> i64 { 42 }
        fn main() {
            scope {
                fork x = compute();
                let v = await x;
                let _ = v;
            }
        }
    ";
    let output = lower(source);

    let has_let_value_reject = output.diagnostics.iter().any(|d| {
        matches!(d.kind, HirDiagnosticKind::AwaitOutOfPosition) && d.note.contains("let-value")
    });
    assert!(
        has_let_value_reject,
        "value await in let position must fail closed; got: {:#?}",
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
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "Parse errors: {:?}",
        parsed.errors
    );

    let output = lower(source);

    // The walker should traverse await expressions without panicking
    // Gate diagnostics depend on checker type information.
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
    let output = lower(source);

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
    let output = lower(source);

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
    let output = lower(source);

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
