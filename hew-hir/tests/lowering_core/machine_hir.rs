//! Tests for HIR machine lowering and static checks.

use hew_hir::{HirDiagnosticKind, HirExpr, HirExprKind, HirItem, HirLiteral, HirStmtKind};

use crate::support;

fn lower(source: &str) -> hew_hir::LowerOutput {
    support::checker_pipeline::lower_through_checker(source)
}

fn first_machine_emit(expr: &HirExpr) -> Option<(usize, &[(String, HirExpr)])> {
    match &expr.kind {
        HirExprKind::MachineEmit { event_idx, fields } => Some((*event_idx, fields.as_slice())),
        HirExprKind::Block(block) | HirExprKind::GenBlock { body: block, .. } => {
            for stmt in &block.statements {
                let found = match &stmt.kind {
                    HirStmtKind::Expr(expr)
                    | HirStmtKind::Let(_, Some(expr))
                    | HirStmtKind::Return(Some(expr)) => first_machine_emit(expr),
                    HirStmtKind::Assign { value, .. } => first_machine_emit(value),
                    _ => None,
                };
                if found.is_some() {
                    return found;
                }
            }
            block.tail.as_deref().and_then(first_machine_emit)
        }
        HirExprKind::Yield { value, .. } => value.as_deref().and_then(first_machine_emit),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => first_machine_emit(condition)
            .or_else(|| first_machine_emit(then_expr))
            .or_else(|| else_expr.as_deref().and_then(first_machine_emit)),
        _ => None,
    }
}

/// A minimal two-state Moore machine with full coverage.
const TRAFFIC_LIGHT_SRC: &str = r"
machine TrafficLight {
    events {
        Tick;
    }

    state Red;
    state Green;


    on Tick: Red => Green;
    on Tick: Green => Red;
}
";

/// A Mealy machine with entry/exit blocks.
const MEALY_MACHINE_SRC: &str = r"
machine Door {
    events {
        OpenDoor;
        CloseDoor;
    }

    state Closed {
        entry { Closed }
        exit { Closed }
    }
    state Open;


    on OpenDoor: Closed => Open;
    on OpenDoor: Open => Open;
    on CloseDoor: Open => Closed;
    on CloseDoor: Closed => Closed;
}
";

#[test]
fn accept_moore_machine_lowers_to_hir() {
    let output = lower(TRAFFIC_LIGHT_SRC);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output.module.items.iter().find_map(|item| {
        if let HirItem::Machine(m) = item {
            Some(m)
        } else {
            None
        }
    });
    let machine = machine.expect("expected Machine HirItem");
    assert_eq!(machine.name, "TrafficLight");
    assert_eq!(machine.states.len(), 2);
    assert_eq!(machine.events.len(), 1);
    assert_eq!(machine.transitions.len(), 2);
    assert!(!machine.has_default);
}

#[test]
fn accept_mealy_machine_with_entry_exit_lowers() {
    let output = lower(MEALY_MACHINE_SRC);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output.module.items.iter().find_map(|item| {
        if let HirItem::Machine(m) = item {
            Some(m)
        } else {
            None
        }
    });
    let machine = machine.expect("expected Machine HirItem");
    let closed = machine.states.iter().find(|s| s.name == "Closed").unwrap();
    assert!(closed.has_entry, "Closed should have entry block");
    assert!(closed.has_exit, "Closed should have exit block");
    let open = machine.states.iter().find(|s| s.name == "Open").unwrap();
    assert!(!open.has_entry, "Open has no entry block");
}

#[test]
fn reject_machine_missing_transition_coverage() {
    // Only covers Tick: Red -> Green, not Tick: Green -> ???
    let src = r"
machine Bad {
    events {
        Tick;
        Reset;
    }

    state Red;
    state Green;


    on Tick: Red => Green;
    on Reset: Red => Red;
}
";
    let output = lower(src);
    let has_exhaustiveness_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineExhaustivenessViolation { machine_name, .. }
            if machine_name == "Bad"
        )
    });
    assert!(
        has_exhaustiveness_error,
        "expected exhaustiveness diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn accept_machine_with_default_arm_satisfies_exhaustiveness() {
    let src = r"
machine WithDefault {
    events {
        Toggle;
        Ping;
    }

    state On;
    state Off;


    on Toggle: On => Off;
    on Toggle: Off => On;

    default { self }
}
";
    let output = lower(src);
    // default arm covers Ping for both states
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn reject_machine_emit_cycle() {
    let src = r"
machine Cyclic {
    events {
        Tick;
    }

    state Active;


    on Tick: Active => Active {
        emit Tick {};
        Active
    }
}
";
    let output = lower(src);
    let has_cycle_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEmitCycle { machine_name, event_name }
            if machine_name == "Cyclic" && event_name == "Tick"
        )
    });
    assert!(
        has_cycle_error,
        "expected emit-cycle diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn accept_indirect_machine_emit_cycle_out_of_scope() {
    // out-of-scope per spec §3.11.2: Lane A checks only direct self-emit
    // (`on A` emits `A`), not graph cycles across multiple events.
    let src = r"
machine Indirect {
    events {
        A;
        B;
    }

    state Active;


    on A: Active => Active reenter {
        emit B {};
        Active
    }

    on B: Active => Active reenter {
        emit A {};
        Active
    }
}
";
    let output = lower(src);
    assert!(
        !output
            .diagnostics
            .iter()
            .any(|d| matches!(&d.kind, HirDiagnosticKind::MachineEmitCycle { .. })),
        "indirect emit cycle is intentionally not detected; got: {:?}",
        output.diagnostics
    );
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
}

#[test]
fn self_transition_flag_recorded() {
    // @reenter is required for a non-empty self-transition body.
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running { count: Int; }


    on Tick: Running => Running reenter {
        Running { count: 1 }
    }
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output.module.items.iter().find_map(|item| {
        if let HirItem::Machine(m) = item {
            Some(m)
        } else {
            None
        }
    });
    let machine = machine.expect("expected Machine HirItem");
    let tr = &machine.transitions[0];
    assert!(
        tr.is_self_transition,
        "Running -> Running should be flagged as self-transition"
    );
    assert!(
        tr.reenter,
        "transition annotated @reenter should carry reenter=true"
    );
}

// ── @reenter rule ────────────────────────────────────────────────────────────

#[test]
fn reject_self_transition_nonempty_body_without_reenter() {
    // A non-empty self-loop without reenter must be rejected.
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running { count: Int; }


    on Tick: Running => Running {
        Running { count: 1 }
    }
}
";
    let output = lower(src);
    let has_reenter_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineSelfTransitionNeedsReenter {
                machine_name,
                event_name,
                ..
            }
            if machine_name == "Counter" && event_name == "Tick"
        )
    });
    assert!(
        has_reenter_error,
        "expected MachineSelfTransitionNeedsReenter diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn accept_self_transition_empty_body_no_reenter() {
    // An empty self-loop (semicolon shorthand) requires no @reenter annotation.
    let src = r"
machine Ping {
    events {
        Noop;
    }

    state Active;


    on Noop: Active => Active;
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "expected no diagnostics for empty self-loop, got: {:?}",
        output.diagnostics
    );
    let machine = output.module.items.iter().find_map(|item| {
        if let HirItem::Machine(m) = item {
            Some(m)
        } else {
            None
        }
    });
    let machine = machine.expect("expected Machine HirItem");
    assert!(!machine.transitions[0].reenter);
}

// ── Effect-parity (entry and exit) ──────────────────────────────────────────

#[test]
fn reject_effect_parity_entry_conflict() {
    // Transition body and target entry block both write the same field.
    // Uses `this.count` (Hew keyword for self-reference in machine context).
    let src = r"
machine Conflict {
    events {
        Start;
        Stop;
    }

    state Idle;
    state Active {
        count: Int;
        entry {
            this.count = 0;
        }
    }


    on Start: Idle => Active {
        this.count = 1;
        Active { count: 1 }
    }
    on Stop: Active => Idle;
    on Start: Active => Active;
    on Stop: Idle => Idle;
}
";
    let output = lower(src);
    let has_parity_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEffectParityViolation {
                machine_name,
                field_name,
                is_entry_conflict,
                ..
            }
            if machine_name == "Conflict" && field_name == "count" && *is_entry_conflict
        )
    });
    assert!(
        has_parity_error,
        "expected entry effect-parity diagnostic, got: {:?}",
        output.diagnostics
    );
}

#[test]
fn reject_effect_parity_exit_conflict() {
    // Transition body and source exit block both write the same field.
    // Uses `this.val` (Hew keyword for self-reference in machine context).
    let src = r"
machine ExitConflict {
    events {
        Move;
        Reset;
    }

    state Source {
        val: Int;
        exit {
            this.val = 0;
        }
    }
    state Target;


    on Move: Source => Target {
        this.val = 99;
        Target
    }
    on Reset: Target => Source;
    on Move: Target => Target;
    on Reset: Source => Source;
}
";
    let output = lower(src);
    let has_exit_parity_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEffectParityViolation {
                machine_name,
                field_name,
                is_entry_conflict,
                ..
            }
            if machine_name == "ExitConflict" && field_name == "val" && !is_entry_conflict
        )
    });
    assert!(
        has_exit_parity_error,
        "expected exit effect-parity diagnostic, got: {:?}",
        output.diagnostics
    );
}

// ── Lowered transition body + entry/exit substrate ──────────────────────────

#[test]
fn transition_body_lowers_to_hir_expr_substrate() {
    // A transition with a non-trivial body (a bare state-name tail expression)
    // should populate `HirMachineTransition::body`, even though bare state-name
    // references aren't yet resolved in HIR (they survive lowering as
    // `HirExprKind::Unsupported` placeholders).
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running { count: Int; }


    on Tick: Running => Running reenter {
        Running { count: 1 }
    }
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Machine(m) = item {
                Some(m)
            } else {
                None
            }
        })
        .expect("expected Machine HirItem");
    let tr = machine
        .transitions
        .iter()
        .find(|t| t.event_name == "Tick")
        .expect("expected Tick transition");
    // The body must be present as some HIR expression form — this test is
    // structural-substrate only, no claims about kind beyond "not empty".
    assert!(
        !matches!(tr.body.kind, HirExprKind::Literal(_)),
        "non-empty transition body should lower to a non-literal HirExpr; got {:?}",
        tr.body.kind
    );
}

#[test]
fn transition_body_scopes_state_event_implicit_bindings() {
    // Lane A reserves `state` and `event` as implicit transition-body bindings.
    // HIR lowering should scope them while lowering the body so identifier reads
    // produce normal BindingRef nodes instead of unresolved-symbol diagnostics.
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running;


    on Tick: Running => Running reenter {
        state;
        event;
        Running
    }
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Machine(m) = item {
                Some(m)
            } else {
                None
            }
        })
        .expect("expected Machine HirItem");
    let tr = machine
        .transitions
        .iter()
        .find(|t| t.event_name == "Tick")
        .expect("expected Tick transition");
    let HirExprKind::Block(block) = &tr.body.kind else {
        panic!(
            "expected lowered transition body block, got {:?}",
            tr.body.kind
        );
    };
    let binding_ref_names: Vec<&str> = block
        .statements
        .iter()
        .filter_map(|stmt| {
            if let hew_hir::HirStmtKind::Expr(expr) = &stmt.kind {
                if let HirExprKind::BindingRef { name, .. } = &expr.kind {
                    return Some(name.as_str());
                }
            }
            None
        })
        .collect();
    assert_eq!(
        binding_ref_names,
        vec!["state", "event"],
        "implicit transition-body bindings should lower as BindingRef statements"
    );
    let snapshot = format!("{tr:#?}");
    assert!(
        snapshot.contains("body: HirExpr"),
        "transition debug snapshot should include body field; got:\n{snapshot}"
    );
}

#[test]
fn entry_exit_blocks_lower_to_hir_block_substrate() {
    // The Door machine has an entry and exit block on `Closed`. Both should
    // appear as `Some(HirBlock)` on the lowered state. The blocks here use
    // only constructs the body-diagnostic filter accounts for (a bare
    // state-name reference) so the test isolates substrate population from
    // the orthogonal "unrelated diagnostics still fire" axis (which is
    // covered by `entry_block_unrelated_unresolved_symbol_still_diagnoses`).
    let src = r"
machine Door {
    events {
        OpenDoor;
        CloseDoor;
    }

    state Closed {
        entry { Closed }
        exit { Closed }
    }
    state Open;


    on OpenDoor: Closed => Open;
    on OpenDoor: Open => Open;
    on CloseDoor: Open => Closed;
    on CloseDoor: Closed => Closed;
}
";
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Machine(m) = item {
                Some(m)
            } else {
                None
            }
        })
        .expect("expected Machine HirItem");
    let closed = machine.states.iter().find(|s| s.name == "Closed").unwrap();
    let entry = closed
        .entry
        .as_ref()
        .expect("Closed.entry should be lowered as Some(HirBlock)");
    let exit = closed
        .exit
        .as_ref()
        .expect("Closed.exit should be lowered as Some(HirBlock)");
    assert!(
        !entry.statements.is_empty() || entry.tail.is_some(),
        "entry block should carry at least one statement or tail expression"
    );
    assert!(
        !exit.statements.is_empty() || exit.tail.is_some(),
        "exit block should carry at least one statement or tail expression"
    );
    let open = machine.states.iter().find(|s| s.name == "Open").unwrap();
    assert!(open.entry.is_none(), "Open has no entry block");
    assert!(open.exit.is_none(), "Open has no exit block");
}

#[test]
fn transition_body_with_machine_emit_filters_only_emit_noise() {
    // `emit` now lowers to `HirExprKind::MachineEmit` directly (Slice 2).
    // The body-diagnostic filter must:
    //   1. lower the body to a `HirExpr` substrate without crashing,
    //   2. preserve the `MachineEmitCycle` diagnostic from the HIR emit-cycle
    //      walk (which supersedes the old AST summary walk), and
    //   3. not introduce extra `NotYetImplemented` noise for the
    //      `emit Tick {}` expression itself.
    let src = r"
machine Cyclic {
    events {
        Tick;
    }

    state Active;


    on Tick: Active => Active {
        emit Tick {};
        Active
    }
}
";
    let output = lower(src);
    let cycle_count = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::MachineEmitCycle { .. }))
        .count();
    assert!(cycle_count >= 1, "expected at least one MachineEmitCycle");
    let unsupported_count = output
        .diagnostics
        .iter()
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }))
        .count();
    assert_eq!(
        unsupported_count, 0,
        "Slice 1 fences body-lowering diagnostics; saw: {:?}",
        output.diagnostics
    );
}

#[test]
fn transition_body_machine_emit_resolves_event_idx_and_payload_fields() {
    let src = r#"
machine Payloads {
    events {
        First { code: Int; }
        Second { label: String; }
    }

    state Active;


    on First: Active => Active reenter {
        emit Second { label: "ok" };
        Active
    }

    on Second: Active => Active reenter {
        emit First { code: 7 };
        Active
    }
}
"#;
    let output = lower(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        output.diagnostics
    );
    let machine = output
        .module
        .items
        .iter()
        .find_map(|item| {
            if let HirItem::Machine(m) = item {
                Some(m)
            } else {
                None
            }
        })
        .expect("expected Machine HirItem");

    let first_transition = machine
        .transitions
        .iter()
        .find(|t| t.event_name == "First")
        .expect("expected First transition");
    let (second_event_idx, second_fields) =
        first_machine_emit(&first_transition.body).expect("expected emit Second in First body");
    assert_eq!(
        second_event_idx, 1,
        "Second is the second declared event, so it must lower to event_idx=1"
    );
    assert_eq!(second_fields.len(), 1);
    assert_eq!(second_fields[0].0, "label");
    assert!(
        matches!(
            &second_fields[0].1.kind,
            HirExprKind::Literal(HirLiteral::String(value)) if value == "ok"
        ),
        "expected populated label payload field, got {:?}",
        second_fields[0].1.kind
    );

    let second_transition = machine
        .transitions
        .iter()
        .find(|t| t.event_name == "Second")
        .expect("expected Second transition");
    let (first_event_idx, first_fields) =
        first_machine_emit(&second_transition.body).expect("expected emit First in Second body");
    assert_eq!(
        first_event_idx, 0,
        "First is the first declared event, so it must lower to event_idx=0"
    );
    assert_eq!(first_fields.len(), 1);
    assert_eq!(first_fields[0].0, "code");
    assert!(
        matches!(
            &first_fields[0].1.kind,
            HirExprKind::Literal(HirLiteral::Integer(7))
        ),
        "expected populated code payload field, got {:?}",
        first_fields[0].1.kind
    );
}

// ── Negative falsification: unrelated unresolved constructs still diagnose ──

#[test]
fn transition_body_unrelated_unresolved_symbol_still_diagnoses() {
    // The body-diagnostic filter must drop only the expected machine-body
    // noise (state-name identifier refs and `this`). An unrelated
    // unresolved identifier inside a transition body — here the call
    // `not_a_real_helper()` — must still produce a visible diagnostic so
    // a typo in user code cannot be silently embedded as success-shaped HIR.
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running { count: Int; }


    on Tick: Running => Running reenter {
        not_a_real_helper();
        Running { count: 1 }
    }
}
";
    let output = lower(src);
    let unresolved_helper = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "not_a_real_helper"
        )
    });
    assert!(
        unresolved_helper,
        "unrelated unresolved identifier inside a transition body must surface; \
         got: {:?}",
        output.diagnostics
    );
    // Sanity: the state-name reference (`Running`) is still allowlisted so
    // its own UnresolvedSymbol does not appear.
    let state_name_leaked = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "Running"
        )
    });
    assert!(
        !state_name_leaked,
        "state-name reference `Running` should be filtered; got: {:?}",
        output.diagnostics
    );
}

#[test]
fn entry_block_unrelated_unresolved_symbol_still_diagnoses() {
    // Same falsification for an entry block: the diagnostic filter must
    // not swallow `not_a_real_helper`.
    let src = r"
machine Door {
    events {
        OpenDoor;
        CloseDoor;
    }

    state Closed {
        entry { not_a_real_helper(); }
    }
    state Open;


    on OpenDoor: Closed => Open;
    on OpenDoor: Open => Open;
    on CloseDoor: Open => Closed;
    on CloseDoor: Closed => Closed;
}
";
    let output = lower(src);
    let unresolved_helper = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "not_a_real_helper"
        )
    });
    assert!(
        unresolved_helper,
        "unrelated unresolved identifier inside an entry block must surface; \
         got: {:?}",
        output.diagnostics
    );
}

#[test]
fn exit_block_unrelated_unresolved_symbol_still_diagnoses() {
    // Same falsification for an exit block.
    let src = r"
machine Door {
    events {
        OpenDoor;
        CloseDoor;
    }

    state Closed {
        exit { not_a_real_helper(); }
    }
    state Open;


    on OpenDoor: Closed => Open;
    on OpenDoor: Open => Open;
    on CloseDoor: Open => Closed;
    on CloseDoor: Closed => Closed;
}
";
    let output = lower(src);
    let unresolved_helper = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "not_a_real_helper"
        )
    });
    assert!(
        unresolved_helper,
        "unrelated unresolved identifier inside an exit block must surface; \
         got: {:?}",
        output.diagnostics
    );
}

#[test]
fn transition_body_non_state_name_identifier_still_diagnoses() {
    // Allowlist is narrow: only identifiers whose name matches a declared
    // state are filtered. `NotAState`, which is *not* a state name, must
    // still produce an UnresolvedSymbol diagnostic.
    let src = r"
machine Counter {
    events {
        Tick;
    }

    state Running;


    on Tick: Running => Running reenter {
        NotAState
    }
}
";
    let output = lower(src);
    let unresolved_unknown = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::UnresolvedSymbol { name } if name == "NotAState"
        )
    });
    assert!(
        unresolved_unknown,
        "identifier `NotAState` is not a declared state and must still \
         produce an UnresolvedSymbol diagnostic; got: {:?}",
        output.diagnostics
    );
}

// ── HIR emit-cycle walker descends into conditional branches ─────────────────

#[test]
fn reject_machine_emit_cycle_inside_conditional_or_match() {
    // The emit-cycle check must fire even when the self-emit is nested inside
    // an `if` branch, not just at the top level of the transition body.
    //
    // Before Slice 2 the emit-cycle walker inspected the raw AST and only
    // traversed `Expr::Block` children; a `MachineEmit` inside an `if` body
    // would be invisible to it.  After Slice 2 the walker operates on the
    // lowered `HirExpr` tree which explicitly descends into `HirExprKind::If`
    // branches — this test pins that property.
    // Use `if` as the trailing expression so it reaches `Expr::If` lowering
    // rather than `Stmt::If`. Both paths lower to `HirExprKind::If`; this test
    // exercises the expression-position path. The statement-position `else if`
    // path is covered by `reject_machine_emit_cycle_in_stmt_else_if` below.
    // The emit-cycle property depends on `collect_hir_emitted_events`
    // descending into the `HirExprKind::If` branch, not on statement lowering.
    let src = r"
machine Conditional {
    events {
        Tick;
    }

    state Active;


    on Tick: Active => Active reenter {
        if true { emit Tick {}; Active } else { Active }
    }
}
";
    let output = lower(src);
    let has_cycle_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEmitCycle { machine_name, event_name }
            if machine_name == "Conditional" && event_name == "Tick"
        )
    });
    assert!(
        has_cycle_error,
        "expected emit-cycle diagnostic for emit nested inside an if branch; \
         got: {:?}",
        output.diagnostics
    );
}

#[test]
fn reject_machine_emit_cycle_in_stmt_else_if() {
    // The emit-cycle walker must also fire when the self-emit is inside a
    // statement-position `else if` branch (i.e. `Stmt::If` with is_if=true).
    //
    // A statement-position `if` occurs when the if-chain is not the trailing
    // expression of the block — here `Active` is the trailing state expression,
    // so the `if ... else if ...` is parsed as `Stmt::If`.  The else-if arm is
    // lowered via `lower_stmt` recursion into a nested `HirExprKind::If` which
    // `collect_hir_emitted_events` then descends into normally.
    //
    // Without the fix, `else_expr` was `None` for `is_if=true` else-blocks, so
    // the self-emit in the else-if arm was invisible to the walker.
    let src = r"
machine StmtElseIf {
    events {
        Tick;
    }

    state Active;


    on Tick: Active => Active reenter {
        if false { }
        else if true { emit Tick {} }
        Active
    }
}
";
    let output = lower(src);
    let has_cycle_error = output.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEmitCycle { machine_name, event_name }
            if machine_name == "StmtElseIf" && event_name == "Tick"
        )
    });
    assert!(
        has_cycle_error,
        "expected emit-cycle diagnostic for self-emit inside a statement-position \
         else-if branch; got: {:?}",
        output.diagnostics
    );
}

// ── type-param bound carry-through (HirMachineBound) ──────────────────
//
// Inline `<T: Trait>` and `where T: Trait` predicates lower to a
// flat `Vec<HirMachineBound>` on `HirMachineDecl.type_param_bounds`.
// One entry per `(param, trait_bound)` pair in authored order;
// `WhereOrigin` preserves which form each entry came from.

fn first_machine(output: &hew_hir::LowerOutput) -> &hew_hir::HirMachineDecl {
    output
        .module
        .items
        .iter()
        .find_map(|item| match item {
            HirItem::Machine(m) => Some(m),
            _ => None,
        })
        .expect("expected exactly one machine in lowered output")
}

#[test]
fn machine_inline_bound_lowers_with_inline_origin() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { path: i64; }

impl Resource for File {
    fn close(self) {}
}

machine Holder<T: Resource> {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let output = lower(source);
    let machine = first_machine(&output);
    assert_eq!(machine.type_params, vec!["T".to_string()]);
    assert_eq!(
        machine.type_param_bounds.len(),
        1,
        "expected one bound entry from inline `<T: Resource>`, got: {:?}",
        machine.type_param_bounds
    );
    let bound = &machine.type_param_bounds[0];
    assert_eq!(bound.param, "T");
    assert_eq!(bound.trait_bound.trait_name, "Resource");
    assert!(
        matches!(bound.origin, hew_hir::WhereOrigin::Inline),
        "expected inline origin, got: {:?}",
        bound.origin
    );
}

#[test]
fn machine_where_clause_bound_lowers_with_predicate_span_origin() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { path: i64; }

impl Resource for File {
    fn close(self) {}
}

machine Holder<T> where T: Resource {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let output = lower(source);
    let machine = first_machine(&output);
    assert_eq!(machine.type_params, vec!["T".to_string()]);
    assert_eq!(machine.type_param_bounds.len(), 1);
    let bound = &machine.type_param_bounds[0];
    assert_eq!(bound.param, "T");
    assert_eq!(bound.trait_bound.trait_name, "Resource");
    match &bound.origin {
        hew_hir::WhereOrigin::WhereClause(span) => {
            assert!(
                !span.is_empty(),
                "where-clause origin must carry a non-empty predicate-LHS span"
            );
        }
        hew_hir::WhereOrigin::Inline => {
            panic!("expected WhereClause origin, got Inline");
        }
    }
}

#[test]
fn machine_inline_plus_where_bound_lowers_both_with_distinct_origin() {
    let source = r"
trait Resource {
    fn close(self);
}

trait Display {
    fn show(self);
}

type File { path: i64; }

impl Resource for File {
    fn close(self) {}
}

impl Display for File {
    fn show(self) {}
}

machine Holder<T: Resource> where T: Display {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }


    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let output = lower(source);
    let machine = first_machine(&output);
    assert_eq!(
        machine.type_param_bounds.len(),
        2,
        "expected two bound entries (one inline, one where), got: {:?}",
        machine.type_param_bounds
    );
    // Inline-first ordering per the lowering contract.
    let inline = &machine.type_param_bounds[0];
    assert_eq!(inline.param, "T");
    assert_eq!(inline.trait_bound.trait_name, "Resource");
    assert!(matches!(inline.origin, hew_hir::WhereOrigin::Inline));
    let where_bound = &machine.type_param_bounds[1];
    assert_eq!(where_bound.param, "T");
    assert_eq!(where_bound.trait_bound.trait_name, "Display");
    assert!(matches!(
        where_bound.origin,
        hew_hir::WhereOrigin::WhereClause(_)
    ));
}

#[test]
fn machine_without_bounds_lowers_empty_type_param_bounds() {
    let source = r"
machine Counter<T> {
    events {
        Start;
        Stop;
    }

    state Idle;
    state Running;


    on Start: Idle => Running { Running }
    on Stop: Running => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let output = lower(source);
    let machine = first_machine(&output);
    assert_eq!(machine.type_params, vec!["T".to_string()]);
    assert!(
        machine.type_param_bounds.is_empty(),
        "unbounded machine must lower to empty type_param_bounds, got: {:?}",
        machine.type_param_bounds
    );
}

// ---------------------------------------------------------------------------
// S-14 regression: event.undeclared_field must emit E_HIR (MachineEventFieldNotFound),
// not E_NOT_YET_IMPLEMENTED.
// ---------------------------------------------------------------------------

/// `event.wrong_field` in a machine transition body must produce
/// `MachineEventFieldNotFound` with the machine name, event name, wrong
/// field name, and the list of actual event fields — NOT `NotYetImplemented`.
#[test]
fn machine_event_undeclared_field_emits_field_not_found_not_nyi() {
    let source = r"
        machine Light {
            events {
                Switch { brightness: i64; }
            }

            state Off;
            state On;

            on Switch: Off => On {
                let _ = event.wrong_field;
                On
            }

            default { state }
        }

        fn main() {}
    ";
    let output = lower(source);

    // Must emit exactly MachineEventFieldNotFound — not NotYetImplemented.
    let nyi = output
        .diagnostics
        .iter()
        .any(|d| matches!(&d.kind, HirDiagnosticKind::NotYetImplemented { .. }));
    assert!(
        !nyi,
        "event.wrong_field must NOT emit NotYetImplemented; got: {:?}",
        output.diagnostics
    );

    let field_not_found = output.diagnostics.iter().find(|d| {
        matches!(
            &d.kind,
            HirDiagnosticKind::MachineEventFieldNotFound {
                field_name,
                ..
            } if field_name == "wrong_field"
        )
    });
    let diag = field_not_found.unwrap_or_else(|| {
        panic!(
            "expected MachineEventFieldNotFound for `wrong_field`; got: {:?}",
            output.diagnostics
        )
    });

    // The available-fields list must include the declared field.
    match &diag.kind {
        HirDiagnosticKind::MachineEventFieldNotFound {
            machine_name,
            event_name,
            field_name,
            available_fields,
        } => {
            assert_eq!(machine_name, "Light");
            assert_eq!(field_name, "wrong_field");
            assert!(
                event_name.contains("Switch") || event_name.contains("Light"),
                "event_name should identify the triggering event; got `{event_name}`"
            );
            assert!(
                available_fields.contains(&"brightness".to_string()),
                "available_fields must include declared field `brightness`; got: {available_fields:?}"
            );
        }
        other => panic!("unexpected diagnostic kind: {other:?}"),
    }
}
