//! Tests for HIR machine lowering and static checks.

use hew_hir::{lower_program, HirDiagnosticKind, HirExprKind, HirItem, ResolutionCtx};
use hew_types::TypeCheckOutput;

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(&parsed.program, &TypeCheckOutput::default(), &ResolutionCtx)
}

/// A minimal two-state Moore machine with full coverage.
const TRAFFIC_LIGHT_SRC: &str = r"
machine TrafficLight {
    state Red;
    state Green;

    event Tick;

    on Tick: Red -> Green;
    on Tick: Green -> Red;
}
";

/// A Mealy machine with entry/exit blocks.
const MEALY_MACHINE_SRC: &str = r"
machine Door {
    state Closed {
        entry { log(closed_entered); }
        exit { log(closed_exited); }
    }
    state Open;

    event OpenDoor;
    event CloseDoor;

    on OpenDoor: Closed -> Open;
    on OpenDoor: Open -> Open;
    on CloseDoor: Open -> Closed;
    on CloseDoor: Closed -> Closed;
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
    state Red;
    state Green;

    event Tick;
    event Reset;

    on Tick: Red -> Green;
    on Reset: Red -> Red;
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
    state On;
    state Off;

    event Toggle;
    event Ping;

    on Toggle: On -> Off;
    on Toggle: Off -> On;

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
    state Active;

    event Tick;

    on Tick: Active -> Active {
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
fn self_transition_flag_recorded() {
    // @reenter is required for a non-empty self-transition body.
    let src = r"
machine Counter {
    state Running { count: Int; }

    event Tick;

    on Tick: Running -> Running @reenter {
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
    // A non-empty self-loop without @reenter must be rejected.
    let src = r"
machine Counter {
    state Running { count: Int; }

    event Tick;

    on Tick: Running -> Running {
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
    state Active;

    event Noop;

    on Noop: Active -> Active;
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
    state Idle;
    state Active {
        count: Int;
        entry {
            this.count = 0;
        }
    }

    event Start;
    event Stop;

    on Start: Idle -> Active {
        this.count = 1;
        Active { count: 1 }
    }
    on Stop: Active -> Idle;
    on Start: Active -> Active;
    on Stop: Idle -> Idle;
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
    state Source {
        val: Int;
        exit {
            this.val = 0;
        }
    }
    state Target;

    event Move;
    event Reset;

    on Move: Source -> Target {
        this.val = 99;
        Target
    }
    on Reset: Target -> Source;
    on Move: Target -> Target;
    on Reset: Source -> Source;
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

// ── Slice 1 substrate: transition body + entry/exit lowered to HIR ──────────

#[test]
fn transition_body_lowers_to_hir_expr_substrate() {
    // A transition with a non-trivial body (a bare state-name tail expression)
    // should populate `HirMachineTransition::body` as Slice 1 substrate, even
    // though bare state-name references aren't yet resolved in HIR (they
    // lower to `HirExprKind::Unsupported` placeholders — see Slice 2).
    let src = r"
machine Counter {
    state Running { count: Int; }

    event Tick;

    on Tick: Running -> Running @reenter {
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
    // The body must be present as some HIR expression form — slice 1 is
    // structural substrate only, no claims about kind beyond "not empty".
    assert!(
        !matches!(tr.body.kind, HirExprKind::Literal(_)),
        "non-empty transition body should lower to a non-literal HirExpr; got {:?}",
        tr.body.kind
    );
}

#[test]
fn entry_exit_blocks_lower_to_hir_block_substrate() {
    // The Door machine has an entry and exit block on `Closed`. Both should
    // appear as `Some(HirBlock)` on the lowered state. Constructs inside that
    // can't yet round-trip through HIR (e.g. unresolved `log` call) are fenced
    // so existing diagnostics stay quiet.
    let src = r"
machine Door {
    state Closed {
        entry { log(closed_entered); }
        exit { log(closed_exited); }
    }
    state Open;

    event OpenDoor;
    event CloseDoor;

    on OpenDoor: Closed -> Open;
    on OpenDoor: Open -> Open;
    on CloseDoor: Open -> Closed;
    on CloseDoor: Closed -> Closed;
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
fn transition_body_with_machine_emit_is_fenced_but_cycle_diagnostic_fires() {
    // Bodies that include `emit` are not yet routed through `HirExprKind`
    // (Slice 2 owns that). Slice 1 must:
    //   1. still lower the body to a `HirExpr` substrate without crashing,
    //   2. preserve the Lane A `MachineEmitCycle` diagnostic (from the AST
    //      summary walk), and
    //   3. not introduce extra "unsupported" diagnostic noise for the emit.
    let src = r"
machine Cyclic {
    state Active;

    event Tick;

    on Tick: Active -> Active {
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
        .filter(|d| matches!(&d.kind, HirDiagnosticKind::CutoverUnsupported { .. }))
        .count();
    assert_eq!(
        unsupported_count, 0,
        "Slice 1 fences body-lowering diagnostics; saw: {:?}",
        output.diagnostics
    );
}
