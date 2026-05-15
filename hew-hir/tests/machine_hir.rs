//! Tests for HIR machine lowering and static checks.

use hew_hir::{lower_program, HirDiagnosticKind, HirItem, ResolutionCtx};

fn lower(source: &str) -> hew_hir::LowerOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    lower_program(&parsed.program, &ResolutionCtx)
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
    // No exhaustiveness error (single state, single event, covered)
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
}
