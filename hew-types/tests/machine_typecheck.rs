//! Tests for machine type checking: registration, exhaustiveness, and pattern matching.

use hew_parser::ast::*;
use hew_types::{Checker, TypeCheckOutput};

fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = Checker::new();
    checker.check_program(&program)
}

/// Build a simple machine declaration for testing.
fn make_machine(
    name: &str,
    states: Vec<MachineState>,
    events: Vec<MachineEvent>,
    transitions: Vec<MachineTransition>,
) -> MachineDecl {
    MachineDecl {
        visibility: Visibility::Pub,
        name: name.to_string(),
        states,
        events,
        transitions,
    }
}

fn unit_state(name: &str) -> MachineState {
    MachineState {
        name: name.to_string(),
        fields: vec![],
    }
}

fn state_with_fields(name: &str, fields: Vec<(&str, &str)>) -> MachineState {
    MachineState {
        name: name.to_string(),
        fields: fields
            .into_iter()
            .map(|(fname, tname)| {
                (
                    fname.to_string(),
                    (
                        TypeExpr::Named {
                            name: tname.to_string(),
                            type_args: None,
                        },
                        0..0,
                    ),
                )
            })
            .collect(),
    }
}

fn unit_event(name: &str) -> MachineEvent {
    MachineEvent {
        name: name.to_string(),
        fields: vec![],
    }
}

fn transition(event: &str, source: &str, target: &str) -> MachineTransition {
    // Body is a boolean literal — a placeholder that always typechecks.
    MachineTransition {
        event_name: event.to_string(),
        source_state: source.to_string(),
        target_state: target.to_string(),
        body: (
            Expr::Literal(Literal::Bool(true)),
            0..0,
        ),
    }
}

fn wildcard_transition(event: &str) -> MachineTransition {
    MachineTransition {
        event_name: event.to_string(),
        source_state: "_".to_string(),
        target_state: "_".to_string(),
        body: (Expr::Identifier("self".to_string()), 0..0),
    }
}

// ── Test: well-formed machine type-checks OK ────────────────────────

#[test]
fn well_formed_machine_no_errors() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

// ── Test: machine with wildcard covers all states ───────────────────

#[test]
fn wildcard_transition_covers_all_states() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle"), unit_event("Dim")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
            wildcard_transition("Dim"), // covers (Off, Dim) and (On, Dim)
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}

// ── Test: missing transition produces error ─────────────────────────

#[test]
fn missing_transition_error() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle"), unit_event("Dim")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
            // Missing: (Off, Dim) and (On, Dim)
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        !output.errors.is_empty(),
        "expected exhaustiveness errors"
    );
    // Should have errors for both (Off, Dim) and (On, Dim)
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("does not handle event 'Dim'")),
        "expected Dim error, got: {messages:?}"
    );
}

// ── Test: machine registers type def ────────────────────────────────

#[test]
fn machine_registers_type_def() {
    let md = make_machine(
        "TcpState",
        vec![
            unit_state("Closed"),
            state_with_fields("Established", vec![("seq", "i64")]),
        ],
        vec![unit_event("Connect")],
        vec![
            transition("Connect", "Closed", "Established"),
            wildcard_transition("Connect"), // wildcard fills Established->Connect
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );

    // Machine should be registered as a type
    assert!(
        output.type_defs.contains_key("TcpState"),
        "machine type not registered"
    );
    let td = &output.type_defs["TcpState"];
    assert!(td.variants.contains_key("Closed"));
    assert!(td.variants.contains_key("Established"));

    // step() and state_name() should be registered as methods
    assert!(td.methods.contains_key("step"), "step method not registered");
    assert!(
        td.methods.contains_key("state_name"),
        "state_name method not registered"
    );
}

// ── Test: companion event enum is generated ─────────────────────────

#[test]
fn companion_event_enum_generated() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.type_defs.contains_key("LightEvent"),
        "companion event type not generated"
    );
    let event_td = &output.type_defs["LightEvent"];
    assert!(event_td.variants.contains_key("Toggle"));
}

// ── Test: state with fields registers struct variant ─────────────────

#[test]
fn state_fields_registered() {
    let md = make_machine(
        "Counter",
        vec![
            unit_state("Idle"),
            state_with_fields("Counting", vec![("value", "i64")]),
        ],
        vec![unit_event("Start"), unit_event("Stop")],
        vec![
            transition("Start", "Idle", "Counting"),
            transition("Stop", "Counting", "Idle"),
            wildcard_transition("Start"),
            wildcard_transition("Stop"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );

    let td = &output.type_defs["Counter"];
    match &td.variants["Counting"] {
        hew_types::VariantDef::Struct(fields) => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "value");
        }
        other => panic!("expected Struct variant, got: {other:?}"),
    }
}

// ── Test: duplicate explicit transition produces error ───────────────

#[test]
fn duplicate_transition_error() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "Off", "On"), // duplicate
            transition("Toggle", "On", "Off"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.iter().any(|e| e.message.contains("duplicate transition for event 'Toggle' in state 'Off'")),
        "expected duplicate transition error, got: {:?}",
        output.errors
    );
}

// ── Test: unknown state name in transition ──────────────────────────

#[test]
fn unknown_state_name_error() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "Onn", "Off"), // misspelled source
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("transition references unknown state 'Onn'")),
        "expected unknown state error, got: {messages:?}"
    );
}

// ── Test: unknown event name in transition ──────────────────────────

#[test]
fn unknown_event_name_error() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggl", "On", "Off"), // misspelled event
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("transition references unknown event 'Toggl'")),
        "expected unknown event error, got: {messages:?}"
    );
}

// ── Test: duplicate wildcard transition ─────────────────────────────

#[test]
fn duplicate_wildcard_error() {
    let md = make_machine(
        "Light",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            wildcard_transition("Toggle"),
            wildcard_transition("Toggle"), // duplicate wildcard
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("duplicate wildcard transition for event 'Toggle'")),
        "expected duplicate wildcard error, got: {messages:?}"
    );
}

// ── Test: machine with fewer than 2 states ──────────────────────────

#[test]
fn too_few_states_error() {
    let md = make_machine(
        "Broken",
        vec![unit_state("Only")],
        vec![unit_event("Ping")],
        vec![wildcard_transition("Ping")],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("must declare at least 2 states")),
        "expected min-states error, got: {messages:?}"
    );
}

// ── Test: machine with 0 events ─────────────────────────────────────

#[test]
fn zero_events_error() {
    let md = make_machine(
        "Broken",
        vec![unit_state("A"), unit_state("B")],
        vec![],
        vec![],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages.iter().any(|m| m.contains("must declare at least 1 event")),
        "expected min-events error, got: {messages:?}"
    );
}

// ── Test: valid machine still passes ────────────────────────────────

#[test]
fn valid_machine_all_checks_pass() {
    let md = make_machine(
        "Door",
        vec![unit_state("Open"), unit_state("Closed")],
        vec![unit_event("Push"), unit_event("Pull")],
        vec![
            transition("Push", "Open", "Closed"),
            transition("Push", "Closed", "Closed"),
            transition("Pull", "Closed", "Open"),
            transition("Pull", "Open", "Open"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "expected no errors, got: {:?}",
        output.errors
    );
}
