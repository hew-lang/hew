//! Tests for machine type checking: registration, exhaustiveness, and pattern matching.

use hew_parser::ast::*;
use hew_parser::module::{Module, ModuleGraph, ModuleId};
mod common;

use common::{isolated_checker, typecheck_isolated};
use hew_types::error::TypeErrorKind;
use hew_types::{MachineMethodKind, Ty, TypeCheckOutput};

fn check_items(items: Vec<Spanned<Item>>) -> TypeCheckOutput {
    let program = Program {
        module_graph: None,
        items,
        module_doc: None,
    };
    let mut checker = isolated_checker();
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
        type_params: vec![],
        const_params: vec![],
        where_clause: None,
        has_default: false,
        states,
        events,
        emits: vec![],
        transitions,
        composite_groups: vec![],
    }
}

fn unit_state(name: &str) -> MachineState {
    MachineState {
        name: name.to_string(),
        fields: vec![],
        entry: None,
        exit: None,
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
        entry: None,
        exit: None,
    }
}

fn unit_event(name: &str) -> MachineEvent {
    MachineEvent {
        name: name.to_string(),
        fields: vec![],
    }
}

fn transition(event: &str, source: &str, target: &str) -> MachineTransition {
    // Body is a bare `state` identifier — the implicit self-binding that is
    // always in scope in a transition body and always has the machine type.
    // Previously this used `true` (a bool literal), which passed through
    // synthesize (result discarded).  Now that transition bodies are
    // check_against'd against the machine type, the body must actually have
    // the machine type.
    MachineTransition {
        event_name: event.to_string(),
        source_state: source.to_string(),
        target_state: target.to_string(),
        event_bindings: vec![],
        composite_prelude_len: 0,
        guard: None,
        body: (Expr::Identifier("state".to_string()), 0..0),
        reenter: false,
    }
}

fn wildcard_transition(event: &str) -> MachineTransition {
    MachineTransition {
        event_name: event.to_string(),
        source_state: "_".to_string(),
        target_state: "_".to_string(),
        event_bindings: vec![],
        composite_prelude_len: 0,
        guard: None,
        body: (Expr::Identifier("state".to_string()), 0..0),
        reenter: false,
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
    assert!(!output.errors.is_empty(), "expected exhaustiveness errors");
    // Should have errors for both (Off, Dim) and (On, Dim)
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages
            .iter()
            .any(|m| m.contains("does not handle event 'Dim'")),
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
    assert!(
        td.methods.contains_key("step"),
        "step method not registered"
    );
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
        output.errors.iter().any(|e| e
            .message
            .contains("duplicate transition for event 'Toggle' in state 'Off'")),
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
        messages
            .iter()
            .any(|m| m.contains("transition references unknown state 'Onn'")),
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
        messages
            .iter()
            .any(|m| m.contains("transition references unknown event 'Toggl'")),
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
        messages
            .iter()
            .any(|m| m.contains("duplicate wildcard transition for event 'Toggle'")),
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
        messages
            .iter()
            .any(|m| m.contains("must declare at least 2 states")),
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
        messages
            .iter()
            .any(|m| m.contains("must declare at least 1 event")),
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

// ── Property-based: exhaustive 3×3 machine ─────────────────────────

#[test]
fn exhaustive_3x3_machine_ok() {
    let states = vec![unit_state("A"), unit_state("B"), unit_state("C")];
    let events = vec![unit_event("X"), unit_event("Y"), unit_event("Z")];
    let transitions = vec![
        transition("X", "A", "B"),
        transition("Y", "A", "C"),
        transition("Z", "A", "A"),
        transition("X", "B", "A"),
        transition("Y", "B", "B"),
        transition("Z", "B", "C"),
        transition("X", "C", "C"),
        transition("Y", "C", "A"),
        transition("Z", "C", "B"),
    ];
    let md = make_machine("M", states, events, transitions);
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "fully exhaustive 3×3 machine should have no errors: {:?}",
        output.errors
    );
}

// ── Property-based: removing any single transition causes error ─────

#[test]
fn property_removal_of_any_transition_causes_error() {
    let state_names = ["A", "B", "C"];
    let event_names = ["X", "Y"];
    let n_transitions = state_names.len() * event_names.len(); // 6

    let mut transitions = Vec::new();
    for s in &state_names {
        for e in &event_names {
            transitions.push((*e, *s));
        }
    }

    // Full machine should pass
    let full_trans: Vec<_> = transitions
        .iter()
        .map(|(e, s)| transition(e, s, "A"))
        .collect();
    let md = make_machine(
        "M",
        state_names.iter().map(|n| unit_state(n)).collect(),
        event_names.iter().map(|n| unit_event(n)).collect(),
        full_trans,
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "full machine should pass: {:?}",
        output.errors
    );

    // Removing any single transition should fail
    for i in 0..n_transitions {
        let mut reduced: Vec<_> = transitions
            .iter()
            .map(|(e, s)| transition(e, s, "A"))
            .collect();
        reduced.remove(i);
        let md = make_machine(
            "M",
            state_names.iter().map(|n| unit_state(n)).collect(),
            event_names.iter().map(|n| unit_event(n)).collect(),
            reduced,
        );
        let output = check_items(vec![(Item::Machine(md), 0..0)]);
        assert!(
            !output.errors.is_empty(),
            "removing transition {i} ({},{}) should cause an error",
            transitions[i].0,
            transitions[i].1
        );
    }
}

// ── Property-based: wildcard covers remaining states ────────────────

#[test]
fn wildcard_covers_remaining_3_state() {
    let md = make_machine(
        "M",
        vec![unit_state("A"), unit_state("B"), unit_state("C")],
        vec![unit_event("X"), unit_event("Y")],
        vec![
            transition("X", "A", "B"),
            wildcard_transition("X"), // covers B and C for event X
            wildcard_transition("Y"), // covers all 3 for event Y
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "wildcards should cover remaining states: {:?}",
        output.errors
    );
}

// ── Property-based: missing single transition in 3×3 detected ───────

#[test]
fn missing_single_transition_detected_3x3() {
    // 3 states × 3 events = 9 transitions, remove the one for (B, Y)
    let md = make_machine(
        "M",
        vec![unit_state("A"), unit_state("B"), unit_state("C")],
        vec![unit_event("X"), unit_event("Y"), unit_event("Z")],
        vec![
            transition("X", "A", "B"),
            transition("Y", "A", "C"),
            transition("Z", "A", "A"),
            transition("X", "B", "A"),
            /* MISSING: Y,B */ transition("Z", "B", "C"),
            transition("X", "C", "C"),
            transition("Y", "C", "A"),
            transition("Z", "C", "B"),
        ],
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        !output.errors.is_empty(),
        "missing transition must be detected"
    );
    let msg = format!("{:?}", output.errors);
    assert!(
        msg.contains('B') && msg.contains('Y'),
        "error should mention state B and event Y: {msg}"
    );
}

// ── Property-based: transition count = states × events ──────────────

#[test]
fn transition_count_equals_states_times_events() {
    let state_names = ["S0", "S1", "S2", "S3"];
    let event_names = ["E0", "E1", "E2"];
    let expected = state_names.len() * event_names.len(); // 12

    let mut transitions = Vec::new();
    for s in &state_names {
        for e in &event_names {
            transitions.push(transition(e, s, "S0"));
        }
    }
    assert_eq!(transitions.len(), expected);

    let md = make_machine(
        "Big",
        state_names.iter().map(|n| unit_state(n)).collect(),
        event_names.iter().map(|n| unit_event(n)).collect(),
        transitions,
    );
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "4×3 fully exhaustive machine should pass: {:?}",
        output.errors
    );
}

// ── Test: generic machine preserves type params in TypeDef ──────────

fn make_generic_machine(name: &str, type_params: &[&str]) -> MachineDecl {
    MachineDecl {
        visibility: Visibility::Pub,
        name: name.to_string(),
        type_params: type_params
            .iter()
            .map(|n| TypeParam {
                name: (*n).to_string(),
                bounds: vec![],
            })
            .collect(),
        const_params: vec![],
        where_clause: None,
        has_default: false,
        states: vec![unit_state("Idle"), unit_state("Active")],
        events: vec![unit_event("Start"), unit_event("Stop")],
        emits: vec![],
        transitions: vec![
            transition("Start", "Idle", "Active"),
            transition("Stop", "Active", "Idle"),
            wildcard_transition("Start"),
            wildcard_transition("Stop"),
        ],
        composite_groups: vec![],
    }
}

#[test]
fn generic_machine_type_params_survive_registration() {
    let md = make_generic_machine("Worker", &["T"]);
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "generic machine should type-check without errors, got: {:?}",
        output.errors
    );
    let td = output
        .type_defs
        .get("Worker")
        .expect("Worker should be registered as a type");
    assert_eq!(
        td.type_params,
        vec!["T".to_string()],
        "generic machine type param T must survive into TypeDef"
    );
}

#[test]
fn generic_machine_multi_params_survive_registration() {
    let md = make_generic_machine("Pipeline", &["In", "Out"]);
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "multi-param generic machine should type-check, got: {:?}",
        output.errors
    );
    let td = output
        .type_defs
        .get("Pipeline")
        .expect("Pipeline should be registered as a type");
    assert_eq!(
        td.type_params,
        vec!["In".to_string(), "Out".to_string()],
        "multi-param generic machine type params must survive into TypeDef"
    );
}

#[test]
fn non_generic_machine_type_params_empty() {
    // Regression: non-generic machines must still register with empty type_params.
    let md = make_generic_machine("Light", &[]);
    let output = check_items(vec![(Item::Machine(md), 0..0)]);
    assert!(
        output.errors.is_empty(),
        "non-generic machine should type-check, got: {:?}",
        output.errors
    );
    let td = output
        .type_defs
        .get("Light")
        .expect("Light should be registered as a type");
    assert!(
        td.type_params.is_empty(),
        "non-generic machine must have empty type_params, got: {:?}",
        td.type_params
    );
}

#[test]
fn machine_step_dispatch() {
    let output = typecheck_isolated(
        r"
        machine Light {
            events {
                Toggle;
            }

            state Off;
            state On;
            on Toggle: Off => On;
            on Toggle: On => Off;
        }

        fn main() {
            var light: Light = Light::Off;
            light.step(LightEvent::Toggle);
            let name: string = light.state_name();
            let _ = name;
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "machine step/state_name dispatch should type-check, got: {:?}",
        output.errors
    );
    assert!(
        output.machine_method_dispatch.values().any(|kind| {
            matches!(
                kind,
                MachineMethodKind::Step { machine_name } if machine_name == "Light"
            )
        }),
        "expected a checker-owned Light::step dispatch entry, got: {:?}",
        output.machine_method_dispatch
    );
    assert!(
        output.machine_method_dispatch.values().any(|kind| {
            matches!(
                kind,
                MachineMethodKind::StateName { machine_name } if machine_name == "Light"
            )
        }),
        "expected a checker-owned Light::state_name dispatch entry, got: {:?}",
        output.machine_method_dispatch
    );

    let step_sig = &output.type_defs["Light"].methods["step"];
    assert_eq!(
        step_sig.params,
        vec![Ty::Named {
            builtin: None,
            name: "LightEvent".to_string(),
            args: vec![],
        }],
        "step must dispatch through the nominal companion event type"
    );
    assert_eq!(
        output.type_defs["Light"].methods["state_name"].return_type,
        Ty::String
    );
}

#[test]
fn machine_step_suppresses_unused_mut_warning() {
    let output = typecheck_isolated(
        r"
        machine Light {
            events {
                Toggle;
            }

            state Off;
            state On;
            on Toggle: Off => On;
            on Toggle: On => Off;
        }

        fn main() {
            var light: Light = Light::Off;
            light.step(LightEvent::Toggle);
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "machine .step() on var receiver should type-check, got: {:?}",
        output.errors
    );
    assert!(
        !output
            .warnings
            .iter()
            .any(|w| w.message.contains("never reassigned")),
        ".step() must mark its receiver as written so UnusedMut is not warned, got: {:?}",
        output.warnings
    );
}

#[test]
fn machine_step_on_let_receiver_is_rejected() {
    let output = typecheck_isolated(
        r"
        machine Light {
            events {
                Toggle;
            }

            state Off;
            state On;
            on Toggle: Off => On;
            on Toggle: On => Off;
        }

        fn main() {
            let light: Light = Light::Off;
            light.step(LightEvent::Toggle);
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == hew_types::error::TypeErrorKind::MutabilityError),
        "Expected MutabilityError for .step() on let-bound receiver, got errors: {:?}",
        output.errors
    );
}

#[test]
fn machine_state_pattern_match_uses_variant_infrastructure() {
    let output = typecheck_isolated(
        r"
        machine TcpState {
            events {
                Connect;
                Disconnect;
            }

            state Closed;
            state Established { seq: i64; }
            on Connect: Closed => Established { seq: 1 }
            on Connect: Established => Established { seq: state.seq }
            on Disconnect: Closed => Closed;
            on Disconnect: Established => Closed;
        }

        fn seq_or_zero(state: TcpState) -> i64 {
            match state {
                TcpState::Closed => 0,
                TcpState::Established { seq } => seq,
            }
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "machine state pattern match should type-check, got: {:?}",
        output.errors
    );
    let matched_variants: Vec<_> = output
        .pattern_resolutions
        .values()
        .filter_map(|resolution| resolution.variant_match.as_ref())
        .filter(|variant_match| variant_match.type_name == "TcpState")
        .map(|variant_match| variant_match.variant_name.as_str())
        .collect();
    assert!(
        matched_variants.contains(&"Closed") && matched_variants.contains(&"Established"),
        "expected machine state variants to resolve through enum-pattern metadata, got: {:?}",
        output.pattern_resolutions
    );
}

#[test]
fn generic_machine_threads_type_params_into_state_event_and_step() {
    let output = typecheck_isolated(
        r"
        machine Lifecycle<T> {
            events {
                Load { value: T; }
                Reset;
            }

            state Empty;
            state Loaded { value: T; }
            on Load: Empty => Loaded { value: event.value }
            on Load: Loaded => Loaded { value: event.value }
            on Reset: Empty => Empty;
            on Reset: Loaded => Empty;
        }

        fn main() {
            var lifecycle: Lifecycle<i64> = Lifecycle::Loaded { value: 1 };
            lifecycle.step(LifecycleEvent::Load { value: 2 });
            let value: i64 = match lifecycle {
                Lifecycle::Empty => 0,
                Lifecycle::Loaded { value } => value,
            };
            let _ = value;
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "generic machine state/event threading should type-check, got: {:?}",
        output.errors
    );

    let machine_td = &output.type_defs["Lifecycle"];
    assert_eq!(machine_td.type_params, vec!["T".to_string()]);
    match &machine_td.variants["Loaded"] {
        hew_types::VariantDef::Struct(fields) => assert_eq!(
            fields,
            &vec![(
                "value".to_string(),
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
            )]
        ),
        other => panic!("expected Loaded to be a struct variant, got: {other:?}"),
    }

    let event_td = &output.type_defs["LifecycleEvent"];
    assert_eq!(event_td.type_params, vec!["T".to_string()]);
    match &event_td.variants["Load"] {
        hew_types::VariantDef::Struct(fields) => assert_eq!(
            fields,
            &vec![(
                "value".to_string(),
                Ty::Named {
                    builtin: None,
                    name: "T".to_string(),
                    args: vec![],
                },
            )]
        ),
        other => panic!("expected Load to be a struct variant, got: {other:?}"),
    }

    assert_eq!(
        machine_td.methods["step"].params,
        vec![Ty::Named {
            builtin: None,
            name: "LifecycleEvent".to_string(),
            args: vec![Ty::Named {
                builtin: None,
                name: "T".to_string(),
                args: vec![],
            }],
        }]
    );
}

#[test]
fn user_defined_type_does_not_inherit_machine_methods() {
    let output = typecheck_isolated(
        r"
        type Light {
            value: i64
        }

        fn main() {
            let light: Light = Light { value: 1 };
            light.state_name();
        }
        ",
    );

    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::UndefinedMethod),
        "plain user-defined types must not inherit the machine method surface, got: {:?}",
        output.errors
    );
}

#[test]
fn machine_event_match_outside_transition_rejected() {
    let output = typecheck_isolated(
        r"
        machine Light {
            events {
                Toggle;
            }

            state Off;
            state On;
            on Toggle: Off => On;
            on Toggle: On => Off;
        }

        fn main() {
            let event: LightEvent = LightEvent::Toggle;
            let _: i64 = match event {
                LightEvent::Toggle => 1,
            };
        }
        ",
    );

    assert!(
        output.errors.iter().any(|error| {
            error.kind == TypeErrorKind::InvalidOperation
                && error
                    .message
                    .contains("outside a transition body is not supported")
        }),
        "event enum matching outside transition bodies is out of scope for this slice; got: {:?}",
        output.errors
    );
}

// ── Module-graph import-scope tests ─────────────────────────────────────────
//
// These tests exercise the `collect_types` non-root-module loop, which is the
// code path exercised when a machine is defined in a library module and then
// loaded as a dependency via the module graph. This is distinct from the
// `Import { resolved_items }` path.
//
// Regression for: imported machine state/event binding tables not populated
// when module is in the non-root loop of `collect_types`.

fn module_node_with_items(id: &str, items: Vec<Spanned<Item>>) -> Module {
    Module {
        id: ModuleId::new(vec![id.to_string()]),
        items,
        imports: vec![],
        source_paths: Vec::new(),
        doc: None,
    }
}

/// Machine in a non-root module-graph node: after `collect_types`, the
/// machine's `TypeDef` and unit-state constructor `fn_sig` must be registered
/// so that transition bodies in the same module can reference bare state names.
#[test]
fn imported_machine_unit_state_constructor_resolves() {
    // Build a 2-state machine in a non-root module.
    let md = make_machine(
        "Traffic",
        vec![unit_state("Red"), unit_state("Green")],
        vec![unit_event("Change")],
        vec![
            transition("Change", "Red", "Green"),
            transition("Change", "Green", "Red"),
        ],
    );

    let mut graph = ModuleGraph::new(ModuleId::new(vec!["root".to_string()]));
    graph
        .add_module(module_node_with_items(
            "lights",
            vec![(Item::Machine(md), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items("root", vec![]))
        .unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "machine in non-root module must type-check without errors, got: {:?}",
        output.errors
    );

    // State constructors must be registered in fn_sigs so that transition bodies
    // that reference bare state names can resolve them.
    assert!(
        output.fn_sigs.contains_key("Red"),
        "unit state 'Red' constructor must be registered in fn_sigs for the import path"
    );
    assert!(
        output.fn_sigs.contains_key("Green"),
        "unit state 'Green' constructor must be registered in fn_sigs for the import path"
    );

    // Machine TypeDef must be populated with state variants
    assert!(
        output.type_defs.contains_key("Traffic"),
        "machine type 'Traffic' must be registered in type_defs for the import path"
    );
    let td = &output.type_defs["Traffic"];
    assert!(
        td.variants.contains_key("Red"),
        "state variant 'Red' must appear in Traffic TypeDef"
    );
    assert!(
        td.variants.contains_key("Green"),
        "state variant 'Green' must appear in Traffic TypeDef"
    );

    // Companion event enum must also be registered
    assert!(
        output.type_defs.contains_key("TrafficEvent"),
        "companion event enum 'TrafficEvent' must be registered for the import path"
    );
}

/// Payload state constructors (struct variants) resolve in a non-root module.
/// Regression: `Running { handle: event.handle }` style bodies failed with
/// "undefined variable" when the machine wasn't registered in `collect_types`'
/// non-root loop.
#[test]
fn imported_machine_payload_state_struct_literal_resolves() {
    // Machine with a payload state — the struct-literal transition body
    // `Counting { value: 0 }` requires the variant to be registered.
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

    let mut graph = ModuleGraph::new(ModuleId::new(vec!["root".to_string()]));
    graph
        .add_module(module_node_with_items(
            "counters",
            vec![(Item::Machine(md), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items("root", vec![]))
        .unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "payload-state machine in non-root module must type-check without errors: {:?}",
        output.errors
    );

    let td = output
        .type_defs
        .get("Counter")
        .expect("Counter type must be registered in the non-root module path");
    match &td.variants["Counting"] {
        hew_types::VariantDef::Struct(fields) => {
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].0, "value");
        }
        other => panic!("expected Struct variant for 'Counting', got: {other:?}"),
    }
}

/// `check_machine_exhaustiveness` is invoked for machines in non-root modules.
/// Without the fix, the machine was never registered and `check_item` was
/// still called with an unregistered machine — exhaustiveness errors may be
/// suppressed because state/event tables were empty.
#[test]
fn imported_machine_exhaustiveness_runs() {
    // Deliberately incomplete machine: (Off, Dim) and (On, Dim) are missing.
    let md = make_machine(
        "Lamp",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle"), unit_event("Dim")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
            // Missing: (Off, Dim) and (On, Dim) — must be reported
        ],
    );

    let mut graph = ModuleGraph::new(ModuleId::new(vec!["root".to_string()]));
    graph
        .add_module(module_node_with_items(
            "lighting",
            vec![(Item::Machine(md), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items("root", vec![]))
        .unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        !output.errors.is_empty(),
        "exhaustiveness check must run for machines in non-root modules"
    );
    let messages: Vec<_> = output.errors.iter().map(|e| e.message.clone()).collect();
    assert!(
        messages
            .iter()
            .any(|m| m.contains("does not handle event 'Dim'")),
        "expected missing-Dim exhaustiveness error for imported machine, got: {messages:?}"
    );
}

/// Generic machine in a non-root module: type params survive into `TypeDef`.
/// Mirrors the Lifecycle<T> shape from the S8 stdlib.
#[test]
fn imported_generic_machine_type_params_survive_registration() {
    let md = MachineDecl {
        visibility: Visibility::Pub,
        name: "Worker".to_string(),
        type_params: vec![TypeParam {
            name: "T".to_string(),
            bounds: vec![],
        }],
        const_params: vec![],
        where_clause: None,
        has_default: false,
        states: vec![unit_state("Idle"), unit_state("Active")],
        events: vec![unit_event("Start"), unit_event("Stop")],
        emits: vec![],
        transitions: vec![
            transition("Start", "Idle", "Active"),
            transition("Stop", "Active", "Idle"),
            wildcard_transition("Start"),
            wildcard_transition("Stop"),
        ],
        composite_groups: vec![],
    };

    let mut graph = ModuleGraph::new(ModuleId::new(vec!["root".to_string()]));
    graph
        .add_module(module_node_with_items(
            "workers",
            vec![(Item::Machine(md), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items("root", vec![]))
        .unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "generic machine in non-root module must type-check without errors: {:?}",
        output.errors
    );

    let td = output
        .type_defs
        .get("Worker")
        .expect("generic machine 'Worker' must be registered via the non-root module path");
    assert_eq!(
        td.type_params,
        vec!["T".to_string()],
        "generic type param T must survive into TypeDef when registered via module graph"
    );
    let event_td = output
        .type_defs
        .get("WorkerEvent")
        .expect("companion event enum 'WorkerEvent' must be registered");
    assert_eq!(
        event_td.type_params,
        vec!["T".to_string()],
        "companion event enum must carry the same type param T"
    );
}

/// Audit: two modules in the graph each define a different machine — no
/// cross-module collision in `fn_sigs` or `type_defs`.
#[test]
fn two_modules_with_different_machines_no_collision() {
    let md_a = make_machine(
        "Alpha",
        vec![unit_state("Off"), unit_state("On")],
        vec![unit_event("Toggle")],
        vec![
            transition("Toggle", "Off", "On"),
            transition("Toggle", "On", "Off"),
        ],
    );
    let md_b = make_machine(
        "Beta",
        vec![unit_state("Idle"), unit_state("Busy")],
        vec![unit_event("Run"), unit_event("Done")],
        vec![
            transition("Run", "Idle", "Busy"),
            transition("Done", "Busy", "Idle"),
            wildcard_transition("Run"),
            wildcard_transition("Done"),
        ],
    );

    let root_id = ModuleId::new(vec!["root".to_string()]);
    let mut graph = ModuleGraph::new(root_id.clone());
    graph
        .add_module(module_node_with_items(
            "mod_a",
            vec![(Item::Machine(md_a), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items(
            "mod_b",
            vec![(Item::Machine(md_b), 0..0)],
        ))
        .unwrap();
    graph
        .add_module(module_node_with_items("root", vec![]))
        .unwrap();
    graph.compute_topo_order().expect("no cycles");

    let program = Program {
        items: vec![],
        module_doc: None,
        module_graph: Some(graph),
    };
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);

    assert!(
        output.errors.is_empty(),
        "two machines in separate non-root modules must not collide: {:?}",
        output.errors
    );

    assert!(
        output.type_defs.contains_key("Alpha"),
        "Alpha must be registered"
    );
    assert!(
        output.type_defs.contains_key("Beta"),
        "Beta must be registered"
    );
    assert!(
        output.fn_sigs.contains_key("Off"),
        "Alpha::Off constructor must be registered"
    );
    assert!(
        output.fn_sigs.contains_key("Idle"),
        "Beta::Idle constructor must be registered"
    );
}

/// Gate program: a machine declaring `<T: Resource>` parses and
/// type-checks cleanly. Bound enforcement is parsed-only at this layer;
/// downstream slices will validate bound resolution against trait
/// definitions.
#[test]
fn machine_with_trait_bound_parses_and_checks() {
    let source = r"
trait Resource {
    fn close(self);
}

machine Lifecycle<T: Resource> {
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
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "trait-bounded machine should type-check, got: {:?}",
        output.errors
    );
    let td = output
        .type_defs
        .get("Lifecycle")
        .expect("Lifecycle should be registered as a type");
    assert_eq!(td.type_params, vec!["T".to_string()]);
}

/// Slice β positive gate: a use site `Lifecycle<File>` where `File: Resource`
/// satisfies the declared bound — must type-check without bound diagnostics.
#[test]
fn machine_generic_use_site_satisfies_bound() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { path: i64; }

impl Resource for File {
    fn close(self) {}
}

machine Lifecycle<T: Resource> {
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

fn main() {
    let f = File { path: 7 };
    let m = Active { handle: f };
}
";
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "use-site satisfying bound must type-check, got: {:?}",
        output.errors
    );
}

/// Slice β negative gate: a use site `Lifecycle<Plain>` where `Plain` does
/// not implement `Resource` must produce a `BoundsNotSatisfied` diagnostic
/// naming the trait and the type-param.
#[test]
fn machine_generic_use_site_violates_bound_errors() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

machine Lifecycle<T: Resource> {
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

fn main() {
    let p = Plain { x: 1 };
    let m = Active { handle: p };
}
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "use-site violating bound must produce exactly one BoundsNotSatisfied (dedup gate); got: {:?}",
        output.errors
    );
    let msg = &bound_errors[0].message;
    assert!(
        msg.contains("Plain") && msg.contains("Resource"),
        "diagnostic must name the offending type and the unmet trait, got: {msg}"
    );
}

/// Slice β negative gate: an unknown trait name in a machine generic bound
/// must produce an `UndefinedType` diagnostic at the machine declaration
/// site rather than silently passing through.
#[test]
fn machine_generic_unknown_trait_in_bound_errors() {
    let source = r"
machine Lifecycle<T: NonExistentTrait> {
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
    let output = typecheck_isolated(source);
    let unknown_trait_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| {
            e.kind == TypeErrorKind::UndefinedType
                && e.message.contains("NonExistentTrait")
                && e.message.contains("unknown trait")
        })
        .collect();
    assert!(
        !unknown_trait_errors.is_empty(),
        "unknown trait in machine generic bound must produce UndefinedType, got: {:?}",
        output.errors
    );
}

// ── State entry/exit lifecycle block checks ──────────────────────────────────

/// A machine with well-typed entry/exit blocks must type-check without errors.
#[test]
fn machine_state_entry_exit_well_typed_no_errors() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed {
                entry { let _x: i64 = 1; }
                exit  { let _y: i64 = 2; }
            }
            state Open;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "machine with well-typed entry/exit blocks must type-check without errors, got: {:?}",
        output.errors
    );
}

/// A type error inside a state entry block must be reported.
#[test]
fn machine_state_entry_type_error_reported() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed {
                entry {
                    // assigning a bool to an i64 — must be a type error
                    let _x: i64 = true;
                }
            }
            state Open;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "type error inside state entry block must produce a diagnostic"
    );
}

/// A type error inside a state exit block must be reported.
#[test]
fn machine_state_exit_type_error_reported() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Open {
                exit {
                    // assigning a bool to an i64 — must be a type error
                    let _x: i64 = true;
                }
            }
            state Closed;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "type error inside state exit block must produce a diagnostic"
    );
}

/// Referencing `event` inside a state entry block is a name-resolution error.
/// `event` is only in scope inside transition bodies — never in lifecycle hooks.
#[test]
fn machine_state_entry_event_binding_not_in_scope() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed {
                entry {
                    // `event` is a transition-scope binding; must be undefined here
                    let _e = event;
                }
            }
            state Open;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedVariable && e.message.contains("event")),
        "referencing `event` inside a state entry block must be UndefinedVariable, got: {:?}",
        output.errors
    );
}

/// Same rejection: `event` inside a state exit block.
#[test]
fn machine_state_exit_event_binding_not_in_scope() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Open {
                exit {
                    // `event` is a transition-scope binding; must be undefined here
                    let _e = event;
                }
            }
            state Closed;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedVariable && e.message.contains("event")),
        "referencing `event` inside a state exit block must be UndefinedVariable, got: {:?}",
        output.errors
    );
}

/// `state` IS in scope inside entry/exit — reading it must not produce an error.
#[test]
fn machine_state_entry_state_binding_in_scope() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed {
                entry {
                    // `state` is the machine value; discarding it must be fine
                    let _s = state;
                }
            }
            state Open;


            on Push: Closed => Open;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "`state` must be in scope inside an entry block, got: {:?}",
        output.errors
    );
}

/// Payload state fields resolve in an entry block.
/// `state.seq` must type-check to `i64` when the state is `Established { seq: i64 }`.
#[test]
fn machine_state_entry_payload_field_resolves() {
    let output = typecheck_isolated(
        r"
        machine TcpState {
            events {
                Connect;
                Disconnect;
            }

            state Closed;
            state Established {
                entry {
                    // `state.seq` must resolve - payload field on the current state
                    let _n: i64 = state.seq;
                }
                seq: i64;
            }


            on Connect:    Closed      => Established { seq: 0 }
            on Connect:    Established => Established { seq: state.seq }
            on Disconnect: Closed      => Closed;
            on Disconnect: Established => Closed;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "payload field `state.seq` must resolve inside a state entry block, got: {:?}",
        output.errors
    );
}

/// Payload state fields resolve in an exit block.
/// `state.seq` must type-check to `i64` when the state is `Established { seq: i64 }`.
#[test]
fn machine_state_exit_payload_field_resolves() {
    let output = typecheck_isolated(
        r"
        machine TcpState {
            events {
                Connect;
                Disconnect;
            }

            state Closed;
            state Established {
                exit {
                    // `state.seq` must resolve - payload field on the current state
                    let _n: i64 = state.seq;
                }
                seq: i64;
            }


            on Connect:    Closed      => Established { seq: 0 }
            on Connect:    Established => Established { seq: state.seq }
            on Disconnect: Closed      => Closed;
            on Disconnect: Established => Closed;
        }
        ",
    );
    assert!(
        output.errors.is_empty(),
        "payload field `state.seq` must resolve inside a state exit block, got: {:?}",
        output.errors
    );
}

/// A field that does not exist on the payload state must be an error in entry.
#[test]
fn machine_state_entry_nonexistent_payload_field_errors() {
    let output = typecheck_isolated(
        r"
        machine TcpState {
            events {
                Connect;
                Disconnect;
            }

            state Closed;
            state Established {
                entry {
                    let _n: i64 = state.no_such_field;
                }
                seq: i64;
            }


            on Connect:    Closed      => Established { seq: 0 }
            on Connect:    Established => Established { seq: state.seq }
            on Disconnect: Closed      => Closed;
            on Disconnect: Established => Closed;
        }
        ",
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::UndefinedField),
        "accessing a nonexistent payload field in entry must be UndefinedField, got: {:?}",
        output.errors
    );
}

/// Transition guard expressions must still be type-checked (regression guard).
/// A guard with a type error must be reported.
#[test]
fn machine_transition_guard_type_error_reported() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed;
            state Open;


            // guard expects bool, but 42 is i64 — type error
            on Push: Closed => Open when 42;
            on Push: Open   => Open;
            on Pull: Open   => Closed;
            on Pull: Closed => Closed;
        }
        ",
    );
    assert!(
        !output.errors.is_empty(),
        "type error in transition guard must produce a diagnostic"
    );
}

/// A machine that has both entry/exit blocks and exhaustiveness gaps must
/// produce both kinds of diagnostics.
#[test]
fn machine_entry_exit_errors_and_exhaustiveness_both_reported() {
    let output = typecheck_isolated(
        r"
        machine Door {
            events {
                Push;
                Pull;
            }

            state Closed {
                entry { let _x: i64 = true; }   // type error
            }
            state Open;


            on Push: Closed => Open;
            // Missing: Open -> Push and both Pull transitions
        }
        ",
    );
    assert!(
        output.errors.iter().any(|e| matches!(
            e.kind,
            TypeErrorKind::Mismatch {
                ref expected,
                ref actual
            } if expected == "i64" && actual == "bool"
        )),
        "entry-block lifecycle type error must be reported alongside exhaustiveness, got: {:?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::MachineExhaustivenessError),
        "exhaustiveness gap must still be reported alongside entry-block error, got: {:?}",
        output.errors
    );
}

// ── where-clause bound acceptance ────────────────────────────────────

/// `machine M<T> where T: Resource` — checker accepts the where-clause
/// form, populates the `machine_type_param_bounds` side table from it,
/// and downstream bound-enforcement at struct-init treats the where
/// bound identically to an inline `<T: Resource>`.
#[test]
fn machine_where_clause_bound_resolves_and_enforces() {
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

fn main() {
    let f = File { path: 7 };
    let m = Active { handle: f };
}
";
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "where-clause bound satisfied at use site must type-check, got: {:?}",
        output.errors
    );
}

/// `machine M<T> where T: Resource` with use site supplying a type
/// that does NOT implement `Resource` must produce `BoundsNotSatisfied`.
/// Confirms the where-clause form reaches the same enforcement path
/// as inline bounds.
#[test]
fn machine_where_clause_bound_violation_errors() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn main() {
    let p = Plain { x: 1 };
    let m = Active { handle: p };
}
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "where-clause bound violated at use site must produce exactly one BoundsNotSatisfied (dedup gate); got: {:?}",
        output.errors
    );
}

/// `machine M<T> where Foo: Trait` where `Foo` is not declared in the
/// machine's type-param list is a closed user error — fail with
/// `UndefinedType` at the where-clause site rather than silently
/// ignoring the predicate.
#[test]
fn machine_where_clause_undeclared_param_errors() {
    let source = r"
trait Resource {
    fn close(self);
}

machine Bogus<T> where U: Resource {
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
    let output = typecheck_isolated(source);
    let undef: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::UndefinedType)
        .collect();
    assert!(
        undef.iter().any(|e| e.message.contains('U')
            && e.message.contains("not a declared type parameter")),
        "where-clause on undeclared param must fail closed with descriptive UndefinedType, got: {:?}",
        output.errors
    );
}

/// `machine M<T: Resource> where T: Resource` — duplicate inline + where
/// bound on the same trait collapses to a single bound list. No
/// diagnostic emitted; the side table has exactly one entry for
/// `Resource`.
#[test]
fn machine_duplicate_inline_and_where_bound_dedups() {
    let source = r"
trait Resource {
    fn close(self);
}

type File { path: i64; }

impl Resource for File {
    fn close(self) {}
}

machine Twin<T: Resource> where T: Resource {
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

fn main() {
    let f = File { path: 7 };
    let m = Active { handle: f };
}
";
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "dedup'd inline+where bound must type-check, got: {:?}",
        output.errors
    );
    // Internal bound-table dedup is not directly observable at the
    // checker layer (the table is a private side structure). What is
    // observable, and what this test pins, is the surface contract:
    // the program type-checks and bound enforcement at the use site
    // recognises the satisfied bound exactly once — no spurious
    // double diagnostic from the duplicated inline + where predicate.
}

// ── machine-instantiation bound enforcement: type-annotation path ────
//
// Annotations like `var x: Holder<Plain>` resolve through
// `resolve_type_expr`. The canonical helper fires at this seam so
// the annotation site itself rejects unsatisfied bounds — bound
// enforcement is no longer deferred until brace-init at the value
// position.

/// `var x: Holder<File>` with `File: Resource` accepts cleanly.
#[test]
fn machine_type_annotation_bound_satisfied_typechecks() {
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

fn use_holder(h: Holder<File>) -> Holder<File> {
    h
}
";
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "machine annotation `Holder<File>` with File: Resource must type-check, got: {:?}",
        output.errors
    );
}

/// `var x: Holder<Plain>` with `Plain` not implementing `Resource`
/// emits `BoundsNotSatisfied` at the annotation site even when no
/// brace-init occurs in the same function — the canonical helper
/// fires at type-annotation resolution.
#[test]
fn machine_type_annotation_bound_violation_errors_at_annotation_site() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn use_holder(h: Holder<Plain>) {
}
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    // Exact-count assertion: one annotation site (`h: Holder<Plain>`)
    // → exactly one BoundsNotSatisfied diagnostic. Asserting a
    // precise count rather than "at least one" demonstrates the
    // helper dedup gate is on (no duplicate emission from
    // signature-registration + body-resolution sweeps over the same
    // span).
    assert_eq!(
        bound_errors.len(),
        1,
        "annotation `Holder<Plain>` must emit exactly 1 BoundsNotSatisfied at the parameter annotation site, got: {:?}",
        output.errors
    );
}

/// Annotation in `let` binding form (`let h: Holder<Plain> = …`) also
/// routes through `resolve_type_expr` — the bound violation surfaces
/// regardless of whether the annotation occupies a parameter or local
/// binding slot.
#[test]
fn machine_let_annotation_bound_violation_errors() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn main() {
    let p = Plain { x: 1 };
    let h: Holder<Plain> = Active { handle: p };
}
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "let annotation `Holder<Plain>` must emit exactly 1 BoundsNotSatisfied at the binding site, got: {:?}",
        output.errors
    );
}

/// A non-machine generic name (e.g. a generic record type) routed
/// through the same `resolve_type_expr` annotation seam must NOT
/// emit a spurious `BoundsNotSatisfied` diagnostic — the canonical
/// helper's first action is to look up the name in the machine
/// bounds table, which returns `None` for non-machine carriers and
/// short-circuits cleanly.
#[test]
fn non_machine_annotation_does_not_trigger_machine_bound_check() {
    let source = r"
type Box<T> { value: T; }

fn use_box(b: Box<i64>) -> Box<i64> {
    b
}
";
    let output = typecheck_isolated(source);
    assert!(
        output.errors.is_empty(),
        "non-machine generic annotation must not trigger machine bound enforcement, got: {:?}",
        output.errors
    );
}

// ── machine-instantiation bound enforcement: ctor coercion-arm path ──
//
// Constructor forms where the expected type pins the machine
// instantiation (`var m: Holder<Plain> = Holder::Active { … }`) route
// through the enum-variant coercion arm in `check_against`. The
// canonical helper fires alongside `record_concrete_record_init_type_args`
// at that site so the bound is enforced from the expected-type pin,
// not just from inferred-T at the synthesis path.

/// Negative: coercion-arm ctor with expected machine type whose
/// substitution violates the declared bound must emit
/// `BoundsNotSatisfied`. Pins Path-3 wiring.
#[test]
fn machine_ctor_coercion_arm_bound_violation_errors() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn build() -> Holder<Plain> {
    let p = Plain { x: 1 };
    Active { handle: p }
}
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "ctor coercion arm with expected `Holder<Plain>` must emit exactly one BoundsNotSatisfied (dedup gate); got: {:?}",
        output.errors
    );
}

// =====================================================================
// Nested-position bound enforcement (F2 fix).
//
// `resolve_type_expr` must enforce machine-trait bounds on every
// nested `Ty::Named` inside an annotation, not only the outermost
// type. The recursive walker visits Tuple, Array/Slice/Pointer/Task,
// Function/Closure params+ret+captures, TraitObject trait-args +
// assoc-bindings, and Named's own `args`. Dedup of identical
// `(machine_name, type_args)` pairs within a single annotation is
// covered by the multi-occurrence test below.
// =====================================================================

/// `Option<Holder<Plain>>` — generic-wrapper nesting. The
/// outermost `Ty::Named` is `Option`, which is not a machine, so
/// the original single-level check at `resolve_type_expr` missed
/// the inner `Holder<Plain>`. The recursive walker must descend
/// into `Option`'s type-arg and enforce on the inner
/// instantiation.
#[test]
fn machine_nested_in_generic_wrapper_enforces_bound() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn takes(opt: Option<Holder<Plain>>) -> i64 { 0 }
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "nested `Holder<Plain>` inside `Option<…>` must emit exactly 1 BoundsNotSatisfied, got: {:?}",
        output.errors
    );
}

/// `(Holder<Plain>, i64)` — tuple nesting. The outer resolved
/// type is a `Ty::Tuple`, which the original single-level check
/// never inspected. The walker must descend into each tuple
/// element.
#[test]
fn machine_nested_in_tuple_enforces_bound() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn takes(pair: (Holder<Plain>, i64)) -> i64 { 0 }
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "nested `Holder<Plain>` inside tuple must emit exactly 1 BoundsNotSatisfied, got: {:?}",
        output.errors
    );
}

/// `fn(Holder<Plain>) -> i64` — function-type nesting. The outer
/// resolved type is `Ty::Function`, whose params + ret were not
/// reachable from the original outer `Ty::Named` check.
#[test]
fn machine_nested_in_function_type_enforces_bound() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn takes_fn(f: fn(Holder<Plain>) -> i64) -> i64 { 0 }
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "nested `Holder<Plain>` inside `fn(…) -> …` must emit exactly 1 BoundsNotSatisfied, got: {:?}",
        output.errors
    );
}

/// Multi-resolution surface — the same `Holder<Plain>` appears
/// twice in a single tuple annotation. Both occurrences resolve
/// to identical `(machine_name="Holder", args=[Plain])` and would
/// otherwise emit two duplicate diagnostics keyed on the same
/// outer annotation span. The walker's per-annotation
/// `(name, args)` dedup must collapse them to exactly one
/// diagnostic.
#[test]
fn machine_multi_resolution_in_one_annotation_dedups() {
    let source = r"
trait Resource {
    fn close(self);
}

type Plain { x: i64; }

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

fn takes(pair: (Holder<Plain>, Holder<Plain>)) -> i64 { 0 }
";
    let output = typecheck_isolated(source);
    let bound_errors: Vec<_> = output
        .errors
        .iter()
        .filter(|e| e.kind == TypeErrorKind::BoundsNotSatisfied)
        .collect();
    assert_eq!(
        bound_errors.len(),
        1,
        "two occurrences of `Holder<Plain>` within one annotation must dedup to exactly 1 BoundsNotSatisfied, got: {:?}",
        output.errors
    );
}

// ── W3.039 Stage 2: const-generic machine registration ──────────────────

#[test]
fn machine_const_param_decl_passes_typecheck() {
    let src = r"machine FixedBuffer<const N: usize = 16> {
    events {
        Write;
        Drain;
    }

    state Empty;
    state Full;
    on Write: Empty => Full { Full }
    on Drain: Full => Empty { Empty }
    default { self }
}
";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let out = check_items(parsed.program.items);
    let errs: Vec<_> = out
        .errors
        .iter()
        .filter(|e| e.severity == hew_types::error::Severity::Error)
        .collect();
    assert!(
        errs.is_empty(),
        "expected no type errors on a const-param machine; got: {errs:?}"
    );
}

#[test]
fn machine_mixed_type_and_const_params_pass_typecheck() {
    let src = r"machine M<T, const N: usize> {
    events {
        Put { payload: T; }
        Take;
    }

    state Empty;
    state Full { val: T; }
    on Put: Empty => Full { Full { val: event.payload } }
    on Take: Full => Empty { Empty }
    default { self }
}
";
    let parsed = hew_parser::parse(src);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let out = check_items(parsed.program.items);
    let errs: Vec<_> = out
        .errors
        .iter()
        .filter(|e| e.severity == hew_types::error::Severity::Error)
        .collect();
    assert!(
        errs.is_empty(),
        "expected no type errors on a mixed-param machine; got: {errs:?}"
    );
}
