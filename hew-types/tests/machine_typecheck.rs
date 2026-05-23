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
        has_default: false,
        states,
        events,
        transitions,
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
        has_default: false,
        states: vec![unit_state("Idle"), unit_state("Active")],
        events: vec![unit_event("Start"), unit_event("Stop")],
        transitions: vec![
            transition("Start", "Idle", "Active"),
            transition("Stop", "Active", "Idle"),
            wildcard_transition("Start"),
            wildcard_transition("Stop"),
        ],
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
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
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
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
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
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
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
            state Closed;
            state Established { seq: i64; }
            event Connect;
            event Disconnect;
            on Connect: Closed -> Established { seq: 1 }
            on Connect: Established -> Established { seq: state.seq }
            on Disconnect: Closed -> Closed;
            on Disconnect: Established -> Closed;
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
            state Empty;
            state Loaded { value: T; }
            event Load { value: T; }
            event Reset;
            on Load: Empty -> Loaded { value: event.value }
            on Load: Loaded -> Loaded { value: event.value }
            on Reset: Empty -> Empty;
            on Reset: Loaded -> Empty;
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
            state Off;
            state On;
            event Toggle;
            on Toggle: Off -> On;
            on Toggle: On -> Off;
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
        has_default: false,
        states: vec![unit_state("Idle"), unit_state("Active")],
        events: vec![unit_event("Start"), unit_event("Stop")],
        transitions: vec![
            transition("Start", "Idle", "Active"),
            transition("Stop", "Active", "Idle"),
            wildcard_transition("Start"),
            wildcard_transition("Stop"),
        ],
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
    state Idle;
    state Active { handle: T; }

    event Start { handle: T; }
    event Stop;

    on Start: Idle -> Active { Active { handle: event.handle } }
    on Stop: Active -> Idle { Idle }
    on Start: _ -> _ { state }
    on Stop: _ -> _ { state }
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
