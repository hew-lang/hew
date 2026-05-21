//! MIR lowering for machine declarations.
//!
//! Every `HirItem::Machine` produces a synthesised `<Name>__step` MIR
//! function with the correct signature and a fail-closed single-block
//! body. The state×event dispatch tree, transition body lowering, and
//! `entry`/`exit`/`@reenter` semantics are grown into this seam once the
//! tagged-union value layout is decided downstream.
//!
//! These tests pin the shape of the substrate. If a later change replaces
//! the synthesised body with a real dispatch tree, the trap-only block
//! assertion below must be updated alongside the codegen-side layout
//! work that justifies the change.

use hew_hir::{
    HirExpr, HirExprKind, HirField, HirItem, HirLiteral, HirMachineDecl, HirMachineEvent,
    HirMachineState, HirMachineTransition, HirModule, IntentKind, ValueClass,
};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, Place, Terminator, TrapKind};
use hew_parser::ast::ResourceMarker;
use hew_types::ResolvedTy;
use std::collections::HashMap;

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        supervisor_child_slots: HashMap::default(),
    }
}

fn unit_literal_expr() -> HirExpr {
    HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    }
}

fn make_state(name: &str) -> HirMachineState {
    HirMachineState {
        name: name.to_string(),
        fields: Vec::<HirField>::new(),
        has_entry: false,
        has_exit: false,
        entry_writes: Vec::new(),
        exit_writes: Vec::new(),
        entry: None,
        exit: None,
        span: 0..0,
    }
}

fn make_event(name: &str) -> HirMachineEvent {
    HirMachineEvent {
        name: name.to_string(),
        fields: Vec::<HirField>::new(),
        span: 0..0,
    }
}

fn make_transition(event_name: &str, source: &str, target: &str) -> HirMachineTransition {
    HirMachineTransition {
        event_name: event_name.to_string(),
        source_state: source.to_string(),
        target_state: target.to_string(),
        has_guard: false,
        is_self_transition: source == target,
        reenter: false,
        body_writes: Vec::new(),
        body_emits: Vec::new(),
        body: unit_literal_expr(),
        span: 0..0,
    }
}

fn traffic_light_machine() -> HirMachineDecl {
    HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "TrafficLight".to_string(),
        type_params: Vec::new(),
        states: vec![make_state("Red"), make_state("Green")],
        events: vec![make_event("Tick")],
        transitions: vec![
            make_transition("Tick", "Red", "Green"),
            make_transition("Tick", "Green", "Red"),
        ],
        has_default: false,
        span: 0..0,
    }
}

fn generic_lifecycle_machine() -> HirMachineDecl {
    HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "Lifecycle".to_string(),
        type_params: vec!["T".to_string()],
        states: vec![make_state("Idle"), make_state("Running")],
        events: vec![make_event("Start")],
        transitions: vec![make_transition("Start", "Idle", "Running")],
        has_default: false,
        span: 0..0,
    }
}

#[test]
fn machine_decl_synthesises_step_function_symbol() {
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_machine(),
    )]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "machine synthesis should not produce diagnostics on a well-formed decl: {:?}",
        pipeline.diagnostics
    );

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("synthesised <Name>__step function must be present in raw_mir");
    assert_eq!(
        step_fn.name, "TrafficLight__step",
        "step function symbol uses double-underscore-step mangling"
    );
}

#[test]
fn synthesised_step_signature_is_self_event_returning_self() {
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_machine(),
    )]));

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("synthesised step function present");

    let self_ty = ResolvedTy::Named {
        name: "TrafficLight".to_string(),
        args: vec![],
    };
    let event_ty = ResolvedTy::Named {
        name: "TrafficLightEvent".to_string(),
        args: vec![],
    };

    assert_eq!(
        step_fn.params,
        vec![self_ty.clone(), event_ty],
        "step takes (self: <Name>, event: <Name>Event)"
    );
    assert_eq!(
        step_fn.return_ty, self_ty,
        "step returns the next machine value; the public m.step(ev) rewrite owns store-back"
    );
    assert_eq!(
        step_fn.call_conv,
        FunctionCallConv::Default,
        "step uses the default call conv; the public m.step(ev) rewrite is the call-site seam"
    );
}

#[test]
fn synthesised_step_body_contains_dispatch_tree_with_trap_fallthrough() {
    // Slice 4b: the step function dispatch tree contains one trap block as
    // the default fall-through (undeclared (state, event) pairs), and at
    // least one block per state-check. TrafficLight has 2 states + 1 event
    // + 2 transitions, producing entry + 2 state-checks + 2 state-bodies +
    // 2 arm-checks + 2 arm-bodies + trap = 10 blocks.
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_machine(),
    )]));

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("synthesised step function present");

    assert!(
        step_fn.blocks.len() > 1,
        "Slice 4b dispatch tree spans multiple blocks; got {}",
        step_fn.blocks.len()
    );
    assert!(
        step_fn.blocks.iter().any(|b| matches!(
            b.terminator,
            Terminator::Trap {
                kind: TrapKind::MachineDispatchUnreachable
            }
        )),
        "fail-closed: at least one block terminates in MachineDispatchUnreachable trap"
    );
    assert!(
        step_fn
            .blocks
            .iter()
            .any(|b| matches!(b.terminator, Terminator::Return)),
        "dispatch arms return the next-state value via Terminator::Return"
    );
}

#[test]
fn synthesised_step_locals_match_parameter_prologue_convention() {
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_machine(),
    )]));

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("synthesised step function present");

    // Parameter locals occupy the low indices in declaration order; codegen
    // emits one alloca per parameter and stores `get_nth_param(i)` into
    // `Place::Local(i)` before the first user instruction. Slice 4b adds
    // body-local slots for the state/event tag scratch and per-arm
    // comparison locals, so total locals exceed 2.
    assert!(
        step_fn.locals.len() >= 2,
        "at least two parameter slots are present"
    );
    assert_eq!(step_fn.locals[0], step_fn.params[0]);
    assert_eq!(step_fn.locals[1], step_fn.params[1]);
}

#[test]
fn generic_machine_preserves_type_params_in_synthesised_signature() {
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        generic_lifecycle_machine(),
    )]));

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Lifecycle__step")
        .expect("synthesised step function present for generic machine");

    // For generic machines, the self type carries each type parameter as a
    // free `ResolvedTy::Named` arg (matching the `hew-types` convention for
    // unbound type variables in registered type definitions). Monomorphisation
    // for generic machines arrives with the stdlib machine catalogue.
    let expected_self = ResolvedTy::Named {
        name: "Lifecycle".to_string(),
        args: vec![ResolvedTy::Named {
            name: "T".to_string(),
            args: vec![],
        }],
    };
    assert_eq!(step_fn.params[0], expected_self);
    assert_eq!(step_fn.return_ty, expected_self);

    // The event type is non-generic in v0.5 (the event companion enum does
    // not carry the machine's type params); registration in `hew-types` uses
    // an empty `type_params` vec for the companion.
    assert_eq!(
        step_fn.params[1],
        ResolvedTy::Named {
            name: "LifecycleEvent".to_string(),
            args: vec![],
        }
    );
}

#[test]
fn synthesised_step_function_emitted_once_per_machine_decl() {
    let pipeline = lower_hir_module(&empty_module(vec![
        HirItem::Machine(traffic_light_machine()),
        HirItem::Machine(generic_lifecycle_machine()),
    ]));

    let traffic_step_count = pipeline
        .raw_mir
        .iter()
        .filter(|f| f.name == "TrafficLight__step")
        .count();
    let lifecycle_step_count = pipeline
        .raw_mir
        .iter()
        .filter(|f| f.name == "Lifecycle__step")
        .count();

    assert_eq!(traffic_step_count, 1, "exactly one step fn per machine");
    assert_eq!(lifecycle_step_count, 1, "exactly one step fn per machine");
}

/// Slice 4a substrate test: verifies the step function has the correct
/// signature shape AND that `IrPipeline.machine_layouts` carries a layout
/// entry for the machine with the correct name and state-count-derived
/// `tag_width`. This test is the anchor for the three missing items from
/// Slice 4a (plan §3–5):
///
/// - `Place::MachineTag` / `Place::MachineVariant` are declared in the
///   model and the layout entry's `variants` naming confirms the plan
///   ordered them correctly. Slice 5 populates `field_tys`.
/// - `IrPipeline.machine_layouts` carries one entry per machine declaration.
///
/// The "switch shape" in the name refers to the step function's dispatch
/// shell — currently a single-block trap (the fail-closed seam). Slice 4b
/// replaces the trap with a real state×event switch; at that point this
/// test must be updated to assert the switch structure. For now, asserting
/// the signature + layout entry is the correct Slice 4a verification target.
#[test]
fn step_shell_signature_and_switch_shape() {
    // TrafficLight: 2 states (Red, Green), 1 event (Tick).
    // tag_width = max(1, ceil(log2(2))) = 1.
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_machine(),
    )]));

    assert!(
        pipeline.diagnostics.is_empty(),
        "no diagnostics on well-formed machine: {:?}",
        pipeline.diagnostics
    );

    // Step function signature: (self: TrafficLight, event: TrafficLightEvent) -> TrafficLight.
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "TrafficLight__step")
        .expect("step function present");

    let self_ty = ResolvedTy::Named {
        name: "TrafficLight".to_string(),
        args: vec![],
    };
    let event_ty = ResolvedTy::Named {
        name: "TrafficLightEvent".to_string(),
        args: vec![],
    };
    assert_eq!(
        step_fn.params,
        vec![self_ty.clone(), event_ty],
        "step signature: (self: Name, event: NameEvent)"
    );
    assert_eq!(
        step_fn.return_ty, self_ty,
        "step return type: Name (next-state value)"
    );
    assert_eq!(
        step_fn.call_conv,
        FunctionCallConv::Default,
        "step uses Default call conv"
    );

    // Slice 4b dispatch tree: multiple blocks; at least one trap (default
    // fall-through) and at least one Return (matched arm).
    assert!(
        step_fn.blocks.len() > 1,
        "Slice 4b dispatch tree has multiple blocks; got {}",
        step_fn.blocks.len()
    );
    assert!(
        step_fn.blocks.iter().any(|b| matches!(
            b.terminator,
            Terminator::Trap {
                kind: TrapKind::MachineDispatchUnreachable
            }
        )),
        "fail-closed default arm: undeclared (state, event) pairs trap"
    );

    // machine_layouts carries one entry for TrafficLight.
    assert_eq!(
        pipeline.machine_layouts.len(),
        1,
        "one MachineLayout per machine declaration"
    );
    let layout = &pipeline.machine_layouts[0];
    assert_eq!(
        layout.name, "TrafficLight",
        "layout name matches machine declaration"
    );
    // TrafficLight has 2 states: tag_width = max(1, ceil(log2(2))) = 1.
    assert_eq!(
        layout.tag_width, 1,
        "2-state machine needs 1-bit tag (2^1 = 2 encodings)"
    );
    // Slice 4a: variants are present (one per state) but field_tys are empty.
    // Slice 5 populates field_tys when it walks HirMachineState.fields.
    assert_eq!(layout.variants.len(), 2, "one variant per declared state");
    assert_eq!(layout.variants[0].name, "Red", "first state name preserved");
    assert_eq!(
        layout.variants[1].name, "Green",
        "second state name preserved"
    );
    assert!(
        layout.variants[0].field_tys.is_empty(),
        "Slice 4a: field_tys empty; Slice 5 populates from HirMachineState.fields"
    );
    assert!(
        layout.variants[1].field_tys.is_empty(),
        "Slice 4a: field_tys empty; Slice 5 populates from HirMachineState.fields"
    );
}

/// Slice 4c: verifies that `@resource`-bearing machine state payload fields
/// receive an `Instr::Drop` targeting `Place::MachineVariant` when a true
/// cross-state transition leaves the state. The drop must appear in the arm
/// body block BEFORE the transition body's value instructions.
///
/// Fixture machine `Conn`:
///   - State `Open { handle: ConnHandle }` where `ConnHandle` is `@resource`.
///   - State `Closed` (zero-field state; no drops expected).
///   - Event `Close` with transition `Open → Closed` (true transition; drop fires).
///   - Event `Ping` with self-transition `Open → Open`, no `@reenter`
///     (no-op self-transition; drop must NOT fire).
///
/// `type_classes` seeds `"ConnHandle" → (Resource, Some("close"))` so
/// `ValueClass::of_ty` returns `AffineResource` for `ConnHandle` fields.
///
/// Assert invariants:
/// 1. The `Close` arm emits exactly one `Instr::Drop` with
///    `Place::MachineVariant { variant_idx: 0, field_idx: 0, .. }` (Open is
///    state 0, `handle` is field 0).
/// 2. The drop appears BEFORE the body result instruction in the same block.
/// 3. The `Ping` arm (self-transition, no `@reenter`) emits NO
///    `Instr::Drop` targeting the Open variant's field.
/// 4. No diagnostics on the well-formed module.
#[test]
#[allow(
    clippy::too_many_lines,
    reason = "test function with multiple fixture builders and assertion groups"
)]
fn resource_field_transition_out_drops() {
    // Build a `ConnHandle` type reference.
    let conn_handle_ty = ResolvedTy::Named {
        name: "ConnHandle".to_string(),
        args: vec![],
    };

    // State 0: Open { handle: ConnHandle } — one @resource field.
    let open_state = HirMachineState {
        name: "Open".to_string(),
        fields: vec![HirField {
            name: "handle".to_string(),
            ty: conn_handle_ty.clone(),
            span: 0..0,
        }],
        has_entry: false,
        has_exit: false,
        entry_writes: vec![],
        exit_writes: vec![],
        entry: None,
        exit: None,
        span: 0..0,
    };

    // State 1: Closed — zero fields; no drops should ever emit for this state.
    let closed_state = HirMachineState {
        name: "Closed".to_string(),
        fields: vec![],
        has_entry: false,
        has_exit: false,
        entry_writes: vec![],
        exit_writes: vec![],
        entry: None,
        exit: None,
        span: 0..0,
    };

    // Unit-typed body for both transitions (no self-field writes needed).
    let unit_body = HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Literal(HirLiteral::Unit),
        span: 0..0,
    };

    // `Close`: true cross-state transition Open → Closed. Drop fires.
    let close_transition = HirMachineTransition {
        event_name: "Close".to_string(),
        source_state: "Open".to_string(),
        target_state: "Closed".to_string(),
        has_guard: false,
        is_self_transition: false,
        reenter: false,
        body_writes: vec![],
        body_emits: vec![],
        body: unit_body.clone(),
        span: 0..0,
    };

    // `Ping`: self-transition Open → Open, no @reenter. Drop must NOT fire.
    let ping_transition = HirMachineTransition {
        event_name: "Ping".to_string(),
        source_state: "Open".to_string(),
        target_state: "Open".to_string(),
        has_guard: false,
        is_self_transition: true,
        reenter: false,
        body_writes: vec![],
        body_emits: vec![],
        body: unit_body.clone(),
        span: 0..0,
    };

    let machine = HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "Conn".to_string(),
        type_params: vec![],
        states: vec![open_state, closed_state],
        events: vec![
            HirMachineEvent {
                name: "Close".to_string(),
                fields: vec![],
                span: 0..0,
            },
            HirMachineEvent {
                name: "Ping".to_string(),
                fields: vec![],
                span: 0..0,
            },
        ],
        transitions: vec![close_transition, ping_transition],
        has_default: false,
        span: 0..0,
    };

    // Seed the type_classes table: `ConnHandle` is @resource with close method
    // `"close"`. The drop_fn will be formatted as `"ConnHandle::close"`.
    let mut type_classes = HashMap::new();
    type_classes.insert(
        "ConnHandle".to_string(),
        (ResourceMarker::Resource, Some("close".to_string())),
    );

    let module = HirModule {
        items: vec![HirItem::Machine(machine)],
        type_classes,
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        supervisor_child_slots: HashMap::default(),
    };

    let pipeline = lower_hir_module(&module);

    assert!(
        pipeline.diagnostics.is_empty(),
        "well-formed @resource machine must produce no diagnostics: {:?}",
        pipeline.diagnostics
    );

    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Conn__step")
        .expect("Conn__step synthesised");

    // Collect all Drop instructions across all blocks, recording (block_id,
    // instr_position, place).
    let all_drops: Vec<(u32, usize, &Place)> = step_fn
        .blocks
        .iter()
        .flat_map(|b| {
            b.instructions
                .iter()
                .enumerate()
                .filter_map(move |(pos, instr)| {
                    if let Instr::Drop { place, .. } = instr {
                        Some((b.id, pos, place))
                    } else {
                        None
                    }
                })
        })
        .collect();

    // Assert 1: exactly one Drop targets the Open variant (variant_idx=0,
    // field_idx=0). This comes from the `Close` arm.
    let open_field_drops: Vec<_> = all_drops
        .iter()
        .filter(|(_, _, place)| {
            matches!(
                place,
                Place::MachineVariant {
                    variant_idx: 0,
                    field_idx: 0,
                    ..
                }
            )
        })
        .collect();

    assert_eq!(
        open_field_drops.len(),
        1,
        "exactly one Drop for Open.handle (variant_idx=0, field_idx=0); \
         found {}: {:#?}",
        open_field_drops.len(),
        all_drops
    );

    // Assert 2: the Drop appears before the body result Move in the same block.
    // The transition body for a unit literal emits a ConstI64(0) (unit
    // representation). The Drop must precede any ConstI64 in the arm block.
    let (drop_block_id, drop_pos, _) = open_field_drops[0];
    let drop_block = step_fn
        .blocks
        .iter()
        .find(|b| b.id == *drop_block_id)
        .expect("drop block present");

    // Find the first instruction that is NOT a tag/eq comparison setup
    // (ConstI64 for state/event tag comparisons happen in check blocks, not
    // arm body blocks). Any ConstI64 in the arm body block is the unit-literal
    // body value. The drop must appear before it.
    let first_body_instr_pos = drop_block
        .instructions
        .iter()
        .enumerate()
        .position(|(_, i)| matches!(i, Instr::ConstI64 { .. }));

    if let Some(body_pos) = first_body_instr_pos {
        assert!(
            *drop_pos < body_pos,
            "Drop (pos {drop_pos}) must precede body ConstI64 (pos {body_pos}) \
             in the same arm block — exit→drop→body ordering violated"
        );
    }

    // Assert 3: no Drop targets the Closed variant (variant_idx=1). It has no
    // fields, so no drops should ever appear for it.
    let closed_variant_drops: Vec<_> = all_drops
        .iter()
        .filter(|(_, _, place)| matches!(place, Place::MachineVariant { variant_idx: 1, .. }))
        .collect();

    assert!(
        closed_variant_drops.is_empty(),
        "Closed state has no @resource fields; no drops expected for variant_idx=1: \
         {closed_variant_drops:#?}"
    );

    // Assert 4: the `Ping` arm (self-transition, no @reenter) emits no Drop.
    // Since the Ping and Close arms are in the same state body (Open, state_idx=0),
    // and we've already verified only one Drop for Open.handle exists total,
    // the single Drop must be from Close and not from Ping.
    // Additionally verify the drop_fn is correctly derived.
    let (_, confirmed_drop_pos, _) = open_field_drops[0];
    let drop_instr = &drop_block.instructions[*confirmed_drop_pos];
    assert!(
        matches!(
            drop_instr,
            Instr::Drop {
                drop_fn: Some(name),
                ..
            } if name == "ConnHandle::close"
        ),
        "Drop must carry drop_fn = Some(\"ConnHandle::close\"); got: {drop_instr:?}"
    );
}
