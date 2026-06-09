//! MIR lowering for machine declarations.
//!
//! Every `HirItem::Machine` produces a synthesised `<Name>__step` MIR
//! function with the correct signature and a fail-closed state×event
//! dispatch shell. Transition bodies and lifecycle hooks lower into matched
//! arms; transition-out drops are emitted for tag-dominated resource payloads.

use hew_hir::node::HirRecordDecl;
use hew_hir::ResourceMarker as HirResourceMarker;
use hew_hir::{
    BindingId, HirBlock, HirExpr, HirExprKind, HirField, HirItem, HirMachineDecl, HirMachineEvent,
    HirMachineState, HirMachineTransition, HirModule, HirStmt, HirStmtKind, IntentKind,
    ResolvedRef, ValueClass,
};
use hew_mir::{lower_hir_module, FunctionCallConv, Instr, Place, Terminator, TrapKind};
use hew_types::ResolvedTy;
use std::collections::{BTreeSet, HashMap};

const MACHINE_SELF_BINDING: BindingId = BindingId(u32::MAX);

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        regex_literals: vec![],
    }
}

fn machine_ctor_expr(machine_name: &str, state_idx: usize) -> HirExpr {
    HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: ResolvedTy::Named {
            name: machine_name.to_string(),
            args: vec![],
            builtin: None,
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::MachineVariantCtor {
            machine_name: machine_name.to_string(),
            state_idx,
            payload: None,
        },
        span: 0..0,
    }
}

fn machine_self_expr(machine_name: &str) -> HirExpr {
    HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: ResolvedTy::Named {
            name: machine_name.to_string(),
            args: vec![],
            builtin: None,
        },
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::BindingRef {
            name: "self".to_string(),
            resolved: ResolvedRef::Binding(MACHINE_SELF_BINDING),
        },
        span: 0..0,
    }
}

fn machine_emit_expr(event_idx: usize) -> HirExpr {
    HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: ResolvedTy::Unit,
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::MachineEmit {
            event_idx,
            fields: Vec::new(),
        },
        span: 0..0,
    }
}

fn machine_emit_stmt(event_idx: usize) -> HirStmt {
    HirStmt {
        node: hew_hir::HirNodeId(0),
        kind: HirStmtKind::Expr(machine_emit_expr(event_idx)),
        span: 0..0,
    }
}

fn hir_block(statements: Vec<HirStmt>, tail: Option<HirExpr>, ty: ResolvedTy) -> HirBlock {
    HirBlock {
        node: hew_hir::HirNodeId(0),
        scope: hew_hir::ScopeId(0),
        statements,
        tail: tail.map(Box::new),
        ty,
        span: 0..0,
    }
}

fn transition_body(machine_name: &str, target_idx: usize, emits: &[usize]) -> HirExpr {
    let machine_ty = ResolvedTy::Named {
        name: machine_name.to_string(),
        args: vec![],
        builtin: None,
    };
    HirExpr {
        node: hew_hir::HirNodeId(0),
        site: hew_hir::SiteId(0),
        ty: machine_ty.clone(),
        value_class: ValueClass::BitCopy,
        intent: IntentKind::Read,
        kind: HirExprKind::Block(hir_block(
            emits.iter().copied().map(machine_emit_stmt).collect(),
            Some(machine_ctor_expr(machine_name, target_idx)),
            machine_ty,
        )),
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

fn make_state_with_fields(name: &str, fields: Vec<HirField>) -> HirMachineState {
    HirMachineState {
        name: name.to_string(),
        fields,
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

fn named_ty(name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: name.to_string(),
        args: vec![],
        builtin: None,
    }
}

fn make_transition(
    machine_name: &str,
    event_name: &str,
    source: &str,
    target: &str,
    target_idx: usize,
) -> HirMachineTransition {
    HirMachineTransition {
        event_name: event_name.to_string(),
        source_state: source.to_string(),
        target_state: target.to_string(),
        guard: None,
        is_self_transition: source == target,
        reenter: false,
        body_writes: Vec::new(),
        body_emits: Vec::new(),
        body: machine_ctor_expr(machine_name, target_idx),
        span: 0..0,
    }
}

fn traffic_light_machine() -> HirMachineDecl {
    HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "TrafficLight".to_string(),
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        states: vec![make_state("Red"), make_state("Green")],
        events: vec![make_event("Tick")],
        transitions: vec![
            make_transition("TrafficLight", "Tick", "Red", "Green", 1),
            make_transition("TrafficLight", "Tick", "Green", "Red", 0),
        ],
        has_default: false,
        span: 0..0,
    }
}

fn traffic_light_with_wildcard_machine() -> HirMachineDecl {
    HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "TrafficLight".to_string(),
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        states: vec![make_state("Red"), make_state("Green")],
        events: vec![make_event("Tick"), make_event("Timeout")],
        transitions: vec![
            make_transition("TrafficLight", "Tick", "Red", "Green", 1),
            make_transition("TrafficLight", "Timeout", "_", "Red", 0),
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
        type_param_bounds: Vec::new(),
        states: vec![make_state("Idle"), make_state("Running")],
        events: vec![make_event("Start")],
        transitions: vec![make_transition("Lifecycle", "Start", "Idle", "Running", 1)],
        has_default: false,
        span: 0..0,
    }
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single snapshot-style test covers real, self, and @reenter hook ordering"
)]
fn transition_bodies_entry_exit_reenter() {
    const EXIT_IDLE: usize = 3;
    const BODY_REAL: usize = 4;
    const ENTRY_ACTIVE: usize = 5;
    const BODY_SELF: usize = 6;
    const EXIT_ACTIVE: usize = 7;
    const BODY_REENTER: usize = 8;

    let machine_ty = ResolvedTy::Named {
        name: "Lifecycle".to_string(),
        args: vec![],
        builtin: None,
    };
    let idle = HirMachineState {
        name: "Idle".to_string(),
        fields: Vec::new(),
        has_entry: false,
        has_exit: true,
        entry_writes: Vec::new(),
        exit_writes: Vec::new(),
        entry: None,
        exit: Some(hir_block(
            vec![machine_emit_stmt(EXIT_IDLE)],
            None,
            ResolvedTy::Unit,
        )),
        span: 0..0,
    };
    let active = HirMachineState {
        name: "Active".to_string(),
        fields: Vec::new(),
        has_entry: true,
        has_exit: true,
        entry_writes: Vec::new(),
        exit_writes: Vec::new(),
        entry: Some(hir_block(
            vec![machine_emit_stmt(ENTRY_ACTIVE)],
            None,
            ResolvedTy::Unit,
        )),
        exit: Some(hir_block(
            vec![machine_emit_stmt(EXIT_ACTIVE)],
            None,
            ResolvedTy::Unit,
        )),
        span: 0..0,
    };
    let mut real = make_transition("Lifecycle", "Start", "Idle", "Active", 1);
    real.body = transition_body("Lifecycle", 1, &[BODY_REAL]);
    real.body_emits = vec!["BodyReal".to_string()];

    let mut self_no_reenter = make_transition("Lifecycle", "Stay", "Active", "Active", 1);
    self_no_reenter.body = transition_body("Lifecycle", 1, &[BODY_SELF]);
    self_no_reenter.body_emits = vec!["BodySelf".to_string()];

    let mut self_reenter = make_transition("Lifecycle", "Pulse", "Active", "Active", 1);
    self_reenter.reenter = true;
    self_reenter.body = transition_body("Lifecycle", 1, &[BODY_REENTER]);
    self_reenter.body_emits = vec!["BodyReenter".to_string()];

    let machine = HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "Lifecycle".to_string(),
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        states: vec![idle, active],
        events: vec![
            make_event("Start"),
            make_event("Stay"),
            make_event("Pulse"),
            make_event("ExitIdle"),
            make_event("BodyReal"),
            make_event("EntryActive"),
            make_event("BodySelf"),
            make_event("ExitActive"),
            make_event("BodyReenter"),
        ],
        transitions: vec![real, self_no_reenter, self_reenter],
        has_default: false,
        span: 0..0,
    };

    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(machine)]));
    assert!(
        pipeline.diagnostics.is_empty(),
        "machine body lowering should not produce diagnostics: {:?}",
        pipeline.diagnostics
    );
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "Lifecycle__step")
        .expect("Lifecycle__step synthesised");
    assert_eq!(step_fn.return_ty, machine_ty);

    let block_emit_indices = |needle| {
        step_fn
            .blocks
            .iter()
            .find_map(|block| {
                let emits = block
                    .instructions
                    .iter()
                    .filter_map(|instr| {
                        if let Instr::MachineEmitPlaceholder { event_idx, .. } = instr {
                            Some(*event_idx)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();
                emits.contains(&needle).then_some(emits)
            })
            .unwrap_or_else(|| panic!("return block containing emit event_idx={needle}"))
    };
    let block_tag_write_index = |needle| {
        let block = step_fn
            .blocks
            .iter()
            .find(|block| {
                block.instructions.iter().any(|instr| {
                    matches!(
                        instr,
                        Instr::MachineEmitPlaceholder {
                            event_idx,
                            ..
                        } if *event_idx == needle
                    )
                })
            })
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle}"));
        block
            .instructions
            .iter()
            .position(|instr| {
                matches!(
                    instr,
                    Instr::Move {
                        dest: Place::MachineTag(_),
                        ..
                    }
                )
            })
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle} writes target tag"))
    };
    let block_emit_instr_index = |needle| {
        let block = step_fn
            .blocks
            .iter()
            .find(|block| {
                block.instructions.iter().any(|instr| {
                    matches!(
                        instr,
                        Instr::MachineEmitPlaceholder {
                            event_idx,
                            ..
                        } if *event_idx == needle
                    )
                })
            })
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle}"));
        block
            .instructions
            .iter()
            .position(|instr| {
                matches!(
                    instr,
                    Instr::MachineEmitPlaceholder {
                        event_idx,
                        ..
                    } if *event_idx == needle
                )
            })
            .unwrap_or_else(|| panic!("emit event_idx={needle} present"))
    };

    let real_emits = block_emit_indices(BODY_REAL);
    assert_eq!(
        real_emits,
        vec![EXIT_IDLE, BODY_REAL, ENTRY_ACTIVE],
        "real transition fires source exit, then body, then target entry"
    );
    assert!(
        block_tag_write_index(BODY_REAL) > block_emit_instr_index(BODY_REAL),
        "real transition constructs target after body side effects"
    );
    assert!(
        block_tag_write_index(BODY_REAL) < block_emit_instr_index(ENTRY_ACTIVE),
        "real transition invokes entry after target construction"
    );

    let self_emits = block_emit_indices(BODY_SELF);
    assert_eq!(
        self_emits,
        vec![BODY_SELF],
        "self-transition without @reenter runs only the body"
    );

    let reenter_emits = block_emit_indices(BODY_REENTER);
    assert_eq!(
        reenter_emits,
        vec![EXIT_ACTIVE, BODY_REENTER, ENTRY_ACTIVE],
        "@reenter self-transition fires exit, then body, then entry"
    );
    assert!(
        block_tag_write_index(BODY_REENTER) > block_emit_instr_index(BODY_REENTER),
        "@reenter transition constructs target after body side effects"
    );
    assert!(
        block_tag_write_index(BODY_REENTER) < block_emit_instr_index(ENTRY_ACTIVE),
        "@reenter transition invokes entry after target construction"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "matrix test covers resource transition-out, self persistence, reenter, and non-resource states"
)]
fn resource_field_transition_out_drops() {
    const BODY_TO_IDLE: usize = 4;
    const BODY_REENTER: usize = 5;
    const BODY_FINISH: usize = 6;

    let handle_ty = named_ty("FileHandle");
    let holding = make_state_with_fields(
        "Holding",
        vec![HirField {
            name: "handle".to_string(),
            ty: handle_ty.clone(),
            span: 0..0,
        }],
    );
    let idle = make_state("Idle");
    let done = make_state("Done");

    let mut to_idle = make_transition("ResourceMachine", "ToIdle", "Holding", "Idle", 1);
    to_idle.body = transition_body("ResourceMachine", 1, &[BODY_TO_IDLE]);

    let mut stay = make_transition("ResourceMachine", "Stay", "Holding", "Holding", 0);
    stay.body = machine_self_expr("ResourceMachine");

    let mut reenter = make_transition("ResourceMachine", "Reenter", "Holding", "Holding", 0);
    reenter.reenter = true;
    reenter.body = transition_body("ResourceMachine", 0, &[BODY_REENTER]);

    let mut finish = make_transition("ResourceMachine", "Finish", "Idle", "Done", 2);
    finish.body = transition_body("ResourceMachine", 2, &[BODY_FINISH]);

    let machine = HirMachineDecl {
        id: hew_hir::ItemId(0),
        node: hew_hir::HirNodeId(0),
        name: "ResourceMachine".to_string(),
        type_params: Vec::new(),
        type_param_bounds: Vec::new(),
        states: vec![holding.clone(), idle, done],
        events: vec![
            make_event("ToIdle"),
            make_event("Stay"),
            make_event("Reenter"),
            make_event("Finish"),
            make_event("BodyToIdle"),
            make_event("BodyReenter"),
            make_event("BodyFinish"),
        ],
        transitions: vec![to_idle, stay, reenter, finish],
        has_default: false,
        span: 0..0,
    };

    let file_handle_record = HirRecordDecl {
        id: hew_hir::ItemId(1),
        node: hew_hir::HirNodeId(1),
        name: "FileHandle".to_string(),
        type_params: Vec::new(),
        fields: vec![HirField {
            name: "raw".to_string(),
            ty: ResolvedTy::I64,
            span: 0..0,
        }],
        span: 0..0,
    };
    let mut module = empty_module(vec![
        HirItem::Record(file_handle_record),
        HirItem::Machine(machine.clone()),
    ]);
    module.type_classes.insert(
        "FileHandle".to_string(),
        (HirResourceMarker::Resource, Some("close".to_string())),
    );

    let pipeline = lower_hir_module(&module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "machine resource transition lowering should not produce diagnostics: {:?}",
        pipeline.diagnostics
    );
    let step_fn = pipeline
        .raw_mir
        .iter()
        .find(|f| f.name == "ResourceMachine__step")
        .expect("ResourceMachine__step synthesised");

    let block_containing_emit = |needle| {
        step_fn
            .blocks
            .iter()
            .find(|block| {
                block.instructions.iter().any(|instr| {
                    matches!(
                        instr,
                        Instr::MachineEmitPlaceholder {
                            event_idx,
                            ..
                        } if *event_idx == needle
                    )
                })
            })
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle}"))
    };
    let drops_in_block = |needle| -> BTreeSet<String> {
        block_containing_emit(needle)
            .instructions
            .iter()
            .filter_map(|instr| match instr {
                Instr::Drop { place, .. } => Some(format!("{place:?}")),
                _ => None,
            })
            .collect()
    };
    let drop_summaries = |needle| -> Vec<String> {
        block_containing_emit(needle)
            .instructions
            .iter()
            .filter_map(|instr| match instr {
                Instr::Drop { place, drop_fn, .. } => Some(format!(
                    "{place:?} via {}",
                    drop_fn.as_deref().unwrap_or("<none>")
                )),
                _ => None,
            })
            .collect()
    };
    let expected_source_resource_places = |source_variant_idx: usize| -> BTreeSet<String> {
        let self_local = 0;
        machine.states[source_variant_idx]
            .fields
            .iter()
            .enumerate()
            .filter_map(|(field_idx, field)| {
                (ValueClass::of_ty(&field.ty, &module.type_classes) == ValueClass::AffineResource)
                    .then_some(format!(
                        "{:?}",
                        Place::MachineVariant {
                            local: self_local,
                            variant_idx: u32::try_from(source_variant_idx)
                                .expect("variant index fits u32"),
                            field_idx: u32::try_from(field_idx).expect("field index fits u32"),
                        }
                    ))
            })
            .collect()
    };
    let first_drop_index = |needle| {
        block_containing_emit(needle)
            .instructions
            .iter()
            .position(|instr| matches!(instr, Instr::Drop { .. }))
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle} has a Drop"))
    };
    let first_tag_write_index = |needle| {
        block_containing_emit(needle)
            .instructions
            .iter()
            .position(|instr| {
                matches!(
                    instr,
                    Instr::Move {
                        dest: Place::MachineTag(_),
                        ..
                    }
                )
            })
            .unwrap_or_else(|| panic!("block containing emit event_idx={needle} writes target tag"))
    };

    let holding_resource_places = expected_source_resource_places(0);
    assert_eq!(
        drops_in_block(BODY_TO_IDLE),
        holding_resource_places,
        "lattice-correct: dropped places equal source variant @resource fields"
    );
    assert!(
        first_drop_index(BODY_TO_IDLE) < first_tag_write_index(BODY_TO_IDLE),
        "transition-out drop precedes target tag write"
    );
    assert_eq!(
        drops_in_block(BODY_REENTER),
        expected_source_resource_places(0),
        "@reenter self-transition drops source variant resources"
    );
    assert!(
        first_drop_index(BODY_REENTER) < first_tag_write_index(BODY_REENTER),
        "@reenter drop precedes re-construction tag write"
    );
    assert!(
        drops_in_block(BODY_FINISH).is_empty(),
        "transition between non-resource states emits no drops"
    );

    let self_persist_blocks: Vec<_> = step_fn
        .blocks
        .iter()
        .filter(|block| {
            matches!(block.terminator, Terminator::Return)
                && block.instructions.iter().any(|instr| {
                    matches!(
                        instr,
                        Instr::Move {
                            dest: Place::ReturnSlot,
                            src: Place::Local(0),
                        }
                    )
                })
        })
        .collect();
    assert_eq!(
        self_persist_blocks.len(),
        1,
        "self-transition default {{ self }} arm is present exactly once"
    );
    assert!(
        self_persist_blocks[0]
            .instructions
            .iter()
            .all(|instr| !matches!(instr, Instr::Drop { .. })),
        "self-transition without @reenter emits zero drops"
    );

    let snapshot = vec![
        format!("Holding->Idle drops: {:?}", drop_summaries(BODY_TO_IDLE)),
        format!(
            "Holding->Idle dropped-set: {:?}",
            drops_in_block(BODY_TO_IDLE)
        ),
        format!("Holding @resource-set: {holding_resource_places:?}"),
        format!(
            "Holding->Idle order: drop@{} < tag@{}",
            first_drop_index(BODY_TO_IDLE),
            first_tag_write_index(BODY_TO_IDLE)
        ),
        "Holding->Holding default { self } drops: []".to_string(),
        format!(
            "Holding->Holding @reenter drops: {:?}",
            drop_summaries(BODY_REENTER)
        ),
        format!(
            "Holding->Holding @reenter order: drop@{} < tag@{}",
            first_drop_index(BODY_REENTER),
            first_tag_write_index(BODY_REENTER)
        ),
        format!("Idle->Done drops: {:?}", drop_summaries(BODY_FINISH)),
    ];
    assert_eq!(
        snapshot,
        vec![
            "Holding->Idle drops: [\"MachineVariant { local: 0, variant_idx: 0, field_idx: 0 } via FileHandle::close\"]",
            "Holding->Idle dropped-set: {\"MachineVariant { local: 0, variant_idx: 0, field_idx: 0 }\"}",
            "Holding @resource-set: {\"MachineVariant { local: 0, variant_idx: 0, field_idx: 0 }\"}",
            "Holding->Idle order: drop@0 < tag@3",
            "Holding->Holding default { self } drops: []",
            "Holding->Holding @reenter drops: [\"MachineVariant { local: 0, variant_idx: 0, field_idx: 0 } via FileHandle::close\"]",
            "Holding->Holding @reenter order: drop@0 < tag@3",
            "Idle->Done drops: []",
        ],
        "machine transition-out resource-drop snapshot"
    );
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
        builtin: None,
    };
    let event_ty = ResolvedTy::Named {
        name: "TrafficLightEvent".to_string(),
        args: vec![],
        builtin: None,
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
    // Slice 4a: the step function dispatch shell contains one trap block as
    // the default fall-through (undeclared (state, event) pairs) and return
    // blocks for matched transition placeholders.
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
        "Slice 4a dispatch shell spans multiple blocks; got {}",
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
        "dispatch arms return target-state placeholders via Terminator::Return"
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
    // `Place::Local(i)` before the first user instruction.
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

    // The executable generic-machine substrate currently defaults machine
    // type parameters to the concrete fixture instantiation used by the
    // stdlib catalogue path.
    let expected_self = ResolvedTy::Named {
        name: "Lifecycle".to_string(),
        args: vec![ResolvedTy::I64],
        builtin: None,
    };
    assert_eq!(step_fn.params[0], expected_self);
    assert_eq!(step_fn.return_ty, expected_self);

    // The companion event enum mirrors the machine type arguments.
    assert_eq!(
        step_fn.params[1],
        ResolvedTy::Named {
            name: "LifecycleEvent".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: None,
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

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "single substrate test asserts signature, dispatch ordering, and layout together"
)]
fn step_shell_signature_and_switch_shape() {
    // TrafficLight: 2 states (Red, Green), 2 events (Tick, Timeout). Red has
    // a specific Tick arm followed by a wildcard Timeout arm; the test asserts
    // that specific arms remain higher priority than wildcard/default arms.
    // tag_width = max(1, ceil(log2(2))) = 1.
    let pipeline = lower_hir_module(&empty_module(vec![HirItem::Machine(
        traffic_light_with_wildcard_machine(),
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
        builtin: None,
    };
    let event_ty = ResolvedTy::Named {
        name: "TrafficLightEvent".to_string(),
        args: vec![],
        builtin: None,
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

    // Slice 4a dispatch shell: multiple blocks; at least one trap (default
    // fall-through) and placeholder Return arms for matched transitions.
    assert!(
        step_fn.blocks.len() > 1,
        "Slice 4a dispatch shell has multiple blocks; got {}",
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
    assert!(
        step_fn
            .blocks
            .iter()
            .any(|b| matches!(b.terminator, Terminator::Return)),
        "matched arms return target-state placeholders"
    );
    assert!(
        step_fn.blocks.iter().any(|b| {
            b.instructions.iter().any(|i| {
                matches!(
                    i,
                    Instr::Move {
                        src: Place::MachineTag(_),
                        ..
                    }
                )
            })
        }),
        "outer switch shell reads state via Place::MachineTag"
    );
    assert!(
        step_fn.blocks.iter().any(|b| {
            b.instructions
                .iter()
                .any(|i| matches!(i, Instr::EnumTagLoad { .. }))
        }),
        "inner switch shell reads event via Instr::EnumTagLoad"
    );
    assert!(
        step_fn.blocks.iter().any(|b| {
            b.instructions.iter().any(|i| {
                matches!(
                    i,
                    Instr::Move {
                        dest: Place::MachineTag(_),
                        ..
                    }
                )
            })
        }),
        "placeholder return writes the target state tag through Place::MachineTag"
    );
    assert!(
        !step_fn.blocks.iter().any(|b| {
            b.instructions
                .iter()
                .any(|i| matches!(i, Instr::Drop { .. }))
        }),
        "Slice 4a emits no transition-out drop elaboration"
    );

    let block = |id| {
        step_fn
            .blocks
            .iter()
            .find(|b| b.id == id)
            .unwrap_or_else(|| panic!("block {id} present"))
    };
    let event_check_value = |id| {
        block(id)
            .instructions
            .iter()
            .find_map(|instr| {
                if let Instr::ConstI64 { value, .. } = instr {
                    Some(*value)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("event-check block {id} has ConstI64"))
    };
    let Terminator::Goto {
        target: first_state_check,
    } = block(0).terminator
    else {
        panic!("entry block should goto first state check");
    };
    let Terminator::Branch {
        then_target: red_state_body,
        ..
    } = block(first_state_check).terminator
    else {
        panic!("first state check should branch to Red state body");
    };
    let Terminator::Goto {
        target: red_first_event_check,
    } = block(red_state_body).terminator
    else {
        panic!("Red state body should goto first event check");
    };
    assert_eq!(
        event_check_value(red_first_event_check),
        0,
        "specific Tick arm (event_idx=0) is checked before wildcard arms"
    );
    let Terminator::Branch {
        else_target: red_second_event_check,
        ..
    } = block(red_first_event_check).terminator
    else {
        panic!("first Red event check should branch");
    };
    assert_eq!(
        event_check_value(red_second_event_check),
        1,
        "wildcard Timeout arm (event_idx=1) is slotted after specific arms"
    );
    let trap_block = step_fn
        .blocks
        .iter()
        .find(|b| {
            matches!(
                b.terminator,
                Terminator::Trap {
                    kind: TrapKind::MachineDispatchUnreachable
                }
            )
        })
        .expect("default trap block present");
    let Terminator::Branch {
        else_target: wildcard_else,
        ..
    } = block(red_second_event_check).terminator
    else {
        panic!("wildcard Red event check should branch");
    };
    assert_eq!(
        wildcard_else, trap_block.id,
        "default trap is the lowest-priority event arm"
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
    // Variants and events are present in declaration order for downstream
    // tagged-union layout consumers.
    assert_eq!(layout.variants.len(), 2, "one variant per declared state");
    assert_eq!(
        layout.events.len(),
        2,
        "one event variant per declared event"
    );
    assert_eq!(layout.variants[0].name, "Red", "first state name preserved");
    assert_eq!(
        layout.variants[1].name, "Green",
        "second state name preserved"
    );
    assert_eq!(layout.events[0].name, "Tick", "first event name preserved");
    assert_eq!(
        layout.events[1].name, "Timeout",
        "second event name preserved"
    );
}
