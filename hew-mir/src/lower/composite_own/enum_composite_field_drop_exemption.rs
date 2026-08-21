//! Pins for the enum composite prover's `FieldDropInPlace` handling: the
//! blanket-scan exemption (the op is an interior discharge, not a payload
//! READ into an owning sink) paired with the DIRECT exclusion rule (a
//! base that is an alias member or a payload binder frees payload leaves
//! through a byte-alias of the composite's storage, so the composite must
//! be excluded — its `EnumInPlace` walk would re-free them; the
//! empirically reproduced two-step nested destructure `match opt {
//! Some(row) => match row { Row { a, b: _ } => … } }` aborted under
//! Guard-Malloc while the composite stayed admitted). The differential
//! control proves a genuine owning-sink read of the same binder still
//! excludes the composite.
use super::*;

fn opt_ty() -> ResolvedTy {
    ResolvedTy::named_user("Opt", vec![])
}

fn row_ty() -> ResolvedTy {
    ResolvedTy::named_user("Row", vec![])
}

fn scope(parent: Option<ScopeId>) -> ScopeInfoEntry {
    ScopeInfoEntry {
        parent,
        min_start: 0,
        max_end: 0,
    }
}

#[test]
fn payload_handoff_scope_order_accepts_only_same_or_lexically_nested_destinations() {
    let root = ScopeId(10);
    let child = ScopeId(20);
    let grandchild = ScopeId(30);
    let unrelated = ScopeId(40);
    let cycle_a = ScopeId(50);
    let cycle_b = ScopeId(60);
    let cycle_c = ScopeId(70);
    let scope_info = [
        (root, scope(None)),
        (child, scope(Some(root))),
        (grandchild, scope(Some(child))),
        (unrelated, scope(None)),
        (cycle_a, scope(Some(cycle_b))),
        (cycle_b, scope(Some(cycle_c))),
        (cycle_c, scope(Some(cycle_a))),
    ]
    .into_iter()
    .collect();

    assert!(scope_is_same_or_nested(root, root, &scope_info));
    assert!(scope_is_same_or_nested(child, root, &scope_info));
    assert!(scope_is_same_or_nested(grandchild, root, &scope_info));
    assert!(!scope_is_same_or_nested(root, child, &scope_info));
    assert!(!scope_is_same_or_nested(unrelated, root, &scope_info));
    assert!(!scope_is_same_or_nested(ScopeId(80), root, &scope_info));
    assert!(!scope_is_same_or_nested(
        ScopeId(80),
        ScopeId(80),
        &scope_info
    ));
    assert!(!scope_is_same_or_nested(cycle_a, root, &scope_info));
    assert!(!scope_is_same_or_nested(cycle_a, cycle_b, &scope_info));
    assert!(!scope_is_same_or_nested(cycle_b, cycle_a, &scope_info));
    assert!(!scope_is_same_or_nested(cycle_a, cycle_a, &scope_info));
}

fn derive(instrs: Vec<Instr>) -> (BindingId, HashSet<BindingId>) {
    let b = BindingId(1);
    let owned = vec![(b, "o".to_string(), opt_ty())];
    let mut binding_locals: HashMap<BindingId, Place> = HashMap::new();
    binding_locals.insert(b, Place::Local(0));
    // local 0: the Opt composite; local 1: the Row payload binder;
    // local 2: a general-storage sink for the differential control.
    let local_tys = vec![
        opt_ty(),
        row_ty(),
        ResolvedTy::Tuple(vec![row_ty()]),
        ResolvedTy::String,
        ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![ResolvedTy::String]),
    ];
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    record_field_orders.insert(
        "Row".to_string(),
        vec![
            ("inner".to_string(), ResolvedTy::String),
            ("tag".to_string(), ResolvedTy::String),
            (
                "values".to_string(),
                ResolvedTy::named_builtin(
                    "Vec",
                    hew_types::BuiltinType::Vec,
                    vec![ResolvedTy::String],
                ),
            ),
        ],
    );
    let enum_layouts = vec![crate::model::EnumLayout {
        name: "Opt".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Some".to_string(),
                field_tys: vec![row_ty()],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "None".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions: instrs,
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &owned,
        &binding_locals,
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
        &local_tys,
        &record_field_orders,
        &enum_layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );
    (b, allowed)
}

/// `Some(r)` destructure: the payload binder receives the interior
/// projection of the composite.
fn payload_destructure() -> Instr {
    Instr::Move {
        dest: Place::Local(1),
        src: Place::EnumVariant {
            local: 0,
            variant_idx: 0,
            field_idx: 0,
        },
    }
}

fn derive_string_payload_handoff(instructions: Vec<Instr>) -> (BindingId, HashSet<BindingId>) {
    let parent = BindingId(20);
    let payload = BindingId(21);
    let copy = BindingId(22);
    let box_ty = ResolvedTy::named_user("TextBox", vec![]);
    let binding_locals = HashMap::from([
        (parent, Place::Local(0)),
        (payload, Place::Local(1)),
        (copy, Place::Local(2)),
    ]);
    let binding_scope = HashMap::from([
        (parent, ScopeId(1)),
        (payload, ScopeId(2)),
        (copy, ScopeId(3)),
    ]);
    let layouts = vec![crate::model::EnumLayout {
        name: "TextBox".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Text".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[(parent, "box".to_string(), box_ty.clone())],
        &binding_locals,
        &binding_scope,
        &HashMap::new(),
        &HashMap::new(),
        &[box_ty, ResolvedTy::String, ResolvedTy::String],
        &HashMap::new(),
        &layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );
    (parent, allowed)
}

#[test]
fn exact_retained_string_payload_handoff_keeps_parent_drop() {
    let (parent, allowed) = derive_string_payload_handoff(vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 0,
                variant_idx: 0,
                field_idx: 0,
            },
        },
        Instr::StringRetain {
            value: Place::Local(1),
            condition: StringRetainCondition::Always,
        },
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(1),
        },
    ]);
    assert!(
        allowed.contains(&parent),
        "the parent still owns the original payload ref after the retained copy"
    );
}

#[test]
fn mismatched_string_payload_retain_stays_fail_closed() {
    let (parent, allowed) = derive_string_payload_handoff(vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 0,
                variant_idx: 0,
                field_idx: 0,
            },
        },
        Instr::StringRetain {
            value: Place::Local(2),
            condition: StringRetainCondition::Always,
        },
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(1),
        },
    ]);
    assert!(
        !allowed.contains(&parent),
        "a mismatched retain cannot prove that the escaping payload handoff is independent"
    );
}

#[test]
fn proven_borrow_payload_call_keeps_the_enum_shell_owner() {
    let parent = BindingId(20);
    let payload = BindingId(21);
    let box_ty = ResolvedTy::named_user("TextBox", vec![]);
    let binding_locals = HashMap::from([(parent, Place::Local(0)), (payload, Place::Local(1))]);
    let binding_scope = HashMap::from([(parent, ScopeId(1)), (payload, ScopeId(2))]);
    let layouts = vec![crate::model::EnumLayout {
        name: "TextBox".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Text".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }];
    let blocks = [BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::Move {
            dest: Place::Local(1),
            src: Place::EnumVariant {
                local: 0,
                variant_idx: 0,
                field_idx: 0,
            },
        }],
        terminator: Terminator::Call {
            callee: "inspect".to_string(),
            authority: crate::model::CallAuthority::default(),
            args: vec![Place::Local(1)],
            dest: None,
            next: 1,
        },
    }];
    let derive = |proven_borrow_call_args: &HashMap<u32, HashSet<usize>>| {
        derive_enum_composite_drop_allowed(
            &blocks,
            &HashMap::new(),
            &[(parent, "box".to_string(), box_ty.clone())],
            &binding_locals,
            &binding_scope,
            &HashMap::new(),
            &HashMap::new(),
            &[box_ty.clone(), ResolvedTy::String],
            &HashMap::new(),
            &layouts,
            &hew_hir::TypeClassTable::default(),
            &[],
            &[],
            &hew_hir::LifecycleRegistry::default(),
            &HashSet::new(),
            proven_borrow_call_args,
            &HashSet::new(),
            &HashSet::new(),
            &crate::return_provenance::ExternContractTable::default(),
        )
    };

    let proven = HashMap::from([(0, HashSet::from([0]))]);
    assert!(
        derive(&proven).contains(&parent),
        "a proven read-only payload call leaves the enum shell as the sole owner"
    );
    assert!(
        !derive(&HashMap::new()).contains(&parent),
        "an unproven payload call must remain an ownership escape"
    );
}

fn bytes_box_ty() -> ResolvedTy {
    ResolvedTy::named_user("BytesBox", vec![])
}

fn bytes_box_layout() -> crate::model::EnumLayout {
    crate::model::EnumLayout {
        name: "BytesBox".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Data".to_string(),
                field_tys: vec![ResolvedTy::Bytes],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }
}

fn bytes_payload_load() -> Instr {
    Instr::Move {
        dest: Place::Local(1),
        src: Place::EnumVariant {
            local: 0,
            variant_idx: 0,
            field_idx: 0,
        },
    }
}

fn bytes_payload_handoff() -> Instr {
    Instr::Move {
        dest: Place::Local(2),
        src: Place::Local(1),
    }
}

fn test_block(instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
    BasicBlock {
        id: 0,
        statements: vec![],
        instructions,
        terminator,
    }
}

fn derive_bytes_payload_parent(instructions: Vec<Instr>) -> (BindingId, HashSet<BindingId>) {
    let parent = BindingId(20);
    let payload = BindingId(21);
    let copy = BindingId(22);
    let box_ty = bytes_box_ty();
    let binding_locals = HashMap::from([
        (parent, Place::Local(0)),
        (payload, Place::Local(1)),
        (copy, Place::Local(2)),
    ]);
    // The destination deliberately lives in a nested lexical scope. Without a
    // corroborated retain this is an escaping payload handoff and the parent
    // must stay excluded.
    let binding_scope = HashMap::from([
        (parent, ScopeId(1)),
        (payload, ScopeId(2)),
        (copy, ScopeId(3)),
    ]);
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions,
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[(parent, "box".to_string(), box_ty.clone())],
        &binding_locals,
        &binding_scope,
        &HashMap::new(),
        &HashMap::new(),
        &[box_ty, ResolvedTy::Bytes, ResolvedTy::Bytes],
        &HashMap::new(),
        &[bytes_box_layout()],
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );
    (parent, allowed)
}

#[test]
fn exact_retained_bytes_payload_handoff_keeps_parent_drop() {
    let (parent, allowed) = derive_bytes_payload_parent(vec![
        bytes_payload_load(),
        Instr::BytesRetain {
            value: Place::Local(1),
        },
        bytes_payload_handoff(),
    ]);
    assert!(
        allowed.contains(&parent),
        "the parent balances the original ref while the retained destination owns its +1"
    );
}

#[test]
fn unretained_or_mismatched_bytes_payload_handoff_stays_fail_closed() {
    for (label, instructions) in [
        (
            "unretained",
            vec![bytes_payload_load(), bytes_payload_handoff()],
        ),
        (
            "mismatched",
            vec![
                bytes_payload_load(),
                Instr::BytesRetain {
                    value: Place::Local(2),
                },
                bytes_payload_handoff(),
            ],
        ),
    ] {
        let (parent, allowed) = derive_bytes_payload_parent(instructions);
        assert!(
            !allowed.contains(&parent),
            "{label} payload copy cannot preserve the parent's competing drop"
        );
    }
}

#[test]
fn exact_bytes_payload_handoff_site_is_proven() {
    let copy = BindingId(22);
    let candidates = HashMap::from([(2, copy)]);
    let bindings = HashSet::from([1, 2]);
    let bytes_tys = vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::Bytes];
    let exact = vec![test_block(
        vec![bytes_payload_load(), bytes_payload_handoff()],
        Terminator::Return,
    )];
    assert_eq!(
        provable_bytes_payload_handoff_sites(&exact, &bytes_tys, &candidates, &bindings),
        HashMap::from([(
            (0, 1),
            BytesPayloadHandoff {
                source: Place::Local(1),
                dest_local: 2,
                dest_binding: copy,
            },
        )])
    );
}

#[test]
fn bytes_payload_handoff_proof_rejects_reuse_gap_cycle_and_wrong_type() {
    let copy = BindingId(22);
    let candidates = HashMap::from([(2, copy)]);
    let bindings = HashSet::from([1, 2]);
    let bytes_tys = vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::Bytes];
    let cases = [
        (
            "nonadjacent",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    Instr::UnitLit {
                        dest: Place::Local(3),
                    },
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
            vec![
                bytes_box_ty(),
                ResolvedTy::Bytes,
                ResolvedTy::Bytes,
                ResolvedTy::Unit,
            ],
        ),
        (
            "multiply-written destination",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    bytes_payload_handoff(),
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
            bytes_tys.clone(),
        ),
        (
            "multiply-written source",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    bytes_payload_load(),
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
            bytes_tys.clone(),
        ),
        (
            "terminator overwrite",
            vec![
                test_block(
                    vec![bytes_payload_load(), bytes_payload_handoff()],
                    Terminator::Call {
                        callee: "produce".to_string(),
                        authority: crate::model::CallAuthority::default(),
                        args: vec![],
                        dest: Some(Place::Local(2)),
                        next: 1,
                    },
                ),
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ],
            bytes_tys.clone(),
        ),
        (
            "cyclic generation",
            vec![test_block(
                vec![bytes_payload_load(), bytes_payload_handoff()],
                Terminator::Goto { target: 0 },
            )],
            bytes_tys.clone(),
        ),
        (
            "wrong destination type",
            vec![test_block(
                vec![bytes_payload_load(), bytes_payload_handoff()],
                Terminator::Return,
            )],
            vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::String],
        ),
    ];
    for (label, blocks, local_tys) in cases {
        assert!(
            provable_bytes_payload_handoff_sites(&blocks, &local_tys, &candidates, &bindings)
                .is_empty(),
            "{label} must preserve projection taint and emit no ownership proof"
        );
    }
}

#[test]
fn exact_bytes_retain_move_is_corroborated() {
    let bytes_tys = vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::Bytes];
    let retain = Instr::BytesRetain {
        value: Place::Local(1),
    };
    let exact = vec![test_block(
        vec![
            bytes_payload_load(),
            retain.clone(),
            bytes_payload_handoff(),
        ],
        Terminator::Return,
    )];
    assert_eq!(
        corroborated_retained_bytes_move_sites(&exact, &bytes_tys),
        HashSet::from([(0, 2)])
    );
}

#[test]
fn corroborated_bytes_retain_rejects_gap_mismatch_rewrite_and_cycle() {
    let bytes_tys = vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::Bytes];
    let retain = Instr::BytesRetain {
        value: Place::Local(1),
    };
    let cases = [
        (
            "nonadjacent",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    retain.clone(),
                    Instr::UnitLit {
                        dest: Place::Local(3),
                    },
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
        ),
        (
            "mismatched",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    Instr::BytesRetain {
                        value: Place::Local(2),
                    },
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
        ),
        (
            "multiply-written destination",
            vec![test_block(
                vec![
                    bytes_payload_load(),
                    retain.clone(),
                    bytes_payload_handoff(),
                    bytes_payload_handoff(),
                ],
                Terminator::Return,
            )],
        ),
        (
            "cyclic generation",
            vec![test_block(
                vec![bytes_payload_load(), retain, bytes_payload_handoff()],
                Terminator::Goto { target: 0 },
            )],
        ),
    ];
    for (label, blocks) in cases {
        assert!(
            corroborated_retained_bytes_move_sites(&blocks, &bytes_tys).is_empty(),
            "{label} cannot sever parent payload provenance"
        );
    }
}

#[test]
fn corroborated_bytes_retain_rejects_terminator_overwrite_and_wrong_type() {
    let bytes_tys = vec![bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::Bytes];
    let terminator_overwrite = vec![
        test_block(
            vec![
                bytes_payload_load(),
                Instr::BytesRetain {
                    value: Place::Local(1),
                },
                bytes_payload_handoff(),
            ],
            Terminator::Call {
                callee: "produce".to_string(),
                authority: crate::model::CallAuthority::default(),
                args: vec![],
                dest: Some(Place::Local(2)),
                next: 1,
            },
        ),
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        },
    ];
    assert!(
        corroborated_retained_bytes_move_sites(&terminator_overwrite, &bytes_tys).is_empty(),
        "a terminator-overwritten destination has no stable retained generation"
    );

    let wrong_type = vec![test_block(
        vec![
            bytes_payload_load(),
            Instr::BytesRetain {
                value: Place::Local(1),
            },
            bytes_payload_handoff(),
        ],
        Terminator::Return,
    )];
    assert!(
        corroborated_retained_bytes_move_sites(
            &wrong_type,
            &[bytes_box_ty(), ResolvedTy::Bytes, ResolvedTy::String],
        )
        .is_empty(),
        "a bytes opcode cannot grant ownership to a differently typed destination"
    );
}

/// A `FieldDropInPlace` discharging one skipped field of the payload
/// binder frees payload leaves through the binder's byte-alias of the
/// composite's storage — the composite must be EXCLUDED, or its
/// `EnumInPlace` walk re-frees the discharged field (the reproduced
/// nested-destructure double-free: Guard-Malloc SIGSEGV on the second
/// iteration while the composite stayed admitted). Exclusion leaks the
/// payload remainder instead — the fail-closed direction. This is the
/// direct rule's pin; the blanket-scan exemption alone left the
/// composite admitted.
#[test]
fn field_drop_on_payload_binder_excludes_composite() {
    let (b, allowed) = derive(vec![
        payload_destructure(),
        Instr::FieldDropInPlace {
            base: Place::Local(1),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        },
    ]);
    assert!(
        !allowed.contains(&b),
        "a FieldDropInPlace against the payload binder discharged payload \
         leaves the composite's EnumInPlace walk would re-free; the \
         composite must be excluded (leak-not-double-free); got {allowed:?}"
    );
}

/// Differential control: a genuine owning-sink read of the same payload
/// binder (an aggregate construction) still excludes the composite —
/// the exemption admits exactly the interior discharge op, nothing wider.
#[test]
fn owning_sink_read_of_payload_binder_still_excludes_composite() {
    let (b, allowed) = derive(vec![
        payload_destructure(),
        Instr::TupleConstruct {
            elements: vec![Place::Local(1)],
            dest: Place::Local(2),
        },
    ]);
    assert!(
        !allowed.contains(&b),
        "a payload binder read into an owning sink escaped the composite; \
         it must be excluded (fail-closed); got {allowed:?}"
    );
}

#[test]
fn retained_string_field_read_stays_admitted_but_vec_and_field_drop_do_not() {
    let (b, allowed) = derive(vec![
        payload_destructure(),
        Instr::RecordFieldLoad {
            record: Place::Local(1),
            field_offset: FieldOffset(0),
            dest: Place::Local(3),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(3),
        },
    ]);
    assert!(
        allowed.contains(&b),
        "a retained cloned string field read must not exclude the enum shell; got {allowed:?}"
    );

    let (b, allowed) = derive(vec![
        payload_destructure(),
        Instr::RecordFieldLoad {
            record: Place::Local(1),
            field_offset: FieldOffset(2),
            dest: Place::Local(4),
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(4),
        },
    ]);
    assert!(
        !allowed.contains(&b),
        "a transferred Vec field read into an owning sink must exclude the enum shell; got \
         {allowed:?}"
    );

    let (b, allowed) = derive(vec![
        payload_destructure(),
        Instr::FieldDropInPlace {
            base: Place::Local(1),
            field: crate::model::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::String,
        },
    ]);
    assert!(
        !allowed.contains(&b),
        "FieldDropInPlace must continue excluding the enum shell; got {allowed:?}"
    );
}

#[test]
fn retained_string_exemption_does_not_mask_a_reused_payload_generation() {
    let blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::RecordFieldLoad {
                record: Place::Local(1),
                field_offset: FieldOffset(0),
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(3),
                src: Place::MachineVariant {
                    local: 0,
                    variant_idx: 0,
                    field_idx: 0,
                },
            },
        ],
        terminator: Terminator::Return,
    }];
    assert!(
        !uniquely_defined_retained_string_field_load_aliases(
            &blocks,
            &[opt_ty(), row_ty(), ResolvedTy::I64, ResolvedTy::String],
        )
        .contains(&3),
        "a retained field-load generation must not erase payload taint after the local is \
        reused for an unrelated generation"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the single derivation keeps the direct alias, unneutralized forward, \
              corroborated neutralized transfer, and unrelated-owner controls on \
              one identical MIR/type fixture"
)]
fn nested_enum_payload_candidate_never_duplicates_its_parent_owner() {
    let outer = ResolvedTy::named_user("Outer", vec![]);
    let inner = ResolvedTy::named_user("Inner", vec![]);
    let parent = BindingId(1);
    let nested = BindingId(2);
    let unrelated = BindingId(3);
    let forwarded = BindingId(4);
    let consumed_parent = BindingId(5);
    let consumed = BindingId(6);
    let unsafe_forwarded = BindingId(7);
    let arm_scope = ScopeId(10);
    let nested_arm_scope = ScopeId(11);
    let layouts = vec![
        crate::model::EnumLayout {
            name: "Outer".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "Wrapped".to_string(),
                    field_tys: vec![inner.clone()],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "Empty".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        },
        crate::model::EnumLayout {
            name: "Inner".to_string(),
            tag_width: 1,
            variants: vec![
                crate::model::MachineVariantLayout {
                    name: "Message".to_string(),
                    field_tys: vec![ResolvedTy::String],
                    field_names: vec![],
                },
                crate::model::MachineVariantLayout {
                    name: "Empty".to_string(),
                    field_tys: vec![],
                    field_names: vec![],
                },
            ],
            is_indirect: false,
        },
    ];
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(1),
                    src: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
                Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::Local(5),
                    src: Place::MachineVariant {
                        local: 4,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
                Instr::NeutralizePayloadSlot {
                    place: Place::MachineVariant {
                        local: 4,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                    transferee: None,
                    authority: crate::model::NeutralizeAuthority::MoveOutArmConsume,
                },
                Instr::Move {
                    dest: Place::Local(6),
                    src: Place::Local(5),
                },
            ],
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[
            (parent, "parent".to_string(), outer.clone()),
            (nested, "nested".to_string(), inner.clone()),
            (unrelated, "unrelated".to_string(), inner.clone()),
            (forwarded, "forwarded".to_string(), inner.clone()),
            (
                consumed_parent,
                "consumed_parent".to_string(),
                outer.clone(),
            ),
            (consumed, "consumed".to_string(), inner.clone()),
            (
                unsafe_forwarded,
                "unsafe_forwarded".to_string(),
                inner.clone(),
            ),
        ],
        &[
            (parent, Place::Local(0)),
            (nested, Place::Local(1)),
            (unrelated, Place::Local(2)),
            (forwarded, Place::Local(6)),
            (consumed_parent, Place::Local(4)),
            (consumed, Place::Local(5)),
            (unsafe_forwarded, Place::Local(3)),
        ]
        .into_iter()
        .collect(),
        &[(consumed, arm_scope), (forwarded, nested_arm_scope)]
            .into_iter()
            .collect(),
        &HashMap::new(),
        &[
            (arm_scope, scope(None)),
            (nested_arm_scope, scope(Some(arm_scope))),
        ]
        .into_iter()
        .collect(),
        &[
            outer.clone(),
            inner.clone(),
            inner.clone(),
            inner.clone(),
            outer,
            inner,
            ResolvedTy::named_user("Inner", vec![]),
        ],
        &HashMap::new(),
        &layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );

    assert!(allowed.contains(&parent), "the parent remains the owner");
    assert!(
        !allowed.contains(&nested),
        "a shallow nested enum payload binder must not receive a second EnumInPlace"
    );
    assert!(
        !allowed.contains(&unsafe_forwarded),
        "an unneutralized whole-local forward remains an alias of the parent and \
         must not receive a second EnumInPlace"
    );
    assert!(
        allowed.contains(&unrelated),
        "the interior-alias denial must not suppress an unrelated enum owner"
    );
    assert!(
        allowed.contains(&forwarded),
        "a same-scope handoff from the direct binder becomes the sole owner after \
         transfer neutralization and must retain its EnumInPlace"
    );
    assert!(
        allowed.contains(&consumed),
        "a direct payload destination whose source slot is move-out neutralized is \
         the sole owner and must retain its EnumInPlace"
    );
    assert!(
        allowed.contains(&consumed_parent),
        "the neutralized parent retains its tag-aware no-op drop"
    );
}

#[test]
fn depth_two_nested_enum_payload_candidate_does_not_duplicate_outer_owner() {
    let outer = ResolvedTy::named_user("Outer", vec![]);
    let middle = ResolvedTy::named_user("Middle", vec![]);
    let inner = ResolvedTy::named_user("Inner", vec![]);
    let parent = BindingId(1);
    let nested = BindingId(2);
    let layouts = vec![
        crate::model::EnumLayout {
            name: "Outer".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "Wrapped".to_string(),
                field_tys: vec![middle.clone()],
                field_names: vec![],
            }],
            is_indirect: false,
        },
        crate::model::EnumLayout {
            name: "Middle".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "Wrapped".to_string(),
                field_tys: vec![inner.clone()],
                field_names: vec![],
            }],
            is_indirect: false,
        },
        crate::model::EnumLayout {
            name: "Inner".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "Message".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            }],
            is_indirect: false,
        },
    ];
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(1),
                    src: Place::MachineVariant {
                        local: 0,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::MachineVariant {
                        local: 1,
                        variant_idx: 0,
                        field_idx: 0,
                    },
                },
            ],
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[
            (parent, "parent".to_string(), outer.clone()),
            (nested, "nested".to_string(), inner.clone()),
        ],
        &[(parent, Place::Local(0)), (nested, Place::Local(2))]
            .into_iter()
            .collect(),
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
        &[outer, middle, inner],
        &HashMap::new(),
        &layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );

    assert!(
        allowed.contains(&parent),
        "the outer enum remains the owner"
    );
    assert!(
        !allowed.contains(&nested),
        "a depth-two nested payload alias must not receive a second EnumInPlace"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the paired safe/unsafe tuple-field cases must share one layout and \
              derivation so only ancestor neutralization differs"
)]
fn nested_enum_tuple_field_transfer_requires_ancestor_and_field_neutralization() {
    let inner = ResolvedTy::named_user("Inner", vec![]);
    let tuple = ResolvedTy::Tuple(vec![inner.clone(), ResolvedTy::I64]);
    let outer = ResolvedTy::named_user("Outer", vec![]);
    let safe_parent = BindingId(1);
    let safe_inner = BindingId(2);
    let unsafe_parent = BindingId(3);
    let unsafe_inner = BindingId(4);
    let layouts = vec![
        crate::model::EnumLayout {
            name: "Outer".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "Wrapped".to_string(),
                field_tys: vec![tuple.clone()],
                field_names: vec![],
            }],
            is_indirect: false,
        },
        crate::model::EnumLayout {
            name: "Inner".to_string(),
            tag_width: 1,
            variants: vec![crate::model::MachineVariantLayout {
                name: "Message".to_string(),
                field_tys: vec![ResolvedTy::String],
                field_names: vec![],
            }],
            is_indirect: false,
        },
    ];
    let safe_parent_payload = Place::MachineVariant {
        local: 0,
        variant_idx: 0,
        field_idx: 0,
    };
    let unsafe_parent_payload = Place::MachineVariant {
        local: 3,
        variant_idx: 0,
        field_idx: 0,
    };
    let allowed = derive_enum_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(1),
                    src: safe_parent_payload,
                },
                Instr::NeutralizePayloadSlot {
                    place: safe_parent_payload,
                    transferee: None,
                    authority: crate::model::NeutralizeAuthority::MoveOutArmConsume,
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(1),
                    field_index: 0,
                    dest: Place::Local(2),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(1),
                    fields: vec![0],
                    transferee: Place::Local(2),
                    scope_exit_owner: None,
                },
                Instr::Move {
                    dest: Place::Local(4),
                    src: unsafe_parent_payload,
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(4),
                    field_index: 0,
                    dest: Place::Local(5),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(4),
                    fields: vec![0],
                    transferee: Place::Local(5),
                    scope_exit_owner: None,
                },
            ],
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[
            (safe_parent, "safe_parent".to_string(), outer.clone()),
            (safe_inner, "safe_inner".to_string(), inner.clone()),
            (unsafe_parent, "unsafe_parent".to_string(), outer.clone()),
            (unsafe_inner, "unsafe_inner".to_string(), inner.clone()),
        ],
        &[
            (safe_parent, Place::Local(0)),
            (safe_inner, Place::Local(2)),
            (unsafe_parent, Place::Local(3)),
            (unsafe_inner, Place::Local(5)),
        ]
        .into_iter()
        .collect(),
        &HashMap::new(),
        &HashMap::new(),
        &HashMap::new(),
        &[
            outer.clone(),
            tuple.clone(),
            inner.clone(),
            outer,
            tuple,
            inner,
        ],
        &HashMap::new(),
        &layouts,
        &hew_hir::TypeClassTable::default(),
        &[],
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &HashSet::new(),
        &HashMap::new(),
        &HashSet::new(),
        &HashSet::new(),
        &crate::return_provenance::ExternContractTable::default(),
    );

    assert!(
        allowed.contains(&safe_inner),
        "neutralizing both ancestor slot and tuple field transfers the sole owner"
    );
    assert!(
        !allowed.contains(&unsafe_inner),
        "neutralizing only the tuple copy cannot clear its unneutralized outer parent"
    );
}
