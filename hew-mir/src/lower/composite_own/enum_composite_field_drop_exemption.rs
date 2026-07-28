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
        &local_tys,
        &record_field_orders,
        &enum_layouts,
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
        &[box_ty, ResolvedTy::String, ResolvedTy::String],
        &HashMap::new(),
        &layouts,
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
        &HashMap::new(),
        &HashMap::new(),
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
        &[outer, middle, inner],
        &HashMap::new(),
        &layouts,
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
