use super::*;

fn field_drop(base: u32, field: u32, ty: ResolvedTy) -> Instr {
    Instr::FieldDropInPlace {
        base: Place::Local(base),
        field: crate::model::FieldAddr::Tuple(field),
        ty,
    }
}

fn blocks() -> Vec<BasicBlock> {
    vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::TupleConstruct {
                    elements: vec![],
                    dest: Place::Local(2),
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(0),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(0),
                    fields: vec![0],
                    transferee: Place::Local(1),
                    scope_exit_owner: None,
                },
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(9),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                field_drop(2, 0, ResolvedTy::String),
                field_drop(2, 1, ResolvedTy::String),
            ],
            terminator: Terminator::Goto { target: 2 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(2),
                },
            ],
            terminator: Terminator::Return,
        },
    ]
}

fn proves(blocks: &[BasicBlock]) -> bool {
    proves_with(
        blocks,
        &[
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
            ResolvedTy::String,
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::String]),
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::String]),
        ],
        &HashMap::new(),
    )
}

fn proves_with(
    blocks: &[BasicBlock],
    local_tys: &[ResolvedTy],
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
) -> bool {
    derive_tuple_projection_forward_transfers(
        blocks,
        &HashMap::new(),
        &[(0, 0)].into_iter().collect(),
        local_tys,
        record_field_orders,
        &[],
    )
    .owner_exempt_roots
    .get(&3)
    .is_some_and(|roots| roots.contains(&0))
}

fn enum_carrier_ty() -> ResolvedTy {
    ResolvedTy::named_user("Carrier", vec![])
}

fn enum_layouts() -> Vec<crate::model::EnumLayout> {
    vec![crate::model::EnumLayout {
        name: "Carrier".to_string(),
        tag_width: 1,
        variants: vec![
            crate::model::MachineVariantLayout {
                name: "Full".to_string(),
                field_tys: vec![ResolvedTy::named_builtin(
                    "Vec",
                    hew_types::BuiltinType::Vec,
                    vec![ResolvedTy::String],
                )],
                field_names: vec![],
            },
            crate::model::MachineVariantLayout {
                name: "Empty".to_string(),
                field_tys: vec![],
                field_names: vec![],
            },
        ],
        is_indirect: false,
    }]
}

fn empty_enum_carrier_blocks() -> Vec<BasicBlock> {
    vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(5),
                    value: 1,
                },
                Instr::Move {
                    dest: Place::MachineTag(4),
                    src: Place::Local(5),
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(4),
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(0),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(0),
                    fields: vec![0],
                    transferee: Place::Local(1),
                    scope_exit_owner: None,
                },
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(9),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 2 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
                Instr::Move {
                    dest: Place::Local(3),
                    src: Place::Local(2),
                },
            ],
            terminator: Terminator::Return,
        },
    ]
}

fn empty_enum_carrier_proofs(
    blocks: &[BasicBlock],
    layouts: &[crate::model::EnumLayout],
) -> TupleProjectionForwardProofs {
    let carrier = enum_carrier_ty();
    derive_tuple_projection_forward_transfers(
        blocks,
        &HashMap::new(),
        &[(0, 0)].into_iter().collect(),
        &[
            ResolvedTy::Tuple(vec![carrier.clone(), ResolvedTy::I64]),
            carrier.clone(),
            carrier.clone(),
            carrier.clone(),
            carrier,
            ResolvedTy::I64,
        ],
        &HashMap::new(),
        layouts,
    )
}

fn empty_enum_carrier_proves(blocks: &[BasicBlock]) -> bool {
    empty_enum_carrier_proofs(blocks, &enum_layouts())
        .owner_exempt_roots
        .get(&3)
        .is_some_and(|roots| roots.contains(&0))
}

#[test]
fn exact_tuple_projection_forwarding_is_proven() {
    assert!(
        proves(&blocks()),
        "the exact branch-cleanup-join forwarding shape must earn its scoped proof"
    );
}

#[test]
fn proven_empty_enum_carrier_allows_projection_forwarding() {
    assert!(
        empty_enum_carrier_proves(&empty_enum_carrier_blocks()),
        "an empty cleanup is sound only when the exact initial enum generation has one \
             constructor tag for a declared payload-free variant"
    );
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the explicit six-block fixture keeps the disjoint transfer and empty-cleanup CFG \
              visible to the ownership proof"
)]
fn disjoint_empty_assignment_cleanup_preserves_projection_forwarding() {
    let carrier = enum_carrier_ty();
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(5),
                    value: 1,
                },
                Instr::Move {
                    dest: Place::MachineTag(4),
                    src: Place::Local(5),
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(4),
                },
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(9),
                then_target: 1,
                else_target: 3,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                Instr::TupleFieldLoad {
                    tuple: Place::Local(0),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(0),
                    fields: vec![0],
                    transferee: Place::Local(1),
                    scope_exit_owner: None,
                },
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(8),
                then_target: 2,
                else_target: 4,
            },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 4 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![
                Instr::Drop {
                    place: Place::Local(2),
                    ty: carrier.clone(),
                    drop_fn: Some(crate::model::DropFnSpec::InPlace(
                        crate::ownership::InPlaceReleaseKind::Enum,
                    )),
                },
                Instr::ConstI64 {
                    dest: Place::Local(7),
                    value: 1,
                },
                Instr::Move {
                    dest: Place::MachineTag(6),
                    src: Place::Local(7),
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(6),
                },
            ],
            terminator: Terminator::Goto { target: 5 },
        },
        BasicBlock {
            id: 4,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            }],
            terminator: Terminator::Goto { target: 5 },
        },
        BasicBlock {
            id: 5,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(2),
            }],
            terminator: Terminator::Return,
        },
    ];
    let proofs = derive_tuple_projection_forward_transfers(
        &blocks,
        &HashMap::new(),
        &[(0, 0)].into_iter().collect(),
        &[
            ResolvedTy::Tuple(vec![carrier.clone(), ResolvedTy::I64]),
            carrier.clone(),
            carrier.clone(),
            carrier.clone(),
            carrier.clone(),
            ResolvedTy::I64,
            carrier.clone(),
            ResolvedTy::I64,
            ResolvedTy::Bool,
            ResolvedTy::Bool,
        ],
        &HashMap::new(),
        &enum_layouts(),
    );
    assert!(
        proofs
            .owner_exempt_roots
            .get(&3)
            .is_some_and(|roots| roots.contains(&0)),
        "an enum-in-place cleanup confined to the alternate exact-empty assignment arm \
         must not suppress the tuple sibling's residual drop authority"
    );
}

#[test]
fn owner_exemption_does_not_admit_an_unrelated_tuple_root() {
    let alias_of = [(10, 0), (11, 1)].into_iter().collect();
    let mut excluded = HashSet::new();
    let exempt = [0].into_iter().collect();

    exclude_tuple_roots_except(&alias_of, &mut excluded, Some(&exempt));

    assert_eq!(
        excluded,
        [1].into_iter().collect(),
        "a proven owner exemption must remain scoped to its proven tuple root"
    );
}

#[test]
fn enum_carrier_requires_exact_empty_generation_evidence() {
    let mut nonempty_tag = empty_enum_carrier_blocks();
    let Instr::ConstI64 { value, .. } = &mut nonempty_tag[0].instructions[0] else {
        panic!("fixture must initialize the enum tag");
    };
    *value = 0;
    assert!(
        !empty_enum_carrier_proves(&nonempty_tag),
        "an empty cleanup cannot overwrite a generation tagged for a payload-bearing variant"
    );

    let mut payload_write = empty_enum_carrier_blocks();
    payload_write[0].instructions.insert(
        2,
        Instr::Move {
            dest: Place::MachineVariant {
                local: 4,
                variant_idx: 0,
                field_idx: 0,
            },
            src: Place::Local(1),
        },
    );
    assert!(
        !empty_enum_carrier_proves(&payload_write),
        "a seed with any payload write must not earn an empty-cleanup exemption"
    );

    let mut reused_tag = empty_enum_carrier_blocks();
    reused_tag[0].instructions.insert(
        2,
        Instr::IntAdd {
            dest: Place::Local(8),
            lhs: Place::Local(5),
            rhs: Place::Local(9),
        },
    );
    assert!(
        !empty_enum_carrier_proves(&reused_tag),
        "the constructor tag must be uniquely consumed by its exact enum-tag write"
    );

    let carrier = enum_carrier_ty();
    let no_layout_proof = derive_tuple_projection_forward_transfers(
        &empty_enum_carrier_blocks(),
        &HashMap::new(),
        &[(0, 0)].into_iter().collect(),
        &[
            ResolvedTy::Tuple(vec![carrier.clone(), ResolvedTy::I64]),
            carrier.clone(),
            carrier.clone(),
            carrier.clone(),
            carrier,
            ResolvedTy::I64,
        ],
        &HashMap::new(),
        &[],
    );
    assert!(
        no_layout_proof.owner_exempt_roots.is_empty(),
        "an unresolved enum layout must remain denied rather than treating an empty cleanup \
            as type-only evidence"
    );
}

#[test]
fn enum_carrier_rejects_ambiguous_tags_layouts_and_definitions() {
    let mut duplicate_tag_write = empty_enum_carrier_blocks();
    duplicate_tag_write[0].instructions.insert(
        2,
        Instr::Move {
            dest: Place::MachineTag(4),
            src: Place::Local(5),
        },
    );
    assert!(
        !empty_enum_carrier_proves(&duplicate_tag_write),
        "two writes of the constructor tag cannot prove one exact empty generation"
    );

    let mut nonconstant_tag = empty_enum_carrier_blocks();
    nonconstant_tag[0].instructions[0] = Instr::IntAdd {
        dest: Place::Local(5),
        lhs: Place::Local(6),
        rhs: Place::Local(7),
    };
    assert!(
        !empty_enum_carrier_proves(&nonconstant_tag),
        "a computed tag source cannot prove the declared payload-free variant"
    );

    let mut active_definition_before_neutralize = empty_enum_carrier_blocks();
    active_definition_before_neutralize[0].instructions.insert(
        3,
        Instr::Move {
            dest: Place::Local(2),
            src: Place::Local(3),
        },
    );
    assert!(
        !empty_enum_carrier_proves(&active_definition_before_neutralize),
        "a second carrier generation reaching neutralization must not inherit empty cleanup"
    );

    let mut indirect_layouts = enum_layouts();
    indirect_layouts[0].is_indirect = true;
    assert!(
        empty_enum_carrier_proofs(&empty_enum_carrier_blocks(), &indirect_layouts)
            .owner_exempt_roots
            .is_empty(),
        "an indirect enum layout must remain outside the inline empty-generation proof"
    );
}

#[test]
fn detached_owner_exemption_requires_one_dominating_destination_generation() {
    let mut reused_dest = empty_enum_carrier_blocks();
    reused_dest[2].instructions.extend([
        Instr::Move {
            dest: Place::Local(6),
            src: Place::MachineVariant {
                local: 3,
                variant_idx: 0,
                field_idx: 0,
            },
        },
        Instr::Move {
            dest: Place::Local(6),
            src: Place::Local(4),
        },
    ]);

    let proofs = empty_enum_carrier_proofs(&reused_dest, &enum_layouts());
    assert!(
        proofs
            .owner_exempt_roots
            .get(&6)
            .is_none_or(HashSet::is_empty),
        "a reused destination must not apply one detached generation's exemption to another"
    );
}

#[test]
fn empty_enum_constructor_tag_must_dominate_carrier_initialization() {
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Branch {
                cond: Place::Local(9),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                Instr::ConstI64 {
                    dest: Place::Local(5),
                    value: 1,
                },
                Instr::Move {
                    dest: Place::MachineTag(4),
                    src: Place::Local(5),
                },
            ],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(4),
            }],
            terminator: Terminator::Return,
        },
    ];
    assert!(
        !exact_empty_enum_carrier_initialization(
            &blocks,
            &block_dominators(&blocks),
            InstrSite { block: 3, index: 0 },
            2,
            &[
                ResolvedTy::I64,
                ResolvedTy::I64,
                enum_carrier_ty(),
                ResolvedTy::I64,
                enum_carrier_ty(),
                ResolvedTy::I64,
            ],
            &enum_layouts(),
        ),
        "reachability from one predecessor is not proof that the empty tag reaches the \
             constructor assignment on every path"
    );
}

#[test]
fn partial_duplicate_mismatched_or_extra_cleanup_rejects() {
    let mut partial = blocks();
    partial[1].instructions.pop();
    assert!(
        !proves(&partial),
        "a partial carrier cleanup must not grant a forwarding exemption"
    );

    let mut duplicate = blocks();
    duplicate[1]
        .instructions
        .push(field_drop(2, 1, ResolvedTy::String));
    assert!(
        !proves(&duplicate),
        "a duplicate carrier field cleanup must fail closed"
    );

    let mut wrong_address_kind = blocks();
    let Instr::FieldDropInPlace { field, .. } = &mut wrong_address_kind[1].instructions[0] else {
        panic!("fixture cleanup must remain a field drop");
    };
    *field = crate::model::FieldAddr::Record(FieldOffset(0));
    assert!(
        !proves(&wrong_address_kind),
        "a tuple carrier cleanup must use tuple field addresses"
    );

    let mut wrong_declared_type = blocks();
    let Instr::FieldDropInPlace { ty, .. } = &mut wrong_declared_type[1].instructions[0] else {
        panic!("fixture cleanup must remain a field drop");
    };
    *ty = ResolvedTy::I64;
    assert!(
        !proves(&wrong_declared_type),
        "a cleanup declared type must exactly match its carrier field"
    );

    let mut extra = blocks();
    extra[1]
        .instructions
        .push(field_drop(2, 2, ResolvedTy::String));
    assert!(
        !proves(&extra),
        "an extra carrier cleanup field must fail closed"
    );
}

#[test]
fn record_carrier_cleanup_uses_declared_order_and_leaf_contract() {
    let mut cfg = blocks();
    cfg[1].instructions = vec![
        Instr::RecordFieldDrop {
            record: Place::Local(2),
            field_offset: FieldOffset(0),
            ty: ResolvedTy::String,
            drop_fn: crate::model::DropFnSpec::Release("hew_string_drop"),
        },
        Instr::FieldDropInPlace {
            base: Place::Local(2),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        },
    ];
    let record_ty = ResolvedTy::named_user("Carrier", vec![]);
    let local_tys = vec![
        ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]),
        ResolvedTy::String,
        record_ty.clone(),
        record_ty,
    ];
    let record_field_orders = [(
        "Carrier".to_string(),
        vec![
            ("first".to_string(), ResolvedTy::String),
            ("second".to_string(), ResolvedTy::String),
        ],
    )]
    .into_iter()
    .collect();
    assert!(
        proves_with(&cfg, &local_tys, &record_field_orders),
        "a record carrier must clean every declared heap-owning field exactly once"
    );

    let Instr::RecordFieldDrop { drop_fn, .. } = &mut cfg[1].instructions[0] else {
        panic!("fixture cleanup must remain a RecordFieldDrop");
    };
    *drop_fn = crate::model::DropFnSpec::Release("hew_bytes_drop");
    assert!(
        !proves_with(&cfg, &local_tys, &record_field_orders),
        "RecordFieldDrop must retain its Bytes leaf release-contract validation"
    );
}

#[test]
fn record_field_drop_contract_requires_an_exact_leaf_symbol() {
    let release = |symbol| crate::model::DropFnSpec::Release(symbol);
    let vec_string =
        ResolvedTy::named_builtin("Vec", hew_types::BuiltinType::Vec, vec![ResolvedTy::String]);

    assert!(record_field_drop_contract_is_valid(
        &ResolvedTy::String,
        &release("hew_string_drop"),
        &HashMap::new(),
        &[],
    ));
    assert!(record_field_drop_contract_is_valid(
        &ResolvedTy::Bytes,
        &release("hew_bytes_drop"),
        &HashMap::new(),
        &[],
    ));
    assert!(!record_field_drop_contract_is_valid(
        &ResolvedTy::String,
        &release("hew_hashmap_free_layout"),
        &HashMap::new(),
        &[],
    ));
    assert!(!record_field_drop_contract_is_valid(
        &ResolvedTy::String,
        &release("hew_vec_free"),
        &HashMap::new(),
        &[],
    ));
    assert!(!record_field_drop_contract_is_valid(
        &vec_string,
        &release("hew_string_drop"),
        &HashMap::new(),
        &[],
    ));
    assert!(record_field_drop_contract_is_valid(
        &vec_string,
        &release("hew_vec_free"),
        &HashMap::new(),
        &[],
    ));
    assert!(!record_field_drop_contract_is_valid(
        &ResolvedTy::Tuple(vec![ResolvedTy::String]),
        &release("hew_string_drop"),
        &HashMap::new(),
        &[],
    ));
}

#[test]
fn carrier_third_write_rejects() {
    let mut cfg = blocks();
    cfg[2].instructions.push(Instr::ConstI64 {
        dest: Place::Local(2),
        value: 0,
    });
    assert!(!proves(&cfg), "a third carrier definition must fail closed");
}

#[test]
fn final_owner_second_write_rejects() {
    let mut cfg = blocks();
    cfg[2].instructions.push(Instr::ConstI64 {
        dest: Place::Local(3),
        value: 0,
    });
    assert!(!proves(&cfg), "the final owner must be uniquely written");
}

#[test]
fn two_forward_destinations_reject() {
    let mut cfg = blocks();
    cfg[2].instructions.push(Instr::Move {
        dest: Place::Local(4),
        src: Place::Local(2),
    });
    assert!(!proves(&cfg), "a second carrier forward must fail closed");
}

#[test]
fn extra_cleanup_or_join_predecessor_rejects() {
    let mut cleanup_pred = blocks();
    cleanup_pred.push(BasicBlock {
        id: 3,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Goto { target: 1 },
    });
    assert!(
        !proves(&cleanup_pred),
        "cleanup must have only the neutralize predecessor"
    );

    let mut join_pred = blocks();
    join_pred.push(BasicBlock {
        id: 3,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Goto { target: 2 },
    });
    assert!(
        !proves(&join_pred),
        "join must have only the neutralize and cleanup predecessors"
    );
}

#[test]
fn cleanup_carrier_read_foreign_drop_or_unrecorded_access_rejects() {
    let mut carrier_read = blocks();
    carrier_read[1].instructions.push(Instr::IntAdd {
        dest: Place::Local(8),
        lhs: Place::Local(2),
        rhs: Place::Local(9),
    });
    assert!(
        !proves(&carrier_read),
        "a cleanup carrier read outside FieldDropInPlace must fail closed"
    );

    let mut foreign_drop = blocks();
    foreign_drop[1]
        .instructions
        .push(field_drop(8, 1, ResolvedTy::String));
    assert!(
        !proves(&foreign_drop),
        "a cleanup drop through a foreign base must fail closed"
    );

    let mut cleanup_move = blocks();
    cleanup_move[1].instructions.push(Instr::Move {
        dest: Place::Local(8),
        src: Place::Local(2),
    });
    assert!(
        !proves(&cleanup_move),
        "an unrecorded cleanup carrier move must fail closed"
    );
}

#[test]
fn root_path_mismatch_or_multiply_defined_seed_rejects() {
    let mut path_mismatch = blocks();
    let Instr::AggregateProjectionNeutralize { fields, .. } = &mut path_mismatch[0].instructions[2]
    else {
        panic!("fixture neutralize must remain at its recorded site");
    };
    *fields = vec![1];
    assert!(
        !proves(&path_mismatch),
        "the neutralize path must exactly corroborate the TupleFieldLoad chain"
    );

    let mut multiple_seed_defs = blocks();
    multiple_seed_defs[2].instructions.push(Instr::ConstI64 {
        dest: Place::Local(1),
        value: 0,
    });
    assert!(
        !proves(&multiple_seed_defs),
        "a multiply-defined projection seed must fail closed"
    );
}

fn nested_blocks() -> Vec<BasicBlock> {
    vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::TupleConstruct {
                    elements: vec![],
                    dest: Place::Local(2),
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(0),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(1),
                    field_index: 1,
                    dest: Place::Local(3),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(0),
                    fields: vec![0, 1],
                    transferee: Place::Local(3),
                    scope_exit_owner: None,
                },
            ],
            terminator: Terminator::Branch {
                cond: Place::Local(9),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![
                field_drop(2, 0, ResolvedTy::String),
                field_drop(2, 1, ResolvedTy::String),
            ],
            terminator: Terminator::Goto { target: 2 },
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(3),
                },
                Instr::Move {
                    dest: Place::Local(5),
                    src: Place::Local(2),
                },
            ],
            terminator: Terminator::Return,
        },
    ]
}

fn nested_proves(blocks: &[BasicBlock]) -> bool {
    let inner = ResolvedTy::Tuple(vec![ResolvedTy::I64, ResolvedTy::String]);
    let root = ResolvedTy::Tuple(vec![inner.clone(), ResolvedTy::I64]);
    derive_tuple_projection_forward_transfers(
        blocks,
        &HashMap::new(),
        &[(0, 0)].into_iter().collect(),
        &[
            root,
            inner.clone(),
            ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::String]),
            ResolvedTy::String,
            inner,
            ResolvedTy::String,
        ],
        &HashMap::new(),
        &[],
    )
    .owner_exempt_roots
    .get(&5)
    .is_some_and(|roots| roots.contains(&0))
}

#[test]
fn clean_two_hop_projection_chain_is_proven() {
    assert!(
        nested_proves(&nested_blocks()),
        "a unique, in-order root-to-intermediate-to-seed chain must earn its scoped proof"
    );
}

#[test]
fn out_of_order_or_overwritten_nested_projection_chain_rejects() {
    let mut out_of_order = nested_blocks();
    out_of_order[0].instructions.swap(1, 2);
    assert!(
        !nested_proves(&out_of_order),
        "nested projection definitions must occur strictly root-to-leaf before neutralization"
    );

    let mut overwritten = nested_blocks();
    let matching_root_projection = overwritten[0].instructions.remove(1);
    overwritten[0].instructions.insert(
        1,
        Instr::TupleFieldLoad {
            tuple: Place::Local(4),
            field_index: 0,
            dest: Place::Local(1),
        },
    );
    overwritten[0]
        .instructions
        .insert(3, matching_root_projection);
    let type_classes = hew_hir::TypeClassTable::new();
    assert!(
        crate::dataflow::analyze(&overwritten, &type_classes, &[])
            .checks
            .is_empty(),
        "the adversarial chain must be accepted by the checked-MIR dataflow authority"
    );
    assert!(
        !nested_proves(&overwritten),
        "a foreign intermediate load used to produce the seed, then overwritten by the \
             matching root projection, must not earn the forwarding exemption"
    );
}

#[test]
fn cfg_cycle_suspend_or_backedge_rejects() {
    let mut cycle = blocks();
    cycle[2].terminator = Terminator::Goto { target: 0 };
    assert!(!proves(&cycle), "a proof-region CFG cycle must fail closed");

    let mut suspend = blocks();
    suspend.push(BasicBlock {
        id: 3,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Suspend {
            resume: 3,
            cleanup: 3,
            is_final: false,
        },
    });
    assert!(
        !proves(&suspend),
        "an unclassified suspend carrier must fail closed"
    );

    let mut backedge = blocks();
    backedge[2].terminator = Terminator::Goto { target: 3 };
    backedge.push(BasicBlock {
        id: 3,
        statements: vec![],
        instructions: vec![],
        terminator: Terminator::Goto { target: 1 },
    });
    assert!(
        !proves(&backedge),
        "a join backedge into the proof region must fail closed"
    );
}

#[test]
fn ordinary_unique_same_block_forwarding_remains_admitted() {
    let binding = BindingId(1);
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]);
    let allowed = derive_tuple_composite_drop_allowed(
        &[BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![
                Instr::TupleFieldLoad {
                    tuple: Place::Local(0),
                    field_index: 0,
                    dest: Place::Local(1),
                },
                Instr::AggregateProjectionNeutralize {
                    root: Place::Local(0),
                    fields: vec![0],
                    transferee: Place::Local(1),
                    scope_exit_owner: None,
                },
                Instr::Move {
                    dest: Place::Local(2),
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        &HashMap::new(),
        &[(binding, "pair".to_string(), tuple_ty.clone())],
        &[(binding, Place::Local(0))].into_iter().collect(),
        &[tuple_ty, ResolvedTy::String, ResolvedTy::String],
        &HashMap::new(),
        &[],
        &[],
        &HashMap::new(),
    );
    assert!(
        allowed.contains(&binding),
        "the cross-block proof must not change ordinary same-block forwarding"
    );
}
