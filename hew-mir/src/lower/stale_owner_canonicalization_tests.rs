use super::*;
use crate::model::{OwnerDropRecipe, OwnerId, OwnershipEvent, OwnershipGuardKind};

fn owner(binding: u32, generation: u32) -> OwnerId {
    OwnerId {
        binding: BindingId(binding),
        generation,
    }
}

fn string_recipe() -> OwnerDropRecipe {
    OwnerDropRecipe {
        ty: ResolvedTy::String,
        drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
        kind: DropKind::CowHeap {
            release: crate::ownership::CowHeapRelease::String,
        },
        declaration_order: 0,
    }
}

fn one_block(instructions: Vec<Instr>) -> Vec<BasicBlock> {
    vec![BasicBlock {
        id: ENTRY_BLOCK_ID,
        statements: Vec::new(),
        instructions,
        terminator: Terminator::Return,
    }]
}

#[test]
fn stale_relocation_after_adoption_rekeys_to_the_unique_live_successor() {
    let provisional = owner(1, 0);
    let adopted = owner(2, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: provisional,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: provisional,
            from: Place::Local(0),
            to: Some(Place::Local(1)),
            to_owner: Some(adopted),
            to_ty: Some(ResolvedTy::String),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: provisional,
            from: Place::Local(1),
            to: Place::ReturnSlot,
        }),
    ]);

    canonicalize_stale_relocation_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[2],
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if owner == adopted
    ));
}

#[test]
fn ambiguous_relocation_source_stays_stale_for_the_validator() {
    let first = owner(1, 0);
    let second = owner(2, 0);
    let stale = owner(3, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: first,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: second,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: stale,
            from: Place::Local(0),
            to: Place::Local(1),
        }),
    ]);

    canonicalize_stale_relocation_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[2],
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if owner == stale
    ));
}

#[test]
fn replayed_lifecycle_turns_a_dead_rearm_reservation_into_a_fresh_definition() {
    let joined = owner(7, 2);
    let historical = owner(7, 0);
    let replacement = owner(7, 1);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: joined,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: joined,
            from: Place::Local(0),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Rearm {
            previous: historical,
            replacement,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
    ]);

    materialize_edge_lifecycle_owner_transitions(&mut blocks, &mut Builder::default());

    assert!(matches!(
        blocks[0].instructions[2],
        Instr::OwnershipEvent(OwnershipEvent::Mint { owner, .. }) if owner == replacement
    ));
}

#[test]
fn live_predecessor_keeps_reset_semantics() {
    let previous = owner(7, 0);
    let replacement = owner(7, 1);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: previous,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Reset {
            previous,
            replacement,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
    ]);

    materialize_edge_lifecycle_owner_transitions(&mut blocks, &mut Builder::default());

    assert!(matches!(
        blocks[0].instructions[1],
        Instr::OwnershipEvent(OwnershipEvent::Reset { previous: owner, .. }) if owner == previous
    ));
}

#[test]
fn replayed_join_inputs_keep_one_exact_owner_per_predecessor_edge() {
    let binding = BindingId(17);
    let preheader_owner = OwnerId {
        binding,
        generation: 0,
    };
    let latch_owner = OwnerId {
        binding,
        generation: 1,
    };
    let place = Place::Local(4);
    let exact_exits = HashMap::from([
        (10, HashMap::from([(preheader_owner, place)])),
        (11, HashMap::from([(latch_owner, place)])),
    ]);
    let maybe_exits = HashMap::from([
        (10, HashSet::from([(preheader_owner, place)])),
        (11, HashSet::from([(latch_owner, place)])),
    ]);

    let inputs = replayed_edge_owner_inputs(
        &[10, 11],
        binding,
        place,
        &exact_exits,
        &maybe_exits,
        &HashMap::new(),
        &HashMap::new(),
    )
    .expect("one exact owner at the expected place on each edge");
    assert_eq!(
        inputs,
        BTreeMap::from([(10, preheader_owner), (11, latch_owner)])
    );

    let mut wrong_place_maybe = maybe_exits;
    wrong_place_maybe
        .get_mut(&11)
        .expect("latch edge exists")
        .insert((latch_owner, Place::Local(5)));
    assert!(
        replayed_edge_owner_inputs(
            &[10, 11],
            binding,
            place,
            &exact_exits,
            &wrong_place_maybe,
            &HashMap::new(),
            &HashMap::new(),
        )
        .is_none(),
        "the producer refuses ambiguous or wrong-place edge facts"
    );
}

#[test]
fn guarded_release_after_join_rekeys_to_the_unique_live_generation() {
    let historical = owner(7, 0);
    let joined = owner(7, 2);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: historical,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: historical,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: historical,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: historical,
            from: Place::Local(0),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: joined,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: joined,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: joined,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
            owner: historical,
            place: Place::Local(0),
            flag: Place::Local(1),
        }),
    ]);

    canonicalize_release_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[7],
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease { owner, .. }) if owner == joined
    ));
}

#[test]
fn ambiguous_guarded_release_owner_stays_validator_visible() {
    let historical = owner(7, 0);
    let first = owner(7, 2);
    let second = owner(7, 3);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: historical,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: historical,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: historical,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: historical,
            from: Place::Local(0),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: first,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: first,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: first,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: second,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner: second,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner: second,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
            owner: historical,
            place: Place::Local(0),
            flag: Place::Local(1),
        }),
    ]);

    canonicalize_release_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[10],
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease { owner, .. }) if owner == historical
    ));
}

#[test]
fn wrong_place_guarded_release_stays_validator_visible() {
    let owner = owner(7, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::DropRecipe {
            owner,
            recipe: string_recipe(),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Guard {
            owner,
            flag: Place::Local(1),
            kind: OwnershipGuardKind::Overwrite,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner,
            from: Place::Local(0),
            to: Place::Local(2),
        }),
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
            owner,
            place: Place::Local(0),
            flag: Place::Local(1),
        }),
    ]);

    canonicalize_release_owner_ids(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[4],
        Instr::OwnershipEvent(OwnershipEvent::GuardedRelease {
            owner: guarded,
            place: Place::Local(0),
            ..
        }) if guarded == owner
    ));
}

#[test]
fn terminal_handoff_replaces_the_historical_post_move_relocation() {
    let current = owner(9, 2);
    let historical = owner(9, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: current,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: current,
            from: Place::Local(0),
            to: Some(Place::ReturnSlot),
            to_owner: None,
            to_ty: None,
        }),
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(0),
        },
        Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::ReturnSlot),
            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: historical,
            from: Place::Local(0),
            to: Place::ReturnSlot,
        }),
    ]);

    canonicalize_pre_move_terminal_relocations(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[4],
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner,
            from: Place::Local(0),
            to: Some(Place::ReturnSlot),
            to_owner: None,
            ..
        }) if owner == current
    ));
}

#[test]
fn unrelated_post_move_relocation_stays_validator_visible() {
    let current = owner(9, 2);
    let historical = owner(9, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: current,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: current,
            from: Place::Local(0),
            to: Some(Place::ReturnSlot),
            to_owner: None,
            to_ty: None,
        }),
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(0),
        },
        Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::ReturnSlot),
            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: historical,
            from: Place::Local(1),
            to: Place::ReturnSlot,
        }),
    ]);

    canonicalize_pre_move_terminal_relocations(&mut blocks);

    assert!(matches!(
        blocks[0].instructions[4],
        Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }) if owner == historical
    ));
}

#[test]
fn joined_relocation_supersedes_historical_copy_before_typed_adoption() {
    let historical = owner(9, 0);
    let joined = owner(9, 2);
    let adopted = owner(10, 0);
    let block = &one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: joined,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: joined,
            from: Place::Local(0),
            to: Place::Local(1),
        }),
        Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::Local(1)),
            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: historical,
            from: Place::Local(0),
            to: Place::Local(1),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: joined,
            from: Place::Local(1),
            to: Some(Place::Local(1)),
            to_owner: Some(adopted),
            to_ty: Some(ResolvedTy::String),
        }),
    ])[0];

    assert!(historical_relocation_duplicates_exact_handoff(
        block,
        4,
        &HashMap::new(),
    ));
}

#[test]
fn executable_gap_keeps_historical_relocation_validator_visible() {
    let historical = owner(9, 0);
    let joined = owner(9, 2);
    let adopted = owner(10, 0);
    let block = &one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: joined,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: joined,
            from: Place::Local(0),
            to: Place::Local(1),
        }),
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: historical,
            from: Place::Local(0),
            to: Place::Local(1),
        }),
        Instr::ConstI64 {
            dest: Place::Local(2),
            value: 0,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: joined,
            from: Place::Local(1),
            to: Some(Place::Local(1)),
            to_owner: Some(adopted),
            to_ty: Some(ResolvedTy::String),
        }),
    ])[0];

    assert!(!historical_relocation_duplicates_exact_handoff(
        block,
        3,
        &HashMap::new(),
    ));
}

#[test]
fn actor_state_store_commit_owns_the_terminal_transfer_program_point() {
    let current = owner(11, 0);
    let block = &one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: current,
            from: Place::Local(4),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::ActorStateFieldStore {
            field_offset: FieldOffset(3),
            src: Place::Local(4),
            handoff: crate::model::ActorStateStoreHandoff::ConsumeSource,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: current,
            from: Place::Local(4),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::NeutralizePayloadSlot {
            place: Place::Local(4),
            transferee: None,
            authority: crate::model::NeutralizeAuthority::ActorStateStoreConsume,
        },
    ])[0];

    assert!(terminal_transfer_precedes_actor_state_store_commit(
        block, 0
    ));
}

#[test]
fn actor_state_store_does_not_hide_a_distinct_terminal_transfer() {
    let premature = owner(11, 0);
    let committed = owner(12, 0);
    let block = &one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: premature,
            from: Place::Local(4),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::ActorStateFieldStore {
            field_offset: FieldOffset(3),
            src: Place::Local(4),
            handoff: crate::model::ActorStateStoreHandoff::ConsumeSource,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: committed,
            from: Place::Local(4),
            to: None,
            to_owner: None,
            to_ty: None,
        }),
        Instr::NeutralizePayloadSlot {
            place: Place::Local(4),
            transferee: None,
            authority: crate::model::NeutralizeAuthority::ActorStateStoreConsume,
        },
    ])[0];

    assert!(!terminal_transfer_precedes_actor_state_store_commit(
        block, 0
    ));
}

#[test]
fn selected_call_carrier_release_is_claimed_by_the_arm_scope_exit() {
    let carrier = owner(19, 0);
    let block = &one_block(vec![
        Instr::Drop {
            place: Place::Local(3),
            ty: ResolvedTy::Named {
                name: "Option".to_string(),
                args: vec![ResolvedTy::String],
                builtin: Some(BuiltinType::Option),
                is_opaque: false,
            },
            drop_fn: Some(crate::model::DropFnSpec::InPlace(
                crate::ownership::InPlaceReleaseKind::Enum,
            )),
        },
        Instr::OwnershipEvent(OwnershipEvent::Release {
            owner: carrier,
            place: Place::Local(3),
        }),
        Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
            scopes: vec![ScopeId(7)],
            owners: Vec::new(),
            carry_places: Vec::new(),
            carried: Vec::new(),
        }),
    ])[0];

    assert_eq!(
        adjacent_scope_release_owners(
            block,
            2,
            &HashSet::from([ScopeId(7)]),
            &HashMap::new(),
            &HashSet::from([3]),
        ),
        vec![carrier],
    );
}

#[test]
fn unrelated_release_is_not_claimed_by_an_arm_scope_exit() {
    let unrelated = owner(20, 0);
    let block = &one_block(vec![
        Instr::Drop {
            place: Place::Local(4),
            ty: ResolvedTy::String,
            drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
        },
        Instr::OwnershipEvent(OwnershipEvent::Release {
            owner: unrelated,
            place: Place::Local(4),
        }),
        Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
            scopes: vec![ScopeId(7)],
            owners: Vec::new(),
            carry_places: Vec::new(),
            carried: Vec::new(),
        }),
    ])[0];

    assert!(adjacent_scope_release_owners(
        block,
        2,
        &HashSet::from([ScopeId(7)]),
        &HashMap::new(),
        &HashSet::from([4]),
    )
    .is_empty());
}

#[allow(
    clippy::too_many_lines,
    reason = "the complete four-block join fixture makes the negative topology explicit"
)]
fn retained_string_join_fixture(owned_left_edge: bool) -> (Vec<BasicBlock>, Builder, OwnerId) {
    let source_owner = owner(30, 0);
    let destination_owner = owner(31, 0);
    let source = Place::Local(0);
    let destination = Place::Local(1);
    let left_instructions = if owned_left_edge {
        vec![
            Instr::OwnershipEvent(OwnershipEvent::Mint {
                owner: source_owner,
                place: source,
                ty: ResolvedTy::String,
            }),
            Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                scopes: vec![ScopeId(4)],
                owners: Vec::new(),
                carry_places: vec![source],
                carried: vec![source_owner],
            }),
            Instr::OwnershipEvent(OwnershipEvent::EdgeCarry {
                owner: source_owner,
                place: source,
                target: 3,
            }),
        ]
    } else {
        vec![
            Instr::StringLit {
                bytes: b"left".to_vec(),
                dest: source,
            },
            Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                scopes: vec![ScopeId(4)],
                owners: Vec::new(),
                carry_places: Vec::new(),
                carried: Vec::new(),
            }),
        ]
    };
    let blocks = vec![
        BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: Vec::new(),
            terminator: Terminator::Branch {
                cond: Place::Local(2),
                then_target: 1,
                else_target: 2,
            },
        },
        BasicBlock {
            id: 1,
            statements: Vec::new(),
            instructions: left_instructions,
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 2,
            statements: Vec::new(),
            instructions: vec![
                Instr::StringLit {
                    bytes: b"right".to_vec(),
                    dest: source,
                },
                Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
                    scopes: vec![ScopeId(5)],
                    owners: Vec::new(),
                    carry_places: Vec::new(),
                    carried: Vec::new(),
                }),
            ],
            terminator: Terminator::Goto { target: 3 },
        },
        BasicBlock {
            id: 3,
            statements: Vec::new(),
            instructions: vec![
                Instr::OwnershipEvent(OwnershipEvent::Mint {
                    owner: destination_owner,
                    place: destination,
                    ty: ResolvedTy::String,
                }),
                Instr::StringRetain {
                    value: source,
                    condition: crate::model::StringRetainCondition::Always,
                },
                Instr::Move {
                    dest: destination,
                    src: source,
                },
                Instr::NeutralizePayloadSlot {
                    place: source,
                    transferee: Some(destination),
                    authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
                },
            ],
            terminator: Terminator::Return,
        },
    ];
    let mut builder = Builder::default();
    // Mirror the three locals named by the hand-built block fixture so the
    // edge-retirement pass allocates a genuinely fresh scratch slot.
    let _ = builder.alloc_local(ResolvedTy::String);
    let _ = builder.alloc_local(ResolvedTy::String);
    let _ = builder.alloc_local(ResolvedTy::Bool);
    builder
        .binding_scope
        .insert(source_owner.binding, ScopeId(4));
    (blocks, builder, source_owner)
}

#[test]
fn retained_join_retires_the_conditionally_owned_input_on_its_edge() {
    let (mut blocks, mut builder, source_owner) = retained_string_join_fixture(true);

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert!(!blocks[3]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
    assert!(blocks[1].instructions.windows(5).any(|window| matches!(
        window,
        [
            Instr::StringRetain { value: Place::Local(0), .. },
            Instr::Move {
                dest: Place::Local(3),
                src: Place::Local(0),
            },
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner: relocated_owner,
                from: Place::Local(0),
                to: Place::Local(3),
            }),
            Instr::Drop { place: Place::Local(3), .. },
            Instr::OwnershipEvent(OwnershipEvent::Release {
                owner,
                place: Place::Local(3),
            }),
        ] if *relocated_owner == source_owner && *owner == source_owner
    )));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::Drop {
            place: Place::Local(0),
            ..
        }
    )));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::EdgeCarry { owner, .. })
            if *owner == source_owner
    )));
    assert!(blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::ScopeExit {
            owners,
            carry_places,
            carried,
            ..
        }) if owners.contains(&source_owner)
            && !carry_places.contains(&Place::Local(0))
            && !carried.contains(&source_owner)
    )));
    assert!(blocks[2]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
}

#[test]
fn retained_join_uses_exact_edge_carry_for_an_outer_scope_owner() {
    let (mut blocks, mut builder, source_owner) = retained_string_join_fixture(true);
    builder
        .binding_scope
        .insert(source_owner.binding, ScopeId(99));

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert!(!blocks[3]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
    assert!(blocks[1].instructions.windows(5).any(|window| matches!(
        window,
        [
            Instr::StringRetain { value: Place::Local(0), .. },
            Instr::Move { src: Place::Local(0), .. },
            Instr::OwnershipEvent(OwnershipEvent::Relocate { owner, .. }),
            Instr::Drop { .. },
            Instr::OwnershipEvent(OwnershipEvent::Release { owner: released, .. }),
        ] if *owner == source_owner && *released == source_owner
    )));
    assert!(blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::ScopeExit { owners, .. })
            if !owners.contains(&source_owner)
    )));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::EdgeCarry { owner, .. })
            if *owner == source_owner
    )));
}

#[test]
fn all_borrowed_join_inputs_do_not_trigger_edge_local_release() {
    let (mut blocks, mut builder, _) = retained_string_join_fixture(false);

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert_eq!(builder.locals.len(), 3, "no retirement scratch is needed");
    assert!(blocks[3]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
    assert!(!blocks[..3].iter().any(|block| block
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::Drop { .. }))));
}

#[test]
fn ambiguous_retained_join_topology_remains_unchanged() {
    let (mut blocks, mut builder, _) = retained_string_join_fixture(true);
    blocks[1].terminator = Terminator::Branch {
        cond: Place::Local(2),
        then_target: 3,
        else_target: 4,
    };
    blocks.push(BasicBlock {
        id: 4,
        statements: Vec::new(),
        instructions: Vec::new(),
        terminator: Terminator::Trap {
            kind: crate::model::TrapKind::ExhaustivenessFallthrough,
        },
    });

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert_eq!(builder.locals.len(), 3, "ambiguous edges must fail closed");
    assert!(blocks[3]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { .. })
    )));
}

#[test]
fn multiply_owned_retained_join_source_remains_unchanged() {
    let (mut blocks, mut builder, _) = retained_string_join_fixture(true);
    blocks[1].instructions.insert(
        1,
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: owner(32, 0),
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
    );

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert_eq!(builder.locals.len(), 3, "multiple owners must fail closed");
    assert!(blocks[3]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::StringRetain { .. })));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { .. })
    )));
}

#[test]
fn non_string_retained_join_spine_remains_unchanged() {
    let (mut blocks, mut builder, _) = retained_string_join_fixture(true);
    blocks[3].instructions[1] = Instr::BytesRetain {
        value: Place::Local(0),
    };

    materialize_edge_local_retained_join_releases(&mut blocks, &mut builder);

    assert_eq!(builder.locals.len(), 3, "non-string joins are out of scope");
    assert!(matches!(
        blocks[3].instructions[1],
        Instr::BytesRetain {
            value: Place::Local(0)
        }
    ));
    assert!(!blocks[1].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { .. })
    )));
}

#[test]
fn retained_string_copy_mints_destination_without_ending_source() {
    let source_owner = owner(41, 0);
    let destination_owner = owner(42, 0);
    let mut blocks = one_block(vec![
        Instr::StringRetain {
            value: Place::Local(0),
            condition: crate::model::StringRetainCondition::Always,
        },
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: source_owner,
            from: Place::Local(0),
            to: Some(Place::Local(1)),
            to_owner: Some(destination_owner),
            to_ty: Some(ResolvedTy::String),
        }),
    ]);

    canonicalize_retained_copy_owner_transfers(&mut blocks, &HashSet::new());

    assert!(matches!(
        blocks[0].instructions[2],
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner,
            place: Place::Local(1),
            ty: ResolvedTy::String,
        }) if owner == destination_owner
    ));
}

#[test]
fn guarded_actor_record_share_keeps_source_and_record_owners() {
    let source_owner = owner(43, 0);
    let record_owner = owner(44, 0);
    let source = Place::Local(0);
    let record = Place::Local(1);
    let record_ty = ResolvedTy::named_user("Wrap", vec![]);
    let mut blocks = one_block(vec![
        Instr::StringRetain {
            value: source,
            condition: crate::model::StringRetainCondition::Always,
        },
        Instr::RecordInit {
            ty: record_ty.clone(),
            fields: vec![(FieldOffset(0), source)],
            dest: record,
        },
        Instr::NeutralizePayloadSlot {
            place: source,
            transferee: Some(record),
            authority: crate::model::NeutralizeAuthority::AggregateMemberConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: source_owner,
            from: source,
            to: Some(record),
            to_owner: None,
            to_ty: None,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: record_owner,
            place: record,
            ty: record_ty,
        }),
    ]);

    canonicalize_retained_copy_owner_transfers(&mut blocks, &HashSet::from([source]));

    assert_eq!(blocks[0].instructions.len(), 3);
    assert!(matches!(
        blocks[0].instructions[2],
        Instr::OwnershipEvent(OwnershipEvent::Mint { owner, place, .. })
            if owner == record_owner && place == record
    ));
    assert!(!blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::NeutralizePayloadSlot { .. }
            | Instr::OwnershipEvent(OwnershipEvent::Transfer { .. })
    )));
}

#[test]
fn guarded_actor_record_share_requires_the_exact_record_mint() {
    let source = Place::Local(0);
    let record = Place::Local(1);
    let mut blocks = one_block(vec![
        Instr::StringRetain {
            value: source,
            condition: crate::model::StringRetainCondition::Always,
        },
        Instr::RecordInit {
            ty: ResolvedTy::named_user("Wrap", vec![]),
            fields: vec![(FieldOffset(0), source)],
            dest: record,
        },
        Instr::NeutralizePayloadSlot {
            place: source,
            transferee: Some(record),
            authority: crate::model::NeutralizeAuthority::AggregateMemberConsume,
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: owner(45, 0),
            from: source,
            to: Some(record),
            to_owner: None,
            to_ty: None,
        }),
    ]);

    canonicalize_retained_copy_owner_transfers(&mut blocks, &HashSet::from([source]));

    assert!(blocks[0]
        .instructions
        .iter()
        .any(|instruction| matches!(instruction, Instr::NeutralizePayloadSlot { .. })));
    assert!(blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Transfer { .. })
    )));
}

#[test]
fn fresh_share_move_keeps_both_owner_places() {
    let source_owner = owner(51, 0);
    let destination_owner = owner(52, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: source_owner,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: destination_owner,
            place: Place::Local(1),
            ty: ResolvedTy::String,
        }),
        Instr::StringRetain {
            value: Place::Local(0),
            condition: crate::model::StringRetainCondition::FreshShare,
        },
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
    ]);
    let mut builder = Builder::default();

    materialize_explicit_move_relocations(&mut blocks, &mut builder);

    assert!(!blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::Local(1)),
            ..
        } | Instr::OwnershipEvent(OwnershipEvent::Relocate {
            from: Place::Local(0),
            to: Place::Local(1),
            ..
        })
    )));
    let exact_states = drop_plan::exact_owner_states(&blocks);
    let exits = &exact_states.1;
    let exit = &exits[&0];
    assert_eq!(exit.get(&source_owner), Some(&Place::Local(0)));
    assert_eq!(exit.get(&destination_owner), Some(&Place::Local(1)));
}

#[test]
fn bare_move_still_materializes_source_relocation() {
    let source_owner = owner(61, 0);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: source_owner,
            place: Place::Local(0),
            ty: ResolvedTy::String,
        }),
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
    ]);
    let mut builder = Builder::default();

    materialize_explicit_move_relocations(&mut blocks, &mut builder);

    assert!(blocks[0].instructions.windows(3).any(|window| matches!(
        window,
        [
            Instr::Move {
                dest: Place::Local(1),
                src: Place::Local(0),
            },
            Instr::NeutralizePayloadSlot {
                place: Place::Local(0),
                transferee: Some(Place::Local(1)),
                ..
            },
            Instr::OwnershipEvent(OwnershipEvent::Relocate {
                owner,
                from: Place::Local(0),
                to: Place::Local(1),
            }),
        ] if *owner == source_owner
    )));
}

#[test]
fn typed_nonowning_vec_iter_copy_keeps_source_owner_place() {
    let source_owner = owner(62, 0);
    let source = Place::Local(0);
    let destination = Place::Local(1);
    let mut blocks = one_block(vec![
        Instr::OwnershipEvent(OwnershipEvent::Mint {
            owner: source_owner,
            place: source,
            ty: ResolvedTy::named_builtin(
                "VecIter",
                hew_types::BuiltinType::VecIter,
                vec![ResolvedTy::I64],
            ),
        }),
        Instr::Move {
            dest: destination,
            src: source,
        },
    ]);
    let mut builder = Builder::default();
    builder
        .nonowning_vec_iter_copy_moves
        .insert((0, source, destination));

    materialize_explicit_move_relocations(&mut blocks, &mut builder);

    assert!(!blocks[0].instructions.iter().any(|instruction| matches!(
        instruction,
        Instr::NeutralizePayloadSlot {
            place,
            transferee: Some(to),
            ..
        } if *place == source && *to == destination
    ) || matches!(
        instruction,
        Instr::OwnershipEvent(OwnershipEvent::Relocate { from, to, .. })
            if *from == source && *to == destination
    )));
    let exact_states = drop_plan::exact_owner_states(&blocks);
    let exits = &exact_states.1;
    assert_eq!(exits[&0].get(&source_owner), Some(&source));
}

#[test]
fn unretained_handoff_remains_a_transfer() {
    let source_owner = owner(41, 0);
    let destination_owner = owner(42, 0);
    let mut blocks = one_block(vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: source_owner,
            from: Place::Local(0),
            to: Some(Place::Local(1)),
            to_owner: Some(destination_owner),
            to_ty: Some(ResolvedTy::String),
        }),
    ]);

    canonicalize_retained_copy_owner_transfers(&mut blocks, &HashSet::new());

    assert!(matches!(
        blocks[0].instructions[1],
        Instr::OwnershipEvent(OwnershipEvent::Transfer { owner, .. })
            if owner == source_owner
    ));
}

#[test]
fn typed_bytes_handoff_with_later_source_read_is_a_retained_share() {
    let source_owner = owner(71, 0);
    let destination_owner = owner(72, 0);
    let blocks = one_block(vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: source_owner,
            from: Place::Local(0),
            to: Some(Place::Local(1)),
            to_owner: Some(destination_owner),
            to_ty: Some(ResolvedTy::Bytes),
        }),
        Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::Local(1)),
            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
        },
        Instr::Move {
            dest: Place::ReturnSlot,
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Relocate {
            owner: source_owner,
            from: Place::Local(0),
            to: Place::ReturnSlot,
        }),
    ]);

    assert_eq!(
        prove_retained_bytes_local_share(&blocks, &HashMap::new(), 0, 0),
        Some(RetainedBytesLocalShareProof {
            source: Place::Local(0),
            destination: Place::Local(1),
            neutralize_index: 2,
        })
    );
}

#[test]
fn ambiguous_bytes_handoff_neutralization_fails_closed() {
    let source_owner = owner(81, 0);
    let destination_owner = owner(82, 0);
    let mut instructions = vec![
        Instr::Move {
            dest: Place::Local(1),
            src: Place::Local(0),
        },
        Instr::OwnershipEvent(OwnershipEvent::Transfer {
            owner: source_owner,
            from: Place::Local(0),
            to: Some(Place::Local(1)),
            to_owner: Some(destination_owner),
            to_ty: Some(ResolvedTy::Bytes),
        }),
    ];
    for _ in 0..2 {
        instructions.push(Instr::NeutralizePayloadSlot {
            place: Place::Local(0),
            transferee: Some(Place::Local(1)),
            authority: crate::model::NeutralizeAuthority::WholeCarrierConsume,
        });
    }
    instructions.push(Instr::Move {
        dest: Place::ReturnSlot,
        src: Place::Local(0),
    });
    let blocks = one_block(instructions);

    assert_eq!(
        prove_retained_bytes_local_share(&blocks, &HashMap::new(), 0, 0),
        None,
        "multiple source neutralizations must never authorize a retained share"
    );
}
