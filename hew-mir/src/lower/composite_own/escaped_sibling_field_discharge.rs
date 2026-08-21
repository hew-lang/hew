//! Structural tests for `apply_escaped_record_sibling_field_drops` — the
//! #2212 sibling-discharge emitter. The positive shape (one attributed
//! instruction escape, record untouched afterwards) splices one
//! `FieldDropInPlace` per dischargeable owned sibling right after the
//! escape; every fail-closed refusal condition must leave the blocks
//! untouched (leak-as-before, never a double-free).
use super::*;

fn rec_ty() -> ResolvedTy {
    ResolvedTy::named_user("Rec", vec![])
}

fn outer_ty() -> ResolvedTy {
    ResolvedTy::named_user("Outer", vec![])
}

fn mid_ty() -> ResolvedTy {
    ResolvedTy::named_user("Mid", vec![])
}

fn leaf_ty() -> ResolvedTy {
    ResolvedTy::named_user("Leaf", vec![])
}

fn is_rec(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { name, .. } if name == "Rec")
}

fn never_owned_record(_: &ResolvedTy) -> bool {
    false
}

fn is_chain_rec(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::Named { name, .. } if matches!(name.as_str(), "Outer" | "Mid" | "Leaf"))
}

/// `Rec { inner: string, tag: string }` — field 0 escapes in the test
/// shapes, field 1 is the dischargeable sibling.
fn owned_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
    if is_rec(ty) {
        vec![(0, ResolvedTy::String), (1, ResolvedTy::String)]
    } else {
        Vec::new()
    }
}

fn chain_owned_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
    let ResolvedTy::Named { name, .. } = ty else {
        return Vec::new();
    };
    match name.as_str() {
        "Outer" => vec![(0, mid_ty()), (1, ResolvedTy::String)],
        "Mid" => vec![(0, leaf_ty()), (1, ResolvedTy::String)],
        "Leaf" => vec![(0, ResolvedTy::String), (1, ResolvedTy::String)],
        _ => Vec::new(),
    }
}

fn dischargeable(ty: &ResolvedTy) -> bool {
    matches!(ty, ResolvedTy::String)
}

/// Tuple owned-element list: every `string` element of a tuple node.
fn owned_tuple_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
    let ResolvedTy::Tuple(items) = ty else {
        return Vec::new();
    };
    items
        .iter()
        .enumerate()
        .filter(|(_, item)| matches!(item, ResolvedTy::String))
        .filter_map(|(idx, item)| u32::try_from(idx).ok().map(|i| (i, item.clone())))
        .collect()
}

fn field_orders() -> HashMap<String, Vec<(String, ResolvedTy)>> {
    let mut orders = HashMap::new();
    orders.insert(
        "Rec".to_string(),
        vec![
            ("inner".to_string(), ResolvedTy::String),
            ("tag".to_string(), ResolvedTy::String),
        ],
    );
    orders.insert(
        "Leaf".to_string(),
        vec![
            ("s".to_string(), ResolvedTy::String),
            ("t".to_string(), ResolvedTy::String),
        ],
    );
    orders.insert(
        "Mid".to_string(),
        vec![
            ("leaf".to_string(), leaf_ty()),
            ("x".to_string(), ResolvedTy::String),
        ],
    );
    orders.insert(
        "Outer".to_string(),
        vec![
            ("mid".to_string(), mid_ty()),
            ("c".to_string(), ResolvedTy::String),
        ],
    );
    orders
}

fn block(id: u32, instructions: Vec<Instr>, terminator: Terminator) -> BasicBlock {
    BasicBlock {
        id,
        statements: vec![],
        instructions,
        terminator,
    }
}

fn apply(
    blocks: &mut [BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
) {
    apply_with(
        blocks,
        owned,
        binding_locals,
        local_tys,
        &[],
        &is_rec,
        &owned_fields,
    );
}

fn apply_with(
    blocks: &mut [BasicBlock],
    owned: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    local_tys: &[ResolvedTy],
    alias_chain: &[(u32, u32, u32)],
    is_owned_record: &dyn Fn(&ResolvedTy) -> bool,
    owned_field_list: &dyn Fn(&ResolvedTy) -> Vec<(u32, ResolvedTy)>,
) {
    let mut instr_spans = BTreeMap::new();
    apply_escaped_record_sibling_field_drops(
        blocks,
        &HashMap::new(),
        owned,
        binding_locals,
        local_tys,
        &field_orders(),
        &[],
        &hew_hir::LifecycleRegistry::default(),
        alias_chain,
        &HashSet::new(),
        &HashSet::new(),
        is_owned_record,
        owned_field_list,
        &owned_tuple_fields,
        &dischargeable,
        &mut instr_spans,
    );
}

fn match_hop_record_blocks(terminator: Terminator) -> Vec<BasicBlock> {
    vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(2),
                field_offset: FieldOffset(0),
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(4),
            },
        ],
        terminator,
    )]
}

fn apply_match_hop(blocks: &mut [BasicBlock], local_tys: &[ResolvedTy]) {
    let root = BindingId(1);
    let owned = vec![(root, "o".to_string(), outer_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(root, Place::Local(0))].into_iter().collect();
    let alias_chain = vec![(1, 0, 0)];
    apply_with(
        blocks,
        &owned,
        &binding_locals,
        local_tys,
        &alias_chain,
        &is_chain_rec,
        &chain_owned_fields,
    );
}

/// The #2212 shape: one field loaded out and returned, record untouched
/// afterwards → the owned sibling gets its in-place discharge spliced
/// directly after the escape instruction.
#[test]
fn single_attributed_escape_discharges_owned_sibling() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert_eq!(
        blocks[0].instructions.len(),
        3,
        "exactly one sibling discharge must be spliced; got {:?}",
        blocks[0].instructions
    );
    assert_eq!(
        blocks[0].instructions[2],
        Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        },
        "the discharge must address the NON-escaped sibling (field 1) on \
         the root local, typed at the field"
    );
}

#[test]
fn consuming_call_discharges_mixed_record_sibling_in_unique_continuation() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let vec_string = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]);
    let local_tys = vec![rec_ty(), vec_string, ResolvedTy::String, ResolvedTy::String];
    let mut blocks = vec![
        block(
            0,
            vec![
                Instr::RecordFieldStore {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    src: Place::Local(2),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(1),
                    dest: Place::Local(3),
                },
                Instr::Drop {
                    place: Place::Local(3),
                    ty: ResolvedTy::String,
                    drop_fn: Some(crate::model::DropFnSpec::Release("hew_string_drop")),
                },
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
            ],
            Terminator::Call {
                callee: "consume".to_string(),
                authority: crate::model::CallAuthority::default(),
                args: vec![Place::Local(1)],
                dest: None,
                next: 1,
            },
        ),
        block(1, vec![], Terminator::Return),
    ];

    apply_with(
        &mut blocks,
        &owned,
        &binding_locals,
        &local_tys,
        &[],
        &never_owned_record,
        &owned_fields,
    );
    assert_eq!(
        blocks[1].instructions,
        vec![Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        }],
        "the surviving sibling must drop only after the consuming call returns"
    );
}

#[test]
fn consuming_call_with_a_shared_continuation_refuses_discharge() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![
        block(
            0,
            vec![Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            }],
            Terminator::Call {
                callee: "consume".to_string(),
                authority: crate::model::CallAuthority::default(),
                args: vec![Place::Local(1)],
                dest: None,
                next: 2,
            },
        ),
        block(1, vec![], Terminator::Goto { target: 2 }),
        block(2, vec![], Terminator::Return),
    ];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        blocks.iter().all(|block| block
            .instructions
            .iter()
            .all(|instr| !matches!(instr, Instr::FieldDropInPlace { .. }))),
        "a shared continuation cannot prove that the consuming call ran"
    );
}

/// The escaped field itself must never be discharged: when field 1 is the
/// escapee, the spliced set contains ONLY field 0 — a discharge of the
/// escaped slot would free the buffer the escapee now owns.
#[test]
fn escaped_field_is_never_in_the_discharge_set() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(1),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    let ops: Vec<_> = blocks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
        .collect();
    assert_eq!(
        ops,
        vec![&Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Record(FieldOffset(0)),
            ty: ResolvedTy::String,
        }],
        "only the non-escaped sibling (field 0) may be discharged — a \
         discharge of escaped field 1 double-frees the escapee"
    );
}

/// A read of the record after the escape (a later field load) refuses the
/// discharge — freeing the sibling earlier would be a use-after-free at
/// that read.
#[test]
fn record_read_after_escape_refuses_discharge() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String, ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(1),
                dest: Place::Local(2),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a record field read after the escape must refuse the discharge; \
         got {:?}",
        blocks[0].instructions
    );
}

/// A second escape event refuses the discharge — a per-escape splice
/// would run twice on one path.
#[test]
fn two_escape_events_refuse_discharge() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String, ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(1),
                dest: Place::Local(2),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(2),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "two escape events on one root must refuse the discharge; got {:?}",
        blocks[0].instructions
    );
}

/// An escape inside a loop (its block reachable from itself) refuses the
/// discharge — the splice would re-run per iteration.
#[test]
fn escape_in_cycle_refuses_discharge() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Goto { target: 0 },
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "an escape whose block is self-reachable must refuse the \
         discharge; got {:?}",
        blocks[0].instructions
    );
}

/// A binder that is itself an `owned_locals` base (`let g = r.field`) has
/// its own release path; the discharge must refuse rather than race it.
#[test]
fn extracted_owned_binding_refuses_discharge() {
    let root = BindingId(1);
    let extracted = BindingId(2);
    let owned = vec![
        (root, "r".to_string(), rec_ty()),
        (extracted, "g".to_string(), ResolvedTy::String),
    ];
    let binding_locals: HashMap<BindingId, Place> =
        [(root, Place::Local(0)), (extracted, Place::Local(1))]
            .into_iter()
            .collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a binder that is an owned binding's base has its own release \
         path; the discharge must refuse; got {:?}",
        blocks[0].instructions
    );
}

/// A whole-value copy of the record (`let b2 = b`) refuses the discharge
/// — the copies byte-share field pointers and the pass frees through the
/// root slot only.
#[test]
fn whole_value_alias_copy_refuses_discharge() {
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String, rec_ty()];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(0),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(2),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];

    apply(&mut blocks, &owned, &binding_locals, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a whole-value alias copy of the record must refuse the \
         discharge; got {:?}",
        blocks[0].instructions
    );
}

/// An owned sibling whose shape the field-drop contract does not cover
/// keeps its leak while the coverable sibling is still discharged —
/// partial discharge is strictly better and never double-frees.
#[test]
fn uncoverable_sibling_keeps_leak_while_coverable_discharges() {
    fn three_fields(ty: &ResolvedTy) -> Vec<(u32, ResolvedTy)> {
        if is_rec(ty) {
            vec![
                (0, ResolvedTy::String),
                (1, ResolvedTy::String),
                (
                    2,
                    ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]),
                ),
            ]
        } else {
            Vec::new()
        }
    }
    let b = BindingId(1);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals: HashMap<BindingId, Place> = [(b, Place::Local(0))].into_iter().collect();
    let local_tys = vec![rec_ty(), ResolvedTy::String];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::RecordFieldLoad {
                record: Place::Local(0),
                field_offset: FieldOffset(0),
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        Terminator::Return,
    )];

    let mut instr_spans = BTreeMap::new();
    apply_escaped_record_sibling_field_drops(
        &mut blocks,
        &HashMap::new(),
        &owned,
        &binding_locals,
        &local_tys,
        &field_orders(),
        &[],
        &hew_hir::LifecycleRegistry::default(),
        &[],
        &HashSet::new(),
        &HashSet::new(),
        &is_rec,
        &three_fields,
        &owned_tuple_fields,
        &dischargeable,
        &mut instr_spans,
    );
    let ops: Vec<_> = blocks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
        .collect();
    assert_eq!(
        ops,
        vec![&Instr::FieldDropInPlace {
            base: Place::Local(0),
            field: crate::model::FieldAddr::Record(FieldOffset(1)),
            ty: ResolvedTy::String,
        }],
        "the string sibling is discharged; the uncovered Vec sibling \
         keeps its leak (fail-closed partial discharge)"
    );
}

/// #2387: an intermediate chain hop bound through `match` preserves the
/// immediate parent relation (`leaf -> match-scrutinee-mid -> outer`) so
/// the escaped-chain compensator releases only the non-escaped siblings at
/// both levels.
#[test]
fn match_bound_hop_chain_discharges_record_siblings() {
    let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), leaf_ty(), leaf_ty()];
    let mut blocks = match_hop_record_blocks(Terminator::Return);

    apply_match_hop(&mut blocks, &local_tys);
    let ops: Vec<_> = blocks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i, Instr::FieldDropInPlace { .. }))
        .collect();
    assert_eq!(
        ops,
        vec![
            &Instr::FieldDropInPlace {
                base: Place::Local(2),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            },
            &Instr::FieldDropInPlace {
                base: Place::Local(0),
                field: crate::model::FieldAddr::Record(FieldOffset(1)),
                ty: ResolvedTy::String,
            },
        ],
        "the match-bound escape must discharge mid.x through the scrutinee \
         alias and outer.c through the root, never the escaped leaf"
    );
}

/// Non-escaping match-bound destructures keep the leak-safety posture:
/// sibling discharges are only compensation for a proven owning escape.
#[test]
fn non_escaping_match_bound_hop_emits_no_sibling_discharge() {
    let local_tys = vec![
        outer_ty(),
        mid_ty(),
        mid_ty(),
        leaf_ty(),
        ResolvedTy::String,
    ];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(2),
                field_offset: FieldOffset(0),
                dest: Place::Local(3),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(3),
                field_offset: FieldOffset(0),
                dest: Place::Local(4),
            },
        ],
        Terminator::Return,
    )];

    apply_match_hop(&mut blocks, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a match-bound hop that never escapes must not add sibling drops; \
         got {:?}",
        blocks[0].instructions
    );
}

/// A read of any chain node after the escape refuses the discharge: freeing
/// a sibling before that read would be a use-after-free through the alias.
#[test]
fn match_bound_hop_post_escape_read_refuses_discharge() {
    let local_tys = vec![
        outer_ty(),
        mid_ty(),
        mid_ty(),
        leaf_ty(),
        leaf_ty(),
        ResolvedTy::String,
    ];
    let mut blocks = match_hop_record_blocks(Terminator::Return);
    blocks[0].instructions.push(Instr::RecordFieldLoad {
        record: Place::Local(2),
        field_offset: FieldOffset(1),
        dest: Place::Local(5),
    });

    apply_match_hop(&mut blocks, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a chain-node read after the escape must keep the fail-closed leak; \
         got {:?}",
        blocks[0].instructions
    );
}

/// Retained string fields are not byte-copy aggregate aliases. Returning a
/// string loaded through the match-bound path must not be attributed as an
/// escaped aggregate chain.
#[test]
fn match_bound_retained_string_load_is_not_attributed() {
    let local_tys = vec![
        outer_ty(),
        mid_ty(),
        mid_ty(),
        ResolvedTy::String,
        ResolvedTy::String,
    ];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(2),
                field_offset: FieldOffset(1),
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(4),
            },
        ],
        Terminator::Return,
    )];

    apply_match_hop(&mut blocks, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a retained string field load is a fresh owner, not a byte-copy \
         aggregate alias; got {:?}",
        blocks[0].instructions
    );
}

#[test]
fn retained_string_tuple_chain_is_not_discharged_as_a_transfer() {
    let b = BindingId(1);
    let tuple_ty = ResolvedTy::Tuple(vec![ResolvedTy::String, ResolvedTy::I64]);
    let owned = vec![(b, "r".to_string(), rec_ty())];
    let binding_locals = HashMap::from([(b, Place::Local(0))]);
    let local_tys = vec![rec_ty(), tuple_ty, ResolvedTy::String];
    let mut blocks = vec![
        block(0, vec![], Terminator::Goto { target: 1 }),
        block(
            1,
            vec![
                Instr::RecordFieldLoad {
                    record: Place::Local(0),
                    field_offset: FieldOffset(0),
                    dest: Place::Local(1),
                },
                Instr::TupleFieldLoad {
                    tuple: Place::Local(1),
                    field_index: 0,
                    dest: Place::Local(2),
                },
                Instr::Move {
                    dest: Place::ReturnSlot,
                    src: Place::Local(2),
                },
            ],
            Terminator::Return,
        ),
    ];

    apply_with(
        &mut blocks,
        &owned,
        &binding_locals,
        &local_tys,
        &[(1, 0, 0), (2, 1, 0)],
        &is_rec,
        &owned_fields,
    );
    assert!(
        !blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .any(|instr| matches!(instr, Instr::FieldDropInPlace { .. })),
        "a retained tuple string is an independent share, so the record keeps its full drop"
    );
}

/// Handle-transfer fields are also not byte-copy aggregate aliases. The
/// helper must keep `local_is_byte_copy_aggregate` as the gate and leave the
/// pre-existing fail-closed posture unchanged.
#[test]
fn match_bound_handle_transfer_load_is_not_attributed() {
    let vec_ty = ResolvedTy::named_builtin("Vec", BuiltinType::Vec, vec![ResolvedTy::String]);
    let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), vec_ty.clone(), vec_ty];
    let mut blocks = vec![block(
        0,
        vec![
            Instr::Move {
                dest: Place::Local(2),
                src: Place::Local(1),
            },
            Instr::RecordFieldLoad {
                record: Place::Local(2),
                field_offset: FieldOffset(0),
                dest: Place::Local(3),
            },
            Instr::Move {
                dest: Place::Local(4),
                src: Place::Local(3),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(4),
            },
        ],
        Terminator::Return,
    )];

    apply_match_hop(&mut blocks, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a transferred handle field must not be treated as a match-hop \
         byte-copy aggregate alias; got {:?}",
        blocks[0].instructions
    );
}

/// The self-reachable-block bail-out remains load-bearing for match-hop
/// chains: inline-composite sibling drops are not idempotent in loops.
#[test]
fn match_bound_hop_escape_in_cycle_refuses_discharge() {
    let local_tys = vec![outer_ty(), mid_ty(), mid_ty(), leaf_ty(), leaf_ty()];
    let mut blocks = match_hop_record_blocks(Terminator::Goto { target: 0 });

    apply_match_hop(&mut blocks, &local_tys);
    assert!(
        !blocks[0]
            .instructions
            .iter()
            .any(|i| matches!(i, Instr::FieldDropInPlace { .. })),
        "a match-hop escape whose block is self-reachable must refuse the \
         discharge; got {:?}",
        blocks[0].instructions
    );
}
