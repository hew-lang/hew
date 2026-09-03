//! Pins for the enum composite prover's `FieldDropInPlace` handling: the
//! blanket-scan exemption (the op is an interior discharge, not a payload
//! READ into an owning sink) paired with the DIRECT exclusion rule (a
//! base that is an alias member or a payload binder frees payload leaves
//! through a byte-alias of the composite's storage, so the composite must
//! be excluded — its `EnumInPlace` walk would re-free them; the
//! empirically reproduced two-step nested destructure `match opt {
//! .Some(row) => match row { Row { a, b: _ } => … } }` aborted under
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

fn bytes_box_ty() -> ResolvedTy {
    ResolvedTy::named_user("BytesBox", vec![])
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
