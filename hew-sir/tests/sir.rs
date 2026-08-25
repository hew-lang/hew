use hew_hir::ItemId;
use hew_parser::ast::BinaryOp;
use hew_sir::{
    dump_sir, verify_module, BlockArg, BlockId, Edge, OpId, Operand, Provenance, SemBlock,
    SemFunction, SemModule, SemOp, SemOpKind, SemTerminator, UseMode, ValueDef, ValueId,
};
use hew_types::ResolvedTy;

fn definition(id: u32) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty: ResolvedTy::I64,
    }
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "one complete SSA diamond fixture is easier to audit as a single IR graph"
)]
fn block_arguments_are_ssa_join_values() {
    let function = SemFunction {
        id: ItemId(0),
        name: "f".to_string(),
        params: vec![
            BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
            },
        ],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![
                    SemOp {
                        id: OpId(0),
                        results: vec![definition(2)],
                        kind: SemOpKind::ConstI64(0),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: OpId(1),
                        results: vec![ValueDef {
                            id: ValueId(3),
                            ty: ResolvedTy::Bool,
                        }],
                        kind: SemOpKind::Binary {
                            op: BinaryOp::Greater,
                            lhs: Operand {
                                value: ValueId(0),
                                mode: UseMode::Read,
                            },
                            rhs: Operand {
                                value: ValueId(2),
                                mode: UseMode::Read,
                            },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Branch {
                    condition: ValueId(3),
                    then_target: Edge {
                        target: BlockId(1),
                        args: vec![ValueId(1)],
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: vec![ValueId(1)],
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(4),
                    ty: ResolvedTy::I64,
                }],
                ops: vec![
                    SemOp {
                        id: OpId(2),
                        results: vec![definition(5)],
                        kind: SemOpKind::ConstI64(1),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: OpId(3),
                        results: vec![definition(6)],
                        kind: SemOpKind::Binary {
                            op: BinaryOp::Add,
                            lhs: Operand {
                                value: ValueId(4),
                                mode: UseMode::Read,
                            },
                            rhs: Operand {
                                value: ValueId(5),
                                mode: UseMode::Read,
                            },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![ValueId(6)],
                }),
            },
            SemBlock {
                id: BlockId(2),
                args: vec![BlockArg {
                    value: ValueId(7),
                    ty: ResolvedTy::I64,
                }],
                ops: vec![
                    SemOp {
                        id: OpId(4),
                        results: vec![definition(8)],
                        kind: SemOpKind::ConstI64(2),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: OpId(5),
                        results: vec![definition(9)],
                        kind: SemOpKind::Binary {
                            op: BinaryOp::Add,
                            lhs: Operand {
                                value: ValueId(7),
                                mode: UseMode::Read,
                            },
                            rhs: Operand {
                                value: ValueId(8),
                                mode: UseMode::Read,
                            },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![ValueId(9)],
                }),
            },
            SemBlock {
                id: BlockId(3),
                args: vec![BlockArg {
                    value: ValueId(10),
                    ty: ResolvedTy::I64,
                }],
                ops: vec![
                    SemOp {
                        id: OpId(6),
                        results: vec![definition(11)],
                        kind: SemOpKind::ConstI64(3),
                        provenance: Provenance::Synthesized,
                    },
                    SemOp {
                        id: OpId(7),
                        results: vec![definition(12)],
                        kind: SemOpKind::Binary {
                            op: BinaryOp::Multiply,
                            lhs: Operand {
                                value: ValueId(10),
                                mode: UseMode::Read,
                            },
                            rhs: Operand {
                                value: ValueId(11),
                                mode: UseMode::Read,
                            },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(12)),
                },
            },
        ],
    };
    let module = SemModule {
        functions: vec![function],
    };
    assert!(
        verify_module(&module).is_empty(),
        "SIR must verify: {:#?}",
        verify_module(&module)
    );
    let dump = dump_sir(&module);
    assert!(dump.contains("bb3(%10: i64):"));
    assert!(dump.contains("goto bb3(%6)"));
}
