use hew_hir::ItemId;
use hew_parser::ast::BinaryOp;
use hew_sir::{
    dump_sir, verify_module, BlockArg, BlockId, Edge, EffectSet, FunctionSourceOrigin, OpId,
    Operand, Provenance, SemBlock, SemFunction, SemModule, SemOp, SemOpKind, SemTerminator,
    SirDiagnosticKind, UseMode, ValueDef, ValueId,
};
use hew_types::{CallTarget, DefId, ResolvedTy};

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
        declaration: DefId::for_test("f"),
        name: "f".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
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

#[test]
fn verifier_rejects_entry_block_arguments() {
    let module = SemModule {
        functions: vec![SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("bad_entry_args"),
            name: "bad_entry_args".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: Vec::new(),
            return_ty: ResolvedTy::I64,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: vec![BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::I64,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            }],
        }],
    };

    assert!(verify_module(&module).iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::EntryBlockArgs {
            entry: BlockId(0),
            actual: 1,
        }
    )));
}

#[test]
fn operation_effects_are_derived_and_conservative() {
    let checked_add = SemOpKind::Binary {
        op: BinaryOp::Add,
        lhs: Operand {
            value: ValueId(0),
            mode: UseMode::Read,
        },
        rhs: Operand {
            value: ValueId(1),
            mode: UseMode::Read,
        },
    };
    assert_eq!(checked_add.effects(), EffectSet::MAY_TRAP);
    assert!(checked_add.effects().may_trap());

    let wrapping_add = SemOpKind::Binary {
        op: BinaryOp::WrappingAdd,
        lhs: Operand {
            value: ValueId(0),
            mode: UseMode::Read,
        },
        rhs: Operand {
            value: ValueId(1),
            mode: UseMode::Read,
        },
    };
    assert!(wrapping_add.effects().is_pure());

    let unresolved_call = SemOpKind::Call {
        target: CallTarget::Builtin {
            endpoint: "test".to_string(),
        },
        args: Vec::new(),
    };
    assert_eq!(unresolved_call.effects(), EffectSet::UNKNOWN_CALL);
    assert!(unresolved_call.effects().contains(EffectSet::MAY_TRAP));
    assert!(unresolved_call.effects().may_trap());
}

#[test]
fn verifier_rejects_eager_logical_ops_and_ownership_modes_without_an_owner() {
    let module = SemModule {
        functions: vec![SemFunction {
            id: ItemId(0),
            declaration: DefId::for_test("bad_logical"),
            name: "bad_logical".to_string(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::Bool,
            }],
            return_ty: ResolvedTy::Bool,
            entry: BlockId(0),
            blocks: vec![SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::Bool,
                    }],
                    kind: SemOpKind::Binary {
                        op: BinaryOp::And,
                        lhs: Operand {
                            value: ValueId(0),
                            mode: UseMode::BorrowShared,
                        },
                        rhs: Operand {
                            value: ValueId(0),
                            mode: UseMode::Read,
                        },
                    },
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(1)),
                },
            }],
        }],
    };

    let diagnostics = verify_module(&module);
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("only Read is legal")
    )));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("must be represented as SIR branch CFG")
    )));
}

#[test]
fn verifier_rejects_duplicate_semantic_and_emitted_function_identities() {
    let function = SemFunction {
        id: ItemId(0),
        declaration: DefId::for_test("duplicate"),
        name: "duplicate".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return { value: None },
        }],
    };
    let diagnostics = verify_module(&SemModule {
        functions: vec![function.clone(), function],
    });
    assert!(diagnostics
        .iter()
        .any(|diagnostic| matches!(diagnostic.kind, SirDiagnosticKind::DuplicateFunctionName(_))));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::DuplicateFunctionDeclaration(_)
    )));
}
