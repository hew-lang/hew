use hew_hir::ItemId;
use hew_parser::ast::BinaryOp;
use hew_sir::{
    dump_sir, verify_function_in_module, verify_module, BlockArg, BlockId, CallableId, Edge,
    EffectSet, EffectSummary, FunctionSourceOrigin, OpId, Operand, Provenance, SemAbiParam,
    SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind,
    SemParamPassing, SemSignature, SemTerminator, SirDiagnosticKind, UseMode, ValueDef, ValueId,
};
use hew_types::{DefId, ResolvedTy};

fn definition(id: u32) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty: ResolvedTy::I64,
    }
}

fn callable_for(function: &SemFunction) -> SemCallable {
    SemCallable {
        id: function.callable,
        function: function.id,
        declaration: function.declaration.clone(),
        symbol: function.name.clone(),
        source_origin: function.source_origin.clone(),
        signature: SemSignature {
            params: function
                .params
                .iter()
                .map(|parameter| SemAbiParam {
                    ty: parameter.ty.clone(),
                    passing: SemParamPassing::ReadOnly,
                    caller_visible_projection: false,
                })
                .collect(),
            return_ty: function.return_ty.clone(),
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
        effect_summary: EffectSummary::Unknown,
    }
}

fn module(functions: Vec<SemFunction>) -> SemModule {
    let mut callables = Vec::new();
    for function in &functions {
        if !callables
            .iter()
            .any(|callable: &SemCallable| callable.id == function.callable)
        {
            callables.push(callable_for(function));
        }
    }
    SemModule {
        callables,
        root_unit_callables: Vec::new(),
        entry_callable: None,
        functions,
    }
}

fn entry_module(function: SemFunction) -> SemModule {
    let mut module = module(vec![function]);
    module.root_unit_callables = vec![CallableId(0)];
    module.entry_callable = Some(CallableId(0));
    module
}

fn unit_function(
    id: u32,
    declaration: &str,
    name: &str,
    source_origin: FunctionSourceOrigin,
    params: Vec<BlockArg>,
) -> SemFunction {
    SemFunction {
        id: ItemId(id),
        callable: CallableId(0),
        declaration: DefId::for_test(declaration),
        name: name.to_string(),
        span: 0..0,
        source_origin,
        params,
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return { value: None },
        }],
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
        callable: CallableId(0),
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
    let module = module(vec![function]);
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
    let module = module(vec![SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
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
    }]);

    assert!(verify_module(&module).iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::EntryBlockArgs {
            entry: BlockId(0),
            actual: 1,
        }
    )));
}

#[test]
fn verifier_requires_zero_results_for_a_unit_direct_call() {
    let unit_helper = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("unit_helper"),
        name: "unit_helper".to_string(),
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
    let caller = SemFunction {
        id: ItemId(1),
        callable: CallableId(1),
        declaration: DefId::for_test("caller"),
        name: "caller".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: Vec::new(),
                kind: SemOpKind::Call {
                    callee: CallableId(0),
                    args: Vec::new(),
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return { value: None },
        }],
    };
    let mut valid = module(vec![unit_helper, caller]);
    assert!(
        verify_module(&valid).is_empty(),
        "a unit direct call with no SSA result must verify: {:#?}",
        verify_module(&valid)
    );

    valid.functions[1].blocks[0].ops[0].results = vec![definition(0)];
    assert!(
        verify_module(&valid).iter().any(|diagnostic| matches!(
            diagnostic.kind,
            SirDiagnosticKind::InvalidCallResultArity {
                callee: CallableId(0),
                expected: 0,
                actual: 1,
                ..
            }
        )),
        "a unit direct call must reject a fabricated SSA result: {:#?}",
        verify_module(&valid)
    );
}

#[test]
fn verifier_rejects_noncanonical_block_ids_and_order() {
    let non_contiguous = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("non_contiguous"),
        name: "non_contiguous".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(2),
                    args: Vec::new(),
                }),
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![definition(0)],
                    kind: SemOpKind::ConstI64(1),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            },
        ],
    };
    let non_contiguous_diagnostics = verify_module(&module(vec![non_contiguous]));
    assert!(non_contiguous_diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::NonCanonicalBlockOrder {
            expected: BlockId(1),
            actual: BlockId(2),
        }
    )));

    let out_of_order = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("out_of_order"),
        name: "out_of_order".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![definition(0)],
                    kind: SemOpKind::ConstI64(1),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(ValueId(0)),
                },
            },
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(1),
                    args: Vec::new(),
                }),
            },
        ],
    };
    let out_of_order_diagnostics = verify_module(&module(vec![out_of_order]));
    assert!(out_of_order_diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::NonCanonicalBlockOrder {
            expected: BlockId(0),
            actual: BlockId(1),
        }
    )));
    assert!(out_of_order_diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::NonCanonicalBlockOrder {
            expected: BlockId(1),
            actual: BlockId(0),
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
        callee: CallableId(0),
        args: Vec::new(),
    };
    assert_eq!(unresolved_call.effects(), EffectSet::UNKNOWN_CALL);
    assert!(unresolved_call.effects().contains(EffectSet::MAY_TRAP));
    assert!(unresolved_call.effects().may_trap());
    assert_eq!(EffectSummary::MayTrap.effects(), EffectSet::MAY_TRAP);
}

#[test]
fn verifier_checks_resolved_direct_call_signature_and_use_mode() {
    let target = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("target"),
        name: "target".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(ValueId(0)),
            },
        }],
    };
    let caller = SemFunction {
        id: ItemId(1),
        callable: CallableId(1),
        declaration: DefId::for_test("caller"),
        name: "caller".to_string(),
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
                kind: SemOpKind::Call {
                    callee: CallableId(0),
                    args: vec![Operand {
                        value: ValueId(0),
                        mode: UseMode::BorrowShared,
                    }],
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(ValueId(1)),
            },
        }],
    };
    let diagnostics = verify_module(&module(vec![target, caller]));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("only Read is legal")
    )));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("direct call result")
    )));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("direct call argument 0")
    )));
}

#[test]
fn verifier_rejects_unknown_direct_callable() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("caller"),
        name: "caller".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: vec![definition(0)],
                kind: SemOpKind::Call {
                    callee: CallableId(9),
                    args: Vec::new(),
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(ValueId(0)),
            },
        }],
    };
    assert!(verify_module(&module(vec![function]))
        .iter()
        .any(|diagnostic| matches!(
            diagnostic.kind,
            SirDiagnosticKind::UnknownCallable {
                op: OpId(0),
                callee: CallableId(9),
            }
        )));
}

#[test]
fn verifier_requires_one_result_for_a_non_unit_direct_call() {
    let scalar_callee = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("scalar_callee"),
        name: "scalar_callee".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: vec![definition(0)],
                kind: SemOpKind::ConstI64(1),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(ValueId(0)),
            },
        }],
    };
    let caller = SemFunction {
        id: ItemId(1),
        callable: CallableId(1),
        declaration: DefId::for_test("unit_caller"),
        name: "unit_caller".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: Vec::new(),
                kind: SemOpKind::Call {
                    callee: CallableId(0),
                    args: Vec::new(),
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return { value: None },
        }],
    };
    assert!(verify_module(&module(vec![scalar_callee, caller]))
        .iter()
        .any(|diagnostic| matches!(
            diagnostic.kind,
            SirDiagnosticKind::InvalidCallResultArity {
                callee: CallableId(0),
                expected: 1,
                actual: 0,
                ..
            }
        )));
}

#[test]
fn verifier_rejects_eager_logical_ops_and_ownership_modes_without_an_owner() {
    let module = module(vec![SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
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
    }]);

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
        callable: CallableId(0),
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
    let diagnostics = verify_module(&module(vec![function.clone(), function]));
    assert!(diagnostics
        .iter()
        .any(|diagnostic| matches!(diagnostic.kind, SirDiagnosticKind::DuplicateFunctionName(_))));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        diagnostic.kind,
        SirDiagnosticKind::DuplicateFunctionDeclaration(_)
    )));
}

#[test]
fn verifier_requires_entry_to_be_canonical_parameterless_root_main_with_portable_abi() {
    let valid = entry_module(unit_function(
        0,
        "main",
        "main",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(
        verify_module(&valid).is_empty(),
        "canonical source main must satisfy the SIR entry boundary: {:#?}",
        verify_module(&valid)
    );

    let non_main = entry_module(unit_function(
        0,
        "helper",
        "helper",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(verify_module(&non_main).iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
            if reason.contains("canonical root-unit source `main`")
    )));

    let parameterized_main = entry_module(unit_function(
        0,
        "main",
        "main",
        FunctionSourceOrigin::RootUnit,
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
        }],
    ));
    assert!(verify_module(&parameterized_main)
        .iter()
        .any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
                if reason.contains("parameterless")
        )));

    let wrong_symbol = entry_module(unit_function(
        0,
        "main",
        "not_main",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(verify_module(&wrong_symbol)
        .iter()
        .any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
                if reason.contains("emitted `main` symbol")
        )));

    let bool_main = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("main"),
        name: "main".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::RootUnit,
        params: Vec::new(),
        return_ty: ResolvedTy::Bool,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: vec![ValueDef {
                    id: ValueId(0),
                    ty: ResolvedTy::Bool,
                }],
                kind: SemOpKind::ConstBool(true),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(ValueId(0)),
            },
        }],
    };
    let bool_main = entry_module(bool_main);
    assert!(verify_module(&bool_main).iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
            if reason.contains("unit or an integer exit status")
    )));
}

#[test]
fn function_verifier_keeps_callable_table_diagnostics() {
    let function = unit_function(
        0,
        "f",
        "f",
        FunctionSourceOrigin::Unknown,
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
        }],
    );
    let mut module = module(vec![function.clone()]);
    module.callables[0].signature.params[0].caller_visible_projection = true;

    let diagnostics = verify_function_in_module(&module, &function);
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidCallable { callable: CallableId(0), reason }
            if reason.contains("caller-visible projection")
    )), "function-at-boundary verification must not discard malformed callable-table diagnostics: {diagnostics:#?}");
}

#[test]
fn verifier_rejects_value_carrying_return_from_unit_function() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("returns_unit"),
        name: "returns_unit".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::Unit,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: vec![SemOp {
                id: OpId(0),
                results: vec![definition(0)],
                kind: SemOpKind::ConstI64(1),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(ValueId(0)),
            },
        }],
    };

    let diagnostics = verify_module(&module(vec![function]));
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            diagnostic.kind,
            SirDiagnosticKind::UnitReturnValue { value: ValueId(0) }
        )),
        "unit returns must remain zero-value terminators: {diagnostics:#?}"
    );
}
