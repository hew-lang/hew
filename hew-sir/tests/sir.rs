use hew_hir::ItemId;
use hew_parser::ast::BinaryOp;
use hew_sir::{
    build_def_use, dump_sir, replace_all_uses, replace_use, verify_function_in_module,
    verify_module, BlockArg, BlockId, CallableId, CallableInstance, Edge, EffectSet,
    FunctionSourceOrigin, GenericTemplateId, OpId, Operand, OperandSlot, OwnKind, Provenance,
    RewriteError, SemAbiParam, SemBlock, SemCallConv, SemCallable, SemCallableKind, SemFunction,
    SemModule, SemOp, SemOpKind, SemParamPassing, SemSignature, SemTerminator, SirDiagnosticKind,
    SirInstanceKey, UseSite, ValueDef, ValueId,
};
use hew_types::{DefId, ResolvedTy};
use std::collections::BTreeMap;

fn definition(id: u32) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
        provenance: None,
    }
}

fn read(value: ValueId) -> Operand {
    Operand { value }
}

fn callable_for(function: &SemFunction) -> SemCallable {
    SemCallable {
        id: function.callable,
        function: function.id,
        declaration: function.declaration.clone(),
        instance: CallableInstance::Monomorphic,
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
        generic_templates: Vec::new(),
        root_unit_callables: Vec::new(),
        entry_callable: None,
        functions,
        type_facts: BTreeMap::new(),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
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
        places: Vec::new(),
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
                own: OwnKind::None,
                provenance: None,
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
                provenance: None,
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
                            own: OwnKind::None,
                            provenance: None,
                        }],
                        kind: SemOpKind::Binary {
                            op: BinaryOp::Greater,
                            lhs: Operand { value: ValueId(0) },
                            rhs: Operand { value: ValueId(2) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Branch {
                    condition: read(ValueId(3)),
                    then_target: Edge {
                        target: BlockId(1),
                        args: vec![read(ValueId(1))],
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: vec![read(ValueId(1))],
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(4),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
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
                            lhs: Operand { value: ValueId(4) },
                            rhs: Operand { value: ValueId(5) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![read(ValueId(6))],
                }),
            },
            SemBlock {
                id: BlockId(2),
                args: vec![BlockArg {
                    value: ValueId(7),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
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
                            lhs: Operand { value: ValueId(7) },
                            rhs: Operand { value: ValueId(8) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![read(ValueId(9))],
                }),
            },
            SemBlock {
                id: BlockId(3),
                args: vec![BlockArg {
                    value: ValueId(10),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
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
                            lhs: Operand { value: ValueId(10) },
                            rhs: Operand { value: ValueId(11) },
                        },
                        provenance: Provenance::Synthesized,
                    },
                ],
                terminator: SemTerminator::Return {
                    value: Some(read(ValueId(12))),
                },
            },
        ],
        places: Vec::new(),
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

/// The callable-to-body association is the one place the "exactly one body"
/// rule lives. A callable claimed by two bodies must be refused rather than
/// resolved to whichever body happens to come first.
#[test]
fn the_function_index_refuses_a_callable_claimed_by_two_bodies() {
    let function = unit_function(
        0,
        "claimed",
        "claimed",
        FunctionSourceOrigin::Unknown,
        Vec::new(),
    );
    let mut duplicate = function.clone();
    duplicate.id = ItemId(1);

    // Negative control: one body for the callable resolves.
    let single = module(vec![function.clone()]);
    assert_eq!(
        single
            .function_index()
            .function(CallableId(0))
            .map(|body| body.id),
        Some(ItemId(0)),
    );

    let ambiguous = module(vec![function, duplicate]);
    assert!(
        ambiguous.function_index().function(CallableId(0)).is_none(),
        "two bodies for one callable must not resolve to an arbitrary winner"
    );
    assert!(
        !verify_module(&ambiguous).is_empty(),
        "and the module itself must be rejected"
    );
}

/// A callable that legitimately has no body — the entry closure never demanded
/// one — is an absence, not a malformed table.
#[test]
fn the_function_index_reports_a_bodyless_callable_as_absent() {
    let mut module = module(vec![unit_function(
        0,
        "present",
        "present",
        FunctionSourceOrigin::Unknown,
        Vec::new(),
    )]);
    module.callables.push(SemCallable {
        id: CallableId(1),
        function: ItemId(1),
        declaration: DefId::for_test("headerless"),
        instance: CallableInstance::Monomorphic,
        symbol: "headerless".to_string(),
        source_origin: FunctionSourceOrigin::Unknown,
        signature: SemSignature {
            params: Vec::new(),
            return_ty: ResolvedTy::Unit,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
    });

    let index = module.function_index();
    assert!(index.function(CallableId(0)).is_some());
    assert!(index.function(CallableId(1)).is_none());
    assert!(
        verify_module(&module).is_empty(),
        "a bodyless callable header is legal: {:#?}",
        verify_module(&module)
    );
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
                own: OwnKind::None,
                provenance: None,
            }],
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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
        places: Vec::new(),
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
        places: Vec::new(),
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
                    value: Some(read(ValueId(0))),
                },
            },
        ],
        places: Vec::new(),
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
                    value: Some(read(ValueId(0))),
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
        places: Vec::new(),
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
        lhs: Operand { value: ValueId(0) },
        rhs: Operand { value: ValueId(1) },
    };
    assert_eq!(checked_add.effects(), EffectSet::MAY_TRAP);
    assert!(checked_add.effects().may_trap());

    let wrapping_add = SemOpKind::Binary {
        op: BinaryOp::WrappingAdd,
        lhs: Operand { value: ValueId(0) },
        rhs: Operand { value: ValueId(1) },
    };
    assert!(wrapping_add.effects().is_pure());

    let unresolved_call = SemOpKind::Call {
        callee: CallableId(0),
        args: Vec::new(),
    };
    assert_eq!(unresolved_call.effects(), EffectSet::UNKNOWN_CALL);
    assert!(unresolved_call.effects().contains(EffectSet::MAY_TRAP));
    assert!(unresolved_call.effects().may_trap());
}

#[test]
fn verifier_checks_resolved_direct_call_signature() {
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
            own: OwnKind::None,
            provenance: None,
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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
            own: OwnKind::None,
            provenance: None,
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
                    own: OwnKind::None,
                    provenance: None,
                }],
                kind: SemOpKind::Call {
                    callee: CallableId(0),
                    args: vec![Operand { value: ValueId(0) }],
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(read(ValueId(1))),
            },
        }],
        places: Vec::new(),
    };
    let diagnostics = verify_module(&module(vec![target, caller]));
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
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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
        places: Vec::new(),
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
fn verifier_rejects_eager_logical_ops() {
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
            own: OwnKind::None,
            provenance: None,
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
                    own: OwnKind::None,
                    provenance: None,
                }],
                kind: SemOpKind::Binary {
                    op: BinaryOp::And,
                    lhs: Operand { value: ValueId(0) },
                    rhs: Operand { value: ValueId(0) },
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(read(ValueId(1))),
            },
        }],
        places: Vec::new(),
    }]);

    let diagnostics = verify_module(&module);
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
        places: Vec::new(),
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

/// Naming is not part of the entry boundary: HIR owns entry identity and SIR
/// only joins on the id it published. Neither the declaration path nor the
/// emitted symbol may decide whether a callable is the entry.
///
/// The negative controls for the rules that *do* survive live in
/// [`verifier_requires_entry_to_be_a_parameterless_root_callable_with_a_portable_abi`].
#[test]
fn verifier_admits_an_entry_whose_declaration_and_symbol_are_not_spelled_main() {
    let non_main = entry_module(unit_function(
        0,
        "helper",
        "helper",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(
        verify_module(&non_main).is_empty(),
        "an entry whose declaration is not spelled `main` must satisfy the same shape rule: {:#?}",
        verify_module(&non_main)
    );

    let renamed_symbol = entry_module(unit_function(
        0,
        "main",
        "not_main",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(
        verify_module(&renamed_symbol).is_empty(),
        "the emitted entry symbol is a linkage decision codegen owns, not a SIR entry rule: {:#?}",
        verify_module(&renamed_symbol)
    );
}

/// The entry boundary is a *shape* rule: root-unit provenance, no parameters,
/// and a portable exit status.
#[test]
fn verifier_requires_entry_to_be_a_parameterless_root_callable_with_a_portable_abi() {
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

    let foreign_entry = entry_module(unit_function(
        0,
        "main",
        "main",
        FunctionSourceOrigin::Foreign("dep".to_string()),
        Vec::new(),
    ));
    assert!(
        verify_module(&foreign_entry)
            .iter()
            .any(|diagnostic| matches!(
                &diagnostic.kind,
                SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
                    if reason.contains("root-unit callable")
            )),
        "provenance, not spelling, is what the entry rule fails closed on: {:#?}",
        verify_module(&foreign_entry)
    );

    let parameterized_main = entry_module(unit_function(
        0,
        "main",
        "main",
        FunctionSourceOrigin::RootUnit,
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
            provenance: None,
        }],
    ));
    assert!(verify_module(&parameterized_main)
        .iter()
        .any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
                if reason.contains("parameterless")
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
                    own: OwnKind::None,
                    provenance: None,
                }],
                kind: SemOpKind::ConstBool(true),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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
            own: OwnKind::None,
            provenance: None,
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
                value: Some(read(ValueId(0))),
            },
        }],
        places: Vec::new(),
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

#[expect(
    clippy::too_many_lines,
    reason = "one complete hand-built body makes every rewrite site auditable in one place"
)]
fn rewrite_fixture() -> SemFunction {
    SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("rewrite_fixture"),
        name: "rewrite_fixture".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![
            BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
                provenance: None,
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
                provenance: None,
            },
            BlockArg {
                value: ValueId(2),
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
                provenance: None,
            },
            BlockArg {
                value: ValueId(7),
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
                provenance: None,
            },
        ],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![definition(3)],
                    kind: SemOpKind::Binary {
                        op: BinaryOp::Add,
                        lhs: read(ValueId(0)),
                        rhs: read(ValueId(0)),
                    },
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(ValueId(2)),
                    then_target: Edge {
                        target: BlockId(1),
                        args: vec![read(ValueId(0))],
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: vec![read(ValueId(0))],
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(4),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![read(ValueId(4))],
                }),
            },
            SemBlock {
                id: BlockId(2),
                args: vec![BlockArg {
                    value: ValueId(5),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: vec![read(ValueId(5))],
                }),
            },
            SemBlock {
                id: BlockId(3),
                args: vec![BlockArg {
                    value: ValueId(6),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                    provenance: None,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(read(ValueId(6))),
                },
            },
        ],
        places: Vec::new(),
    }
}

#[test]
fn def_use_sites_are_deterministic_and_support_precise_rewrites() {
    let mut function = rewrite_fixture();
    let index = build_def_use(&function);
    let expected_x_uses = vec![
        UseSite::Operation {
            op: OpId(0),
            operand: OperandSlot(0),
            value: ValueId(0),
        },
        UseSite::Operation {
            op: OpId(0),
            operand: OperandSlot(1),
            value: ValueId(0),
        },
        UseSite::Terminator {
            block: BlockId(0),
            operand: OperandSlot(1),
            value: ValueId(0),
        },
        UseSite::Terminator {
            block: BlockId(0),
            operand: OperandSlot(2),
            value: ValueId(0),
        },
    ];
    assert_eq!(index.uses_of(ValueId(0)), expected_x_uses);
    assert_eq!(index.use_count(ValueId(0)), 4);

    replace_use(&mut function, expected_x_uses[0], ValueId(1))
        .expect("a fresh indexed operation use must be rewriteable");
    assert_eq!(
        build_def_use(&function).uses_of(ValueId(0)),
        &expected_x_uses[1..],
        "a precise rewrite must leave every other concrete use intact"
    );

    assert_eq!(
        replace_all_uses(&mut function, ValueId(0), ValueId(1))
            .expect("a stable function must rewrite every indexed use"),
        3
    );
    assert!(build_def_use(&function).uses_of(ValueId(0)).is_empty());

    assert_eq!(
        replace_all_uses(&mut function, ValueId(2), ValueId(7))
            .expect("a stable function must rewrite the branch-condition use"),
        1,
        "replace_all_uses must reach a branch-condition operand"
    );
    assert!(
        build_def_use(&function).uses_of(ValueId(2)).is_empty(),
        "the only branch-condition use must have been replaced"
    );

    assert_eq!(
        replace_all_uses(&mut function, ValueId(4), ValueId(1))
            .expect("a stable function must rewrite the goto-edge use"),
        1,
        "replace_all_uses must reach a goto-edge operand"
    );
    assert!(
        build_def_use(&function).uses_of(ValueId(4)).is_empty(),
        "the goto-edge use must have been replaced"
    );

    let return_site = build_def_use(&function)
        .uses_of(ValueId(6))
        .first()
        .copied()
        .expect("join block value must have a concrete return use");
    assert_eq!(
        return_site,
        UseSite::Terminator {
            block: BlockId(3),
            operand: OperandSlot(0),
            value: ValueId(6),
        }
    );
    assert_eq!(
        replace_all_uses(&mut function, ValueId(6), ValueId(1))
            .expect("a stable function must rewrite the return use"),
        1,
        "replace_all_uses must reach a return operand"
    );
    assert!(
        build_def_use(&function).uses_of(ValueId(6)).is_empty(),
        "the return use must have been replaced"
    );

    assert!(
        verify_module(&module(vec![function])).is_empty(),
        "slot-addressed rewrites must preserve a valid scalar SIR graph"
    );
}

#[test]
fn replace_all_uses_is_atomic_when_malformed_identities_make_a_site_stale() {
    let mut function = rewrite_fixture();
    function.blocks[0].ops.push(SemOp {
        // Deliberately duplicate OpId(0): `build_def_use` can still produce
        // useful diagnostics for malformed SIR, but it must not make a
        // partially applied public rewrite look successful.
        id: OpId(0),
        results: vec![definition(8)],
        kind: SemOpKind::Unary {
            op: hew_parser::ast::UnaryOp::Negate,
            value: read(ValueId(0)),
        },
        provenance: Provenance::Synthesized,
    });
    let before = function.clone();

    assert!(matches!(
        replace_all_uses(&mut function, ValueId(0), ValueId(1)),
        Err(RewriteError::StaleUseSite(_))
    ));
    assert_eq!(
        function, before,
        "a failed all-uses rewrite must leave the original SIR graph intact"
    );
}

#[test]
fn indexed_rewrites_refuse_a_stale_operand_value() {
    let mut function = rewrite_fixture();
    let site = build_def_use(&function)
        .uses_of(ValueId(0))
        .first()
        .copied()
        .expect("fixture must have an operation use of its first parameter");
    function.blocks[0].ops[0].visit_operands_mut(|slot, operand| {
        if slot == OperandSlot(0) {
            operand.value = ValueId(1);
        }
    });

    assert_eq!(
        replace_use(&mut function, site, ValueId(0)),
        Err(RewriteError::StaleUseSite(site)),
        "an old index must not replace a different value in the same slot"
    );
}

#[test]
fn operand_and_successor_visitors_centralize_cfg_rewrites() {
    let mut function = rewrite_fixture();
    let block = &mut function.blocks[0];
    let mut op_slots = Vec::new();
    block.ops[0].visit_operands(|slot, operand| op_slots.push((slot, operand.value)));
    assert_eq!(
        op_slots,
        vec![(OperandSlot(0), ValueId(0)), (OperandSlot(1), ValueId(0))]
    );

    let mut successors = Vec::new();
    block
        .terminator
        .visit_successors(|edge| successors.push(edge.target));
    assert_eq!(successors, vec![BlockId(1), BlockId(2)]);

    block.terminator.visit_successors_mut(|edge| {
        edge.visit_operands_mut(|_, operand| operand.value = ValueId(1));
    });
    let mut terminator_uses = Vec::new();
    block
        .terminator
        .visit_operands(|slot, operand| terminator_uses.push((slot, operand.value)));
    assert_eq!(
        terminator_uses,
        vec![
            (OperandSlot(0), ValueId(2)),
            (OperandSlot(1), ValueId(1)),
            (OperandSlot(2), ValueId(1)),
        ],
        "the terminator visitor must cover the condition and both edge operands in slot order"
    );
}

/// The entry is the program's one monomorphic root body.
///
/// A generic instance can satisfy every other entry shape rule — root-unit
/// provenance, listed as a root, parameterless, unit return — and must still
/// be refused: a specialization is one of many bodies derived from a template,
/// so there is no single source body for the native and WASI entry adapters to
/// name. Production lowering cannot build this shape today (generic callables
/// never reach the branch that assigns `entry_callable`), which is exactly why
/// the rule needs a hand-built module to be provable at all.
#[test]
fn verifier_refuses_a_generic_instance_as_the_module_entry() {
    let monomorphic = entry_module(unit_function(
        0,
        "main",
        "main",
        FunctionSourceOrigin::RootUnit,
        Vec::new(),
    ));
    assert!(
        verify_module(&monomorphic).is_empty(),
        "control: the same module with a monomorphic entry must verify clean: {:#?}",
        verify_module(&monomorphic)
    );

    let mut generic = monomorphic;
    generic.callables[0].instance = CallableInstance::Generic(SirInstanceKey {
        template: GenericTemplateId {
            declaration: DefId::for_test("main"),
        },
        type_args: vec![ResolvedTy::I64],
    });
    assert!(
        verify_module(&generic).iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidEntryCallable { callable: CallableId(0), reason }
                if reason.contains("monomorphic source body")
        )),
        "changing only the entry callable's instance kind must fail the entry rule closed: {:#?}",
        verify_module(&generic)
    );
}
