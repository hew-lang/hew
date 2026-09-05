use hew_hir::ItemId;
use hew_parser::ast::BinaryOp;
use hew_sir::{
    build_def_use, dump_sir, replace_all_uses, replace_use, verify_function_in_module,
    verify_module, BlockArg, BlockId, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind,
    CallableId, CallableInstance, Edge, EffectSet, FunctionSourceOrigin, GenericTemplateId, OpId,
    Operand, OperandSlot, OwnKind, Provenance, RewriteError, SemAbiParam, SemBlock, SemCallConv,
    SemCallable, SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind, SemParamPassing,
    SemSignature, SemTerminator, SirDiagnosticKind, SirInstanceKey, SuspendKind, TrapKind, UseSite,
    ValueDef, ValueId,
};
use hew_types::{DefId, ResolvedTy};
use std::collections::BTreeMap;

fn definition(id: u32) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
    }
}

fn read(value: ValueId) -> Operand {
    Operand { value }
}

fn returned(value: ValueId) -> BoundaryOperand {
    BoundaryOperand {
        operand: read(value),
        decision: BoundaryDecision::Move,
    }
}

fn copy_argument(value: ValueId) -> BoundaryOperand {
    BoundaryOperand {
        operand: read(value),
        decision: BoundaryDecision::Copy,
    }
}

fn call(
    id: u32,
    callee: u32,
    args: Vec<BoundaryOperand>,
    result: CallResult,
    normal: u32,
    normal_args: Vec<Operand>,
) -> SemTerminator {
    SemTerminator::Call {
        id: OpId(id),
        callee: CallableId(callee),
        args,
        result,
        normal: Edge {
            target: BlockId(normal),
            args: normal_args,
        },
        unwind: CallUnwind::NotApplicable,
    }
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
        entry_exit_plan: None,
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
        bindings: Vec::new(),
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
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
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
                    value: Some(returned(ValueId(12))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
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
            }],
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(returned(ValueId(0))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
        bindings: Vec::new(),
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
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: call(0, 0, Vec::new(), CallResult::Unit, 1, Vec::new()),
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    let mut valid = module(vec![unit_helper, caller]);
    assert!(
        verify_module(&valid).is_empty(),
        "a unit direct call with no SSA result must verify: {:#?}",
        verify_module(&valid)
    );

    if let SemTerminator::Call { result, .. } = &mut valid.functions[1].blocks[0].terminator {
        *result = CallResult::Value(definition(0));
    }
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
                    value: Some(returned(ValueId(0))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
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
                    value: Some(returned(ValueId(0))),
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
        bindings: Vec::new(),
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
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Return {
                value: Some(returned(ValueId(0))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
        }],
        return_ty: ResolvedTy::Bool,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: call(
                    0,
                    0,
                    vec![copy_argument(ValueId(0))],
                    CallResult::Value(ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::Bool,
                        own: OwnKind::None,
                    }),
                    1,
                    vec![read(ValueId(1))],
                ),
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(2),
                    ty: ResolvedTy::Bool,
                    own: OwnKind::None,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(2))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
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
            ops: Vec::new(),
            terminator: call(
                0,
                9,
                Vec::new(),
                CallResult::Value(definition(0)),
                0,
                Vec::new(),
            ),
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
                value: Some(returned(ValueId(0))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: call(0, 0, Vec::new(), CallResult::Unit, 1, Vec::new()),
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
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
                }],
                kind: SemOpKind::Binary {
                    op: BinaryOp::And,
                    lhs: Operand { value: ValueId(0) },
                    rhs: Operand { value: ValueId(0) },
                },
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(returned(ValueId(1))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
        bindings: Vec::new(),
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
                }],
                kind: SemOpKind::ConstBool(true),
                provenance: Provenance::Synthesized,
            }],
            terminator: SemTerminator::Return {
                value: Some(returned(ValueId(0))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
                value: Some(returned(ValueId(0))),
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
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
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            },
            BlockArg {
                value: ValueId(2),
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            },
            BlockArg {
                value: ValueId(7),
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
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
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(6))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
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

/// A `Suspend` whose shape no §1.5 row admits - `Await` takes exactly one
/// resume edge, and a `Move` of a `BitCopy` scalar has nothing to move - still
/// reaches MIR unchallenged if the relation table simply says nothing about the
/// terminator. The operation table already refuses an operation it does not
/// verify; a terminator it does not verify is the same case.
#[test]
fn verifier_rejects_a_suspend_no_relation_row_admits() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("parks"),
        name: "parks".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Suspend {
                    kind: SuspendKind::Await,
                    inputs: vec![BoundaryOperand {
                        operand: read(ValueId(0)),
                        decision: BoundaryDecision::Move,
                    }],
                    resumes: Vec::new(),
                    cancel: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(0))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    let diagnostics = verify_module(&module(vec![function]));
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidTerminator { reason }
                if reason.contains("outside the verified SIR relation table")
        )),
        "a suspend no §1.5 row admits must be refused, got {diagnostics:?}"
    );
}

/// The same refusal for `Trap`: §1.6 gives a trap endpoint a kind table this
/// phase does not check, so the terminator is refused rather than admitted.
#[test]
fn verifier_rejects_a_trap_endpoint_it_states_no_rule_for() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("traps"),
        name: "traps".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: Vec::new(),
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Trap {
                kind: TrapKind::DivideByZero,
            },
        }],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    let diagnostics = verify_module(&module(vec![function]));
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::InvalidTerminator { .. }
        )),
        "a trap endpoint must be refused, got {diagnostics:?}"
    );
}

/// The counterfactual for both rows above: the terminators this table does
/// state a relation for are still admitted, so the refusal is about the
/// unverified kinds and not about terminators in general.
#[test]
fn verifier_still_admits_the_terminators_it_states_rules_for() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("branches"),
        name: "branches".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }],
        return_ty: ResolvedTy::Bool,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Branch {
                    condition: read(ValueId(0)),
                    then_target: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                    else_target: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(0))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    assert!(verify_module(&module(vec![function])).is_empty());
}

/// The §1.2 kind is a pure function of the type's class, so a definition that
/// says otherwise is a fact nothing downstream can trust: an `i64` declared
/// `Owned` owes a consuming use it can never have, and `Guaranteed` is the
/// kind of a `begin_borrow` result no phase emits. `own` was a free field the
/// lowering wrote and nothing read, so both were representable.
#[test]
fn verifier_refuses_an_own_kind_the_class_table_contradicts() {
    let diagnostics = verify_module(&module(vec![own_kind_function(
        OwnKind::Owned,
        OwnKind::Guaranteed,
    )]));
    let refused: Vec<&ValueId> = diagnostics
        .iter()
        .filter_map(|diagnostic| match &diagnostic.kind {
            SirDiagnosticKind::OwnershipKind { value, .. } => Some(value),
            _ => None,
        })
        .collect();
    assert!(
        refused.contains(&&ValueId(1)) && refused.contains(&&ValueId(2)),
        "both the `Owned` i64 result and the `Guaranteed` block argument must be refused, got {diagnostics:?}"
    );
}

/// A type the class rule cannot decide has no kind to check against, and §1.1
/// has no default: the verifier refuses it rather than admitting whatever the
/// definition happens to claim.
///
/// The type sits on an operation result rather than on a parameter, because a
/// parameter of a non-scalar type is already refused by the callable header
/// check (`InvalidCallable`). Nothing else refuses an undecidable type in the
/// middle of a body, so this is the case that needs the value-level rule.
#[test]
fn verifier_refuses_a_value_whose_type_the_class_rule_cannot_decide() {
    let mut function = own_kind_function(OwnKind::None, OwnKind::None);
    let undecidable = ResolvedTy::Named {
        name: "Conn".to_string(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    function.blocks[0].ops[0].results[0].ty = undecidable.clone();
    function.blocks[1].args[0].ty = undecidable;
    let diagnostics = verify_module(&module(vec![function]));
    assert!(
        diagnostics.iter().any(|diagnostic| matches!(
            &diagnostic.kind,
            SirDiagnosticKind::OwnershipKind { value, reason }
                if *value == ValueId(1) && reason.contains("cannot decide the ownership kind")
        )),
        "an operation result whose type the class rule cannot reach must be refused, got {diagnostics:?}"
    );
}

/// The counterfactual for both rows above: the same shape with the kinds the
/// class table actually gives passes clean, so the refusal is about the
/// contradiction and not about carrying an ownership kind at all.
#[test]
fn verifier_admits_the_own_kinds_the_class_table_gives() {
    assert!(
        verify_module(&module(vec![own_kind_function(
            OwnKind::None,
            OwnKind::None
        )]))
        .is_empty(),
        "kind-correct definitions must verify: {:?}",
        verify_module(&module(vec![own_kind_function(
            OwnKind::None,
            OwnKind::None
        )]))
    );
}

/// One `i64` result flowing on a `Goto` edge into one `i64` block argument.
/// Both carry a caller-chosen ownership kind so a test can state the kind the
/// class table gives (`OwnKind::None` for a `BitCopy` scalar) or one it does
/// not.
fn own_kind_function(result_own: OwnKind, arg_own: OwnKind) -> SemFunction {
    SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("kinds"),
        name: "kinds".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                        own: result_own,
                    }],
                    kind: SemOpKind::ConstI64(7),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(1),
                    args: vec![read(ValueId(1))],
                }),
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(2),
                    ty: ResolvedTy::I64,
                    own: arg_own,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(2))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    }
}

/// `ValueDef.own` and `BlockArg.own` are the §1.2 obligation the model now
/// carries. A field the lowering writes and nothing ever reads is a fact no
/// reviewer and no later lane can see, so the dump renders the two kinds that
/// carry an obligation.
///
/// This module fails `verify_own_kind` by construction - no class produces
/// `Guaranteed` - and the dump must render it anyway: a dump is what a
/// reviewer reads about malformed IR, so it stays total where the verifier
/// refuses.
#[test]
fn the_dump_renders_the_ownership_kind_a_value_carries() {
    let function = SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test("owns"),
        name: "owns".to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params: vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        }],
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![ValueDef {
                        id: ValueId(1),
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }],
                    kind: SemOpKind::ConstI64(7),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(1),
                    args: vec![read(ValueId(1))],
                }),
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: ValueId(2),
                    ty: ResolvedTy::I64,
                    own: OwnKind::Guaranteed,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(ValueId(2))),
                },
            },
        ],
        places: Vec::new(),
        bindings: Vec::new(),
    };
    let dump = dump_sir(&module(vec![function]));
    assert!(
        dump.contains("fn owns(%0: string owned)"),
        "an owned parameter must say so: {dump}"
    );
    assert!(
        dump.contains("bb1(%2: i64 guaranteed):"),
        "a guaranteed block argument must say so: {dump}"
    );
    // The counterfactual: the kind with no obligation prints nothing, so the
    // suffix is a fact about the value and not decoration on every line.
    assert!(
        dump.contains("    %1 = const 7\n"),
        "a no-obligation result must render unchanged: {dump}"
    );
}

/// A callee whose header slot is `passing`, and a caller that calls it.
fn borrow_slot_module(passing: SemParamPassing) -> SemModule {
    let callee = unit_function(
        0,
        "borrow_callee",
        "borrow_callee",
        FunctionSourceOrigin::Unknown,
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::I64,
            own: match passing {
                SemParamPassing::Borrow => OwnKind::Guaranteed,
                SemParamPassing::ReadOnly => OwnKind::None,
            },
        }],
    );
    let mut call_site = unit_function(
        1,
        "borrow_caller",
        "borrow_caller",
        FunctionSourceOrigin::Unknown,
        Vec::new(),
    );
    call_site.callable = CallableId(1);
    call_site.blocks[0].ops = vec![SemOp {
        id: OpId(0),
        results: vec![definition(1)],
        kind: SemOpKind::ConstI64(7),
        provenance: Provenance::Synthesized,
    }];
    call_site.blocks[0].terminator = call(
        1,
        0,
        vec![copy_argument(ValueId(1))],
        CallResult::Unit,
        1,
        Vec::new(),
    );
    call_site.blocks.push(SemBlock {
        id: BlockId(1),
        args: Vec::new(),
        ops: Vec::new(),
        terminator: SemTerminator::Return { value: None },
    });
    let mut module = module(vec![callee, call_site]);
    module.callables[0].signature.params[0].passing = passing;
    module
}

/// Does any diagnostic carry `needle` in its reason?
fn any_reason_contains(diagnostics: &[hew_sir::SirDiagnostic], needle: &str) -> bool {
    diagnostics.iter().any(|diagnostic| match &diagnostic.kind {
        SirDiagnosticKind::InvalidCallable { reason, .. }
        | SirDiagnosticKind::InvalidGenericTemplate { reason, .. }
        | SirDiagnosticKind::InvalidOperation { reason, .. } => reason.contains(needle),
        _ => false,
    })
}

/// §1.2 rule 3's `Borrow` header slot is representable and walled: the callable
/// table refuses a header that carries it, before any body reads it.
#[test]
fn verifier_refuses_a_callable_header_carrying_a_borrow_slot() {
    let diagnostics = verify_module(&borrow_slot_module(SemParamPassing::Borrow));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidCallable { callable, reason }
            if *callable == CallableId(0)
                && reason.contains("parameter 0 has non-ReadOnly ABI passing")
    )));
}

/// The second wall on the same module: a direct call to a callee whose slot is
/// `Borrow` is refused at the call site too, so a header that slipped through
/// cannot be reached by a call.
#[test]
fn verifier_refuses_a_direct_call_to_a_borrow_slot_parameter() {
    let diagnostics = verify_module(&borrow_slot_module(SemParamPassing::Borrow));
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidOperation { reason, .. }
            if reason.contains("parameter 0 has non-ReadOnly ABI passing")
    )));
}

/// The counterfactual for both walls: the same header and the same call with a
/// `ReadOnly` slot verify clean, so the refusals are about the slot and not
/// about the module shape.
#[test]
fn verifier_admits_the_same_header_and_call_with_a_read_only_slot() {
    let diagnostics = verify_module(&borrow_slot_module(SemParamPassing::ReadOnly));
    assert!(
        !any_reason_contains(&diagnostics, "non-ReadOnly ABI passing"),
        "{diagnostics:#?}"
    );
}

/// A generic template header carries the same wall: SIR does not own parameter
/// ownership policy before substitution either.
#[test]
fn verifier_refuses_a_generic_template_parameter_carrying_a_borrow_slot() {
    let mut module = borrow_slot_module(SemParamPassing::ReadOnly);
    module.generic_templates = vec![hew_sir::SemGenericTemplate {
        id: GenericTemplateId {
            declaration: DefId::for_test("borrow_template"),
        },
        function: ItemId(2),
        symbol: "borrow_template".to_string(),
        source_origin: FunctionSourceOrigin::Unknown,
        type_params: vec!["T".to_string()],
        signature: SemSignature {
            params: vec![SemAbiParam {
                ty: ResolvedTy::I64,
                passing: SemParamPassing::Borrow,
                caller_visible_projection: false,
            }],
            return_ty: ResolvedTy::Unit,
        },
    }];
    let diagnostics = verify_module(&module);
    assert!(diagnostics.iter().any(|diagnostic| matches!(
        &diagnostic.kind,
        SirDiagnosticKind::InvalidGenericTemplate { reason, .. }
            if reason.contains("template parameter 0 carries ownership or caller-visible ABI policy")
    )));
}

/// The counterfactual for the template wall: the same template with a
/// `ReadOnly` slot and no caller-visible projection is admitted.
#[test]
fn verifier_admits_a_generic_template_parameter_with_a_read_only_slot() {
    let mut module = borrow_slot_module(SemParamPassing::ReadOnly);
    module.generic_templates = vec![hew_sir::SemGenericTemplate {
        id: GenericTemplateId {
            declaration: DefId::for_test("read_only_template"),
        },
        function: ItemId(2),
        symbol: "read_only_template".to_string(),
        source_origin: FunctionSourceOrigin::Unknown,
        type_params: vec!["T".to_string()],
        signature: SemSignature {
            params: vec![SemAbiParam {
                ty: ResolvedTy::I64,
                passing: SemParamPassing::ReadOnly,
                caller_visible_projection: false,
            }],
            return_ty: ResolvedTy::Unit,
        },
    }];
    let diagnostics = verify_module(&module);
    assert!(
        !any_reason_contains(
            &diagnostics,
            "carries ownership or caller-visible ABI policy"
        ),
        "{diagnostics:#?}"
    );
}
