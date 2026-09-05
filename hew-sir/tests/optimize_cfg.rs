use hew_hir::ItemId;
use hew_sir::{
    canonicalize_module_constant_cfg, verify_function_in_module, verify_module, BlockArg, BlockId,
    BoundaryDecision, BoundaryOperand, CallableId, CallableInstance, CfgCanonicalizationReport,
    Edge, FunctionSourceOrigin, OpId, Operand, OwnKind, Provenance, SemAbiParam, SemBlock,
    SemCallConv, SemCallable, SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind,
    SemParamPassing, SemSignature, SemTerminator, SirDiagnosticKind, SirOptimizationError,
    ValueDef, ValueId,
};
use hew_types::{DefId, ResolvedTy, TypeFactContext, TypeFactService};
use std::collections::BTreeMap;

fn read(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
    }
}

fn returned(value: u32) -> BoundaryOperand {
    BoundaryOperand {
        operand: read(value),
        decision: BoundaryDecision::Move,
    }
}

fn value(id: u32, ty: ResolvedTy) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty,
        own: OwnKind::None,
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

fn module(function: SemFunction) -> SemModule {
    let mut fact_service = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
    for ty in function
        .params
        .iter()
        .map(|value| &value.ty)
        .chain(std::iter::once(&function.return_ty))
        .chain(function.blocks.iter().flat_map(|block| {
            block.args.iter().map(|value| &value.ty).chain(
                block
                    .ops
                    .iter()
                    .flat_map(|op| op.results.iter().map(|value| &value.ty)),
            )
        }))
    {
        let _ = fact_service.require(ty);
    }
    SemModule {
        callables: vec![callable_for(&function)],
        generic_templates: Vec::new(),
        root_unit_callables: Vec::new(),
        entry_exit_plan: None,
        entry_callable: None,
        functions: vec![function],
        aggregate_shapes: Vec::new(),
        type_facts: fact_service.into_rows(),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    }
}

fn function(
    name: &str,
    params: Vec<BlockArg>,
    return_ty: ResolvedTy,
    blocks: Vec<SemBlock>,
) -> SemFunction {
    SemFunction {
        id: ItemId(0),
        callable: CallableId(0),
        declaration: DefId::for_test(name),
        name: name.to_string(),
        span: 0..0,
        source_origin: FunctionSourceOrigin::Unknown,
        params,
        return_ty,
        entry: BlockId(0),
        blocks,
        places: Vec::new(),
        bindings: Vec::new(),
    }
}

/// Canonicalize one function through the module entry.
///
/// A parameter's §1.2 kind is its ABI header slot before it is its type's
/// class, so canonicalization runs against the callable table that names those
/// slots. This keeps each test's subject the function it built while the pass
/// still sees the facts it audits against.
fn canonicalize(
    function: &mut SemFunction,
) -> Result<CfgCanonicalizationReport, SirOptimizationError> {
    let mut wrapper = module(function.clone());
    let reports = canonicalize_module_constant_cfg(&mut wrapper)?;
    *function = wrapper.functions.remove(0);
    Ok(reports
        .into_iter()
        .next()
        .expect("a one-function module reports one canonicalization")
        .1)
}

fn false_same_target_diamond() -> SemFunction {
    function(
        "false_same_target_diamond",
        vec![
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
        ResolvedTy::I64,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(2, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(false),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(2),
                    then_target: Edge {
                        target: BlockId(2),
                        args: vec![read(0)],
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: vec![read(1)],
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(1),
                    results: vec![value(3, ResolvedTy::I64)],
                    kind: SemOpKind::ConstI64(99),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(returned(3)),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![BlockArg {
                    value: ValueId(4),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(4)),
                },
            },
        ],
    )
}

#[test]
fn false_branch_keeps_the_selected_duplicate_target_edge_and_remaps_blocks() {
    let mut function = false_same_target_diamond();
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report = canonicalize(&mut function).expect("verified SIR must canonicalize successfully");
    assert_eq!(
        report,
        CfgCanonicalizationReport {
            folded_branches: 1,
            removed_blocks: vec![BlockId(1)],
            block_remap: [(BlockId(0), BlockId(0)), (BlockId(2), BlockId(1))]
                .into_iter()
                .collect(),
        }
    );
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
    assert_eq!(function.blocks.len(), 2);
    assert!(matches!(
        &function.blocks[0].terminator,
        SemTerminator::Goto(Edge { target: BlockId(1), args })
            if args == &vec![read(1)]
    ));
    assert_eq!(function.blocks[1].args[0].value, ValueId(4));
}

#[test]
fn compaction_preserves_every_surviving_non_block_identity_and_fact() {
    let mut function = false_same_target_diamond();
    let before = function.clone();

    let report =
        canonicalize(&mut function).expect("verified duplicate-edge CFG must canonicalize");

    assert_eq!(function.id, before.id);
    assert_eq!(function.callable, before.callable);
    assert_eq!(function.declaration, before.declaration);
    assert_eq!(function.name, before.name);
    assert_eq!(function.span, before.span);
    assert_eq!(function.source_origin, before.source_origin);
    assert_eq!(function.params, before.params);
    assert_eq!(function.return_ty, before.return_ty);

    for (former_id, canonical_id) in &report.block_remap {
        let former = before
            .blocks
            .iter()
            .find(|block| block.id == *former_id)
            .expect("every remap source must name an input block");
        let canonical = function
            .blocks
            .iter()
            .find(|block| block.id == *canonical_id)
            .expect("every remap target must name an output block");

        // These equalities jointly cover block-argument and operation
        // ValueIds, OpIds, types, provenance and operands.
        assert_eq!(canonical.args, former.args);
        assert_eq!(canonical.ops, former.ops);

        let mut expected_terminator = if *former_id == BlockId(0) {
            match &former.terminator {
                SemTerminator::Branch { else_target, .. } => {
                    SemTerminator::Goto(else_target.clone())
                }
                other => panic!("fixture entry must be a branch, found {other:?}"),
            }
        } else {
            former.terminator.clone()
        };
        expected_terminator.visit_successors_mut(|edge| {
            edge.target = report.block_remap[&edge.target];
        });
        assert_eq!(canonical.terminator, expected_terminator);
    }
}

#[test]
fn discard_safety_preserves_a_trapping_arm_that_structural_verification_accepts() {
    let mut function = function(
        "discarded_trap",
        Vec::new(),
        ResolvedTy::Unit,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(0, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(true),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(0),
                    then_target: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Trap {
                    kind: hew_sir::TrapKind::DivideByZero,
                },
            },
        ],
    );
    let before = function.clone();
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    // This is the counterfactual: the ordinary post-fold verifier alone sees
    // valid SSA even though compaction would erase a trapping terminator.
    let mut structurally_valid_but_unsafe = function.clone();
    structurally_valid_but_unsafe.blocks[0].terminator = SemTerminator::Goto(Edge {
        target: BlockId(1),
        args: Vec::new(),
    });
    assert!(verify_function_in_module(
        &module(structurally_valid_but_unsafe.clone()),
        &structurally_valid_but_unsafe
    )
    .is_empty());

    let report = canonicalize(&mut function)
        .expect("an unsafe optional fold must retain the original valid CFG");
    assert_eq!(report.folded_branches, 0);
    assert!(report.removed_blocks.is_empty());
    assert_eq!(function, before);
}

#[test]
fn constant_branch_preserves_a_reachable_semantic_unreachable_endpoint() {
    let mut function = function(
        "constant_to_unreachable",
        Vec::new(),
        ResolvedTy::Unit,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(0, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(true),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(0),
                    then_target: Edge {
                        target: BlockId(2),
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
                terminator: SemTerminator::Return { value: None },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Unreachable,
            },
        ],
    );
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report = canonicalize(&mut function).expect("constant CFG must fold");
    assert_eq!(report.removed_blocks, vec![BlockId(1)]);
    assert!(matches!(
        &function.blocks[0].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(1),
            ..
        })
    ));
    assert!(matches!(
        &function.blocks[1].terminator,
        SemTerminator::Unreachable
    ));
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
}

#[test]
fn compaction_accepts_a_newly_dead_block_that_reads_an_entry_parameter() {
    let mut function = function(
        "dead_parameter_read",
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }],
        ResolvedTy::Bool,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(1, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(true),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(1),
                    then_target: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(0)),
                },
            },
            // Folding bb0 makes this block unreachable before the pass's
            // second structural stage removes it. Its entry-param use remains
            // semantically valid during that verifier boundary.
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(returned(0)),
                },
            },
        ],
    );
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report = canonicalize(&mut function)
        .expect("dead blocks may retain entry-value uses until compaction");
    assert_eq!(report.removed_blocks, vec![BlockId(2)]);
    assert_eq!(function.blocks.len(), 2);
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
}

#[test]
fn compaction_remaps_reachable_self_loops() {
    let mut function = function(
        "loop_remap",
        Vec::new(),
        ResolvedTy::Unit,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(0, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(true),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(0),
                    then_target: Edge {
                        target: BlockId(2),
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
                terminator: SemTerminator::Return { value: None },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(2),
                    args: Vec::new(),
                }),
            },
        ],
    );
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    canonicalize(&mut function).expect("loop CFG must canonicalize");
    assert_eq!(function.blocks.len(), 2);
    assert!(matches!(
        &function.blocks[1].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(1),
            ..
        })
    ));
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
}

#[test]
#[allow(
    clippy::too_many_lines,
    reason = "the explicit CFG construction and every remapped edge assertion keep this structural regression auditable"
)]
fn compaction_remaps_multiblock_loop_edges_after_dead_block_removal() {
    let mut function = function(
        "multiblock_loop_remap",
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }],
        ResolvedTy::Unit,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(1, ResolvedTy::Bool)],
                    kind: SemOpKind::ConstBool(true),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Branch {
                    condition: read(1),
                    then_target: Edge {
                        target: BlockId(2),
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
                terminator: SemTerminator::Return { value: None },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(3),
                    args: Vec::new(),
                }),
            },
            SemBlock {
                id: BlockId(3),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Branch {
                    condition: read(0),
                    then_target: Edge {
                        target: BlockId(2),
                        args: Vec::new(),
                    },
                    else_target: Edge {
                        target: BlockId(4),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(4),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
        ],
    );
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report = canonicalize(&mut function)
        .expect("a constant entry edge must compact a multiblock loop safely");
    assert_eq!(report.removed_blocks, vec![BlockId(1)]);
    assert_eq!(
        report.block_remap,
        [
            (BlockId(0), BlockId(0)),
            (BlockId(2), BlockId(1)),
            (BlockId(3), BlockId(2)),
            (BlockId(4), BlockId(3)),
        ]
        .into_iter()
        .collect()
    );
    assert_eq!(
        function
            .blocks
            .iter()
            .map(|block| block.id)
            .collect::<Vec<_>>(),
        vec![BlockId(0), BlockId(1), BlockId(2), BlockId(3)]
    );
    assert!(matches!(
        &function.blocks[0].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(1),
            ..
        })
    ));
    assert!(matches!(
        &function.blocks[1].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(2),
            ..
        })
    ));
    assert!(matches!(
        &function.blocks[2].terminator,
        SemTerminator::Branch {
            then_target: Edge {
                target: BlockId(1),
                ..
            },
            else_target: Edge {
                target: BlockId(3),
                ..
            },
            ..
        }
    ));
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
}

#[test]
fn dynamic_branch_is_a_byte_for_byte_noop() {
    let mut function = function(
        "dynamic_branch",
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }],
        ResolvedTy::I64,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Branch {
                    condition: read(0),
                    then_target: Edge {
                        target: BlockId(1),
                        args: Vec::new(),
                    },
                    else_target: Edge {
                        target: BlockId(2),
                        args: Vec::new(),
                    },
                },
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(0),
                    results: vec![value(1, ResolvedTy::I64)],
                    kind: SemOpKind::ConstI64(1),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(returned(1)),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: vec![SemOp {
                    id: OpId(1),
                    results: vec![value(2, ResolvedTy::I64)],
                    kind: SemOpKind::ConstI64(2),
                    provenance: Provenance::Synthesized,
                }],
                terminator: SemTerminator::Return {
                    value: Some(returned(2)),
                },
            },
        ],
    );
    let before = function.clone();
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report = canonicalize(&mut function).expect("dynamic CFG remains valid");
    assert_eq!(report.folded_branches, 0);
    assert!(report.removed_blocks.is_empty());
    assert_eq!(function, before);
}

#[test]
fn malformed_input_is_rejected_atomically() {
    let mut function = function(
        "missing_successor",
        Vec::new(),
        ResolvedTy::Unit,
        vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Goto(Edge {
                target: BlockId(1),
                args: Vec::new(),
            }),
        }],
    );
    let before = function.clone();

    assert!(matches!(
        canonicalize(&mut function),
        Err(SirOptimizationError::InvalidInput(_))
    ));
    assert_eq!(function, before);
}

#[test]
fn duplicate_block_identity_is_rejected_before_cfg_indexing_or_mutation() {
    let mut function = false_same_target_diamond();
    function.blocks[1].id = BlockId(0);
    let before = function.clone();

    let Err(SirOptimizationError::InvalidInput(diagnostics)) = canonicalize(&mut function) else {
        panic!("a duplicate block identity is malformed input");
    };
    assert!(diagnostics
        .iter()
        .any(|diagnostic| diagnostic.kind == SirDiagnosticKind::DuplicateBlock(BlockId(0))));
    assert_eq!(function, before);
}

#[test]
fn noncanonical_unique_block_order_is_rejected_atomically() {
    let mut function = function(
        "noncanonical_unique_blocks",
        Vec::new(),
        ResolvedTy::Unit,
        vec![
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
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
        ],
    );
    let before = function.clone();

    assert!(matches!(
        canonicalize(&mut function),
        Err(SirOptimizationError::InvalidInput(_))
    ));
    assert_eq!(function, before);
}

#[test]
fn compaction_canonicalizes_a_nonzero_entry_block() {
    let mut function = function(
        "nonzero_entry",
        Vec::new(),
        ResolvedTy::Unit,
        vec![
            SemBlock {
                id: BlockId(0),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Unreachable,
            },
            SemBlock {
                id: BlockId(1),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Return { value: None },
            },
            SemBlock {
                id: BlockId(2),
                args: Vec::new(),
                ops: Vec::new(),
                terminator: SemTerminator::Goto(Edge {
                    target: BlockId(1),
                    args: Vec::new(),
                }),
            },
        ],
    );
    function.entry = BlockId(2);
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());

    let report =
        canonicalize(&mut function).expect("a valid nonzero entry must canonicalize successfully");
    assert_eq!(report.folded_branches, 0);
    assert_eq!(report.removed_blocks, vec![BlockId(0)]);
    assert_eq!(
        report.block_remap,
        [(BlockId(1), BlockId(1)), (BlockId(2), BlockId(0))]
            .into_iter()
            .collect()
    );
    assert_eq!(function.entry, BlockId(0));
    assert!(matches!(
        &function.blocks[0].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(1),
            ..
        })
    ));
    assert!(verify_function_in_module(&module(function.clone()), &function).is_empty());
}

#[test]
fn module_canonicalization_keeps_callable_validation_and_is_atomic() {
    let mut module = module(false_same_target_diamond());
    assert!(verify_module(&module).is_empty());
    let reports = canonicalize_module_constant_cfg(&mut module)
        .expect("verified module must canonicalize as one transaction");
    assert_eq!(reports.len(), 1);
    assert_eq!(reports[0].0, CallableId(0));
    assert_eq!(reports[0].1.folded_branches, 1);
    assert!(verify_module(&module).is_empty());
}

#[test]
fn module_canonicalization_rejects_an_invalid_body_atomically() {
    let valid = false_same_target_diamond();
    let invalid = function(
        "invalid_module_body",
        Vec::new(),
        ResolvedTy::Unit,
        vec![SemBlock {
            id: BlockId(0),
            args: Vec::new(),
            ops: Vec::new(),
            terminator: SemTerminator::Goto(Edge {
                target: BlockId(1),
                args: Vec::new(),
            }),
        }],
    );
    let mut invalid = invalid;
    invalid.id = ItemId(1);
    invalid.callable = CallableId(1);

    let mut module = SemModule {
        callables: vec![callable_for(&valid), callable_for(&invalid)],
        generic_templates: Vec::new(),
        root_unit_callables: Vec::new(),
        entry_exit_plan: None,
        entry_callable: None,
        functions: vec![valid, invalid],
        aggregate_shapes: Vec::new(),
        type_facts: BTreeMap::new(),
        string_literals: BTreeMap::new(),
        bytes_literals: BTreeMap::new(),
    };
    let before = module.clone();

    assert!(matches!(
        canonicalize_module_constant_cfg(&mut module),
        Err(SirOptimizationError::InvalidInput(_))
    ));
    assert_eq!(module, before);
}
