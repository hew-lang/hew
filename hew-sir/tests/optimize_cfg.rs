use hew_hir::ItemId;
use hew_sir::{
    canonicalize_constant_cfg, canonicalize_module_constant_cfg, verify_function, verify_module,
    BlockArg, BlockId, CallableId, CallableInstance, CfgCanonicalizationReport, Edge,
    EffectSummary, FunctionSourceOrigin, OpId, Operand, Provenance, SemAbiParam, SemBlock,
    SemCallConv, SemCallable, SemCallableKind, SemFunction, SemModule, SemOp, SemOpKind,
    SemParamPassing, SemSignature, SemTerminator, SirOptimizationError, UseMode, ValueDef, ValueId,
};
use hew_types::{DefId, ResolvedTy};

fn read(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
        mode: UseMode::Read,
    }
}

fn value(id: u32, ty: ResolvedTy) -> ValueDef {
    ValueDef {
        id: ValueId(id),
        ty,
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
        effect_summary: EffectSummary::Unknown,
    }
}

fn module(function: SemFunction) -> SemModule {
    SemModule {
        callables: vec![callable_for(&function)],
        generic_templates: Vec::new(),
        root_unit_callables: Vec::new(),
        entry_callable: None,
        functions: vec![function],
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
    }
}

fn false_same_target_diamond() -> SemFunction {
    function(
        "false_same_target_diamond",
        vec![
            BlockArg {
                value: ValueId(0),
                ty: ResolvedTy::I64,
            },
            BlockArg {
                value: ValueId(1),
                ty: ResolvedTy::I64,
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
                    value: Some(read(3)),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![BlockArg {
                    value: ValueId(4),
                    ty: ResolvedTy::I64,
                }],
                ops: Vec::new(),
                terminator: SemTerminator::Return {
                    value: Some(read(4)),
                },
            },
        ],
    )
}

#[test]
fn false_branch_keeps_the_selected_duplicate_target_edge_and_remaps_blocks() {
    let mut function = false_same_target_diamond();
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function)
        .expect("verified SIR must canonicalize successfully");
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
    assert!(verify_function(&function).is_empty());
    assert_eq!(function.blocks.len(), 2);
    assert!(matches!(
        &function.blocks[0].terminator,
        SemTerminator::Goto(Edge { target: BlockId(1), args })
            if args == &vec![read(1)]
    ));
    assert_eq!(function.blocks[1].args[0].value, ValueId(4));
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
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function).expect("constant CFG must fold");
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
    assert!(verify_function(&function).is_empty());
}

#[test]
fn compaction_accepts_a_newly_dead_block_that_reads_an_entry_parameter() {
    let mut function = function(
        "dead_parameter_read",
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
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
                    value: Some(read(0)),
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
                    value: Some(read(0)),
                },
            },
        ],
    );
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function)
        .expect("dead blocks may retain entry-value uses until compaction");
    assert_eq!(report.removed_blocks, vec![BlockId(2)]);
    assert_eq!(function.blocks.len(), 2);
    assert!(verify_function(&function).is_empty());
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
    assert!(verify_function(&function).is_empty());

    canonicalize_constant_cfg(&mut function).expect("loop CFG must canonicalize");
    assert_eq!(function.blocks.len(), 2);
    assert!(matches!(
        &function.blocks[1].terminator,
        SemTerminator::Goto(Edge {
            target: BlockId(1),
            ..
        })
    ));
    assert!(verify_function(&function).is_empty());
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
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function)
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
    assert!(verify_function(&function).is_empty());
}

#[test]
fn dynamic_branch_is_a_byte_for_byte_noop() {
    let mut function = function(
        "dynamic_branch",
        vec![BlockArg {
            value: ValueId(0),
            ty: ResolvedTy::Bool,
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
                    value: Some(read(1)),
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
                    value: Some(read(2)),
                },
            },
        ],
    );
    let before = function.clone();
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function).expect("dynamic CFG remains valid");
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
        canonicalize_constant_cfg(&mut function),
        Err(SirOptimizationError::InvalidInput(_))
    ));
    assert_eq!(function, before);
}

#[test]
fn duplicate_block_identity_is_rejected_before_cfg_indexing_or_mutation() {
    let mut function = false_same_target_diamond();
    function.blocks[1].id = BlockId(0);
    let before = function.clone();

    assert!(matches!(
        canonicalize_constant_cfg(&mut function),
        Err(SirOptimizationError::InvalidInput(_))
    ));
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
        canonicalize_constant_cfg(&mut function),
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
    assert!(verify_function(&function).is_empty());

    let report = canonicalize_constant_cfg(&mut function)
        .expect("a valid nonzero entry must canonicalize successfully");
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
    assert!(verify_function(&function).is_empty());
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
        entry_callable: None,
        functions: vec![valid, invalid],
    };
    let before = module.clone();

    assert!(matches!(
        canonicalize_module_constant_cfg(&mut module),
        Err(SirOptimizationError::InvalidInput(_))
    ));
    assert_eq!(module, before);
}
