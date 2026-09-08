//! Hand-built inline deferred bodies using checked callable and type facts.

use hew_sir::*;
use hew_types::ResolvedTy;

pub fn operand(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
    }
}
pub fn edge(block: u32) -> Edge {
    Edge {
        target: BlockId(block),
        args: vec![],
    }
}
pub fn op(kind: SemOpKind) -> SemOp {
    SemOp {
        id: OpId(0),
        kind,
        results: vec![],
        provenance: Provenance::Synthesized,
    }
}
pub fn value(kind: SemOpKind, id: u32, ty: ResolvedTy, own: OwnKind) -> SemOp {
    SemOp {
        results: vec![ValueDef {
            id: ValueId(id),
            ty,
            own,
        }],
        ..op(kind)
    }
}
pub fn integer(kind: SemOpKind, id: u32) -> SemOp {
    value(kind, id, ResolvedTy::I64, OwnKind::None)
}
pub fn boundary(value: ValueId, decision: BoundaryDecision) -> BoundaryOperand {
    BoundaryOperand {
        operand: Operand { value },
        decision,
    }
}
pub fn block(id: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
    SemBlock {
        id: BlockId(id),
        args: vec![],
        ops,
        terminator,
    }
}
pub fn probe(module: &mut SemModule) -> &mut SemFunction {
    module
        .functions
        .iter_mut()
        .find(|f| f.declaration.full_path() == "probe")
        .unwrap()
}

pub fn normalize(module: &mut SemModule) {
    for function in &mut module.functions {
        let mut next = 0;
        for block in &mut function.blocks {
            for op in &mut block.ops {
                op.id = OpId(next);
                next += 1;
            }
            match &mut block.terminator {
                SemTerminator::CheckedBinary { id, .. }
                | SemTerminator::Call { id, .. }
                | SemTerminator::RtCall { id, .. }
                | SemTerminator::ValueCall { id, .. }
                | SemTerminator::IndirectCall { id, .. }
                | SemTerminator::SwitchVariant { id, .. } => {
                    *id = OpId(next);
                    next += 1;
                }
                _ => {}
            }
        }
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "the complete two-defer CFG is the native execution oracle"
)]
pub fn module(failing: bool) -> SemModule {
    let source = "fn fail(message: string) { panic(message); } fn probe(existing: bool, body: string, older: string, newer: string) -> i64 { 4 } fn main() -> i64 { probe(false, \"body\", \"D1\", \"D2\") }";
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]))
        .check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir =
        hew_hir::lower_program_host_target(&parsed.program, &checked, &hew_hir::ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let mut module =
        lower_module_with_demand(&hir.module, &checked, SirLoweringDemand::EveryCallable).module;
    let fail = module
        .callables
        .iter()
        .find(|c| c.declaration.full_path() == "fail")
        .unwrap()
        .id;
    let function = probe(&mut module);
    let existing = function.params[0].value;
    let body = function.params[1].value;
    let older = function.params[2].value;
    let newer = function.params[3].value;
    function.entry = BlockId(0);
    function.bindings.clear();
    function.places = vec![
        PlaceDecl {
            id: PlaceId(0),
            ty: ResolvedTy::I64,
            origin: PlaceOrigin::Local,
        },
        PlaceDecl {
            id: PlaceId(1),
            ty: ResolvedTy::I64,
            origin: PlaceOrigin::Local,
        },
    ];
    let finish = |defer, next| SemTerminator::FinishDefer {
        defer: DeferId(defer),
        park: FaultParkId(0),
        next: edge(next),
    };
    function.blocks = vec![
        block(
            0,
            vec![
                op(SemOpKind::AllocPlace { place: PlaceId(0) }),
                op(SemOpKind::AllocPlace { place: PlaceId(1) }),
                integer(SemOpKind::ConstInteger(4), 10),
                op(SemOpKind::StoreInit {
                    place: PlaceId(0),
                    value: operand(10),
                }),
                op(SemOpKind::RegisterDefer {
                    defer: DeferId(1),
                    scope: DeferScopeId(0),
                    dependencies: vec![PlaceId(0)],
                }),
                op(SemOpKind::RegisterDefer {
                    defer: DeferId(2),
                    scope: DeferScopeId(0),
                    dependencies: vec![PlaceId(0)],
                }),
            ],
            SemTerminator::Branch {
                condition: Operand { value: existing },
                then_target: edge(1),
                else_target: edge(2),
            },
        ),
        block(
            1,
            vec![],
            SemTerminator::Panic {
                message: boundary(body, BoundaryDecision::Borrow),
                cleanup: edge(3),
            },
        ),
        block(
            2,
            vec![op(SemOpKind::StoreInit {
                place: PlaceId(1),
                value: operand(10),
            })],
            SemTerminator::Goto(edge(3)),
        ),
        block(
            3,
            vec![],
            SemTerminator::EnterDefer {
                defer: DeferId(2),
                park: FaultParkId(0),
                body: edge(4),
            },
        ),
        block(
            4,
            vec![
                integer(SemOpKind::LoadCopy { place: PlaceId(0) }, 11),
                integer(SemOpKind::ConstInteger(1), 12),
            ],
            SemTerminator::CheckedBinary {
                id: OpId(0),
                op: hew_parser::ast::BinaryOp::Add,
                lhs: operand(11),
                rhs: operand(12),
                result: ValueDef {
                    id: ValueId(13),
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                },
                normal: Edge {
                    target: BlockId(5),
                    args: vec![operand(13)],
                },
                failures: vec![CheckedFailure {
                    kind: TrapKind::IntegerOverflow,
                    edge: edge(15),
                }],
            },
        ),
        block(
            5,
            vec![op(SemOpKind::StoreAssign {
                place: PlaceId(0),
                value: operand(14),
            })],
            if failing {
                SemTerminator::Call {
                    id: OpId(0),
                    callee: fail,
                    args: vec![boundary(newer, BoundaryDecision::Borrow)],
                    result: CallResult::Unit,
                    normal: Some(edge(6)),
                    unwind: CallUnwind::Cleanup(edge(7)),
                }
            } else {
                SemTerminator::Goto(edge(7))
            },
        ),
        // The write after a failed call must not execute. Its constant is the
        // fixture's known x + 100; the earlier increment is actual checked CFG.
        block(
            6,
            vec![
                integer(SemOpKind::ConstInteger(105), 16),
                op(SemOpKind::StoreAssign {
                    place: PlaceId(0),
                    value: operand(16),
                }),
            ],
            SemTerminator::Goto(edge(7)),
        ),
        block(7, vec![], finish(2, 8)),
        block(
            8,
            vec![],
            SemTerminator::EnterDefer {
                defer: DeferId(1),
                park: FaultParkId(0),
                body: edge(9),
            },
        ),
        block(
            9,
            vec![integer(SemOpKind::LoadCopy { place: PlaceId(0) }, 17)],
            SemTerminator::RtCall {
                id: OpId(0),
                family: hew_types::RuntimeCallFamily::Print {
                    kind: hew_types::runtime_call::PrintKind::I64,
                    newline: true,
                },
                args: vec![boundary(ValueId(17), BoundaryDecision::Copy)],
                result: CallResult::Unit,
                normal: edge(10),
                unwind: CallUnwind::NotApplicable,
            },
        ),
        block(
            10,
            vec![],
            if failing {
                SemTerminator::Panic {
                    message: boundary(older, BoundaryDecision::Borrow),
                    cleanup: edge(11),
                }
            } else {
                SemTerminator::Goto(edge(11))
            },
        ),
        block(11, vec![], finish(1, 12)),
        block(
            12,
            vec![op(SemOpKind::EndLifetime { place: PlaceId(0) })],
            SemTerminator::CleanupDispatch {
                normal: edge(13),
                fault: edge(14),
            },
        ),
        block(
            13,
            vec![
                integer(SemOpKind::LoadTake { place: PlaceId(1) }, 18),
                op(SemOpKind::EndLifetime { place: PlaceId(1) }),
            ],
            SemTerminator::Return {
                value: Some(boundary(ValueId(18), BoundaryDecision::Borrow)),
            },
        ),
        block(
            14,
            vec![op(SemOpKind::EndLifetime { place: PlaceId(1) })],
            SemTerminator::ResumeUnwind,
        ),
        block(
            15,
            vec![],
            SemTerminator::CheckedRaiseFault {
                kind: TrapKind::IntegerOverflow,
                cleanup: edge(7),
            },
        ),
    ];
    function.blocks[5].args.push(BlockArg {
        value: ValueId(14),
        ty: ResolvedTy::I64,
        own: OwnKind::None,
    });
    normalize(&mut module);
    module
}

pub fn nested(failing: bool) -> SemModule {
    let mut module = module(failing);
    let function = probe(&mut module);
    let newer = function.params[3].value;
    let after_nested = function.blocks[5].terminator.clone();
    function.places.push(PlaceDecl {
        id: PlaceId(2),
        ty: ResolvedTy::String,
        origin: PlaceOrigin::Local,
    });
    function.blocks[5].ops.push(op(SemOpKind::RegisterDefer {
        defer: DeferId(3),
        scope: DeferScopeId(1),
        dependencies: vec![],
    }));
    function.blocks[5].terminator = SemTerminator::Goto(edge(16));
    function.blocks.extend([
        block(
            16,
            vec![],
            SemTerminator::EnterDefer {
                defer: DeferId(3),
                park: FaultParkId(1),
                body: edge(17),
            },
        ),
        block(
            17,
            vec![
                op(SemOpKind::AllocPlace { place: PlaceId(2) }),
                value(
                    SemOpKind::CopyValue {
                        source: Operand { value: newer },
                    },
                    60,
                    ResolvedTy::String,
                    OwnKind::Owned,
                ),
                op(SemOpKind::StoreInit {
                    place: PlaceId(2),
                    value: operand(60),
                }),
                value(
                    SemOpKind::LoadBorrow { place: PlaceId(2) },
                    61,
                    ResolvedTy::String,
                    OwnKind::Guaranteed,
                ),
            ],
            if failing {
                SemTerminator::Panic {
                    message: boundary(ValueId(61), BoundaryDecision::Borrow),
                    cleanup: edge(18),
                }
            } else {
                SemTerminator::Goto(edge(18))
            },
        ),
        block(
            18,
            vec![
                op(SemOpKind::EndBorrow {
                    borrow: operand(61),
                }),
                op(SemOpKind::EndLifetime { place: PlaceId(2) }),
            ],
            SemTerminator::FinishDefer {
                defer: DeferId(3),
                park: FaultParkId(1),
                next: edge(19),
            },
        ),
        block(
            19,
            vec![],
            SemTerminator::CleanupDispatch {
                normal: edge(20),
                fault: edge(7),
            },
        ),
        block(20, vec![], after_nested),
    ]);
    normalize(&mut module);
    module
}
