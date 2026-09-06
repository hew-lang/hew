//! Source-independent Local bodies retaining checked source types and callables.

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_sir::*;
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

pub fn source(text: &str) -> SemModule {
    let parsed = hew_parser::parse(text);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&parsed.program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&parsed.program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = lower_module_with_demand(&hir.module, &checked, SirLoweringDemand::EveryCallable);
    assert!(
        lowered
            .statuses
            .iter()
            .filter(|entry| matches!(entry.name.as_str(), "probe" | "make" | "main"))
            .all(|entry| matches!(entry.status, SirLoweringStatus::Lowered)),
        "{:?}",
        lowered.statuses
    );
    check_module(&lowered.module).unwrap();
    lowered.module
}

pub fn probe(module: &mut SemModule) -> &mut SemFunction {
    module
        .functions
        .iter_mut()
        .find(|function| function.name == "probe")
        .unwrap()
}

pub fn operand(value: u32) -> Operand {
    Operand {
        value: ValueId(value),
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
pub fn alloc() -> SemOp {
    op(SemOpKind::AllocPlace { place: PlaceId(0) })
}
pub fn init(value: u32) -> SemOp {
    op(SemOpKind::StoreInit {
        place: PlaceId(0),
        value: operand(value),
    })
}
pub fn assign(value: u32) -> SemOp {
    op(SemOpKind::StoreAssign {
        place: PlaceId(0),
        value: operand(value),
    })
}
pub fn end() -> SemOp {
    op(SemOpKind::EndLifetime { place: PlaceId(0) })
}
pub fn destroy(value: u32) -> SemOp {
    op(SemOpKind::DestroyValue {
        value: operand(value),
    })
}
pub fn load(kind: SemOpKind, id: u32, ty: ResolvedTy, own: OwnKind) -> SemOp {
    SemOp {
        results: vec![ValueDef {
            id: ValueId(id),
            ty,
            own,
        }],
        ..op(kind)
    }
}
pub fn edge(target: u32, args: &[u32]) -> Edge {
    Edge {
        target: BlockId(target),
        args: args.iter().copied().map(operand).collect(),
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
pub fn done() -> SemTerminator {
    SemTerminator::Return { value: None }
}
pub fn branch(flag: u32, yes: u32, no: u32) -> SemTerminator {
    SemTerminator::Branch {
        condition: operand(flag),
        then_target: edge(yes, &[]),
        else_target: edge(no, &[]),
    }
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
                SemTerminator::IndirectCall { id, .. } | SemTerminator::RtCall { id, .. } => {
                    *id = OpId(next);
                    next += 1;
                }
                _ => {}
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Case {
    JoinEnd,
    JoinAssign,
    Borrow,
    EmptyLoop,
    ZeroSized,
    LinearTrap,
}

#[allow(
    clippy::too_many_lines,
    reason = "explicit SIR blocks make each storage transition visible in the fixture"
)]
pub fn module(case: Case) -> SemModule {
    let mut module = source(match case {
        Case::ZeroSized => "type Empty {} fn probe(owner: Empty, flag: bool) {} fn main() {}",
        Case::LinearTrap => "type Token { payload: string } fn probe(consume owner: Token, flag: bool) {} fn main() {}",
        _ => "fn probe(consume owner: string, flag: bool) {} fn main() {}",
    });
    let function = probe(&mut module);
    let ty = function.params[0].ty.clone();
    function.bindings.clear();
    function.places = vec![PlaceDecl {
        id: PlaceId(0),
        ty: ty.clone(),
        origin: PlaceOrigin::Local,
    }];
    let take = |value| {
        load(
            SemOpKind::LoadTake { place: PlaceId(0) },
            value,
            ty.clone(),
            if matches!(case, Case::ZeroSized) {
                OwnKind::None
            } else {
                OwnKind::Owned
            },
        )
    };
    function.blocks = match case {
        Case::JoinEnd | Case::JoinAssign => {
            let mut initial = vec![alloc(), init(0)];
            let mut final_ops = vec![];
            if matches!(case, Case::JoinAssign) {
                initial.push(load(
                    SemOpKind::LoadCopy { place: PlaceId(0) },
                    2,
                    ty.clone(),
                    OwnKind::Owned,
                ));
                final_ops.push(assign(2));
            }
            final_ops.push(end());
            vec![
                block(0, initial, branch(1, 1, 2)),
                block(
                    1,
                    vec![take(3), destroy(3)],
                    SemTerminator::Goto(edge(2, &[])),
                ),
                block(2, final_ops, done()),
            ]
        }
        Case::Borrow => vec![block(
            0,
            vec![
                alloc(),
                init(0),
                load(
                    SemOpKind::LoadBorrow { place: PlaceId(0) },
                    2,
                    ty.clone(),
                    OwnKind::Guaranteed,
                ),
                load(
                    SemOpKind::BeginBorrow { owner: operand(2) },
                    3,
                    ty.clone(),
                    OwnKind::Guaranteed,
                ),
                op(SemOpKind::EndBorrow { borrow: operand(3) }),
                op(SemOpKind::EndBorrow { borrow: operand(2) }),
                end(),
            ],
            done(),
        )],
        Case::EmptyLoop => vec![
            block(0, vec![destroy(0)], SemTerminator::Goto(edge(1, &[1]))),
            SemBlock {
                args: vec![BlockArg {
                    value: ValueId(2),
                    ty: ResolvedTy::Bool,
                    own: OwnKind::None,
                }],
                ..block(1, vec![alloc(), end()], branch(2, 3, 2))
            },
            block(2, vec![], done()),
            block(
                3,
                vec![load(
                    SemOpKind::ConstBool(false),
                    3,
                    ResolvedTy::Bool,
                    OwnKind::None,
                )],
                SemTerminator::Goto(edge(1, &[3])),
            ),
        ],
        Case::ZeroSized => vec![block(
            0,
            vec![alloc(), init(0), take(2), init(2), end()],
            done(),
        )],
        Case::LinearTrap => vec![block(
            0,
            vec![alloc(), init(0), end()],
            SemTerminator::Trap {
                kind: TrapKind::IndexOutOfBounds,
            },
        )],
    };
    if matches!(case, Case::LinearTrap) {
        let row = module
            .type_facts
            .get_mut(&hew_types::TypeInstanceKey(ty.clone()))
            .unwrap();
        row.class = hew_types::ValueClass::Linear;
        row.clone = hew_types::CloneKind::None;
        module
            .aggregate_shapes
            .iter_mut()
            .find(|shape| shape.aggregate_ty == ty)
            .unwrap()
            .marker = hew_types::DeclarationMarker::Linear;
    }
    normalize(&mut module);
    check_module(&module).unwrap();
    module
}
