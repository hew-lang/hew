//! Borrowed selection independent of the source select producer.

use hew_sir::*;
use hew_types::{ResolvedTy, TypeFactContext, TypeFactService};
use std::collections::BTreeMap;

#[allow(
    clippy::too_many_lines,
    reason = "one fixture exposes every typed selection edge"
)]
pub fn module(task_count: u32, has_timeout: bool) -> SemModule {
    let task_ty = ResolvedTy::Task(Box::new(ResolvedTy::I64));
    let mut params = Vec::new();
    let mut abi = Vec::new();
    let mut inputs = Vec::new();
    for index in 0..task_count + u32::from(has_timeout) {
        let task = index < task_count;
        let ty = if task {
            task_ty.clone()
        } else {
            ResolvedTy::Duration
        };
        params.push(BlockArg {
            value: ValueId(index),
            ty: ty.clone(),
            own: if task {
                OwnKind::Guaranteed
            } else {
                OwnKind::None
            },
        });
        abi.push(SemAbiParam {
            ty,
            passing: if task {
                SemParamPassing::Borrow
            } else {
                SemParamPassing::ReadOnly
            },
            caller_visible_projection: false,
        });
        inputs.push(BoundaryOperand {
            operand: Operand {
                value: ValueId(index),
            },
            decision: if task {
                BoundaryDecision::Borrow
            } else {
                BoundaryDecision::Copy
            },
        });
    }
    let raw = ValueId(u32::try_from(params.len()).unwrap());
    let selected = ValueId(raw.0 + 1);
    let callable = SemCallable {
        id: CallableId(0),
        function: hew_hir::ItemId(0),
        declaration: hew_types::DefId::for_test("select_probe"),
        instance: CallableInstance::Monomorphic,
        symbol: "select_probe".into(),
        source_origin: FunctionSourceOrigin::Unknown,
        signature: SemSignature {
            params: abi,
            return_ty: ResolvedTy::I64,
        },
        call_conv: SemCallConv::Default,
        kind: SemCallableKind::HewDirect,
    };
    let edge = |target| Edge {
        target: BlockId(target),
        args: vec![],
    };
    let function = SemFunction {
        id: callable.function,
        callable: callable.id,
        declaration: callable.declaration.clone(),
        name: callable.symbol.clone(),
        span: 0..0,
        source_origin: callable.source_origin.clone(),
        terminal_receiver: None,
        params,
        return_ty: ResolvedTy::I64,
        entry: BlockId(0),
        places: vec![],
        bindings: vec![],
        blocks: vec![
            SemBlock {
                id: BlockId(0),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::Suspend {
                    kind: SuspendKind::Select {
                        has_timeout,
                        order: hew_sir::TaskSelectionOrder::Source,
                    },
                    inputs,
                    result: CallResult::Value(ValueDef {
                        id: raw,
                        ty: ResolvedTy::I64,
                        own: OwnKind::None,
                    }),
                    resumes: vec![Edge {
                        target: BlockId(1),
                        args: vec![Operand { value: raw }],
                    }],
                    cancel: edge(2),
                    unwind: edge(3),
                },
            },
            SemBlock {
                id: BlockId(1),
                args: vec![BlockArg {
                    value: selected,
                    ty: ResolvedTy::I64,
                    own: OwnKind::None,
                }],
                ops: vec![],
                terminator: SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: Operand { value: selected },
                        decision: BoundaryDecision::Copy,
                    }),
                },
            },
            SemBlock {
                id: BlockId(2),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::ResumeUnwind,
            },
            SemBlock {
                id: BlockId(3),
                args: vec![],
                ops: vec![],
                terminator: SemTerminator::ResumeUnwind,
            },
        ],
    };
    let mut facts = TypeFactService::new(TypeFactContext::default(), BTreeMap::new());
    facts.require(&ResolvedTy::I64).unwrap();
    if has_timeout {
        facts.require(&ResolvedTy::Duration).unwrap();
    }
    let mut resources = BTreeMap::new();
    if task_count != 0 {
        facts.require(&task_ty).unwrap();
        resources.insert(task_ty, ResourceRelease::Task);
    }
    SemModule {
        regex_patterns: Vec::new(),
        callables: vec![callable],
        functions: vec![function],
        type_facts: facts.rows().clone(),
        resources,
        ..SemModule::default()
    }
}
