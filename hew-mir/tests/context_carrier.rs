use hew_mir::{
    bracket_actor_handler_blocks, validate_context_markers, BasicBlock, FunctionCallConv, Instr,
    MirCheck, Place, RawMirFunction, Terminator, TrapKind,
};
use hew_types::ResolvedTy;

fn actor_handler(blocks: Vec<BasicBlock>) -> RawMirFunction {
    RawMirFunction {
        name: "handler".to_string(),
        return_ty: ResolvedTy::I64,
        call_conv: FunctionCallConv::ActorHandler,
        params: vec![],
        locals: vec![ResolvedTy::I64],
        blocks,
        decisions: vec![],
    }
}

#[test]
fn actor_handler_bracketing_inserts_entry_and_exit_markers() {
    let mut blocks = vec![
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::ConstI64 {
                dest: Place::Local(0),
                value: 1,
            }],
            terminator: Terminator::Goto { target: 1 },
        },
        BasicBlock {
            id: 1,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            }],
            terminator: Terminator::Return,
        },
        BasicBlock {
            id: 2,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Trap {
                kind: TrapKind::IntegerOverflow,
            },
        },
    ];

    bracket_actor_handler_blocks(&mut blocks);

    assert!(matches!(
        blocks[0].instructions.first(),
        Some(Instr::EnterContext)
    ));
    assert!(matches!(
        blocks[1].instructions.last(),
        Some(Instr::ExitContext)
    ));
    assert!(matches!(
        blocks[2].instructions.last(),
        Some(Instr::ExitContext)
    ));
}

#[test]
fn actor_handler_missing_enter_context_fails_mir_check() {
    let func = actor_handler(vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::ExitContext],
        terminator: Terminator::Return,
    }]);

    let checks = validate_context_markers(&func);

    assert!(
        checks.iter().any(|check| matches!(
            check,
            MirCheck::ContextBoundaryViolation {
                kind: "missing-enter-context",
                ..
            }
        )),
        "missing EnterContext must be a MIR check failure: {checks:?}"
    );
}

#[test]
fn actor_handler_return_without_exit_context_fails_mir_check() {
    let func = actor_handler(vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![Instr::EnterContext],
        terminator: Terminator::Return,
    }]);

    let checks = validate_context_markers(&func);

    assert!(
        checks.iter().any(|check| matches!(
            check,
            MirCheck::ContextBoundaryViolation {
                kind: "missing-exit-context",
                ..
            }
        )),
        "Return without ExitContext must be rejected: {checks:?}"
    );
}

#[test]
fn context_field_derived_place_cannot_flow_past_exit_context() {
    let blocks = vec![BasicBlock {
        id: 0,
        statements: vec![],
        instructions: vec![
            Instr::EnterContext,
            Instr::ContextField {
                dest: Place::Local(0),
                offset: 0,
            },
            Instr::ExitContext,
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(0),
            },
        ],
        terminator: Terminator::Return,
    }];

    let type_classes = hew_hir::TypeClassTable::default();
    let checks = hew_mir::dataflow::analyze(&blocks, &type_classes, &[]).checks;

    assert!(
        checks.iter().any(|check| matches!(
            check,
            MirCheck::ContextBindingEscapes {
                place: Place::Local(0),
                block: 0,
            }
        )),
        "context-derived place used after ExitContext must fail dataflow: {checks:?}"
    );
}
