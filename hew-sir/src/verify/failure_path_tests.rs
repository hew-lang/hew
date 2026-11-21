use super::FailurePaths;
use crate::{
    BlockId, CallResult, Edge, Operand, Provenance, SemBlock, SemTerminator, SuspendKind,
    TaskScopeId, TaskScopeJoinMode, TrapKind, ValueId,
};

fn edge(id: u32) -> Edge {
    Edge {
        target: BlockId(id),
        args: Vec::new(),
    }
}

fn block(id: u32, terminator: SemTerminator) -> SemBlock {
    SemBlock {
        id: BlockId(id),
        args: Vec::new(),
        ops: Vec::new(),
        terminator,
        terminator_provenance: Provenance::Synthesized,
    }
}

fn branch(left: u32, right: u32) -> SemTerminator {
    SemTerminator::Branch {
        condition: Operand { value: ValueId(0) },
        then_target: edge(left),
        else_target: edge(right),
    }
}

#[test]
fn shared_cleanup_proofs_keep_trap_kinds_distinct_and_reject_cycles() {
    let overflow = TrapKind::IntegerOverflow;
    let blocks = [
        block(0, branch(1, 2)),
        block(1, SemTerminator::Goto(edge(3))),
        block(2, SemTerminator::Goto(edge(3))),
        block(3, SemTerminator::Trap { kind: overflow }),
        block(4, branch(5, 0)),
        block(5, SemTerminator::Goto(edge(4))),
        block(6, branch(0, 4)),
    ];
    let blocks = blocks.iter().map(|b| (b.id, b)).collect();
    let mut paths = FailurePaths::new(&blocks);
    // Reject the cycle first. Its unexplored sibling is a valid diamond.
    assert!(!paths.matches_exit(&edge(4), Some(overflow)));
    assert!(paths.matches_exit(&edge(0), Some(overflow)));
    assert!(!paths.matches_exit(&edge(6), Some(overflow)));
    assert!(!paths.matches_exit(&edge(0), Some(TrapKind::DivideByZero)));
    assert!(!paths.matches_exit(&edge(0), None));
}

#[test]
fn fault_dispatch_checks_only_fault_and_join_requires_every_outcome() {
    let join = |resume, cancel, unwind| SemTerminator::Suspend {
        kind: SuspendKind::Join {
            scope: TaskScopeId(0),
            mode: TaskScopeJoinMode::PropagateFault,
        },
        inputs: Vec::new(),
        result: CallResult::Unit,
        resumes: vec![edge(resume)],
        cancel: edge(cancel),
        unwind: edge(unwind),
    };
    let blocks = [
        block(
            0,
            SemTerminator::CleanupDispatch {
                normal: edge(1),
                fault: edge(2),
            },
        ),
        block(1, SemTerminator::Return { value: None }),
        block(2, SemTerminator::ResumeUnwind { handback: None }),
        block(3, join(0, 0, 0)),
        block(4, join(5, 0, 0)),
        block(
            5,
            SemTerminator::Trap {
                kind: TrapKind::HeapExceeded,
            },
        ),
        block(6, join(0, 5, 0)),
        block(7, join(0, 0, 5)),
    ];
    let blocks = blocks.iter().map(|b| (b.id, b)).collect();
    let mut paths = FailurePaths::new(&blocks);
    assert!(paths.matches_exit(&edge(3), None));
    assert!(!paths.matches_exit(&edge(0), Some(TrapKind::HeapExceeded)));
    for id in [4, 6, 7] {
        assert!(!paths.matches_exit(&edge(id), None));
    }
}

#[test]
fn a_deep_cleanup_ladder_reaches_its_exit_without_native_recursion() {
    let depth = 16_384;
    let mut blocks: Vec<_> = (0..depth)
        .map(|id| block(id, SemTerminator::Goto(edge(id + 1))))
        .collect();
    blocks.push(block(depth, SemTerminator::ResumeUnwind { handback: None }));
    let blocks = blocks.iter().map(|b| (b.id, b)).collect();
    let mut paths = FailurePaths::new(&blocks);
    for id in 0..depth {
        assert!(paths.matches_exit(&edge(id), None));
    }
}
