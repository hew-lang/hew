//! Inspect checked continuations without treating a normal block argument as
//! another copy, or treating the failure path as a successful materialization.
#![allow(
    dead_code,
    reason = "Shared test support serves independently compiled integration tests"
)]

use hew_sir::{SemBlock, SemFunction, SemTerminator, ValueId};

pub fn source(function: &SemFunction, mut value: ValueId) -> ValueId {
    for _ in 0..function.blocks.len() {
        let Some((block, index)) = function.blocks.iter().find_map(|block| {
            block
                .args
                .iter()
                .position(|arg| arg.value == value)
                .map(|index| (block, index))
        }) else {
            return value;
        };
        let incoming: Vec<_> = function
            .blocks
            .iter()
            .filter_map(|predecessor| {
                let edge = match &predecessor.terminator {
                    SemTerminator::Materialize { normal, .. } => normal,
                    SemTerminator::Goto(edge) => edge,
                    _ => return None,
                };
                (edge.target == block.id).then(|| edge.args[index].value)
            })
            .collect();
        assert_eq!(incoming.len(), 1, "expected one forwarding continuation");
        value = incoming[0];
    }
    panic!("cyclic forwarding continuation")
}

pub fn normal_path(function: &SemFunction, mut block: hew_sir::BlockId) -> Vec<&SemBlock> {
    let mut path = Vec::new();
    for _ in 0..function.blocks.len() {
        let current = function
            .blocks
            .iter()
            .find(|candidate| candidate.id == block)
            .unwrap();
        path.push(current);
        block = match &current.terminator {
            SemTerminator::Materialize { normal, .. } => normal.target,
            _ => return path,
        };
    }
    panic!("cyclic materialization continuation")
}
