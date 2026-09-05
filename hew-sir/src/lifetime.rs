//! Path-sensitive availability of owned SSA obligations.
//!
//! Guaranteed inputs can be read or explicitly copied, never consumed or
//! escaped. This is not local borrow-region or place-initialization verification;
//! those producers remain closed until their checks and lowering exist.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use crate::{
    BlockId, BoundaryDecision, CallUnwind, Edge, OwnKind, SemFunction, SemOpKind, SemTerminator,
    SnapshotDecision, ValueId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Violation {
    pub block: BlockId,
    pub value: ValueId,
    pub reason: &'static str,
}

// A set of possible availability states. Joins union possibilities; a use
// requires LIVE alone, while a definition requires DEAD alone. Definitions
// reset availability, so a loop-local SSA definition creates a fresh dynamic
// obligation rather than inheriting the previous iteration's consumption.
const DEAD: u8 = 1;
const LIVE: u8 = 2;
type State = Vec<u8>;

pub(crate) fn verify(function: &SemFunction) -> Vec<Violation> {
    let flow = Flow::new(function);
    if (flow.values.is_empty() && flow.guaranteed.is_empty())
        || !flow.blocks.contains_key(&function.entry)
    {
        return Vec::new();
    }
    let mut initial = vec![DEAD; flow.values.len()];
    for param in &function.params {
        if let Some(&index) = flow.indices.get(&param.value) {
            initial[index] = LIVE;
        }
    }
    let mut incoming = BTreeMap::from([(function.entry, initial)]);
    let mut queue = VecDeque::from([function.entry]);
    let mut queued = BTreeSet::from([function.entry]);
    while let Some(block) = queue.pop_front() {
        queued.remove(&block);
        for (target, state) in flow.block(block, incoming[&block].clone(), &mut |_| {}) {
            let changed = if let Some(previous) = incoming.get_mut(&target) {
                let mut changed = false;
                for (before, after) in previous.iter_mut().zip(state) {
                    let joined = *before | after;
                    changed |= joined != *before;
                    *before = joined;
                }
                changed
            } else {
                incoming.insert(target, state);
                true
            };
            if changed && queued.insert(target) {
                queue.push_back(target);
            }
        }
    }
    // Diagnose the fixed point, not a transient partial predecessor set.
    let mut violations = Vec::new();
    for (block, state) in incoming {
        flow.block(block, state, &mut |v| violations.push(v));
    }
    violations
}

struct Flow<'a> {
    blocks: BTreeMap<BlockId, &'a crate::SemBlock>,
    indices: BTreeMap<ValueId, usize>,
    values: Vec<ValueId>,
    guaranteed: BTreeSet<ValueId>,
}

impl<'a> Flow<'a> {
    fn new(function: &'a SemFunction) -> Self {
        let mut values = BTreeSet::new();
        let mut guaranteed = BTreeSet::new();
        let mut record = |value, own| {
            if own == OwnKind::Owned {
                values.insert(value);
            } else if own == OwnKind::Guaranteed {
                guaranteed.insert(value);
            }
        };
        for param in &function.params {
            record(param.value, param.own);
        }
        for block in &function.blocks {
            for arg in &block.args {
                record(arg.value, arg.own);
            }
            for op in &block.ops {
                for result in &op.results {
                    record(result.id, result.own);
                }
            }
            block
                .terminator
                .visit_results(|value| record(value.id, value.own));
        }
        let values: Vec<_> = values.into_iter().collect();
        Self {
            blocks: function
                .blocks
                .iter()
                .map(|block| (block.id, block))
                .collect(),
            indices: values
                .iter()
                .enumerate()
                .map(|(index, &value)| (value, index))
                .collect(),
            values,
            guaranteed,
        }
    }

    fn access(
        &self,
        block: BlockId,
        value: ValueId,
        consume: bool,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        if consume && self.guaranteed.contains(&value) {
            emit(Violation {
                block,
                value,
                reason: "guaranteed input cannot be consumed; copy it into an owned value first",
            });
            return;
        }
        let Some(&index) = self.indices.get(&value) else {
            return;
        };
        if state[index] != LIVE {
            emit(Violation {
                block,
                value,
                reason: "owned value is not live on every incoming path",
            });
        }
        if consume {
            state[index] = DEAD;
        }
    }

    fn define(
        &self,
        block: BlockId,
        value: ValueId,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        let Some(&index) = self.indices.get(&value) else {
            return;
        };
        if state[index] != DEAD {
            emit(Violation {
                block,
                value,
                reason: "previous dynamic owner remains live at definition",
            });
        }
        state[index] = LIVE;
    }

    fn edge(
        &self,
        from: BlockId,
        edge: &Edge,
        mut state: State,
        emit: &mut impl FnMut(Violation),
    ) -> Option<(BlockId, State)> {
        let target = self.blocks.get(&edge.target)?;
        // All sources transfer first: a loop edge can pass its own block
        // argument back to itself or permute several owning arguments.
        for argument in &edge.args {
            self.access(from, argument.value, true, &mut state, emit);
        }
        for argument in &target.args {
            self.define(edge.target, argument.value, &mut state, emit);
        }
        Some((edge.target, state))
    }

    fn block(
        &self,
        id: BlockId,
        mut state: State,
        emit: &mut impl FnMut(Violation),
    ) -> Vec<(BlockId, State)> {
        let block = self.blocks[&id];
        for op in &block.ops {
            let consumes = operation_consumes_operands(&op.kind);
            op.visit_operands(|_, operand| {
                self.access(id, operand.value, consumes, &mut state, emit);
            });
            for result in &op.results {
                self.define(id, result.id, &mut state, emit);
            }
        }
        self.boundary_inputs(id, &block.terminator, &mut state, emit);
        let mut successors = Vec::new();
        match &block.terminator {
            SemTerminator::Call { normal, unwind, .. }
            | SemTerminator::RtCall { normal, unwind, .. } => {
                let mut returned = state.clone();
                block
                    .terminator
                    .visit_results(|result| self.define(id, result.id, &mut returned, emit));
                successors.extend(self.edge(id, normal, returned, emit));
                if let CallUnwind::Cleanup(edge) = unwind {
                    successors.extend(self.edge(id, edge, state, emit));
                }
            }
            SemTerminator::CheckedBinary {
                lhs,
                rhs,
                result,
                normal,
                failures,
                ..
            } => successors
                .extend(self.checked_binary(id, lhs, rhs, result, normal, failures, state, emit)),
            SemTerminator::Goto(edge) => successors.extend(self.edge(id, edge, state, emit)),
            SemTerminator::Branch {
                condition,
                then_target,
                else_target,
            } => {
                self.access(id, condition.value, false, &mut state, emit);
                successors.extend(self.edge(id, then_target, state.clone(), emit));
                successors.extend(self.edge(id, else_target, state, emit));
            }
            SemTerminator::SwitchVariant {
                scrutinee, arms, ..
            } => successors.extend(self.variant_switch(id, scrutinee, arms, &state, emit)),
            SemTerminator::Suspend {
                resumes, cancel, ..
            } => {
                for edge in resumes.iter().chain(std::iter::once(cancel)) {
                    successors.extend(self.edge(id, edge, state.clone(), emit));
                }
            }
            SemTerminator::Return { .. }
            | SemTerminator::ResumeUnwind
            | SemTerminator::Trap { .. }
            | SemTerminator::Unreachable => {
                for (index, &value) in self.values.iter().enumerate() {
                    if state[index] & LIVE != 0 {
                        emit(Violation {
                            block: id,
                            value,
                            reason: "owned value remains live at exit",
                        });
                    }
                }
            }
        }
        successors
    }

    fn variant_switch(
        &self,
        id: BlockId,
        scrutinee: &crate::Operand,
        arms: &[crate::SemVariantArm],
        state: &State,
        emit: &mut impl FnMut(Violation),
    ) -> Vec<(BlockId, State)> {
        let mut successors = Vec::with_capacity(arms.len());
        for arm in arms {
            let mut arm_state = state.clone();
            self.access(id, scrutinee.value, true, &mut arm_state, emit);
            for field in &arm.fields {
                self.define(id, field.id, &mut arm_state, emit);
            }
            successors.extend(self.edge(id, &arm.target, arm_state, emit));
        }
        successors
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "the transfer receives one closed checked-binary terminator shape"
    )]
    fn checked_binary(
        &self,
        id: BlockId,
        lhs: &crate::Operand,
        rhs: &crate::Operand,
        result: &crate::ValueDef,
        normal: &Edge,
        failures: &[crate::CheckedFailure],
        mut state: State,
        emit: &mut impl FnMut(Violation),
    ) -> Vec<(BlockId, State)> {
        self.access(id, lhs.value, false, &mut state, emit);
        self.access(id, rhs.value, false, &mut state, emit);
        let mut succeeded = state.clone();
        self.define(id, result.id, &mut succeeded, emit);
        let mut successors = Vec::new();
        successors.extend(self.edge(id, normal, succeeded, emit));
        for failure in failures {
            successors.extend(self.edge(id, &failure.edge, state.clone(), emit));
        }
        successors
    }

    fn boundary_inputs(
        &self,
        id: BlockId,
        terminator: &SemTerminator,
        state: &mut State,
        emit: &mut impl FnMut(Violation),
    ) {
        terminator.visit_boundary_operands(|_, operand| {
            let value = operand.operand.value;
            if self.guaranteed.contains(&value) {
                // The call contract must separately prove a synchronous,
                // non-retaining borrow. Other boundaries require an explicit
                // owned copy so their lifetime never depends on this input.
                let scoped_call_borrow = matches!(
                    terminator,
                    SemTerminator::Call { .. } | SemTerminator::RtCall { .. }
                ) && operand.decision == BoundaryDecision::Borrow;
                if !scoped_call_borrow {
                    emit(Violation {
                        block: id,
                        value,
                        reason: "guaranteed input requires an explicit owned copy at this boundary",
                    });
                }
                return;
            }
            let consumes = matches!(
                operand.decision,
                BoundaryDecision::Move | BoundaryDecision::Snapshot(SnapshotDecision::Transfer)
            );
            self.access(id, value, consumes, state, emit);
        });
    }
}

fn operation_consumes_operands(kind: &SemOpKind) -> bool {
    matches!(
        kind,
        SemOpKind::TupleMake { .. }
            | SemOpKind::AggregateMake { .. }
            | SemOpKind::VariantMake { .. }
            | SemOpKind::DestroyValue { .. }
            | SemOpKind::Move { .. }
            | SemOpKind::Fork { .. }
            | SemOpKind::Destructure { .. }
            | SemOpKind::StoreInit { .. }
            | SemOpKind::StoreAssign { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::verify;
    use crate::{
        BlockArg, BlockId, BoundaryDecision, BoundaryOperand, CallResult, CallUnwind, CallableId,
        Edge, FunctionSourceOrigin, OpId, Operand, OwnKind, Provenance, SemBlock, SemFunction,
        SemOp, SemOpKind, SemTerminator, StringLiteralId, ValueDef, ValueId,
    };
    use hew_hir::ItemId;
    use hew_types::{DefId, ResolvedTy};

    fn operand(id: u32) -> Operand {
        Operand { value: ValueId(id) }
    }

    fn owned(id: u32) -> ValueDef {
        ValueDef {
            id: ValueId(id),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        }
    }

    fn boundary(id: u32) -> BoundaryOperand {
        BoundaryOperand {
            operand: operand(id),
            decision: BoundaryDecision::Move,
        }
    }

    fn edge(target: u32, args: &[u32]) -> Edge {
        Edge {
            target: BlockId(target),
            args: args.iter().copied().map(operand).collect(),
        }
    }

    fn op(id: u32, kind: SemOpKind, results: Vec<ValueDef>) -> SemOp {
        SemOp {
            id: OpId(id),
            kind,
            results,
            provenance: Provenance::Synthesized,
        }
    }

    fn destroy(id: u32, value: u32) -> SemOp {
        op(
            id,
            SemOpKind::DestroyValue {
                value: operand(value),
            },
            Vec::new(),
        )
    }

    fn block(id: u32, ops: Vec<SemOp>, terminator: SemTerminator) -> SemBlock {
        SemBlock {
            id: BlockId(id),
            args: Vec::new(),
            ops,
            terminator,
        }
    }

    fn function(blocks: Vec<SemBlock>) -> SemFunction {
        SemFunction {
            id: ItemId(0),
            callable: CallableId(0),
            declaration: DefId::for_test("lifetime"),
            name: "lifetime".into(),
            span: 0..0,
            source_origin: FunctionSourceOrigin::Unknown,
            params: vec![
                BlockArg {
                    value: ValueId(0),
                    ty: ResolvedTy::String,
                    own: OwnKind::Owned,
                },
                BlockArg {
                    value: ValueId(99),
                    ty: ResolvedTy::Bool,
                    own: OwnKind::None,
                },
            ],
            return_ty: ResolvedTy::Tuple(Vec::new()),
            entry: BlockId(0),
            blocks,
            places: Vec::new(),
            bindings: Vec::new(),
        }
    }

    fn done() -> SemTerminator {
        SemTerminator::Return { value: None }
    }

    #[test]
    fn consumed_parameter_has_no_remaining_obligation() {
        assert!(verify(&function(vec![block(0, vec![destroy(0, 0)], done())])).is_empty());
    }

    #[test]
    fn borrowed_parameter_cannot_be_consumed() {
        let mut f = function(vec![block(0, vec![destroy(0, 0)], done())]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).iter().any(|v| v.value == ValueId(0)));
    }

    #[test]
    fn borrowed_parameter_requires_explicit_copy_before_return() {
        for decision in [
            BoundaryDecision::Move,
            BoundaryDecision::Borrow,
            BoundaryDecision::Copy,
        ] {
            let mut f = function(vec![block(
                0,
                Vec::new(),
                SemTerminator::Return {
                    value: Some(BoundaryOperand {
                        operand: operand(0),
                        decision,
                    }),
                },
            )]);
            f.params[0].own = OwnKind::Guaranteed;
            assert!(
                verify(&f).iter().any(|v| v.value == ValueId(0)),
                "{decision:?}"
            );
        }
    }

    #[test]
    fn borrowed_parameter_can_create_an_owned_copy() {
        let mut f = function(vec![block(
            0,
            vec![
                op(
                    0,
                    SemOpKind::CopyValue { source: operand(0) },
                    vec![owned(1)],
                ),
                destroy(1, 1),
            ],
            done(),
        )]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn borrowed_parameter_can_be_borrowed_by_a_call() {
        let mut f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Call {
                    id: OpId(0),
                    callee: CallableId(1),
                    args: vec![BoundaryOperand {
                        operand: operand(0),
                        decision: BoundaryDecision::Borrow,
                    }],
                    result: CallResult::Unit,
                    normal: edge(1, &[]),
                    unwind: CallUnwind::Cleanup(edge(2, &[])),
                },
            ),
            block(1, Vec::new(), done()),
            block(2, Vec::new(), SemTerminator::ResumeUnwind),
        ]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn borrowed_parameter_cannot_be_transferred_into_an_owned_block_argument() {
        let mut continuation = block(1, vec![destroy(1, 1)], done());
        continuation.args.push(BlockArg {
            value: ValueId(1),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        let mut f = function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0]))),
            continuation,
        ]);
        f.params[0].own = OwnKind::Guaranteed;
        assert!(verify(&f).iter().any(|v| v.value == ValueId(0)));
    }

    #[test]
    fn return_transfers_the_owned_result() {
        let mut f = function(vec![block(
            0,
            Vec::new(),
            SemTerminator::Return {
                value: Some(boundary(0)),
            },
        )]);
        f.return_ty = ResolvedTy::String;
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn nonterminating_loop_may_keep_its_owner_live() {
        let f = function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[]))),
            block(
                1,
                vec![
                    op(
                        0,
                        SemOpKind::CopyValue { source: operand(0) },
                        vec![owned(1)],
                    ),
                    destroy(1, 1),
                ],
                SemTerminator::Goto(edge(1, &[])),
            ),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
    }

    #[test]
    fn duplicate_owner_on_one_edge_is_rejected() {
        let mut exit = block(1, vec![destroy(0, 1), destroy(1, 2)], done());
        for value in [1, 2] {
            exit.args.push(BlockArg {
                value: ValueId(value),
                ty: ResolvedTy::String,
                own: OwnKind::Owned,
            });
        }
        let errors = verify(&function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0, 0]))),
            exit,
        ]));
        assert!(
            errors.iter().any(|e| e.value == ValueId(0)
                && e.reason == "owned value is not live on every incoming path"),
            "{errors:?}"
        );
    }

    #[test]
    fn parameter_live_at_return_is_rejected() {
        let errors = verify(&function(vec![block(0, Vec::new(), done())]));
        assert!(
            errors
                .iter()
                .any(|e| e.value == ValueId(0) && e.reason == "owned value remains live at exit"),
            "{errors:?}"
        );
        let diagnostics = crate::verify_function(&function(vec![block(0, Vec::new(), done())]));
        assert!(
            diagnostics.iter().any(|diagnostic| matches!(
                diagnostic.kind,
                crate::SirDiagnosticKind::OwnershipLifetime {
                    value: ValueId(0),
                    ..
                }
            )),
            "{diagnostics:?}"
        );
    }

    #[test]
    fn repeated_consumption_is_rejected() {
        let errors = verify(&function(vec![block(
            0,
            vec![destroy(0, 0), destroy(1, 0)],
            done(),
        )]));
        assert!(
            errors
                .iter()
                .any(|e| e.reason == "owned value is not live on every incoming path"),
            "{errors:?}"
        );
    }

    #[test]
    fn each_branch_may_consume_the_same_incoming_owner() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(1, vec![destroy(0, 0)], done()),
            block(2, vec![destroy(1, 0)], done()),
        ]);
        assert!(verify(&f).is_empty());
    }

    #[test]
    fn consumed_on_one_predecessor_cannot_be_read_after_join() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(1, vec![destroy(0, 0)], SemTerminator::Goto(edge(3, &[]))),
            block(2, Vec::new(), SemTerminator::Goto(edge(3, &[]))),
            block(3, vec![destroy(1, 0)], done()),
        ]);
        let errors = verify(&f);
        assert!(
            errors
                .iter()
                .any(|e| e.block == BlockId(3) && e.value == ValueId(0)),
            "{errors:?}"
        );
    }

    #[test]
    fn loop_local_definition_is_a_new_dynamic_owner() {
        let mut f = function(vec![
            block(0, vec![destroy(0, 0)], SemTerminator::Goto(edge(1, &[]))),
            block(
                1,
                vec![
                    op(1, SemOpKind::ConstStr(StringLiteralId(0)), vec![owned(1)]),
                    destroy(2, 1),
                ],
                SemTerminator::Branch {
                    condition: operand(99),
                    then_target: edge(1, &[]),
                    else_target: edge(2, &[]),
                },
            ),
            block(2, Vec::new(), done()),
        ]);
        assert!(verify(&f).is_empty());
        f.blocks[1].ops.pop();
        let errors = verify(&f);
        assert!(
            errors.iter().any(|e| e.value == ValueId(1)
                && e.reason == "previous dynamic owner remains live at definition"),
            "{errors:?}"
        );
    }

    #[test]
    fn loop_argument_transfers_before_rebinding_itself() {
        let mut body = block(
            1,
            Vec::new(),
            SemTerminator::Branch {
                condition: operand(99),
                then_target: edge(1, &[1]),
                else_target: edge(2, &[1]),
            },
        );
        body.args.push(BlockArg {
            value: ValueId(1),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        let mut exit = block(2, vec![destroy(0, 2)], done());
        exit.args.push(BlockArg {
            value: ValueId(2),
            ty: ResolvedTy::String,
            own: OwnKind::Owned,
        });
        assert!(verify(&function(vec![
            block(0, Vec::new(), SemTerminator::Goto(edge(1, &[0]))),
            body,
            exit
        ]))
        .is_empty());
    }

    #[test]
    fn call_moves_apply_to_both_exits_but_results_only_to_normal_exit() {
        let f = function(vec![
            block(
                0,
                Vec::new(),
                SemTerminator::Call {
                    id: OpId(0),
                    callee: CallableId(1),
                    args: vec![boundary(0)],
                    result: CallResult::Value(owned(1)),
                    normal: edge(1, &[]),
                    unwind: CallUnwind::Cleanup(edge(2, &[])),
                },
            ),
            block(1, vec![destroy(1, 1)], done()),
            block(2, Vec::new(), SemTerminator::ResumeUnwind),
        ]);
        assert!(verify(&f).is_empty(), "{:?}", verify(&f));
        let mut invalid = f;
        invalid.blocks[2].ops.push(destroy(2, 0));
        let errors = verify(&invalid);
        assert!(
            errors
                .iter()
                .any(|e| e.block == BlockId(2) && e.value == ValueId(0)),
            "{errors:?}"
        );
    }
}
