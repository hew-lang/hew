//! Exit cleanup order. Every exit from a lexical region owes the same kinds of
//! release in the same order: join task scopes, end argument loans, destroy
//! temporaries, then per scope (innermost first) run its defers, end its scope
//! loans and end its bindings. The `plan_*` functions compute that order from
//! the builder's lexical state without emitting; the emitters replay it.

use std::collections::BTreeMap;

use super::{
    BindingId, BindingTarget, Builder, Edge, PlaceOrigin, Provenance, ResolvedTy, SemOpKind,
};
use crate::{DeferId, PlaceId, SemTerminator, TaskScopeId, TaskScopeJoinMode, ValueId};

/// One release an exit owes.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum ExitStep {
    /// Join a task scope's children, then close it.
    JoinScope {
        scope: TaskScopeId,
        mode: TaskScopeJoinMode,
    },
    EndLoan(ValueId),
    Destroy(ValueId),
    /// Run a pending defer. `view` is the binding environment its body is
    /// lowered against: a scalar `var` rebinds to a new value on assignment,
    /// so the body reads whatever its bindings name at this exit.
    Defer {
        id: DeferId,
        view: Vec<(BindingId, BindingTarget)>,
    },
    /// End a local binding's storage, or a deferred actor-state seat (D447).
    EndLifetime(PlaceId),
    /// Dispatch the drained cleanup's outcome on a fault path whose normal and
    /// fault successors coincide.
    Dispatch,
}

/// The ordered releases of a scope drain, and whether its outcome must be
/// dispatched afterwards.
pub(super) struct DrainPlan {
    steps: Vec<ExitStep>,
    ran_defer: bool,
    may_fault: bool,
}

impl Builder<'_, '_> {
    /// Whether releasing a value of `ty` can run an authored `close`.
    pub(super) fn release_may_fault(&self, ty: &ResolvedTy) -> bool {
        crate::resource::release_may_fault(
            &crate::resource::authored_close_in_hir(self.service.module),
            &self.service.aggregate_shapes,
            &self.service.variant_shapes,
            ty,
        )
    }

    /// Join every task scope opened at or above `floor`, innermost first.
    pub(super) fn plan_task_joins(&self, floor: usize, cancel: bool) -> Vec<ExitStep> {
        self.task_scopes
            .iter()
            .rev()
            .take_while(|frame| frame.depth >= floor)
            .map(|frame| ExitStep::JoinScope {
                scope: frame.scope,
                mode: match (frame.race, cancel) {
                    (false, false) => TaskScopeJoinMode::Wait,
                    (false, true) => TaskScopeJoinMode::PropagateFault,
                    (true, false) => TaskScopeJoinMode::CancelLosers,
                    (true, true) => TaskScopeJoinMode::CancelLosersAfterFault,
                },
            })
            .collect()
    }

    /// End the argument and receiver loans opened since `depth`, innermost first.
    pub(super) fn plan_argument_loans(&self, depth: usize) -> Vec<ExitStep> {
        self.argument_receiver_loans
            .get(depth..)
            .unwrap_or_default()
            .iter()
            .rev()
            .filter(|loan| !self.ended_loans.contains(loan))
            .map(|&loan| ExitStep::EndLoan(loan))
            .collect()
    }

    /// Destroy every live owner not in `baseline`, newest first.
    pub(super) fn plan_destroys(
        &self,
        baseline: &BTreeMap<ValueId, ResolvedTy>,
        may_fault: &mut bool,
    ) -> Vec<ExitStep> {
        self.owned_live
            .iter()
            .rev()
            .filter(|(value, _)| !baseline.contains_key(value))
            .map(|(&value, _)| {
                if let Some(ty) = self.value_ty(value) {
                    *may_fault |= self.release_may_fault(&ty);
                }
                ExitStep::Destroy(value)
            })
            .collect()
    }

    /// Drain the scopes at or above `floor`, innermost first: run each scope's
    /// defers, end the loans taken inside it (a loan is a dependent of what it
    /// borrows), then end its bindings in reverse declaration order.
    pub(super) fn plan_scope_drain(&self, floor: usize) -> DrainPlan {
        let mut steps = Vec::new();
        let mut ran_defer = false;
        let mut may_fault = false;
        let mut defers = self.defers.len();
        let mut loans = self.scope_loans.len();
        for index in (floor..self.scopes.len()).rev() {
            while defers > 0 && self.defers[defers - 1].floor == index {
                defers -= 1;
                steps.push(self.defer_step(&self.defers[defers]));
                ran_defer = true;
            }
            let loan_floor = self
                .scope_loan_floors
                .get(index)
                .copied()
                .unwrap_or(loans)
                .min(loans);
            steps.extend(
                self.scope_loans[loan_floor..loans]
                    .iter()
                    .rev()
                    .filter(|loan| !self.ended_loans.contains(loan))
                    .map(|&loan| ExitStep::EndLoan(loan)),
            );
            loans = loan_floor;
            for binding in self.scopes[index].iter().rev() {
                let declaration = self.binding_declarations[binding];
                if let BindingTarget::Place(place) = self.source_bindings[declaration].target {
                    let decl = &self.places[place.0 as usize];
                    if decl.origin == PlaceOrigin::Local {
                        may_fault |= self.release_may_fault(&decl.ty);
                        steps.push(ExitStep::EndLifetime(place));
                    }
                }
            }
        }
        DrainPlan {
            steps,
            ran_defer,
            may_fault,
        }
    }

    fn defer_step(&self, action: &super::deferred::PendingDefer) -> ExitStep {
        ExitStep::Defer {
            id: action.id,
            view: action
                .visible
                .iter()
                .filter_map(|binding| self.bindings.get(binding).map(|target| (*binding, *target)))
                .collect(),
        }
    }

    /// Emit one step into the current block, leaving the builder at its
    /// continuation with the lexical state the step consumed removed.
    pub(super) fn emit_exit_step(&mut self, step: &ExitStep) -> Result<(), String> {
        match step {
            ExitStep::JoinScope { scope, mode } => {
                let frame = self.task_scopes.pop().expect("active task scope");
                debug_assert_eq!(frame.scope, *scope);
                let next = self.new_block(Vec::new());
                self.set_terminator(SemTerminator::Suspend {
                    kind: crate::SuspendKind::Join {
                        scope: *scope,
                        mode: *mode,
                    },
                    inputs: Vec::new(),
                    result: crate::CallResult::Unit,
                    resumes: vec![edge(next)],
                    cancel: edge(next),
                    unwind: edge(next),
                })?;
                self.current = next;
                self.emit_place_operation(
                    SemOpKind::TaskScopeClose { scope: *scope },
                    Provenance::Synthesized,
                )
            }
            ExitStep::EndLoan(loan) => self.emit_place_operation(
                SemOpKind::EndBorrow {
                    borrow: crate::Operand { value: *loan },
                },
                Provenance::Synthesized,
            ),
            ExitStep::Destroy(value) => {
                self.emit_place_operation(
                    SemOpKind::DestroyValue {
                        value: crate::Operand { value: *value },
                    },
                    Provenance::Synthesized,
                )?;
                self.owned_live.remove(value);
                Ok(())
            }
            ExitStep::Defer { id, .. } => {
                let index = self
                    .defers
                    .iter()
                    .rposition(|action| action.id == *id)
                    .expect("pending defer");
                let action = self.defers.remove(index);
                self.emit_deferred_body(&action)
            }
            ExitStep::EndLifetime(place) => {
                self.deferred_initialized.remove(place);
                self.emit_place_operation(
                    SemOpKind::EndLifetime { place: *place },
                    Provenance::Synthesized,
                )
            }
            ExitStep::Dispatch => {
                let next = self.new_block(Vec::new());
                self.set_terminator(SemTerminator::CleanupDispatch {
                    normal: edge(next),
                    fault: edge(next),
                })?;
                self.current = next;
                Ok(())
            }
        }
    }

    /// Emit an exit's scope drain inline, without changing the declaration
    /// context used to generate another successor. A failed drain escalates
    /// through the enclosing cleanup boundary instead of resuming the exit.
    pub(super) fn end_scopes(&mut self, floor: usize) -> Result<(), String> {
        let plan = self.plan_scope_drain(floor);
        let previous_draining = self.cleanup_draining;
        self.cleanup_draining = true;
        for step in &plan.steps {
            self.emit_exit_step(step)?;
        }
        let loan_floor = self
            .scope_loan_floors
            .get(floor)
            .copied()
            .unwrap_or(self.scope_loans.len())
            .min(self.scope_loans.len());
        self.scope_loans.truncate(loan_floor);
        self.cleanup_draining = previous_draining;
        self.cleanup_may_fail |= plan.may_fault;
        if plan.ran_defer || self.cleanup_may_fail {
            self.cleanup_may_fail = false;
            let normal = self.new_block(Vec::new());
            let fault = self.new_block(Vec::new());
            self.set_terminator(SemTerminator::CleanupDispatch {
                normal: edge(normal),
                fault: edge(fault),
            })?;
            let saved = self.control_state();
            self.current = fault;
            while self.scopes.len() > floor {
                self.leave_scope();
            }
            self.finish_fault_exit()?;
            self.restore_control_state(&saved);
            self.current = normal;
        }
        Ok(())
    }

    /// The releases a fault exit owes after its `var self` handback (if any)
    /// has been taken: temporaries, the scope drain and, leaving the body,
    /// the deferred state seats this path initialized (D447).
    pub(super) fn plan_fault_drain(
        &self,
        floor: usize,
        preserved: &BTreeMap<ValueId, ResolvedTy>,
        leaves_body: bool,
    ) -> Vec<ExitStep> {
        let mut may_fault = self.cleanup_may_fail;
        let mut steps = self.plan_destroys(preserved, &mut may_fault);
        let drain = self.plan_scope_drain(floor);
        steps.extend(drain.steps);
        if drain.ran_defer || may_fault || drain.may_fault {
            steps.push(ExitStep::Dispatch);
        }
        if leaves_body {
            // Spawn-supplied fields stay with the spawn, which destroys them
            // when init reports failure.
            steps.extend(
                self.deferred_initialized
                    .iter()
                    .rev()
                    .map(|&place| ExitStep::EndLifetime(place)),
            );
        }
        steps
    }
}

fn edge(target: crate::BlockId) -> Edge {
    Edge {
        target,
        args: Vec::new(),
    }
}
