//! Task selection borrows readiness and transfers only the winning task.

use super::{BindingTarget, Builder, MatchExit, OwnedBindingUse};
use crate::{
    BlockArg, BoundaryDecision, BoundaryOperand, CallResult, Edge, Operand, OwnKind, Provenance,
    SemOpKind, SemTerminator, SuspendKind, ValueDef, ValueId,
};
use hew_hir::{HirBinding, HirExpr, HirSelect, HirSelectArmKind};
use hew_types::ResolvedTy;

/// One evaluated `select` source, in arm order.
enum SelectSource<'a> {
    /// A prepared task handle the winning arm awaits.
    Task {
        target: BindingTarget,
        ty: ResolvedTy,
        output: ResolvedTy,
    },
    /// A borrowed channel receiver the winning arm receives from. `output` is
    /// the `Option<T>` the arm binds.
    Channel {
        receiver: &'a HirExpr,
        output: ResolvedTy,
    },
}

impl Builder<'_, '_> {
    #[expect(
        clippy::too_many_lines,
        reason = "selection evaluation, observation and branch ownership form one boundary"
    )]
    pub(super) fn lower_task_select(
        &mut self,
        expression: &HirExpr,
        select: &HirSelect,
    ) -> Result<Option<ValueId>, String> {
        let outer_live = self.owned_live.clone();
        let outer_bindings = self.bindings.keys().copied().collect();
        let probe_depth = self.argument_receiver_loans.len();
        let result_ty = self.ty(&expression.ty);
        let provenance = Provenance::Site(expression.site);
        let mut sources: Vec<(usize, SelectSource<'_>)> = Vec::new();
        let mut timer = None;
        let mut inputs = Vec::new();
        let mut loans = Vec::new();
        let loan_depth = self.argument_receiver_loans.len();
        // Borrow each source as it is evaluated. Later operands cannot replace
        // a prepared task; their failure edges share call-argument loan cleanup.
        for (arm_index, arm) in select.arms.iter().enumerate() {
            match &arm.kind {
                HirSelectArmKind::TaskAwait { task } => {
                    self.require_consuming_capture(task)?;
                    let ty = self.ty(&task.ty);
                    let ResolvedTy::Task(output) = &ty else {
                        return Err("task selection operand lacks its checked output".into());
                    };
                    let target = if let Some(place) = self.expression_projection(task)? {
                        BindingTarget::Place(place)
                    } else {
                        let value =
                            self.lower_expr_with_binding_use(task, OwnedBindingUse::Move)?;
                        if self.value_own_kind(value) != Some(OwnKind::Owned) {
                            return Err("task selection requires an owning task handle".into());
                        }
                        BindingTarget::Value(value)
                    };
                    let value = match target {
                        BindingTarget::Place(place) => {
                            let value = self.emit_typed(
                                provenance.clone(),
                                &ty,
                                SemOpKind::LoadBorrow { place },
                            )?;
                            loans.push(value);
                            self.argument_receiver_loans.push(value);
                            value
                        }
                        BindingTarget::Value(value) => value,
                    };
                    inputs.push(BoundaryOperand {
                        operand: Operand { value },
                        decision: BoundaryDecision::Borrow,
                    });
                    sources.push((
                        arm_index,
                        SelectSource::Task {
                            target,
                            ty: ty.clone(),
                            output: *output.clone(),
                        },
                    ));
                }
                HirSelectArmKind::ChannelRecv { receiver } => {
                    // The selection observes readiness only: the winning arm
                    // performs the ordinary receive on its own borrow.
                    let ty = self.ty(&receiver.ty);
                    let element = crate::receiver_element(&ty)
                        .ok_or("channel selection operand lacks its element type")?
                        .clone();
                    let operand = self.lower_borrowed_read(receiver, &mut loans)?;
                    inputs.push(BoundaryOperand {
                        operand,
                        decision: BoundaryDecision::Borrow,
                    });
                    sources.push((
                        arm_index,
                        SelectSource::Channel {
                            receiver,
                            output: ResolvedTy::named_builtin(
                                "Option",
                                hew_types::BuiltinType::Option,
                                vec![element],
                            ),
                        },
                    ));
                }
                HirSelectArmKind::AfterTimer { duration } => {
                    if timer.is_some() || self.ty(&duration.ty) != ResolvedTy::Duration {
                        return Err("task selection requires at most one duration timer".into());
                    }
                    timer = Some(self.lower_read_operand(duration, "selection timer")?);
                }
                _ => {
                    return Err(
                        "native selection arm requires a task, a channel receive or a timer".into(),
                    )
                }
            }
        }
        if sources.is_empty() && timer.is_none() {
            return Err("task selection has no arms".into());
        }
        self.argument_receiver_loans.truncate(loan_depth);
        if let Some(duration) = &timer {
            inputs.push(BoundaryOperand {
                operand: duration.clone(),
                decision: BoundaryDecision::Copy,
            });
        }
        let raw = self.fresh_value();
        let selected = self.fresh_value();
        let resumed = self.new_block(vec![BlockArg {
            value: selected,
            ty: ResolvedTy::I64,
            own: OwnKind::None,
        }]);
        let cancel = self.new_block(Vec::new());
        let unwind = self.new_block(Vec::new());
        let inherited = self.control_state();
        self.set_terminator(SemTerminator::Suspend {
            kind: SuspendKind::Select {
                has_timeout: timer.is_some(),
                order: select.order,
            },
            inputs,
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::I64,
                own: OwnKind::None,
            }),
            resumes: vec![Edge {
                target: resumed,
                args: vec![Operand { value: raw }],
            }],
            cancel: Edge {
                target: cancel,
                args: Vec::new(),
            },
            unwind: Edge {
                target: unwind,
                args: Vec::new(),
            },
        })?;
        for cleanup in [cancel, unwind] {
            self.restore_control_state(&inherited);
            self.current = cleanup;
            self.end_call_loans(&loans)?;
            self.finish_fault_exit()?;
        }
        self.restore_control_state(&inherited);
        self.current = resumed;
        self.end_call_loans(&loans)?;
        let mut exits = Vec::new();
        for (arm_index, arm) in select.arms.iter().enumerate() {
            let source = sources
                .iter()
                .enumerate()
                .find(|(_, (index, _))| *index == arm_index);
            // The suspension contract validates the index before resuming.
            // Its final alternative is exhaustive, with no synthetic exit.
            let failure = if arm_index + 1 == select.arms.len() {
                None
            } else {
                let selected_index = source.map_or(sources.len(), |(index, _)| index);
                let index = self.emit_typed(
                    provenance.clone(),
                    &ResolvedTy::I64,
                    SemOpKind::ConstInteger(
                        i64::try_from(selected_index)
                            .map_err(|_| "selection index exceeds i64")?
                            .into(),
                    ),
                )?;
                let condition = self.emit_typed(
                    provenance.clone(),
                    &ResolvedTy::Bool,
                    SemOpKind::Binary {
                        op: hew_parser::ast::BinaryOp::Equal,
                        lhs: Operand { value: selected },
                        rhs: Operand { value: index },
                    },
                )?;
                Some(self.branch_candidate_test(condition)?)
            };
            if let Some((_, (_, source))) = source {
                let (value, output) = match source {
                    SelectSource::Task { target, ty, output } => {
                        let value = match target {
                            BindingTarget::Place(place) => self.emit_typed(
                                provenance.clone(),
                                ty,
                                SemOpKind::LoadTake { place: *place },
                            )?,
                            BindingTarget::Value(value) => *value,
                        };
                        (self.lower_task_await_value(value, output)?, output.clone())
                    }
                    SelectSource::Channel { receiver, output } => {
                        // Readiness was observed, nothing taken: the winner
                        // receives, and a closed channel resolves to `None`.
                        let value = self.lower_channel_recv_into(receiver, output.clone(), true)?;
                        (Some(value), output.clone())
                    }
                };
                if !self.is_open() {
                    if let Some(failure) = failure {
                        self.restore_control_state(&failure);
                    }
                    continue;
                }
                if let (Some(binding), Some(name)) = (arm.binding_id, &arm.binding_name) {
                    let value = match value {
                        Some(value) => value,
                        None => self.emit_typed(
                            provenance.clone(),
                            &ResolvedTy::Unit,
                            SemOpKind::ConstUnit,
                        )?,
                    };
                    self.bind_source_value(
                        &HirBinding {
                            id: binding,
                            name: name.clone(),
                            ty: output.clone(),
                            mutable: false,
                            span: arm.body.span.clone(),
                            is_consume: false,
                        },
                        value,
                    )?;
                }
            }
            let result = self.lower_selected_body(&arm.body, &result_ty)?;
            if self.is_open() {
                let mut protected_live = outer_live.clone();
                if let Some(result) = &result {
                    if let Some(ty) = self.owned_live.get(&result.value) {
                        protected_live.insert(result.value, ty.clone());
                    }
                }
                // The selected result survives normal candidate cleanup, but
                // must still be released if closing another owner fails.
                self.cleanup_match_candidate(&protected_live, probe_depth, &outer_bindings)?;
                if let Some(result) = &result {
                    self.owned_live.remove(&result.value);
                }
                exits.push(MatchExit {
                    state: self.control_state(),
                    result,
                });
            }
            if let Some(failure) = failure {
                self.restore_control_state(&failure);
            }
        }
        self.merge_match_exits(exits, &result_ty)
    }
}
