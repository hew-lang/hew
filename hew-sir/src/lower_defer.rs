//! Lexical cleanup elaboration. Deferred bodies use the enclosing places;
//! the verified schedule owns execution order and the fault parks.

use std::collections::{BTreeMap, BTreeSet};

use super::{
    BlockId, Builder, Edge, HirExpr, PlaceOrigin, Provenance, ResolvedTy, SemOpKind, SemTerminator,
    ValueId,
};

#[derive(Clone)]
pub(super) struct PendingDefer {
    pub(super) id: crate::DeferId,
    scope: crate::DeferScopeId,
    pub(super) floor: usize,
    body: HirExpr,
    /// The bindings in scope where the defer was registered, which are the
    /// only ones its body can name.
    pub(super) visible: Vec<super::BindingId>,
}

#[derive(Clone)]
pub(super) struct BodyBoundary {
    floor: usize,
    finish: BlockId,
    preserved: BTreeMap<ValueId, ResolvedTy>,
    loop_depth: usize,
    loan_depth: usize,
}

fn edge(target: BlockId) -> Edge {
    Edge {
        target,
        args: Vec::new(),
    }
}

impl Builder<'_, '_> {
    pub(super) fn finish_return_value(
        &mut self,
        value: Option<crate::BoundaryOperand>,
    ) -> Result<(), String> {
        let saved = self.control_state();
        let recovery = self.recovery_bodies.clone();
        let preserved = value
            .as_ref()
            .and_then(|result| {
                self.owned_live
                    .get(&result.operand.value)
                    .map(|ty| (result.operand.value, ty.clone()))
            })
            .into_iter()
            .collect();
        if self.terminal_linear_receiver().is_some() {
            self.emit_place_operation(SemOpKind::FinishLinearReceiver, Provenance::Synthesized)?;
        }
        let outer_dual_return = self.dual_return.take();
        if self.callable.signature.hands_back_receiver() {
            self.dual_return = value.as_ref().map(|result| result.operand.value);
        }
        self.finish_recovery_scopes(0, &preserved)?;
        self.finish_task_scopes(0)?;
        self.end_loans_since(0)?;
        self.destroy_live_since(&preserved)?;
        self.end_scopes(0)?;
        self.dual_return = outer_dual_return;
        if let Some(result) = &value {
            self.owned_live.remove(&result.operand.value);
        }
        self.set_terminator(SemTerminator::Return { value })?;
        let terminal = self.current;
        self.restore_control_state(&saved);
        self.recovery_bodies = recovery;
        self.current = terminal;
        Ok(())
    }

    /// Finish a recovery boundary before an exit starts draining outer work.
    /// Its handler stays active until this scope's own cleanup has succeeded.
    pub(super) fn finish_recovery_scopes(
        &mut self,
        floor: usize,
        preserved: &BTreeMap<ValueId, ResolvedTy>,
    ) -> Result<(), String> {
        while let Some(boundary) = self
            .recovery_bodies
            .last()
            .filter(|body| body.floor >= floor)
            .cloned()
        {
            self.finish_task_scopes(boundary.floor)?;
            let mut keep = boundary.preserved;
            keep.extend(preserved.iter().map(|(value, ty)| (*value, ty.clone())));
            self.destroy_live_since(&keep)?;
            self.end_scopes(boundary.floor)?;
            self.recovery_bodies.pop();
            while self.scopes.len() > boundary.floor {
                self.leave_scope();
            }
        }
        Ok(())
    }

    pub(super) fn register_defer(&mut self, body: &HirExpr, scope: u32) -> Result<(), String> {
        let id = crate::DeferId(self.ops);
        let scope = crate::DeferScopeId(scope);
        self.emit_place_operation(
            SemOpKind::RegisterDefer {
                defer: id,
                scope,
                dependencies: Vec::new(),
            },
            Provenance::Site(body.site),
        )?;
        let mut visible = self.bindings.keys().copied().collect::<Vec<_>>();
        visible.sort_unstable();
        self.defers.push(PendingDefer {
            id,
            scope,
            floor: self.scopes.len() - 1,
            body: body.clone(),
            visible,
        });
        Ok(())
    }

    pub(super) fn in_deferred_body(&self) -> bool {
        !self.defer_bodies.is_empty()
    }

    pub(super) fn check_deferred_loop_exit(&self, target_depth: usize) -> Result<(), String> {
        if self
            .defer_bodies
            .last()
            .is_some_and(|body| target_depth < body.loop_depth)
        {
            return Err("break or continue cannot escape a deferred body".into());
        }
        Ok(())
    }

    /// A temporary's asynchronous cleanup can fail between source expressions.
    /// Propagate that fault before evaluating the next ordinary expression.
    pub(super) fn dispatch_value_cleanup(&mut self) -> Result<(), String> {
        self.cleanup_may_fail = false;
        let normal = self.new_block(Vec::new());
        let fault = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::CleanupDispatch {
            normal: edge(normal),
            fault: edge(fault),
        })?;
        let saved = self.control_state();
        self.current = fault;
        self.finish_fault_exit()?;
        self.restore_control_state(&saved);
        self.current = normal;
        Ok(())
    }

    /// Generate a fault successor without changing its sibling's lexical state.
    pub(super) fn finish_fault_exit(&mut self) -> Result<(), String> {
        let saved = self.control_state();
        self.cleanup_draining = true;
        let boundary = self
            .defer_bodies
            .last()
            .into_iter()
            .chain(self.recovery_bodies.last())
            .max_by_key(|body| body.floor)
            .cloned();
        let floor = boundary.as_ref().map_or(0, |body| body.floor);
        let loan_floor = boundary.as_ref().map_or(0, |body| body.loan_depth);
        let mut steps = self.plan_task_joins(floor, true);
        steps.extend(self.plan_argument_loans(loan_floor));
        for step in &steps {
            self.emit_exit_step(step)?;
        }
        self.argument_receiver_loans.truncate(loan_floor);
        let preserved = boundary
            .as_ref()
            .map(|b| b.preserved.clone())
            .unwrap_or_default();
        let handback = if boundary.is_none() {
            self.take_fault_handback()?
        } else {
            None
        };
        for step in self.plan_fault_drain(floor, &preserved, boundary.is_none()) {
            self.emit_exit_step(&step)?;
        }
        self.set_terminator(
            boundary.map_or(SemTerminator::ResumeUnwind { handback }, |body| {
                SemTerminator::Goto(edge(body.finish))
            }),
        )?;
        let terminal = self.current;
        self.restore_control_state(&saved);
        self.current = terminal;
        Ok(())
    }

    /// A `var self` method mutates its caller's place rather than consuming
    /// it. When it fails, it hands the receiver, as last written, back to the
    /// caller instead of releasing it, so the caller's place is never empty.
    fn take_fault_handback(&mut self) -> Result<Option<crate::BoundaryOperand>, String> {
        let Some(binding) = self.function.var_self_receiver else {
            return Ok(None);
        };
        if !self.callable.signature.hands_back_receiver() {
            return Ok(None);
        }
        let receiver = &self.callable.signature.params[0];
        let ty = receiver.ty.clone();
        // An owning receiver was consumed and moves back out; a plain one was
        // copied in and is copied back out.
        let owned = receiver.passing == crate::SemParamPassing::Consume;
        let decision = if owned {
            crate::BoundaryDecision::Move
        } else {
            crate::BoundaryDecision::Copy
        };
        if owned
            && self.dual_return.is_none()
            && self
                .state_taken
                .iter()
                .any(|place| self.in_var_self_receiver(*place))
        {
            return Err(
                "a `var self` method must keep its receiver whole wherever it can fail; \
                 `self` or one of its fields is moved out here. Call methods on the field \
                 in place, or restore it before anything that can fail"
                    .into(),
            );
        }
        if let Some(dual) = self
            .dual_return
            .filter(|dual| !owned || self.owned_live.contains_key(dual))
        {
            let dual_ty = self.callable.signature.return_ty.clone();
            let fields = self.emit_destructure_value(
                dual,
                &dual_ty,
                crate::AggregateShapeRef::Tuple,
                Provenance::Synthesized,
            )?;
            self.owned_live.remove(&fields[1].id);
            return Ok(Some(crate::BoundaryOperand {
                operand: crate::Operand {
                    value: fields[1].id,
                },
                decision,
            }));
        }
        // A copied receiver stays in its place, where the defers this fault
        // still runs read it.
        let value = match self.binding_target(binding)? {
            super::BindingTarget::Place(place) => self.emit_typed(
                Provenance::Synthesized,
                &ty,
                if owned {
                    SemOpKind::LoadTake { place }
                } else {
                    SemOpKind::LoadCopy { place }
                },
            )?,
            super::BindingTarget::Value(value) => value,
        };
        if owned && self.owned_live.remove(&value).is_none() {
            return Err("a `var self` receiver must be whole where its method can fail".into());
        }
        Ok(Some(crate::BoundaryOperand {
            operand: crate::Operand { value },
            decision,
        }))
    }

    pub(super) fn finish_checked_fault(&mut self, kind: crate::TrapKind) -> Result<(), String> {
        let cleanup = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::CheckedRaiseFault {
            kind,
            cleanup: edge(cleanup),
        })?;
        self.current = cleanup;
        self.finish_fault_exit()
    }

    pub(super) fn emit_deferred_body(&mut self, action: &PendingDefer) -> Result<(), String> {
        let saved = self.control_state();
        let park = crate::FaultParkId(action.scope.0);
        let body = self.new_block(Vec::new());
        let finish = self.new_block(Vec::new());
        let place_floor = self.places.len();
        let block_floor = body.0 as usize;
        self.set_terminator(SemTerminator::EnterDefer {
            defer: action.id,
            park,
            body: edge(body),
        })?;
        self.current = body;
        self.cleanup_draining = false;
        self.cleanup_may_fail = false;
        self.defer_bodies.push(BodyBoundary {
            floor: self.scopes.len(),
            finish,
            preserved: self.owned_live.clone(),
            loop_depth: self.loops.len(),
            loan_depth: self.argument_receiver_loans.len(),
        });
        self.lower_discarded_expr(&action.body)?;
        if self.is_open() {
            self.destroy_live_since(&saved.owned_live)?;
            self.set_terminator(SemTerminator::Goto(edge(finish)))?;
        }
        self.defer_bodies.pop();

        // Emitted operations provide the dependencies. The verifier independently
        // proves the body's exact free places and their availability at every exit.
        let mut used = BTreeSet::new();
        let mut body_values = BTreeSet::new();
        for block in &self.blocks[block_floor..] {
            body_values.extend(block.args.iter().map(|arg| arg.value));
            for operation in &block.ops {
                operation.kind.visit_places(|place| {
                    used.insert(place);
                });
                body_values.extend(operation.results.iter().map(|value| value.id));
            }
            if let Some(term) = &block.terminator {
                term.visit_results(|value| {
                    body_values.insert(value.id);
                });
            }
        }
        let mut dependencies = BTreeSet::new();
        for place in used {
            if matches!(
                self.places[place.0 as usize].origin,
                PlaceOrigin::Capture { .. } | PlaceOrigin::ActorState { .. }
            ) {
                dependencies.insert(place);
                continue;
            }
            let (root, _) = crate::projection::place_path(&self.places, place)?;
            match root {
                crate::OwnerRoot::Local(local) if (local.0 as usize) < place_floor => {
                    dependencies.insert(place);
                }
                crate::OwnerRoot::Value(value) if !body_values.contains(&value) => {
                    dependencies.insert(place);
                }
                _ => {}
            }
        }
        for block in &mut self.blocks {
            for operation in &mut block.ops {
                if let SemOpKind::RegisterDefer {
                    defer,
                    dependencies: declared,
                    ..
                } = &mut operation.kind
                {
                    if *defer == action.id {
                        *declared = dependencies.iter().copied().collect();
                    }
                }
            }
        }
        self.restore_control_state(&saved);
        self.current = finish;
        let next = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::FinishDefer {
            defer: action.id,
            park,
            next: edge(next),
        })?;
        self.current = next;
        Ok(())
    }
}

impl Builder<'_, '_> {
    pub(super) fn lower_scope_recovery(
        &mut self,
        whole: &HirExpr,
        scope: &HirExpr,
        error: &hew_hir::HirBinding,
        handler: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        let inherited = self.control_state();
        let catch = self.new_block(Vec::new());
        self.recovery_bodies.push(BodyBoundary {
            floor: self.scopes.len(),
            finish: catch,
            preserved: self.owned_live.clone(),
            loop_depth: self.loops.len(),
            loan_depth: self.argument_receiver_loans.len(),
        });
        let result_ty = self.ty(&whole.ty);
        let result = match &scope.kind {
            hew_hir::HirExprKind::Scope { body } => self.lower_task_scope(body, true)?,
            hew_hir::HirExprKind::ScopeDeadline { body, duration } => {
                self.lower_task_scope_with_deadline(body, Some(duration), true)?
            }
            _ => return Err("scope recovery requires a checked lexical scope".into()),
        };
        self.recovery_bodies.pop();
        let mut exits = Vec::new();
        if self.is_open() {
            if let Some(value) = result {
                self.owned_live.remove(&value);
            }
            exits.push(super::MatchExit {
                state: self.control_state(),
                result: result
                    .filter(|_| result_ty != ResolvedTy::Unit)
                    .map(|value| crate::Operand { value }),
            });
        }
        self.restore_control_state(&inherited);
        self.current = catch;
        let failure_ty = self.ty(&error.ty);
        let shape = self.service.require_variant_shape(&failure_ty)?;
        let variants = &self.service.variant_shapes[shape.0 as usize].variants;
        let tag = |name: &str| -> Result<u32, String> {
            variants
                .iter()
                .position(|variant| variant.name == name)
                .and_then(|index| u32::try_from(index).ok())
                .ok_or_else(|| format!("scope failure has no {name} variant"))
        };
        let deadline_variant = tag("Deadline")?;
        let fault_variant = tag("Fault")?;
        let produced_failure = self.fresh_value();
        let failure = self.fresh_value();
        let normal = self.new_block(vec![crate::BlockArg {
            value: failure,
            ty: failure_ty.clone(),
            own: crate::OwnKind::Owned,
        }]);
        let unwind = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::RecoverFault {
            result: crate::ValueDef {
                id: produced_failure,
                ty: failure_ty.clone(),
                own: crate::OwnKind::Owned,
            },
            deadline_variant,
            fault_variant,
            normal: Edge {
                target: normal,
                args: vec![crate::Operand {
                    value: produced_failure,
                }],
            },
            unwind: edge(unwind),
        })?;
        self.current = unwind;
        self.finish_fault_exit()?;
        self.restore_control_state(&inherited);
        self.current = normal;
        self.owned_live.insert(failure, failure_ty);
        let floor = self.scopes.len();
        self.open_scope();
        self.bind_source_value(error, failure)?;
        let hew_hir::HirExprKind::Block(body) = &handler.kind else {
            return Err("scope recovery handler must be a lexical block".into());
        };
        let result = self.lower_block(body, super::OwnedBindingUse::Return)?;
        if self.is_open() {
            self.end_scopes(floor)?;
            if let Some(result) = &result {
                self.owned_live.remove(&result.value);
            }
            self.leave_scope();
            exits.push(super::MatchExit {
                state: self.control_state(),
                result,
            });
        } else {
            self.leave_scope();
        }
        self.merge_match_exits(exits, &result_ty)
    }
}
