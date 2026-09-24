//! Builder construction, bindings, control-state, cleanup and loan bookkeeping.

use super::{
    lower_initial_value_transfer, projection, require_initial_scalar_read, tasks, BTreeMap,
    BTreeSet, Binding, BindingId, BindingTarget, BlockArg, BlockId, BodySource, Builder,
    CallableInstance, ControlState, Cow, Edge, HashMap, HirBinding, HirExpr, HirExprKind, HirFn,
    InstanceService, OpId, Operand, OwnKind, OwnedBindingUse, PendingBlock, PlaceId, PlaceOrigin,
    Provenance, ResolvedRef, ResolvedTy, SemCallable, SemFunction, SemOp, SemOpKind, SemTerminator,
    TypeInstanceKey, TypeSubstitution, ValueId,
};

impl<'hir, 'service> Builder<'hir, 'service> {
    #[expect(
        clippy::too_many_lines,
        reason = "constructs the complete body state and binds its checked ABI once"
    )]
    pub(super) fn new(
        function: Cow<'hir, HirFn>,
        callable: SemCallable,
        substitution: TypeSubstitution,
        source: &BodySource,
        service: &'service mut InstanceService<'hir>,
    ) -> Result<Self, String> {
        let source_params = source.parameters(&function)?.to_vec();
        let receiver_count = usize::from(matches!(
            source,
            BodySource::Closure(_) | BodySource::Actor { .. }
        ));
        // A stream producer owns the caller's sink as an implicit trailing
        // parameter with no source binding.
        let stream = match source {
            BodySource::Actor { actor, .. } => service.actors[actor.0 as usize]
                .handlers
                .iter()
                .find(|handler| handler.callable == callable.id)
                .and_then(|handler| handler.stream.clone()),
            BodySource::Function | BodySource::Closure(_) | BodySource::EntryAdapter(_) => None,
        };
        if source_params.len() + receiver_count + usize::from(stream.is_some())
            != callable.signature.params.len()
        {
            return Err(format!(
                "SIR callable `{}` has {} parameter ABI facts, but its HIR template has {} parameter(s)",
                callable.symbol,
                callable.signature.params.len(),
                source_params.len()
            ));
        }
        service.require_signature_shapes(&callable.signature)?;
        let entry = BlockId(0);
        let mut values = u32::try_from(receiver_count).expect("at most one receiver");
        let mut bindings = HashMap::new();
        let params = source_params.iter()
            .zip(callable.signature.params.iter().skip(receiver_count))
            .enumerate()
            .map(|(index, (param, abi))| {
                let ty = substitution.apply(&param.ty);
                if ty != abi.ty {
                    return Err(format!(
                        "SIR callable `{}` parameter {index} has `{}`, but its substituted HIR template has `{}`",
                        callable.symbol,
                        abi.ty.user_facing(),
                        ty.user_facing()
                    ));
                }
                let value = ValueId(values);
                values += 1;
                bindings.insert(param.id, BindingTarget::Value(value));
                // The header decides whether the caller retains the obligation
                // or transfers it to this body's normal and fault cleanup.
                let own = OwnKind::of_param(&ty, abi.passing, service.checked_facts.rows())?;
                Ok((
                    BlockArg { value, ty, own },
                    Binding {
                        id: crate::BindingId(u32::try_from(index).map_err(|_| {
                            "SIR source binding count exceeds u32".to_string()
                        })?),
                        name: param.name.clone(),
                        span: param.span.clone(),
                        mutable: param.mutable,
                        target: crate::BindingTarget::Value(value),
                    },
                ))
            })
            .collect::<Result<Vec<(BlockArg, Binding)>, String>>()?;
        let (mut params, source_bindings): (Vec<BlockArg>, Vec<Binding>) =
            params.into_iter().unzip();
        let stream_sink = stream.map(|element| {
            let sink = BlockArg {
                value: ValueId(values),
                ty: callable.signature.params[callable.signature.params.len() - 1]
                    .ty
                    .clone(),
                own: OwnKind::Owned,
            };
            values += 1;
            let seat = sink.value;
            params.push(sink);
            (seat, element)
        });
        let owned_live = params
            .iter()
            .filter(|param| param.own == OwnKind::Owned)
            .map(|param| (param.value, param.ty.clone()))
            .collect();
        let binding_declarations = source_params
            .iter()
            .enumerate()
            .map(|(index, param)| (param.id, index))
            .collect();
        let mut builder = Self {
            function,
            service,
            callable,
            substitution,
            blocks: vec![PendingBlock::new(entry, Vec::new())],
            current_site: None,
            current: entry,
            values,
            ops: 0,
            bindings,
            binding_declarations,
            owned_live,
            borrow_parents: HashMap::new(),
            scope_loans: Vec::new(),
            scope_loan_floors: vec![0],
            binding_loans: Vec::new(),
            ended_loans: std::collections::HashSet::new(),
            branch_depth: 0,
            scopes: vec![Vec::new()],
            source_bindings,
            params,
            loops: Vec::new(),
            places: Vec::new(),
            capture_places: HashMap::new(),
            argument_receiver_loans: Vec::new(),
            defers: Vec::new(),
            defer_bodies: Vec::new(),
            recovery_bodies: Vec::new(),
            task_scopes: Vec::new(),
            cleanup_may_fail: false,
            cleanup_draining: false,
            deferred_initialized: BTreeSet::new(),
            state_taken: BTreeSet::new(),
            stream_sink,
            dual_return: None,
        };
        builder.bind_captures(source)?;
        builder.bind_actor_state(source)?;
        builder.bind_private_value_parameters(&source_params)?;
        for parameter in &source_params {
            if let BindingTarget::Value(value) = builder.binding_target(parameter.id)? {
                if (parameter.mutable && builder.value_own_kind(value) == Some(OwnKind::None))
                    || builder.value_own_kind(value) == Some(OwnKind::Owned)
                {
                    let target = if parameter.mutable {
                        builder.acquire_local_target(value)?
                    } else {
                        builder.acquire_binding_target(value)?
                    };
                    builder.bindings.insert(parameter.id, target);
                    let declaration = builder.binding_declarations[&parameter.id];
                    builder.source_bindings[declaration].target = target;
                }
            }
            builder.declare_in_scope(parameter.id);
        }
        Ok(builder)
    }

    /// Mutable value parameters operate on private values. Use the canonical
    /// copy contract for both direct callables and their aggregate containers;
    /// the incoming borrowed ABI keeps the caller's value intact.
    pub(super) fn bind_private_value_parameters(
        &mut self,
        parameters: &[HirBinding],
    ) -> Result<(), String> {
        for parameter in parameters {
            if !parameter.mutable || parameter.is_consume {
                continue;
            }
            let ty = self.ty(&parameter.ty);
            let BindingTarget::Value(source) = self.binding_target(parameter.id)? else {
                continue;
            };
            if self.value_own_kind(source) != Some(OwnKind::Guaranteed) {
                continue;
            }
            self.service.require_type_facts(&ty)?;
            if self.service.checked_facts.rows()[&hew_types::TypeInstanceKey(ty.clone())].clone
                == hew_types::CloneKind::None
            {
                // Replacements own one parameter-scope slot. The incoming
                // borrow remains the readable value until this path assigns.
                let place = self.allocate_local(ty)?;
                let declaration = self.binding_declarations[&parameter.id];
                self.source_bindings[declaration].target = BindingTarget::Place(place);
                continue;
            }
            let copied = self.emit_typed(
                Provenance::Synthesized,
                &ty,
                SemOpKind::CopyValue {
                    source: Operand { value: source },
                },
            )?;
            let target = self.acquire_binding_target(copied)?;
            self.bindings.insert(parameter.id, target);
            let declaration = self.binding_declarations[&parameter.id];
            self.source_bindings[declaration].target = target;
        }
        Ok(())
    }

    pub(super) fn bind_captures(&mut self, source: &BodySource) -> Result<(), String> {
        let BodySource::Closure(expression) = source else {
            return Ok(());
        };
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            unreachable!()
        };
        let abi = &self.callable.signature.params[0];
        let own = OwnKind::of_param(&abi.ty, abi.passing, self.service.checked_facts.rows())?;
        self.params.insert(
            0,
            BlockArg {
                value: ValueId(0),
                ty: abi.ty.clone(),
                own,
            },
        );
        if own == OwnKind::Owned {
            self.owned_live.insert(ValueId(0), abi.ty.clone());
        }
        for (index, capture) in captures.iter().enumerate() {
            let field =
                u32::try_from(index).map_err(|_| "capture count exceeds u32".to_string())?;
            let place =
                PlaceId(u32::try_from(self.places.len()).map_err(|_| "place count exceeds u32")?);
            self.places.push(crate::PlaceDecl {
                id: place,
                ty: self.ty(&capture.ty),
                origin: crate::PlaceOrigin::Capture {
                    environment: ValueId(0),
                    field,
                },
            });
            self.capture_places.insert(capture.binding, place);
            self.bindings
                .insert(capture.binding, BindingTarget::Place(place));
            self.source_bindings.push(Binding {
                id: crate::BindingId(
                    u32::try_from(self.source_bindings.len())
                        .map_err(|_| "binding count exceeds u32".to_string())?,
                ),
                name: capture.name.clone(),
                span: expression.span.clone(),
                mutable: capture.access == hew_types::ClosureCaptureAccess::Var,
                target: crate::BindingTarget::Place(place),
            });
            let declaration = self.source_bindings.len() - 1;
            self.binding_declarations
                .insert(capture.binding, declaration);
            self.declare_in_scope(capture.binding);
        }
        for capture in captures {
            if capture.consumption == hew_types::ClosureCaptureConsumption::Consumed {
                let value = self.load_capture(capture.binding, Provenance::Synthesized, true)?;
                self.capture_places.remove(&capture.binding);
                let target = self.acquire_binding_target(value)?;
                self.bindings.insert(capture.binding, target);
                let declaration = self.binding_declarations[&capture.binding];
                self.source_bindings[declaration].target = target;
            }
        }
        Ok(())
    }

    pub(super) fn lower(mut self, source: BodySource) -> Result<SemFunction, String> {
        if self.callable.function != self.function.id
            || self.callable.declaration != self.function.declaration
        {
            return Err(
                "SIR callable provenance does not match the HIR function's checked identity"
                    .to_string(),
            );
        }
        if self.function.intrinsic_id.is_some() {
            return Err("floor intrinsic has no checked SIR operation contract".to_string());
        }
        match (
            &self.callable.instance,
            self.function.type_params.is_empty(),
        ) {
            (CallableInstance::Monomorphic | CallableInstance::SupervisorChild { .. }, true) => {}
            (CallableInstance::ActorMember, true) if matches!(source, BodySource::Actor { .. }) => {
            }
            (CallableInstance::Closure(_), _) if matches!(source, BodySource::Closure(_)) => {}
            (CallableInstance::EntryAdapter, _)
                if matches!(source, BodySource::EntryAdapter(_)) => {}
            (CallableInstance::Generic(key), false)
                if key.template.declaration == self.function.declaration
                    && key.type_args == self.substitution.args => {}
            _ => return Err(
                "SIR callable instance does not match its HIR template and semantic substitution"
                    .to_string(),
            ),
        }
        if matches!(source, BodySource::Function)
            && self.callable.signature.return_ty != self.ty(&self.function.return_ty)
        {
            return Err(format!(
                "SIR callable `{}` return type `{}` differs from substituted HIR template return `{}`",
                self.callable.symbol,
                self.callable.signature.return_ty.user_facing(),
                self.ty(&self.function.return_ty).user_facing()
            ));
        }
        self.enter_task_scope()?;
        let result = self.lower_source_body(source)?;
        let result = result
            .map(|operand| {
                self.coerce_value(
                    operand.value,
                    &self.callable.signature.return_ty.clone(),
                    Provenance::Synthesized,
                )
                .map(|value| Operand { value })
            })
            .transpose()?;
        if self.is_open() {
            self.finish_return_value(result.map(|operand| crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Move,
            }))?;
        }
        let blocks = std::mem::take(&mut self.blocks)
            .into_iter()
            .map(PendingBlock::into_sem_block)
            .collect::<Result<Vec<_>, _>>()?;
        let mut function = SemFunction {
            id: self.function.id,
            callable: self.callable.id,
            declaration: self.function.declaration,
            name: self.callable.symbol.clone(),
            span: self.function.span.clone(),
            source_origin: self.callable.source_origin.clone(),
            terminal_receiver: self.terminal_linear_receiver(),
            params: self.params,
            return_ty: self.callable.signature.return_ty.clone(),
            entry: BlockId(0),
            blocks,
            places: self.places,
            bindings: self.source_bindings,
        };
        tasks::remove_empty_scopes(&mut function);
        projection::complete_edge_partitions(
            &mut function,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        Ok(function)
    }

    /// A stream producer reads a snapshot of every state field it captures
    /// (§4.12): the copy is a mutable local of this turn, and the actor's own
    /// seat is untouched.
    pub(super) fn snapshot_stream_captures(
        &mut self,
        captures: &[hew_hir::HirGenCapture],
        state_bindings: &[HirBinding],
    ) -> Result<(), String> {
        for capture in captures {
            if capture.source != hew_hir::HirGenCaptureSource::ActorStateField {
                continue;
            }
            let binding = state_bindings
                .iter()
                .find(|binding| binding.id == capture.binding)
                .ok_or("stream producer captures an unknown state field")?;
            let BindingTarget::Place(place) = self.binding_target(binding.id)? else {
                return Err("stream producer state capture has no state seat".into());
            };
            let ty = self.ty(&capture.ty);
            self.service.require_type_facts(&ty)?;
            if self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].clone
                == hew_types::CloneKind::None
            {
                return Err(format!(
                    "stream producer cannot snapshot state field `{}`: its type has no copy",
                    binding.name
                ));
            }
            let value =
                self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::LoadCopy { place })?;
            self.bind_source_value(binding, value)?;
        }
        Ok(())
    }

    pub(super) fn lower_source_body(
        &mut self,
        source: BodySource,
    ) -> Result<Option<Operand>, String> {
        match source {
            BodySource::Actor { state_bindings, .. } if self.stream_sink.is_some() => {
                // HIR shapes a stream producer as a generator block; the body
                // runs as this actor turn and yields straight into the sink.
                let body = self.function.body.clone();
                let Some(HirExpr {
                    kind:
                        HirExprKind::GenBlock {
                            body: producer,
                            captures,
                            ..
                        },
                    ..
                }) = body.tail.as_deref()
                else {
                    return Err("stream producer body is not a generator block".into());
                };
                if !body.statements.is_empty() {
                    return Err("stream producer body carries statements outside its block".into());
                }
                self.snapshot_stream_captures(captures, &state_bindings)?;
                self.lower_block(producer, OwnedBindingUse::Return)
            }
            BodySource::Function | BodySource::Actor { .. } => {
                let body = self.function.body.clone();
                self.lower_block(&body, OwnedBindingUse::Return)
            }
            BodySource::EntryAdapter(adapter) => {
                self.lower_entry_adapter(&adapter)?;
                Ok(None)
            }
            BodySource::Closure(expression) => {
                let HirExprKind::Closure { body, ret_ty, .. } = &expression.kind else {
                    unreachable!()
                };
                if self.ty(ret_ty) != self.callable.signature.return_ty {
                    return Err("closure body return differs from its exact signature".to_string());
                }
                if matches!(self.ty(&body.ty), ResolvedTy::Unit | ResolvedTy::Never) {
                    self.lower_discarded_expr(body)?;
                    return Ok(None);
                }
                // A body block with no tail expression carries its value out
                // through a `return` instead, which already sealed the
                // callable's return. There is nothing to transfer here and the
                // divergence doctrine forbids inventing a placeholder, so the
                // body is complete without a tail: `{ ...; return v; }` and
                // `{ ...; v }` are the same callable.
                if let HirExprKind::Block(block) = &body.kind {
                    if block.tail.is_none() {
                        let block = block.clone();
                        self.lower_scoped_block(&block, OwnedBindingUse::Return)?;
                        return Ok(None);
                    }
                }
                Ok(Some(Operand {
                    value: lower_initial_value_transfer(
                        self,
                        body,
                        "closure body result",
                        OwnedBindingUse::Return,
                    )?,
                }))
            }
        }
    }

    /// Lower one HIR expression in a semantic operand position.
    ///
    /// The initial scalar SIR domain admits only read uses, but it still
    /// translates every HIR intent before rejecting a non-read mode. This
    /// prevents a source move/borrow/discharge from being silently weakened
    /// into a reusable SIR value during the migration.
    pub(super) fn lower_read_operand(
        &mut self,
        expr: &HirExpr,
        context: &str,
    ) -> Result<Operand, String> {
        require_initial_scalar_read(expr.intent)
            .map_err(|reason| format!("{context}: {reason}"))?;
        Ok(Operand {
            value: self.lower_expr(expr)?,
        })
    }

    /// A binding whose value has no copy operation. `Some` is the loan the
    /// binding names, read through that loan rather than transferred out of
    /// it; `None` means the binding owns its value and transfers normally.
    pub(super) fn read_bound_loan(
        &mut self,
        binding: BindingId,
        source: ValueId,
        binding_use: OwnedBindingUse,
    ) -> Result<Option<ValueId>, String> {
        self.require_selected_binding(binding, source)?;
        let name = self.source_bindings[self.binding_declarations[&binding]]
            .name
            .clone();
        if binding_use == OwnedBindingUse::Probe
            && self.value_own_kind(source) == Some(OwnKind::Guaranteed)
        {
            if self.ended_loans.contains(&source) {
                return Err(format!(
                    "E_OWN_CONSUME_BORROWED: `{name}` borrows a collection that was since mutated or drained; the loan ended there and cannot be read again"
                ));
            }
            return Ok(Some(source));
        }
        if self.value_own_kind(source) != Some(OwnKind::Owned) {
            return Err(format!(
                "E_OWN_CONSUME_BORROWED: `{name}` is borrowed here; a value with no copy operation transfers only from an owning binding"
            ));
        }
        Ok(None)
    }

    /// A match that only probes a copy-less state field reads it through a
    /// loan; the seat keeps its owner and the match ends the loan.
    pub(super) fn probe_state_field(
        &mut self,
        expr: &HirExpr,
        ty: &ResolvedTy,
    ) -> Result<Option<ValueId>, String> {
        let Some(place) = self.expression_projection(expr)? else {
            return Ok(None);
        };
        if !matches!(
            self.places[place.0 as usize].origin,
            crate::PlaceOrigin::ActorState { .. }
        ) {
            return Ok(None);
        }
        let value = self.emit_typed(
            Provenance::Site(expr.site),
            ty,
            SemOpKind::LoadBorrow { place },
        )?;
        self.scope_loans.push(value);
        Ok(Some(value))
    }

    pub(super) fn lower_owned_transfer(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        let mut expr = expr;
        while let HirExprKind::SubsumedValue { source } = &expr.kind {
            if self.ty(&source.ty) != self.ty(&expr.ty) {
                return Err("transparent value transfer must preserve its exact type".into());
            }
            expr = source;
        }
        let ty = self.ty(&expr.ty);
        let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
        let movable_owner = self
            .service
            .checked_facts
            .rows()
            .get(&TypeInstanceKey(ty.clone()))
            .is_some_and(|row| row.clone == hew_types::CloneKind::None);
        if own == OwnKind::Owned {
            let movable = movable_owner;
            if movable {
                if binding_use == OwnedBindingUse::Probe {
                    if let Some(value) = self.probe_state_field(expr, &ty)? {
                        return Ok(value);
                    }
                }
                if let Some(value) = self.lower_consuming_projection(expr)? {
                    return Ok(value);
                }
            }
            if let HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } = &expr.kind
            {
                match self.binding_target(*binding)? {
                    BindingTarget::Place(place) => {
                        let mut take = movable
                            || binding_use == OwnedBindingUse::Move
                            || (binding_use == OwnedBindingUse::Return && self.defers.is_empty());
                        if take {
                            if let Some((_, field)) = self.capture_field(*binding) {
                                if field.consumption
                                    != hew_types::ClosureCaptureConsumption::Consumed
                                {
                                    if movable {
                                        return Err("E_OWN_CONSUME_BORROWED: capture transfer requires consuming access".into());
                                    }
                                    take = false;
                                }
                            }
                        }
                        if let Some(drained) = self.drain_state_vec_seat(place, expr)? {
                            return Ok(drained);
                        }
                        take = take && !self.state_field_leaves_as_copy(place, expr)?;
                        if take && self.in_var_self_receiver(place) {
                            self.state_taken.insert(place);
                        }
                        return self.emit(
                            expr,
                            if take {
                                SemOpKind::LoadTake { place }
                            } else {
                                SemOpKind::LoadCopy { place }
                            },
                        );
                    }
                    BindingTarget::Value(source) => {
                        if movable {
                            if let Some(loan) =
                                self.read_bound_loan(*binding, source, binding_use)?
                            {
                                return Ok(loan);
                            }
                        }
                        return self.emit(
                            expr,
                            SemOpKind::CopyValue {
                                source: Operand { value: source },
                            },
                        );
                    }
                }
            }
        }
        let loan_floor = self.scope_loans.len();
        let source = self.lower_expr_with_binding_use(expr, binding_use)?;
        // A loan of a clone-free value has no owned copy to make: the binding
        // holds the loan and the wall against consuming it is the loan itself.
        if own == OwnKind::Owned
            && !movable_owner
            && self.value_own_kind(source) == Some(OwnKind::Guaranteed)
        {
            let copy = self.emit(
                expr,
                SemOpKind::CopyValue {
                    source: Operand { value: source },
                },
            )?;
            // This snapshot owns its contents. Loans created only to evaluate
            // the read no longer support a live result; keeping them would
            // falsely tie the snapshot to its collection across branches.
            // Loans that existed before this expression still belong to their
            // original binding or enclosing read and remain live.
            self.end_expression_loans(loan_floor)?;
            Ok(copy)
        } else {
            Ok(source)
        }
    }

    pub(super) fn capture_field(
        &self,
        binding: BindingId,
    ) -> Option<(crate::PlaceId, crate::SemCaptureField)> {
        let place = *self.capture_places.get(&binding)?;
        let PlaceOrigin::Capture { field, .. } = self.places.get(place.0 as usize)?.origin else {
            return None;
        };
        let CallableInstance::Closure(id) = self.callable.instance else {
            return None;
        };
        self.service
            .closures
            .get(id.0 as usize)?
            .fields
            .get(field as usize)
            .cloned()
            .map(|field| (place, field))
    }

    pub(super) fn load_capture(
        &mut self,
        binding: BindingId,
        provenance: Provenance,
        take: bool,
    ) -> Result<ValueId, String> {
        let (place, field) = self
            .capture_field(binding)
            .ok_or_else(|| "capture binding has no exact environment field".to_string())?;
        if take && field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
            return Err("capture extraction lacks checker-selected consuming access".to_string());
        }
        self.emit_typed(
            provenance,
            &field.ty,
            if take {
                SemOpKind::LoadTake { place }
            } else {
                SemOpKind::LoadCopy { place }
            },
        )
    }

    pub(super) fn lower_closure(&mut self, expression: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::Closure { captures, .. } = &expression.kind else {
            unreachable!()
        };
        let closure =
            self.service
                .request_closure(self.callable.id, expression, &self.substitution)?;
        let mut fields = Vec::with_capacity(captures.len());
        for capture in captures {
            let ty = self.ty(&capture.ty);
            let take = capture.acquisition == hew_types::ClosureCaptureAcquisition::Move;
            let provenance = Provenance::Site(expression.site);
            let target = self.binding_target(capture.binding)?;
            if self.target_ty(target)? != ty {
                return Err("closure acquisition changes its captured binding type".into());
            }
            let value = match target {
                BindingTarget::Place(place) => {
                    if take {
                        if let Some((_, field)) = self.capture_field(capture.binding) {
                            if field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
                                return Err("capture extraction lacks consuming access".into());
                            }
                        }
                    }
                    self.emit_typed(
                        provenance,
                        &ty,
                        if take {
                            SemOpKind::LoadTake { place }
                        } else {
                            SemOpKind::LoadCopy { place }
                        },
                    )?
                }
                BindingTarget::Value(source) => {
                    if take && self.value_own_kind(source) == Some(OwnKind::Owned) {
                        self.owned_live.remove(&source);
                        source
                    } else if OwnKind::of_ty(&ty, self.service.checked_facts.rows())?
                        == OwnKind::Owned
                    {
                        self.emit_typed(
                            provenance,
                            &ty,
                            SemOpKind::CopyValue {
                                source: Operand { value: source },
                            },
                        )?
                    } else {
                        source
                    }
                }
            };
            self.owned_live.remove(&value);
            fields.push(Operand { value });
        }
        self.emit(expression, SemOpKind::ClosureMake { closure, fields })
    }

    pub(super) fn coerce_value(
        &mut self,
        value: ValueId,
        target: &ResolvedTy,
        provenance: Provenance,
    ) -> Result<ValueId, String> {
        let source = self
            .value_ty(value)
            .ok_or_else(|| "coercion has no typed source value".to_string())?;
        if crate::call_boundary_types_match(&source, target) {
            return Ok(value);
        }
        if source.can_implicitly_numeric_normalize_to(target) {
            return self.emit_typed(
                provenance,
                target,
                SemOpKind::Cast {
                    value: Operand { value },
                    to: target.clone(),
                },
            );
        }
        self.service.require_type_facts(target)?;
        let generator = crate::generator_parts(&source).is_some();
        let verify = if generator {
            crate::verify_generator_coercion
        } else {
            crate::verify_callable_coercion
        };
        verify(&source, target, self.service.checked_facts.rows()).map_err(|reason| {
            format!("value coercion from {source:?} to {target:?} refused: {reason}")
        })?;
        self.owned_live.remove(&value);
        self.emit_typed(
            provenance,
            target,
            if generator {
                SemOpKind::GeneratorCoerce {
                    source: Operand { value },
                }
            } else {
                SemOpKind::CallableCoerce {
                    source: Operand { value },
                }
            },
        )
    }

    /// A pattern binding names its probed payload until the arm is selected;
    /// an owning payload transfers exactly once, after every guard has passed.
    pub(super) fn require_selected_binding(
        &self,
        binding: BindingId,
        value: ValueId,
    ) -> Result<(), String> {
        let borrowed = self.value_own_kind(value) == Some(OwnKind::Guaranteed);
        if borrowed
            || self.value_own_kind(value) == Some(OwnKind::Owned)
            || self.argument_receiver_loans.contains(&value)
        {
            let name = &self.source_bindings[self.binding_declarations[&binding]].name;
            if borrowed {
                // A loaned payload never becomes an owner, in a guard or in the
                // arm body: the consume wall, not the guard rule, is what it
                // meets.
                if !self.argument_receiver_loans.contains(&value) {
                    return Ok(());
                }
                return Err(format!(
                    "E_OWN_CONSUME_BORROWED: `{name}` is borrowed here; a value with no copy operation transfers only from an owning binding"
                ));
            }
            return Err(format!(
                "E_OWN_GUARD_CONSUME: match guard consumes pattern binding `{name}`; a guard can only read its bindings"
            ));
        }
        Ok(())
    }

    /// A value is defined once, so the newest block that names it is the one
    /// that defines it. Searching from the newest block keeps a lookup close to
    /// the operation that just produced the value; searching forward costs the
    /// square of the body size on a function with hundreds of bindings.
    pub(super) fn value_own_kind(&self, value: ValueId) -> Option<OwnKind> {
        self.params
            .iter()
            .find(|param| param.value == value)
            .map(|param| param.own)
            .or_else(|| {
                self.blocks
                    .iter()
                    .rev()
                    .flat_map(|block| block.args.iter())
                    .find(|arg| arg.value == value)
                    .map(|arg| arg.own)
            })
            .or_else(|| {
                self.blocks
                    .iter()
                    .rev()
                    .flat_map(|block| block.ops.iter())
                    .flat_map(|op| op.results.iter())
                    .find(|result| result.id == value)
                    .map(|result| result.own)
            })
    }

    pub(super) fn value_ty(&self, value: ValueId) -> Option<ResolvedTy> {
        self.params
            .iter()
            .find(|param| param.value == value)
            .map(|param| param.ty.clone())
            .or_else(|| {
                self.blocks
                    .iter()
                    .rev()
                    .flat_map(|block| block.args.iter())
                    .find(|arg| arg.value == value)
                    .map(|arg| arg.ty.clone())
            })
            .or_else(|| {
                self.blocks
                    .iter()
                    .rev()
                    .flat_map(|block| block.ops.iter())
                    .flat_map(|op| op.results.iter())
                    .find(|result| result.id == value)
                    .map(|result| result.ty.clone())
            })
            .or_else(|| {
                self.blocks.iter().rev().find_map(|block| {
                    let mut found = None;
                    block.terminator.as_ref()?.visit_results(|result| {
                        if result.id == value {
                            found = Some(result.ty.clone());
                        }
                    });
                    found
                })
            })
    }

    pub(super) fn record_binding_version(
        &mut self,
        binding: BindingId,
        value: ValueId,
    ) -> Result<(), String> {
        let declaration = *self.binding_declarations.get(&binding).ok_or_else(|| {
            format!("binding `{binding}` has no source declaration in SIR lowering")
        })?;
        let source = self.source_bindings[declaration].clone();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(self.source_bindings.len())
                    .map_err(|_| "SIR source binding count exceeds u32".to_string())?,
            ),
            name: source.name,
            span: source.span,
            mutable: source.mutable,
            target: crate::BindingTarget::Value(value),
        });
        Ok(())
    }

    pub(super) fn bind_source_value(
        &mut self,
        binding: &HirBinding,
        value: ValueId,
    ) -> Result<(), String> {
        if binding.mutable && self.value_own_kind(value) == Some(OwnKind::Guaranteed) {
            // Like an affine borrowed parameter, the incoming loan remains the
            // readable value; only a replacement initializes this local owner.
            let ty = self.value_ty(value).ok_or("borrowed binding has no type")?;
            let place = self.allocate_local(ty)?;
            self.bind_source_target(binding, BindingTarget::Place(place))?;
            self.bindings
                .insert(binding.id, BindingTarget::Value(value));
            return Ok(());
        }
        let target = if binding.mutable {
            self.acquire_local_target(value)?
        } else {
            self.acquire_binding_target(value)?
        };
        self.bind_source_target(binding, target)
    }

    pub(super) fn bind_source_target(
        &mut self,
        binding: &HirBinding,
        target: BindingTarget,
    ) -> Result<(), String> {
        let declaration = self.source_bindings.len();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?,
            ),
            name: binding.name.clone(),
            span: binding.span.clone(),
            mutable: binding.mutable,
            target,
        });
        self.binding_declarations.insert(binding.id, declaration);
        self.bindings.insert(binding.id, target);
        self.declare_in_scope(binding.id);
        Ok(())
    }

    pub(super) fn mutable_bindings(&self) -> Vec<BindingId> {
        let mut bindings = self
            .binding_declarations
            .iter()
            .filter_map(|(binding, &index)| {
                (self.source_bindings[index].mutable
                    && matches!(self.bindings.get(binding), Some(BindingTarget::Value(_))))
                .then_some(*binding)
            })
            .collect::<Vec<_>>();
        bindings.sort_unstable();
        bindings
    }

    pub(super) fn terminal_linear_receiver(&self) -> Option<ValueId> {
        let receiver = self.function.terminal_receiver?;
        let index = self
            .function
            .params
            .iter()
            .position(|param| param.id == receiver)?;
        let param = self.params.get(index)?;
        (self
            .service
            .checked_facts
            .declaration_marker(&param.ty)
            .ok()
            == Some(hew_types::DeclarationMarker::Linear))
        .then_some(param.value)
    }

    pub(super) fn emit_destroy(&mut self, value: ValueId) -> Result<(), String> {
        if let Some(ty) = self.value_ty(value) {
            self.note_release_may_fault(&ty);
        }
        let id = OpId(self.ops);
        self.current_block_mut().append_op(SemOp {
            id,
            results: Vec::new(),
            kind: SemOpKind::DestroyValue {
                value: Operand { value },
            },
            provenance: Provenance::Synthesized,
        })?;
        self.ops += 1;
        self.owned_live.remove(&value);
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }

    pub(super) fn destroy_all_live(&mut self) -> Result<(), String> {
        // Keep the lexical stack: another generated continuation can still
        // finish the enclosing call's argument evaluation normally.
        self.end_call_loans(&self.argument_receiver_loans.clone())?;
        let values: Vec<_> = self.owned_live.keys().copied().collect();
        for value in values.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        self.end_scopes(0)
    }

    pub(super) fn control_state(&self) -> ControlState {
        ControlState {
            block: self.current,
            bindings: self.bindings.clone(),
            binding_declarations: self.binding_declarations.clone(),
            owned_live: self.owned_live.clone(),
            loans: self.argument_receiver_loans.clone(),
            scopes: self.scopes.clone(),
            scope_loans: self.scope_loans.clone(),
            scope_loan_floors: self.scope_loan_floors.clone(),
            ended_loans: self.ended_loans.clone(),
            defers: self.defers.clone(),
            task_scopes: self.task_scopes.clone(),
            cleanup_may_fail: self.cleanup_may_fail,
            cleanup_draining: self.cleanup_draining,
            deferred_initialized: self.deferred_initialized.clone(),
            state_taken: self.state_taken.clone(),
        }
    }

    pub(super) fn restore_control_state(&mut self, state: &ControlState) {
        self.current = state.block;
        self.bindings.clone_from(&state.bindings);
        self.binding_declarations
            .clone_from(&state.binding_declarations);
        self.owned_live = state.owned_live.clone();
        self.argument_receiver_loans.clone_from(&state.loans);
        self.scopes.clone_from(&state.scopes);
        self.scope_loans.clone_from(&state.scope_loans);
        self.scope_loan_floors.clone_from(&state.scope_loan_floors);
        self.ended_loans.clone_from(&state.ended_loans);
        self.defers.clone_from(&state.defers);
        self.task_scopes.clone_from(&state.task_scopes);
        self.cleanup_may_fail = state.cleanup_may_fail;
        self.cleanup_draining = state.cleanup_draining;
        self.deferred_initialized
            .clone_from(&state.deferred_initialized);
        self.state_taken.clone_from(&state.state_taken);
    }

    pub(super) fn retain_bindings(
        bindings: &HashMap<BindingId, BindingTarget>,
        retained: &std::collections::HashSet<BindingId>,
    ) -> HashMap<BindingId, BindingTarget> {
        bindings
            .iter()
            .filter(|(binding, _)| retained.contains(binding))
            .map(|(binding, value)| (*binding, *value))
            .collect()
    }

    /// End the loans opened since `depth`, innermost first.
    pub(super) fn end_loans_since(&mut self, depth: usize) -> Result<(), String> {
        let loans = self.argument_receiver_loans.split_off(depth);
        self.end_call_loans(&loans)
    }

    pub(super) fn cleanup_match_candidate(
        &mut self,
        root_live: &BTreeMap<ValueId, ResolvedTy>,
        root_loans: usize,
        outer_bindings: &std::collections::HashSet<BindingId>,
    ) -> Result<(), String> {
        self.end_loans_since(root_loans)?;
        let keep = root_live
            .iter()
            .filter(|(value, _)| self.owned_live.contains_key(value))
            .map(|(value, ty)| (*value, ty.clone()))
            .collect();
        self.destroy_live_since(&keep)?;
        let leaving = self
            .scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev())
            .filter(|binding| !outer_bindings.contains(binding))
            .copied()
            .collect::<Vec<_>>();
        for binding in leaving {
            self.end_binding_scope(binding)?;
        }
        for scope in &mut self.scopes {
            scope.retain(|binding| outer_bindings.contains(binding));
        }
        self.bindings = Self::retain_bindings(&self.bindings, outer_bindings);
        self.binding_declarations
            .retain(|binding, _| outer_bindings.contains(binding));
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }

    pub(super) fn merge_control_states(&mut self, states: Vec<ControlState>) -> Result<(), String> {
        let Some(first) = states.first() else {
            return Err("control-flow join has no live predecessor".to_string());
        };
        if states.len() == 1 {
            self.restore_control_state(first);
            return Ok(());
        }
        let edge_args = vec![Vec::new(); states.len()];
        self.join_control_states(states, Vec::new(), edge_args)
    }

    pub(super) fn join_control_states(
        &mut self,
        states: Vec<ControlState>,
        mut block_args: Vec<BlockArg>,
        mut edge_args: Vec<Vec<Operand>>,
    ) -> Result<(), String> {
        let first = states
            .first()
            .ok_or_else(|| "control-flow join has no predecessor".to_string())?;
        if edge_args.len() != states.len() {
            return Err("control-flow join has inconsistent edge metadata".into());
        }
        let keys = first.bindings.keys().copied().collect::<BTreeSet<_>>();
        if states.iter().any(|state| {
            state.bindings.keys().copied().collect::<BTreeSet<_>>() != keys
                || state.binding_declarations != first.binding_declarations
                || state.scopes != first.scopes
        }) {
            return Err("control-flow predecessors expose different lexical declarations".into());
        }
        if states
            .iter()
            .any(|state| state.owned_live != first.owned_live)
        {
            return Err("control-flow predecessors leave different temporary owners live".into());
        }
        if states
            .iter()
            .any(|state| state.deferred_initialized != first.deferred_initialized)
        {
            return Err(
                "control-flow predecessors disagree on which deferred actor fields are initialized"
                    .into(),
            );
        }
        if states
            .iter()
            .any(|state| state.state_taken != first.state_taken)
        {
            return Err(
                "control-flow predecessors disagree on consumed actor state or receiver fields"
                    .into(),
            );
        }
        for binding in &keys {
            if states
                .iter()
                .any(|state| matches!(state.bindings[binding], BindingTarget::Place(_)))
                && states
                    .iter()
                    .any(|state| state.bindings[binding] != first.bindings[binding])
            {
                return Err("lexical place identity changed across a control-flow edge".into());
            }
        }
        let mut joined = first.clone();
        joined.cleanup_may_fail = states.iter().any(|state| state.cleanup_may_fail);
        self.binding_declarations
            .clone_from(&first.binding_declarations);
        self.bindings.clone_from(&first.bindings);
        for binding in self.mutable_bindings() {
            let values = states
                .iter()
                .map(|state| match state.bindings[&binding] {
                    BindingTarget::Value(value) => Ok(value),
                    BindingTarget::Place(_) => {
                        Err("scalar join received a place binding".to_string())
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            let ty = self
                .value_ty(values[0])
                .ok_or_else(|| "scalar join value has no type".to_string())?;
            if values.iter().any(|value| {
                self.value_ty(*value).as_ref() != Some(&ty)
                    || self.value_own_kind(*value) == Some(OwnKind::Owned)
            }) {
                return Err("scalar binding join has inconsistent type or ownership".into());
            }
            let own = self
                .value_own_kind(values[0])
                .ok_or_else(|| "scalar join has no ownership facts".to_string())?;
            let value = self.fresh_value();
            block_args.push(BlockArg { value, ty, own });
            for (args, value) in edge_args.iter_mut().zip(values) {
                args.push(Operand { value });
            }
            joined.bindings.insert(binding, BindingTarget::Value(value));
            self.record_binding_version(binding, value)?;
        }
        let join = self.new_block(block_args);
        for (state, args) in states.into_iter().zip(edge_args) {
            self.current = state.block;
            self.set_terminator(SemTerminator::Goto(Edge { target: join, args }))?;
        }
        joined.block = join;
        self.restore_control_state(&joined);
        Ok(())
    }
}
