//! Call, dyn-dispatch and runtime-operation lowering.

use super::{
    collection_type_arguments, dyn_boundary_passing, dyn_receiver_passing, evaluation_sequence,
    is_initial_value_type, lower_initial_value_transfer, positional_call_target,
    require_initial_scalar_read, require_type_facts, BindingTarget, BlockArg, Builder, CallResult,
    CallTarget, CallUnwind, Edge, FaultHandback, HirExpr, HirExprKind, IntentKind, OpId, Operand,
    OwnKind, OwnedBindingUse, PlaceId, PlaceOrigin, PreparedCallee, Provenance, ResolvedRef,
    ResolvedTy, SemAbiParam, SemCallableKind, SemOp, SemOpKind, SemParamPassing, SemSignature,
    SemTerminator, TypeInstanceKey, ValueDef, ValueId, WritableRoot,
};

impl Builder<'_, '_> {
    /// These expressions cannot consume a prior argument's owner or branch to
    /// cleanup while a call-local loan is open. Other evaluation requires a
    /// snapshot of copyable arguments or a protected loan of affine arguments.
    pub(super) fn stable_argument_read(expr: &HirExpr) -> bool {
        match &expr.kind {
            HirExprKind::Literal(_) | HirExprKind::BindingRef { .. } => true,
            HirExprKind::FieldAccess { object, .. } => Self::stable_argument_read(object),
            HirExprKind::TupleIndex { tuple, .. } => Self::stable_argument_read(tuple),
            HirExprKind::SubsumedValue { source, .. } => Self::stable_argument_read(source),
            _ => false,
        }
    }

    /// A call-local projection keeps its immediate parent live. Recursing over
    /// a field chain therefore protects its root without copying intermediate
    /// owning records. Whole-value operands already have the call's borrow
    /// boundary and do not need an additional projection loan.
    #[expect(
        clippy::too_many_lines,
        reason = "one borrow boundary covers bindings, projections and computed supervisor roles"
    )]
    pub(super) fn lower_borrowed_read(
        &mut self,
        expr: &HirExpr,
        loans: &mut Vec<ValueId>,
    ) -> Result<Operand, String> {
        // Declared child access computes a stable role; it does not borrow a
        // field from the supervisor handle's physical representation.
        if self
            .service
            .module
            .supervisor_child_slots
            .contains_key(&expr.site)
        {
            return self.lower_expr(expr).map(|value| Operand { value });
        }
        if let Some(place) = self.expression_projection(expr)? {
            let owning = OwnKind::of_ty(&self.ty(&expr.ty), self.service.checked_facts.rows())?
                == OwnKind::Owned;
            let kind = if owning {
                SemOpKind::LoadBorrow { place }
            } else {
                SemOpKind::LoadCopy { place }
            };
            let value = self.emit(expr, kind)?;
            if owning {
                loans.push(value);
            }
            return Ok(Operand { value });
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &expr.kind
        {
            if let Some((place, field)) = self.capture_field(*binding) {
                if OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                    let value = self.emit_typed(
                        Provenance::Site(expr.site),
                        &field.ty,
                        SemOpKind::LoadBorrow { place },
                    )?;
                    loans.push(value);
                    return Ok(Operand { value });
                }
                return Ok(Operand {
                    value: self.load_capture(*binding, Provenance::Site(expr.site), false)?,
                });
            }
            if let BindingTarget::Value(value) = self.binding_target(*binding)? {
                if self.ended_loans.contains(&value) {
                    return Err("E_OWN_CONSUME_BORROWED: call argument names an ended loan".into());
                }
                return Ok(Operand { value });
            }
        }
        let (object, shape, field) = match &expr.kind {
            HirExprKind::FieldAccess { object, field } => {
                require_initial_scalar_read(expr.intent)?;
                let (shape, field) = self.aggregate_projection_shape(expr, object, field)?;
                (object.as_ref(), shape, field)
            }
            HirExprKind::TupleIndex { tuple, index } => {
                require_initial_scalar_read(expr.intent)?;
                let field = self.tuple_projection_index(expr, tuple, *index)?;
                let ty = self.ty(&tuple.ty);
                if is_initial_value_type(&ty) {
                    return self.lower_read_operand(expr, "borrowed call argument");
                }
                (
                    tuple.as_ref(),
                    self.service.require_aggregate_shape(&ty)?,
                    field,
                )
            }
            HirExprKind::SubsumedValue { source, .. } => {
                if self.ty(&source.ty) != self.ty(&expr.ty) {
                    return Err("transparent borrowed value must preserve its exact type".into());
                }
                return self.lower_borrowed_read(source, loans);
            }
            _ if expr.intent == IntentKind::Read => {
                return self.lower_read_operand(expr, "borrowed call argument")
            }
            _ => {
                return Ok(Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "borrowed call argument",
                        OwnedBindingUse::Copy,
                    )?,
                })
            }
        };
        let aggregate = self.lower_borrowed_read(object, loans)?;
        let owning = OwnKind::of_ty(&self.ty(&expr.ty), self.service.checked_facts.rows())?
            == OwnKind::Owned;
        let kind = if owning {
            SemOpKind::AggregateProjectBorrow {
                shape,
                aggregate,
                field,
            }
        } else {
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            }
        };
        let value = self.emit(expr, kind)?;
        if owning {
            loans.push(value);
        }
        Ok(Operand { value })
    }

    /// End the loans a `let` binding holds on `root` before the collection
    /// they borrow is taken. The take is past the binding's last use, so the
    /// loan ends here; a read of the binding afterwards is refused by name.
    pub(super) fn end_binding_loans_on(&mut self, root: crate::OwnerRoot) -> Result<(), String> {
        let mut ending = Vec::new();
        for group in &self.binding_loans {
            if group.root != root {
                continue;
            }
            // A loan its scope already ended is not this take's concern.
            let live: Vec<ValueId> = group
                .loans
                .iter()
                .copied()
                .filter(|loan| !self.ended_loans.contains(loan) && self.scope_loans.contains(loan))
                .collect();
            if live.is_empty() {
                continue;
            }
            // Ending it inside a branch or a loop would leave it live on the
            // sibling path, so a take there meets the loan instead.
            if group.loop_depth != self.loops.len() || group.branch_depth != self.branch_depth {
                return Err(
                    "E_OWN_CONSUME_BORROWED: this collection is borrowed by a live element \
                     loan; the loop or read holding it must end before the collection is \
                     mutated or drained"
                        .to_string(),
                );
            }
            ending.extend(live);
        }
        if ending.is_empty() {
            return Ok(());
        }
        self.end_call_loans(&ending)?;
        self.ended_loans.extend(ending);
        Ok(())
    }

    pub(super) fn end_call_loans(&mut self, loans: &[ValueId]) -> Result<(), String> {
        for &value in loans.iter().rev() {
            // A binding's loan may have ended at its last use already.
            if self.ended_loans.contains(&value) {
                continue;
            }
            let op = SemOp {
                id: OpId(self.ops),
                results: Vec::new(),
                kind: SemOpKind::EndBorrow {
                    borrow: Operand { value },
                },
                provenance: Provenance::Synthesized,
            };
            self.current_block_mut().append_op(op)?;
            self.ops += 1;
        }
        Ok(())
    }

    /// Capture an earlier value before a later argument can replace its owner.
    /// An affine argument cannot be snapshotted: its loan remains live during
    /// later evaluation, which must not consume or replace the borrowed owner.
    pub(super) fn lower_call_read(
        &mut self,
        expr: &HirExpr,
        loans: &mut Vec<ValueId>,
        later_arguments_are_stable: bool,
        can_borrow_projection: bool,
    ) -> Result<Operand, String> {
        let scope_loan_floor = self.scope_loans.len();
        let call_loan_floor = loans.len();
        let ty = self.ty(&expr.ty);
        self.service.require_type_facts(&ty)?;
        let affine = self.service.checked_facts.rows()[&TypeInstanceKey(ty)].clone
            == hew_types::CloneKind::None;
        let operand = if !later_arguments_are_stable && !affine {
            Ok(Operand {
                value: lower_initial_value_transfer(
                    self,
                    expr,
                    "call argument snapshot",
                    OwnedBindingUse::Copy,
                )?,
            })
        } else if can_borrow_projection || affine {
            let mut operand = self.lower_borrowed_read(expr, loans)?;
            if affine && self.value_own_kind(operand.value) == Some(OwnKind::Owned) {
                operand.value = self.emit(
                    expr,
                    SemOpKind::BeginBorrow {
                        owner: operand.clone(),
                    },
                )?;
                loans.push(operand.value);
            }
            Ok(operand)
        } else {
            self.lower_read_operand(expr, "call argument")
        }?;
        // Interior loans created while evaluating an argument belong to this
        // call. An independent return cannot keep them alive in the caller's
        // lexical scope; a borrowed runtime result explicitly promotes the
        // call's loans when it needs them.
        if self.scope_loans.len() > scope_loan_floor {
            let interior = self.scope_loans.split_off(scope_loan_floor);
            // New projected field loans depend on these interior parents.
            loans.splice(call_loan_floor..call_loan_floor, interior);
        }
        Ok(operand)
    }

    pub(super) fn lower_value_equality(
        &mut self,
        expr: &HirExpr,
        args: [&HirExpr; 2],
    ) -> Result<ValueId, String> {
        let ty = self.ty(&args[0].ty);
        if self.ty(&args[1].ty) != ty || self.ty(&expr.ty) != ResolvedTy::Bool {
            return Err("selected equality requires matching operands and a bool result".into());
        }
        self.service
            .require_value_capability(&ty, hew_types::ValueCapability::Eq)?;
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let mut lowered_args = Vec::new();
        let argument_loan_depth = self.argument_receiver_loans.len();
        for (index, arg) in args.iter().enumerate() {
            let loan_floor = loans.len();
            let stable_tail = args[index + 1..]
                .iter()
                .all(|arg| Self::stable_argument_read(arg));
            let operand = self.lower_call_read(arg, &mut loans, stable_tail, true)?;
            lowered_args.push(crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Borrow,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(argument_loan_depth);
        let live_at_call = self.owned_live.clone();
        let argument_temporaries: Vec<_> = live_at_call
            .keys()
            .filter(|value| !live_before_arguments.contains(value))
            .copied()
            .collect();
        let raw = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }]);
        let unwind = self.new_block(Vec::new());
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::ValueCall {
            id,
            ty,
            capability: hew_types::ValueCapability::Eq,
            args: lowered_args,
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: CallUnwind::Cleanup(Edge {
                target: unwind,
                args: Vec::new(),
            }),
        })?;
        self.current = unwind;
        self.owned_live = live_at_call.clone();
        self.end_call_loans(&loans)?;
        self.finish_fault_exit()?;
        self.current = normal;
        self.owned_live = live_at_call;
        self.end_call_loans(&loans)?;
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(continuation)
    }

    /// A projected consume needs a verified transfer of the selected field.
    /// Check before evaluating the receiver: ordinary projection lowering is a
    /// copy and must never manufacture an owner for a consuming field call.
    pub(super) fn reject_projected_callable_consume(&self, callee: &HirExpr) -> Result<(), String> {
        let mut root = callee;
        let mut projected = false;
        loop {
            root = match &root.kind {
                HirExprKind::SubsumedValue { source } => source,
                HirExprKind::FieldAccess { object, .. } => {
                    projected = true;
                    object
                }
                HirExprKind::TupleIndex { tuple, .. } => {
                    projected = true;
                    tuple
                }
                _ => break,
            };
        }
        if !projected {
            return Ok(());
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &root.kind
        {
            if self
                .bindings
                .get(binding)
                .is_some_and(|target| matches!(target, BindingTarget::Value(value) if self.value_own_kind(*value) == Some(OwnKind::Guaranteed)))
            {
                return Err("E_OWN_CONSUME_BORROWED: a borrowed aggregate field cannot be consumed; acquire an owned aggregate and destructure it first".into());
            }
        }
        Err("E_OWN_PARTIAL_CONSUME: a live aggregate field cannot be consumed; destructure the aggregate into owning bindings before calling the once field".into())
    }

    /// A state field the body cannot publish back leaves as a copy, so the seat
    /// keeps its value for the actor's lifetime. A field without a copy cannot
    /// leave that way.
    pub(super) fn state_field_leaves_as_copy(
        &mut self,
        place: PlaceId,
        expression: &HirExpr,
    ) -> Result<bool, String> {
        if !matches!(
            self.places[place.0 as usize].origin,
            crate::PlaceOrigin::ActorState { .. }
        ) {
            return Ok(false);
        }
        let ty = self.ty(&expression.ty);
        self.service.require_type_facts(&ty)?;
        if self.service.checked_facts.rows()[&TypeInstanceKey(ty)].clone
            == hew_types::CloneKind::None
        {
            if expression.intent == IntentKind::Consume
                && self.binding_root_is_mutable(expression)?
            {
                return Ok(false);
            }
            return Err(
                "an actor state field without a copy must be consumed from a mutable seat".into(),
            );
        }
        Ok(true)
    }

    /// A state seat the runtime consumed on a failure edge keeps whatever the
    /// take left in it until the actor's release reads the field, so its
    /// carrier must have an empty form that release accepts.
    ///
    /// Map and set carriers do: their release treats an empty carrier as an
    /// empty collection. Nothing else reaches here today, and a family that
    /// starts to must say what its empty carrier means before it does.
    pub(super) fn require_empty_carrier_seat(&mut self, place: PlaceId) -> Result<(), String> {
        let ty = self.places[place.0 as usize].ty.clone();
        if matches!(
            collection_type_arguments(&ty),
            Some((
                hew_types::BuiltinType::HashMap | hew_types::BuiltinType::HashSet,
                _
            ))
        ) {
            return Ok(());
        }
        Err(format!(
            "an actor state field of type `{}` cannot be taken by a call that keeps nothing on \
             its failure edge: its carrier has no empty form the actor's release accepts",
            ty.user_facing()
        ))
    }

    /// A `Vec` state seat consumed by value is drained rather than copied: the
    /// buffer moves to the consumer and the seat keeps a valid empty vector
    /// with its element representation intact, which a later dispatch refills.
    pub(super) fn drain_state_vec_seat(
        &mut self,
        place: PlaceId,
        expression: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        if expression.intent != IntentKind::Consume
            || !matches!(
                self.places[place.0 as usize].origin,
                crate::PlaceOrigin::ActorState { .. }
            )
            || !matches!(
                self.ty(&expression.ty),
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Vec),
                    ..
                }
            )
            || !self.binding_root_is_mutable(expression)?
        {
            return Ok(None);
        }
        self.lower_runtime_operation_with(
            expression,
            hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::TakeAll),
            (&[expression], &[]),
            true,
            &[],
        )
    }

    /// Whether a place is a capture of a body that borrows its environment.
    /// The environment keeps the field after this call, so it must be whole
    /// at every exit.
    pub(super) fn is_borrowed_capture(&self, place: PlaceId) -> bool {
        matches!(self.places[place.0 as usize].origin, PlaceOrigin::Capture { environment, .. }
        if self.params.iter().any(|param| {
            param.value == environment && param.own != OwnKind::Owned
        }))
    }

    /// Whether a place is a `var self` method's receiver seat or lies beneath
    /// it. Its method hands that seat back when it fails, so the seat must be
    /// whole wherever the method can fail.
    pub(super) fn in_var_self_receiver(&self, place: PlaceId) -> bool {
        let Some(binding) = self.function.var_self_receiver else {
            return false;
        };
        if !self.callable.signature.hands_back_receiver() {
            return false;
        }
        let Ok(BindingTarget::Place(receiver)) = self.binding_target(binding) else {
            return false;
        };
        let mut current = place;
        loop {
            if current == receiver {
                return true;
            }
            match self.places[current.0 as usize].origin {
                PlaceOrigin::Aggregate {
                    base: crate::PlaceBase::Place(base),
                    ..
                } => current = base,
                _ => return false,
            }
        }
    }

    /// Whether the binding this expression is rooted at was declared mutable.
    pub(super) fn binding_root_is_mutable(&mut self, expression: &HirExpr) -> Result<bool, String> {
        let Some(place) = self.resolve_binding_place(expression)? else {
            return Ok(false);
        };
        Ok(self
            .binding_declarations
            .get(&place.binding)
            .is_some_and(|declaration| self.source_bindings[*declaration].mutable))
    }

    /// Taking a field from an owned temporary transfers its siblings into the
    /// existing cleanup relation. No temporary container remains to own them.
    pub(super) fn lower_consuming_projection(
        &mut self,
        expression: &HirExpr,
    ) -> Result<Option<ValueId>, String> {
        if let Some(place) = self.expression_projection(expression)? {
            if let Some(drained) = self.drain_state_vec_seat(place, expression)? {
                return Ok(Some(drained));
            }
            let kind = if self.state_field_leaves_as_copy(place, expression)? {
                SemOpKind::LoadCopy { place }
            } else {
                if matches!(
                    self.places[place.0 as usize].origin,
                    crate::PlaceOrigin::ActorState { .. }
                ) || self.in_var_self_receiver(place)
                {
                    self.state_taken.insert(place);
                }
                SemOpKind::LoadTake { place }
            };
            return self.emit(expression, kind).map(Some);
        }
        let mut root = expression;
        let mut projections = Vec::new();
        loop {
            let (object, shape, field) = match &root.kind {
                HirExprKind::SubsumedValue { source } => {
                    root = source;
                    continue;
                }
                HirExprKind::FieldAccess { object, field } => {
                    let (shape, field) = self.aggregate_projection_shape(root, object, field)?;
                    (object.as_ref(), shape, field)
                }
                HirExprKind::TupleIndex { tuple, index } => {
                    let field = self.tuple_projection_index(root, tuple, *index)?;
                    let shape = self.service.require_aggregate_shape(&self.ty(&tuple.ty))?;
                    (tuple.as_ref(), shape, field)
                }
                _ => break,
            };
            projections.push((self.ty(&object.ty), shape, field));
            root = object;
        }
        if projections.is_empty() {
            return Ok(None);
        }
        if matches!(root.kind, HirExprKind::BindingRef { .. }) {
            // Local roots need persistent projected-place availability.
            self.reject_projected_callable_consume(expression)?;
        }
        let mut value = self.lower_expr_with_binding_use(root, OwnedBindingUse::Move)?;
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err(
                "E_OWN_CONSUME_BORROWED: projected consumption requires an owned aggregate".into(),
            );
        }
        for (ty, shape, field) in projections.into_iter().rev() {
            let fields =
                self.emit_destructure_value(value, &ty, shape, Provenance::Site(expression.site))?;
            value = fields[usize::try_from(field).map_err(|_| "aggregate field exceeds usize")?].id;
        }
        Ok(Some(value))
    }

    /// Transfer a receiver or argument before evaluating later arguments. Its
    /// new owner remains live for argument-failure cleanup until the call starts.
    pub(super) fn lower_consuming_value(&mut self, argument: &HirExpr) -> Result<ValueId, String> {
        self.require_consuming_capture(argument)?;
        let mut source = argument;
        while let HirExprKind::SubsumedValue { source: inner } = &source.kind {
            source = inner;
        }
        let value = match self.lower_consuming_projection(source)? {
            Some(value) => value,
            None => self.lower_expr_with_binding_use(source, OwnedBindingUse::Move)?,
        };
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err("E_OWN_CONSUME_BORROWED: a consuming argument requires an owned value; declare the forwarding parameter consume".into());
        }
        self.owned_live.remove(&value);
        self.emit(
            argument,
            SemOpKind::Move {
                source: Operand { value },
            },
        )
    }

    /// Lower one adopted runtime operand that still has a copy recipe into the
    /// independent owner the operation takes. The binding is read as a copy, so
    /// the caller keeps its own value; that copy transfers rather than being
    /// cloned again inside the operation and destroyed on the normal edge.
    pub(super) fn lower_adopted_copy(&mut self, argument: &HirExpr) -> Result<ValueId, String> {
        let value = lower_initial_value_transfer(
            self,
            argument,
            "runtime operand adoption",
            OwnedBindingUse::Copy,
        )?;
        if self.value_own_kind(value) != Some(OwnKind::Owned) {
            return Err("an adopted runtime operand requires an owned value".into());
        }
        self.owned_live.remove(&value);
        self.emit(
            argument,
            SemOpKind::Move {
                source: Operand { value },
            },
        )
    }

    pub(super) fn require_consuming_capture(&self, expression: &HirExpr) -> Result<(), String> {
        let mut source = expression;
        while let HirExprKind::SubsumedValue { source: inner } = &source.kind {
            source = inner;
        }
        if let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &source.kind
        {
            if let Some((_, field)) = self.capture_field(*binding) {
                if field.consumption != hew_types::ClosureCaptureConsumption::Consumed {
                    return Err("E_OWN_CONSUME_BORROWED: consuming a captured argument requires an owning capture transfer".into());
                }
            }
        }
        Ok(())
    }

    /// Erase one owned concrete value into a trait object.
    pub(super) fn lower_dyn_make(
        &mut self,
        expr: &HirExpr,
        value: &HirExpr,
        concrete_type: &ResolvedTy,
        entries: &[hew_types::DynVtableEntry],
    ) -> Result<ValueId, String> {
        let dyn_ty = self.ty(&expr.ty);
        let concrete_ty = self.ty(concrete_type);
        let vtable = self
            .service
            .request_vtable(&dyn_ty, &concrete_ty, entries)?;
        if self.ty(&value.ty) != concrete_ty {
            return Err(format!(
                "erasure input `{}` differs from the checker's concrete type `{}`",
                self.ty(&value.ty).user_facing(),
                concrete_ty.user_facing()
            ));
        }
        // A bit-copy concrete value carries no obligation to transfer; the
        // box holds its bits and the table's drop slot has nothing to run.
        let source =
            if OwnKind::of_ty(&concrete_ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                let source = self.lower_consuming_value(value)?;
                self.owned_live.remove(&source);
                source
            } else {
                self.lower_expr(value)?
            };
        self.emit(
            expr,
            SemOpKind::DynMake {
                vtable,
                value: Operand { value: source },
            },
        )
    }

    /// The erased dispatch boundary for one trait method.
    ///
    /// Parameter transfer follows the same rule the implementations were
    /// admitted under, so the verifier can compare this boundary against every
    /// table that erases into the receiver's trait object.
    pub(super) fn dyn_dispatch_signature(
        &mut self,
        args: &[HirExpr],
        return_ty: &ResolvedTy,
    ) -> Result<SemSignature, String> {
        let mut params = Vec::with_capacity(args.len());
        for arg in args {
            let ty = self.ty(&arg.ty);
            self.service.require_type_facts(&ty)?;
            let own = OwnKind::of_ty(&ty, self.service.checked_facts.rows())?;
            params.push(SemAbiParam {
                passing: dyn_boundary_passing(own),
                ty,
                caller_visible_projection: false,
            });
        }
        Ok(SemSignature {
            params,
            return_ty: return_ty.clone(),
        })
    }

    /// Dispatch one trait method through the receiver's vtable slot.
    pub(super) fn lower_dyn_call(
        &mut self,
        expr: &HirExpr,
        receiver: &HirExpr,
        target: &hew_types::CallTarget,
        args: &[HirExpr],
        evaluation_order: &[usize],
        signature: &hew_types::FnSig,
    ) -> Result<Option<ValueId>, String> {
        let hew_types::CallTarget::DynamicVtable { method, slot, .. } = target else {
            return Err("dynamic dispatch carries no checker vtable target".to_string());
        };
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let return_ty = self.ty(&expr.ty);
        let dispatch = self.dyn_dispatch_signature(args, &return_ty)?;
        let decision = match dyn_receiver_passing(signature) {
            SemParamPassing::Consume => crate::BoundaryDecision::Move,
            SemParamPassing::BorrowMut => crate::BoundaryDecision::BorrowMut,
            SemParamPassing::Borrow | SemParamPassing::ReadOnly => crate::BoundaryDecision::Borrow,
        };
        let value = if decision == crate::BoundaryDecision::Move {
            self.lower_consuming_value(receiver)?
        } else {
            self.lower_borrowed_read(receiver, &mut loans)?.value
        };
        let lowered_args =
            self.lower_user_arguments(args, evaluation_order, &dispatch.params, &mut loans)?;
        self.finish_user_call(
            PreparedCallee::Dyn {
                receiver: crate::BoundaryOperand {
                    operand: Operand { value },
                    decision,
                },
                slot: *slot,
                method: *method,
            },
            dispatch,
            lowered_args,
            &loans,
            &live_before_arguments,
            true,
            None,
        )
    }

    /// Direct and indirect user calls share argument capture and both cleanup paths.
    #[allow(
        clippy::too_many_lines,
        reason = "one user-call boundary owns evaluation order, receiver transfer and both continuations"
    )]
    pub(super) fn lower_direct_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let HirExprKind::Call {
            target,
            callee,
            args,
            evaluation_order,
        } = &expr.kind
        else {
            return Err("user-call lowering received a non-call".to_string());
        };
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let (callee, signature, actor) = match target {
            CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                let local_actor_method =
                    if let SemCallableKind::HewActor(actor) = self.callable.kind {
                        self.service.actors[actor.0 as usize]
                            .methods
                            .iter()
                            .find_map(|id| {
                                self.service
                                    .callable(*id)
                                    .filter(|method| &method.declaration == declaration)
                                    .cloned()
                            })
                    } else {
                        None
                    };
                let target = match local_actor_method {
                    Some(method) => method,
                    None => self.service.resolve_direct_call(
                        *declaration,
                        expr.site,
                        &self.substitution,
                    )?,
                };
                let actor = match target.kind {
                    SemCallableKind::HewActor(actor) => Some(actor),
                    SemCallableKind::HewDirect | SemCallableKind::HewClosure => None,
                };
                (PreparedCallee::Direct(target.id), target.signature, actor)
            }
            CallTarget::IndirectFunctionValue => {
                let ty = self.ty(&callee.ty);
                self.service.require_type_facts(&ty)?;
                let signature =
                    crate::callable_value_signature(&ty, self.service.checked_facts.rows())?;
                let (_, _, capabilities) = crate::callable_parts(&ty)?;
                let value = if capabilities.call == hew_types::CallableCallMode::Once {
                    self.lower_consuming_value(callee)?
                } else {
                    self.lower_borrowed_read(callee, &mut loans)?.value
                };
                let decision = match capabilities.call {
                    hew_types::CallableCallMode::Read => crate::BoundaryDecision::Borrow,
                    hew_types::CallableCallMode::Var => crate::BoundaryDecision::BorrowMut,
                    hew_types::CallableCallMode::Once => crate::BoundaryDecision::Move,
                };
                (
                    PreparedCallee::Indirect(crate::BoundaryOperand {
                        operand: Operand { value },
                        decision,
                    }),
                    signature,
                    None,
                )
            }
            _ => {
                return Err(
                    "user-call lowering requires a resolved direct or indirect target".to_string(),
                )
            }
        };
        let result_ty = self.ty(&expr.ty);
        // An actor method's first parameter is the caller's own state seat.
        let seats = usize::from(actor.is_some());
        if args.len() + seats != signature.params.len()
            || !crate::call_boundary_types_match(&result_ty, &signature.return_ty)
        {
            let name = match target {
                CallTarget::User(declaration) | CallTarget::ImplMethod(declaration) => {
                    self.service.module.defs.path(*declaration)
                }
                _ => "<function value>",
            };
            return Err(format!(
                "user call to `{name}` differs from its semantic signature: {} arguments, expected {}; result {result_ty:?}, expected {:?}",
                args.len(), signature.params.len(), signature.return_ty
            ));
        }
        // The two spellings of a channel half denote one value; the call site's
        // is the one that carries the message type, so the produced value
        // takes it and every downstream shape agrees with the scrutinee.
        let mut signature = signature;
        if signature.return_ty != result_ty {
            signature.return_ty = result_ty.clone();
        }
        // A checker-approved `#[returns_receiver]` call used only for its
        // effect carries a Read receiver even though the selected parameter
        // consumes it. Transfer the seat into the call, then publish the exact
        // returned owner back into that same seat on the normal edge. Calls
        // that produce a value, or consume a temporary without a storage seat,
        // leave their result with the enclosing expression instead. The
        // unwind edge deliberately leaves the seat dead: the callee consumed
        // the receiver and the caller's ordinary fault cleanup must not release
        // it a second time.
        let receiver_writeback = args
            .first()
            .zip(signature.params.get(seats))
            .filter(|(receiver, parameter)| {
                !value_required
                    && receiver.intent == IntentKind::Read
                    && parameter.passing == SemParamPassing::Consume
                    && self.ty(&receiver.ty) == signature.return_ty
            })
            .map(|(receiver, _)| self.expression_projection(receiver))
            .transpose()?
            .flatten();
        let mut lowered_args = self.lower_user_arguments(
            args,
            evaluation_order,
            &signature.params[seats..],
            &mut loans,
        )?;
        if let Some(actor) = actor {
            if self.callable.kind != SemCallableKind::HewActor(actor) {
                return Err("actor method is entered only from its own actor's bodies".into());
            }
            let state = ValueId(0);
            // The callee mutates state exclusively; arguments read from it
            // travel as independent copies.
            self.snapshot_arguments_rooted_at(
                crate::OwnerRoot::Value(state),
                &mut lowered_args,
                &mut loans,
                &Provenance::Site(expr.site),
            )?;
            lowered_args.insert(
                0,
                crate::BoundaryOperand {
                    operand: Operand { value: state },
                    decision: crate::BoundaryDecision::BorrowMut,
                },
            );
        }
        let result = self.finish_user_call(
            callee,
            signature,
            lowered_args,
            &loans,
            &live_before_arguments,
            value_required,
            None,
        )?;
        if let Some(place) = receiver_writeback {
            let value = result.ok_or_else(|| {
                "receiver-preserving consume did not return its receiver owner".to_string()
            })?;
            self.emit_place_operation(
                SemOpKind::StoreInit {
                    place,
                    value: Operand { value },
                },
                Provenance::Site(expr.site),
            )?;
            self.owned_live.remove(&value);
            return Ok(None);
        }
        Ok(result)
    }

    /// Lower a trait-method call reached through a where-clause bound.
    ///
    /// Once the implementation is selected the receiver is simply its first
    /// parameter, so the call enters the same argument transfer and call
    /// boundary as any other direct call.
    pub(super) fn lower_static_trait_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let HirExprKind::CallTraitMethodStatic {
            receiver,
            target,
            args,
            evaluation_order,
            ..
        } = &expr.kind
        else {
            return Err("static trait call lowering received a non-call".to_string());
        };
        let CallTarget::StaticTraitMethod {
            declaring_trait,
            method,
        } = target
        else {
            return Err("static trait call requires a checker-selected trait method".to_string());
        };
        let receiver_ty = self.ty(&receiver.ty);
        let callee = self.service.resolve_static_trait_call(
            *declaring_trait,
            *method,
            &receiver_ty,
            expr.site,
            &self.substitution,
        )?;
        let signature = callee.signature.clone();
        let result_ty = self.ty(&expr.ty);
        let arguments: Vec<HirExpr> = std::iter::once((**receiver).clone())
            .chain(args.iter().cloned())
            .collect();
        if arguments.len() != signature.params.len() || result_ty != signature.return_ty {
            return Err(format!(
                "static trait call to `{}` differs from its semantic signature: {} arguments, expected {}; result {result_ty:?}, expected {:?}",
                self.service.module.defs.path(callee.declaration),
                arguments.len(),
                signature.params.len(),
                signature.return_ty
            ));
        }
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let evaluation_order = if evaluation_order.is_empty() {
            Vec::new()
        } else {
            std::iter::once(0)
                .chain(evaluation_order.iter().map(|index| index + 1))
                .collect()
        };
        let lowered_args = self.lower_user_arguments(
            &arguments,
            &evaluation_order,
            &signature.params,
            &mut loans,
        )?;
        self.finish_user_call(
            PreparedCallee::Direct(callee.id),
            signature,
            lowered_args,
            &loans,
            &live_before_arguments,
            value_required,
            None,
        )
    }

    /// Capture arguments in their evaluation order while keeping earlier
    /// consumed values live until the call, and pass them in parameter order.
    pub(super) fn lower_user_arguments(
        &mut self,
        args: &[HirExpr],
        evaluation_order: &[usize],
        params: &[SemAbiParam],
        loans: &mut Vec<ValueId>,
    ) -> Result<Vec<crate::BoundaryOperand>, String> {
        if args.len() != params.len() {
            return Err("call arguments differ from the callee's parameters".into());
        }
        let receiver_loan_depth = self.argument_receiver_loans.len();
        self.argument_receiver_loans.extend(loans.iter().copied());
        let order = evaluation_sequence(evaluation_order, args.len());
        let mut lowered_args = vec![None; args.len()];
        for (position, &index) in order.iter().enumerate() {
            let (arg, expected) = (&args[index], &params[index]);
            let loan_floor = loans.len();
            let stable_tail = order[position + 1..]
                .iter()
                .all(|&later| Self::stable_argument_read(&args[later]));
            let operand = if expected.passing == SemParamPassing::Consume {
                let value = self.lower_consuming_value(arg)?;
                Operand {
                    value: self.coerce_value(value, &expected.ty, Provenance::Site(arg.site))?,
                }
            } else if expected.passing == SemParamPassing::ReadOnly
                && arg.intent == IntentKind::Consume
            {
                Operand {
                    value: lower_initial_value_transfer(
                        self,
                        arg,
                        "trivial consuming argument",
                        OwnedBindingUse::Move,
                    )?,
                }
            } else if self.ty(&arg.ty) == expected.ty {
                self.lower_call_read(arg, loans, stable_tail, true)?
            } else {
                let value = lower_initial_value_transfer(
                    self,
                    arg,
                    "call argument coercion",
                    OwnedBindingUse::Copy,
                )?;
                Operand {
                    value: self.coerce_value(value, &expected.ty, Provenance::Site(arg.site))?,
                }
            };
            lowered_args[index] = Some(crate::BoundaryOperand {
                operand,
                decision: match expected.passing {
                    SemParamPassing::ReadOnly => crate::BoundaryDecision::Copy,
                    SemParamPassing::Borrow => crate::BoundaryDecision::Borrow,
                    SemParamPassing::Consume => crate::BoundaryDecision::Move,
                    SemParamPassing::BorrowMut => {
                        return Err(
                            "direct parameter transfer requires its source contract".to_string()
                        )
                    }
                },
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(receiver_loan_depth);
        Ok(lowered_args.into_iter().flatten().collect())
    }

    /// One user-call boundary owns argument temporaries and both continuations.
    #[allow(
        clippy::too_many_lines,
        reason = "normal and fault continuations share one ownership boundary"
    )]
    #[allow(
        clippy::too_many_arguments,
        reason = "every user call shape shares this boundary; the handback is its unwind half"
    )]
    pub(super) fn finish_user_call(
        &mut self,
        callee: PreparedCallee,
        signature: SemSignature,
        lowered_args: Vec<crate::BoundaryOperand>,
        loans: &[ValueId],
        live_before_arguments: &std::collections::HashSet<ValueId>,
        value_required: bool,
        handback: Option<FaultHandback>,
    ) -> Result<Option<ValueId>, String> {
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        if let PreparedCallee::Indirect(receiver) | PreparedCallee::Dyn { receiver, .. } = &callee {
            if receiver.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&receiver.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        let temporaries: Vec<_> = live_at_call
            .keys()
            .filter(|value| !live_before_arguments.contains(value))
            .copied()
            .collect();
        let return_ty = signature.return_ty.clone();
        let (result, normal, continuation) = if return_ty == ResolvedTy::Never {
            (CallResult::Never, None, None)
        } else if return_ty == ResolvedTy::Unit {
            if value_required {
                return Err("unit-valued call cannot produce an SSA value".to_string());
            }
            (
                CallResult::Unit,
                Some(Edge {
                    target: self.new_block(Vec::new()),
                    args: vec![],
                }),
                None,
            )
        } else {
            self.service.require_type_facts(&return_ty)?;
            let own = OwnKind::of_ty(&return_ty, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: return_ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: return_ty.clone(),
                    own,
                }),
                Some(Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                }),
                Some((continuation, own)),
            )
        };
        let normal_block = normal.as_ref().map(|edge| edge.target);
        let handback_def = handback.as_ref().map(|handback| ValueDef {
            id: self.fresh_value(),
            ty: handback.ty.clone(),
            own: handback.own,
        });
        let returned_receiver = handback_def.as_ref().map(|def| BlockArg {
            value: self.fresh_value(),
            ty: def.ty.clone(),
            own: def.own,
        });
        let unwind = self.new_block(returned_receiver.iter().cloned().collect());
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(
            callee.invoke(
                id,
                signature,
                lowered_args,
                result,
                normal,
                CallUnwind::Cleanup(Edge {
                    target: unwind,
                    args: handback_def
                        .iter()
                        .map(|def| Operand { value: def.id })
                        .collect(),
                }),
                handback_def,
            ),
        )?;
        self.current = unwind;
        self.owned_live = live_at_call.clone();
        self.end_call_loans(loans)?;
        // The handed-back receiver returns to its place only on this edge.
        let before_handback = self.control_state();
        if let (Some(handback), Some(receiver)) = (handback, returned_receiver) {
            if receiver.own == OwnKind::Owned {
                self.owned_live.insert(receiver.value, receiver.ty);
            }
            match handback.seat {
                Some(seat) => {
                    self.publish_receiver(seat, receiver.value, handback.provenance)?;
                }
                None if receiver.own == OwnKind::Owned => self.emit_destroy(receiver.value)?,
                None => {}
            }
        }
        self.finish_fault_exit()?;
        self.restore_control_state(&before_handback);
        let Some(normal_block) = normal_block else {
            self.current = self.new_block(Vec::new());
            self.set_terminator(SemTerminator::Unreachable)?;
            return Ok(None);
        };
        self.current = normal_block;
        self.owned_live = live_at_call;
        // A successful result already owns its value before argument cleanup.
        // Cooperative cleanup may fail, so its unwind edge must release that
        // result along with the caller's other live values.
        if let Some((value, OwnKind::Owned)) = continuation {
            self.owned_live.insert(value, return_ty);
        }
        self.end_call_loans(loans)?;
        for value in temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(continuation.map(|(value, _)| value))
    }

    /// Snapshot borrowed arguments which would otherwise overlap an exclusive
    /// receiver rooted at `root`.
    pub(super) fn snapshot_arguments_rooted_at(
        &mut self,
        root: crate::OwnerRoot,
        lowered_args: &mut [crate::BoundaryOperand],
        loans: &mut Vec<ValueId>,
        provenance: &Provenance,
    ) -> Result<(), String> {
        for argument in lowered_args {
            let value = argument.operand.value;
            if self.value_own_kind(value) == Some(OwnKind::Guaranteed)
                && self.value_borrow_root(value)? == root
            {
                let ty = self
                    .value_ty(value)
                    .ok_or_else(|| "borrowed argument has no type".to_string())?;
                argument.operand.value = self.emit_typed(
                    provenance.clone(),
                    &ty,
                    SemOpKind::CopyValue {
                        source: Operand { value },
                    },
                )?;
            }
        }
        let related = loans
            .iter()
            .copied()
            .filter_map(|loan| match self.value_borrow_root(loan) {
                Ok(owner) if owner == root => Some(Ok(loan)),
                Ok(_) => None,
                Err(error) => Some(Err(error)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.end_call_loans(&related)?;
        loans.retain(|loan| !related.contains(loan));
        Ok(())
    }

    pub(super) fn lower_call(
        &mut self,
        expr: &HirExpr,
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        if matches!(expr.kind, HirExprKind::CallTraitMethodStatic { .. }) {
            return self.lower_static_trait_call(expr, value_required);
        }
        let HirExprKind::Call {
            target,
            args,
            evaluation_order,
            ..
        } = &expr.kind
        else {
            return Err(
                "internal SIR lowering error: call lowering received a non-call".to_string(),
            );
        };
        if positional_call_target(target) && !evaluation_order.is_empty() {
            return Err(format!(
                "call target {target:?} takes positional arguments only"
            ));
        }
        match target {
            CallTarget::DeclaredRuntime {
                family,
                actor_endpoints: Some(endpoints),
                ..
            } => {
                self.lower_actor_attachment_runtime(expr, *family, endpoints, args, value_required)
            }
            CallTarget::Builtin { endpoint } if endpoint == "assert" => {
                self.lower_assert(expr, args)?;
                Ok(None)
            }
            CallTarget::Builtin { endpoint } if endpoint == "sleep" => {
                self.lower_sleep(expr, args)?;
                Ok(None)
            }
            CallTarget::Builtin { endpoint } if endpoint == "sleep_until" => {
                self.lower_sleep_until(expr, args)?;
                Ok(None)
            }
            CallTarget::Extern {
                declaration,
                endpoint,
                ..
            } => self.lower_extern_call(
                expr,
                *declaration,
                endpoint,
                (args, evaluation_order),
                value_required,
            ),
            CallTarget::Runtime(hew_types::RuntimeCallFamily::SupervisorStop) => {
                let [handle] = args.as_slice() else {
                    return Err("supervisor stop takes exactly one handle".into());
                };
                self.lower_supervisor_stop(handle)
            }
            CallTarget::Runtime(family)
            | CallTarget::DeclaredRuntime {
                family,
                actor_endpoints: None,
                ..
            } => self.lower_runtime_operation_with(
                expr,
                *family,
                (&args.iter().collect::<Vec<_>>(), evaluation_order),
                value_required,
                &[],
            ),
            CallTarget::User(_) | CallTarget::ImplMethod(_) | CallTarget::IndirectFunctionValue => {
                self.lower_direct_call(expr, value_required)
            }
            CallTarget::Builtin { endpoint } => {
                let family = hew_types::RuntimeCallFamily::from_catalog_endpoint(endpoint)
                    .ok_or_else(|| {
                        format!(
                            "call target {target:?} has no verified ownership-SIR operation contract"
                        )
                    })?;
                self.lower_runtime_operation_with(
                    expr,
                    family,
                    (&args.iter().collect::<Vec<_>>(), evaluation_order),
                    value_required,
                    &[],
                )
            }
            _ => Err(format!(
                "call target {target:?} has no verified ownership-SIR operation contract"
            )),
        }
    }

    pub(super) fn lower_actor_attachment_runtime(
        &mut self,
        expr: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        endpoints: &hew_types::check::dispatch::ResolvedActorEndpoints,
        args: &[HirExpr],
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let [receiver, handler] = args else {
            return Err("actor attachment requires its receiver and handler".into());
        };
        let handler_ty = self.ty(&handler.ty);
        if handler_ty
            .actor_handle_instance(&self.service.module.defs)
            .is_none_or(|instance| instance.nominal.declaration() != endpoints.actor)
        {
            return Err("actor ingress target differs from its resolved declaration".into());
        }
        let actor = self.service.require_actor(&handler_ty)?;
        let pointer_ty = crate::ActorIngressAdapter::pointer_type();
        let mut adapters = Vec::new();
        for (index, endpoint) in [&endpoints.data, &endpoints.close].into_iter().enumerate() {
            let adapter = crate::ActorIngressAdapter {
                actor,
                message: endpoint.msg_id,
            };
            let selected = adapter.handler(&self.service.actors)?;
            if selected.declaration != endpoint.handler {
                return Err("actor ingress endpoint differs from its resolved declaration".into());
            }
            if selected.params.is_empty() != (index == 1) {
                return Err(
                    "actor ingress data and close endpoints have incompatible payloads".into(),
                );
            }
            adapters.push(self.emit_typed(
                Provenance::Site(expr.site),
                &pointer_ty,
                SemOpKind::ActorIngressAdapter(adapter),
            )?);
        }
        self.lower_runtime_operation_with(
            expr,
            family,
            (&[receiver, handler], &[]),
            value_required,
            &[(2, adapters[0]), (3, adapters[1])],
        )
    }

    pub(super) fn lower_runtime_operation(
        &mut self,
        expr: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        args: &[&HirExpr],
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        self.lower_runtime_operation_with(expr, family, (args, &[]), value_required, &[])
    }

    /// As [`Self::lower_runtime_operation`], with `prelowered` naming argument
    /// positions whose value this body already produced. The named
    /// argument is transferred as it stands instead of being lowered from its
    /// expression, so an element rebuilt in place reaches the set entry rather
    /// than a second read of the slot it is replacing.
    /// Positions beyond `args` append synthesized semantic operands such as
    /// actor ingress adapters, whose existing value carries its exact type.
    #[allow(
        clippy::too_many_lines,
        reason = "runtime contract admission and its explicit success/failure CFG form one semantic boundary"
    )]
    pub(super) fn lower_runtime_operation_with(
        &mut self,
        expr: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        (args, evaluation_order): (&[&HirExpr], &[usize]),
        value_required: bool,
        prelowered: &[(usize, ValueId)],
    ) -> Result<Option<ValueId>, String> {
        use hew_types::{RuntimeArgumentEffect, RuntimeResultEffect};

        let order = evaluation_sequence(evaluation_order, args.len());

        if matches!(
            family,
            hew_types::RuntimeCallFamily::StreamClose
                | hew_types::RuntimeCallFamily::SinkClose
                | hew_types::RuntimeCallFamily::ActorCallFree
                | hew_types::RuntimeCallFamily::ActorRequestRelease
        ) {
            let [owner] = args else {
                return Err("consuming release requires exactly one owner".into());
            };
            if value_required {
                return Err("unit-valued release cannot produce an SSA value".into());
            }
            let value = if let Some((_, value)) = prelowered.iter().find(|(index, _)| *index == 0) {
                *value
            } else {
                self.lower_consuming_value(owner)?
            };
            self.emit_destroy(value)?;
            return Ok(None);
        }

        let observation = match family {
            hew_types::RuntimeCallFamily::ActorLink => Some(crate::LocalObservationKind::Link),
            hew_types::RuntimeCallFamily::ActorMonitor => {
                Some(crate::LocalObservationKind::Monitor)
            }
            hew_types::RuntimeCallFamily::ActorUnlink => Some(crate::LocalObservationKind::Unlink),
            hew_types::RuntimeCallFamily::ActorDemonitor => {
                Some(crate::LocalObservationKind::Demonitor)
            }
            _ => None,
        };
        let remote = match family {
            hew_types::RuntimeCallFamily::LinkRemote => Some(crate::RemoteObservationKind::Link),
            hew_types::RuntimeCallFamily::NodeMonitor => {
                Some(crate::RemoteObservationKind::Monitor)
            }
            _ => None,
        };
        if let Some(kind) = remote {
            let operation = crate::ActorOperation::RemoteObservation {
                kind,
                params: args.iter().map(|arg| self.ty(&arg.ty)).collect(),
                result: self.ty(&expr.ty),
            };
            let signature = self.actor_signature(&operation)?;
            self.service.require_type_facts(&signature.return_ty)?;
            let mut values = vec![None; args.len()];
            for &index in &order {
                let value = self.lower_expr(args[index])?;
                if !self.is_open() {
                    return Ok(Some(value));
                }
                values[index] = Some(value);
            }
            return self.emit_actor_call(
                operation,
                signature,
                values.into_iter().flatten().collect(),
            );
        }
        if let Some(kind) = observation {
            let [target] = args else {
                return Err("local observation takes one target".into());
            };
            let operation = crate::ActorOperation::LocalObservation {
                kind,
                target: self.ty(&target.ty),
                result: self.ty(&expr.ty),
            };
            let signature = self.actor_signature(&operation)?;
            self.service.require_type_facts(&signature.return_ty)?;
            let value = self.lower_expr(target)?;
            return self.emit_actor_call(operation, signature, vec![value]);
        }

        if let hew_types::RuntimeCallFamily::AsyncIo(operation) = family {
            if !evaluation_order.is_empty() {
                return Err("native I/O takes positional arguments only".into());
            }
            return self.lower_native_io(expr, operation, args);
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::StreamSendLayout
                | hew_types::RuntimeCallFamily::StreamTrySendLayout
        ) {
            let ([sink, value], []) = (args, evaluation_order) else {
                return Err("stream write takes one sink and one element, in order".into());
            };
            let park = family == hew_types::RuntimeCallFamily::StreamSendLayout;
            return self.lower_sink_write(expr, sink, value, park).map(Some);
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::StreamNextLayout
                | hew_types::RuntimeCallFamily::StreamTryNextLayout
        ) {
            let [stream] = args else {
                return Err("stream receive takes exactly one stream".into());
            };
            let park = family == hew_types::RuntimeCallFamily::StreamNextLayout;
            return self.lower_stream_next(expr, stream, park).map(Some);
        }
        let contract = family.semantic_contract().ok_or_else(|| {
            format!("runtime family `{family:?}` has no ownership-SIR semantic contract")
        })?;
        let argument_count = args.len().max(
            prelowered
                .iter()
                .map(|(index, _)| index + 1)
                .max()
                .unwrap_or(0),
        );
        let source_types = (0..argument_count)
            .map(|index| {
                if let Some((_, value)) = prelowered.iter().find(|(at, _)| *at == index) {
                    self.value_ty(*value)
                        .ok_or_else(|| "runtime operand lacks its semantic value type".to_string())
                } else {
                    args.get(index).map(|arg| self.ty(&arg.ty)).ok_or_else(|| {
                        "runtime operand position has no source or semantic value".to_string()
                    })
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        let instantiated = contract
            .resolve_types(&source_types, &self.ty(&expr.ty))
            .map_err(|error| format!("runtime operation {family:?}: {error}"))?;
        let parameter_types = &instantiated.arguments;
        // Runtime mutations release receiver-typed contents inside the call.
        // Dispatch their outcome after republishing the receiver and acquiring
        // any result owners on the normal edge.
        let receiver_release = family
            .releases_receiver_contents()
            .then_some(parameter_types)
            .and_then(|types| types.first())
            .cloned();
        for (index, (source, target)) in source_types.iter().zip(parameter_types).enumerate() {
            if prelowered.iter().any(|(at, _)| *at == index)
                && *source == crate::ActorIngressAdapter::pointer_type()
                && source == target
            {
                // An adapter address is synthesized at this foreign boundary;
                // it is not a source-language raw-pointer value or aggregate.
                require_type_facts(&mut self.service.checked_facts, source)?;
            } else {
                self.service.require_type_facts(source)?;
                self.service.require_type_facts(target)?;
            }
            if source != target {
                if contract.arguments[index].effect != RuntimeArgumentEffect::Value {
                    return Err(format!(
                        "runtime argument {index} cannot coerce a non-value operand"
                    ));
                }
                crate::verify_callable_coercion(source, target, self.service.checked_facts.rows())?;
            }
        }
        if matches!(
            family,
            hew_types::RuntimeCallFamily::Map(hew_types::runtime_call::MapValueOp::New)
                | hew_types::RuntimeCallFamily::Set(hew_types::runtime_call::SetValueOp::New)
        ) {
            let result_ty = self.ty(&expr.ty);
            let collection_ty = parameter_types.first().unwrap_or(&result_ty);
            let (_, arguments) = collection_type_arguments(collection_ty)
                .ok_or_else(|| "collection operation has no canonical receiver type".to_string())?;
            self.service.require_key_capabilities(&arguments[0])?;
        }
        for capability in family.value_callback_capabilities() {
            let (_, [element, ..]) = parameter_types
                .first()
                .and_then(collection_type_arguments)
                .ok_or_else(|| "collection callback has no canonical receiver type".to_string())?
            else {
                return Err("collection callback has no element type".into());
            };
            self.service
                .require_value_capability(element, *capability)?;
        }
        if family == hew_types::RuntimeCallFamily::StructuralFormat {
            self.service
                .require_structural_rendering(&crate::StructuralType::canonical(
                    &parameter_types[0],
                ))?;
        }
        if matches!(contract.result, RuntimeResultEffect::IndependentValue(_)) {
            self.service.require_type_facts(&instantiated.result_ty)?;
            if self.service.checked_facts.rows()[&TypeInstanceKey(instantiated.result_ty.clone())]
                .clone
                == hew_types::CloneKind::None
            {
                return Err(
                    "runtime read cannot copy an affine element; use an owning removal".into(),
                );
            }
        }
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut transformed_place = None;
        let mut indexed_path = None;
        let mut indexed_keys = Vec::new();
        let mut lowered_args = Vec::with_capacity(argument_count);
        let mut loans = Vec::new();
        let effects = contract
            .arguments
            .iter()
            .zip(parameter_types)
            .map(|(argument, ty)| {
                argument.effect.resolve_operand(
                    self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].class,
                )
            })
            .collect::<Vec<_>>();
        // An adopted operand that still has a copy recipe keeps the caller's
        // own value: lowering reads the binding as an independent owner and
        // transfers that owner, rather than consuming what the caller named.
        let copied_ingress = contract
            .arguments
            .iter()
            .zip(parameter_types)
            .map(|(argument, ty)| {
                argument.effect == RuntimeArgumentEffect::Value
                    && self.service.checked_facts.rows()[&TypeInstanceKey(ty.clone())].clone
                        != hew_types::CloneKind::None
            })
            .collect::<Vec<bool>>();
        let read_only = effects
            .iter()
            .all(|effect| *effect != RuntimeArgumentEffect::Move);
        let argument_loan_depth = self.argument_receiver_loans.len();
        // Source arguments run in their evaluation order; synthesized
        // operands beyond them follow.
        let sequence: Vec<usize> = order
            .iter()
            .copied()
            .chain(args.len()..argument_count)
            .collect();
        let mut placed: Vec<Option<crate::BoundaryOperand>> =
            (0..argument_count).map(|_| None).collect();
        for (position, &index) in sequence.iter().enumerate() {
            let effect = effects[index];
            let later = || {
                sequence[position + 1..]
                    .iter()
                    .filter_map(|&later| args.get(later))
                    .all(|arg| Self::stable_argument_read(arg))
            };
            if let Some(&(_, value)) = prelowered.iter().find(|(at, _)| *at == index) {
                let decision = match effect {
                    RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
                    RuntimeArgumentEffect::Borrow => crate::BoundaryDecision::Borrow,
                    RuntimeArgumentEffect::Copy | RuntimeArgumentEffect::Value => {
                        crate::BoundaryDecision::Copy
                    }
                };
                placed[index] = Some(crate::BoundaryOperand {
                    operand: Operand { value },
                    decision,
                });
                continue;
            }
            let arg = args[index];
            let loan_floor = loans.len();
            let (value, decision) = if source_types[index] == parameter_types[index] {
                match effect {
                    RuntimeArgumentEffect::Value => unreachable!("value ingress was resolved"),
                    RuntimeArgumentEffect::Borrow => {
                        let stable_tail = later();
                        let operand =
                            self.lower_call_read(arg, &mut loans, stable_tail, read_only)?;
                        (operand.value, crate::BoundaryDecision::Borrow)
                    }
                    RuntimeArgumentEffect::Copy => {
                        let no_owner = OwnKind::of_ty(
                            &parameter_types[index],
                            self.service.checked_facts.rows(),
                        )? == OwnKind::None;
                        let stable_tail = later();
                        let operand = self.lower_call_read(
                            arg,
                            &mut loans,
                            stable_tail,
                            read_only && no_owner,
                        )?;
                        (operand.value, crate::BoundaryDecision::Copy)
                    }
                    RuntimeArgumentEffect::Move
                        if index == 0
                            && matches!(
                                contract.result,
                                RuntimeResultEffect::UpdatedReceiver(_)
                                    | RuntimeResultEffect::UpdatedReceiverAndValue(_)
                            ) =>
                    {
                        let path = self.resolve_writable_path(arg)?;
                        let place = path.base.clone();
                        if Self::path_is_indexed(&path) {
                            indexed_keys = Self::path_indices(&path);
                            indexed_path = Some(path);
                        }
                        if OwnKind::of_ty(&self.ty(&arg.ty), self.service.checked_facts.rows())?
                            != OwnKind::Owned
                        {
                            return Err("runtime transform receiver must be an owned value".into());
                        }
                        transformed_place = Some(place);
                        // The receiver is retaken after later arguments finish;
                        // no operand or snapshot is emitted for it here.
                        continue;
                    }
                    RuntimeArgumentEffect::Move if copied_ingress[index] => {
                        (self.lower_adopted_copy(arg)?, crate::BoundaryDecision::Move)
                    }
                    RuntimeArgumentEffect::Move => (
                        self.lower_consuming_value(arg)?,
                        crate::BoundaryDecision::Move,
                    ),
                }
            } else {
                // Weakening a callable's capabilities must not consume a
                // copyable source binding. Convert an independent owner first.
                let source_clone = self.service.checked_facts.rows()
                    [&TypeInstanceKey(source_types[index].clone())]
                    .clone;
                let value = if source_clone == hew_types::CloneKind::None {
                    self.lower_consuming_value(arg)?
                } else {
                    lower_initial_value_transfer(
                        self,
                        arg,
                        "runtime value coercion",
                        OwnedBindingUse::Copy,
                    )?
                };
                let value =
                    self.coerce_value(value, &parameter_types[index], Provenance::Site(arg.site))?;
                let decision = match effect {
                    RuntimeArgumentEffect::Copy => crate::BoundaryDecision::Copy,
                    RuntimeArgumentEffect::Move => crate::BoundaryDecision::Move,
                    _ => unreachable!("value ingress resolves to copy or move"),
                };
                (value, decision)
            };
            placed[index] = Some(crate::BoundaryOperand {
                operand: Operand { value },
                decision,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        lowered_args.extend(placed.into_iter().flatten());
        self.argument_receiver_loans.truncate(argument_loan_depth);

        // Preserve arguments borrowing the receiver's owner before its take.
        // Alias identity comes from the same declared place paths as loans.
        if let Some(place) = &transformed_place {
            let selected = match self.owned_projection(place)? {
                Some(projected) => projected,
                None => match self.binding_target(place.binding)? {
                    BindingTarget::Place(root) => root,
                    BindingTarget::Value(_) => {
                        return Err("runtime receiver has no owning seat".into())
                    }
                },
            };
            let root = self.place_borrow_root(selected)?;
            self.snapshot_arguments_rooted_at(
                root,
                &mut lowered_args,
                &mut loans,
                &Provenance::Site(expr.site),
            )?;
        }

        // Argument temporaries precede the receiver's actual transfer.
        let argument_temporaries: Vec<_> = self
            .owned_live
            .keys()
            .filter(|value| {
                !live_before_arguments.contains(value)
                    && !indexed_keys.contains(value)
                    && !lowered_args.iter().any(|arg| {
                        arg.decision == crate::BoundaryDecision::Move
                            && arg.operand.value == **value
                    })
            })
            .copied()
            .collect();
        let mut transformed_target = None;
        // Where a failing call that keeps its receiver returns it.
        let mut failure_target = None;
        let mut failure_return = None;
        let mut indexed_writeback = None;
        if let Some(place) = &transformed_place {
            let provenance = Provenance::Site(expr.site);
            let whole_owner = self.whole_owner_root(place)?;
            let projected = match whole_owner {
                Some(root) => root,
                None => self
                    .owned_projection(place)?
                    .ok_or("runtime receiver has no owning seat")?,
            };
            // A transform takes its receiver, which a live element loan of the
            // same owner forbids. Refusing here names the source construct
            // instead of leaving it to the ownership verifier.
            let root = self.place_borrow_root(projected)?;
            self.end_binding_loans_on(root)?;
            for loan in self.scope_loans.clone() {
                if self.ended_loans.contains(&loan) {
                    continue;
                }
                if self.value_borrow_root(loan)? == root {
                    return Err(
                        "E_OWN_CONSUME_BORROWED: this collection is borrowed by a live element \
                         loan; the loop or read holding it must end before the collection is \
                         mutated or drained"
                            .to_string(),
                    );
                }
            }
            let receiver_ty = self.ty(&args[0].ty);
            // Beneath a whole owner, a single transform opens the owner and
            // mutates its field in place. An indexed path runs several steps
            // that can fail, and a transform that releases its receiver when
            // it fails leaves nothing to close the owner around, so those
            // mutate a copy of the field that is assigned back after them.
            let releases = !contract.failures.is_empty() && !contract.preserves_inputs_on_failure();
            let copies_field = indexed_path.is_some() || releases;
            // A closure called again after a recovered fault reads its capture
            // again, so a transform that releases a capture when it fails
            // mutates a copy of the capture too.
            let whole_owner = whole_owner.or_else(|| {
                (releases && indexed_path.is_none() && self.is_borrowed_capture(projected))
                    .then_some(projected)
            });
            let (current, target) = match whole_owner {
                Some(root) if copies_field => (
                    Some(self.copy_through_whole_owner(
                        root,
                        place,
                        &provenance,
                        if indexed_path.is_some() {
                            "an indexed update"
                        } else {
                            "an operation that can release the collection when it fails"
                        },
                    )?),
                    WritableRoot::WholeOwner {
                        root,
                        base: place.clone(),
                    },
                ),
                Some(root) => {
                    let (field, owner) = self.open_owner(root, place, &provenance)?;
                    (Some(field), WritableRoot::Opened(owner))
                }
                None => (
                    None,
                    WritableRoot::Place {
                        leaf: projected,
                        taken: false,
                    },
                ),
            };
            let source = if let Some(path) = indexed_path.take() {
                let container = match current {
                    Some(copy) => copy,
                    None => self.emit_typed(
                        provenance.clone(),
                        &place.leaf_ty,
                        SemOpKind::LoadCopy { place: projected },
                    )?,
                };
                let (source, writeback) =
                    self.acquire_indexed_path(path, container, target, true, &provenance)?;
                indexed_writeback = Some(writeback);
                source
            } else if let Some(field) = current {
                if let WritableRoot::Opened(owner) = &target {
                    failure_target = Some(WritableRoot::Opened(owner.clone()));
                }
                transformed_target = Some(target);
                field
            } else {
                // The receiver leaves its place by take, and the call
                // re-initializes the place on every edge the call owns: with
                // the updated receiver, or the input where the contract keeps
                // it on failure. A state seat the runtime consumed keeps the
                // empty carrier the take left in it. The place never needs a
                // copy of its own value.
                transformed_target = Some(WritableRoot::Place {
                    leaf: projected,
                    taken: true,
                });
                failure_target = Some(WritableRoot::Place {
                    leaf: projected,
                    taken: true,
                });
                if self.in_var_self_receiver(projected) {
                    self.state_taken.insert(projected);
                }
                self.emit_typed(
                    provenance.clone(),
                    &receiver_ty,
                    SemOpKind::LoadTake { place: projected },
                )?
            };
            self.owned_live.remove(&source);
            let moved = self.emit_typed(
                provenance,
                &receiver_ty,
                SemOpKind::Move {
                    source: Operand { value: source },
                },
            )?;
            self.owned_live.remove(&moved);
            failure_return = failure_target.take().map(|target| (target, moved));
            lowered_args.insert(
                0,
                crate::BoundaryOperand {
                    operand: Operand { value: moved },
                    decision: crate::BoundaryDecision::Move,
                },
            );
        }
        // Keep transferred values available to cleanup until pre-mutation close succeeds.
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        if let RuntimeResultEffect::FreshOwnedVariant(kind) = contract.result {
            self.service
                .require_runtime_variant_result_shapes(kind, &instantiated.result_ty)?;
        }
        let mut live_on_failure = live_at_call.clone();
        if contract.preserves_inputs_on_failure() {
            for argument in &lowered_args {
                if argument.decision == crate::BoundaryDecision::Move {
                    let value = argument.operand.value;
                    live_on_failure.insert(
                        value,
                        self.value_ty(value).ok_or("runtime input has no type")?,
                    );
                }
            }
        }
        let semantic_result_ty =
            (instantiated.result_ty != ResolvedTy::Unit).then_some(instantiated.result_ty);
        match (contract.result, &semantic_result_ty) {
            (RuntimeResultEffect::Unit, None) if self.ty(&expr.ty) == ResolvedTy::Unit => {}
            (RuntimeResultEffect::UpdatedReceiver(_), Some(_))
                if self.ty(&expr.ty) == ResolvedTy::Unit && !value_required => {}
            (RuntimeResultEffect::UpdatedReceiverAndValue(_), Some(ResolvedTy::Tuple(fields)))
                if fields.len() == 2 && self.ty(&expr.ty) == fields[1] => {}
            (_, Some(result_ty)) if self.ty(&expr.ty) == *result_ty => {}
            _ => {
                return Err(format!(
                    "runtime family `{family:?}` result contract disagrees with expression type `{}`",
                    self.ty(&expr.ty).user_facing()
                ));
            }
        }

        if semantic_result_ty == Some(ResolvedTy::Never) {
            if !contract.failures.is_empty() {
                return Err(format!(
                    "runtime family `{family:?}` never returns but declares failure edges"
                ));
            }
            // The process ends here: no continuation, no scope cleanup. The
            // mandatory normal edge is a structural unreachable block.
            let unreachable = self.new_block(Vec::new());
            let id = OpId(self.ops);
            self.ops += 1;
            self.set_terminator(SemTerminator::RtCall {
                id,
                family,
                args: lowered_args,
                result: CallResult::Never,
                normal: Edge {
                    target: unreachable,
                    args: Vec::new(),
                },
                unwind: CallUnwind::NotApplicable,
            })?;
            self.current = unreachable;
            self.set_terminator(SemTerminator::Unreachable)?;
            return Ok(None);
        }
        let borrowed_result = matches!(contract.result, RuntimeResultEffect::Borrowed(_));
        // A borrowed contract result is only an actual loan when its concrete
        // type carries an ownership obligation; see `OwnKind::of_loan_result`.
        let mut result_is_loan = false;
        let (result, normal, continuation) = if let Some(result_ty) = semantic_result_ty {
            self.service.require_type_facts(&result_ty)?;
            let own = if borrowed_result {
                OwnKind::of_loan_result(&result_ty, self.service.checked_facts.rows())?
            } else {
                OwnKind::of_ty(&result_ty, self.service.checked_facts.rows())?
            };
            result_is_loan = own == OwnKind::Guaranteed;
            if matches!(contract.result, RuntimeResultEffect::FreshOwnedVariant(_))
                && own != OwnKind::Owned
            {
                return Err(format!(
                    "runtime family `{family:?}` variant result `{}` is not owned",
                    result_ty.user_facing()
                ));
            }
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: result_ty.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: result_ty,
                    own,
                }),
                Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                },
                Some(continuation),
            )
        } else {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        };

        let failure = contract.failures.first().copied();
        if contract.failures.len() > 1 && !contract.propagates_fault() {
            return Err(format!(
                "runtime family `{family:?}` has more failure edges than RtCall currently represents"
            ));
        }
        let failure_block = failure.map(|_| self.new_block(Vec::new()));
        let unwind = failure_block.map_or(CallUnwind::NotApplicable, |target| {
            CallUnwind::Cleanup(Edge {
                target,
                args: Vec::new(),
            })
        });
        let id = OpId(self.ops);
        self.ops += 1;
        let normal_target = normal.target;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family,
            args: lowered_args,
            result,
            normal,
            unwind,
        })?;

        if let (Some(failure), Some(block)) = (failure, failure_block) {
            self.current = block;
            self.owned_live = live_on_failure;
            // Where the contract keeps the receiver on this edge, the place
            // the call took is re-published here, so a failing mutation
            // leaves its target whole: an actor's teardown releases the field
            // once, and a `var self` method hands its receiver back. Where the
            // runtime consumed and released it instead, a state seat keeps the
            // empty carrier the take left in it: nothing but releases may run
            // on a failure edge, and the actor's release reads that carrier as
            // an empty collection.
            if let Some((target, value)) = failure_return {
                if contract.preserves_inputs_on_failure() {
                    self.publish_writable(target, value, &Provenance::Site(expr.site))?;
                } else if let WritableRoot::Place { leaf, .. } = target {
                    if matches!(
                        self.places[leaf.0 as usize].origin,
                        crate::PlaceOrigin::ActorState { .. }
                    ) {
                        self.require_empty_carrier_seat(leaf)?;
                    }
                }
            }
            self.end_call_loans(&loans)?;
            if contract.propagates_fault() {
                self.finish_fault_exit()?;
            } else {
                self.finish_checked_fault(
                    crate::runtime_failure_trap_kind(failure)
                        .ok_or_else(|| "static runtime failure has no trap kind".to_string())?,
                )?;
            }
        }
        self.current = normal_target;
        self.owned_live = live_at_call;
        // The call result already owns its payload on this edge. Retire
        // borrowed argument temporaries only after recording that owner and
        // publishing the updated receiver: a temporary's close can fail, and
        // cleanup must then drain the result and find the receiver's owner
        // whole.
        if let Some(continuation) = continuation {
            let result_ty = self
                .value_ty(continuation)
                .expect("runtime continuation block argument was just created");
            if self.value_own_kind(continuation) == Some(OwnKind::Owned) {
                self.owned_live.insert(continuation, result_ty);
            }
        }
        if result_is_loan {
            // The result is a loan of argument zero: its owner must stay
            // borrowed for as long as the result is readable, and the loan the
            // result itself names ends with the scope that reads it. The
            // enclosing scope ends both on every exit, including a loop
            // back-edge, innermost first.
            self.scope_loans.extend(loans.iter().copied());
            if let Some(continuation) = continuation {
                self.scope_loans.push(continuation);
            }
        } else {
            self.end_call_loans(&loans)?;
        }
        let mut result = continuation;
        if let Some(continuation) = continuation {
            if matches!(
                contract.result,
                RuntimeResultEffect::UpdatedReceiverAndValue(_)
            ) {
                let ty = self
                    .value_ty(continuation)
                    .ok_or_else(|| "runtime transform result disappeared".to_string())?;
                let shape = self.service.require_aggregate_shape(&ty)?;
                let results = self.emit_destructure_value(
                    continuation,
                    &ty,
                    shape,
                    Provenance::Site(expr.site),
                )?;
                if let Some(writeback) = indexed_writeback {
                    self.publish_indexed_path(
                        writeback,
                        results[0].id,
                        &Provenance::Site(expr.site),
                    )?;
                } else {
                    self.publish_writable(
                        transformed_target.ok_or("runtime transform has no source place")?,
                        results[0].id,
                        &Provenance::Site(expr.site),
                    )?;
                }
                result = Some(results[1].id);
            } else if matches!(contract.result, RuntimeResultEffect::UpdatedReceiver(_)) {
                if let Some(writeback) = indexed_writeback {
                    self.publish_indexed_path(
                        writeback,
                        continuation,
                        &Provenance::Site(expr.site),
                    )?;
                    result = None;
                } else if let Some(target) = transformed_target {
                    self.publish_writable(target, continuation, &Provenance::Site(expr.site))?;
                    result = None;
                }
                // Otherwise a prelowered receiver belongs to an enclosing
                // writable path, which publishes it.
            }
        } else if value_required {
            return Err(format!(
                "unit-valued runtime family `{family:?}` cannot produce an SSA value"
            ));
        }
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        self.dispatch_runtime_release(receiver_release.as_ref())?;
        Ok(result)
    }

    /// Dispatch on the outcome of a release the call performed for this frame.
    ///
    /// `ty` is the receiver type when the operation can release its contents.
    pub(super) fn dispatch_runtime_release(
        &mut self,
        ty: Option<&hew_types::ResolvedTy>,
    ) -> Result<(), String> {
        let Some(ty) = ty else {
            return Ok(());
        };
        self.note_release_may_fault(ty);
        if self.cleanup_may_fail && !self.cleanup_draining {
            self.dispatch_value_cleanup()?;
        }
        Ok(())
    }

    /// The exact `extern` declaration behind one call target.
    ///
    /// Both an `extern` block and an `#[extern_symbol]` method reach HIR as a
    /// declaration item carrying the declared parameters, their `consume`
    /// dispositions and the return type, so one lookup serves both.
    pub(super) fn extern_signature(
        &self,
        declaration: hew_types::DefId,
        endpoint: &str,
    ) -> Result<crate::ExternSignature, String> {
        self.service
            .module
            .items
            .iter()
            .find_map(|item| {
                let hew_hir::HirItem::ExternFn(function) = item else {
                    return None;
                };
                (function.declaration == declaration && function.name == endpoint).then(|| {
                    crate::ExternSignature {
                        declaration: function.declaration,
                        symbol: function.name.clone(),
                        params: function.param_tys.clone(),
                        consumes: function.param_consume.clone(),
                        result: function.return_ty.clone(),
                        runtime_capability: function.runtime_capability,
                    }
                })
            })
            .ok_or_else(|| {
                format!("extern `{endpoint}` names no declaration in this compilation unit")
            })
    }

    /// Refuse a declaration that disagrees with the generated ownership row.
    ///
    /// The row is the audited truth for a classified runtime symbol; an
    /// unclassified foreign symbol has no row and the declaration stands
    /// alone. A classified runtime export that moves a Hew owner across the
    /// boundary must be audited: absence is an unanswered question, and
    /// guessing it either leaks or double-frees, so the call is refused.
    ///
    /// Only parameters that carry a Hew obligation are compared: a `#[opaque]`
    /// pointer-width handle is a bit-copied id whose lifecycle belongs to its
    /// `#[resource]` owner, so a row that frees the underlying C allocation
    /// says nothing about the handle's Hew boundary.
    pub(super) fn verify_extern_declaration_ownership(
        signature: &crate::ExternSignature,
        obligations: &[bool],
        result_owned: bool,
    ) -> Result<(), String> {
        use hew_types::ffi_contracts::{
            extern_ownership_contract, ExternParamOwnership, ExternResultOwnership,
        };
        let Some(contract) = extern_ownership_contract(&signature.symbol).contract() else {
            if (obligations.iter().any(|carries| *carries) || result_owned)
                && hew_types::jit_symbols::is_classified_hew_ffi_symbol(&signature.symbol)
            {
                return Err(format!(
                    "extern `{}` is a classified runtime export with no audited ownership row; \
                     add its `[[ownership.contracts]]` entry to \
                     scripts/runtime-export-classification.toml",
                    signature.symbol
                ));
            }
            return Ok(());
        };
        if contract.params.len() == signature.consumes.len() {
            for (index, (declared, audited)) in
                signature.consumes.iter().zip(contract.params).enumerate()
            {
                if obligations[index] && *declared != (*audited == ExternParamOwnership::Consume) {
                    return Err(format!(
                        "extern `{}` parameter {index} declares {}, its audited ownership row says {audited:?}",
                        signature.symbol,
                        if *declared { "`consume`" } else { "a borrow" }
                    ));
                }
            }
        }
        match contract.result {
            ExternResultOwnership::Borrowed => Err(format!(
                "extern `{}` returns a borrow of a foreign allocation, which has no owner to borrow from",
                signature.symbol
            )),
            ExternResultOwnership::None if result_owned => Err(format!(
                "extern `{}` declares an owned result, its audited ownership row transfers nothing",
                signature.symbol
            )),
            _ => Ok(()),
        }
    }

    /// Lower a call to a declared C-ABI symbol.
    ///
    /// The `extern` declaration is the ownership authority: `consume` pins a
    /// transfer, its absence a borrow, and the declared return type decides
    /// whether the caller receives an owner. A C call cannot raise a Hew
    /// fault, so the call has no unwind edge.
    #[expect(
        clippy::too_many_lines,
        reason = "one extern boundary: declaration admission, operand transfer and result"
    )]
    pub(super) fn lower_extern_call(
        &mut self,
        expr: &HirExpr,
        declaration: hew_types::DefId,
        endpoint: &str,
        (args, evaluation_order): (&[HirExpr], &[usize]),
        value_required: bool,
    ) -> Result<Option<ValueId>, String> {
        let signature = self.extern_signature(declaration, endpoint)?;
        if signature.params.len() != args.len() || signature.consumes.len() != args.len() {
            return Err(format!(
                "extern `{endpoint}` declares {} parameters, called with {}",
                signature.params.len(),
                args.len()
            ));
        }
        if signature.result == ResolvedTy::Never {
            return Err(format!(
                "extern `{endpoint}` cannot be declared to never return"
            ));
        }
        if self.ty(&expr.ty) != signature.result {
            return Err(format!(
                "extern `{endpoint}` returns `{}`, used as `{}`",
                signature.result.user_facing(),
                self.ty(&expr.ty).user_facing()
            ));
        }
        for ty in signature
            .params
            .iter()
            .chain(std::iter::once(&signature.result))
        {
            if *ty != ResolvedTy::Unit {
                self.service.require_type_facts(ty)?;
            }
        }
        let mut decisions = Vec::with_capacity(args.len());
        for (index, ty) in signature.params.iter().enumerate() {
            let own = OwnKind::of_ty(ty, self.service.checked_facts.rows())?;
            decisions.push(signature.param_decision(index, own));
        }
        let obligations = decisions
            .iter()
            .map(|decision| *decision != crate::BoundaryDecision::Copy)
            .collect::<Vec<_>>();
        let result_owned = signature.result != ResolvedTy::Unit
            && OwnKind::of_ty(&signature.result, self.service.checked_facts.rows())?
                == OwnKind::Owned;
        Self::verify_extern_declaration_ownership(&signature, &obligations, result_owned)?;
        let read_only = decisions
            .iter()
            .all(|decision| *decision != crate::BoundaryDecision::Move);
        let live_before_arguments: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        let mut placed: Vec<Option<crate::BoundaryOperand>> = args.iter().map(|_| None).collect();
        let mut loans = Vec::new();
        let argument_loan_depth = self.argument_receiver_loans.len();
        let order = evaluation_sequence(evaluation_order, args.len());
        for (position, &index) in order.iter().enumerate() {
            let (arg, decision) = (&args[index], &decisions[index]);
            let loan_floor = loans.len();
            let value = if *decision == crate::BoundaryDecision::Move {
                self.lower_consuming_value(arg)?
            } else {
                let stable_tail = order[position + 1..]
                    .iter()
                    .all(|&later| Self::stable_argument_read(&args[later]));
                self.lower_call_read(arg, &mut loans, stable_tail, read_only)?
                    .value
            };
            placed[index] = Some(crate::BoundaryOperand {
                operand: Operand { value },
                decision: *decision,
            });
            self.argument_receiver_loans
                .extend_from_slice(&loans[loan_floor..]);
        }
        self.argument_receiver_loans.truncate(argument_loan_depth);
        let lowered_args: Vec<_> = placed.into_iter().flatten().collect();
        let argument_temporaries: Vec<_> = self
            .owned_live
            .keys()
            .filter(|value| {
                !live_before_arguments.contains(value)
                    && !lowered_args.iter().any(|arg| {
                        arg.decision == crate::BoundaryDecision::Move
                            && arg.operand.value == **value
                    })
            })
            .copied()
            .collect();
        for argument in &lowered_args {
            if argument.decision == crate::BoundaryDecision::Move {
                self.owned_live.remove(&argument.operand.value);
            }
        }
        let live_at_call = self.owned_live.clone();
        let (result, normal, continuation) = if signature.result == ResolvedTy::Unit {
            (
                CallResult::Unit,
                Edge {
                    target: self.new_block(Vec::new()),
                    args: Vec::new(),
                },
                None,
            )
        } else {
            let own = OwnKind::of_ty(&signature.result, self.service.checked_facts.rows())?;
            let raw = self.fresh_value();
            let continuation = self.fresh_value();
            let normal = self.new_block(vec![BlockArg {
                value: continuation,
                ty: signature.result.clone(),
                own,
            }]);
            (
                CallResult::Value(ValueDef {
                    id: raw,
                    ty: signature.result.clone(),
                    own,
                }),
                Edge {
                    target: normal,
                    args: vec![Operand { value: raw }],
                },
                Some(continuation),
            )
        };
        let id = OpId(self.ops);
        self.ops += 1;
        let normal_target = normal.target;
        self.set_terminator(SemTerminator::ExternCall {
            id,
            signature: Box::new(signature),
            args: lowered_args,
            result,
            normal,
            unwind: CallUnwind::NotApplicable,
        })?;
        self.current = normal_target;
        self.owned_live = live_at_call;
        self.end_call_loans(&loans)?;
        for value in argument_temporaries.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        if let Some(continuation) = continuation {
            let ty = self
                .value_ty(continuation)
                .ok_or_else(|| "extern continuation lost its type".to_string())?;
            if self.value_own_kind(continuation) == Some(OwnKind::Owned) {
                self.owned_live.insert(continuation, ty);
            }
        } else if value_required {
            return Err(format!(
                "unit-valued extern `{endpoint}` cannot produce an SSA value"
            ));
        }
        Ok(continuation)
    }
}
