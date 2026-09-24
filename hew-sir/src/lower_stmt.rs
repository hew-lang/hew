//! Statement, block and assignment lowering.

use super::{
    lower_initial_unit_return, lower_initial_value_transfer, require_initial_scalar_read, tasks,
    AggregateSelection, BindingId, BindingLoans, BindingPlace, BindingTarget, Builder, CallTarget,
    Edge, HirBlock, HirExpr, HirExprKind, HirStmtKind, IntentKind, Operand, OwnKind,
    OwnedBindingUse, PlaceOrigin, Provenance, ResolvedRef, ResolvedTy, ScalarAggregateParent,
    SemOpKind, SemTerminator, TypeInstanceKey, ValueId,
};

impl Builder<'_, '_> {
    /// Lower one `let` statement: evaluate its initializer, name the value and
    /// record any loan the initializer took on a collection.
    pub(super) fn lower_let_statement(
        &mut self,
        binding: &hew_hir::HirBinding,
        value: Option<&HirExpr>,
    ) -> Result<(), String> {
        if let Some(expr) = value.filter(|expr| self.ty(&expr.ty) == ResolvedTy::Never) {
            self.lower_discarded_expr(expr)?;
            if self.is_open() {
                return Err(
                    "Never-typed binding initializer did not terminate its SIR block".to_string(),
                );
            }
            return Ok(());
        }
        let loan_floor = self.scope_loans.len();
        let value = value
            .map(|expr| {
                if !binding.is_consume {
                    if let HirExprKind::BindingRef {
                        resolved: ResolvedRef::Binding(source),
                        ..
                    } = &expr.kind
                    {
                        if let BindingTarget::Value(source) = self.binding_target(*source)? {
                            let ty = self.ty(&expr.ty);
                            self.service.require_type_facts(&ty)?;
                            if self.value_own_kind(source) == Some(OwnKind::Guaranteed)
                                && self.service.checked_facts.rows()[&TypeInstanceKey(ty)].clone
                                    == hew_types::CloneKind::None
                            {
                                return self
                                    .lower_expr_with_binding_use(expr, OwnedBindingUse::Probe);
                            }
                        }
                    }
                }
                lower_initial_value_transfer(
                    self,
                    expr,
                    "binding initializer",
                    if binding.is_consume {
                        OwnedBindingUse::Move
                    } else {
                        OwnedBindingUse::Copy
                    },
                )
            })
            .transpose()?
            .ok_or_else(|| {
                "uninitialised bindings are not in the initial SIR subset".to_string()
            })?;
        // §1.6: the value a binding names carries the binding's name, span and
        // mutability, so a rule 2, 3, 4 or 6 violation rooted in it renders its
        // `E_OWN_*` code rather than `E_SIR_ICE`, and rule 6a has a mutability
        // bit to read. A `let` aliases the SSA value its initializer produced
        // rather than defining one of its own, so the provenance lands on that
        // definition — and only when it has none, because `let y = x` must not
        // rename the parameter `x` already named.
        let value = self.coerce_value(value, &self.ty(&binding.ty), Provenance::Synthesized)?;
        // The binding names the initializer's loans: they end at its last use
        // rather than at this scope's exit. Their root is the collection the
        // read borrowed.
        if self.scope_loans.len() > loan_floor {
            let loans = self.scope_loans[loan_floor..].to_vec();
            let root = self.value_borrow_root(loans[0])?;
            self.binding_loans.push(BindingLoans {
                root,
                loans,
                loop_depth: self.loops.len(),
                branch_depth: self.branch_depth,
            });
        }
        self.bind_source_value(binding, value)
    }

    pub(super) fn lower_block(
        &mut self,
        block: &HirBlock,
        tail_binding_use: OwnedBindingUse,
    ) -> Result<Option<Operand>, String> {
        for statement in &block.statements {
            if !self.is_open() {
                break;
            }
            match &statement.kind {
                HirStmtKind::Let(binding, value) => {
                    self.lower_let_statement(binding, value.as_ref())?;
                }
                HirStmtKind::Expr(expr) => {
                    let loan_floor = self.scope_loans.len();
                    self.lower_discarded_expr(expr)?;
                    // A discarded expression cannot retain a borrowed result.
                    // End its interior element loans before the next statement,
                    // so an indexed field read does not freeze its collection.
                    if self.is_open() && self.scope_loans.len() > loan_floor {
                        let loans = self.scope_loans.split_off(loan_floor);
                        self.end_call_loans(&loans)?;
                    }
                }
                HirStmtKind::Return(value) => {
                    self.lower_function_return(value.as_ref())?;
                }
                HirStmtKind::Assign {
                    target,
                    value,
                    first_store,
                } => {
                    self.lower_assignment(target, value, *first_store)?;
                }
                HirStmtKind::Destructure { value, fields } => {
                    self.lower_destructure(value, fields)?;
                }
                HirStmtKind::Defer { body, scope_id } => {
                    self.register_defer(body, scope_id.0)?;
                }
            }
        }
        if self.is_open() {
            match block.tail.as_deref() {
                Some(expr) if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                    let divergent = self.ty(&expr.ty) == ResolvedTy::Never;
                    self.lower_discarded_expr(expr)?;
                    if divergent && self.is_open() {
                        return Err(
                            "Never-typed block tail did not terminate its SIR block".to_string()
                        );
                    }
                    Ok(None)
                }
                Some(expr) => Ok(Some(Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "block tail value",
                        tail_binding_use,
                    )?,
                })),
                None => Ok(None),
            }
        } else {
            Ok(None)
        }
    }

    pub(super) fn lower_scoped_block(
        &mut self,
        block: &HirBlock,
        tail_binding_use: OwnedBindingUse,
    ) -> Result<Option<Operand>, String> {
        let floor = self.scopes.len();
        let live_before = self.owned_live.clone();
        self.open_scope();
        let result = self.lower_block(block, tail_binding_use)?;
        if self.is_open() {
            self.end_scopes(floor)?;
            // Temporaries created inside the block die with it, exactly as its
            // bindings do. Only the block's own result leaves; without this a
            // conditional block hands its leftover owners to the join, which
            // then sees predecessors with different live temporaries.
            let mut keep = live_before;
            if let Some(result) = &result {
                if let Some(ty) = self.owned_live.get(&result.value) {
                    keep.insert(result.value, ty.clone());
                }
            }
            self.destroy_live_since(&keep)?;
        }
        self.leave_scope();
        Ok(result)
    }

    pub(super) fn lower_assignment(
        &mut self,
        target: &HirExpr,
        value: &HirExpr,
        first_store: bool,
    ) -> Result<(), String> {
        if let HirExprKind::Index { container, index } = &target.kind {
            let family = match self.ty(&container.ty) {
                ResolvedTy::Array(_, _) => Some(hew_types::RuntimeCallFamily::Array(
                    hew_types::runtime_call::ArrayValueOp::Set,
                )),
                ResolvedTy::Bytes => Some(hew_types::RuntimeCallFamily::BytesSet),
                ResolvedTy::Named {
                    builtin: Some(hew_types::BuiltinType::Vec),
                    ..
                } => Some(hew_types::RuntimeCallFamily::Vector(
                    hew_types::runtime_call::VecValueOp::Set,
                )),
                _ => None,
            };
            if let Some(family) = family {
                let mut operation = target.clone();
                operation.ty = ResolvedTy::Unit;
                let mut replacement = value.clone();
                replacement.intent = IntentKind::Read;
                self.lower_runtime_operation(
                    &operation,
                    family,
                    &[container.as_ref(), index.as_ref(), &replacement],
                    false,
                )?;
                return Ok(());
            }
        }
        if matches!(
            target.kind,
            HirExprKind::FieldAccess { .. } | HirExprKind::TupleIndex { .. }
        ) {
            return self.lower_field_assignment(target, value);
        }
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = &target.kind
        else {
            return Err("assignment requires a resolved local binding target".into());
        };
        let declaration = *self
            .binding_declarations
            .get(binding)
            .ok_or_else(|| "assignment target has no declaration".to_string())?;
        if !self.source_bindings[declaration].mutable {
            return Err("assignment target is not mutable".into());
        }
        let target = self.binding_target(*binding)?;
        let ty = self.target_ty(target)?;
        let new =
            lower_initial_value_transfer(self, value, "assignment value", OwnedBindingUse::Copy)?;
        let new = self.coerce_value(new, &ty, Provenance::Site(value.site))?;
        match target {
            BindingTarget::Place(place) if first_store || self.state_taken.contains(&place) => {
                // A deferred field, a mutable field consumed earlier in this
                // body, or a `var self` receiver moved out earlier has an
                // empty seat. Publish the replacement without trying to
                // release the value that left it.
                let initialized = match self.places[place.0 as usize].origin {
                    PlaceOrigin::ActorState { initialized, .. } => initialized,
                    _ if !first_store && self.in_var_self_receiver(place) => true,
                    _ => {
                        return Err(
                            "a first store requires an uninitialized actor state seat".into()
                        )
                    }
                };
                self.emit_place_operation(
                    SemOpKind::StoreInit {
                        place,
                        value: Operand { value: new },
                    },
                    Provenance::Site(value.site),
                )?;
                self.owned_live.remove(&new);
                self.state_taken.remove(&place);
                if !initialized {
                    self.deferred_initialized.insert(place);
                }
                Ok(())
            }
            BindingTarget::Place(place) => {
                self.store_projected(place, new, Provenance::Site(value.site))
            }
            BindingTarget::Value(_) => {
                if self.value_own_kind(new) == Some(OwnKind::Owned) {
                    let BindingTarget::Place(place) = self.source_bindings[declaration].target
                    else {
                        return Err("owned assignment has no lexical storage declaration".into());
                    };
                    self.store_projected(place, new, Provenance::Site(value.site))?;
                    self.bindings.insert(*binding, BindingTarget::Place(place));
                    return Ok(());
                }
                self.bindings.insert(*binding, BindingTarget::Value(new));
                self.record_binding_version(*binding, new)
            }
        }
    }

    /// Assignment and runtime receiver mutation resolve and rebuild the same
    /// mutable place. Evaluate the RHS before taking its current root apart.
    pub(super) fn lower_field_assignment(
        &mut self,
        target: &HirExpr,
        value: &HirExpr,
    ) -> Result<(), String> {
        let path = self.resolve_writable_path(target)?;
        if Self::path_is_indexed(&path) {
            let replacement = lower_initial_value_transfer(
                self,
                value,
                "indexed field assignment",
                OwnedBindingUse::Copy,
            )?;
            let replacement = self.coerce_value(
                replacement,
                &self.ty(&target.ty),
                Provenance::Site(value.site),
            )?;
            let provenance = Provenance::Site(target.site);
            let (container, root) = self.stage_indexed_base(&path.base, &provenance)?;
            let (old, writeback) =
                self.acquire_indexed_path(path, container, root, false, &provenance)?;
            if self.owned_live.contains_key(&old) {
                self.emit_destroy(old)?;
            }
            return self.publish_indexed_path(writeback, replacement, &provenance);
        }
        let place = path.base;
        let replacement = lower_initial_value_transfer(
            self,
            value,
            "record field assignment",
            OwnedBindingUse::Copy,
        )?;
        let replacement =
            self.coerce_value(replacement, &place.leaf_ty, Provenance::Site(value.site))?;
        let provenance = Provenance::Site(target.site);
        if let Some(projected) = self.owned_projection(&place)? {
            return self.store_projected(projected, replacement, provenance);
        }
        if let Some(root) = self.whole_owner_root(&place)? {
            return self.assign_through_whole_owner(root, &place, replacement, provenance);
        }
        let (_, parents) = self.take_scalar_place(&place, &provenance)?;
        self.replace_scalar_aggregate_leaf(place.binding, replacement, parents, &provenance)
    }

    pub(super) fn resolve_mutable_place(
        &mut self,
        target: &HirExpr,
    ) -> Result<BindingPlace, String> {
        let place = self
            .resolve_binding_place(target)?
            .ok_or_else(|| "mutable place requires a local binding root".to_string())?;
        let declaration = *self
            .binding_declarations
            .get(&place.binding)
            .ok_or_else(|| {
                format!(
                    "mutable place root `{}` has no source declaration",
                    place.binding
                )
            })?;
        if !self.source_bindings[declaration].mutable {
            return Err(format!(
                "mutable place root `{}` is not mutable",
                place.binding
            ));
        }
        Ok(place)
    }

    /// The aggregate selections between `target` and the expression they are
    /// rooted at, outermost first. The root is whatever the chain reaches: a
    /// binding, a collection element, or any other expression.
    pub(super) fn projection_chain<'expr>(
        &mut self,
        target: &'expr HirExpr,
    ) -> Result<(&'expr HirExpr, Vec<AggregateSelection>), String> {
        let mut root = target;
        let mut projections = Vec::new();
        loop {
            let (object, shape, index) = match &root.kind {
                HirExprKind::FieldAccess { object, field } => {
                    let (shape, index) = self.aggregate_projection_shape(root, object, field)?;
                    (object.as_ref(), shape, index)
                }
                HirExprKind::TupleIndex { tuple, index } => {
                    let index = self.tuple_projection_index(root, tuple, *index)?;
                    let shape = self.service.require_aggregate_shape(&self.ty(&tuple.ty))?;
                    (tuple.as_ref(), shape, index)
                }
                HirExprKind::SubsumedValue { source } => {
                    root = source;
                    continue;
                }
                _ => break,
            };
            projections.push((
                self.ty(&object.ty),
                shape,
                usize::try_from(index).map_err(|_| "mutable place field exceeds usize")?,
            ));
            root = object;
        }
        projections.reverse();
        Ok((root, projections))
    }

    pub(super) fn resolve_binding_place(
        &mut self,
        target: &HirExpr,
    ) -> Result<Option<BindingPlace>, String> {
        let (root, projections) = self.projection_chain(target)?;
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(binding),
            ..
        } = root.kind
        else {
            return Ok(None);
        };
        Ok(Some(BindingPlace {
            binding,
            root_ty: self.ty(&root.ty),
            leaf_ty: self.ty(&target.ty),
            projections,
        }))
    }

    /// Extract a scalar leaf and retain its non-owning sibling fields.
    pub(super) fn take_scalar_place(
        &mut self,
        place: &BindingPlace,
        provenance: &Provenance,
    ) -> Result<(ValueId, Vec<ScalarAggregateParent>), String> {
        let mut current = self.scalar_binding(place.binding)?;
        if self.value_own_kind(current) != Some(OwnKind::None)
            || self.value_ty(current).as_ref() != Some(&place.root_ty)
        {
            return Err("scalar aggregate update requires a non-owning root".into());
        }
        let mut parents = Vec::new();
        for (ty, shape, index) in &place.projections {
            let fields = self.emit_destructure_value(current, ty, *shape, provenance.clone())?;
            if fields.iter().any(|field| field.own != OwnKind::None) {
                return Err("scalar aggregate update cannot acquire ownership".into());
            }
            current = fields[*index].id;
            parents.push(ScalarAggregateParent {
                ty: ty.clone(),
                shape: *shape,
                index: *index,
                fields,
            });
        }
        Ok((current, parents))
    }

    /// Rebuild a non-owning aggregate after a scalar field assignment.
    pub(super) fn replace_scalar_aggregate_leaf(
        &mut self,
        binding: BindingId,
        replacement: ValueId,
        parents: Vec<ScalarAggregateParent>,
        provenance: &Provenance,
    ) -> Result<(), String> {
        if self.value_own_kind(replacement) != Some(OwnKind::None) {
            return Err("scalar aggregate replacement cannot carry ownership".into());
        }
        let mut updated = replacement;
        for ScalarAggregateParent {
            ty,
            shape,
            index,
            fields,
        } in parents.into_iter().rev()
        {
            let fields = fields
                .into_iter()
                .enumerate()
                .map(|(position, field)| Operand {
                    value: if position == index { updated } else { field.id },
                })
                .collect();
            updated = self.emit_typed(
                provenance.clone(),
                &ty,
                SemOpKind::AggregateMake { shape, fields },
            )?;
        }
        self.bindings.insert(binding, BindingTarget::Value(updated));
        self.record_binding_version(binding, updated)
    }

    /// Lower an expression whose value is intentionally discarded.
    ///
    /// Scalar expressions keep their ordinary one-result SSA operation even
    /// when the result is unused.  A unit direct call is different: there is
    /// no semantic value to define, but the call itself must remain in SIR so
    /// later lowering can realize its call/continuation CFG edge.
    #[expect(
        clippy::too_many_lines,
        reason = "effect-position dispatch keeps control flow and cleanup together"
    )]
    pub(super) fn lower_discarded_expr(&mut self, expr: &HirExpr) -> Result<(), String> {
        // A statement whose value is discarded reaches several lowerings that
        // do not go through `lower_expr_inner`, so stamp its source point here
        // as well: a call statement's terminator must name its own line.
        self.current_site = Some(expr.site);
        match &expr.kind {
            HirExprKind::ActorDelivery {
                operation: hew_types::actor_delivery::ActorDeliveryCall::AwaitClosed,
                ..
            } => {
                self.lower_actor_boundary(expr)?;
                return Ok(());
            }
            HirExprKind::ActorDelivery { .. } => {
                let value = self.lower_actor_delivery(expr)?;
                if self.owned_live.contains_key(&value) {
                    self.emit_destroy(value)?;
                }
                return Ok(());
            }
            HirExprKind::Yield { value, yield_ty } => {
                return self.lower_generator_yield(expr, value.as_deref(), yield_ty)
            }
            HirExprKind::ScopeRecovery {
                scope,
                error,
                handler,
            } if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                self.lower_scope_recovery(expr, scope, error, handler)?;
                return Ok(());
            }

            HirExprKind::Select(select)
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_select(expr, select)?;
                return Ok(());
            }
            HirExprKind::AwaitTask { operand, .. }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_task_await(expr, operand)?;
                return Ok(());
            }
            HirExprKind::Race { body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never)
                    || tasks::contains_task(&self.ty(&expr.ty)) =>
            {
                self.lower_race(body, false)?;
                return Ok(());
            }
            HirExprKind::Scope { body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never)
                    || tasks::contains_task(&self.ty(&expr.ty)) =>
            {
                self.lower_task_scope(body, false)?;
                return Ok(());
            }
            HirExprKind::ScopeDeadline { duration, body }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never)
                    || tasks::contains_task(&self.ty(&expr.ty)) =>
            {
                self.lower_task_scope_with_deadline(body, Some(duration), false)?;
                return Ok(());
            }
            HirExprKind::SubsumedValue { source } => {
                if self.ty(&source.ty) != self.ty(&expr.ty) {
                    return Err(
                        "transparent discarded expression must preserve its exact type".into(),
                    );
                }
                return self.lower_discarded_expr(source);
            }

            HirExprKind::Return { value } => return self.lower_function_return(value.as_deref()),
            HirExprKind::Call {
                target: CallTarget::Builtin { endpoint },
                args,
                ..
            } if endpoint == "panic" => return self.lower_panic(expr, args),
            _ => {}
        }
        if expr.intent != IntentKind::Consume {
            require_initial_scalar_read(expr.intent)
                .map_err(|reason| format!("discarded expression: {reason}"))?;
        }
        let live_before_expression: std::collections::HashSet<_> =
            self.owned_live.keys().copied().collect();
        if expr.intent == IntentKind::Consume
            && !matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never)
        {
            // `let _ = value` takes the value and releases it here.
            let value =
                lower_initial_value_transfer(self, expr, "discarded value", OwnedBindingUse::Move)?;
            if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value) {
                self.emit_destroy(value)?;
            }
            return Ok(());
        }
        match &expr.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(_),
                ..
            } => {
                // Reading an existing binding in effect position needs no
                // owned copy. Keep the borrow so availability is still checked.
                let mut loans = Vec::new();
                self.lower_borrowed_read(expr, &mut loans)?;
                return self.end_call_loans(&loans);
            }
            HirExprKind::Block(block) => {
                if let Some(value) = self.lower_scoped_block(block, OwnedBindingUse::Copy)? {
                    if self.owned_live.contains_key(&value.value)
                        && !live_before_expression.contains(&value.value)
                    {
                        self.emit_destroy(value.value)?;
                    }
                }
                return Ok(());
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) => {
                return self.lower_unit_if(condition, then_expr, else_expr.as_deref());
            }
            HirExprKind::Match { scrutinee, arms }
                if matches!(self.ty(&expr.ty), ResolvedTy::Unit | ResolvedTy::Never) =>
            {
                self.lower_match_control(expr, scrutinee, arms)?;
                return Ok(());
            }
            HirExprKind::Break { label, value } => {
                return self.lower_loop_exit(false, label.as_deref(), value.as_deref());
            }
            HirExprKind::Continue { label } => {
                return self.lower_loop_exit(true, label.as_deref(), None);
            }
            HirExprKind::While {
                label,
                condition,
                body,
            } => {
                return self.lower_while(label.as_deref(), Some(condition), body);
            }
            HirExprKind::Loop { label, body } => {
                return self.lower_while(label.as_deref(), None, body);
            }
            HirExprKind::ForRange {
                label,
                binding,
                start,
                end,
                inclusive,
                step,
                descending,
                body,
            } => {
                return self.lower_for_range(
                    label.as_deref(),
                    binding,
                    start,
                    end,
                    step,
                    *inclusive,
                    *descending,
                    body,
                );
            }
            _ => {}
        }
        if matches!(
            expr.kind,
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. }
        ) {
            if let Some(value) = self.lower_call(expr, false)? {
                if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value)
                {
                    self.emit_destroy(value)?;
                }
            }
            return Ok(());
        }
        let value = self.lower_expr(expr)?;
        if self.owned_live.contains_key(&value) && !live_before_expression.contains(&value) {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    /// Preserve the panic message before releasing its owner and propagating
    /// the active fault through the ordinary function cleanup boundary.
    pub(super) fn lower_panic(&mut self, expr: &HirExpr, args: &[HirExpr]) -> Result<(), String> {
        let [message] = args else {
            return Err("panic requires exactly one string message".into());
        };
        if self.ty(&message.ty) != ResolvedTy::String || self.ty(&expr.ty) != ResolvedTy::Never {
            return Err("panic requires a string message and a Never result".into());
        }
        let mut loans = Vec::new();
        let operand = self.lower_call_read(message, &mut loans, true, true)?;
        self.finish_panic(operand, &loans)
    }

    pub(super) fn lower_assert(&mut self, expr: &HirExpr, args: &[HirExpr]) -> Result<(), String> {
        let [condition] = args else {
            return Err("assert requires exactly one boolean condition".into());
        };
        if self.ty(&condition.ty) != ResolvedTy::Bool || self.ty(&expr.ty) != ResolvedTy::Unit {
            return Err("assert requires a boolean condition and a unit result".into());
        }
        let condition = self.lower_read_operand(condition, "assert condition")?;
        let success = self.new_block(Vec::new());
        let failure = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: success,
                args: Vec::new(),
            },
            else_target: Edge {
                target: failure,
                args: Vec::new(),
            },
        })?;
        let before = self.control_state();
        self.current = failure;
        let literal = self.service.intern_string("assertion failed");
        let message = self.emit_typed(
            Provenance::Site(expr.site),
            &ResolvedTy::String,
            SemOpKind::ConstStr(literal),
        )?;
        self.finish_panic(Operand { value: message }, &[])?;
        self.restore_control_state(&before);
        self.current = success;
        Ok(())
    }

    pub(super) fn finish_panic(
        &mut self,
        operand: Operand,
        loans: &[ValueId],
    ) -> Result<(), String> {
        let cleanup = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Panic {
            message: crate::BoundaryOperand {
                operand,
                decision: crate::BoundaryDecision::Borrow,
            },
            cleanup: Edge {
                target: cleanup,
                args: Vec::new(),
            },
        })?;
        self.current = cleanup;
        self.end_call_loans(loans)?;
        self.finish_fault_exit()
    }

    /// Seal the current block with the one function-return cleanup contract.
    /// Both statement returns and Never-typed HIR return expressions use this
    /// path, so a divergent expression cannot manufacture a placeholder SSA
    /// value or continue evaluating sibling operands.
    pub(super) fn lower_function_return(&mut self, value: Option<&HirExpr>) -> Result<(), String> {
        if self.in_deferred_body() {
            return Err("return and error propagation cannot escape a deferred body".into());
        }
        let mut value = match value {
            Some(expr) if self.ty(&expr.ty) == ResolvedTy::Unit => {
                lower_initial_unit_return(self, expr)?;
                None
            }
            Some(expr) => Some(crate::BoundaryOperand {
                operand: Operand {
                    value: lower_initial_value_transfer(
                        self,
                        expr,
                        "return value",
                        OwnedBindingUse::Return,
                    )?,
                },
                decision: crate::BoundaryDecision::Move,
            }),
            None => None,
        };
        if let Some(value) = &mut value {
            value.operand.value = self.coerce_value(
                value.operand.value,
                &self.callable.signature.return_ty.clone(),
                Provenance::Synthesized,
            )?;
        }
        self.finish_return_value(value)
    }
}
