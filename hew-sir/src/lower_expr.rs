//! Core expression lowering: literals, variants and match dispatch.

use super::{
    collection_type_arguments, hir_expr_kind_name, lower_initial_value_transfer,
    shared_handle_family, AggregateShapeRef, BTreeMap, Binding, BindingId, BindingTarget, BlockArg,
    Builder, CallResult, CallTarget, CallUnwind, ControlState, Edge, HirExpr, HirExprKind, HirItem,
    HirLiteral, MatchExit, OpId, Operand, OwnKind, OwnedBindingUse, Provenance, Range, ResolvedRef,
    ResolvedTy, SemOp, SemOpKind, SemTerminator, SemVariantArm, SemVariantShape, ValueDef, ValueId,
    VariantBranch, VariantShapeId,
};

impl Builder<'_, '_> {
    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    pub(super) fn lower_expr(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        self.lower_expr_with_binding_use(expr, OwnedBindingUse::Copy)
    }

    /// A value result owns its contents or copies its bits. Only a Guaranteed
    /// result carries a loan across the expression boundary. This also covers
    /// projections and aggregate construction over borrowed collection reads.
    pub(super) fn lower_expr_with_binding_use(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        let loan_floor = self.scope_loans.len();
        let value = self.lower_expr_inner(expr, binding_use)?;
        if self.is_open()
            && matches!(
                self.value_own_kind(value),
                Some(OwnKind::Owned | OwnKind::None)
            )
        {
            self.end_expression_loans(loan_floor)?;
        }
        Ok(value)
    }

    pub(super) fn end_expression_loans(&mut self, floor: usize) -> Result<(), String> {
        let interior = self.scope_loans.split_off(floor);
        self.end_call_loans(&interior)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the closed initial HIR-to-SIR expression mapping remains intentionally local"
    )]
    pub(super) fn lower_expr_inner(
        &mut self,
        expr: &HirExpr,
        binding_use: OwnedBindingUse,
    ) -> Result<ValueId, String> {
        self.current_site = Some(expr.site);
        // A supervisor pool accessor is decided by the checker, not by the
        // expression shape: `sup.pool[i]` and `sup.pool.get(i)` are an ordinary
        // index and call until this site table says otherwise.
        if let Some(kind) = self
            .service
            .module
            .pool_accessor_sites
            .get(&expr.site)
            .map(|accessor| accessor.kind)
        {
            return self.lower_pool_accessor(expr, kind, false);
        }
        match &expr.kind {
            HirExprKind::Literal(literal) => self.lower_literal(expr, literal),
            HirExprKind::RcIntrinsic {
                op,
                receiver,
                value,
                ..
            } => {
                let mut operands: Vec<&HirExpr> = Vec::with_capacity(2);
                operands.extend(receiver.as_deref());
                operands.extend(value.as_deref());
                let family = shared_handle_family(*op);
                if self.ty(&expr.ty) == ResolvedTy::Unit {
                    self.lower_runtime_operation(expr, family, &operands, false)?;
                    return self.emit(expr, SemOpKind::ConstUnit);
                }
                self.lower_runtime_operation(expr, family, &operands, true)?
                    .ok_or_else(|| format!("`{op:?}` must produce a shared-handle value"))
            }
            HirExprKind::Select(select) => match self.lower_task_select(expr, select)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent select cannot produce a SIR value".into()),
            },
            HirExprKind::GenBlock { .. } => self.lower_generator(expr),
            HirExprKind::GeneratorNext { receiver, .. } => {
                self.lower_generator_next(expr, receiver)
            }
            HirExprKind::Yield { value, yield_ty } => {
                self.lower_generator_yield(expr, value.as_deref(), yield_ty)?;
                self.emit(expr, SemOpKind::ConstUnit)
            }
            HirExprKind::Closure { .. } => self.lower_closure(expr),
            HirExprKind::ForkBlock { body, captures, .. } => {
                self.lower_fork_block(expr, body, captures)
            }
            HirExprKind::AwaitTask { operand, .. } => match self.lower_task_await(expr, operand)? {
                Some(value) => Ok(value),
                None => self.emit(expr, SemOpKind::ConstUnit),
            },
            HirExprKind::Race { body } => match self.lower_race(body, true)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent race cannot produce a SIR value".into()),
            },
            HirExprKind::Scope { body } => match self.lower_task_scope(body, true)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent scope cannot produce a SIR value".into()),
            },
            HirExprKind::ScopeDeadline { duration, body } => {
                match self.lower_task_scope_with_deadline(body, Some(duration), true)? {
                    Some(value) => Ok(value),
                    None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                    None => Err("divergent scope cannot produce a SIR value".into()),
                }
            }
            HirExprKind::ScopeRecovery {
                scope,
                error,
                handler,
            } => match self.lower_scope_recovery(expr, scope, error, handler)? {
                Some(value) => Ok(value),
                None if self.is_open() => self.emit(expr, SemOpKind::ConstUnit),
                None => Err("divergent recovery cannot produce a SIR value".into()),
            },
            HirExprKind::WireCodec {
                direction,
                operand,
                value_ty,
            } => self.lower_wire_codec(expr, *direction, operand, value_ty),
            HirExprKind::RecordCloneCall { src, .. } => {
                let mut loans = Vec::new();
                let source = self.lower_borrowed_read(src, &mut loans)?;
                // A record whose fields all copy by value carries no ownership,
                // so the borrowed read is already the independent copy. Only an
                // owned record needs the ownership operation.
                let ty = self.ty(&expr.ty);
                let result =
                    if OwnKind::of_ty(&ty, self.service.checked_facts.rows())? == OwnKind::Owned {
                        self.emit(expr, SemOpKind::CopyValue { source })?
                    } else {
                        source.value
                    };
                self.end_call_loans(&loans)?;
                Ok(result)
            }
            HirExprKind::Spawn { .. } => self
                .lower_actor_boundary(expr)?
                .ok_or_else(|| "actor spawn lacks its handle result".into()),
            HirExprKind::ActorSelf => self
                .lower_actor_boundary(expr)?
                .ok_or_else(|| "`self` lacks its actor handle result".into()),
            HirExprKind::ActorMessage { .. } => self.lower_actor_message(expr),
            HirExprKind::ActorDelivery { .. } => self.lower_actor_delivery(expr),
            HirExprKind::ActorAsk { .. } => self.lower_actor_ask(expr),
            HirExprKind::RemoteActorAsk { .. } => self.lower_remote_actor_ask(expr),
            HirExprKind::RemoteActorSend { .. } => self.lower_remote_actor_send(expr),
            HirExprKind::ActorGenStream { .. } => self.lower_actor_stream(expr),
            HirExprKind::CoerceToDynTrait {
                value,
                concrete_type,
                vtable_entries,
                ..
            } => self.lower_dyn_make(expr, value, concrete_type, vtable_entries),
            HirExprKind::CallDynMethod {
                receiver,
                target,
                args,
                evaluation_order,
                signature,
                ..
            } => self
                .lower_dyn_call(expr, receiver, target, args, evaluation_order, signature)?
                .ok_or_else(|| "dynamic dispatch produced no SIR value".to_string()),
            HirExprKind::ArrayLiteral { elements } => self.lower_array_make(expr, elements),
            HirExprKind::ArrayRepeat { value } => self.lower_array_repeat(expr, value),
            HirExprKind::TupleLiteral { elements } => self.lower_tuple_make(expr, elements),
            HirExprKind::TupleIndex { tuple, index } => self.lower_tuple_get(expr, tuple, *index),
            HirExprKind::StructInit { fields, base, .. } => {
                self.lower_aggregate_make(expr, fields, base.as_deref())
            }
            HirExprKind::MachineVariantCtor {
                state_idx, payload, ..
            } => self.lower_variant_make(expr, *state_idx, payload.as_deref()),
            HirExprKind::AwaitRestart { child } => self.lower_supervisor_await_restart(expr, child),
            HirExprKind::FieldAccess { object, field } => {
                if let Some(slot) = self.service.module.supervisor_child_slots.get(&expr.site) {
                    return self.lower_supervisor_child(expr, object, slot);
                }
                self.lower_aggregate_project(expr, object, field)
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                ..
            } => {
                if let Some((_, field)) = self.capture_field(*binding) {
                    let take = binding_use == OwnedBindingUse::Move
                        && field.consumption == hew_types::ClosureCaptureConsumption::Consumed;
                    return self.load_capture(*binding, Provenance::Site(expr.site), take);
                }
                match self.binding_target(*binding)? {
                    BindingTarget::Value(value) => {
                        if !matches!(binding_use, OwnedBindingUse::Copy | OwnedBindingUse::Probe) {
                            self.require_selected_binding(*binding, value)?;
                        }
                        Ok(value)
                    }
                    BindingTarget::Place(place) => self.emit(expr, SemOpKind::LoadCopy { place }),
                }
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Const(item),
                ..
            } => {
                let constant = self
                    .service
                    .module
                    .items
                    .iter()
                    .find_map(|candidate| match candidate {
                        HirItem::Const(constant) if constant.id == *item => Some(constant),
                        _ => None,
                    })
                    .ok_or("const reference has no HIR declaration")?;
                let literal = match &constant.value {
                    hew_hir::HirConstValue::Integer(value) => HirLiteral::Integer(*value),
                    hew_hir::HirConstValue::String(value) => HirLiteral::String(value.clone()),
                    hew_hir::HirConstValue::Float(value) => HirLiteral::Float(*value),
                };
                self.lower_literal(expr, &literal)
            }
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Item(item),
                ..
            } => {
                let declaration = self
                    .service
                    .table
                    .functions_by_item
                    .get(item)
                    .ok_or_else(|| "function value has no checked HIR declaration".to_string())?
                    .declaration
                    .clone();
                let target = self.service.resolve_direct_call(
                    &declaration,
                    expr.site,
                    &self.substitution,
                )?;
                let ty = ResolvedTy::Function {
                    params: target
                        .signature
                        .params
                        .iter()
                        .map(|param| param.ty.clone())
                        .collect(),
                    ret: Box::new(target.signature.return_ty.clone()),
                    capabilities: hew_types::CallableCapabilities::FUNCTION_ITEM,
                };
                let value = self.emit_typed(
                    Provenance::Site(expr.site),
                    &ty,
                    SemOpKind::FunctionMake {
                        callable: target.id,
                    },
                )?;
                self.coerce_value(value, &self.ty(&expr.ty), Provenance::Site(expr.site))
            }
            HirExprKind::Unary { op, operand, .. } => {
                let value = self.lower_read_operand(operand, "unary operand")?;
                if *op == hew_parser::ast::UnaryOp::Negate && self.ty(&expr.ty).is_signed_integer()
                {
                    let zero = self.emit(expr, SemOpKind::ConstInteger(0))?;
                    self.lower_checked_binary(
                        expr,
                        hew_parser::ast::BinaryOp::Subtract,
                        Operand { value: zero },
                        value,
                    )
                } else {
                    self.emit(expr, SemOpKind::Unary { op: *op, value })
                }
            }
            HirExprKind::IdentityCompare { left, right } => {
                let lhs = self.lower_read_operand(left, "identity left operand")?;
                let rhs = self.lower_read_operand(right, "identity right operand")?;
                self.emit(
                    expr,
                    SemOpKind::Binary {
                        op: hew_parser::ast::BinaryOp::Equal,
                        lhs,
                        rhs,
                    },
                )
            }
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::And,
                left,
                right,
            } => self.lower_logical_and(expr, left, right),
            HirExprKind::Binary {
                op: hew_parser::ast::BinaryOp::Or,
                left,
                right,
            } => self.lower_logical_or(expr, left, right),
            HirExprKind::Binary { op, left, right } => {
                if matches!(
                    op,
                    hew_parser::ast::BinaryOp::Equal | hew_parser::ast::BinaryOp::NotEqual
                ) && matches!(
                    self.ty(&left.ty),
                    ResolvedTy::Bytes
                        | ResolvedTy::Tuple(_)
                        | ResolvedTy::Named { .. }
                        | ResolvedTy::Unit
                ) {
                    let equals = self.lower_value_equality(expr, [left, right])?;
                    return if *op == hew_parser::ast::BinaryOp::NotEqual {
                        self.emit(
                            expr,
                            SemOpKind::Unary {
                                op: hew_parser::ast::UnaryOp::Not,
                                value: Operand { value: equals },
                            },
                        )
                    } else {
                        Ok(equals)
                    };
                }
                if self.ty(&left.ty) == ResolvedTy::String {
                    return match op {
                        hew_parser::ast::BinaryOp::Add => self
                            .lower_runtime_operation(
                                expr,
                                hew_types::RuntimeCallFamily::StringConcat,
                                &[left.as_ref(), right.as_ref()],
                                true,
                            )?
                            .ok_or_else(|| "string concatenation must produce a value".to_string()),
                        hew_parser::ast::BinaryOp::Equal | hew_parser::ast::BinaryOp::NotEqual => {
                            let equals = self
                                .lower_runtime_operation(
                                    expr,
                                    hew_types::RuntimeCallFamily::StringEquals,
                                    &[left.as_ref(), right.as_ref()],
                                    true,
                                )?
                                .ok_or_else(|| {
                                    "string comparison must produce a value".to_string()
                                })?;
                            if *op == hew_parser::ast::BinaryOp::NotEqual {
                                self.emit(
                                    expr,
                                    SemOpKind::Unary {
                                        op: hew_parser::ast::UnaryOp::Not,
                                        value: Operand { value: equals },
                                    },
                                )
                            } else {
                                Ok(equals)
                            }
                        }
                        hew_parser::ast::BinaryOp::Less
                        | hew_parser::ast::BinaryOp::LessEqual
                        | hew_parser::ast::BinaryOp::Greater
                        | hew_parser::ast::BinaryOp::GreaterEqual => {
                            let mut comparison = expr.clone();
                            comparison.ty = ResolvedTy::I32;
                            let ordering = self
                                .lower_runtime_operation(
                                    &comparison,
                                    hew_types::RuntimeCallFamily::StringCompare,
                                    &[left.as_ref(), right.as_ref()],
                                    true,
                                )?
                                .ok_or("string ordering must produce a value")?;
                            let zero = self.emit_typed(
                                Provenance::Site(expr.site),
                                &ResolvedTy::I32,
                                SemOpKind::ConstInteger(0),
                            )?;
                            self.emit(
                                expr,
                                SemOpKind::Binary {
                                    op: *op,
                                    lhs: Operand { value: ordering },
                                    rhs: Operand { value: zero },
                                },
                            )
                        }
                        _ => Err(format!(
                            "string binary `{op}` has no ownership-SIR runtime operation"
                        )),
                    };
                }
                let lhs = self.lower_read_operand(left, "binary left operand")?;
                let rhs = self.lower_read_operand(right, "binary right operand")?;
                if crate::checked_binary_failure_kinds(*op, &self.ty(&expr.ty)).is_some() {
                    self.lower_checked_binary(expr, *op, lhs, rhs)
                } else {
                    self.emit(expr, SemOpKind::Binary { op: *op, lhs, rhs })
                }
            }
            HirExprKind::SaturatingWidthCast { .. } => self.lower_saturating_cast(expr),
            HirExprKind::TryWidthCast { .. } => self.lower_try_cast(expr),
            HirExprKind::NumericCast { value, to_ty, .. } => {
                let value = self.lower_read_operand(value, "cast operand")?;
                self.emit(
                    expr,
                    SemOpKind::Cast {
                        value,
                        to: self.ty(to_ty),
                    },
                )
            }
            HirExprKind::VarSelfMethodCall { .. } => self.lower_var_self_call(expr),
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. }
                if self.ty(&expr.ty) == ResolvedTy::Unit =>
            {
                self.lower_call(expr, false)?;
                self.emit(expr, SemOpKind::ConstUnit)
            }
            HirExprKind::Call { .. } | HirExprKind::CallTraitMethodStatic { .. } => self
                .lower_call(expr, true)?
                .ok_or_else(|| "value-producing checked call has no result".to_string()),
            HirExprKind::SubsumedValue { source, .. } => {
                self.lower_expr_with_binding_use(source, binding_use)
            }
            HirExprKind::Index { container, index }
                if matches!(self.ty(&container.ty), ResolvedTy::Array(_, _)) =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Array(
                        hew_types::runtime_call::ArrayValueOp::Index,
                    ),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "array index must produce a value".to_string())
            }
            HirExprKind::BorrowedIndex { container, index }
                if matches!(self.ty(&container.ty), ResolvedTy::Array(_, _)) =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Array(
                        hew_types::runtime_call::ArrayValueOp::IndexBorrow,
                    ),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "array index must produce an element loan".to_string())
            }
            HirExprKind::Index { container, index }
                if matches!(
                    collection_type_arguments(&self.ty(&container.ty)),
                    Some((
                        hew_types::BuiltinType::Vec | hew_types::BuiltinType::HashMap,
                        _
                    ))
                ) =>
            {
                let family = match collection_type_arguments(&self.ty(&container.ty)) {
                    Some((hew_types::BuiltinType::Vec, _)) => {
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Index)
                    }
                    Some((hew_types::BuiltinType::HashMap, _)) => {
                        hew_types::RuntimeCallFamily::Map(
                            hew_types::runtime_call::MapValueOp::Index,
                        )
                    }
                    _ => unreachable!("matched a canonical indexed collection"),
                };
                self.lower_runtime_operation(
                    expr,
                    family,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "collection index must produce a semantic copy".to_string())
            }
            HirExprKind::ResolvedImplCall {
                target:
                    CallTarget::RuntimeCollection(
                        hew_types::MethodTargetFamily::HashMap(hew_types::HashMapMethod::Clone)
                        | hew_types::MethodTargetFamily::HashSet(hew_types::HashSetMethod::Clone)
                        | hew_types::MethodTargetFamily::Vec(hew_types::VecMethod::Clone),
                    ),
                receiver,
                args,
                ..
            } if args.is_empty() => {
                let mut loans = Vec::new();
                let source = self.lower_borrowed_read(receiver, &mut loans)?;
                let copy = self.emit(expr, SemOpKind::CopyValue { source })?;
                self.end_call_loans(&loans)?;
                Ok(copy)
            }
            // D432: the checker decided this read is a loan of the element the
            // vector still owns.
            HirExprKind::BorrowedIndex { container, index } => self
                .lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::IndexBorrow),
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "borrowed element read must produce a SIR value".to_string()),
            HirExprKind::Index { container, index }
                if self.ty(&container.ty) == ResolvedTy::Bytes =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::BytesIndex,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "bytes index must produce a SIR value".to_string())
            }
            HirExprKind::Index { container, index }
                if self.ty(&container.ty) == ResolvedTy::String =>
            {
                self.lower_runtime_operation(
                    expr,
                    hew_types::RuntimeCallFamily::StringIndex,
                    &[container.as_ref(), index.as_ref()],
                    true,
                )?
                .ok_or_else(|| "string index must produce a SIR value".to_string())
            }
            // `x[a..b]` / `x[a..]` / `x[..b]` / `x[..]` over `string`, `bytes`
            // and `Vec<T>`. An absent start is the literal zero; an absent end
            // routes to the receiver's own open-ended family so the container
            // expression is evaluated exactly once. HIR has already rewritten
            // `x[a..=b]` to the exclusive `x[a..b + 1]`.
            HirExprKind::Slice {
                container,
                start,
                end,
            } => {
                let container_ty = self.ty(&container.ty);
                let (ranged, open) = if container_ty == ResolvedTy::String {
                    (
                        hew_types::RuntimeCallFamily::StringSliceCodepoints,
                        hew_types::RuntimeCallFamily::StringSliceCodepointsFrom,
                    )
                } else if container_ty == ResolvedTy::Bytes {
                    (
                        hew_types::RuntimeCallFamily::BytesSlice,
                        hew_types::RuntimeCallFamily::BytesSliceFrom,
                    )
                } else if matches!(
                    collection_type_arguments(&container_ty),
                    Some((hew_types::BuiltinType::Vec, _))
                ) {
                    (
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::Slice),
                        hew_types::RuntimeCallFamily::Vector(hew_types::VecValueOp::SliceFrom),
                    )
                } else {
                    return Err(format!(
                        "`{}` has no ownership-SIR range-slice operation",
                        container_ty.user_facing()
                    ));
                };
                let mut zero = (**container).clone();
                zero.ty = ResolvedTy::I64;
                zero.kind = HirExprKind::Literal(HirLiteral::Integer(0));
                let start: &HirExpr = start.as_deref().unwrap_or(&zero);
                match end {
                    Some(end) => self.lower_runtime_operation(
                        expr,
                        ranged,
                        &[container.as_ref(), start, end.as_ref()],
                        true,
                    )?,
                    None => self.lower_runtime_operation(
                        expr,
                        open,
                        &[container.as_ref(), start],
                        true,
                    )?,
                }
                .ok_or_else(|| "range slice must produce a SIR value".to_string())
            }
            // `re"..."` in value position. The module compiles every literal
            // once at process entry; this materializes an owned pattern from
            // the slot the match arms already read.
            HirExprKind::RegexLiteralRef { literal_id, .. } => {
                let pattern_ty = self.ty(&expr.ty);
                let shape = self.service.require_aggregate_shape(&pattern_ty)?;
                let AggregateShapeRef::Record(id) = shape else {
                    return Err("a regex literal must produce a named pattern record".to_string());
                };
                let [field] = self.service.aggregate_shapes[id.0 as usize]
                    .fields
                    .as_slice()
                else {
                    return Err("a regex pattern record holds exactly its handle".to_string());
                };
                let handle_ty = field.ty.clone();
                let mut slot = expr.clone();
                slot.ty = ResolvedTy::I64;
                slot.kind = HirExprKind::Literal(HirLiteral::Integer(i128::from(*literal_id)));
                let mut handle = expr.clone();
                handle.ty = handle_ty;
                let handle = self
                    .lower_runtime_operation(
                        &handle,
                        hew_types::RuntimeCallFamily::RegexHandle,
                        &[&slot],
                        true,
                    )?
                    .ok_or_else(|| "a regex literal must produce a SIR value".to_string())?;
                let pattern = self.emit(
                    expr,
                    SemOpKind::AggregateMake {
                        shape,
                        fields: vec![Operand { value: handle }],
                    },
                )?;
                self.owned_live.remove(&handle);
                Ok(pattern)
            }
            HirExprKind::Block(block) => self
                .lower_scoped_block(block, binding_use)?
                .map(|value| value.value)
                .ok_or_else(|| "a divergent block cannot produce a SIR value".to_string()),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr: Some(else_expr),
            } => self.lower_if(expr, condition, then_expr, else_expr),
            HirExprKind::Match { scrutinee, arms } => self.lower_match(expr, scrutinee, arms),
            HirExprKind::If {
                else_expr: None, ..
            } => Err(
                "one-armed if expressions are deferred until unit values are modeled".to_string(),
            ),
            _ => Err(format!(
                "unsupported HIR expression kind `{}` in the initial SIR subset",
                hir_expr_kind_name(&expr.kind)
            )),
        }
    }

    pub(super) fn lower_literal(
        &mut self,
        expr: &HirExpr,
        literal: &HirLiteral,
    ) -> Result<ValueId, String> {
        match literal {
            HirLiteral::Unit if self.ty(&expr.ty) == ResolvedTy::Unit => {
                self.emit(expr, SemOpKind::ConstUnit)
            }
            // An integer-spelled literal the checker resolved as a float is
            // the same constant at a float width (D421 admitted the value
            // against that type), so it materializes as the float constant
            // rather than an integer plus a cast.
            #[allow(
                clippy::cast_precision_loss,
                reason = "the checker admitted this literal against its resolved float type"
            )]
            HirLiteral::Integer(value) if self.ty(&expr.ty).is_float() => {
                self.emit(expr, SemOpKind::ConstFloat(*value as f64))
            }
            HirLiteral::Integer(value) => {
                if !self.ty(&expr.ty).is_integer() {
                    return Err(format!(
                        "integer literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstInteger(*value))
            }
            HirLiteral::Bool(value) => {
                if self.ty(&expr.ty) != ResolvedTy::Bool {
                    return Err(format!(
                        "boolean literal resolved as `{}` violates the SIR bool literal invariant",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstBool(*value))
            }
            HirLiteral::Float(value) => {
                if !self.ty(&expr.ty).is_float() {
                    return Err(format!(
                        "floating literal resolved as `{}` needs a dedicated SIR literal representation",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstFloat(*value))
            }
            HirLiteral::Char(value) => {
                if self.ty(&expr.ty) != ResolvedTy::Char {
                    return Err(format!(
                        "character literal resolved as `{}` violates the SIR char literal invariant",
                        self.ty(&expr.ty).user_facing()
                    ));
                }
                self.emit(expr, SemOpKind::ConstChar(*value))
            }
            HirLiteral::String(value) => {
                let literal = self.service.intern_string(value);
                self.emit(expr, SemOpKind::ConstStr(literal))
            }
            HirLiteral::Bytes(value) => {
                let literal = self.service.intern_bytes(value);
                self.emit(expr, SemOpKind::ConstBytes(literal))
            }
            HirLiteral::Duration(value) if self.ty(&expr.ty) == ResolvedTy::Duration => {
                self.emit(expr, SemOpKind::ConstDuration(*value))
            }
            _ => Err("unsupported HIR literal kind in the initial SIR subset".to_string()),
        }
    }

    pub(super) fn lower_variant_make(
        &mut self,
        expr: &HirExpr,
        variant: usize,
        payload: Option<&[(String, HirExpr)]>,
    ) -> Result<ValueId, String> {
        let enum_ty = self.ty(&expr.ty);
        let shape = self.service.require_variant_shape(&enum_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(usize::try_from(shape.0).map_err(|_| "variant shape id exceeds usize")?)
            .filter(|descriptor| descriptor.id == shape)
            .ok_or_else(|| format!("variant shape {} disappeared during lowering", shape.0))?;
        let declared = descriptor.variants.get(variant).cloned().ok_or_else(|| {
            format!(
                "variant constructor tag {variant} is absent from exact shape `{}`",
                enum_ty.user_facing()
            )
        })?;
        let supplied = payload.unwrap_or_default();
        if supplied.len() != declared.fields.len() {
            return Err(format!(
                "variant constructor {} for `{}` has {} field(s), expected {}",
                variant,
                enum_ty.user_facing(),
                supplied.len(),
                declared.fields.len()
            ));
        }
        let mut ordered = vec![None; declared.fields.len()];
        for (name, field) in supplied {
            let index = declared
                .fields
                .iter()
                .position(|candidate| candidate.name == *name)
                .ok_or_else(|| {
                    format!(
                        "variant constructor field `{name}` is absent from exact shape `{}` tag {variant}",
                        enum_ty.user_facing()
                    )
                })?;
            if ordered[index].is_some() {
                return Err(format!(
                    "variant constructor repeats field `{name}` for `{}` tag {variant}",
                    enum_ty.user_facing()
                ));
            }
            let value = lower_initial_value_transfer(
                self,
                field,
                &format!("variant field `{name}`"),
                OwnedBindingUse::Copy,
            )?;
            ordered[index] = Some(Operand {
                value: self.coerce_value(
                    value,
                    &declared.fields[index].ty,
                    Provenance::Site(field.site),
                )?,
            });
        }
        let fields = ordered
            .into_iter()
            .zip(&declared.fields)
            .map(|(operand, field)| {
                operand.ok_or_else(|| {
                    format!(
                        "variant constructor omits field `{}` from exact shape `{}` tag {variant}",
                        field.name,
                        enum_ty.user_facing()
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let value = self.emit(
            expr,
            SemOpKind::VariantMake {
                shape,
                variant: u32::try_from(variant)
                    .map_err(|_| "variant constructor tag exceeds u32".to_string())?,
                fields,
            },
        )?;
        for field in consumed {
            self.owned_live.remove(&field);
        }
        Ok(value)
    }

    pub(super) fn bind_selected_value(
        &mut self,
        binding: BindingId,
        name: &str,
        value: ValueId,
        span: Range<usize>,
    ) -> Result<(), String> {
        // Candidate names refer to the projected owner until every predicate
        // and guard has passed. A failed candidate must leave it available.
        let target = BindingTarget::Value(value);
        let declaration = self.source_bindings.len();
        self.source_bindings.push(Binding {
            id: crate::BindingId(
                u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?,
            ),
            name: name.to_string(),
            span,
            mutable: false,
            target,
        });
        self.binding_declarations.insert(binding, declaration);
        self.bindings.insert(binding, target);
        self.declare_in_scope(binding);
        Ok(())
    }

    /// Rebuild one variant value from the payloads its switch handed out.
    pub(super) fn emit_variant_make(
        &mut self,
        shape: VariantShapeId,
        variant: u32,
        ty: &ResolvedTy,
        fields: &[BlockArg],
    ) -> Result<ValueId, String> {
        let operands = fields
            .iter()
            .map(|field| Operand { value: field.value })
            .collect::<Vec<_>>();
        let value = self.emit_typed(
            Provenance::Synthesized,
            ty,
            SemOpKind::VariantMake {
                shape,
                variant,
                fields: operands,
            },
        )?;
        for field in fields {
            self.owned_live.remove(&field.value);
        }
        Ok(value)
    }

    /// Take a rebuilt variant value apart again, returning its fresh payloads.
    pub(super) fn emit_variant_destructure(
        &mut self,
        shape: VariantShapeId,
        variant: u32,
        descriptor: &SemVariantShape,
        source: ValueId,
    ) -> Result<Vec<BlockArg>, String> {
        let declared = &descriptor
            .variants
            .get(usize::try_from(variant).map_err(|_| "variant tag exceeds usize".to_string())?)
            .ok_or_else(|| format!("variant tag {variant} is absent from its shape"))?
            .fields;
        let mut results = Vec::with_capacity(declared.len());
        for field in declared {
            results.push(ValueDef {
                id: self.fresh_value(),
                ty: field.ty.clone(),
                own: OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())?,
            });
        }
        let id = OpId(self.ops);
        self.current_block_mut().append_op(SemOp {
            id,
            results: results.clone(),
            kind: SemOpKind::VariantDestructure {
                shape,
                variant,
                source: Operand { value: source },
            },
            provenance: Provenance::Synthesized,
        })?;
        self.ops += 1;
        self.owned_live.remove(&source);
        Ok(results
            .into_iter()
            .map(|field| {
                if field.own == OwnKind::Owned {
                    self.owned_live.insert(field.id, field.ty.clone());
                }
                BlockArg {
                    value: field.id,
                    ty: field.ty,
                    own: field.own,
                }
            })
            .collect())
    }

    pub(super) fn destroy_live_since(
        &mut self,
        baseline: &BTreeMap<ValueId, ResolvedTy>,
    ) -> Result<(), String> {
        let values = self
            .owned_live
            .keys()
            .filter(|value| !baseline.contains_key(value))
            .copied()
            .collect::<Vec<_>>();
        for value in values.into_iter().rev() {
            self.emit_destroy(value)?;
        }
        Ok(())
    }

    pub(super) fn emit_variant_switch(
        &mut self,
        shape: VariantShapeId,
        descriptor: &SemVariantShape,
        scrutinee: ValueId,
    ) -> Result<Vec<VariantBranch>, String> {
        let mut semantic_arms = Vec::with_capacity(descriptor.variants.len());
        let mut branches = Vec::with_capacity(descriptor.variants.len());
        let mut inherited_live = self.owned_live.clone();
        inherited_live.remove(&scrutinee);
        // A loaned scrutinee is not destructured: its payloads are loans of the
        // same region, so they carry no release obligation and the switch
        // transfers nothing.
        let borrowed = self.value_own_kind(scrutinee) == Some(OwnKind::Guaranteed);
        for (variant_index, variant) in descriptor.variants.iter().enumerate() {
            let mut fields = Vec::with_capacity(variant.fields.len());
            let mut block_args = Vec::with_capacity(variant.fields.len());
            let mut edge_args = Vec::with_capacity(variant.fields.len());
            let mut branch_live = inherited_live.clone();
            for field in &variant.fields {
                self.service.require_type_facts(&field.ty)?;
                let own = if borrowed {
                    OwnKind::of_loan_result(&field.ty, self.service.checked_facts.rows())?
                } else {
                    OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())?
                };
                let field_value = self.fresh_value();
                fields.push(ValueDef {
                    id: field_value,
                    ty: field.ty.clone(),
                    own,
                });
                edge_args.push(Operand { value: field_value });
                let arg = BlockArg {
                    value: self.fresh_value(),
                    ty: field.ty.clone(),
                    own,
                };
                if own == OwnKind::Owned {
                    branch_live.insert(arg.value, arg.ty.clone());
                }
                block_args.push(arg);
            }
            let block = self.new_block(block_args.clone());
            let variant = u32::try_from(variant_index)
                .map_err(|_| "variant arm index exceeds u32".to_string())?;
            semantic_arms.push(SemVariantArm {
                variant,
                fields,
                target: Edge {
                    target: block,
                    args: edge_args,
                },
            });
            branches.push(VariantBranch {
                variant,
                block,
                fields: block_args,
                owned_live: branch_live,
            });
        }
        let id = OpId(self.ops);
        self.ops += 1;
        self.owned_live.remove(&scrutinee);
        self.set_terminator(SemTerminator::SwitchVariant {
            id,
            shape,
            scrutinee: Operand { value: scrutinee },
            arms: semantic_arms,
        })?;
        Ok(branches)
    }

    pub(super) fn lower_string_equals_values(
        &mut self,
        lhs: ValueId,
        rhs: ValueId,
    ) -> Result<ValueId, String> {
        let raw = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }]);
        let id = OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family: hew_types::RuntimeCallFamily::StringEquals,
            args: vec![
                crate::BoundaryOperand {
                    operand: Operand { value: lhs },
                    decision: crate::BoundaryDecision::Borrow,
                },
                crate::BoundaryOperand {
                    operand: Operand { value: rhs },
                    decision: crate::BoundaryDecision::Borrow,
                },
            ],
            result: CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            }),
            normal: Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: CallUnwind::NotApplicable,
        })?;
        self.current = normal;
        Ok(continuation)
    }

    pub(super) fn branch_candidate_test(
        &mut self,
        condition: ValueId,
    ) -> Result<ControlState, String> {
        let pass = self.new_block(Vec::new());
        let fail = self.new_block(Vec::new());
        self.set_terminator(SemTerminator::Branch {
            condition: Operand { value: condition },
            then_target: Edge {
                target: pass,
                args: Vec::new(),
            },
            else_target: Edge {
                target: fail,
                args: Vec::new(),
            },
        })?;
        let mut failure = self.control_state();
        failure.block = fail;
        self.current = pass;
        Ok(failure)
    }

    /// Give a selected binding a new declaration for its transferred owner.
    /// Failed-candidate control states still reference the original
    /// declaration, so it is never changed under those saved states.
    pub(super) fn redeclare_binding(
        &mut self,
        binding: BindingId,
        target: BindingTarget,
    ) -> Result<(), String> {
        let mut selected = self.source_bindings[self.binding_declarations[&binding]].clone();
        let declaration = self.source_bindings.len();
        selected.id =
            crate::BindingId(u32::try_from(declaration).map_err(|_| "binding count exceeds u32")?);
        selected.target = target;
        self.source_bindings.push(selected);
        self.binding_declarations.insert(binding, declaration);
        self.bindings.insert(binding, target);
        Ok(())
    }

    pub(super) fn lower_selected_body(
        &mut self,
        body: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Result<Option<Operand>, String> {
        if *result_ty == ResolvedTy::Unit {
            self.lower_discarded_expr(body)?;
            return Ok(None);
        }
        if matches!(self.ty(&body.ty), ResolvedTy::Unit | ResolvedTy::Never) {
            self.lower_discarded_expr(body)?;
            if self.is_open() {
                return Err(
                    "non-divergent variant arm does not produce the match result".to_string(),
                );
            }
            return Ok(None);
        }
        let value = if let HirExprKind::Block(block) = &body.kind {
            // A checked value tail may be unreachable after a return or fault.
            // Preserve the block's terminated control flow without inventing
            // an operand for a branch that never reaches the join.
            let result = self.lower_scoped_block(block, OwnedBindingUse::Copy)?;
            if !self.is_open() {
                return Ok(None);
            }
            result
                .ok_or("non-divergent selected block does not produce its result")?
                .value
        } else {
            lower_initial_value_transfer(self, body, "selected arm result", OwnedBindingUse::Copy)?
        };
        let value = self.coerce_value(value, result_ty, Provenance::Site(body.site))?;
        Ok(Some(Operand { value }))
    }

    pub(super) fn merge_match_exits(
        &mut self,
        exits: Vec<MatchExit>,
        result_ty: &ResolvedTy,
    ) -> Result<Option<ValueId>, String> {
        if exits.is_empty() {
            return Ok(None);
        }
        let mut result_arg = None;
        let mut block_args = Vec::new();
        let mut edge_prefixes = vec![Vec::new(); exits.len()];
        if *result_ty != ResolvedTy::Unit {
            self.service.require_type_facts(result_ty)?;
            let own = OwnKind::of_ty(result_ty, self.service.checked_facts.rows())?;
            let joined = self.fresh_value();
            block_args.push(BlockArg {
                value: joined,
                ty: result_ty.clone(),
                own,
            });
            for (index, exit) in exits.iter().enumerate() {
                let result = exit
                    .result
                    .as_ref()
                    .ok_or_else(|| "non-divergent match arm has no result operand".to_string())?;
                edge_prefixes[index].push(result.clone());
            }
            result_arg = Some((joined, own));
        } else if exits.iter().any(|exit| exit.result.is_some()) {
            return Err("unit match arm unexpectedly carries a result".to_string());
        }

        let states = exits.into_iter().map(|exit| exit.state).collect::<Vec<_>>();
        self.join_control_states(states, block_args, edge_prefixes)?;
        if let Some((result, OwnKind::Owned)) = result_arg {
            self.owned_live.insert(result, result_ty.clone());
        }
        Ok(result_arg.map(|(result, _)| result))
    }
}
