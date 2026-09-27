//! Array, tuple and aggregate construction/projection lowering.

use super::{
    is_initial_value_type, lower_initial_value_transfer, require_initial_value_transfer,
    AggregateFieldRecipe, AggregateShapeRef, BindingTarget, Builder, HirDestructureField,
    HirDestructureSelector, HirExpr, HirExprKind, IntentKind, OpId, Operand, OwnKind,
    OwnedBindingUse, PlaceId, Provenance, ResolvedRef, ResolvedTy, SemOp, SemOpKind,
    TypeInstanceKey, ValueDef, ValueId,
};

impl Builder<'_, '_> {
    pub(super) fn lower_array_make(
        &mut self,
        expr: &HirExpr,
        elements: &[HirExpr],
    ) -> Result<ValueId, String> {
        let ResolvedTy::Array(element_ty, length) = self.ty(&expr.ty) else {
            return Err("fixed array literal has no exact array type".into());
        };
        if usize::try_from(length).ok() != Some(elements.len()) {
            return Err("fixed array literal length differs from its checked type".into());
        }
        let mut fields = Vec::with_capacity(elements.len());
        for element in elements {
            if self.ty(&element.ty) != *element_ty {
                return Err("fixed array literal element differs from its checked type".into());
            }
            self.service.require_type_facts(&element_ty)?;
            let binding_use =
                if self.service.checked_facts.rows()[&TypeInstanceKey((*element_ty).clone())].clone
                    == hew_types::CloneKind::None
                {
                    OwnedBindingUse::Move
                } else {
                    OwnedBindingUse::Copy
                };
            let value =
                lower_initial_value_transfer(self, element, "fixed array element", binding_use)?;
            fields.push(Operand { value });
        }
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let result = self.emit(expr, SemOpKind::ArrayMake { fields })?;
        for value in consumed {
            self.owned_live.remove(&value);
        }
        Ok(result)
    }

    pub(super) fn lower_array_repeat(
        &mut self,
        expr: &HirExpr,
        seed: &HirExpr,
    ) -> Result<ValueId, String> {
        let ResolvedTy::Array(element_ty, length) = self.ty(&expr.ty) else {
            return Err("fixed array repeat has no exact array type".into());
        };
        if self.ty(&seed.ty) != *element_ty {
            return Err("fixed array repeat seed differs from its checked element type".into());
        }
        self.service.require_type_facts(&element_ty)?;
        let copy = self.service.checked_facts.rows()[&TypeInstanceKey((*element_ty).clone())].clone;
        if length > 1 && copy == hew_types::CloneKind::None {
            return Err("fixed array repeat requires Clone when its length exceeds one".into());
        }
        let value = lower_initial_value_transfer(
            self,
            seed,
            "fixed array repeat seed",
            if copy == hew_types::CloneKind::None {
                OwnedBindingUse::Move
            } else {
                OwnedBindingUse::Copy
            },
        )?;
        if length == 0 {
            if self.owned_live.contains_key(&value) {
                self.emit_destroy(value)?;
            }
            return self.emit(expr, SemOpKind::ArrayMake { fields: Vec::new() });
        }
        let result = self.emit(
            expr,
            SemOpKind::ArrayRepeat {
                value: Operand { value },
            },
        )?;
        self.owned_live.remove(&value);
        Ok(result)
    }

    /// Construct a tuple from exact semantic fields, including receiver transfers.
    pub(super) fn lower_tuple_make(
        &mut self,
        expr: &HirExpr,
        elements: &[HirExpr],
    ) -> Result<ValueId, String> {
        let tuple_ty = self.ty(&expr.ty);
        if tuple_ty == ResolvedTy::Unit && elements.is_empty() {
            return self.emit(expr, SemOpKind::ConstUnit);
        }
        let ResolvedTy::Tuple(expected_elements) = &tuple_ty else {
            return Err(format!(
                "tuple literal has non-tuple resolved type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        if expected_elements.len() != elements.len() {
            return Err(format!(
                "tuple literal has {} element(s), but its resolved type `{}` has {} element type(s)",
                elements.len(),
                tuple_ty.user_facing(),
                expected_elements.len()
            ));
        }
        // The marked receiver occupies the writeback field of the established
        // dual return. The method result may itself mention Self; it keeps the
        // ordinary source copy policy and must not take the writeback receiver.
        let receiver_return = tuple_ty == self.ty(&self.function.return_ty)
            && self.function.var_self_receiver.is_some_and(|binding| {
                elements.get(1).is_some_and(|element| {
                    element.intent == IntentKind::Consume
                        && matches!(element.kind, HirExprKind::BindingRef { resolved: ResolvedRef::Binding(id), .. }
                            if id == binding)
                })
            });
        let mut lowered_elements = Vec::with_capacity(elements.len());
        for (index, (element, expected_ty)) in elements.iter().zip(expected_elements).enumerate() {
            let actual_ty = self.ty(&element.ty);
            if &actual_ty != expected_ty {
                return Err(format!(
                    "tuple literal element {index} has resolved type `{}`, expected `{}`",
                    actual_ty.user_facing(),
                    expected_ty.user_facing()
                ));
            }
            let value = if is_initial_value_type(&tuple_ty) && !receiver_return {
                self.lower_read_operand(element, &format!("tuple literal element {index}"))?
            } else {
                Operand {
                    value: lower_initial_value_transfer(
                        self,
                        element,
                        &format!("owned tuple field {index}"),
                        if receiver_return && index == 1 {
                            OwnedBindingUse::Move
                        } else {
                            OwnedBindingUse::Copy
                        },
                    )?,
                }
            };
            lowered_elements.push(value);
        }
        if is_initial_value_type(&tuple_ty) {
            self.emit(
                expr,
                SemOpKind::TupleMake {
                    elements: lowered_elements,
                },
            )
        } else {
            let shape = self.service.require_aggregate_shape(&tuple_ty)?;
            let consumed = lowered_elements
                .iter()
                .map(|field| field.value)
                .collect::<Vec<_>>();
            let aggregate = self.emit(
                expr,
                SemOpKind::AggregateMake {
                    shape,
                    fields: lowered_elements,
                },
            )?;
            for field in consumed {
                self.owned_live.remove(&field);
            }
            Ok(aggregate)
        }
    }

    pub(super) fn tuple_projection_index(
        &mut self,
        expr: &HirExpr,
        tuple_expr: &HirExpr,
        index: usize,
    ) -> Result<u32, String> {
        let tuple_ty = self.ty(&tuple_expr.ty);
        let ResolvedTy::Tuple(elements) = &tuple_ty else {
            return Err(format!(
                "tuple projection has non-tuple operand type `{}` in SIR lowering",
                tuple_ty.user_facing()
            ));
        };
        let expected_ty = elements.get(index).ok_or_else(|| {
            format!(
                "tuple projection index {index} is out of bounds for `{}` with {} element(s)",
                tuple_ty.user_facing(),
                elements.len()
            )
        })?;
        let result_ty = self.ty(&expr.ty);
        if &result_ty != expected_ty {
            return Err(format!(
                "tuple projection index {index} from `{}` has result type `{}`, expected `{}`",
                tuple_ty.user_facing(),
                result_ty.user_facing(),
                expected_ty.user_facing()
            ));
        }
        let index = u32::try_from(index).map_err(|_| {
            "tuple projection index exceeds SIR's target-independent u32 field range".to_string()
        })?;
        Ok(index)
    }

    /// Lower a semantic tuple projection without exposing aggregate layout.
    pub(super) fn lower_tuple_get(
        &mut self,
        expr: &HirExpr,
        tuple_expr: &HirExpr,
        index: usize,
    ) -> Result<ValueId, String> {
        if let Some(place) = self.expression_projection(expr)? {
            return self.emit(expr, SemOpKind::LoadCopy { place });
        }
        let index = self.tuple_projection_index(expr, tuple_expr, index)?;
        let tuple_ty = self.ty(&tuple_expr.ty);
        let tuple = self.lower_read_operand(tuple_expr, "tuple projection operand")?;
        if is_initial_value_type(&tuple_ty) {
            self.emit(expr, SemOpKind::TupleGet { tuple, index })
        } else {
            let shape = self.service.require_aggregate_shape(&tuple_ty)?;
            self.emit(
                expr,
                SemOpKind::AggregateProjectCopy {
                    shape,
                    aggregate: tuple,
                    field: index,
                },
            )
        }
    }

    pub(super) fn lower_initial_tuple_destructure(
        &mut self,
        value: &HirExpr,
        fields: &[HirDestructureField],
    ) -> Result<(), String> {
        let aggregate_ty = self.ty(&value.ty);
        let ResolvedTy::Tuple(field_tys) = &aggregate_ty else {
            return Err(format!(
                "irrefutable destructure has non-aggregate type `{}`",
                aggregate_ty.user_facing()
            ));
        };
        if fields.len() != field_tys.len() {
            return Err(format!(
                "tuple destructure for `{}` binds {} field(s), expected {}",
                aggregate_ty.user_facing(),
                fields.len(),
                field_tys.len()
            ));
        }
        require_initial_value_transfer(value.intent, &aggregate_ty, "tuple destructure source")?;
        let tuple = self.lower_expr(value)?;
        for (index, (field, expected_ty)) in fields.iter().zip(field_tys).enumerate() {
            let index = u32::try_from(index)
                .map_err(|_| "tuple destructure index exceeds u32".to_string())?;
            let expected_selector = HirDestructureSelector::Tuple(index);
            let Some(binding) = &field.binding else {
                if field.selector != expected_selector {
                    return Err(format!(
                        "tuple destructure field {index} has selector {:?}, expected {expected_selector:?}",
                        field.selector,
                    ));
                }
                continue;
            };
            let binding_ty = self.ty(&binding.ty);
            if field.selector != expected_selector || binding_ty != *expected_ty {
                return Err(format!(
                    "tuple destructure field {index} has selector {:?} and type `{}`, expected {:?} and `{}`",
                    field.selector,
                    binding_ty.user_facing(),
                    expected_selector,
                    expected_ty.user_facing()
                ));
            }
            let result = self.emit_typed(
                Provenance::Site(value.site),
                expected_ty,
                SemOpKind::TupleGet {
                    tuple: Operand { value: tuple },
                    index,
                },
            )?;
            self.bind_source_value(binding, result)?;
        }
        Ok(())
    }

    /// Lower one checker-normalized irrefutable aggregate pattern.
    ///
    /// A destructure names fields, not the whole value. When the source is an
    /// aggregate this body owns in place, each named field is read from its
    /// own place, so a field the pattern leaves alone stays initialized and
    /// readable afterwards - the same partition `let t = booking.ticket`
    /// already produces. A source with no place of its own (a temporary, a
    /// capture, an actor state seat) is transferred whole and taken apart,
    /// with every ordered field becoming a distinct SSA result so cleanup
    /// stays explicit on every path.
    pub(super) fn lower_destructure(
        &mut self,
        value: &HirExpr,
        fields: &[HirDestructureField],
    ) -> Result<(), String> {
        let aggregate_ty = self.ty(&value.ty);
        if is_initial_value_type(&aggregate_ty) {
            return self.lower_initial_tuple_destructure(value, fields);
        }

        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let recipes = crate::aggregate_field_recipes(
            shape,
            &aggregate_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let expected_selectors = match shape {
            AggregateShapeRef::Tuple => (0..recipes.len())
                .map(|index| {
                    u32::try_from(index)
                        .map(HirDestructureSelector::Tuple)
                        .map_err(|_| "tuple destructure index exceeds u32".to_string())
                })
                .collect::<Result<Vec<_>, _>>()?,
            AggregateShapeRef::Record(id) => self
                .service
                .aggregate_shapes
                .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
                .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?
                .fields
                .iter()
                .map(|field| HirDestructureSelector::Record(field.name.clone()))
                .collect(),
        };
        if fields.len() != recipes.len() {
            return Err(format!(
                "aggregate destructure for `{}` binds {} field(s), expected {}",
                aggregate_ty.user_facing(),
                fields.len(),
                recipes.len()
            ));
        }
        for (index, ((field, recipe), expected_selector)) in fields
            .iter()
            .zip(&recipes)
            .zip(&expected_selectors)
            .enumerate()
        {
            if &field.selector != expected_selector {
                return Err(format!(
                    "aggregate destructure field {index} has selector {:?}, expected {expected_selector:?}",
                    field.selector,
                ));
            }
            let Some(binding) = &field.binding else {
                continue;
            };
            let binding_ty = self.ty(&binding.ty);
            if binding_ty != recipe.ty {
                return Err(format!(
                    "aggregate destructure field {index} has type `{}`, expected `{}`",
                    binding_ty.user_facing(),
                    recipe.ty.user_facing()
                ));
            }
        }

        if self.destructure_in_place(value, shape, &recipes, fields)? {
            return Ok(());
        }

        let aggregate = lower_initial_value_transfer(
            self,
            value,
            "aggregate destructure source",
            OwnedBindingUse::Copy,
        )?;
        if self.value_own_kind(aggregate).is_none() {
            return Err(format!(
                "aggregate destructure source `{}` has no exact ownership facts",
                aggregate_ty.user_facing()
            ));
        }
        let results = self.emit_destructure_value(
            aggregate,
            &aggregate_ty,
            shape,
            Provenance::Site(value.site),
        )?;
        for (field, result) in fields.iter().zip(results) {
            if let Some(binding) = &field.binding {
                self.bind_source_value(binding, result.id)?;
            }
        }
        Ok(())
    }

    /// Read each named field of a destructure straight out of the source's own
    /// place, leaving the fields the pattern does not name initialized.
    ///
    /// Returns `false` when the source has no field places of its own, which
    /// is when the whole-value transfer above is the correct lowering.
    pub(super) fn destructure_in_place(
        &mut self,
        value: &HirExpr,
        shape: AggregateShapeRef,
        recipes: &[AggregateFieldRecipe],
        fields: &[HirDestructureField],
    ) -> Result<bool, String> {
        let Some(places) = self.aggregate_field_places(value, shape, recipes)? else {
            return Ok(false);
        };
        let provenance = Provenance::Site(value.site);
        for ((field, recipe), place) in fields.iter().zip(recipes).zip(places) {
            let Some(binding) = &field.binding else {
                continue;
            };
            if field.nested {
                self.bind_source_target(binding, BindingTarget::Place(place))?;
                continue;
            }
            let kind = if recipe.clone == hew_types::CloneKind::None {
                SemOpKind::LoadTake { place }
            } else {
                SemOpKind::LoadCopy { place }
            };
            let result = self.emit_typed(provenance.clone(), &recipe.ty, kind)?;
            self.bind_source_value(binding, result)?;
        }
        Ok(true)
    }

    /// The ordered field places of an aggregate expression this body owns in
    /// place, or `None` when the expression names no such partition.
    pub(super) fn aggregate_field_places(
        &mut self,
        source: &HirExpr,
        shape: AggregateShapeRef,
        recipes: &[AggregateFieldRecipe],
    ) -> Result<Option<Vec<PlaceId>>, String> {
        let Some(root) = self.resolve_binding_place(source)? else {
            return Ok(None);
        };
        let aggregate_ty = self.ty(&source.ty);
        let mut places = Vec::with_capacity(recipes.len());
        for index in 0..recipes.len() {
            let mut leaf = root.clone();
            leaf.projections.push((aggregate_ty.clone(), shape, index));
            let Some(place) = self.owned_projection(&leaf)? else {
                return Ok(None);
            };
            places.push(place);
        }
        Ok(Some(places))
    }

    pub(super) fn emit_destructure_value(
        &mut self,
        aggregate: ValueId,
        ty: &ResolvedTy,
        shape: AggregateShapeRef,
        provenance: Provenance,
    ) -> Result<Vec<ValueDef>, String> {
        let recipes = crate::aggregate_field_recipes(
            shape,
            ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let mut results = Vec::with_capacity(recipes.len());
        for recipe in &recipes {
            self.service.require_type_facts(&recipe.ty)?;
            results.push(ValueDef {
                id: self.fresh_value(),
                ty: recipe.ty.clone(),
                own: recipe.own,
            });
        }
        let operation = SemOp {
            id: OpId(self.ops),
            results: results.clone(),
            kind: SemOpKind::Destructure {
                shape,
                aggregate: Operand { value: aggregate },
            },
            provenance,
        };
        self.current_block_mut().append_op(operation)?;
        self.ops += 1;
        self.owned_live.remove(&aggregate);
        for result in &results {
            if result.own == OwnKind::Owned {
                self.owned_live.insert(result.id, result.ty.clone());
            }
        }
        Ok(results)
    }

    /// Lower one named aggregate construction in source evaluation order,
    /// then present its operands in the declaration's exact field order.
    ///
    /// A functional update `R { x: v, ..base }` evaluates its named fields
    /// first and fills the rest from the base afterwards. A base that is
    /// consumed - a temporary, or a value with a non-copyable carried field -
    /// is destructured so the carried fields transfer and the overridden ones
    /// are destroyed here. Any other base is only read: every carried field
    /// is an independent copy and the base stays live for its owner.
    pub(super) fn lower_aggregate_make(
        &mut self,
        expr: &HirExpr,
        fields: &[(String, HirExpr)],
        base: Option<&HirExpr>,
    ) -> Result<ValueId, String> {
        let aggregate_ty = self.ty(&expr.ty);
        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let AggregateShapeRef::Record(id) = shape else {
            return Err("struct initializer resolved to a non-record aggregate shape".to_string());
        };
        let declared_fields = self
            .service
            .aggregate_shapes
            .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
            .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?
            .fields
            .clone();
        let mut ordered = vec![None; declared_fields.len()];
        for (name, field) in fields {
            let index = declared_fields
                .iter()
                .position(|declared| declared.name == *name)
                .ok_or_else(|| {
                    format!(
                        "record initializer field `{name}` is absent from exact shape `{}`",
                        aggregate_ty.user_facing()
                    )
                })?;
            if ordered[index].is_some() {
                return Err(format!("record initializer repeats field `{name}`"));
            }
            let value = lower_initial_value_transfer(
                self,
                field,
                &format!("owned record field `{name}`"),
                OwnedBindingUse::Copy,
            )?;
            ordered[index] = Some(Operand {
                value: self.coerce_value(
                    value,
                    &declared_fields[index].ty,
                    Provenance::Site(field.site),
                )?,
            });
        }
        if let Some(base) = base {
            self.lower_aggregate_update_base(base, &aggregate_ty, shape, &mut ordered)?;
        }
        let fields = ordered
            .into_iter()
            .zip(&declared_fields)
            .map(|(operand, declared)| {
                operand.ok_or_else(|| {
                    format!(
                        "record initializer omits field `{}` from exact shape `{}`",
                        declared.name,
                        aggregate_ty.user_facing()
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let consumed = fields.iter().map(|field| field.value).collect::<Vec<_>>();
        let aggregate = self.emit(expr, SemOpKind::AggregateMake { shape, fields })?;
        for field in consumed {
            self.owned_live.remove(&field);
        }
        Ok(aggregate)
    }

    /// Fill every field the update leaves unnamed from its base.
    pub(super) fn lower_aggregate_update_base(
        &mut self,
        base: &HirExpr,
        aggregate_ty: &ResolvedTy,
        shape: AggregateShapeRef,
        ordered: &mut [Option<Operand>],
    ) -> Result<(), String> {
        if self.ty(&base.ty) != *aggregate_ty {
            return Err(format!(
                "functional update base `{}` differs from the constructed `{}`",
                self.ty(&base.ty).user_facing(),
                aggregate_ty.user_facing()
            ));
        }
        let recipes = crate::aggregate_field_recipes(
            shape,
            aggregate_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        let carried = ordered.iter().map(Option::is_none).collect::<Vec<_>>();
        let mut transfer_only = false;
        for (index, recipe) in recipes.iter().enumerate() {
            if !carried[index] {
                continue;
            }
            self.service.require_type_facts(&recipe.ty)?;
            if recipe.own == OwnKind::Owned
                && self.service.checked_facts.rows()[&TypeInstanceKey(recipe.ty.clone())].clone
                    == hew_types::CloneKind::None
            {
                transfer_only = true;
            }
        }
        let provenance = Provenance::Site(base.site);
        let live_before: std::collections::HashSet<_> = self.owned_live.keys().copied().collect();
        let mut loans = Vec::new();
        let source = if transfer_only {
            let value = lower_initial_value_transfer(
                self,
                base,
                "functional update base",
                OwnedBindingUse::Move,
            )?;
            if self.value_own_kind(value) != Some(OwnKind::Owned) {
                return Err(
                    "E_OWN_CONSUME_BORROWED: functional update of a borrowed base cannot transfer its non-copyable fields"
                        .into(),
                );
            }
            value
        } else {
            self.lower_borrowed_read(base, &mut loans)?.value
        };
        let consumed = self.owned_live.contains_key(&source) && !live_before.contains(&source);
        if consumed {
            let results = self.emit_destructure_value(source, aggregate_ty, shape, provenance)?;
            for (index, result) in results.into_iter().enumerate() {
                if carried[index] {
                    ordered[index] = Some(Operand { value: result.id });
                } else if result.own == OwnKind::Owned {
                    self.emit_destroy(result.id)?;
                }
            }
        } else {
            for (index, recipe) in recipes.iter().enumerate() {
                if !carried[index] {
                    continue;
                }
                let field = u32::try_from(index).map_err(|_| "aggregate field exceeds u32")?;
                let value = self.emit_typed(
                    provenance.clone(),
                    &recipe.ty,
                    SemOpKind::AggregateProjectCopy {
                        shape,
                        aggregate: Operand { value: source },
                        field,
                    },
                )?;
                ordered[index] = Some(Operand { value });
            }
        }
        self.end_call_loans(&loans)
    }

    /// Resolve a named projection once for both owned and borrowed reads.
    pub(super) fn aggregate_projection_shape(
        &mut self,
        expr: &HirExpr,
        object: &HirExpr,
        field: &str,
    ) -> Result<(AggregateShapeRef, u32), String> {
        let aggregate_ty = self.ty(&object.ty);
        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let AggregateShapeRef::Record(id) = shape else {
            return Err("named field access resolved to a non-record aggregate shape".to_string());
        };
        let descriptor = self
            .service
            .aggregate_shapes
            .get(usize::try_from(id.0).map_err(|_| "aggregate shape id exceeds usize")?)
            .ok_or_else(|| format!("aggregate shape {} disappeared during lowering", id.0))?;
        let index = descriptor
            .fields
            .iter()
            .position(|candidate| candidate.name == field)
            .ok_or_else(|| {
                format!(
                    "field `{field}` is absent from exact aggregate shape `{}`",
                    aggregate_ty.user_facing()
                )
            })?;
        let expected_ty = descriptor.fields[index].ty.clone();
        let result_ty = self.ty(&expr.ty);
        if !crate::call_boundary_types_match(&result_ty, &expected_ty) {
            return Err(format!(
                "field `{field}` from `{}` has `{}`, expected `{}`",
                aggregate_ty.user_facing(),
                result_ty.user_facing(),
                expected_ty.user_facing()
            ));
        }
        Ok((
            shape,
            u32::try_from(index).map_err(|_| "aggregate field index exceeds u32")?,
        ))
    }

    /// Lower an ordinary named-field read as an explicit independent copy.
    pub(super) fn lower_aggregate_project(
        &mut self,
        expr: &HirExpr,
        object: &HirExpr,
        field: &str,
    ) -> Result<ValueId, String> {
        if let Some(place) = self.expression_projection(expr)? {
            return self.emit(expr, SemOpKind::LoadCopy { place });
        }
        let (shape, field) = self.aggregate_projection_shape(expr, object, field)?;
        let mut loans = Vec::new();
        let aggregate = self.lower_borrowed_read(object, &mut loans)?;
        let value = self.emit(
            expr,
            SemOpKind::AggregateProjectCopy {
                shape,
                aggregate,
                field,
            },
        )?;
        self.end_call_loans(&loans)?;
        Ok(value)
    }
}
