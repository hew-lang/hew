//! One driver for every source `match`.
//!
//! Three roles vary with the scrutinee's shape, and nothing else does:
//!
//! * **probe** - how a candidate's predicate is tested: a scalar compare, a
//!   compiled regex, a variant tag, or an aggregate field projection. Every
//!   test reads the scrutinee without consuming it, so a candidate that fails
//!   leaves it whole for the next one.
//! * **bind** - how the arm's names are introduced from what the probe read.
//!   A candidate name only refers to the probed value: it owns nothing until
//!   no test can still fail.
//! * **select** - what the winning arm takes out of the scrutinee: an
//!   aggregate is destructured exactly once, nested payloads are transferred,
//!   and the candidate names acquire their owners.
//!
//! Candidate order, guard cleanup, control-state restore and merge, the
//! fallthrough and the match exits are the same code for every shape.

use std::collections::{BTreeMap, HashSet};

use super::{
    is_concrete_variant_type, is_initial_value_type, is_unconstructable_variant,
    lower_initial_value_transfer, Builder, ControlState, MatchExit, OwnedBindingUse,
};
use crate::ownership::{AggregateFieldRecipe, OwnKind};
use crate::{
    AggregateShapeRef, BindingTarget, BlockArg, Operand, Provenance, SemOp, SemOpKind,
    SemTerminator, SemVariantShape, ValueDef, ValueId, VariantShapeId,
};
use hew_hir::{
    BindingId, HirExpr, HirLiteral, HirMatchArm, HirMatchArmBinding, HirMatchArmPredicate,
    HirPayloadPredicate, HirPayloadVariantPredicate,
};
use hew_types::ResolvedTy;

/// The scrutinee's shape, resolved from its checked type. The shape is a fact
/// of the type, not of the arms: an arm that does not fit it is malformed HIR
/// and is refused here, before anything is emitted.
enum MatchShape {
    /// An integer, boolean, character or string scrutinee. A candidate
    /// compares the whole value or tests it against a compiled regex.
    Scalar,
    /// A record or tuple scrutinee. A candidate projects its fields in place
    /// and the selected arm destructures it exactly once.
    Aggregate {
        shape: AggregateShapeRef,
        recipes: Vec<AggregateFieldRecipe>,
        /// A tuple of scalars is copied whole: there is nothing to release and
        /// nothing to take apart.
        initial_value: bool,
    },
    /// An enum scrutinee. One switch on the tag gives each branch its payloads
    /// and the arms that can still match there, in source order.
    Variant {
        shape: VariantShapeId,
        descriptor: SemVariantShape,
        /// Arm indices that can still match each variant, in source order.
        candidates: Vec<Vec<usize>>,
    },
}

impl MatchShape {
    /// How the match reads its scrutinee.
    ///
    /// A shape whose selected arm takes the scrutinee apart needs an owner, so
    /// a loan is refused there as it is anywhere else a value is consumed. The
    /// rest only read what they probe, so a loan may be matched on directly.
    fn read(&self) -> OwnedBindingUse {
        match self {
            Self::Aggregate {
                initial_value: false,
                ..
            } => OwnedBindingUse::Copy,
            _ => OwnedBindingUse::Probe,
        }
    }
}

/// What the candidate loop needs that does not change between arms.
struct MatchPlan {
    shape: MatchShape,
    scrutinee: ValueId,
    scrutinee_ty: ResolvedTy,
    /// The scrutinee is a loan of a value the match did not take: its payloads
    /// name that region rather than owning it.
    borrowed: bool,
    provenance: Provenance,
    result_ty: ResolvedTy,
    outer_bindings: HashSet<BindingId>,
    outer_loans: usize,
    outer_live: BTreeMap<ValueId, ResolvedTy>,
}

/// One ordered run of candidates sharing an entry state: the whole match for a
/// scalar or aggregate scrutinee, one switch branch for a variant one.
struct CandidateGroup {
    entry: ControlState,
    /// A switch branch's payloads arrive as block arguments, so its live set is
    /// not the state the match entered with.
    owned_live: BTreeMap<ValueId, ResolvedTy>,
    arms: Vec<usize>,
    /// The payloads a switch branch handed out; empty for the other shapes,
    /// which project their fields per candidate.
    fields: Vec<BlockArg>,
    variant: Option<u32>,
}

impl Builder<'_, '_> {
    pub(super) fn lower_match(
        &mut self,
        whole: &HirExpr,
        scrutinee_expr: &HirExpr,
        source_arms: &[HirMatchArm],
    ) -> Result<ValueId, String> {
        self.lower_match_control(whole, scrutinee_expr, source_arms)?
            .ok_or_else(|| "divergent or unit match cannot produce an SSA value".to_string())
    }

    pub(super) fn lower_match_control(
        &mut self,
        whole: &HirExpr,
        scrutinee_expr: &HirExpr,
        source_arms: &[HirMatchArm],
    ) -> Result<Option<ValueId>, String> {
        let scrutinee_ty = self.ty(&scrutinee_expr.ty);
        let shape = self.resolve_match_shape(&scrutinee_ty, source_arms)?;

        // A scrutinee that is itself a borrowed read holds a loan on the
        // collection it read. The match is what reads that loan, so it is the
        // match that ends it - not the enclosing scope, which would keep the
        // collection borrowed for the rest of the body.
        let scrutinee_loan_floor = self.scope_loans.len();
        let scrutinee =
            lower_initial_value_transfer(self, scrutinee_expr, "match scrutinee", shape.read())?;
        let mut outer_live = self.owned_live.clone();
        outer_live.remove(&scrutinee);
        let plan = MatchPlan {
            borrowed: self.value_own_kind(scrutinee) == Some(OwnKind::Guaranteed),
            shape,
            scrutinee,
            scrutinee_ty,
            provenance: Provenance::Site(scrutinee_expr.site),
            result_ty: self.ty(&whole.ty),
            outer_bindings: self.bindings.keys().copied().collect(),
            outer_loans: self.argument_receiver_loans.len(),
            outer_live,
        };

        let groups = self.open_match_groups(&plan, source_arms)?;
        // The scrutinee is read once before any candidate; every candidate
        // after it is a conditional path, where a loan may not end.
        self.branch_depth += 1;
        let mut exits = Vec::new();
        for group in groups {
            self.lower_candidate_group(&plan, source_arms, group, &mut exits)?;
        }
        self.branch_depth -= 1;

        let result = self.merge_match_exits(exits, &plan.result_ty)?;
        if self.scope_loans.len() > scrutinee_loan_floor {
            let loans = self.scope_loans.split_off(scrutinee_loan_floor);
            self.end_call_loans(&loans)?;
        }
        Ok(result)
    }

    /// Open the ordered candidate runs. Only a variant scrutinee has more than
    /// one: its tag switch gives each branch the payloads and the arms that can
    /// still match there.
    fn open_match_groups(
        &mut self,
        plan: &MatchPlan,
        source_arms: &[HirMatchArm],
    ) -> Result<Vec<CandidateGroup>, String> {
        let MatchShape::Variant {
            shape,
            descriptor,
            candidates,
        } = &plan.shape
        else {
            return Ok(vec![CandidateGroup {
                entry: self.control_state(),
                owned_live: self.owned_live.clone(),
                arms: (0..source_arms.len()).collect(),
                fields: Vec::new(),
                variant: None,
            }]);
        };
        let inherited = self.control_state();
        let branches = self.emit_variant_switch(*shape, descriptor, plan.scrutinee)?;
        branches
            .into_iter()
            .map(|branch| {
                let variant = usize::try_from(branch.variant)
                    .map_err(|_| "variant tag exceeds usize".to_string())?;
                let mut entry = inherited.clone();
                entry.block = branch.block;
                Ok(CandidateGroup {
                    entry,
                    owned_live: branch.owned_live,
                    arms: candidates[variant].clone(),
                    fields: branch.fields,
                    variant: Some(branch.variant),
                })
            })
            .collect()
    }

    /// Run one ordered group of candidates. Each candidate probes, binds,
    /// runs its guard and takes the scrutinee apart only once nothing can
    /// still reject it; a rejected candidate restores the group's entry
    /// ownership and hands the scrutinee to the next one.
    fn lower_candidate_group(
        &mut self,
        plan: &MatchPlan,
        source_arms: &[HirMatchArm],
        group: CandidateGroup,
        exits: &mut Vec<MatchExit>,
    ) -> Result<(), String> {
        self.restore_control_state(&group.entry);
        self.owned_live = group.owned_live;
        // A variant with an uninhabited payload has no values, so its branch is
        // unreachable and the source needs no arm for it.
        if group.arms.is_empty() {
            self.destroy_all_live()?;
            return self.set_terminator(SemTerminator::Unreachable);
        }

        let mut root_live = self.owned_live.clone();
        let mut fields = group.fields;
        let mut fallthrough = true;
        for arm_index in group.arms {
            let arm = &source_arms[arm_index];
            let mut failures = Vec::new();
            // A whole-scrutinee binding on a switch branch rebuilds the value
            // the switch took apart, so a failed guard takes it apart again and
            // the later candidates read the payloads it gives back.
            let mut rebuilt = None;
            self.probe_match_candidate(
                plan,
                arm,
                group.variant,
                &mut fields,
                &mut rebuilt,
                &mut failures,
            )?;

            if let Some(guard) = &arm.guard {
                // A guard runs with the candidate's names bound. What it
                // allocates and what it borrows belong to the guard, not to the
                // arm, and are released before the candidate can be rejected.
                let guard_live = self.owned_live.clone();
                let guard_loans = self.argument_receiver_loans.len();
                let guard_bindings = self.bindings.keys().copied().collect();
                let condition = self.lower_read_operand(guard, "match guard")?.value;
                self.cleanup_match_candidate(&guard_live, guard_loans, &guard_bindings)?;
                failures.push(self.branch_candidate_test(condition)?);
            }

            self.end_loans_since(plan.outer_loans)?;
            self.select_match_candidate(plan, arm, &fields)?;
            self.acquire_selected_match_bindings(&plan.outer_bindings)?;
            let result = self.lower_selected_body(&arm.body, &plan.result_ty)?;
            if self.is_open() {
                for value in plan.outer_live.keys() {
                    if !self.owned_live.contains_key(value) {
                        return Err(format!(
                            "match arm {arm_index} consumes an outer non-binding owner"
                        ));
                    }
                }
                // The selected result survives candidate cleanup, but must
                // still be released if closing another owner fails.
                let mut protected_live = plan.outer_live.clone();
                if let Some(result) = &result {
                    if let Some(ty) = self.owned_live.get(&result.value) {
                        protected_live.insert(result.value, ty.clone());
                    }
                }
                self.cleanup_match_candidate(
                    &protected_live,
                    plan.outer_loans,
                    &plan.outer_bindings,
                )?;
                if let Some(result) = &result {
                    self.owned_live.remove(&result.value);
                }
                exits.push(MatchExit {
                    state: self.control_state(),
                    result,
                });
            }

            if failures.is_empty() {
                fallthrough = false;
                break;
            }
            let mut cleaned = Vec::with_capacity(failures.len());
            for failure in failures {
                self.restore_control_state(&failure);
                // A guard is the only test a whole-scrutinee binding arm can
                // fail, so the rebuilt value is taken apart exactly once.
                if let Some(value) = rebuilt.take() {
                    fields = self.rebuilt_scrutinee_fields(plan, group.variant, value)?;
                    root_live = self.owned_live.clone();
                }
                self.cleanup_match_candidate(&root_live, plan.outer_loans, &plan.outer_bindings)?;
                cleaned.push(self.control_state());
            }
            self.merge_control_states(cleaned)?;
        }

        if fallthrough && self.is_open() {
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::Unreachable)?;
        }
        Ok(())
    }

    /// Probe one candidate's predicate and introduce its names.
    ///
    /// Each failing test appends the control state its rejection lands in. The
    /// payload tests read the candidate's projected fields the same way for a
    /// record, a tuple and a variant payload, so only producing those fields
    /// and naming the whole scrutinee vary by shape.
    fn probe_match_candidate(
        &mut self,
        plan: &MatchPlan,
        arm: &HirMatchArm,
        variant: Option<u32>,
        fields: &mut Vec<BlockArg>,
        rebuilt: &mut Option<ValueId>,
        failures: &mut Vec<ControlState>,
    ) -> Result<(), String> {
        match &plan.shape {
            MatchShape::Scalar => match &arm.predicate {
                HirMatchArmPredicate::Literal { lit, .. } => {
                    let condition = self.scalar_match_literal_test(plan, lit)?;
                    failures.push(self.branch_candidate_test(condition)?);
                }
                HirMatchArmPredicate::Regex { literal_id, .. } => {
                    let condition = self.regex_match_test(plan, *literal_id)?;
                    failures.push(self.branch_candidate_test(condition)?);
                }
                _ => {}
            },
            MatchShape::Aggregate {
                shape,
                recipes,
                initial_value,
            } => {
                *fields =
                    self.probe_aggregate_fields(plan.scrutinee, *shape, recipes, *initial_value)?;
            }
            // The switch already handed this branch its payloads.
            MatchShape::Variant { .. } => {}
        }

        self.bind_match_fields(&arm.bindings, fields, &arm.span)?;
        if let HirMatchArmPredicate::Binding {
            binding_id, name, ..
        } = &arm.predicate
        {
            let value = self.whole_scrutinee_value(plan, variant, fields, rebuilt)?;
            self.bind_selected_value(*binding_id, name, value, arm.span.clone())?;
        }
        for predicate in &arm.payload_predicates {
            let condition = self.lower_payload_literal_test(fields, predicate)?;
            failures.push(self.branch_candidate_test(condition)?);
        }
        for predicate in &arm.payload_variant_predicates {
            failures.extend(self.lower_nested_predicate(fields, predicate, &arm.span)?);
        }
        Ok(())
    }

    /// The value a whole-scrutinee binding names. A scalar or aggregate
    /// scrutinee is still whole; a borrowed variant scrutinee still has its
    /// value, and an owned one is rebuilt from the payloads its switch handed
    /// out so a failed guard can take it apart again.
    fn whole_scrutinee_value(
        &mut self,
        plan: &MatchPlan,
        variant: Option<u32>,
        fields: &[BlockArg],
        rebuilt: &mut Option<ValueId>,
    ) -> Result<ValueId, String> {
        let MatchShape::Variant { shape, .. } = &plan.shape else {
            return Ok(plan.scrutinee);
        };
        if plan.borrowed {
            return Ok(plan.scrutinee);
        }
        let variant = variant.ok_or("variant candidate has no switch branch")?;
        let value = self.emit_variant_make(*shape, variant, &plan.scrutinee_ty, fields)?;
        *rebuilt = Some(value);
        Ok(value)
    }

    /// Take a rejected whole-scrutinee binding's rebuilt value apart again.
    fn rebuilt_scrutinee_fields(
        &mut self,
        plan: &MatchPlan,
        variant: Option<u32>,
        value: ValueId,
    ) -> Result<Vec<BlockArg>, String> {
        let MatchShape::Variant {
            shape, descriptor, ..
        } = &plan.shape
        else {
            return Err("only a variant candidate rebuilds its scrutinee".to_string());
        };
        let variant = variant.ok_or("variant candidate has no switch branch")?;
        self.emit_variant_destructure(*shape, variant, descriptor, value)
    }

    /// Take what the winning arm needs out of the scrutinee, once no test can
    /// still reject it. Everything the candidate probed is a loan until here.
    fn select_match_candidate(
        &mut self,
        plan: &MatchPlan,
        arm: &HirMatchArm,
        fields: &[BlockArg],
    ) -> Result<(), String> {
        match &plan.shape {
            MatchShape::Scalar => Ok(()),
            MatchShape::Aggregate {
                shape,
                initial_value,
                ..
            } => {
                // A tuple of scalars was copied whole, and a whole-scrutinee
                // binding names the aggregate itself: neither is taken apart.
                if *initial_value || matches!(arm.predicate, HirMatchArmPredicate::Binding { .. }) {
                    return Ok(());
                }
                let transferred = self.emit_destructure_value(
                    plan.scrutinee,
                    &plan.scrutinee_ty,
                    *shape,
                    plan.provenance.clone(),
                )?;
                for binding in &arm.bindings {
                    let field = usize::try_from(binding.field_idx)
                        .ok()
                        .and_then(|index| transferred.get(index))
                        .ok_or_else(|| {
                            format!(
                                "match binding `{}` selects missing field {}",
                                binding.name, binding.field_idx
                            )
                        })?;
                    self.redeclare_binding(binding.binding, BindingTarget::Value(field.id))?;
                }
                // The nested enums are now owned by the arm: take each apart
                // once so its own bindings own their payloads and the rest join
                // the candidate's cleanup set.
                let owned = transferred
                    .into_iter()
                    .map(|field| BlockArg {
                        value: field.id,
                        ty: field.ty,
                        own: field.own,
                    })
                    .collect::<Vec<_>>();
                self.transfer_selected_payloads(&owned, &arm.payload_variant_predicates)
            }
            MatchShape::Variant { .. } => {
                if plan.borrowed {
                    // The payloads name the scrutinee's region: they stay
                    // readable for the arm body, and the candidate cleanup ends
                    // them on every exit from it.
                    self.argument_receiver_loans.extend(
                        fields
                            .iter()
                            .filter(|field| field.own == OwnKind::Guaranteed)
                            .map(|field| field.value),
                    );
                }
                self.transfer_selected_payloads(fields, &arm.payload_variant_predicates)
            }
        }
    }

    /// Resolve the scrutinee's shape and check every arm against it.
    fn resolve_match_shape(
        &mut self,
        scrutinee_ty: &ResolvedTy,
        arms: &[HirMatchArm],
    ) -> Result<MatchShape, String> {
        if scrutinee_ty.is_integer()
            || matches!(
                scrutinee_ty,
                ResolvedTy::Bool | ResolvedTy::Char | ResolvedTy::String
            )
        {
            self.check_scalar_arms(scrutinee_ty, arms)?;
            return Ok(MatchShape::Scalar);
        }
        if is_concrete_variant_type(self.service.module, scrutinee_ty) {
            return self.resolve_variant_shape(scrutinee_ty, arms);
        }
        let shape = self.service.require_aggregate_shape(scrutinee_ty)?;
        let recipes = crate::aggregate_field_recipes(
            shape,
            scrutinee_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        self.check_aggregate_arms(scrutinee_ty, &recipes, arms)?;
        Ok(MatchShape::Aggregate {
            shape,
            recipes,
            initial_value: is_initial_value_type(scrutinee_ty),
        })
    }

    /// A binding arm names the whole scrutinee, so it names the scrutinee's
    /// exact type and takes nothing out of it.
    fn check_whole_scrutinee_binding(
        &mut self,
        arm: &HirMatchArm,
        binding_ty: &ResolvedTy,
        scrutinee_ty: &ResolvedTy,
    ) -> Result<(), String> {
        if self.ty(binding_ty) != *scrutinee_ty {
            return Err(format!(
                "whole-scrutinee binding has `{}`, its scrutinee is `{}`",
                self.ty(binding_ty).user_facing(),
                scrutinee_ty.user_facing()
            ));
        }
        if !arm.bindings.is_empty()
            || !arm.payload_predicates.is_empty()
            || !arm.payload_variant_predicates.is_empty()
        {
            return Err(
                "whole-scrutinee binding arm carries impossible payload metadata".to_string(),
            );
        }
        Ok(())
    }

    fn check_scalar_arms(
        &mut self,
        scrutinee_ty: &ResolvedTy,
        arms: &[HirMatchArm],
    ) -> Result<(), String> {
        // Only an uninhabited scrutinee can have no arms, and only the variant
        // shape has one: a scalar always has values to select over.
        if arms.is_empty() {
            return Err("scalar match has no source arms".to_string());
        }
        for arm in arms {
            if !arm.bindings.is_empty()
                || !arm.payload_predicates.is_empty()
                || !arm.payload_variant_predicates.is_empty()
            {
                return Err("scalar arm carries aggregate payload metadata".to_string());
            }
            match &arm.predicate {
                HirMatchArmPredicate::Wildcard => {}
                HirMatchArmPredicate::Literal { ty, .. } => {
                    if self.ty(ty) != *scrutinee_ty {
                        return Err("scalar match literal type differs from its scrutinee".into());
                    }
                }
                HirMatchArmPredicate::Binding { ty, .. } => {
                    self.check_whole_scrutinee_binding(arm, ty, scrutinee_ty)?;
                }
                HirMatchArmPredicate::Regex { captures, .. }
                    if *scrutinee_ty == ResolvedTy::String =>
                {
                    if !captures.is_empty() {
                        return Err(
                            "regex match-arm capture bindings are outside the SIR contract"
                                .to_string(),
                        );
                    }
                }
                _ => {
                    return Err(
                        "scalar match requires a matching scalar literal, binding or wildcard predicate"
                            .to_string(),
                    )
                }
            }
        }
        Ok(())
    }

    fn check_aggregate_arms(
        &mut self,
        scrutinee_ty: &ResolvedTy,
        recipes: &[AggregateFieldRecipe],
        arms: &[HirMatchArm],
    ) -> Result<(), String> {
        // A record or tuple always has values, so it always has arms.
        if arms.is_empty() {
            return Err("aggregate match has no source arms".to_string());
        }
        for arm in arms {
            match &arm.predicate {
                HirMatchArmPredicate::RecordProject { ty } => {
                    if self.ty(ty) != *scrutinee_ty {
                        return Err(format!(
                            "record pattern arm has `{}`, scrutinee has `{}`",
                            self.ty(ty).user_facing(),
                            scrutinee_ty.user_facing()
                        ));
                    }
                }
                HirMatchArmPredicate::TupleProject { arity } => {
                    if usize::try_from(*arity).ok() != Some(recipes.len()) {
                        return Err(format!(
                            "tuple pattern arm has arity {arity}, scrutinee `{}` has {}",
                            scrutinee_ty.user_facing(),
                            recipes.len()
                        ));
                    }
                }
                HirMatchArmPredicate::Binding { ty, .. } => {
                    self.check_whole_scrutinee_binding(arm, ty, scrutinee_ty)?;
                }
                HirMatchArmPredicate::Wildcard => {
                    if !arm.bindings.is_empty() || !arm.payload_predicates.is_empty() {
                        return Err(
                            "wildcard aggregate arm carries impossible field metadata".to_string()
                        );
                    }
                }
                _ => return Err(
                    "aggregate match requires record, tuple, binding or wildcard arm predicates"
                        .to_string(),
                ),
            }
        }
        Ok(())
    }

    fn resolve_variant_shape(
        &mut self,
        scrutinee_ty: &ResolvedTy,
        arms: &[HirMatchArm],
    ) -> Result<MatchShape, String> {
        let shape = self.service.require_variant_shape(scrutinee_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(usize::try_from(shape.0).map_err(|_| "variant shape id exceeds usize")?)
            .filter(|descriptor| descriptor.id == shape)
            .cloned()
            .ok_or_else(|| format!("variant shape {} disappeared during lowering", shape.0))?;
        for arm in arms {
            match &arm.predicate {
                HirMatchArmPredicate::EnumVariant {
                    variant_match,
                    variant_idx,
                } => {
                    let variant = descriptor
                        .variants
                        .get(
                            usize::try_from(*variant_idx)
                                .map_err(|_| "match variant index exceeds usize".to_string())?,
                        )
                        .ok_or_else(|| {
                            format!(
                                "match arm tag {variant_idx} is absent from `{}`",
                                scrutinee_ty.user_facing()
                            )
                        })?;
                    if variant.name != variant_match.variant_name {
                        return Err(format!(
                            "match tag {variant_idx} names `{}`, exact descriptor names `{}`",
                            variant_match.variant_name, variant.name
                        ));
                    }
                }
                HirMatchArmPredicate::Wildcard => {
                    if !arm.bindings.is_empty()
                        || !arm.payload_predicates.is_empty()
                        || !arm.payload_variant_predicates.is_empty()
                    {
                        return Err(
                            "wildcard variant arm carries impossible payload metadata".to_string()
                        );
                    }
                }
                HirMatchArmPredicate::Binding { ty, .. } => {
                    self.check_whole_scrutinee_binding(arm, ty, scrutinee_ty)?;
                }
                _ => {
                    return Err(
                        "non-variant match predicates are outside the variant-switch SIR contract"
                            .to_string(),
                    );
                }
            }
        }

        let candidates = (0..descriptor.variants.len())
            .map(|variant| {
                arms.iter()
                    .enumerate()
                    .filter_map(|(index, arm)| match arm.predicate {
                        HirMatchArmPredicate::EnumVariant { variant_idx, .. }
                            if usize::try_from(variant_idx).ok() == Some(variant) =>
                        {
                            Some(index)
                        }
                        HirMatchArmPredicate::Wildcard | HirMatchArmPredicate::Binding { .. } => {
                            Some(index)
                        }
                        _ => None,
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        for (variant, candidates) in candidates.iter().enumerate() {
            if candidates.is_empty()
                && !is_unconstructable_variant(self.service.module, &descriptor.variants[variant])
            {
                return Err(format!("match is missing exhaustive variant tag {variant}"));
            }
        }
        Ok(MatchShape::Variant {
            shape,
            descriptor,
            candidates,
        })
    }

    /// Read every field of a candidate's aggregate without consuming it.
    /// Owning fields are borrowed until the candidate fails or the arm is
    /// selected; the rest are independent copies.
    fn probe_aggregate_fields(
        &mut self,
        scrutinee: ValueId,
        shape: AggregateShapeRef,
        recipes: &[AggregateFieldRecipe],
        initial_value: bool,
    ) -> Result<Vec<BlockArg>, String> {
        let source = Operand { value: scrutinee };
        let mut fields = Vec::with_capacity(recipes.len());
        for (index, recipe) in recipes.iter().enumerate() {
            let field = u32::try_from(index).map_err(|_| "aggregate field exceeds u32")?;
            let owning = recipe.own == OwnKind::Owned;
            let kind = if initial_value {
                SemOpKind::TupleGet {
                    tuple: source.clone(),
                    index: field,
                }
            } else if owning {
                SemOpKind::AggregateProjectBorrow {
                    shape,
                    aggregate: source.clone(),
                    field,
                }
            } else {
                SemOpKind::AggregateProjectCopy {
                    shape,
                    aggregate: source.clone(),
                    field,
                }
            };
            let value = self.emit_typed(Provenance::Synthesized, &recipe.ty, kind)?;
            if owning && !initial_value {
                self.argument_receiver_loans.push(value);
            }
            fields.push(BlockArg {
                value,
                ty: recipe.ty.clone(),
                own: if owning && !initial_value {
                    OwnKind::Guaranteed
                } else {
                    OwnKind::None
                },
            });
        }
        Ok(fields)
    }

    /// Test the borrowed scrutinee against one compiled regex literal.
    ///
    /// The literal's index selects its slot in the module's handle array, so
    /// each pattern is compiled once at process start rather than per arm
    /// evaluation. The scrutinee is borrowed; the call produces only a bool.
    fn regex_match_test(&mut self, plan: &MatchPlan, literal_id: u32) -> Result<ValueId, String> {
        let index = self.emit_typed(
            plan.provenance.clone(),
            &ResolvedTy::I64,
            SemOpKind::ConstInteger(i128::from(literal_id)),
        )?;
        let raw = self.fresh_value();
        let continuation = self.fresh_value();
        let normal = self.new_block(vec![BlockArg {
            value: continuation,
            ty: ResolvedTy::Bool,
            own: OwnKind::None,
        }]);
        let id = crate::OpId(self.ops);
        self.ops += 1;
        self.set_terminator(SemTerminator::RtCall {
            id,
            family: hew_types::RuntimeCallFamily::RegexMatch,
            args: vec![
                crate::BoundaryOperand {
                    operand: Operand { value: index },
                    decision: crate::BoundaryDecision::Copy,
                },
                crate::BoundaryOperand {
                    operand: Operand {
                        value: plan.scrutinee,
                    },
                    decision: crate::BoundaryDecision::Borrow,
                },
            ],
            result: crate::CallResult::Value(ValueDef {
                id: raw,
                ty: ResolvedTy::Bool,
                own: OwnKind::None,
            }),
            normal: crate::Edge {
                target: normal,
                args: vec![Operand { value: raw }],
            },
            unwind: crate::CallUnwind::NotApplicable,
        })?;
        self.current = normal;
        Ok(continuation)
    }

    fn scalar_match_literal_test(
        &mut self,
        plan: &MatchPlan,
        literal: &HirLiteral,
    ) -> Result<ValueId, String> {
        let ty = &plan.scrutinee_ty;
        let constant =
            match literal {
                HirLiteral::Integer(value) if ty.is_integer() => SemOpKind::ConstInteger(*value),
                HirLiteral::Bool(value) if *ty == ResolvedTy::Bool => SemOpKind::ConstBool(*value),
                HirLiteral::Char(value) if *ty == ResolvedTy::Char => SemOpKind::ConstChar(*value),
                HirLiteral::String(value) if *ty == ResolvedTy::String => {
                    SemOpKind::ConstStr(self.service.intern_string(value))
                }
                _ => return Err(
                    "literal match requires an exact integer, boolean, character or string literal"
                        .into(),
                ),
            };
        let value = self.emit_typed(plan.provenance.clone(), ty, constant)?;
        if *ty == ResolvedTy::String {
            let equals = self.lower_string_equals_values(plan.scrutinee, value)?;
            self.emit_destroy(value)?;
            return Ok(equals);
        }
        self.emit_typed(
            plan.provenance.clone(),
            &ResolvedTy::Bool,
            SemOpKind::Binary {
                op: hew_parser::ast::BinaryOp::Equal,
                lhs: Operand {
                    value: plan.scrutinee,
                },
                rhs: Operand { value },
            },
        )
    }

    fn bind_match_fields(
        &mut self,
        bindings: &[HirMatchArmBinding],
        fields: &[BlockArg],
        span: &std::ops::Range<usize>,
    ) -> Result<(), String> {
        for binding in bindings {
            let field = usize::try_from(binding.field_idx)
                .ok()
                .and_then(|index| fields.get(index))
                .ok_or_else(|| {
                    format!(
                        "match binding `{}` selects missing field {}",
                        binding.name, binding.field_idx
                    )
                })?;
            let binding_ty = self.ty(&binding.ty);
            if binding_ty != field.ty {
                return Err(format!(
                    "match binding `{}` has `{}`, expected `{}`",
                    binding.name,
                    binding_ty.user_facing(),
                    field.ty.user_facing()
                ));
            }
            let value = field.value;
            self.bind_selected_value(binding.binding, &binding.name, value, span.clone())?;
        }
        Ok(())
    }

    /// Give the selected arm's names their own owners, in declaration order.
    fn acquire_selected_match_bindings(
        &mut self,
        outer_bindings: &HashSet<BindingId>,
    ) -> Result<(), String> {
        let mut selected = self
            .binding_declarations
            .iter()
            .filter(|(binding, _)| !outer_bindings.contains(binding))
            .map(|(binding, declaration)| (*binding, *declaration))
            .collect::<Vec<_>>();
        selected.sort_unstable_by_key(|(_, declaration)| *declaration);
        for (binding, _) in selected {
            let BindingTarget::Value(value) = self.bindings[&binding] else {
                continue;
            };
            if self.value_own_kind(value) != Some(OwnKind::Owned) {
                continue;
            }
            let target = self.acquire_binding_target(value)?;
            self.redeclare_binding(binding, target)?;
        }
        Ok(())
    }

    fn lower_payload_literal_test(
        &mut self,
        fields: &[BlockArg],
        predicate: &HirPayloadPredicate,
    ) -> Result<ValueId, String> {
        let field = usize::try_from(predicate.field_idx)
            .ok()
            .and_then(|index| fields.get(index))
            .ok_or_else(|| {
                format!(
                    "payload literal selects missing field {}",
                    predicate.field_idx
                )
            })?;
        let ty = self.ty(&predicate.ty);
        if field.ty != ty {
            return Err(format!(
                "payload literal field has `{}`, predicate expects `{}`",
                field.ty.user_facing(),
                ty.user_facing()
            ));
        }
        let literal = match &predicate.literal {
            HirLiteral::Integer(value) if ty.is_integer() => self.emit_typed(
                Provenance::Synthesized,
                &ty,
                SemOpKind::ConstInteger(*value),
            )?,
            HirLiteral::Bool(value) if ty == ResolvedTy::Bool => {
                self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::ConstBool(*value))?
            }
            HirLiteral::Float(value) if ty.is_float() => {
                self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::ConstFloat(*value))?
            }
            HirLiteral::Char(value) if ty == ResolvedTy::Char => {
                self.emit_typed(Provenance::Synthesized, &ty, SemOpKind::ConstChar(*value))?
            }
            HirLiteral::String(value) if ty == ResolvedTy::String => {
                let literal = self.service.intern_string(value);
                self.emit_typed(
                    Provenance::Synthesized,
                    &ResolvedTy::String,
                    SemOpKind::ConstStr(literal),
                )?
            }
            HirLiteral::Bytes(_) => {
                return Err(
                    "bytes payload literal matching awaits an audited BytesEquals runtime contract"
                        .to_string(),
                );
            }
            _ => {
                return Err(format!(
                    "payload literal has no verified equality operation for `{}`",
                    ty.user_facing()
                ));
            }
        };
        if ty == ResolvedTy::String {
            let equals = self.lower_string_equals_values(field.value, literal)?;
            self.emit_destroy(literal)?;
            Ok(equals)
        } else {
            self.emit_typed(
                Provenance::Synthesized,
                &ResolvedTy::Bool,
                SemOpKind::Binary {
                    op: hew_parser::ast::BinaryOp::Equal,
                    lhs: Operand { value: field.value },
                    rhs: Operand { value: literal },
                },
            )
        }
    }

    /// Probe one nested payload variant without consuming its enum. The tag
    /// test and every projected field leave the owner live for a later
    /// candidate; owning fields are borrowed until the candidate fails or
    /// [`Self::transfer_selected_payloads`] takes them.
    fn lower_nested_predicate(
        &mut self,
        parent_fields: &[BlockArg],
        predicate: &HirPayloadVariantPredicate,
        span: &std::ops::Range<usize>,
    ) -> Result<Vec<ControlState>, String> {
        let field = usize::try_from(predicate.field_idx)
            .ok()
            .and_then(|index| parent_fields.get(index))
            .ok_or_else(|| {
                format!(
                    "nested variant predicate selects missing field {}",
                    predicate.field_idx
                )
            })?;
        let payload_ty = self.ty(&predicate.payload_ty);
        if field.ty != payload_ty {
            return Err(format!(
                "nested variant field has `{}`, predicate expects `{}`",
                field.ty.user_facing(),
                payload_ty.user_facing()
            ));
        }
        let (shape, desired) = self.nested_variant(&payload_ty, predicate)?;
        let source = Operand { value: field.value };
        let condition = self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::Bool,
            SemOpKind::VariantIs {
                shape,
                variant: predicate.variant_idx,
                source: source.clone(),
            },
        )?;
        let mut failures = vec![self.branch_candidate_test(condition)?];
        let mut fields = Vec::with_capacity(desired.fields.len());
        for (index, field) in desired.fields.iter().enumerate() {
            self.service.require_type_facts(&field.ty)?;
            let field_index = u32::try_from(index).map_err(|_| "variant field exceeds u32")?;
            let owning =
                OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())? == OwnKind::Owned;
            let kind = if owning {
                SemOpKind::VariantProjectBorrow {
                    shape,
                    variant: predicate.variant_idx,
                    source: source.clone(),
                    field: field_index,
                }
            } else {
                SemOpKind::VariantProjectCopy {
                    shape,
                    variant: predicate.variant_idx,
                    source: source.clone(),
                    field: field_index,
                }
            };
            let value = self.emit_typed(Provenance::Synthesized, &field.ty, kind)?;
            if owning {
                self.argument_receiver_loans.push(value);
            }
            fields.push(BlockArg {
                value,
                ty: field.ty.clone(),
                own: if owning {
                    OwnKind::Guaranteed
                } else {
                    OwnKind::None
                },
            });
        }
        for literal in &predicate.literals {
            let condition = self.lower_payload_literal_test(&fields, literal)?;
            failures.push(self.branch_candidate_test(condition)?);
        }
        self.bind_match_fields(&predicate.bindings, &fields, span)?;
        for nested in &predicate.nested {
            failures.extend(self.lower_nested_predicate(&fields, nested, span)?);
        }
        Ok(failures)
    }

    fn nested_variant(
        &mut self,
        payload_ty: &ResolvedTy,
        predicate: &HirPayloadVariantPredicate,
    ) -> Result<(VariantShapeId, crate::SemVariant), String> {
        let shape = self.service.require_variant_shape(payload_ty)?;
        let descriptor = self
            .service
            .variant_shapes
            .get(usize::try_from(shape.0).map_err(|_| "variant shape id exceeds usize")?)
            .filter(|descriptor| descriptor.id == shape)
            .ok_or_else(|| format!("variant shape {} disappeared during lowering", shape.0))?;
        let desired = descriptor
            .variants
            .get(
                usize::try_from(predicate.variant_idx)
                    .map_err(|_| "nested variant index exceeds usize".to_string())?,
            )
            .ok_or_else(|| {
                format!(
                    "nested variant tag {} is absent from `{}`",
                    predicate.variant_idx,
                    payload_ty.user_facing()
                )
            })?;
        if desired.name != predicate.variant_match.variant_name {
            return Err(format!(
                "nested variant tag {} names `{}`, exact descriptor names `{}`",
                predicate.variant_idx, predicate.variant_match.variant_name, desired.name
            ));
        }
        Ok((shape, desired.clone()))
    }

    /// Transfer every probed nested payload into the selected arm exactly
    /// once. Each nested enum is consumed after its probe loans have ended;
    /// bound fields become the arm's owners and unbound owning fields join the
    /// candidate's cleanup set.
    fn transfer_selected_payloads(
        &mut self,
        parent_fields: &[BlockArg],
        predicates: &[HirPayloadVariantPredicate],
    ) -> Result<(), String> {
        for predicate in predicates {
            let source = parent_fields[predicate.field_idx as usize].value;
            let payload_ty = self.ty(&predicate.payload_ty);
            let (shape, desired) = self.nested_variant(&payload_ty, predicate)?;
            let mut fields = Vec::with_capacity(desired.fields.len());
            for field in &desired.fields {
                fields.push(ValueDef {
                    id: self.fresh_value(),
                    ty: field.ty.clone(),
                    own: OwnKind::of_ty(&field.ty, self.service.checked_facts.rows())?,
                });
            }
            let id = crate::OpId(self.ops);
            self.current_block_mut().append_op(SemOp {
                id,
                results: fields.clone(),
                kind: SemOpKind::VariantDestructure {
                    shape,
                    variant: predicate.variant_idx,
                    source: Operand { value: source },
                },
                provenance: Provenance::Synthesized,
            })?;
            self.ops += 1;
            self.owned_live.remove(&source);
            let fields = fields
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
                .collect::<Vec<_>>();
            for binding in &predicate.bindings {
                self.redeclare_binding(
                    binding.binding,
                    BindingTarget::Value(fields[binding.field_idx as usize].value),
                )?;
            }
            self.transfer_selected_payloads(&fields, &predicate.nested)?;
        }
        Ok(())
    }
}
