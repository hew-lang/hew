//! Ordered record and tuple patterns probe an aggregate before transferring it.

use std::collections::HashSet;

use super::{
    is_initial_value_type, lower_initial_value_transfer, Builder, MatchExit, OwnedBindingUse,
};
use crate::ownership::OwnKind;
use crate::{
    AggregateShapeRef, BindingTarget, BlockArg, Operand, Provenance, SemOpKind, SemTerminator,
    ValueId,
};
use hew_hir::{HirExpr, HirMatchArm, HirMatchArmPredicate};

impl Builder<'_, '_> {
    /// Lower an ordered record or tuple pattern match.
    ///
    /// A candidate probes the scrutinee's fields in place: owning fields are
    /// borrowed and the rest copied, so a failed literal test or guard leaves
    /// the aggregate whole for the next candidate. The selected arm ends those
    /// loans and destructures the aggregate exactly once; its bindings become
    /// the field owners and the unbound owning fields join the candidate's
    /// cleanup set, which is what makes a partial move legal (A393).
    #[expect(
        clippy::too_many_lines,
        reason = "ordered aggregate selection and candidate ownership form one match boundary"
    )]
    pub(super) fn lower_aggregate_match(
        &mut self,
        whole: &HirExpr,
        scrutinee_expr: &HirExpr,
        source_arms: &[HirMatchArm],
    ) -> Result<Option<ValueId>, String> {
        if source_arms.is_empty() {
            return Err("aggregate match has no source arms".to_string());
        }
        // Every arm is a conditional path: a loan may not end inside one.
        self.branch_depth += 1;
        let aggregate_ty = self.ty(&scrutinee_expr.ty);
        let initial_value = is_initial_value_type(&aggregate_ty);
        let shape = self.service.require_aggregate_shape(&aggregate_ty)?;
        let recipes = crate::aggregate_field_recipes(
            shape,
            &aggregate_ty,
            &self.service.aggregate_shapes,
            self.service.checked_facts.rows(),
        )?;
        for arm in source_arms {
            match &arm.predicate {
                HirMatchArmPredicate::RecordProject { ty } => {
                    if self.ty(ty) != aggregate_ty {
                        return Err(format!(
                            "record pattern arm has `{}`, scrutinee has `{}`",
                            self.ty(ty).user_facing(),
                            aggregate_ty.user_facing()
                        ));
                    }
                }
                HirMatchArmPredicate::TupleProject { arity } => {
                    if usize::try_from(*arity).ok() != Some(recipes.len()) {
                        return Err(format!(
                            "tuple pattern arm has arity {arity}, scrutinee `{}` has {}",
                            aggregate_ty.user_facing(),
                            recipes.len()
                        ));
                    }
                }
                HirMatchArmPredicate::Wildcard => {
                    if !arm.bindings.is_empty() || !arm.payload_predicates.is_empty() {
                        return Err(
                            "wildcard aggregate arm carries impossible field metadata".to_string()
                        );
                    }
                }
                _ => {
                    return Err(
                        "aggregate match requires record, tuple or wildcard arm predicates"
                            .to_string(),
                    )
                }
            }
            if !arm.payload_variant_predicates.is_empty() {
                return Err(
                    "aggregate match arm carries nested variant predicates it cannot select"
                        .to_string(),
                );
            }
        }

        let result_ty = self.ty(&whole.ty);
        let scrutinee = lower_initial_value_transfer(
            self,
            scrutinee_expr,
            "aggregate match scrutinee",
            OwnedBindingUse::Copy,
        )?;
        let outer_bindings = self.bindings.keys().copied().collect::<HashSet<_>>();
        let outer_loans = self.argument_receiver_loans.len();
        let root_live = self.owned_live.clone();
        let mut outer_live = root_live.clone();
        outer_live.remove(&scrutinee);
        let mut exits = Vec::new();
        let mut fallthrough = true;

        for arm in source_arms {
            let mut failures = Vec::new();
            let fields = self.probe_aggregate_fields(scrutinee, shape, &recipes, initial_value)?;
            self.bind_match_fields(&arm.bindings, &fields, &arm.span)?;
            for predicate in &arm.payload_predicates {
                let condition = self.lower_payload_literal_test(&fields, predicate)?;
                failures.push(self.branch_candidate_test(condition)?);
            }
            if let Some(guard) = &arm.guard {
                let guard_live = self.owned_live.clone();
                let condition = self
                    .lower_read_operand(guard, "aggregate match guard")?
                    .value;
                let keep_guard_values = guard_live
                    .iter()
                    .filter(|(value, _)| self.owned_live.contains_key(value))
                    .map(|(value, ty)| (*value, ty.clone()))
                    .collect();
                self.destroy_live_since(&keep_guard_values)?;
                failures.push(self.branch_candidate_test(condition)?);
            }

            self.end_loans_since(outer_loans)?;
            if !initial_value {
                let transferred = self.emit_destructure_value(
                    scrutinee,
                    &aggregate_ty,
                    shape,
                    Provenance::Site(scrutinee_expr.site),
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
            }
            self.acquire_selected_match_bindings(&outer_bindings)?;
            let result = self.lower_selected_match_body(arm, &result_ty)?;
            if self.is_open() {
                let mut protected_live = outer_live.clone();
                if let Some(result) = &result {
                    if let Some(ty) = self.owned_live.get(&result.value) {
                        protected_live.insert(result.value, ty.clone());
                    }
                }
                self.cleanup_match_candidate(&protected_live, outer_loans, &outer_bindings)?;
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
                self.cleanup_match_candidate(&root_live, outer_loans, &outer_bindings)?;
                cleaned.push(self.control_state());
            }
            self.merge_control_states(cleaned)?;
        }

        if fallthrough && self.is_open() {
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::Unreachable)?;
        }
        self.branch_depth -= 1;
        self.merge_match_exits(exits, &result_ty)
    }

    /// Read every field of a candidate's scrutinee without consuming it.
    /// Owning fields are borrowed until the candidate fails or the arm is
    /// selected; the rest are independent copies.
    fn probe_aggregate_fields(
        &mut self,
        scrutinee: ValueId,
        shape: AggregateShapeRef,
        recipes: &[crate::AggregateFieldRecipe],
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
}
