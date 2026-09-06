//! Ordered scalar patterns share ordinary match exits and ownership cleanup.

use super::{Builder, MatchExit};
use crate::{Operand, Provenance, SemOpKind, SemTerminator, ValueId};
use hew_hir::{HirBinding, HirExpr, HirLiteral, HirMatchArm, HirMatchArmPredicate};
use hew_types::ResolvedTy;

impl Builder<'_, '_> {
    pub(super) fn lower_scalar_match(
        &mut self,
        whole: &HirExpr,
        scrutinee: &HirExpr,
        arms: &[HirMatchArm],
    ) -> Result<Option<ValueId>, String> {
        if arms.is_empty() {
            return Err("scalar match has no source arms".to_string());
        }
        let scrutinee_ty = self.ty(&scrutinee.ty);
        let selected = self.lower_read_operand(scrutinee, "scalar match scrutinee")?;
        let result_ty = self.ty(&whole.ty);
        let outer_bindings = self.bindings.keys().copied().collect();
        let outer_live = self.owned_live.clone();
        let mut exits = Vec::new();
        let mut fallthrough = true;
        for arm in arms {
            if !arm.bindings.is_empty()
                || !arm.payload_predicates.is_empty()
                || !arm.payload_variant_predicates.is_empty()
            {
                return Err("scalar arm carries aggregate payload metadata".to_string());
            }
            let mut failures = Vec::new();
            match &arm.predicate {
                HirMatchArmPredicate::Literal { lit, ty } => {
                    if self.ty(ty) != scrutinee_ty {
                        return Err("scalar match literal type differs from its scrutinee".into());
                    }
                    let condition = self.scalar_match_literal_test(
                        &selected, &scrutinee_ty, lit, Provenance::Site(scrutinee.site),
                    )?;
                    failures.push(self.branch_candidate_test(condition)?);
                }
                HirMatchArmPredicate::Wildcard => {}
                HirMatchArmPredicate::Binding {
                    binding_id,
                    name,
                    ty,
                } if self.ty(ty) == scrutinee_ty => {
                    self.bind_source_value(
                        &HirBinding {
                            id: *binding_id,
                            name: name.clone(),
                            ty: scrutinee_ty.clone(),
                            mutable: false,
                            span: arm.span.clone(),
                            is_consume: false,
                        },
                        selected.value,
                    )?;
                }
                _ => {
                    return Err(
                        "scalar match requires a matching scalar literal, binding or wildcard predicate"
                            .to_string(),
                    )
                }
            }
            if let Some(guard) = &arm.guard {
                let guard_bindings = self.bindings.keys().copied().collect();
                let guard_live = self.owned_live.clone();
                let condition = self.lower_read_operand(guard, "scalar match guard")?;
                self.cleanup_match_candidate(&guard_live, &guard_bindings)?;
                failures.push(self.branch_candidate_test(condition.value)?);
            }
            let result = self.lower_selected_match_body(arm, &result_ty)?;
            if self.is_open() {
                if let Some(result) = &result {
                    self.owned_live.remove(&result.value);
                }
                self.cleanup_match_candidate(&outer_live, &outer_bindings)?;
                exits.push(MatchExit {
                    state: self.control_state(),
                    result,
                });
            }
            if failures.is_empty() {
                fallthrough = false;
                break;
            }
            let mut next = Vec::new();
            for failure in failures {
                self.restore_control_state(&failure);
                self.cleanup_match_candidate(&outer_live, &outer_bindings)?;
                next.push(self.control_state());
            }
            self.merge_control_states(next)?;
        }
        if fallthrough && self.is_open() {
            self.destroy_all_live()?;
            self.set_terminator(SemTerminator::Unreachable)?;
        }
        self.merge_match_exits(exits, &result_ty)
    }

    fn scalar_match_literal_test(
        &mut self,
        selected: &Operand,
        ty: &ResolvedTy,
        literal: &HirLiteral,
        provenance: Provenance,
    ) -> Result<ValueId, String> {
        let constant = match literal {
            HirLiteral::Integer(value) if ty.is_integer() => SemOpKind::ConstI64(*value),
            HirLiteral::Bool(value) if *ty == ResolvedTy::Bool => SemOpKind::ConstBool(*value),
            HirLiteral::Char(value) if *ty == ResolvedTy::Char => SemOpKind::ConstChar(*value),
            _ => {
                return Err(
                    "scalar match requires an exact integer, boolean or character literal".into(),
                )
            }
        };
        let value = self.emit_typed(provenance.clone(), ty, constant)?;
        self.emit_typed(
            provenance,
            &ResolvedTy::Bool,
            SemOpKind::Binary {
                op: hew_parser::ast::BinaryOp::Equal,
                lhs: selected.clone(),
                rhs: Operand { value },
            },
        )
    }
}
