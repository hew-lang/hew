//! Checked numeric conversions expand into scalar checks and ordinary joins.
//! Integer limits come from the checker for the requested compilation target.

use hew_parser::ast::BinaryOp;
use hew_types::TryConversionKind;

use super::{
    Builder, ControlState, HirExpr, HirExprKind, MatchExit, Operand, Provenance, ResolvedTy,
    SemOpKind, ValueId,
};

impl Builder<'_, '_> {
    fn numeric_cast(&mut self, value: ValueId, target: &ResolvedTy) -> Result<ValueId, String> {
        self.emit_typed(
            Provenance::Synthesized,
            target,
            SemOpKind::Cast {
                value: Operand { value },
                to: target.clone(),
            },
        )
    }

    fn numeric_compare(
        &mut self,
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
    ) -> Result<ValueId, String> {
        self.emit_typed(
            Provenance::Synthesized,
            &ResolvedTy::Bool,
            SemOpKind::Binary {
                op,
                lhs: Operand { value: lhs },
                rhs: Operand { value: rhs },
            },
        )
    }

    fn numeric_integer(&mut self, ty: &ResolvedTy, value: i128) -> Result<ValueId, String> {
        self.emit_typed(Provenance::Synthesized, ty, SemOpKind::ConstInteger(value))
    }

    fn numeric_exit(&mut self, value: ValueId) -> MatchExit {
        self.owned_live.remove(&value);
        MatchExit {
            state: self.control_state(),
            result: Some(Operand { value }),
        }
    }

    /// Compare in the source domain: a bound is materialized only when it lies
    /// inside that domain. This also handles equal-width sign changes.
    fn integer_conversion_guards(
        &mut self,
        value: ValueId,
        ty: &ResolvedTy,
        from: (i128, i128),
        to: (i128, i128),
    ) -> Result<Vec<ControlState>, String> {
        let mut failures = Vec::new();
        for (needed, bound, op) in [
            (from.0 < to.0, to.0, BinaryOp::GreaterEqual),
            (from.1 > to.1, to.1, BinaryOp::LessEqual),
        ] {
            if needed {
                let bound = self.numeric_integer(ty, bound)?;
                let condition = self.numeric_compare(op, value, bound)?;
                failures.push(self.branch_candidate_test(condition)?);
            }
        }
        Ok(failures)
    }

    /// Integer maxima often round upwards in floating point. Use the exactly
    /// representable exclusive power-of-two upper bound, not a rounded MAX.
    #[allow(
        clippy::cast_precision_loss,
        reason = "integer limits are zero or signed powers of two; MAX + 1 is exact"
    )]
    fn float_integer_guards(
        &mut self,
        value: ValueId,
        ty: &ResolvedTy,
        range: (i128, i128),
    ) -> Result<Vec<ControlState>, String> {
        let mut failures = Vec::new();
        for (bound, op) in [
            (range.0 as f64, BinaryOp::GreaterEqual),
            ((range.1 + 1) as f64, BinaryOp::Less),
        ] {
            let bound =
                self.emit_typed(Provenance::Synthesized, ty, SemOpKind::ConstFloat(bound))?;
            let condition = self.numeric_compare(op, value, bound)?;
            failures.push(self.branch_candidate_test(condition)?);
        }
        Ok(failures)
    }

    pub(super) fn lower_saturating_cast(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::SaturatingWidthCast {
            value,
            from_ty,
            to_ty,
            from_range,
            to_range,
        } = &expr.kind
        else {
            unreachable!()
        };
        let from = from_range.ok_or("saturating cast has no checked source bounds")?;
        let to = to_range.ok_or("saturating cast has no checked target bounds")?;
        let source_ty = self.ty(from_ty);
        let target_ty = self.ty(to_ty);
        let value = self
            .lower_read_operand(value, "saturating conversion operand")?
            .value;
        let mut exits = Vec::new();
        for (needed, bound, op) in [
            (from.0 < to.0, to.0, BinaryOp::GreaterEqual),
            (from.1 > to.1, to.1, BinaryOp::LessEqual),
        ] {
            if needed {
                let limit = self.numeric_integer(&source_ty, bound)?;
                let condition = self.numeric_compare(op, value, limit)?;
                let failure = self.branch_candidate_test(condition)?;
                let success = self.control_state();
                self.restore_control_state(&failure);
                let clamped = self.numeric_integer(&target_ty, bound)?;
                exits.push(self.numeric_exit(clamped));
                self.restore_control_state(&success);
            }
        }
        let converted = self.numeric_cast(value, &target_ty)?;
        exits.push(self.numeric_exit(converted));
        self.merge_match_exits(exits, &target_ty)?
            .ok_or_else(|| "saturating cast has no result".into())
    }

    pub(super) fn lower_try_cast(&mut self, expr: &HirExpr) -> Result<ValueId, String> {
        let HirExprKind::TryWidthCast {
            value,
            from_ty,
            to_ty,
            kind,
            from_range,
            to_range,
        } = &expr.kind
        else {
            unreachable!()
        };
        let source_ty = self.ty(from_ty);
        let target_ty = self.ty(to_ty);
        let value = self
            .lower_read_operand(value, "checked conversion operand")?
            .value;
        let mut failures = match kind {
            TryConversionKind::IntToInt => self.integer_conversion_guards(
                value,
                &source_ty,
                from_range.ok_or("checked integer conversion has no source bounds")?,
                to_range.ok_or("checked integer conversion has no target bounds")?,
            )?,
            TryConversionKind::FloatToInt => self.float_integer_guards(
                value,
                &source_ty,
                to_range.ok_or("checked float conversion has no target bounds")?,
            )?,
            TryConversionKind::IntToFloat | TryConversionKind::FloatToFloat => Vec::new(),
        };
        // A float-to-integer cast only runs after ordered range checks. The
        // existing cast primitive also saturates rather than producing poison.
        let converted = self.numeric_cast(value, &target_ty)?;
        match kind {
            TryConversionKind::IntToFloat => failures.extend(self.float_integer_guards(
                converted,
                &target_ty,
                from_range.ok_or("checked float conversion has no source bounds")?,
            )?),
            TryConversionKind::FloatToFloat => {
                // Infinity round-trips but is deliberately not an exact
                // conversion. x - x is zero precisely for finite x.
                let difference = self.emit_typed(
                    Provenance::Synthesized,
                    &target_ty,
                    SemOpKind::Binary {
                        op: BinaryOp::Subtract,
                        lhs: Operand { value: converted },
                        rhs: Operand { value: converted },
                    },
                )?;
                let zero = self.emit_typed(
                    Provenance::Synthesized,
                    &target_ty,
                    SemOpKind::ConstFloat(0.0),
                )?;
                let finite = self.numeric_compare(BinaryOp::Equal, difference, zero)?;
                failures.push(self.branch_candidate_test(finite)?);
            }
            TryConversionKind::IntToInt | TryConversionKind::FloatToInt => {}
        }
        if *kind != TryConversionKind::IntToInt {
            let roundtrip = self.numeric_cast(converted, &source_ty)?;
            let exact = self.numeric_compare(BinaryOp::Equal, value, roundtrip)?;
            failures.push(self.branch_candidate_test(exact)?);
        }
        self.finish_try_cast(expr, converted, failures)
    }

    fn finish_try_cast(
        &mut self,
        expr: &HirExpr,
        converted: ValueId,
        failures: Vec<ControlState>,
    ) -> Result<ValueId, String> {
        let result_ty = self.ty(&expr.ty);
        let shape = self.service.require_variant_shape(&result_ty)?;
        let descriptor = &self.service.variant_shapes[shape.0 as usize];
        let variant = |name| {
            descriptor
                .variants
                .iter()
                .position(|v| v.name == name)
                .and_then(|index| u32::try_from(index).ok())
                .ok_or_else(|| format!("checked conversion Option shape has no {name} variant"))
        };
        let some = variant("Some")?;
        let none = variant("None")?;
        let present = self.emit(
            expr,
            SemOpKind::VariantMake {
                shape,
                variant: some,
                fields: vec![Operand { value: converted }],
            },
        )?;
        if failures.is_empty() {
            return Ok(present);
        }
        let success = self.numeric_exit(present);
        self.merge_control_states(failures)?;
        let absent = self.emit(
            expr,
            SemOpKind::VariantMake {
                shape,
                variant: none,
                fields: Vec::new(),
            },
        )?;
        let failure = self.numeric_exit(absent);
        self.merge_match_exits(vec![success, failure], &result_ty)?
            .ok_or_else(|| "checked conversion has no result".into())
    }
}
