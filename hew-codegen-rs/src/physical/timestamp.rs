//! Exact realization of checked wall-clock arithmetic.
//!
//! The semantic value remains a normalized seconds/nanoseconds pair. LLVM i128
//! temporaries hold the entire mathematical result, so only the result's own
//! range can select SIR's overflow edge; intermediate i64 arithmetic cannot.

use super::*;

const NANOS_PER_SECOND: i128 = 1_000_000_000;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    fn timestamp_exact_nanos(&self, storage: StorageId) -> CodegenResult<IntValue<'ctx>> {
        let wide = self.ctx.i128_type();
        let value = self.load(storage, "timestamp.operand")?;
        if self.storage(storage)?.ty == ResolvedTy::Duration {
            return self
                .builder
                .build_int_s_extend(value.into_int_value(), wide, "timestamp.duration")
                .llvm_ctx("widen duration for timestamp arithmetic");
        }
        let pair = value.into_struct_value();
        let seconds = self
            .builder
            .build_extract_value(pair, 0, "timestamp.seconds")
            .llvm_ctx("read timestamp seconds")?
            .into_int_value();
        let nanos = self
            .builder
            .build_extract_value(pair, 1, "timestamp.nanoseconds")
            .llvm_ctx("read timestamp nanoseconds")?
            .into_int_value();
        let seconds = self
            .builder
            .build_int_s_extend(seconds, wide, "timestamp.wide.seconds")
            .llvm_ctx("widen signed timestamp seconds")?;
        let nanos = self
            .builder
            .build_int_z_extend(nanos, wide, "timestamp.wide.nanoseconds")
            .llvm_ctx("widen timestamp fraction")?;
        let whole = self
            .builder
            .build_int_mul(
                seconds,
                wide.const_int(1_000_000_000, false),
                "timestamp.whole",
            )
            .llvm_ctx("scale timestamp seconds exactly")?;
        self.builder
            .build_int_add(whole, nanos, "timestamp.exact")
            .llvm_ctx("combine timestamp fields exactly")
    }

    pub(super) fn emit_checked_timestamp(
        &self,
        op: BinaryOp,
        lhs: StorageId,
        rhs: StorageId,
        result: StorageId,
        normal: &PhysicalEdge,
        failures: &[PhysicalCheckedFailure],
    ) -> CodegenResult<()> {
        let left = self.timestamp_exact_nanos(lhs)?;
        let right = self.timestamp_exact_nanos(rhs)?;
        let exact = match op {
            BinaryOp::Add => self.builder.build_int_add(left, right, "timestamp.sum"),
            BinaryOp::Subtract => self
                .builder
                .build_int_sub(left, right, "timestamp.difference"),
            _ => {
                return Err(CodegenError::FailClosed(
                    "invalid checked timestamp operation".into(),
                ))
            }
        }
        .llvm_ctx("compute exact timestamp arithmetic")?;
        let timestamp_result = self
            .storage(result)?
            .ty
            .is_builtin(hew_types::BuiltinType::Timestamp);
        let (minimum, maximum) = if timestamp_result {
            (
                i128::from(i64::MIN) * NANOS_PER_SECOND,
                i128::from(i64::MAX) * NANOS_PER_SECOND + NANOS_PER_SECOND - 1,
            )
        } else {
            (i128::from(i64::MIN), i128::from(i64::MAX))
        };
        let wide = exact.get_type();
        #[expect(
            clippy::cast_possible_truncation,
            reason = "LLVM i128 constants use two low-to-high u64 limbs"
        )]
        let constant = |value: i128| {
            let bits = value.cast_unsigned();
            wide.const_int_arbitrary_precision(&[bits as u64, (bits >> 64) as u64])
        };
        let below = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                exact,
                constant(minimum),
                "timestamp.below",
            )
            .llvm_ctx("check lower timestamp result bound")?;
        let above = self
            .builder
            .build_int_compare(
                IntPredicate::SGT,
                exact,
                constant(maximum),
                "timestamp.above",
            )
            .llvm_ctx("check upper timestamp result bound")?;
        let overflow = self
            .builder
            .build_or(below, above, "timestamp.overflow")
            .llvm_ctx("join timestamp range checks")?;
        let failure = &failures
            .iter()
            .find(|failure| failure.kind == TrapKind::IntegerOverflow)
            .ok_or_else(|| {
                CodegenError::FailClosed("timestamp arithmetic lacks its overflow edge".into())
            })?
            .edge;
        if !timestamp_result {
            let duration = self
                .builder
                .build_int_truncate(exact, self.ctx.i64_type(), "timestamp.duration.result")
                .llvm_ctx("realize checked timestamp difference")?;
            return self.emit_checked_choice(
                overflow,
                duration,
                result,
                normal,
                failure,
                "timestamp",
            );
        }
        let pair = self.normalized_timestamp_value(exact, result)?;
        self.emit_checked_choice(overflow, pair, result, normal, failure, "timestamp")
    }

    fn normalized_timestamp_value(
        &self,
        exact: IntValue<'ctx>,
        result: StorageId,
    ) -> CodegenResult<StructValue<'ctx>> {
        let wide = exact.get_type();
        let billion = wide.const_int(1_000_000_000, false);
        let quotient = self
            .builder
            .build_int_signed_div(exact, billion, "timestamp.quotient")
            .llvm_ctx("split timestamp seconds")?;
        let remainder = self
            .builder
            .build_int_signed_rem(exact, billion, "timestamp.remainder")
            .llvm_ctx("split timestamp fraction")?;
        let negative = self
            .builder
            .build_int_compare(
                IntPredicate::SLT,
                remainder,
                wide.const_zero(),
                "timestamp.negative.fraction",
            )
            .llvm_ctx("test timestamp normalization carry")?;
        let carry = self
            .builder
            .build_int_z_extend(negative, wide, "timestamp.borrow")
            .llvm_ctx("materialize timestamp normalization carry")?;
        let seconds = self
            .builder
            .build_int_sub(quotient, carry, "timestamp.normalized.seconds")
            .llvm_ctx("floor timestamp seconds")?;
        let adjustment = self
            .builder
            .build_select(
                negative,
                billion,
                wide.const_zero(),
                "timestamp.fraction.adjustment",
            )
            .llvm_ctx("select timestamp fraction adjustment")?
            .into_int_value();
        let nanos = self
            .builder
            .build_int_add(remainder, adjustment, "timestamp.normalized.nanoseconds")
            .llvm_ctx("normalize timestamp fraction")?;
        let seconds = self
            .builder
            .build_int_truncate(seconds, self.ctx.i64_type(), "timestamp.seconds.result")
            .llvm_ctx("realize timestamp seconds")?;
        let nanos = self
            .builder
            .build_int_truncate(nanos, self.ctx.i32_type(), "timestamp.nanoseconds.result")
            .llvm_ctx("realize timestamp fraction")?;
        let pair = llvm_type(self.ctx, &self.storage(result)?.layout.repr)?
            .into_struct_type()
            .const_zero();
        let pair = self
            .builder
            .build_insert_value(pair, seconds, 0, "timestamp.with.seconds")
            .llvm_ctx("store timestamp seconds")?
            .into_struct_value();
        let pair = self
            .builder
            .build_insert_value(pair, nanos, 1, "timestamp.result")
            .llvm_ctx("store timestamp fraction")?
            .into_struct_value();
        Ok(pair)
    }
}
