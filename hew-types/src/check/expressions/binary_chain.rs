//! Binary operator chains.
//!
//! The parser folds a left-associative run of operators at one precedence
//! into one [`Expr::Binary`]: `a + b - c` is `first: a` with the steps
//! `+ b` and `- c`. Each step's value is the prefix ending at that step, with
//! the span the left-deep nesting gave it, so every span-keyed fact of an
//! intermediate value keeps its key. Checking walks the steps in order and
//! produces the facts the nested form produced, without recursing per step.

use super::super::{BinaryOp, Expr, Span, Spanned};
use super::{BuiltinType, Checker, SpanKey, Ty};
use hew_parser::ast::BinaryStep;

/// The left operand of one step of a binary chain.
#[derive(Clone, Copy)]
pub(in crate::check) enum BinaryLeft<'a> {
    /// The chain's first operand.
    Operand(&'a Spanned<Expr>),
    /// The value of the steps before this one, already checked as `ty`.
    Prefix {
        first: &'a Spanned<Expr>,
        steps: &'a [BinaryStep],
        ty: &'a Ty,
    },
}

impl<'a> BinaryLeft<'a> {
    /// The left operand of `steps[index]`, whose prefix value has type `ty`.
    fn at(first: &'a Spanned<Expr>, steps: &'a [BinaryStep], index: usize, ty: &'a Ty) -> Self {
        if index == 0 {
            Self::Operand(first)
        } else {
            Self::Prefix {
                first,
                steps: &steps[..index],
                ty,
            }
        }
    }

    /// The source operand, when the left side is the chain's first operand.
    pub(in crate::check) fn operand(self) -> Option<&'a Spanned<Expr>> {
        match self {
            Self::Operand(operand) => Some(operand),
            Self::Prefix { .. } => None,
        }
    }

    pub(in crate::check) fn span(self) -> &'a Span {
        match self {
            Self::Operand(operand) => &operand.1,
            Self::Prefix { steps, .. } => {
                &steps
                    .last()
                    .expect("a binary chain prefix has at least one step")
                    .span
            }
        }
    }
}

/// Integer arithmetic and bitwise operators: the operators whose value takes
/// the contextual integer width.
pub(in crate::check) fn integer_arithmetic_op(op: BinaryOp) -> bool {
    matches!(
        op,
        BinaryOp::Add
            | BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo
            | BinaryOp::WrappingAdd
            | BinaryOp::WrappingSub
            | BinaryOp::WrappingMul
            | BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr
    )
}

impl Checker {
    pub(in crate::check) fn synthesize_binary_left(&mut self, left: BinaryLeft<'_>) -> Ty {
        match left {
            BinaryLeft::Operand(operand) => self.synthesize(&operand.0, &operand.1),
            BinaryLeft::Prefix { ty, .. } => ty.clone(),
        }
    }

    /// Synthesize each step in order. Every intermediate value is recorded
    /// and published at its prefix span; the last step's type is returned
    /// for the caller to record and publish. With `step_tys`, each step's
    /// operator result is collected.
    pub(in crate::check) fn synthesize_binary_steps(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        mut step_tys: Option<&mut Vec<Ty>>,
    ) -> Ty {
        let mut prefix_ty = Ty::Error;
        for (index, step) in steps.iter().enumerate() {
            let left = BinaryLeft::at(first, steps, index, &prefix_ty);
            let ty = self.check_binary_op(left, step.op, &step.right, &step.span);
            if let Some(step_tys) = step_tys.as_deref_mut() {
                step_tys.push(ty.clone());
            }
            if index + 1 == steps.len() {
                return ty;
            }
            self.record_type(&step.span, &ty);
            prefix_ty = self.publish_binary_value(first, &steps[..=index], ty);
        }
        Ty::Error
    }

    /// `synthesize` of the value of `first` and `steps`.
    fn synthesize_binary_value(&mut self, first: &Spanned<Expr>, steps: &[BinaryStep]) -> Ty {
        let ty = self.synthesize_binary_steps(first, steps, None);
        if let Some(last) = steps.last() {
            self.record_type(&last.span, &ty);
        }
        self.publish_binary_value(first, steps, ty)
    }

    /// Publish the checked value of `first` and `steps` at the last step's
    /// span: the facts `publish_checked_expression` keeps for a binary form.
    pub(in crate::check) fn publish_binary_value(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        result: Ty,
    ) -> Ty {
        let Some((last, init)) = steps.split_last() else {
            return result;
        };
        let span = &last.span;
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .entry(key.clone())
            .or_insert_with(|| self.current_module.clone());
        self.expr_types
            .entry(key.clone())
            .or_insert_with(|| result.clone());
        let left_ty = match init.last() {
            None => self.callee_value_type(first),
            Some(previous) => self
                .expr_types
                .get(&SpanKey::in_module(&previous.span, self.current_module_idx))
                .cloned(),
        };
        self.record_binary_expression_effect(span, last.op, left_ty);
        self.check_receiver_whole_at_binary(span, last.op, &result);
        self.expression_value_candidates.remove(&key);
        let candidates = self.binary_value_candidates(span);
        self.expression_value_candidates.insert(key, candidates);
        result
    }

    /// `check_against` of a chain prefix, including its publication.
    pub(in crate::check) fn check_prefix_against(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        expected: &Ty,
    ) -> Ty {
        let expected = self.normalize_for_use(expected);
        let result = self.check_binary_against(first, steps, &expected);
        self.publish_binary_value(first, steps, result)
    }

    /// `check_against_inner` of a binary chain against a normalized
    /// expected type. The arm is chosen by the operator that produces the
    /// chain's value.
    pub(in crate::check) fn check_binary_against(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        expected: &Ty,
    ) -> Ty {
        let Some(last) = steps.last() else {
            return Ty::Error;
        };
        let span = &last.span;
        match (last.op, expected) {
            // Range literal `lo..hi` or `lo..=hi` with a known expected
            // `Range<T>` type: check the bounds directly against the element
            // type so they are recorded with the right concrete integer width.
            (
                BinaryOp::Range | BinaryOp::RangeInclusive,
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Range),
                    args,
                    ..
                },
            ) if steps.len() == 1
                && args.len() == 1
                && !matches!(&args[0], Ty::Error | Ty::Var(_)) =>
            {
                let elem_ty = args[0].clone();
                self.check_against(&first.0, &first.1, &elem_ty);
                self.check_against(&last.right.0, &last.right.1, &elem_ty);
                let range_ty = Ty::range(elem_ty);
                self.record_type(span, &range_ty);
                range_ty
            }
            (op, ty) if integer_arithmetic_op(op) && ty.is_integer() => {
                self.check_integer_chain_against(first, steps, expected)
            }
            // A join where `bytes` is expected checks both sides as `bytes`,
            // so `[1] + [2]` is a buffer there too.
            (BinaryOp::Add, Ty::Bytes) => self.check_bytes_chain_against(first, steps, expected),
            _ => {
                let actual = self.synthesize_binary_value(first, steps);
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }
        }
    }

    /// Integer arithmetic against a contextual integer type. The chain is
    /// synthesized once; while its value is still an integer literal, the
    /// literal prefixes beneath it are settled to the contextual width from
    /// the innermost out, so every leaf and intermediate value of literal
    /// arithmetic takes that width.
    fn check_integer_chain_against(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        expected: &Ty,
    ) -> Ty {
        let count = steps.len();
        let mut step_tys = Vec::with_capacity(count);
        let actual = self.synthesize_binary_steps(first, steps, Some(&mut step_tys));
        let span = &steps[count - 1].span;
        let actual_resolved = self.subst.resolve(&actual);
        if !actual_resolved.is_integer_literal() {
            return self.settle_integer_step(span, actual, expected);
        }
        // `level` counts the steps of the prefix being settled; its left
        // operand is settled before its right one and its own span.
        let mut level = count;
        loop {
            if level == 1 {
                let key = SpanKey::in_module(&first.1, self.current_module_idx);
                let first_ty = self.expr_types[&key].clone();
                self.record_concrete_integer_operand(expected, first, &first_ty);
                break;
            }
            let prefix = &steps[..level - 1];
            let prefix_span = &prefix[prefix.len() - 1].span;
            let key = SpanKey::in_module(prefix_span, self.current_module_idx);
            let left_ty = self.expr_types[&key].clone();
            let literal_arithmetic = self.subst.resolve(&left_ty).is_integer_literal()
                && integer_arithmetic_op(prefix[prefix.len() - 1].op);
            if !literal_arithmetic {
                let left = BinaryLeft::Prefix {
                    first,
                    steps: prefix,
                    ty: &left_ty,
                };
                self.record_concrete_integer_left(expected, left, &left_ty);
                break;
            }
            let prefix_actual = step_tys[level - 2].clone();
            if !self.subst.resolve(&prefix_actual).is_integer_literal() {
                let result = self.settle_integer_step(prefix_span, prefix_actual, expected);
                self.publish_binary_value(first, prefix, result);
                break;
            }
            level -= 1;
        }
        for settled in level..=count {
            let step = &steps[settled - 1];
            let key = SpanKey::in_module(&step.right.1, self.current_module_idx);
            let right_ty = self.expr_types[&key].clone();
            self.record_concrete_integer_operand(expected, &step.right, &right_ty);
            self.record_type(&step.span, expected);
            if settled < count {
                self.publish_binary_value(first, &steps[..settled], expected.clone());
            }
        }
        expected.clone()
    }

    /// A non-literal integer step checked against its contextual type.
    fn settle_integer_step(&mut self, span: &Span, actual: Ty, expected: &Ty) -> Ty {
        let actual_resolved = self.subst.resolve(&actual);
        if matches!(actual_resolved, Ty::Never | Ty::Error) {
            return actual_resolved;
        }
        let n = self.errors.len();
        self.expect_type(expected, &actual, span);
        if self.errors.len() > n {
            Ty::Error
        } else {
            self.record_type(span, &actual);
            actual
        }
    }

    /// `+` where `bytes` is expected checks each operand as `bytes`. The run
    /// of `+` ends at the last other operator, whose prefix is checked whole.
    fn check_bytes_chain_against(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        expected: &Ty,
    ) -> Ty {
        let start = steps
            .iter()
            .rposition(|step| step.op != BinaryOp::Add)
            .map_or(0, |index| index + 1);
        let mut left_ty = if start == 0 {
            self.check_against(&first.0, &first.1, expected)
        } else {
            self.check_prefix_against(first, &steps[..start], expected)
        };
        for (index, step) in steps.iter().enumerate().skip(start) {
            let right_ty = self.check_against(&step.right.0, &step.right.1, expected);
            let result = if matches!(left_ty, Ty::Error) || matches!(right_ty, Ty::Error) {
                Ty::Error
            } else {
                self.record_type(&step.span, expected);
                expected.clone()
            };
            left_ty = if index + 1 < steps.len() {
                self.publish_binary_value(first, &steps[..=index], result)
            } else {
                result
            };
        }
        left_ty
    }

    /// [`Self::record_concrete_integer_operand`] for a step's left operand.
    pub(in crate::check) fn record_concrete_integer_left(
        &mut self,
        common_ty: &Ty,
        left: BinaryLeft<'_>,
        left_ty: &Ty,
    ) {
        let (first, steps) = match left {
            BinaryLeft::Operand(operand) => {
                self.record_concrete_integer_operand(common_ty, operand, left_ty);
                return;
            }
            BinaryLeft::Prefix { first, steps, .. } => (first, steps),
        };
        let span = left.span();
        let resolved = self.subst.resolve(left_ty);
        if resolved.is_integer() && !resolved.is_integer_literal() && resolved != *common_ty {
            self.numeric_operand_coercions.insert(
                SpanKey::in_module(span, self.current_module_idx),
                common_ty.clone(),
            );
        }
        if resolved.is_integer_literal() || matches!(resolved, Ty::Var(_)) {
            let literal_arithmetic = resolved.is_integer_literal()
                && steps
                    .last()
                    .is_some_and(|step| integer_arithmetic_op(step.op));
            if literal_arithmetic {
                self.check_prefix_against(first, steps, common_ty);
            } else {
                self.promote_literal_binding(left_ty, common_ty);
                self.expect_type(common_ty, left_ty, span);
                self.record_type(span, common_ty);
            }
        }
    }
}
