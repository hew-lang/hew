//! Binary operator chain lowering.
//!
//! An AST chain `first op1 r1 op2 r2 …` lowers to one HIR chain whose
//! intermediate steps carry the facts the nested form gave each prefix
//! expression: its site, node, checked type and span. A prefix the checker
//! wraps (a numeric widening, a `dyn` coercion, a user comparison call)
//! becomes the first operand of a new chain, so the HIR nests only where a
//! conversion sits between two steps.

use super::*;
use crate::node::{HirBinaryResult, HirBinaryStep};
use hew_parser::ast::BinaryStep;

/// The value of a chain lowered so far: `first` and its steps, each of
/// which carries its own result.
struct ChainValue {
    first: HirExpr,
    steps: Vec<HirBinaryStep>,
}

impl ChainValue {
    fn start(first: HirExpr) -> Self {
        Self {
            first,
            steps: Vec::new(),
        }
    }

    /// The value as one expression: the last step's result becomes the
    /// enclosing expression's facts.
    fn into_expr(mut self) -> HirExpr {
        let Some(result) = self.steps.last_mut().and_then(|step| step.result.take()) else {
            return self.first;
        };
        HirExpr {
            node: result.node,
            site: result.site,
            ty: result.ty,
            intent: IntentKind::Read,
            kind: HirExprKind::Binary {
                first: Box::new(self.first),
                steps: self.steps,
            },
            span: result.span,
        }
    }
}

/// An operand of a binary step in source form: an expression, or the
/// prefix of a chain.
#[derive(Clone, Copy)]
pub(super) enum BinaryOperand<'a> {
    Expr(&'a Spanned<Expr>),
    Prefix {
        first: &'a Spanned<Expr>,
        steps: &'a [BinaryStep],
    },
}

impl<'a> BinaryOperand<'a> {
    /// The left operand of the last step of `first` and `steps`.
    pub(super) fn last_left(first: &'a Spanned<Expr>, steps: &'a [BinaryStep]) -> Self {
        match steps.len() {
            0 | 1 => Self::Expr(first),
            count => Self::Prefix {
                first,
                steps: &steps[..count - 1],
            },
        }
    }

    pub(super) fn span(self) -> &'a Span {
        match self {
            Self::Expr(expr) => &expr.1,
            Self::Prefix { steps, .. } => &steps[steps.len() - 1].span,
        }
    }
}

impl LowerCtx<'_> {
    /// [`Self::lower_expr`] of a binary operand.
    pub(super) fn lower_binary_operand(
        &mut self,
        operand: BinaryOperand<'_>,
        intent: IntentKind,
    ) -> HirExpr {
        let (first, steps) = match operand {
            BinaryOperand::Expr(expr) => return self.lower_expr(expr, intent),
            BinaryOperand::Prefix { first, steps } => (first, steps),
        };
        let span = operand.span().clone();
        let site = self.source_site(&span);
        if let Some(operations) = self
            .checked_indexed_place_operations
            .get(&self.mk_key(&span))
        {
            self.indexed_place_operations.insert(site, *operations);
        }
        let lowered = match self.lower_binary_chain(first, steps, site, intent, &span) {
            Ok((kind, ty)) => self.finish_inner_expr(kind, ty, site, intent, span.clone()),
            Err(done) => *done,
        };
        let lowered = self.apply_tail_coercion(lowered, &span);
        self.finish_lowered_expr(lowered, &span, intent)
    }

    /// Lower `first` and `steps`, whose value has the already allocated
    /// `site`. `Ok` is the kind and type of an ordinary last step, finished
    /// by the caller like any expression; `Err` is a finished expression.
    pub(super) fn lower_binary_chain(
        &mut self,
        first: &Spanned<Expr>,
        steps: &[BinaryStep],
        site: SiteId,
        intent: IntentKind,
        span: &Span,
    ) -> Result<(HirExprKind, ResolvedTy), Box<HirExpr>> {
        let count = steps.len();
        // Each intermediate value's site precedes its operands', outermost
        // first, as when every prefix was its own expression.
        let mut sites = vec![site; count];
        for index in (0..count.saturating_sub(1)).rev() {
            let prefix_span = &steps[index].span;
            let prefix_site = self.source_site(prefix_span);
            if let Some(operations) = self
                .checked_indexed_place_operations
                .get(&self.mk_key(prefix_span))
            {
                self.indexed_place_operations
                    .insert(prefix_site, *operations);
            }
            sites[index] = prefix_site;
        }
        let mut chain = ChainValue::start(self.lower_expr(first, IntentKind::Read));
        for (index, step) in steps.iter().enumerate() {
            let last = index + 1 == count;
            let step_span = if last { span } else { &step.span };
            let step_intent = if last { intent } else { IntentKind::Read };
            let right = self.lower_expr(&step.right, IntentKind::Read);
            let finished = match self.lower_binary_step(
                chain,
                step.op,
                right,
                sites[index],
                step_intent,
                step_span,
            ) {
                StepOutcome::Chained(value) if last => {
                    return Ok((
                        HirExprKind::Binary {
                            first: Box::new(value.first),
                            steps: value.steps,
                        },
                        value.ty,
                    ));
                }
                StepOutcome::Chained(value) => {
                    let kind = HirExprKind::Binary {
                        first: Box::new(value.first),
                        steps: value.steps,
                    };
                    self.finish_inner_expr(
                        kind,
                        value.ty,
                        sites[index],
                        IntentKind::Read,
                        step_span.clone(),
                    )
                }
                StepOutcome::Finished(done) if last => return Err(Box::new(done)),
                StepOutcome::Finished(done) => done,
            };
            // The value is the next step's left operand: apply the facts the
            // checker keyed to its span, and keep chaining when none wraps it.
            let node = finished.node;
            let finished = self.apply_tail_coercion(finished, step_span);
            let finished = self.finish_lowered_expr(finished, step_span, IntentKind::Read);
            chain = if finished.node == node && matches!(finished.kind, HirExprKind::Binary { .. })
            {
                let finished_site = finished.site;
                let finished_span = finished.span.clone();
                match finished.into_parts() {
                    (HirExprKind::Binary { first, mut steps }, ty) => {
                        if let Some(step) = steps.last_mut() {
                            step.result = Some(HirBinaryResult {
                                node,
                                site: finished_site,
                                ty,
                                span: finished_span,
                            });
                        }
                        ChainValue {
                            first: *first,
                            steps,
                        }
                    }
                    _ => unreachable!("the unchanged value is a binary chain"),
                }
            } else {
                ChainValue::start(finished)
            };
        }
        unreachable!("a binary chain has at least one step")
    }

    /// Apply one operator to the chain so far.
    fn lower_binary_step(
        &mut self,
        chain: ChainValue,
        op: BinaryOp,
        right: HirExpr,
        site: SiteId,
        intent: IntentKind,
        span: &Span,
    ) -> StepOutcome {
        // D340: a user `impl Eq`/`impl Ord`/`impl PartialOrd` for the
        // operand type overrides the compiler's structural default —
        // dispatch to the resolved impl method instead of the structural
        // comparison built below.
        let dispatch_key = self.mk_key(span);
        if let Some(dispatch) = self.user_comparison_dispatch.get(&dispatch_key).cloned() {
            let left = chain.into_expr();
            let call =
                self.lower_user_comparison_dispatch(&dispatch, op, left, right, span.clone());
            let (kind, ty) = call.into_parts();
            return StepOutcome::Finished(HirExpr {
                node: self.ids.node(),
                site,
                ty,
                intent,
                kind,
                span: span.clone(),
            });
        }
        let Some(checked_ty) = self.checked_expr_type(&dispatch_key) else {
            return StepOutcome::Finished(
                self.unsupported_expr(span.clone(), "binary expression has no checked type"),
            );
        };
        let Ok(ty) = ResolvedTy::from_ty(&checked_ty) else {
            return StepOutcome::Finished(
                self.unsupported_expr(span.clone(), "binary expression type is unresolved"),
            );
        };
        if matches!(op, BinaryOp::Range | BinaryOp::RangeInclusive) {
            let start = chain.into_expr();
            return StepOutcome::Finished(self.make_expr(
                HirExprKind::RangeValue {
                    start: Box::new(start),
                    end: Box::new(right),
                    inclusive: op == BinaryOp::RangeInclusive,
                },
                ty,
                intent,
                span.clone(),
            ));
        }
        let ChainValue { first, mut steps } = chain;
        steps.push(HirBinaryStep {
            op,
            right,
            result: None,
        });
        StepOutcome::Chained(ChainedStep { first, steps, ty })
    }
}

enum StepOutcome {
    /// The step extends the chain; its value has type `ty`.
    Chained(ChainedStep),
    /// The step produced a finished expression.
    Finished(HirExpr),
}

struct ChainedStep {
    first: HirExpr,
    steps: Vec<HirBinaryStep>,
    ty: ResolvedTy,
}
