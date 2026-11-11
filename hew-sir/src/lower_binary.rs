//! Binary operator chains.
//!
//! A HIR chain applies its steps in order, each to the value of the steps
//! before it. Each step lowers exactly as the equivalent nested expression
//! did: its own checked-overflow edge, its own short-circuit branch and its
//! own temporaries.

use crate::SirLoweringError;

use super::{
    Builder, Edge, HirExpr, HirExprKind, MatchExit, Operand, ResolvedTy, SemOpKind, SemTerminator,
    ValueId,
};
use hew_hir::{HirBinaryResult, HirBinaryStep};
use hew_parser::ast::{BinaryOp, UnaryOp};

/// The left operand of one step.
#[derive(Clone, Copy)]
enum StepLeft<'a> {
    /// The chain's first operand, not yet lowered.
    Expr(&'a HirExpr),
    /// The value of the steps before this one, with its facts.
    Value(ValueId, &'a HirExpr),
}

impl StepLeft<'_> {
    fn ty(&self) -> &ResolvedTy {
        match self {
            Self::Expr(expr) | Self::Value(_, expr) => &expr.ty,
        }
    }
}

/// An intermediate step's value as an expression shell. The operation
/// helpers it is passed to read only its type and site.
fn step_value(result: &HirBinaryResult) -> HirExpr {
    HirExpr {
        node: result.node,
        site: result.site,
        ty: result.ty.clone(),
        intent: hew_hir::IntentKind::Read,
        kind: HirExprKind::Unsupported(String::new()),
        span: result.span.clone(),
    }
}

impl Builder<'_, '_> {
    pub(super) fn lower_binary_chain(
        &mut self,
        expr: &HirExpr,
        first: &HirExpr,
        steps: &[HirBinaryStep],
    ) -> Result<ValueId, SirLoweringError> {
        // Every intermediate value began where the chain begins, so the
        // loans its operands open end from the same floor.
        let loan_floor = self.scope_loans.len();
        let mut previous: Option<(ValueId, HirExpr)> = None;
        for step in steps {
            let shell = step.result.as_ref().map(step_value);
            let whole = shell.as_ref().unwrap_or(expr);
            let left = match &previous {
                None => StepLeft::Expr(first),
                Some((value, facts)) => StepLeft::Value(*value, facts),
            };
            let value = self.lower_binary_step(whole, step.op, left, &step.right)?;
            let Some(shell) = shell else {
                return Ok(value);
            };
            let value = self.finish_expression_value(&shell, value, loan_floor)?;
            previous = Some((value, shell));
        }
        Err("a binary chain has at least one step".into())
    }

    fn lower_step_left(
        &mut self,
        left: StepLeft<'_>,
        context: &str,
    ) -> Result<Operand, SirLoweringError> {
        match left {
            StepLeft::Expr(expr) => self.lower_read_operand(expr, context),
            StepLeft::Value(value, _) => Ok(Operand { value }),
        }
    }

    /// A runtime operation over a step's operands. A left value from an
    /// earlier step is passed as it stands and released after the call, as
    /// the argument temporary it would have been.
    fn lower_binary_runtime(
        &mut self,
        whole: &HirExpr,
        family: hew_types::RuntimeCallFamily,
        left: StepLeft<'_>,
        right: &HirExpr,
    ) -> Result<Option<ValueId>, SirLoweringError> {
        match left {
            StepLeft::Expr(left) => {
                self.lower_runtime_operation(whole, family, &[left, right], true)
            }
            StepLeft::Value(value, facts) => self.lower_runtime_operation_with_temporaries(
                whole,
                family,
                (&[facts, right], &[]),
                true,
                &[(0, value)],
                &[value],
            ),
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "one operator dispatch over the operand types"
    )]
    fn lower_binary_step(
        &mut self,
        whole: &HirExpr,
        op: BinaryOp,
        left: StepLeft<'_>,
        right: &HirExpr,
    ) -> Result<ValueId, SirLoweringError> {
        match op {
            BinaryOp::And => return self.lower_logical_step(whole, left, right, false),
            BinaryOp::Or => return self.lower_logical_step(whole, left, right, true),
            _ => {}
        }
        let left_ty = self.ty(left.ty());
        if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual)
            && matches!(
                left_ty,
                ResolvedTy::Bytes
                    | ResolvedTy::Tuple(_)
                    | ResolvedTy::Named { .. }
                    | ResolvedTy::Unit
            )
        {
            let StepLeft::Expr(left) = left else {
                return Err("an aggregate comparison's left operand is never a chain value".into());
            };
            let equals =
                self.lower_value_comparison(whole, [left, right], hew_types::ValueCapability::Eq)?;
            return if op == BinaryOp::NotEqual {
                self.emit(
                    whole,
                    SemOpKind::Unary {
                        op: UnaryOp::Not,
                        value: Operand { value: equals },
                    },
                )
            } else {
                Ok(equals)
            };
        }
        let ordering = match op {
            BinaryOp::Less => Some(hew_types::ValueCapability::Less),
            BinaryOp::LessEqual => Some(hew_types::ValueCapability::LessEqual),
            BinaryOp::Greater => Some(hew_types::ValueCapability::Greater),
            BinaryOp::GreaterEqual => Some(hew_types::ValueCapability::GreaterEqual),
            _ => None,
        };
        if let Some(capability) = ordering {
            if matches!(
                left_ty,
                ResolvedTy::Tuple(_)
                    | ResolvedTy::Named {
                        head: hew_types::TypeHead::Builtin(
                            hew_types::BuiltinType::Option | hew_types::BuiltinType::Result
                        ),
                        ..
                    }
                    | ResolvedTy::Named { .. }
            ) {
                let StepLeft::Expr(left) = left else {
                    return Err(
                        "an aggregate ordering's left operand is never a chain value".into(),
                    );
                };
                return self.lower_value_comparison(whole, [left, right], capability);
            }
        }
        if op == BinaryOp::Add && left_ty == ResolvedTy::Bytes {
            return self
                .lower_binary_runtime(
                    whole,
                    hew_types::RuntimeCallFamily::BytesConcat,
                    left,
                    right,
                )?
                .ok_or_else(|| {
                    SirLoweringError::from("bytes concatenation must produce a value".to_string())
                });
        }
        if left_ty == ResolvedTy::String {
            return match op {
                BinaryOp::Add => self
                    .lower_binary_runtime(
                        whole,
                        hew_types::RuntimeCallFamily::StringConcat,
                        left,
                        right,
                    )?
                    .ok_or_else(|| {
                        SirLoweringError::from(
                            "string concatenation must produce a value".to_string(),
                        )
                    }),
                BinaryOp::Equal | BinaryOp::NotEqual => {
                    let equals = self
                        .lower_binary_runtime(
                            whole,
                            hew_types::RuntimeCallFamily::StringEquals,
                            left,
                            right,
                        )?
                        .ok_or_else(|| {
                            SirLoweringError::from(
                                "string comparison must produce a value".to_string(),
                            )
                        })?;
                    if op == BinaryOp::NotEqual {
                        self.emit(
                            whole,
                            SemOpKind::Unary {
                                op: UnaryOp::Not,
                                value: Operand { value: equals },
                            },
                        )
                    } else {
                        Ok(equals)
                    }
                }
                BinaryOp::Less
                | BinaryOp::LessEqual
                | BinaryOp::Greater
                | BinaryOp::GreaterEqual => {
                    let mut comparison = whole.clone();
                    comparison.ty = ResolvedTy::I32;
                    let ordering = self
                        .lower_binary_runtime(
                            &comparison,
                            hew_types::RuntimeCallFamily::StringCompare,
                            left,
                            right,
                        )?
                        .ok_or(SirLoweringError::from(
                            "string ordering must produce a value",
                        ))?;
                    let zero = self.emit_typed(
                        crate::Provenance::Site(whole.site),
                        &ResolvedTy::I32,
                        SemOpKind::ConstInteger(0),
                    )?;
                    self.emit(
                        whole,
                        SemOpKind::Binary {
                            op,
                            lhs: Operand { value: ordering },
                            rhs: Operand { value: zero },
                        },
                    )
                }
                _ => Err(
                    format!("string binary `{op}` has no ownership-SIR runtime operation").into(),
                ),
            };
        }
        let lhs = self.lower_step_left(left, "binary left operand")?;
        let rhs = self.lower_read_operand(right, "binary right operand")?;
        if crate::checked_binary_failure_kinds(op, &self.ty(&whole.ty)).is_some() {
            self.lower_checked_binary(whole, op, lhs, rhs)
        } else {
            self.emit(whole, SemOpKind::Binary { op, lhs, rhs })
        }
    }

    /// Lower short-circuit `&&` and `||` as CFG rather than an eager binary
    /// operation. The short-circuit edge materialises the result while the
    /// other edge alone evaluates the right-hand side, so effectful operations
    /// on the right stay structurally guarded.
    fn lower_logical_step(
        &mut self,
        whole: &HirExpr,
        left: StepLeft<'_>,
        right: &HirExpr,
        short_circuit_value: bool,
    ) -> Result<ValueId, SirLoweringError> {
        let result_ty = self.ty(&whole.ty);
        if result_ty != ResolvedTy::Bool {
            return Err(
                "short-circuit logical expressions must have bool type in SIR"
                    .to_string()
                    .into(),
            );
        }
        let condition = self.lower_step_left(left, "logical condition")?;
        let evaluate_right = self.new_block(Vec::new());
        let short_circuit = self.new_block(Vec::new());
        let (then_target, else_target) = if short_circuit_value {
            (short_circuit, evaluate_right)
        } else {
            (evaluate_right, short_circuit)
        };
        self.set_terminator(SemTerminator::Branch {
            condition,
            then_target: Edge {
                target: then_target,
                args: Vec::new(),
            },
            else_target: Edge {
                target: else_target,
                args: Vec::new(),
            },
        })?;

        let before = self.control_state();
        let mut exits = Vec::new();
        // Both operands are conditional paths: a loan may not end inside one.
        self.branch_depth += 1;

        self.current = evaluate_right;
        let loan_floor = self.scope_loans.len();
        let right_value = self.lower_read_operand(right, "logical right value")?;
        if self.is_open() {
            // The right operand runs on one edge only. Its temporaries and
            // interior loans end on that edge, never after the join, where the
            // short-circuit edge never created them.
            if self.scope_loans.len() > loan_floor {
                let loans = self.scope_loans.split_off(loan_floor);
                self.end_call_loans(&loans)?;
            }
            self.destroy_live_since(&before.owned_live)?;
            exits.push(MatchExit {
                state: self.control_state(),
                result: Some(right_value),
            });
        }

        self.restore_control_state(&before);
        self.current = short_circuit;
        let constant = self.emit(whole, SemOpKind::ConstBool(short_circuit_value))?;
        exits.push(MatchExit {
            state: self.control_state(),
            result: Some(Operand { value: constant }),
        });

        self.branch_depth -= 1;
        self.merge_match_exits(exits, &result_ty)?.ok_or_else(|| {
            SirLoweringError::from(
                "short-circuit logical expression produced no SSA value".to_string(),
            )
        })
    }
}
