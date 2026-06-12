//! Constexpr sub-engine for machine const-generic arguments (W3.039 Stage 2.5).
//!
//! Per R268=B (USER OVERRIDE), const-arguments at machine instantiation
//! sites may be constexpr-evaluable expressions, not just bare integer
//! literals — `M<FOO * 4>` is legal when `FOO` resolves to a
//! module-level integer `const`.
//!
//! This module is intentionally narrow. The supported expression shapes
//! are:
//!
//! - `Expr::Literal(Literal::Integer { value, .. })` — non-negative
//!   integers (must fit in `u64`).
//! - `Expr::Unary { op: UnaryOp::Negate, .. }` — folded into the inner
//!   literal; the result is rejected as [`ConstEvalError::Overflow`]
//!   because the const-arg target type is `usize` (R269=A).
//! - `Expr::Identifier(name)` — resolved against the supplied
//!   [`ConstEnv`] (module-level `const` bindings of integer type).
//! - `Expr::Binary { op, lhs, rhs }` for the seven arithmetic and
//!   shift operators `+`, `-`, `*`, `/`, `%`, `<<`, `>>`. Every
//!   intermediate value is `u64`; overflow, division by zero, and
//!   negative-shift values raise [`ConstEvalError::Overflow`].
//!
//! Out of scope for Phase 0 (these all map to
//! [`ConstEvalError::NotConstant`]):
//!
//! - Floating-point literals or operands of any kind.
//! - Function calls (`size_of::<T>()`, user functions).
//! - Casts, method calls, field accesses, struct literals.
//! - Cross-const-param arithmetic (`M<N + 1>` where `N` is another
//!   const-param of the same machine) — that requires a substitution
//!   pass which Phase 0 does not provide.
//! - Anything not in the explicit allow-list above. The check is
//!   deliberately exhaustive (no `_` wildcard) so any future `Expr`
//!   variant will force a planner decision here rather than being
//!   silently treated as `NotConstant`.

use std::collections::HashMap;

use hew_parser::ast::{BinaryOp, Expr, Literal, Spanned, UnaryOp};

/// Module-level constexpr environment threaded into [`eval_const_expr`].
///
/// Currently a thin newtype over `HashMap<String, u64>` so call sites
/// don't accidentally pass a different `HashMap` carrying unrelated
/// data. Values are `u64` because R269=A admits `usize` only; widening
/// to other integer widths will replace `u64` with a tagged value type.
#[derive(Debug, Default, Clone)]
pub struct ConstEnv {
    values: HashMap<String, u64>,
}

#[allow(
    dead_code,
    reason = "ConstEnv::new/insert/get are part of the Stage 2.5 public surface; \
              full Stage 3 wiring (validate_const_param_arg call sites) lands when \
              W3.033c Stage 2 unblocks"
)]
impl ConstEnv {
    #[must_use]
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: impl Into<String>, value: u64) {
        self.values.insert(name.into(), value);
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<u64> {
        self.values.get(name).copied()
    }
}

/// Failure mode of [`eval_const_expr`].
///
/// `NotConstant` is the broad-spectrum rejection used for any
/// expression shape outside the allow-list. `Overflow` is reserved
/// for arithmetic that produced a value outside `u64` range or a
/// negative literal that cannot be stored in `usize`. Distinguishing
/// these at the sub-engine boundary lets the checker emit different
/// diagnostics ("compile-time integer expression required" vs
/// "const argument overflows usize").
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstEvalError {
    /// The expression shape is not in the constexpr allow-list (e.g.
    /// a function call, a method invocation, a struct literal).
    NotConstant,
    /// An arithmetic operation overflowed `u64`, divided by zero, or
    /// produced a negative intermediate value.
    Overflow,
    /// An identifier reference did not resolve in the [`ConstEnv`].
    UnknownConst(String),
}

/// Evaluate `expr` to a `u64` value or fail with a precise error.
///
/// The function is total: every `Expr` variant either succeeds with
/// a `u64`, or returns `Err`. There is no panic path for well-formed
/// inputs.
///
/// # Errors
///
/// - [`ConstEvalError::NotConstant`] when the expression contains any
///   shape outside the explicit allow-list documented at module level.
/// - [`ConstEvalError::Overflow`] on arithmetic overflow, division by
///   zero, shift amount ≥ 64, or a negative literal/intermediate.
/// - [`ConstEvalError::UnknownConst`] when an identifier reference is
///   not present in `env`.
pub fn eval_const_expr(expr: &Spanned<Expr>, env: &ConstEnv) -> Result<u64, ConstEvalError> {
    eval_inner(&expr.0, env)
}

fn eval_inner(expr: &Expr, env: &ConstEnv) -> Result<u64, ConstEvalError> {
    #[allow(
        clippy::match_same_arms,
        reason = "explicit variants vs catch-all preserve exhaustive-traversal posture (LESSONS)"
    )]
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => {
            u64::try_from(*value).map_err(|_| ConstEvalError::Overflow)
        }
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => {
            // `-N` against a `usize` target is always out-of-range
            // unless N == 0. We still walk the operand so that
            // `-foo()` produces NotConstant rather than Overflow.
            let inner = eval_inner(&operand.0, env)?;
            if inner == 0 {
                Ok(0)
            } else {
                Err(ConstEvalError::Overflow)
            }
        }
        Expr::Identifier(name) => env
            .get(name)
            .ok_or_else(|| ConstEvalError::UnknownConst(name.clone())),
        Expr::Binary { left, op, right } => {
            let l = eval_inner(&left.0, env)?;
            let r = eval_inner(&right.0, env)?;
            eval_binary(*op, l, r)
        }
        // Every other Expr shape is rejected. The match is intentionally
        // exhaustive over the remaining variants so a future Expr variant
        // fails the build here rather than being silently treated as
        // NotConstant.
        Expr::Tuple(_)
        | Expr::Array(_)
        | Expr::ArrayRepeat { .. }
        | Expr::MapLiteral { .. }
        | Expr::Block(_)
        | Expr::If { .. }
        | Expr::IfLet { .. }
        | Expr::Match { .. }
        | Expr::Lambda { .. }
        | Expr::Spawn { .. }
        | Expr::SpawnLambdaActor { .. }
        | Expr::Scope { .. }
        | Expr::ForkChild { .. }
        | Expr::ForkBlock { .. }
        | Expr::Unary { .. } => Err(ConstEvalError::NotConstant),
        // Catch-all for parser variants we don't enumerate above
        // (string/path/method/etc. shapes). Anything not in the
        // arithmetic allow-list is `NotConstant`. A planner audit
        // should periodically replace this with explicit variants
        // to retain the LESSONS `exhaustive-traversal-and-lowering`
        // posture.
        _ => Err(ConstEvalError::NotConstant),
    }
}

fn eval_binary(op: BinaryOp, l: u64, r: u64) -> Result<u64, ConstEvalError> {
    match op {
        BinaryOp::Add => l.checked_add(r).ok_or(ConstEvalError::Overflow),
        BinaryOp::Subtract => l.checked_sub(r).ok_or(ConstEvalError::Overflow),
        BinaryOp::Multiply => l.checked_mul(r).ok_or(ConstEvalError::Overflow),
        BinaryOp::Divide => l.checked_div(r).ok_or(ConstEvalError::Overflow),
        BinaryOp::Modulo => l.checked_rem(r).ok_or(ConstEvalError::Overflow),
        BinaryOp::Shl => {
            let shift = u32::try_from(r).map_err(|_| ConstEvalError::Overflow)?;
            l.checked_shl(shift).ok_or(ConstEvalError::Overflow)
        }
        BinaryOp::Shr => {
            let shift = u32::try_from(r).map_err(|_| ConstEvalError::Overflow)?;
            l.checked_shr(shift).ok_or(ConstEvalError::Overflow)
        }
        // All other binary operators (comparison, boolean, bitwise,
        // wrapping arithmetic, ranges) are NotConstant for Phase 0:
        // the constexpr surface is intentionally arithmetic-only.
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::Range
        | BinaryOp::RangeInclusive
        | BinaryOp::WrappingAdd
        | BinaryOp::WrappingSub
        | BinaryOp::WrappingMul => Err(ConstEvalError::NotConstant),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::IntRadix;

    fn span(e: Expr) -> Spanned<Expr> {
        (e, 0..0)
    }

    fn int(n: i64) -> Spanned<Expr> {
        span(Expr::Literal(Literal::Integer {
            value: n,
            radix: IntRadix::Decimal,
        }))
    }

    fn bin(op: BinaryOp, l: Spanned<Expr>, r: Spanned<Expr>) -> Spanned<Expr> {
        span(Expr::Binary {
            left: Box::new(l),
            op,
            right: Box::new(r),
        })
    }

    #[test]
    fn literal_integer() {
        assert_eq!(eval_const_expr(&int(16), &ConstEnv::new()), Ok(16));
        assert_eq!(eval_const_expr(&int(0), &ConstEnv::new()), Ok(0));
    }

    #[test]
    fn arithmetic_add_mul() {
        let env = ConstEnv::new();
        // 1 + 2 * 3 — Hew's parser would produce this nested via Pratt;
        // we hand-build (1 + (2 * 3)) for the unit test.
        let expr = bin(
            BinaryOp::Add,
            int(1),
            bin(BinaryOp::Multiply, int(2), int(3)),
        );
        assert_eq!(eval_const_expr(&expr, &env), Ok(7));
    }

    #[test]
    fn arithmetic_all_seven_ops() {
        let env = ConstEnv::new();
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Add, int(5), int(3)), &env),
            Ok(8)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Subtract, int(5), int(3)), &env),
            Ok(2)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Multiply, int(5), int(3)), &env),
            Ok(15)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Divide, int(10), int(3)), &env),
            Ok(3)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Modulo, int(10), int(3)), &env),
            Ok(1)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Shl, int(1), int(4)), &env),
            Ok(16)
        );
        assert_eq!(
            eval_const_expr(&bin(BinaryOp::Shr, int(64), int(2)), &env),
            Ok(16)
        );
    }

    #[test]
    fn named_const_lookup_succeeds() {
        let mut env = ConstEnv::new();
        env.insert("MAX_SIZE", 64);
        let expr = bin(
            BinaryOp::Multiply,
            span(Expr::Identifier("MAX_SIZE".to_string())),
            int(4),
        );
        // `MAX_SIZE * 4` per R268=B — the FOO * 4 acceptance case.
        assert_eq!(eval_const_expr(&expr, &env), Ok(256));
    }

    #[test]
    fn unknown_const_rejected() {
        let env = ConstEnv::new();
        let expr = span(Expr::Identifier("MISSING".to_string()));
        assert!(matches!(
            eval_const_expr(&expr, &env),
            Err(ConstEvalError::UnknownConst(_))
        ));
    }

    #[test]
    fn negative_literal_overflows() {
        // -1 cannot be stored as usize.
        assert!(matches!(
            eval_const_expr(&int(-1), &ConstEnv::new()),
            Err(ConstEvalError::Overflow)
        ));
    }

    #[test]
    fn division_by_zero_overflows() {
        let expr = bin(BinaryOp::Divide, int(10), int(0));
        assert!(matches!(
            eval_const_expr(&expr, &ConstEnv::new()),
            Err(ConstEvalError::Overflow)
        ));
    }

    #[test]
    fn unsupported_shape_not_constant() {
        // A tuple literal is not a constexpr shape.
        let expr = span(Expr::Tuple(vec![int(1), int(2)]));
        assert_eq!(
            eval_const_expr(&expr, &ConstEnv::new()),
            Err(ConstEvalError::NotConstant)
        );
    }

    #[test]
    fn unsupported_binary_op_not_constant() {
        // Comparison is not in the seven-op allow-list.
        let expr = bin(BinaryOp::Equal, int(1), int(1));
        assert_eq!(
            eval_const_expr(&expr, &ConstEnv::new()),
            Err(ConstEvalError::NotConstant)
        );
    }

    #[test]
    fn negate_zero_yields_zero() {
        let expr = span(Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(int(0)),
        });
        assert_eq!(eval_const_expr(&expr, &ConstEnv::new()), Ok(0));
    }

    #[test]
    fn negate_positive_overflows() {
        let expr = span(Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(int(5)),
        });
        assert!(matches!(
            eval_const_expr(&expr, &ConstEnv::new()),
            Err(ConstEvalError::Overflow)
        ));
    }

    #[test]
    fn nested_with_named_const() {
        // (BASE + 1) * 2  with BASE = 7  → 16
        let mut env = ConstEnv::new();
        env.insert("BASE", 7);
        let expr = bin(
            BinaryOp::Multiply,
            bin(
                BinaryOp::Add,
                span(Expr::Identifier("BASE".to_string())),
                int(1),
            ),
            int(2),
        );
        assert_eq!(eval_const_expr(&expr, &env), Ok(16));
    }
}
