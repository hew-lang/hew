//! Constexpr sub-engine for integer module constants and machine const-generic
//! arguments (W3.039 Stage 2.5).
//!
//! Per R268=B (USER OVERRIDE), const-arguments at machine instantiation
//! sites may be constexpr-evaluable expressions, not just bare integer
//! literals — `M<FOO * 4>` is legal when `FOO` resolves to a
//! module-level integer `const`.
//!
//! This module is intentionally narrow. The supported expression shapes
//! are:
//!
//! - `Expr::Literal(Literal::Integer { value, .. })` — signed parser integer
//!   literals.
//! - `Expr::Unary { op: UnaryOp::Negate, .. }` — target-typed integer
//!   negation.
//! - `Expr::Identifier(name)` — resolved against the supplied
//!   [`ConstEnv`] (module-level `const` bindings of integer type).
//! - `Expr::Binary { op, lhs, rhs }` for the seven arithmetic and
//!   shift operators `+`, `-`, `*`, `/`, `%`, `<<`, `>>`. Every operation
//!   is evaluated in the declared integer target's range. Arithmetic overflow,
//!   division by zero, and an out-of-range final value are distinct errors.
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

/// Module-level constexpr environment threaded into the integer evaluators.
///
/// Values deliberately use `i128`: the parser admits `i64` literals while
/// arithmetic may need one wider signed carrier to distinguish a typed
/// arithmetic overflow from a literal that is simply outside its declaration.
/// The machine-const wrapper below still exposes its historical `u64`/`usize`
/// contract unchanged.
#[derive(Debug, Default, Clone)]
pub struct ConstEnv {
    values: HashMap<String, i128>,
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

    pub fn insert(&mut self, name: impl Into<String>, value: i128) {
        self.values.insert(name.into(), value);
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<i128> {
        self.values.get(name).copied()
    }
}

/// Failure mode of integer constant evaluation.
///
/// `NotConstant` is the broad-spectrum rejection used for any
/// expression shape outside the allow-list. `Overflow` remains the legacy
/// machine-`usize` wrapper result. Module-constant callers receive the three
/// precise arithmetic/value classes below instead of a lowering-stage NYI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstEvalError {
    /// The expression shape is not in the constexpr allow-list (e.g.
    /// a function call, a method invocation, a struct literal).
    NotConstant,
    /// Legacy machine-`usize` wrapper failure. It preserves the old
    /// const-generic contract: negative, out-of-range, arithmetic-overflow,
    /// and division-by-zero inputs all reject as the existing `usize` failure.
    Overflow,
    /// A target-typed arithmetic operation overflowed its declared integer
    /// domain.
    ArithmeticOverflow,
    /// A target-typed division or remainder used zero as its divisor.
    DivisionByZero,
    /// The completed value does not fit in the declared integer type.
    OutOfRange,
    /// An identifier reference did not resolve in the [`ConstEnv`].
    UnknownConst(String),
}

/// Declared integer target for [`eval_integer_const_expr`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstIntegerTarget {
    min: i128,
    max: i128,
}

impl ConstIntegerTarget {
    /// Build a target from a checker-resolved integer type. `ptr_width` is a
    /// compilation target fact, never the host width, so `isize`/`usize` stay
    /// correct under WASM cross-compilation.
    #[must_use]
    pub fn from_resolved_ty(ty: &crate::ResolvedTy, ptr_width: u8) -> Option<Self> {
        use crate::ResolvedTy;

        #[allow(
            clippy::match_same_arms,
            reason = "platform-sized arms intentionally state the target-width contract beside fixed-width types"
        )]
        let (min, max) = match ty {
            ResolvedTy::I8 => (i128::from(i8::MIN), i128::from(i8::MAX)),
            ResolvedTy::I16 => (i128::from(i16::MIN), i128::from(i16::MAX)),
            ResolvedTy::I32 => (i128::from(i32::MIN), i128::from(i32::MAX)),
            ResolvedTy::I64 => (i128::from(i64::MIN), i128::from(i64::MAX)),
            ResolvedTy::U8 => (0, i128::from(u8::MAX)),
            ResolvedTy::U16 => (0, i128::from(u16::MAX)),
            ResolvedTy::U32 => (0, i128::from(u32::MAX)),
            ResolvedTy::U64 => (0, i128::from(u64::MAX)),
            ResolvedTy::Isize if ptr_width == 32 => (i128::from(i32::MIN), i128::from(i32::MAX)),
            ResolvedTy::Isize => (i128::from(i64::MIN), i128::from(i64::MAX)),
            ResolvedTy::Usize if ptr_width == 32 => (0, i128::from(u32::MAX)),
            ResolvedTy::Usize => (0, i128::from(u64::MAX)),
            _ => return None,
        };
        Some(Self { min, max })
    }

    const fn machine_usize_compat() -> Self {
        Self {
            min: 0,
            max: u64::MAX as i128,
        }
    }

    const fn contains(self, value: i128) -> bool {
        value >= self.min && value <= self.max
    }
}

/// Evaluate `expr` under the historical machine-`usize` const-generic
/// contract. Do not use this entry point for module constants: it deliberately
/// retains the `u64` result and single `Overflow` class expected by existing
/// machine generic checks.
///
/// # Errors
///
/// Returns the historical [`ConstEvalError::Overflow`] class for every
/// machine-`usize` range/arithmetic failure and preserves the other evaluator
/// failures unchanged.
pub fn eval_const_expr(expr: &Spanned<Expr>, env: &ConstEnv) -> Result<u64, ConstEvalError> {
    let value = eval_integer_const_expr(expr, env, ConstIntegerTarget::machine_usize_compat())
        .map_err(machine_const_error)?;
    u64::try_from(value).map_err(|_| ConstEvalError::Overflow)
}

fn machine_const_error(error: ConstEvalError) -> ConstEvalError {
    match error {
        ConstEvalError::ArithmeticOverflow
        | ConstEvalError::DivisionByZero
        | ConstEvalError::OutOfRange => ConstEvalError::Overflow,
        other => other,
    }
}

/// Evaluate a module-constant expression for its declared integer target.
///
/// The result carrier is `i128` so callers can range-check values without
/// reinterpreting signed bits. Parser literals are currently `i64`, making
/// `i128` sufficient for every supported operation plus an exact target range
/// comparison.
///
/// # Errors
///
/// Returns a precise arithmetic-overflow, division-by-zero, out-of-range,
/// unknown-const, or unsupported-expression class.
pub fn eval_integer_const_expr(
    expr: &Spanned<Expr>,
    env: &ConstEnv,
    target: ConstIntegerTarget,
) -> Result<i128, ConstEvalError> {
    let value = eval_inner(&expr.0, env, target)?;
    if target.contains(value) {
        Ok(value)
    } else {
        Err(ConstEvalError::OutOfRange)
    }
}

/// Evaluate one supported expression shape in the shared `i128` carrier.
/// Target range checks happen after every arithmetic operation and once at the
/// outer expression boundary; there is no panic path for well-formed inputs.
fn eval_inner(
    expr: &Expr,
    env: &ConstEnv,
    target: ConstIntegerTarget,
) -> Result<i128, ConstEvalError> {
    #[allow(
        clippy::match_same_arms,
        reason = "explicit variants preserve exhaustive-traversal posture (LESSONS)"
    )]
    match expr {
        Expr::Literal(Literal::Integer { value, .. }) => Ok(i128::from(*value)),
        Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } => {
            // Walk first so `-foo()` stays `NotConstant`. Check this operation
            // in the target domain immediately: this still admits the signed
            // minimum spelling `-128: i8`, while rejecting `-(-128)` before an
            // enclosing operation can mask the overflow.
            let inner = eval_inner(&operand.0, env, target)?;
            let value = inner
                .checked_neg()
                .ok_or(ConstEvalError::ArithmeticOverflow)?;
            if target.contains(value) {
                Ok(value)
            } else {
                Err(ConstEvalError::ArithmeticOverflow)
            }
        }
        Expr::Identifier(name) => env
            .get(name)
            .ok_or_else(|| ConstEvalError::UnknownConst(name.clone())),
        Expr::Binary { left, op, right } => {
            let l = eval_inner(&left.0, env, target)?;
            let r = eval_inner(&right.0, env, target)?;
            eval_binary(*op, l, r, target)
        }
        // Every other Expr shape is rejected. The match is intentionally
        // exhaustive over the remaining variants so a future Expr variant
        // fails the build here rather than being silently treated as
        // NotConstant.
        Expr::Clone(_)
        | Expr::Literal(_)
        | Expr::ContextVariant(_)
        | Expr::GenericApplySuffix { .. }
        | Expr::RecordInitSuffix { .. }
        | Expr::QualifiedAssoc(_)
        | Expr::Tuple(_)
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
        | Expr::ScopeDeadline { .. }
        | Expr::InterpolatedString(_)
        | Expr::Call { .. }
        | Expr::MethodCall { .. }
        | Expr::StructInit { .. }
        | Expr::Select { .. }
        | Expr::Join(_)
        | Expr::Timeout { .. }
        | Expr::UnsafeBlock(_)
        | Expr::Yield(_)
        | Expr::Return(_)
        | Expr::This
        | Expr::FieldAccess { .. }
        | Expr::Index { .. }
        | Expr::Cast { .. }
        | Expr::PostfixTry(_)
        | Expr::Range { .. }
        | Expr::Await(_)
        | Expr::AwaitRestart(_)
        | Expr::RegexLiteral(_)
        | Expr::ByteStringLiteral(_)
        | Expr::ByteArrayLiteral(_)
        | Expr::Is { .. }
        | Expr::MachineEmit { .. }
        | Expr::GenBlock { .. }
        | Expr::Unary { .. } => Err(ConstEvalError::NotConstant),
    }
}

fn eval_binary(
    op: BinaryOp,
    l: i128,
    r: i128,
    target: ConstIntegerTarget,
) -> Result<i128, ConstEvalError> {
    let value = match op {
        BinaryOp::Add => l.checked_add(r).ok_or(ConstEvalError::ArithmeticOverflow)?,
        BinaryOp::Subtract => l.checked_sub(r).ok_or(ConstEvalError::ArithmeticOverflow)?,
        BinaryOp::Multiply => l.checked_mul(r).ok_or(ConstEvalError::ArithmeticOverflow)?,
        BinaryOp::Divide => {
            if r == 0 {
                return Err(ConstEvalError::DivisionByZero);
            }
            l.checked_div(r).ok_or(ConstEvalError::ArithmeticOverflow)?
        }
        BinaryOp::Modulo => {
            if r == 0 {
                return Err(ConstEvalError::DivisionByZero);
            }
            l.checked_rem(r).ok_or(ConstEvalError::ArithmeticOverflow)?
        }
        BinaryOp::Shl => {
            let shift = u32::try_from(r).map_err(|_| ConstEvalError::ArithmeticOverflow)?;
            l.checked_shl(shift)
                .ok_or(ConstEvalError::ArithmeticOverflow)?
        }
        BinaryOp::Shr => {
            let shift = u32::try_from(r).map_err(|_| ConstEvalError::ArithmeticOverflow)?;
            l.checked_shr(shift)
                .ok_or(ConstEvalError::ArithmeticOverflow)?
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
        | BinaryOp::WrappingMul => return Err(ConstEvalError::NotConstant),
    };
    if target.contains(value) {
        Ok(value)
    } else {
        Err(ConstEvalError::ArithmeticOverflow)
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

    fn target(ty: &crate::ResolvedTy) -> ConstIntegerTarget {
        ConstIntegerTarget::from_resolved_ty(ty, 64).expect("integer target")
    }

    #[test]
    fn signed_module_const_binary_unary_and_parenthesized_forms_agree() {
        let env = ConstEnv::new();
        let binary = bin(BinaryOp::Subtract, int(0), int(1));
        let unary = span(Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(int(1)),
        });
        let parenthesized = span(Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(bin(BinaryOp::Add, int(0), int(1))),
        });
        for expr in [&binary, &unary, &parenthesized] {
            assert_eq!(
                eval_integer_const_expr(expr, &env, target(&crate::ResolvedTy::I64)),
                Ok(-1)
            );
        }
    }

    #[test]
    fn module_const_target_reports_range_overflow_and_division_separately() {
        let env = ConstEnv::new();
        assert_eq!(
            eval_integer_const_expr(&int(256), &env, target(&crate::ResolvedTy::U8)),
            Err(ConstEvalError::OutOfRange)
        );
        assert_eq!(
            eval_integer_const_expr(
                &bin(BinaryOp::Subtract, int(0), int(1)),
                &env,
                target(&crate::ResolvedTy::U8),
            ),
            Err(ConstEvalError::ArithmeticOverflow)
        );
        assert_eq!(
            eval_integer_const_expr(
                &bin(BinaryOp::Add, int(i8::MAX.into()), int(1)),
                &env,
                target(&crate::ResolvedTy::I8),
            ),
            Err(ConstEvalError::ArithmeticOverflow)
        );
        assert_eq!(
            eval_integer_const_expr(
                &bin(BinaryOp::Divide, int(1), int(0)),
                &env,
                target(&crate::ResolvedTy::I64),
            ),
            Err(ConstEvalError::DivisionByZero)
        );
    }

    #[test]
    fn module_const_integer_width_boundaries_accept_exact_limits() {
        let env = ConstEnv::new();
        for (ty, min, max) in [
            (
                crate::ResolvedTy::I8,
                i128::from(i8::MIN),
                i128::from(i8::MAX),
            ),
            (
                crate::ResolvedTy::I16,
                i128::from(i16::MIN),
                i128::from(i16::MAX),
            ),
            (
                crate::ResolvedTy::I32,
                i128::from(i32::MIN),
                i128::from(i32::MAX),
            ),
            (
                crate::ResolvedTy::I64,
                i128::from(i64::MIN),
                i128::from(i64::MAX),
            ),
            (crate::ResolvedTy::U8, 0, i128::from(u8::MAX)),
            (crate::ResolvedTy::U16, 0, i128::from(u16::MAX)),
            (crate::ResolvedTy::U32, 0, i128::from(u32::MAX)),
        ] {
            let target = target(&ty);
            let min = i64::try_from(min).expect("fixed-width boundary fits parser literal");
            let max = i64::try_from(max).expect("fixed-width boundary fits parser literal");
            assert_eq!(
                eval_integer_const_expr(&int(min), &env, target),
                Ok(i128::from(min))
            );
            assert_eq!(
                eval_integer_const_expr(&int(max), &env, target),
                Ok(i128::from(max))
            );
        }
    }

    #[test]
    fn module_const_nested_unary_negation_rejects_intermediate_overflow() {
        let triple_negation = span(Expr::Unary {
            op: UnaryOp::Negate,
            operand: Box::new(span(Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(span(Expr::Unary {
                    op: UnaryOp::Negate,
                    operand: Box::new(int(128)),
                })),
            })),
        });

        assert_eq!(
            eval_integer_const_expr(
                &triple_negation,
                &ConstEnv::new(),
                target(&crate::ResolvedTy::I8),
            ),
            Err(ConstEvalError::ArithmeticOverflow)
        );
    }
}
