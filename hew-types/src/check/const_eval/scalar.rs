//! Scalar constant operations consume checked operand types and resolved values.

use super::{eval_binary, ConstEvalError, ConstIntegerTarget};
use crate::compile_time::ConstantValue;
use crate::ResolvedTy;
use hew_parser::ast::{BinaryOp, BinaryStep, Expr, Literal, Span, Spanned, UnaryOp};

/// Evaluate scalar literal structure without invoking source functions.
///
/// The callback supplies checker operand coercions and declaration-resolved
/// constants. Aggregate folding and the declaration dependency graph remain
/// with their existing owner.
///
/// # Errors
/// Returns the shared constant arithmetic errors or `NotConstant` for a
/// non-scalar expression not provided by the callback.
pub fn eval_scalar_const_expr(
    expr: &Spanned<Expr>,
    expected: &ResolvedTy,
    pointer_width: u8,
    facts: &mut impl FnMut(&Span, Option<&Expr>) -> (Option<ResolvedTy>, Option<ConstantValue>),
) -> Result<ConstantValue, ConstEvalError> {
    hew_parser::stack::ensure_sufficient_stack(|| {
        eval_scalar_const_expr_inner(expr, expected, pointer_width, facts)
    })
}

fn eval_scalar_const_expr_inner(
    expr: &Spanned<Expr>,
    expected: &ResolvedTy,
    pointer_width: u8,
    facts: &mut impl FnMut(&Span, Option<&Expr>) -> (Option<ResolvedTy>, Option<ConstantValue>),
) -> Result<ConstantValue, ConstEvalError> {
    let (ty, value) = facts(&expr.1, Some(&expr.0));
    let ty = ty.as_ref().unwrap_or(expected);
    if let Some(value) = value {
        return Ok(coerce_scalar(value, ty));
    }
    let value = match &expr.0 {
        Expr::Literal(literal) => match literal {
            Literal::Integer { value, .. } => coerce_scalar(ConstantValue::Integer(*value), ty),
            Literal::Float(value) => ConstantValue::Float(round_float(*value, ty)),
            Literal::Duration(value) => ConstantValue::Duration(*value),
            Literal::Bool(value) => ConstantValue::Bool(*value),
            Literal::Char(value) => ConstantValue::Char(*value),
            Literal::String(value) => ConstantValue::String(value.clone()),
        },
        Expr::ByteStringLiteral(bytes) | Expr::ByteArrayLiteral(bytes) => {
            ConstantValue::Bytes(bytes.clone())
        }
        Expr::Unary { op, operand } => scalar_unary(*op, operand, ty, pointer_width, facts)?,
        Expr::Binary { first, steps } => {
            return eval_scalar_chain(first, steps, ty, pointer_width, facts);
        }
        Expr::Ident(name) => return Err(ConstEvalError::UnknownConst(name.to_string())),
        Expr::FieldAccess { object, field } => {
            match eval_scalar_const_expr(object, ty, pointer_width, facts)? {
                ConstantValue::Record(fields) => fields
                    .into_iter()
                    .find(|(name, _)| name == field.0.name.as_str())
                    .map(|(_, value)| value.value)
                    .ok_or(ConstEvalError::NotConstant)?,
                ConstantValue::Tuple(fields) => field
                    .0
                    .name
                    .as_str()
                    .parse::<usize>()
                    .ok()
                    .and_then(|index| fields.get(index))
                    .map(|value| value.value.clone())
                    .ok_or(ConstEvalError::NotConstant)?,
                _ => return Err(ConstEvalError::NotConstant),
            }
        }
        Expr::Index { object, index } => {
            let object = eval_scalar_const_expr(object, ty, pointer_width, facts)?;
            let index = eval_scalar_const_expr(index, &ResolvedTy::I64, pointer_width, facts)?;
            let ConstantValue::Integer(index) = index else {
                return Err(ConstEvalError::NotConstant);
            };
            let index = usize::try_from(index).map_err(|_| ConstEvalError::OutOfRange)?;
            match object {
                ConstantValue::Array(values) | ConstantValue::Vector(values) => values
                    .get(index)
                    .map(|value| value.value.clone())
                    .ok_or(ConstEvalError::OutOfRange)?,
                ConstantValue::Bytes(values) => ConstantValue::Integer(i128::from(
                    *values.get(index).ok_or(ConstEvalError::OutOfRange)?,
                )),
                ConstantValue::String(value) => {
                    ConstantValue::Char(value.chars().nth(index).ok_or(ConstEvalError::OutOfRange)?)
                }
                _ => return Err(ConstEvalError::NotConstant),
            }
        }
        _ => return Err(ConstEvalError::NotConstant),
    };
    finish_scalar(value, ty, pointer_width)
}

/// Give an operation's value its checked type, refusing an integer outside it.
fn finish_scalar(
    value: ConstantValue,
    ty: &ResolvedTy,
    pointer_width: u8,
) -> Result<ConstantValue, ConstEvalError> {
    let value = coerce_scalar(value, ty);
    if let ConstantValue::Integer(value) = value {
        if let Some(target) = ConstIntegerTarget::from_resolved_ty(ty, pointer_width) {
            if !target.contains(value) {
                return Err(ConstEvalError::OutOfRange);
            }
        }
    }
    Ok(value)
}

/// Evaluate a binary chain left to right. Each intermediate value takes the
/// checked type of its prefix span, defaulting to the type of the value it
/// feeds, and a prefix the checker already folded is used as it stands.
fn eval_scalar_chain(
    first: &Spanned<Expr>,
    steps: &[BinaryStep],
    ty: &ResolvedTy,
    pointer_width: u8,
    facts: &mut impl FnMut(&Span, Option<&Expr>) -> (Option<ResolvedTy>, Option<ConstantValue>),
) -> Result<ConstantValue, ConstEvalError> {
    let Some(last) = steps.len().checked_sub(1) else {
        return Err(ConstEvalError::NotConstant);
    };
    let mut start = 0;
    let mut seeded = None;
    let mut step_tys = vec![ty.clone()];
    for index in (0..last).rev() {
        let (prefix_ty, prefix_value) = facts(&steps[index].span, None);
        let prefix_ty = prefix_ty.unwrap_or_else(|| step_tys[step_tys.len() - 1].clone());
        if let Some(value) = prefix_value {
            seeded = Some(coerce_scalar(value, &prefix_ty));
            start = index + 1;
            break;
        }
        step_tys.push(prefix_ty);
    }
    step_tys.reverse();
    let mut value = match seeded {
        Some(value) => value,
        None => eval_scalar_const_expr(first, &step_tys[0], pointer_width, facts)?,
    };
    for (offset, step) in steps[start..].iter().enumerate() {
        let index = start + offset;
        let step_ty = &step_tys[offset];
        if matches!(
            (step.op, &value),
            (BinaryOp::And, ConstantValue::Bool(false)) | (BinaryOp::Or, ConstantValue::Bool(true))
        ) {
            continue;
        }
        let right = eval_scalar_const_expr(&step.right, step_ty, pointer_width, facts)?;
        let left_span = index
            .checked_sub(1)
            .map_or(&first.1, |previous| &steps[previous].span);
        let operand_ty = facts(left_span, None).0.unwrap_or_else(|| step_ty.clone());
        let combined = scalar_binary(step.op, value, right, step_ty, &operand_ty, pointer_width)?;
        value = finish_scalar(combined, step_ty, pointer_width)?;
    }
    Ok(value)
}

fn scalar_unary(
    op: UnaryOp,
    operand: &Spanned<Expr>,
    ty: &ResolvedTy,
    pointer_width: u8,
    facts: &mut impl FnMut(&Span, Option<&Expr>) -> (Option<ResolvedTy>, Option<ConstantValue>),
) -> Result<ConstantValue, ConstEvalError> {
    // A signed minimum's positive magnitude is outside its type. Its
    // negation is one literal operation, as in the integer evaluator.
    let value = if let (UnaryOp::Negate, Expr::Literal(Literal::Integer { value, .. })) =
        (op, &operand.0)
    {
        ConstantValue::Integer(
            value
                .checked_neg()
                .ok_or(ConstEvalError::ArithmeticOverflow)?,
        )
    } else {
        let value = eval_scalar_const_expr(operand, ty, pointer_width, facts)?;
        match (op, value) {
            (UnaryOp::Negate, ConstantValue::Integer(value)) => ConstantValue::Integer(
                value
                    .checked_neg()
                    .ok_or(ConstEvalError::ArithmeticOverflow)?,
            ),
            (UnaryOp::Negate, ConstantValue::Float(value)) => {
                ConstantValue::Float(round_float(-value, ty))
            }
            (UnaryOp::Negate, ConstantValue::Duration(value)) => ConstantValue::Duration(
                value
                    .checked_neg()
                    .ok_or(ConstEvalError::ArithmeticOverflow)?,
            ),
            (UnaryOp::Not, ConstantValue::Bool(value)) => ConstantValue::Bool(!value),
            (UnaryOp::BitNot, ConstantValue::Integer(value)) => {
                let target = ConstIntegerTarget::from_resolved_ty(ty, pointer_width)
                    .ok_or(ConstEvalError::NotConstant)?;
                ConstantValue::Integer(target.wrap(!value))
            }
            _ => return Err(ConstEvalError::NotConstant),
        }
    };
    if let ConstantValue::Integer(integer) = value {
        if ConstIntegerTarget::from_resolved_ty(ty, pointer_width)
            .is_some_and(|target| !target.contains(integer))
        {
            return Err(ConstEvalError::ArithmeticOverflow);
        }
    }
    Ok(value)
}

#[allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    reason = "checked numeric operand coercions explicitly convert integers to floating point"
)]
fn coerce_scalar(value: ConstantValue, ty: &ResolvedTy) -> ConstantValue {
    match value {
        ConstantValue::Integer(value) if *ty == ResolvedTy::F32 => {
            ConstantValue::Float(f64::from(value as f32))
        }
        ConstantValue::Integer(value) if *ty == ResolvedTy::F64 => {
            ConstantValue::Float(value as f64)
        }
        ConstantValue::Float(value) => ConstantValue::Float(round_float(value, ty)),
        value => value,
    }
}

#[allow(
    clippy::cast_possible_truncation,
    reason = "f32 constant operations round at the same boundary as runtime f32 operations"
)]
fn round_float(value: f64, ty: &ResolvedTy) -> f64 {
    if *ty == ResolvedTy::F32 {
        f64::from(value as f32)
    } else {
        value
    }
}

fn compare<T: PartialOrd + Copy>(op: BinaryOp, left: T, right: T) -> Option<bool> {
    Some(match op {
        BinaryOp::Equal => left == right,
        BinaryOp::NotEqual => left != right,
        BinaryOp::Less => left < right,
        BinaryOp::LessEqual => left <= right,
        BinaryOp::Greater => left > right,
        BinaryOp::GreaterEqual => left >= right,
        _ => return None,
    })
}

fn scalar_binary(
    op: BinaryOp,
    left: ConstantValue,
    right: ConstantValue,
    result_ty: &ResolvedTy,
    operand_ty: &ResolvedTy,
    pointer_width: u8,
) -> Result<ConstantValue, ConstEvalError> {
    use ConstantValue as V;
    let result = match (left, right) {
        (V::Integer(left), V::Integer(right)) => {
            if let Some(value) = compare(op, left, right) {
                return Ok(V::Bool(value));
            }
            let target = ConstIntegerTarget::from_resolved_ty(operand_ty, pointer_width)
                .ok_or(ConstEvalError::NotConstant)?;
            V::Integer(eval_binary(op, left, right, target)?)
        }
        (V::Float(left), V::Float(right)) => {
            if let Some(value) = compare(op, left, right) {
                return Ok(V::Bool(value));
            }
            V::Float(float_binary(op, left, right, result_ty)?)
        }
        (V::Bool(left), V::Bool(right)) => {
            if let Some(value) = compare(op, left, right) {
                return Ok(V::Bool(value));
            }
            V::Bool(match op {
                BinaryOp::And | BinaryOp::BitAnd => left & right,
                BinaryOp::Or | BinaryOp::BitOr => left | right,
                BinaryOp::BitXor => left ^ right,
                _ => return Err(ConstEvalError::NotConstant),
            })
        }
        (V::String(mut left), V::String(right)) => {
            if let Some(value) = compare(op, &left, &right) {
                return Ok(V::Bool(value));
            }
            if op != BinaryOp::Add {
                return Err(ConstEvalError::NotConstant);
            }
            left.push_str(&right);
            V::String(left)
        }
        (V::Bytes(mut left), V::Bytes(right)) => {
            if let Some(value) = compare(op, &left, &right) {
                return Ok(V::Bool(value));
            }
            if op != BinaryOp::Add {
                return Err(ConstEvalError::NotConstant);
            }
            left.extend(right);
            V::Bytes(left)
        }
        (V::Char(left), V::Char(right)) => {
            V::Bool(compare(op, left, right).ok_or(ConstEvalError::NotConstant)?)
        }
        (V::Unit, V::Unit) => V::Bool(compare(op, (), ()).ok_or(ConstEvalError::NotConstant)?),
        (V::Duration(left), V::Duration(right)) => {
            if let Some(value) = compare(op, left, right) {
                return Ok(V::Bool(value));
            }
            let value = eval_binary(
                op,
                i128::from(left),
                i128::from(right),
                ConstIntegerTarget::from_resolved_ty(&ResolvedTy::I64, pointer_width)
                    .ok_or(ConstEvalError::NotConstant)?,
            )?;
            if *result_ty == ResolvedTy::I64 {
                V::Integer(value)
            } else {
                V::Duration(i64::try_from(value).map_err(|_| ConstEvalError::ArithmeticOverflow)?)
            }
        }
        (V::Duration(left), V::Integer(right)) => {
            duration_scaled(op, i128::from(left), right, pointer_width)?
        }
        (V::Integer(left), V::Duration(right)) if op == BinaryOp::Multiply => {
            duration_scaled(op, left, i128::from(right), pointer_width)?
        }
        _ => return Err(ConstEvalError::NotConstant),
    };
    Ok(result)
}

#[allow(
    clippy::cast_possible_truncation,
    reason = "f32 operations execute at their checked width, including division rounding"
)]
fn float_binary(
    op: BinaryOp,
    left: f64,
    right: f64,
    ty: &ResolvedTy,
) -> Result<f64, ConstEvalError> {
    if *ty == ResolvedTy::F32 {
        let (left, right) = (left as f32, right as f32);
        return Ok(f64::from(match op {
            BinaryOp::Add => left + right,
            BinaryOp::Subtract => left - right,
            BinaryOp::Multiply => left * right,
            BinaryOp::Divide => left / right,
            BinaryOp::Modulo => left % right,
            _ => return Err(ConstEvalError::NotConstant),
        }));
    }
    Ok(match op {
        BinaryOp::Add => left + right,
        BinaryOp::Subtract => left - right,
        BinaryOp::Multiply => left * right,
        BinaryOp::Divide => left / right,
        BinaryOp::Modulo => left % right,
        _ => return Err(ConstEvalError::NotConstant),
    })
}

fn duration_scaled(
    op: BinaryOp,
    left: i128,
    right: i128,
    pointer_width: u8,
) -> Result<ConstantValue, ConstEvalError> {
    let target = ConstIntegerTarget::from_resolved_ty(&ResolvedTy::I64, pointer_width)
        .ok_or(ConstEvalError::NotConstant)?;
    let value = eval_binary(op, left, right, target)?;
    Ok(ConstantValue::Duration(
        i64::try_from(value).map_err(|_| ConstEvalError::ArithmeticOverflow)?,
    ))
}
