//! Split from `expressions.rs`: checker methods, part 3 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    #[expect(
        clippy::too_many_lines,
        reason = "literal coercion requires many match arms with range checks"
    )]
    pub(super) fn check_against_inner(&mut self, expr: &Expr, span: &Span, expected: &Ty) -> Ty {
        // Resolve type variables so that Ty::Var(v) unified with e.g. Ty::I32
        // is seen as Ty::I32 by the coercion arms below.
        let resolved = self.subst.resolve(expected);
        let expected = &resolved;
        // Capture whether THIS expression is a function-return tail (armed by
        // `check_fn_decl` and threaded through `check_block`), then disarm so
        // the recursive operand/field/condition checks below never inherit it —
        // only a genuine tail expression may Ok-coerce. The `Expr::If` and
        // `Expr::Match` arms below re-arm explicitly for their branch bodies
        // (which are themselves tail-flowing), and the default arm consults
        // `tail_ok_armed` to perform the actual coercion.
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        match (expr, expected) {
            (Expr::ContextVariant(context), _) => {
                if let Some(result) = self.dispatch_context_builtin_variant(
                    expected,
                    context,
                    &super::type_members::DottedTypeMemberUse::Reference { span },
                ) {
                    return result;
                }
                let Some(owner) = self.context_variant_expected_owner(expected, span) else {
                    return Ty::Error;
                };
                let Some(variant) = self.context_variant_definition(&owner, &context.name) else {
                    self.report_error(
                        TypeErrorKind::PathMemberNotFound,
                        span,
                        format!(
                            "E_PATH_MEMBER_NOT_FOUND: expected type `{owner}` has no variant `{}`",
                            context.name
                        ),
                    );
                    return Ty::Error;
                };
                let shape_matches = matches!(
                    (&context.record, &variant),
                    (None, VariantDef::Unit) | (Some(_), VariantDef::Struct(_))
                );
                if !shape_matches {
                    self.report_error(
                        TypeErrorKind::PathKindMismatch,
                        span,
                        format!(
                            "E_PATH_KIND_MISMATCH: variant `{owner}.{}` does not use this constructor form",
                            context.name
                        ),
                    );
                    return Ty::Error;
                }
                let qualified_name = format!("{owner}::{}", context.name);
                let compatibility_expr = if let Some(record) = &context.record {
                    Expr::StructInit {
                        name: qualified_name,
                        fields: record.fields.clone(),
                        type_args: None,
                        base: record.base.clone(),
                    }
                } else {
                    Expr::Identifier(qualified_name)
                };
                self.check_against(&compatibility_expr, span, expected)
            }
            // Lambda with expected function type — propagate param types!
            (
                Expr::Lambda {
                    is_move,
                    private_captures,
                    type_params,
                    params,
                    return_type,
                    body,
                    ..
                },
                Ty::Function {
                    params: expected_params,
                    ret,
                    ..
                },
            ) => {
                let result = self.check_lambda(
                    *is_move,
                    private_captures,
                    type_params.as_deref(),
                    params,
                    return_type.as_ref(),
                    body,
                    Some((expected_params, ret)),
                    span,
                    false,
                    false,
                );
                self.expect_type(expected, &result, span);
                self.record_type(span, &result);
                result
            }

            // An unresolved expected type carries no information for the arms,
            // and checking a diverging first arm against it would bind it to
            // `!` before the other arm is seen. Synthesize the join instead,
            // exactly as `check_match_expr` does, and relate it afterwards.
            (Expr::If { .. }, Ty::Var(_)) => {
                let actual = self.synthesize(expr, span);
                if matches!(actual, Ty::Error) {
                    return actual;
                }
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }
            (
                Expr::If {
                    condition,
                    then_block,
                    else_block,
                },
                _,
            ) => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                // Both branch bodies of a tail `if` flow to the function return,
                // so they inherit this expression's armed state; the condition
                // (checked above against `Bool`) does not.
                self.tail_ok_armed = tail_ok_armed;
                let entry = self.env.ownership_snapshot();
                let then_ty = self.check_expr_with_expected(&then_block.0, &then_block.1, expected);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join(&then_ty),
                };
                let actual = if let Some(else_block) = else_block {
                    self.tail_ok_armed = tail_ok_armed;
                    self.env.restore_ownership(&entry);
                    let else_ty =
                        self.check_expr_with_expected(&else_block.0, &else_block.1, expected);
                    let else_exit = BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&else_ty),
                    };
                    self.join_branch_ownership(&entry, &[then_exit, else_exit]);
                    if matches!(then_ty, Ty::Error) || matches!(else_ty, Ty::Error) {
                        Ty::Error
                    } else if matches!(then_ty, Ty::Never) && matches!(else_ty, Ty::Never) {
                        Ty::Never
                    } else {
                        self.subst.resolve(expected)
                    }
                } else {
                    self.join_fall_through(&entry, then_exit);
                    Ty::Unit
                };
                if matches!(actual, Ty::Never | Ty::Error) {
                    actual
                } else {
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        self.record_type(span, &actual);
                        actual
                    }
                }
            }

            (Expr::Match { scrutinee, arms }, _) => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                // A tail `match`'s arm bodies flow to the function return, so
                // re-arm before checking them; the scrutinee (synthesized above)
                // does not. `check_match_expr` threads the flag to each arm body.
                self.tail_ok_armed = tail_ok_armed;
                let actual = self.check_match_expr(&scr_ty, scrutinee, arms, span, Some(expected));
                if matches!(actual, Ty::Never | Ty::Error) {
                    actual
                } else {
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        self.record_type(span, &actual);
                        actual
                    }
                }
            }

            // Range literal `lo..hi` or `lo..=hi` with a known expected
            // `Range<T>` type — check the bounds directly against the element
            // type so they are recorded with the right concrete integer width.
            (
                Expr::Binary {
                    left,
                    op: op @ (BinaryOp::Range | BinaryOp::RangeInclusive),
                    right,
                },
                Ty::Named {
                    builtin: Some(BuiltinType::Range),
                    args,
                    ..
                },
            ) if args.len() == 1 && !matches!(&args[0], Ty::Error | Ty::Var(_)) => {
                let elem_ty = args[0].clone();
                self.check_against(&left.0, &left.1, &elem_ty);
                self.check_against(&right.0, &right.1, &elem_ty);
                let range_ty = Ty::range(elem_ty);
                self.record_type(span, &range_ty);
                range_ty
            }

            (
                Expr::Binary {
                    left,
                    op:
                        op @ (BinaryOp::Add
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
                        | BinaryOp::Shr),
                    right,
                },
                ty,
            ) if ty.is_integer() => {
                let actual = self.check_binary_op(left, *op, right, span);
                let actual_resolved = self.subst.resolve(&actual);
                if actual_resolved.is_integer_literal() {
                    for operand in [left, right] {
                        let key = SpanKey::in_module(&operand.1, self.current_module_idx);
                        let operand_ty = self.expr_types[&key].clone();
                        self.record_concrete_integer_operand(expected, operand, &operand_ty);
                    }
                    self.record_type(span, expected);
                    expected.clone()
                } else if matches!(actual_resolved, Ty::Never | Ty::Error) {
                    actual_resolved
                } else {
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        self.record_type(span, &actual);
                        actual
                    }
                }
            }

            (
                Expr::Unary {
                    op: op @ (UnaryOp::BitNot | UnaryOp::Negate),
                    operand,
                },
                ty,
            ) if ty.is_integer()
                && !ty.is_integer_literal()
                && !(*op == UnaryOp::Negate
                    && matches!(operand.0, Expr::Literal(Literal::Integer { .. }))) =>
            {
                // Complement and negation both use the contextual width for
                // their operand and result, including nested literal
                // expressions (`-(1 + 2)` against `i32` narrows the `1 + 2`
                // arithmetic to `i32` the same way `~(1 + 2)` already did;
                // otherwise the literal defaults to `i64` and MIR has no
                // lowering for the resulting mixed-width unary). A bare
                // `-LITERAL` is excluded: it stays on the `is_integer_literal`
                // arm below, which negates before the range check so the
                // most-negative value of each width (`-128i8`, `i32::MIN`,
                // …) is admitted even though the positive literal alone
                // would overflow.
                let operand_ty = self.check_against(&operand.0, &operand.1, expected);
                if matches!(operand_ty, Ty::Never | Ty::Error) {
                    operand_ty
                } else {
                    self.record_type(span, expected);
                    expected.clone()
                }
            }

            // Integer literal can coerce to any integer type (with range check)
            (expr, ty) if is_integer_literal(expr) && ty.is_integer() => {
                if !expected.is_numeric_literal() {
                    if let Some(value) = extract_integer_literal_value(expr) {
                        let ptr_width = self.pointer_width();
                        if value < 0
                            && !integer_type_info(expected, ptr_width).is_some_and(|i| i.signed)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "negative literal `{value}` cannot be assigned to unsigned type `{}`",
                                    expected.user_facing()
                                ),
                            );
                            return Ty::Error;
                        }
                        if !integer_fits_type(value, expected, ptr_width) {
                            let (lo, hi) =
                                integer_type_range(expected, ptr_width).unwrap_or((0, 0));
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "integer literal `{value}` does not fit in `{}` (range {lo}..={hi})",
                                    expected.user_facing()
                                ),
                            );
                            return Ty::Error;
                        }
                    }
                }
                self.record_integer_literal_type(expr, span, expected);
                expected.clone()
            }

            // Integer literal can coerce to float types (with range check)
            (expr, ty) if is_integer_literal(expr) && ty.is_float() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Float literal can coerce to any float type (with range check)
            (expr, ty) if is_float_literal(expr) && ty.is_float() => {
                if !expected.is_numeric_literal() {
                    if let Some(value) = extract_float_literal_value(expr) {
                        if !float_fits_type(value, expected) {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "float literal `{value}` does not fit in `{}`",
                                    expected.user_facing()
                                ),
                            );
                            return Ty::Error;
                        }
                    }
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Array literal can coerce to Vec<T> when expected
            (
                Expr::Array(elems),
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    args,
                    ..
                },
            ) => {
                let elem_ty = args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                for element in elems {
                    let (operand, operand_span) = element.expr();
                    if element.is_spread() {
                        let want = Self::vec_of(elem_ty.clone());
                        self.check_against(operand, operand_span, &want);
                        self.refuse_uncopyable_spread_element(&elem_ty, operand_span);
                    } else {
                        self.check_against(operand, operand_span, &elem_ty);
                        self.record_value_transfer(operand, operand_span);
                    }
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Array literals checked against [T; N] require exact arity.
            (Expr::Array(elems), Ty::Array(elem_ty, size)) => {
                // A fixed-size array's length is part of its type, and a
                // spread operand's length is a runtime value. Spread builds a
                // `Vec`; a `[T; N]` literal names each element.
                if let Some(spread) = elems.iter().find(|element| element.is_spread()) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &spread.expr().1,
                        format!(
                            "spread `..` is not allowed in a `{}` literal: a fixed-size array's \
                             length is part of its type, and a spread's length is only known at \
                             run time",
                            expected.user_facing()
                        ),
                    );
                    return Ty::Error;
                }
                let Ok(actual_len) = u64::try_from(elems.len()) else {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "array literal has {} elements, which exceeds the supported fixed-array length",
                            elems.len()
                        ),
                    );
                    return Ty::Error;
                };

                if actual_len != *size {
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "array literal length mismatch: expected {size} elements for `{}`, found {actual_len}",
                            expected.user_facing()
                        ),
                    );
                    return Ty::Error;
                }

                for element in elems {
                    let (operand, operand_span) = element.expr();
                    self.check_against(operand, operand_span, elem_ty);
                    self.record_value_transfer(operand, operand_span);
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Map literal can coerce to HashMap<K,V> when expected
            (
                Expr::MapLiteral { entries },
                Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    args,
                    ..
                },
            ) => {
                let key_ty = args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                let val_ty = args.get(1).cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                for (k, v) in entries {
                    self.check_against(&k.0, &k.1, &key_ty);
                    self.check_against(&v.0, &v.1, &val_ty);
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Empty block {} coerces to HashMap<K,V> when expected
            (
                Expr::Block(block),
                Ty::Named {
                    builtin: Some(BuiltinType::HashMap),
                    ..
                },
            ) if block.stmts.is_empty() && block.trailing_expr.is_none() => {
                self.record_type(span, expected);
                expected.clone()
            }

            (Expr::Block(_) | Expr::UnsafeBlock(_), _) => {
                self.tail_ok_armed = tail_ok_armed;
                self.check_expr_with_expected(expr, span, expected)
            }
            // Array repeat coercion to Array<T, N> type. The declared length
            // `N` is part of the fixed-array type, so — like the plain array
            // literal arm above — the repeat count must agree with it. A
            // constant count that differs is rejected with the same arity
            // diagnostic; a count that is not a compile-time constant cannot be
            // proven to equal `N` in a fixed-array position and is rejected too.
            (Expr::ArrayRepeat { value, count }, Ty::Array(elem_ty, size)) => {
                self.check_against(&value.0, &value.1, elem_ty);
                self.record_value_transfer(&value.0, &value.1);
                if *size > 1
                    && self.vec_iteration_element_mode(elem_ty, span)
                        != Some(super::types::VecIterationMode::Clone)
                {
                    self.report_error(TypeErrorKind::InvalidOperation, &value.1,
                        format!("fixed array repeat of length {size} requires a Clone element; `{}` cannot be duplicated", elem_ty.user_facing()));
                    return Ty::Error;
                }
                self.check_against(&count.0, &count.1, &Ty::I64);
                let const_env = self.const_eval_env();
                match crate::check::const_eval::eval_const_expr(count, &const_env) {
                    Ok(actual_count) if actual_count != *size => {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            &count.1,
                            format!(
                                "array repeat length mismatch: expected {size} elements for `{}`, found {actual_count}",
                                expected.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                    Ok(_) => {}
                    Err(_) => {
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            &count.1,
                            format!(
                                "array repeat count must be a compile-time integer equal to the declared length {size} of `{}`",
                                expected.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Known compile-time numeric literal identifiers can coerce to
            // compatible numeric types using the same literal-kind rules.
            (Expr::Identifier(name), ty) if ty.is_numeric() => {
                if let Some(cv) = self.const_values.get(name).cloned() {
                    match (&cv, expected) {
                        (ConstValue::Integer(value), ty) if ty.is_integer() => {
                            if !expected.is_numeric_literal() {
                                let ptr_width = self.pointer_width();
                                if *value < 0
                                    && !integer_type_info(expected, ptr_width)
                                        .is_some_and(|i| i.signed)
                                {
                                    self.report_error(
                                        TypeErrorKind::InvalidOperation,
                                        span,
                                        format!(
                                            "constant `{name}` (value {value}) cannot be assigned to unsigned type `{}`",
                                            expected.user_facing()
                                        ),
                                    );
                                    return Ty::Error;
                                }
                                if !integer_fits_type(*value, expected, ptr_width) {
                                    let (lo, hi) =
                                        integer_type_range(expected, ptr_width).unwrap_or((0, 0));
                                    self.report_error(
                                        TypeErrorKind::InvalidOperation,
                                        span,
                                        format!(
                                            "constant `{name}` (value {value}) does not fit in `{}` (range {lo}..={hi})",
                                            expected.user_facing()
                                        ),
                                    );
                                    return Ty::Error;
                                }
                            }
                            // Mark the identifier as used and register any
                            // closure capture. `synthesize_identifier` uses
                            // `lookup_with_depth` which tracks the scope index
                            // and pushes `lambda_capture_facts` when the binding
                            // is from an outer scope — `env.lookup` would not.
                            self.expect_inferable_literal_binding(name, expected, span);
                            let _ = self.synthesize_identifier(name, span);
                            self.record_type(span, expected);
                            return expected.clone();
                        }
                        (ConstValue::Integer(_), ty) if ty.is_float() => {
                            self.expect_inferable_literal_binding(name, expected, span);
                            let _ = self.synthesize_identifier(name, span);
                            self.record_type(span, expected);
                            return expected.clone();
                        }
                        (ConstValue::Float(value), ty) if ty.is_float() => {
                            if !expected.is_numeric_literal() && !float_fits_type(*value, expected)
                            {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    span,
                                    format!(
                                        "constant `{name}` (value {value}) does not fit in `{}`",
                                        expected.user_facing()
                                    ),
                                );
                                return Ty::Error;
                            }
                            self.expect_inferable_literal_binding(name, expected, span);
                            let _ = self.synthesize_identifier(name, span);
                            self.record_type(span, expected);
                            return expected.clone();
                        }
                        _ => {} // fall through to default
                    }
                }
                // Not a coercible const — fall through to default behaviour
                let actual = self.synthesize(expr, span);
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }

            // Unit literal coercion
            (Expr::Tuple(elems), Ty::Unit) if elems.is_empty() => {
                self.record_type(span, expected);
                expected.clone()
            }

            // Tuple literal coercion: propagate expected element types
            (Expr::Tuple(elems), Ty::Tuple(expected_tys)) if elems.len() == expected_tys.len() => {
                let elements = elems
                    .iter()
                    .zip(expected_tys.iter())
                    .map(|(elem, expected_ty)| {
                        let actual = self.check_against(&elem.0, &elem.1, expected_ty);
                        self.record_value_transfer(&elem.0, &elem.1);
                        actual
                    })
                    .collect();
                let actual = Ty::Tuple(elements);
                self.record_type(span, &actual);
                actual
            }

            // Module-qualified struct init coercion: a bare construction name
            // (`Widget { … }`) constrained by a module-qualified expected type
            // (`widgeti8.Widget`) must resolve its field types from the
            // QUALIFIED type def, not the bare `type_defs["Widget"]` key — which
            // is last-write-wins across two packages that each export `Widget`.
            // Two same-bare-name types from different modules are distinct
            // identities; pinning the construction to the expected module's def
            // keeps each `Widget`'s field layout its own (the i8 vs i64
            // collision). The struct-init site records the QUALIFIED name so the
            // qualifier survives into HIR/MIR layout keying. Only fires when the
            // expected name is qualified (`module.Type`), shares the bare
            // construction name's short form, and is a non-generic struct/record
            // (generics route through the arms below); single-module programs
            // never reach it (bare construction == bare expected).
            (
                Expr::StructInit {
                    name,
                    fields,
                    type_args,
                    base,
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    ..
                },
            ) if name != expected_name
                && expected_args.is_empty()
                && !name.contains('.')
                && !name.contains("::")
                && expected_name.contains('.')
                && crate::short_name(expected_name) == name
                && self.lookup_type_def(expected_name).is_some_and(|td| {
                    td.type_params.is_empty()
                        && matches!(td.kind, TypeDefKind::Struct | TypeDefKind::Record)
                }) =>
            {
                let actual = self.check_struct_init(
                    expected_name,
                    fields,
                    type_args.as_deref(),
                    base.as_deref(),
                    span,
                );
                // `check_struct_init` returns the qualified `Named` but does not
                // record the init site; the synthesize path records via
                // `synthesize_inner`'s tail, which this arm bypasses. Record the
                // qualified type so HIR/MIR key the layout by the module
                // identity, not the bare last-write-wins name.
                self.record_type(span, &actual);
                actual
            }

            // Generic sibling of the arm above: a bare GENERIC construction
            // (`Holder { … }`) constrained by a module-qualified generic expected
            // type (`qualshapes.Holder<qualshapes.Box>`). The bare outer name is
            // legitimate here because the annotation pins the identity, so route
            // the construction through the QUALIFIED expected name (which carries
            // a `.` and so bypasses the bare-scope gate in `check_struct_init`)
            // and let the existing generic-coercion handling below resolve the
            // field type args from `expected`. Only fires for a generic
            // struct/record whose short name matches the bare construction name.
            (
                Expr::StructInit {
                    name,
                    fields,
                    type_args,
                    base,
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    ..
                },
            ) if name != expected_name
                && !expected_args.is_empty()
                && !name.contains('.')
                && !name.contains("::")
                && expected_name.contains('.')
                && crate::short_name(expected_name) == name
                && self.lookup_type_def(expected_name).is_some_and(|td| {
                    !td.type_params.is_empty()
                        && matches!(td.kind, TypeDefKind::Struct | TypeDefKind::Record)
                }) =>
            {
                // Re-dispatch against the same expected type with the qualified
                // construction name, so the generic-struct coercion arm below
                // pins the field type args without the bare-name scope gate
                // rejecting the legitimate annotated construction.
                let qualified_init = Expr::StructInit {
                    name: expected_name.clone(),
                    fields: fields.clone(),
                    type_args: type_args.clone(),
                    base: base.clone(),
                };
                self.check_against(&qualified_init, span, expected)
            }

            // Struct init coercion: propagate expected type args into field checking.
            //
            // A pipe half is excluded by its builtin discriminator, not by its
            // spelling: the resolver renders `std.stream.Sink` under the
            // catalog's bare `Sink`, so a user `type Sink<T>` matches it by
            // name here. No struct literal constructs a substrate handle, so
            // the pair falls through to ordinary coercion and is refused there.
            (
                Expr::StructInit {
                    name,
                    fields,
                    type_args,
                    ..
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    builtin: expected_builtin,
                },
            ) if name == expected_name
                && !expected_builtin.is_some_and(crate::BuiltinType::is_substrate_handle) =>
            {
                // If the literal carries explicit type args, validate that they agree
                // with the expected args coming from the binding site.  Conflicting
                // annotations (`Wrapper<String>` when expected is `Wrapper<int>`) are
                // rejected here rather than being silently dropped.
                if let Some(explicit_args) = type_args {
                    if explicit_args.len() == expected_args.len() {
                        for (te, expected_arg) in explicit_args.iter().zip(expected_args.iter()) {
                            let resolved_arg = self.resolve_type_expr(te);
                            let expected_resolved = self.subst.resolve(expected_arg);
                            if resolved_arg != expected_resolved
                                && !matches!(resolved_arg, Ty::Error)
                            {
                                self.report_error(
                                    TypeErrorKind::Mismatch {
                                        expected: expected_resolved.user_facing().to_string(),
                                        actual: resolved_arg.user_facing().to_string(),
                                    },
                                    span,
                                    format!(
                                        "explicit type argument `{}` conflicts with expected `{}`",
                                        resolved_arg.user_facing(),
                                        expected_resolved.user_facing(),
                                    ),
                                );
                            }
                        }
                    } else {
                        let kind_label = self
                            .lookup_type_def(name)
                            .map_or("type", |type_def| value_type_kind_label(type_def.kind));
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "{kind_label} `{name}` has {} type parameter(s) but {} type argument(s) were supplied",
                                expected_args.len(),
                                explicit_args.len()
                            ),
                        );
                    }
                }

                if let Some(td) = self.lookup_type_def(name) {
                    if td.type_params.len() == expected_args.len() && !expected_args.is_empty() {
                        // Pre-seed type arg map from the expected type
                        let mut type_arg_map: HashMap<String, Ty> = td
                            .type_params
                            .iter()
                            .zip(expected_args.iter())
                            .map(|(p, a)| (p.clone(), a.clone()))
                            .collect();

                        for (field_name, (fexpr, fs)) in fields {
                            if let Some(declared_ty) = td.fields.get(field_name) {
                                let field_expected =
                                    declared_ty.substitute_named_params_parallel(&type_arg_map);
                                let actual = self.check_against(fexpr, fs, &field_expected);
                                self.record_value_transfer(fexpr, fs);

                                // Still infer any remaining unbound type params
                                for tp in &td.type_params {
                                    if !type_arg_map.contains_key(tp)
                                        && *declared_ty
                                            == (Ty::Named {
                                                builtin: None,
                                                name: tp.clone(),
                                                args: vec![],
                                            })
                                    {
                                        type_arg_map.insert(tp.clone(), actual.clone());
                                    }
                                }
                            } else {
                                let similar = crate::error::find_similar(
                                    field_name,
                                    td.fields.keys().map(String::as_str),
                                );
                                self.report_error_with_suggestions(
                                    TypeErrorKind::UndefinedField,
                                    span,
                                    format!(
                                        "no field `{field_name}` on {} `{name}`",
                                        value_type_kind_label(td.kind)
                                    ),
                                    similar,
                                );
                            }
                        }
                        // Check for missing required fields
                        let provided: HashSet<&str> =
                            fields.iter().map(|(n, _)| n.as_str()).collect();
                        for declared in td.fields.keys() {
                            if !provided.contains(declared.as_str()) {
                                self.report_error(
                                    TypeErrorKind::UndefinedField,
                                    span,
                                    format!(
                                        "missing field `{declared}` in initializer of `{name}`"
                                    ),
                                );
                            }
                        }

                        // Also record the inferred / annotation-bound type args
                        // from this coercion arm.  Without this,
                        // `let b: Box<int> = Box { value: 1 }` would bypass
                        // `check_struct_init` entirely and the side-table would
                        // miss the instantiation.  Emits unconditionally;
                        // `validate_record_init_type_args_output_contract` in
                        // `admissibility.rs` prunes any entry whose args still
                        // carry a `Ty::Var` after substitution settles.
                        let resolved_args: Vec<Ty> = td
                            .type_params
                            .iter()
                            .map(|tp| {
                                type_arg_map
                                    .get(tp)
                                    .cloned()
                                    .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                            })
                            .collect();
                        self.record_concrete_record_init_type_args(span, &resolved_args);
                        // Declaration-bound enforcement on coercion-arm ctor:
                        // when the expected type pins a nominal instantiation
                        // (e.g. `let b: Box<Plain> = Box { … }`), the arg
                        // vector built from the coercion is the substitution
                        // the user is committing to. Route through the
                        // canonical helper; bound-free names short-circuit.
                        self.enforce_type_def_instantiation_bounds(name, &resolved_args, span);
                        self.record_type(span, expected);
                        return expected.clone();
                    }
                }
                // Fall through: non-generic or arity mismatch — synthesize normally
                let actual = self.synthesize(expr, span);
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                // If expect_type added a new error, return Ty::Error so callers
                // (e.g. check_fn_decl's outer expect_type) don't re-fire the same
                // mismatch as a duplicate diagnostic.
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }

            // Enum struct-variant init with a known expected enum type:
            // pre-seed type params from the expected args before field checking
            // so that nested generic fields (e.g. Box<T> → Box<int>) resolve
            // correctly.  This mirrors the plain-struct coercion arm above but
            // matches when the init name is a variant, not the type itself.
            (
                Expr::StructInit {
                    name,
                    fields,
                    type_args,
                    ..
                },
                Ty::Named {
                    name: expected_enum_name,
                    args: expected_args,
                    builtin: expected_builtin,
                    ..
                },
            ) => {
                // Fail-closed: explicit type args on enum variant struct forms are not
                // yet supported in the check_against path.  The expected type already
                // provides the type args from the binding site, so there is no safe
                // way to reconcile conflicting annotations here for this slice.
                if type_args.is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "explicit type arguments on enum variant struct initializer `{name}` \
                             are not yet supported when the expected type is already known"
                        ),
                    );
                }
                let short = name.rsplit("::").next().unwrap_or(name.as_str());
                // Reject a mismatched qualified owner (e.g.
                // `right.Status::Ready` when the expected nominal is
                // `left.Status`). Alias and current-module lexical spellings
                // are projected by the shared exact variant-owner authority.
                let expected_nominal = Ty::Named {
                    name: expected_enum_name.clone(),
                    args: expected_args.clone(),
                    builtin: *expected_builtin,
                };
                let prefix_ok = self.variant_surface_owner_matches(name, &expected_nominal);

                let mut handled = false;
                if prefix_ok {
                    if let Some(td) = self.lookup_type_def(expected_enum_name) {
                        let variant_def = td
                            .variants
                            .get(name.as_str())
                            .or_else(|| td.variants.get(short))
                            .cloned();
                        if let Some(VariantDef::Struct(variant_fields)) = variant_def {
                            let type_params = td.type_params.clone();
                            // Only pre-seed when arity matches and there are
                            // type params to substitute.
                            if type_params.len() == expected_args.len() && !type_params.is_empty() {
                                handled = true;
                                // Clone early so we can mutably borrow `self`.
                                let expected_args = expected_args.clone();
                                let mut type_arg_map: HashMap<String, Ty> = type_params
                                    .iter()
                                    .zip(expected_args.iter())
                                    .map(|(p, a)| (p.clone(), a.clone()))
                                    .collect();

                                for (field_name, (fexpr, fs)) in fields {
                                    if let Some((_, declared_ty)) =
                                        variant_fields.iter().find(|(n, _)| n == field_name)
                                    {
                                        let declared_ty = declared_ty.clone();
                                        let field_expected = declared_ty
                                            .substitute_named_params_parallel(&type_arg_map);
                                        let actual = self.check_against(fexpr, fs, &field_expected);
                                        self.record_value_transfer(fexpr, fs);
                                        // Bind any remaining unbound type params
                                        for tp in &type_params {
                                            if !type_arg_map.contains_key(tp)
                                                && declared_ty
                                                    == (Ty::Named {
                                                        builtin: None,
                                                        name: tp.clone(),
                                                        args: vec![],
                                                    })
                                            {
                                                type_arg_map.insert(tp.clone(), actual.clone());
                                            }
                                        }
                                    } else {
                                        let similar = crate::error::find_similar(
                                            field_name,
                                            variant_fields.iter().map(|(n, _)| n.as_str()),
                                        );
                                        self.report_error_with_suggestions(
                                            TypeErrorKind::UndefinedField,
                                            span,
                                            format!("no field `{field_name}` on variant `{name}`"),
                                            similar,
                                        );
                                    }
                                }
                                let provided: HashSet<&str> =
                                    fields.iter().map(|(n, _)| n.as_str()).collect();
                                for (declared, _) in &variant_fields {
                                    if !provided.contains(declared.as_str()) {
                                        self.report_error(
                                            TypeErrorKind::UndefinedField,
                                            span,
                                            format!(
                                                "missing field `{declared}` in initializer of `{name}`"
                                            ),
                                        );
                                    }
                                }
                                // Emit unconditionally; see the struct coercion
                                // arm above for the boundary-prune rationale.
                                let resolved_args: Vec<Ty> = type_params
                                    .iter()
                                    .map(|tp| {
                                        type_arg_map
                                            .get(tp)
                                            .cloned()
                                            .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                                    })
                                    .collect();
                                self.record_concrete_record_init_type_args(span, &resolved_args);
                                // Machine bound enforcement on enum-variant
                                // ctor with expected enum: identical motivation
                                // to the plain-struct coercion arm above; the
                                // enum name carrier IS the machine name when
                                // the expected type is a machine instantiation
                                // (`var m: Holder<File> = Holder::Active { … }`).
                                self.enforce_type_def_instantiation_bounds(
                                    expected_enum_name,
                                    &resolved_args,
                                    span,
                                );
                                self.record_type(span, expected);
                            }
                        }
                    }
                }
                if handled {
                    expected.clone()
                } else {
                    // Variant not found in the expected enum, non-generic, or
                    // arity mismatch — fall back to synthesize + unify.
                    let actual = self.synthesize(expr, span);
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        actual
                    }
                }
            }

            (
                Expr::MethodCall {
                    receiver,
                    method,
                    args,
                },
                _,
            ) => {
                let actual = self
                    .check_dotted_type_member_call_against_expected(
                        receiver, method, args, expected, span,
                    )
                    .unwrap_or_else(|| self.synthesize(expr, span));
                self.finish_named_arguments(args, || format!("method `{method}`"), &actual, span);
                if tail_ok_armed {
                    if let Some(coerced) = self.try_tail_ok_coercion(expected, &actual, span) {
                        return coerced;
                    }
                }
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }

            (
                Expr::Call {
                    function,
                    type_args,
                    args,
                    is_tail_call: _,
                },
                _,
            ) => {
                if let Some(actual) = self.check_call_against_expected_constructor(
                    function,
                    type_args.as_deref(),
                    args,
                    expected,
                    span,
                ) {
                    self.finish_named_arguments(
                        args,
                        || Self::callee_label(function),
                        &actual,
                        span,
                    );
                    actual
                } else {
                    let actual = self.synthesize(expr, span);
                    // Function-tail Ok-coercion for a bare call tail (e.g.
                    // `fn f() -> Result<i64, E> { value() }` where `value(): i64`).
                    // `tail_ok_armed` is true only at a genuine tail position —
                    // the recursive operand/argument checks disarm it — so a call
                    // appearing as an argument or non-tail sub-expression never
                    // reaches here armed. Probe the same sound two-step as the
                    // default arm: full-`Result` tail → no wrap; `Ok`-payload tail
                    // → `Ok(call)`. Both miss → fall through to the normal
                    // unify-and-diagnose below.
                    if tail_ok_armed {
                        if let Some(coerced) = self.try_tail_ok_coercion(expected, &actual, span) {
                            return coerced;
                        }
                    }
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        actual
                    }
                }
            }

            // Unit enum-variant identifier under a known expected named type:
            // when the identifier names a unit variant of the expected type,
            // return the expected type directly (with its generic args already
            // in place).  This is the generic-machine event arm: passing a bare
            // `Initialise` to `step()` on `Machine<i64>` must produce
            // `MachineEvent<i64>`, not `MachineEvent<>` (which synthesize returns
            // from `resolve_identifier_variant`, which has no expected-type context).
            //
            // Guard: only fire when the expected type is a user-defined enum/machine
            // event type that actually contains the named unit variant.  The check is
            // purely additive — the existing synthesize+unify fallback handles all
            // other shapes.
            //
            // A bare builtin `None` under an expected `Option` is refused with the
            // contextual fix-it the expected type makes available.
            (
                Expr::Identifier(name),
                Ty::Named {
                    builtin: Some(crate::BuiltinType::Option),
                    ..
                },
            ) if name == "None" => {
                self.report_bare_variant_expr(name, ".None", span);
                self.record_type(span, expected);
                expected.clone()
            }
            (
                Expr::Identifier(name),
                Ty::Named {
                    name: expected_type_name,
                    args: expected_args,
                    ..
                },
            ) => {
                // Qualified unit-variant identifier (`SplitMode::SplitWords`)
                // under a known expected nominal: the expected type's resolved
                // identity is the resolution authority for its own source-leaf
                // qualifier, exactly as pattern position already resolves a
                // qualified variant against its scrutinee's nominal
                // (`variant_surface_owner_matches`). This is identity-based:
                // the prefix must canonicalize to the expected declaration's
                // exact nominal — a local/source declaration claims the bare
                // spelling first and a foreign owner never folds in — so two
                // same-leaf enums cannot merge here; a mismatched owner falls
                // through to synthesize-and-diagnose.
                let variant_after_owner = name
                    .rsplit_once("::")
                    .filter(|(prefix, _)| !prefix.contains('.'))
                    .filter(|_| self.variant_surface_owner_matches(name, expected))
                    .map(|(_, variant)| variant.to_string());
                let expected_type_def = self.lookup_type_def(expected_type_name);
                let is_unit_variant = expected_type_def
                    .as_ref()
                    .and_then(|td| {
                        td.variants
                            .get(variant_after_owner.as_deref().unwrap_or(name.as_str()))
                            .cloned()
                    })
                    .is_some_and(|v| matches!(v, VariantDef::Unit))
                    && (variant_after_owner.is_some() || !name.contains("::"));
                // A `machine`'s states are not enum variants in expression
                // position (HEW-SPEC-2026 §3.11.3, "State names are not
                // variants"): the target name after `=>` in a body-less
                // transition (`on E: Src => Tgt;`) desugars to a bare
                // `Expr::Identifier(Tgt)` checked against the machine's own
                // type, and resolving it here must not suggest the enum
                // `.Variant` fix-it — that fix-it is for real enum bare
                // variants (#3264).
                let bare_state_here = self.machine_state_is_bare_here(expected_type_name);
                if is_unit_variant {
                    if !name.contains("::") && !bare_state_here {
                        self.report_bare_variant_expr(name, &format!(".{name}"), span);
                    }
                    self.enforce_type_def_instantiation_bounds(
                        expected_type_name,
                        expected_args,
                        span,
                    );
                    self.record_type(span, expected);
                    expected.clone()
                } else {
                    // Not a unit variant of this type — synthesize and unify.
                    let actual = self.synthesize(expr, span);
                    // Function-tail Ok-coercion for a bare identifier tail (e.g.
                    // `fn f(x: i64) -> Result<i64, E> { x }`, including the
                    // generic `fn g<T>(x: T) -> Result<T, E> { x }`). `tail_ok_armed`
                    // is true only at a genuine tail — recursive checks disarm it —
                    // so an identifier used as an argument or non-tail
                    // sub-expression never reaches here armed. Same two-step probe
                    // as the default arm.
                    if tail_ok_armed {
                        if let Some(coerced) = self.try_tail_ok_coercion(expected, &actual, span) {
                            return coerced;
                        }
                    }
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        actual
                    }
                }
            }

            // Default: synthesize and unify
            _ => {
                let actual = self.synthesize(expr, span);
                // Function-tail Ok-coercion. When this expression is the tail of
                // a `Result<Ok, Err>`-returning function (and only then —
                // `tail_ok_armed` is set exclusively at tail positions) and its
                // type is the `Ok` payload rather than the full `Result`, wrap
                // it in `Ok(..)`. This is type-directed and unambiguous: the
                // full-`Result` case is probed FIRST and takes the no-coercion
                // path, so a tail already typed `Result<Ok, Err>` is returned
                // directly (no double-wrap into `Result<Result<..>, ..>`), and a
                // genuine `Ok`-payload tail (e.g. `db.find(id)?` typed `User`
                // under `-> Result<User, E>`) is wrapped. For finite types the
                // two are mutually exclusive (no `T == Result<T, E>`).
                if tail_ok_armed {
                    if let Some(coerced) = self.try_tail_ok_coercion(expected, &actual, span) {
                        return coerced;
                    }
                }
                let n = self.errors.len();
                self.expect_type(expected, &actual, span);
                // Same duplicate-suppression as the struct-init fallthrough above.
                if self.errors.len() > n {
                    Ty::Error
                } else {
                    actual
                }
            }
        }
    }

    pub(in crate::check) fn context_variant_expected_owner(
        &mut self,
        expected: &Ty,
        span: &Span,
    ) -> Option<String> {
        let resolved = self.subst.resolve(expected);
        // An already-broken expected type has a diagnostic of its own. Naming
        // it again as "found `<error>`" is a cascade, and since v0.6.0 the
        // dotted spelling is the only one users write, so every scrutinee or
        // argument that fails to resolve would carry this second error.
        if matches!(resolved, Ty::Error) {
            return None;
        }
        let Ty::Named { name, builtin, .. } = &resolved else {
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: contextual variant requires one expected enum or machine type, found `{}`",
                    resolved.user_facing()
                ),
            );
            return None;
        };

        if !name.contains('.') {
            if let Some(owners) = self.published_bare_type_owners.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                name.clone(),
            )) {
                if owners.len() > 1 {
                    let candidates = owners.iter().cloned().collect::<Vec<_>>();
                    self.report_error_with_suggestions(
                        TypeErrorKind::ContextVariantAmbiguous,
                        span,
                        format!(
                            "E_CONTEXT_VARIANT_AMBIGUOUS: expected type `{name}` has {} imported owners",
                            candidates.len()
                        ),
                        candidates
                            .iter()
                            .map(|candidate| format!("use an owner-qualified type such as `{candidate}`"))
                            .collect(),
                    );
                    return None;
                }
            }
        }

        if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
            return Some(name.clone());
        }
        let Some(definition) = self.type_defs.get(name) else {
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: expected type `{name}` is not an enum or machine"
                ),
            );
            return None;
        };
        if !matches!(definition.kind, TypeDefKind::Enum | TypeDefKind::Machine) {
            self.report_error(
                TypeErrorKind::ContextVariantNoType,
                span,
                format!(
                    "E_CONTEXT_VARIANT_NO_TYPE: expected type `{name}` is not an enum or machine"
                ),
            );
            return None;
        }
        Some(name.clone())
    }

    pub(in crate::check) fn context_variant_definition(
        &self,
        owner: &str,
        variant: &str,
    ) -> Option<VariantDef> {
        self.type_defs
            .get(owner)
            .and_then(|definition| definition.variants.get(variant))
            .cloned()
    }

    /// Attempt the function-tail Ok-coercion described in
    /// [`TypeCheckOutput::tail_ok_coercions`].
    ///
    /// `expected` must be the (already substitution-resolved) declared return
    /// type and `actual` the synthesized tail expression type. Returns
    /// `Some(expected.clone())` — the full `Result` type — when the tail is
    /// Ok-wrapped, recording the coercion at `span` for HIR lowering. Returns
    /// `None` when no coercion applies (expected is not `Result`, the tail
    /// already unifies with the full `Result`, or the tail does not unify with
    /// the `Ok` payload); the caller then runs its normal unify-and-diagnose
    /// path. Probes are snapshot-guarded so a failed trial unification leaves
    /// the substitution untouched.
    pub(super) fn try_tail_ok_coercion(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) -> Option<Ty> {
        let (ok_ty, err_ty) = expected.as_result()?;
        let ok_ty = ok_ty.clone();
        let err_ty = err_ty.clone();

        // Probe 1 — does the tail already produce the FULL `Result<Ok, Err>`?
        // If so this is `fn f() -> Result<..> { g() }` where `g()` returns the
        // Result directly: no coercion, fall back to the normal path (which
        // re-unifies). Roll the probe back so it commits nothing.
        let snapshot = self.subst.snapshot();
        let full_result = Ty::result(ok_ty.clone(), err_ty.clone());
        let unifies_full = self.try_unify_with_owner_identity(&full_result, actual);
        self.subst.restore(snapshot);
        if unifies_full {
            return None;
        }

        // Probe 2 — does the tail produce the `Ok` payload? If so, Ok-wrap it.
        // Commit this unification (it is the path we take) so the tail
        // expression's recorded type and any inference variables settle against
        // the `Ok` payload.
        let snapshot = self.subst.snapshot();
        if self.try_unify_with_owner_identity(&ok_ty, actual) {
            self.record_suspension_obligations(&ok_ty, actual, span);
            self.tail_ok_coercions
                .insert(SpanKey::in_module(span, self.current_module_idx));
            // Return the full `Result` as this expression's check-against
            // result so the block / function-return type-check sees a satisfied
            // return. Do NOT overwrite the recorded type at `span` with the
            // `Result`: the tail and its inner expression (e.g. the `?`
            // expression) share this span, and HIR lowering reads the inner
            // `Ok`-payload type back at lowering time. `wrap_tail_ok` supplies
            // the outer `Result` type when it wraps the lowered value in
            // `Ok(..)`, so the recorded span type must stay the inner payload.
            return Some(expected.clone());
        }
        self.subst.restore(snapshot);
        None
    }

    #[expect(
        clippy::too_many_lines,
        reason = "builtin method resolution requires many cases"
    )]
    pub(in crate::check) fn check_binary_op(
        &mut self,
        left: &Spanned<Expr>,
        op: BinaryOp,
        right: &Spanned<Expr>,
        expr_span: &Span,
    ) -> Ty {
        let left_is_coercible = self.is_coercible_numeric(&left.0);
        let right_is_coercible = self.is_coercible_numeric(&right.0);

        // When one side is a numeric literal (or literal-backed const) and the
        // other is a concrete numeric type, use check_against so the literal
        // adopts the non-literal's type instead of defaulting immediately.
        let (left_ty, right_ty) = if left_is_coercible && !right_is_coercible {
            let rt = self.synthesize(&right.0, &right.1);
            let rt_resolved = self.subst.resolve(&rt);
            if rt_resolved.is_numeric() {
                let lt = self.check_against(&left.0, &left.1, &rt_resolved);
                (lt, rt)
            } else {
                let lt = self.synthesize(&left.0, &left.1);
                (lt, rt)
            }
        } else if right_is_coercible && !left_is_coercible {
            let lt = self.synthesize(&left.0, &left.1);
            let lt_resolved = self.subst.resolve(&lt);
            if lt_resolved.is_numeric() {
                let rt = self.check_against(&right.0, &right.1, &lt_resolved);
                (lt, rt)
            } else {
                let rt = self.synthesize(&right.0, &right.1);
                (lt, rt)
            }
        } else {
            let lt = self.synthesize(&left.0, &left.1);
            let rt = self.synthesize(&right.0, &right.1);
            (lt, rt)
        };

        // Resolve type variables through substitution so we check against
        // concrete types when available (bidirectional inference).
        let left_resolved = self.subst.resolve(&left_ty);
        let right_resolved = self.subst.resolve(&right_ty);
        if matches!(left_resolved, Ty::Error) || matches!(right_resolved, Ty::Error) {
            return Ty::Error;
        }

        if left_resolved.is_float() && right_resolved.is_float() {
            if let Some(common_ty) =
                common_numeric_type(&left_resolved, &right_resolved, self.pointer_width())
            {
                for (operand, source_ty) in [(left, &left_resolved), (right, &right_resolved)] {
                    if !source_ty.is_numeric_literal() && *source_ty != common_ty {
                        self.numeric_operand_coercions.insert(
                            SpanKey::in_module(&operand.1, self.current_module_idx),
                            common_ty.clone(),
                        );
                    }
                }
            }
        }

        match op {
            // Wrapping arithmetic: integer-only. No string concat, no duration,
            // no float. Both operands must be integer types of the same width.
            BinaryOp::WrappingAdd | BinaryOp::WrappingSub | BinaryOp::WrappingMul => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        self.expect_concrete_integer_operands(
                            &common_ty, left, &left_ty, right, &right_ty,
                        );
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "`{op}` requires compatible integer types; found `{}` and `{}`",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else if matches!(&left_resolved, Ty::Var(_)) && right_resolved.is_integer() {
                    self.expect_type(&right_ty, &left_ty, &left.1);
                    right_ty
                } else if left_resolved.is_integer() && matches!(&right_resolved, Ty::Var(_))
                    || matches!((&left_resolved, &right_resolved), (Ty::Var(_), Ty::Var(_)))
                {
                    // Either only the right is a type variable (constrain it to
                    // the left's integer type) or both are type variables
                    // (unify them and leave the result polymorphic).
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!(
                            "`{op}` requires integer operands; found `{}` and `{}`",
                            left_resolved.user_facing(),
                            right_resolved.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }
            BinaryOp::Add
            | BinaryOp::Subtract
            | BinaryOp::Multiply
            | BinaryOp::Divide
            | BinaryOp::Modulo => {
                if left_resolved.is_duration()
                    || right_resolved.is_duration()
                    || left_resolved.is_instant()
                    || right_resolved.is_instant()
                {
                    return self.check_duration_arithmetic(
                        op,
                        &left_resolved,
                        &right_resolved,
                        &left.1,
                    );
                }
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        self.expect_concrete_integer_operands(
                            &common_ty, left, &left_ty, right, &right_ty,
                        );
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` in arithmetic; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if Self::concrete_integer_float_mismatch(
                        &left_resolved,
                        &right_resolved,
                        self.pointer_width(),
                    ) {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` in arithmetic; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                    if let Some(common_ty) =
                        common_numeric_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` in arithmetic; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else if matches!(&left_resolved, Ty::Var(_)) && right_resolved.is_numeric() {
                    // Type variable on left — constrain it to the right's numeric type
                    self.expect_type(&right_ty, &left_ty, &left.1);
                    right_ty
                } else if left_resolved.is_numeric() && matches!(&right_resolved, Ty::Var(_)) {
                    // Type variable on right — constrain it to the left's numeric type
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else if matches!((&left_resolved, &right_resolved), (Ty::Var(_), Ty::Var(_))) {
                    // Both are type variables — unify them, result stays polymorphic
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else if matches!(op, BinaryOp::Add)
                    && left_resolved == Ty::String
                    && right_resolved == Ty::String
                {
                    Ty::String // string concatenation
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!(
                            "cannot apply `{op}` to `{}` and `{}`",
                            left_resolved.user_facing(),
                            right_resolved.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }
            BinaryOp::BitAnd
            | BinaryOp::BitOr
            | BinaryOp::BitXor
            | BinaryOp::Shl
            | BinaryOp::Shr => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        self.expect_concrete_integer_operands(
                            &common_ty, left, &left_ty, right, &right_ty,
                        );
                        common_ty
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "bitwise `{op}` requires compatible integer types; found `{}` and `{}`",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else if matches!(&left_resolved, Ty::Var(_)) && right_resolved.is_integer() {
                    self.expect_type(&right_ty, &left_ty, &left.1);
                    right_ty
                } else if (left_resolved.is_integer() && matches!(&right_resolved, Ty::Var(_)))
                    || matches!((&left_resolved, &right_resolved), (Ty::Var(_), Ty::Var(_)))
                {
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    left_ty
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!(
                            "bitwise `{op}` requires integer operands, found `{}` and `{}`",
                            left_resolved.user_facing(),
                            right_resolved.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::Less
            | BinaryOp::LessEqual
            | BinaryOp::Greater
            | BinaryOp::GreaterEqual => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        self.expect_concrete_integer_operands(
                            &common_ty, left, &left_ty, right, &right_ty,
                        );
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` for comparison; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                    }
                } else if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if Self::concrete_integer_float_mismatch(
                        &left_resolved,
                        &right_resolved,
                        self.pointer_width(),
                    ) {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` for comparison; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        return Ty::Bool;
                    }
                    if common_numeric_type(&left_resolved, &right_resolved, self.pointer_width())
                        .is_none()
                    {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "cannot implicitly coerce `{}` and `{}` for comparison; use an explicit conversion",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                    }
                } else {
                    let errors_before = self.errors.len();
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    // Only run the record-comparison gate when the operand
                    // types agree — a mismatch already produced the more
                    // precise error above.
                    if self.errors.len() == errors_before {
                        self.reject_unbounded_generic_ordering(
                            op,
                            &left_resolved,
                            &right_resolved,
                            &left.1,
                            &right.1,
                        );
                        self.reject_record_comparison(
                            op,
                            &left_resolved,
                            &right_resolved,
                            &left.1,
                            &right.1,
                            expr_span,
                        );
                    }
                }
                Ty::Bool
            }
            BinaryOp::And | BinaryOp::Or => {
                self.expect_type(&Ty::Bool, &left_ty, &left.1);
                self.expect_type(&Ty::Bool, &right_ty, &right.1);
                Ty::Bool
            }
            BinaryOp::Range | BinaryOp::RangeInclusive => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&left_resolved, &right_resolved, self.pointer_width())
                    {
                        // When both bounds are integer literals (e.g. `0..8`),
                        // use a fresh type variable so the element type can be
                        // inferred from context (e.g. how the loop variable is
                        // used).  If nothing constrains it, it stays as-is
                        // and defaults to the literal type (i64).
                        if left_is_coercible && right_is_coercible {
                            let var_tv = TypeVar::fresh();
                            // When a bound is a bare identifier referring to an
                            // unannotated `let`-bound literal (`let n = 6; ...
                            // 0 .. n`), that identifier's OWN inference var
                            // (from `infer_integer_literal_binding_type`) is
                            // already bound to `IntLiteral` by the time this
                            // range is checked — it is a SEPARATE unknown from
                            // the range's fresh `var_tv`. A later use-site
                            // constraint on the loop variable (e.g.
                            // `vec.push(i)` forcing `i32`) narrows only
                            // `var_tv`; the bound identifier's own var still
                            // defaults to `i64` independently, producing a
                            // `Range<i32>` whose own end-bound expression
                            // resolves to `i64` — a self-inconsistent range
                            // MIR correctly rejects as a narrowing. Record each
                            // identifier bound's own binding var alongside the
                            // deferred span so `apply_deferred_range_bound_types`
                            // can promote it too once `var_tv` resolves.
                            let left_binding_var =
                                Self::coercible_identifier_binding_var(&self.env, &left.0);
                            let right_binding_var =
                                Self::coercible_identifier_binding_var(&self.env, &right.0);
                            // Stash the bound spans + literal values for the
                            // post-inference pass that re-records them with
                            // the concrete resolved element type.
                            // Extract the inner operand span when the bound
                            // is a negated integer literal (`-5`). The inner
                            // literal's span must also be re-recorded by
                            // `apply_deferred_range_bound_types` so HIR
                            // lowering sees the narrowed type (e.g. `i32`)
                            // rather than the `IntLiteral`→`I64` default.
                            let left_inner_span = if let hew_parser::ast::Expr::Unary {
                                op: hew_parser::ast::UnaryOp::Negate,
                                operand,
                            } = &left.0
                            {
                                if matches!(
                                    operand.0,
                                    hew_parser::ast::Expr::Literal(
                                        hew_parser::ast::Literal::Integer { .. }
                                    )
                                ) {
                                    Some(operand.1.clone())
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            let right_inner_span = if let hew_parser::ast::Expr::Unary {
                                op: hew_parser::ast::UnaryOp::Negate,
                                operand,
                            } = &right.0
                            {
                                if matches!(
                                    operand.0,
                                    hew_parser::ast::Expr::Literal(
                                        hew_parser::ast::Literal::Integer { .. }
                                    )
                                ) {
                                    Some(operand.1.clone())
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            self.deferred_range_bounds.push((
                                left.1.clone(),
                                var_tv,
                                extract_integer_literal_value(&left.0),
                                left_inner_span,
                                self.current_module_idx,
                                left_binding_var,
                            ));
                            self.deferred_range_bounds.push((
                                right.1.clone(),
                                var_tv,
                                extract_integer_literal_value(&right.0),
                                right_inner_span,
                                self.current_module_idx,
                                right_binding_var,
                            ));
                            Ty::range(Ty::Var(var_tv))
                        } else {
                            Ty::range(common_ty)
                        }
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &left.1,
                            format!(
                                "range bounds require compatible integer types; found `{}` and `{}`",
                                left_resolved.user_facing(),
                                right_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else {
                    self.expect_type(&left_ty, &right_ty, &right.1);
                    Ty::range(left_ty)
                }
            }
        }
    }

    /// If `expr` is a bare identifier bound (via an unannotated `let`) to a
    /// still-open literal-defaulting `TypeVar`, return that var.
    ///
    /// Only `infer_integer_literal_binding_type` creates this shape — it
    /// gives an unannotated `let n = 6;` its own `Ty::Var` (immediately
    /// unified with `IntLiteral`, but re-promotable later, same as any other
    /// literal-defaulting var) rather than the plain `Ty::IntLiteral` tag a
    /// bare literal expression carries. A range-bound identifier of this
    /// shape needs its OWN var promoted alongside the range's fresh element
    /// var — see the call site in `check_binary_op`'s Range arm.
    pub(super) fn coercible_identifier_binding_var(
        env: &crate::env::TypeEnv,
        expr: &Expr,
    ) -> Option<TypeVar> {
        let Expr::Identifier(name) = expr else {
            return None;
        };
        match env.lookup_ref(name)?.ty {
            Ty::Var(v) => Some(v),
            _ => None,
        }
    }

    pub(super) fn reject_unbounded_generic_ordering(
        &mut self,
        op: BinaryOp,
        left_resolved: &Ty,
        right_resolved: &Ty,
        left_span: &Span,
        right_span: &Span,
    ) {
        if !matches!(
            op,
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual
        ) {
            return;
        }
        let Some(param_name) = self.same_current_type_param_name(left_resolved, right_resolved)
        else {
            return;
        };
        if self.type_param_carries_bound(&param_name, "PartialOrd") {
            return;
        }
        let span = Span {
            start: left_span.start,
            end: right_span.end,
        };
        self.report_error(
            TypeErrorKind::InvalidOperation,
            &span,
            format!("`{op}` requires type parameter `{param_name}` to be bounded by `PartialOrd`"),
        );
    }

    pub(super) fn same_current_type_param_name(&self, left: &Ty, right: &Ty) -> Option<String> {
        let left_name = self.current_type_param_name(left)?;
        let right_name = self.current_type_param_name(right)?;
        (left_name == right_name).then_some(left_name)
    }

    pub(super) fn current_type_param_name(&self, ty: &Ty) -> Option<String> {
        let Ty::Named {
            name,
            args,
            builtin: None,
        } = ty
        else {
            return None;
        };
        if !args.is_empty() {
            return None;
        }
        if self
            .current_type_param_bounds
            .iter()
            .rev()
            .any(|frame| frame.bounds.contains_key(name))
        {
            return Some(name.clone());
        }
        let fn_name = self.current_function.as_ref()?;
        self.fn_sigs.get(fn_name).and_then(|sig| {
            sig.type_params
                .iter()
                .any(|param_name| param_name == name)
                .then_some(name.clone())
        })
    }

    pub(in crate::check) fn current_type_param_names(&self) -> HashSet<String> {
        let mut names = HashSet::new();
        for frame in &self.current_type_param_bounds {
            names.extend(frame.bounds.keys().cloned());
        }
        if let Some(fn_name) = &self.current_function {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                names.extend(sig.type_params.iter().cloned());
            }
        }
        names
    }

    /// Like `current_type_param_names`, but carries each name's declared
    /// bounds instead of discarding them. A deferred check that replays
    /// admission after inference settles (`finalize_hashmap_admission`) needs
    /// the actual bounds to answer `type_param_has_marker_bound`; the
    /// original declaration scope is gone by then, so this is the one point
    /// that captures it.
    pub(in crate::check) fn current_type_param_bounds_map(&self) -> HashMap<String, Vec<String>> {
        let mut bounds: HashMap<String, Vec<String>> = HashMap::new();
        for frame in &self.current_type_param_bounds {
            for (name, param_bounds) in &frame.bounds {
                bounds
                    .entry(name.clone())
                    .or_insert_with(|| param_bounds.clone());
            }
        }
        if let Some(fn_name) = &self.current_function {
            if let Some(sig) = self.fn_sigs.get(fn_name) {
                for param_name in &sig.type_params {
                    bounds.entry(param_name.clone()).or_insert_with(|| {
                        sig.type_param_bounds
                            .get(param_name)
                            .cloned()
                            .unwrap_or_default()
                    });
                }
            }
        }
        bounds
    }

    /// Equality uses the selected Eq authority after declarations and inference
    /// settle. Ordinary numeric comparisons bypass this gate and retain IEEE
    /// float semantics; selecting aggregate Eq does not change ordering.
    pub(super) fn reject_record_comparison(
        &mut self,
        op: BinaryOp,
        left_resolved: &Ty,
        right_resolved: &Ty,
        left_span: &Span,
        right_span: &Span,
        expr_span: &Span,
    ) {
        if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
            // Preserve the exact top-level user-method dispatch route. Nested
            // user methods are selected recursively by TypeFactService.
            if let Ty::Named { builtin: None, .. } = left_resolved {
                if let Some((method, _)) =
                    self.trait_impl_method_declaration(left_resolved, "Eq", "eq")
                {
                    self.record_user_comparison_dispatch(
                        expr_span,
                        UserComparisonDispatch::Eq { method },
                    );
                    return;
                }
            }
            self.record_eq_requirement(left_resolved, expr_span);
            return;
        }
        if let Ty::Named { builtin: None, .. } = left_resolved {
            if let Some((method, _)) =
                self.trait_impl_method_declaration(left_resolved, "Ord", "lt")
            {
                self.record_user_comparison_dispatch(
                    expr_span,
                    UserComparisonDispatch::Ord { method },
                );
                return;
            }
            if let Some((method, _)) =
                self.trait_impl_method_declaration(left_resolved, "PartialOrd", "lt")
            {
                self.record_user_comparison_dispatch(
                    expr_span,
                    UserComparisonDispatch::PartialOrd { method },
                );
                return;
            }
        }
        let Some(type_name) = [left_resolved, right_resolved].into_iter().find_map(|ty| {
            let aggregate = match ty {
                Ty::Tuple(_)
                | Ty::Named {
                    builtin: Some(BuiltinType::Option | BuiltinType::Result),
                    ..
                } => true,
                Ty::Named { name, .. } => self.type_defs.get(name).is_some_and(|definition| {
                    matches!(
                        definition.kind,
                        TypeDefKind::Struct | TypeDefKind::Record | TypeDefKind::Enum
                    )
                }),
                _ => false,
            };
            aggregate.then(|| ty.user_facing().to_string())
        }) else {
            return;
        };
        let span = left_span.start..right_span.end;
        if !self
            .registry
            .implements_marker(left_resolved, MarkerTrait::PartialOrd)
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &span,
                format!(
                    "`{op}` is not available for `{type_name}` because the type does not \
                     derive `PartialOrd`; provide a user `impl Ord` or `impl PartialOrd`"
                ),
            );
            return;
        }
        self.report_error(
            TypeErrorKind::DerivedOrdUnavailable {
                type_name: type_name.clone(),
            },
            &span,
            format!(
                "E_LIMIT_DERIVED_ORD: `{op}` has no derived ordering for `{type_name}` yet \
                 — provide `impl Ord for {type_name}` (or `impl PartialOrd`) with a `lt` method"
            ),
        );
    }

    /// Record that the binary expression at `span` must dispatch to a user
    /// trait impl rather than the compiler's structural comparison. See
    /// [`UserComparisonDispatch`].
    pub(super) fn record_user_comparison_dispatch(
        &mut self,
        span: &Span,
        dispatch: UserComparisonDispatch,
    ) {
        self.user_comparison_dispatch
            .insert(SpanKey::in_module(span, self.current_module_idx), dispatch);
    }

    /// True when `ty` still names one of `params`.
    ///
    /// Implemented by substituting every parameter for a type that cannot occur
    /// in a checked program (`Ty::Never`) and comparing: this reuses the one
    /// substitution traversal instead of adding a second walk that could drift
    /// out of sync with it as `Ty` grows variants.
    pub(in crate::check) fn ty_mentions_type_params(ty: &Ty, params: &[String]) -> bool {
        if params.is_empty() {
            return false;
        }
        let probe: HashMap<String, Ty> = params
            .iter()
            .cloned()
            .map(|param| (param, Ty::Never))
            .collect();
        ty.substitute_named_params_parallel(&probe) != *ty
    }

    /// Record an Eq demand in the existing instantiation obligation graph.
    /// Concrete demands are checked once declarations and inference settle;
    /// abstract demands are substituted at the graph's concrete call roots.
    pub(in crate::check) fn record_eq_requirement(&mut self, ty: &Ty, span: &Span) {
        let owner = self.current_function.clone();
        let params = owner
            .as_ref()
            .and_then(|key| self.fn_sigs.get(key))
            .map_or_else(Vec::new, |sig| sig.type_params.clone());
        let requirements = self.eq_requirements.entry(owner).or_default();
        if requirements.iter().any(|existing| {
            existing.ty == *ty
                && existing.span == *span
                && existing.source_module == self.current_module
        }) {
            return;
        }
        requirements.push(EqRequirement {
            ty: ty.clone(),
            owner_type_params: params,
            span: span.clone(),
            source_module: self.current_module.clone(),
        });
    }

    /// The single recording authority for a generic application.
    ///
    /// Every application shape — free function, module-qualified function,
    /// method, actor method, trait-impl method — funnels through
    /// `apply_instantiated_call_signature_with_assoc`, and that is the only
    /// caller of this function. Recording anywhere else would reintroduce the
    /// exact gap this closes: obligations discharged for direct calls only,
    /// while a method instantiation walked straight into codegen.
    ///
    /// Two independent sources pin the callee's parameters and BOTH are merged
    /// by name: the signature instantiation (method-level parameters) and the
    /// receiver's type arguments (impl-level parameters, which
    /// `lookup_named_method_sig` has already substituted out of the signature).
    pub(in crate::check) fn record_generic_application(
        &mut self,
        callee: GenericCallee<'_>,
        sig_type_params: &[String],
        sig_type_args: &[Ty],
        span: &Span,
    ) {
        // The one place method identity is joined into a `fn_sigs` key.
        let (callee_key, owner) = match callee {
            GenericCallee::Function { key } => (key.to_string(), None),
            GenericCallee::Method {
                type_name,
                method,
                owner_type_args,
            } => (
                format!("{type_name}::{method}"),
                Some((type_name, owner_type_args)),
            ),
        };
        let Some(declared_params) = self
            .fn_sigs
            .get(&callee_key)
            .map(|sig| sig.type_params.clone())
            .filter(|params| !params.is_empty())
        else {
            return;
        };
        let mut substitution: HashMap<String, Ty> = HashMap::new();
        if sig_type_params.len() == sig_type_args.len() {
            for (param, arg) in sig_type_params.iter().zip(sig_type_args) {
                substitution.insert(param.clone(), self.subst.resolve(arg));
            }
        }
        if let Some((owner_name, owner_args)) = owner {
            let owner_params = self
                .type_defs
                .get(owner_name)
                .map(|type_def| type_def.type_params.clone())
                .unwrap_or_default();
            if owner_params.len() == owner_args.len() {
                for (param, arg) in owner_params.iter().zip(owner_args) {
                    substitution
                        .entry(param.clone())
                        .or_insert_with(|| self.subst.resolve(arg));
                }
            }
        }
        // Nothing pinned means nothing to discharge; a partially pinned
        // application still records, and the walk refuses to decide any
        // obligation whose substituted form is still abstract.
        if !declared_params
            .iter()
            .any(|param| substitution.contains_key(param))
        {
            return;
        }
        let enclosing = self.current_function.clone();
        let enclosing_params = enclosing
            .as_ref()
            .and_then(|name| self.fn_sigs.get(name))
            .map_or_else(Vec::new, |sig| sig.type_params.clone());
        self.generic_fn_instantiation_sites
            .push(GenericFnInstantiationSite {
                caller: enclosing,
                caller_type_params: enclosing_params,
                callee: callee_key,
                substitution,
                span: span.clone(),
                source_module: self.current_module.clone(),
            });
    }

    /// Split the recorded applications into concrete roots and generic → generic
    /// edges.
    ///
    /// An application whose substitution still names the enclosing generic
    /// function's own parameters proves nothing on its own; it becomes an edge,
    /// reachable only once a concrete root pins those parameters.
    pub(super) fn partition_generic_instantiation_sites(
        &self,
        sites: Vec<GenericFnInstantiationSite>,
    ) -> (
        Vec<PendingInstantiation>,
        HashMap<String, Vec<GenericCallEdge>>,
    ) {
        let mut roots: Vec<PendingInstantiation> = Vec::new();
        let mut edges: HashMap<String, Vec<GenericCallEdge>> = HashMap::new();
        for site in sites {
            let substitution: HashMap<String, Ty> = site
                .substitution
                .iter()
                .map(|(param, ty)| {
                    (
                        param.clone(),
                        self.subst.resolve(ty).materialize_literal_defaults(),
                    )
                })
                .collect();
            let still_abstract = substitution
                .values()
                .any(|ty| Self::ty_mentions_type_params(ty, &site.caller_type_params));
            if still_abstract {
                if let Some(owner) = site.caller {
                    edges.entry(owner).or_default().push(GenericCallEdge {
                        callee: site.callee,
                        substitution,
                    });
                }
                continue;
            }
            roots.push(PendingInstantiation {
                chain: vec![site.callee.clone()],
                callee: site.callee,
                substitution,
                report_span: site.span,
                report_module: site.source_module,
                depth: 0,
            });
        }
        (roots, edges)
    }

    /// Stable rendering of a substitution, for the visited-set key.
    pub(super) fn render_substitution(substitution: &HashMap<String, Ty>) -> String {
        let mut pairs: Vec<String> = substitution
            .iter()
            .map(|(param, ty)| format!("{param}={}", ty.user_facing()))
            .collect();
        pairs.sort();
        pairs.join(", ")
    }

    /// Build the diagnostic for one ineligible instantiation of a generic
    /// callee that requires Eq for `template`.
    pub(super) fn generic_structural_eq_instantiation_error(
        template: &Ty,
        concrete: &Ty,
        pending: &PendingInstantiation,
    ) -> crate::error::TypeError {
        let callee = &pending.callee;
        let mut err = crate::error::TypeError::new(
            TypeErrorKind::InvalidOperation,
            pending.report_span.clone(),
            format!(
                "`{callee}` requires Eq for `{}`; this instantiation `{}` has no selected Eq implementation",
                template.user_facing(),
                concrete.user_facing(),
            ),
        )
        .with_suggestion(format!(
            "instantiate `{callee}` with a type that supports Eq, or provide an Eq implementation"
        ));
        if let Some(module) = pending.report_module.clone() {
            err = err.with_source_module(module);
        }
        err
    }

    /// Fail-closed diagnostic for an instantiation chain that outruns the hop
    /// budget.
    ///
    /// Dropping the obligation here would hand the un-analysed instantiation to
    /// codegen — the very thing this pass exists to prevent — so the budget
    /// refuses the program and names the chain that hit it.
    pub(super) fn generic_structural_eq_depth_error(
        pending: &PendingInstantiation,
        budget: u32,
    ) -> crate::error::TypeError {
        let chain = pending.chain.join(" → ");
        let mut err = crate::error::TypeError::new(
            TypeErrorKind::InvalidOperation,
            pending.report_span.clone(),
            format!(
                "structural-equality obligations for this instantiation could not be \
                 discharged: the generic instantiation chain exceeded {budget} hops \
                 ({chain}). The checker refuses rather than hand an unanalysed \
                 instantiation to codegen.",
            ),
        )
        .with_suggestion(
            "break the generic call chain — give an intermediate function a concrete type \
             argument, or move the comparison to a non-generic helper"
                .to_string(),
        );
        if let Some(module) = pending.report_module.clone() {
            err = err.with_source_module(module);
        }
        err
    }

    pub(in crate::check) fn selected_eq_available(service: &mut TypeFactService, ty: &Ty) -> bool {
        ResolvedTy::from_ty(ty).ok().is_some_and(|resolved| {
            service
                .capability_plan(&resolved, crate::ValueCapability::Eq)
                .is_ok_and(|selection| selection.is_some())
        })
    }

    pub(super) fn check_concrete_eq_requirements(
        &self,
        requirements: &HashMap<Option<String>, Vec<EqRequirement>>,
        service: &mut TypeFactService,
    ) -> Vec<crate::error::TypeError> {
        let mut new_errors = Vec::new();
        let mut demands: Vec<_> = requirements.values().flatten().collect();
        demands.sort_by_key(|demand| (&demand.source_module, demand.span.start, demand.span.end));
        for requirement in demands {
            let concrete = self
                .normalize_for_use(&requirement.ty)
                .materialize_literal_defaults();
            if concrete.contains_error()
                || concrete.has_inference_var()
                || concrete.contains_assoc_type()
                || Self::ty_mentions_type_params(&concrete, &requirement.owner_type_params)
            {
                continue;
            }
            if !Self::selected_eq_available(service, &concrete) {
                let mut error = crate::error::TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    requirement.span.clone(),
                    format!(
                        "`{}` has no selected Eq implementation for equality comparison",
                        concrete.user_facing()
                    ),
                );
                if let Some(module) = &requirement.source_module {
                    error = error.with_source_module(module.clone());
                }
                new_errors.push(error);
            }
        }
        new_errors
    }
}
