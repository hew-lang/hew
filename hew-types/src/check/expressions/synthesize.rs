//! Checker methods grouped by responsibility: synthesize.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
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
    /// Synthesize: infer the type of an expression (bottom-up).
    pub(in crate::check) fn synthesize(&mut self, expr: &Expr, span: &Span) -> Ty {
        if self.deferred_body.is_some()
            && matches!(
                expr,
                Expr::ReturnError(_)
                    | Expr::PostfixTry(_)
                    | Expr::Await(_)
                    | Expr::AwaitRestart(_)
                    | Expr::Yield(_)
                    | Expr::ScopeDeadline { .. }
                    | Expr::ForkChild { .. }
                    | Expr::ForkBlock { .. }
                    | Expr::Select { .. }
                    | Expr::Race(_)
            )
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "a deferred body cannot suspend or propagate an error out of its scope".to_string(),
            );
            return Ty::Error;
        }
        // Synthesis runs without an expected type, so no expression reached
        // through `synthesize` is in `check_against` tail position. Clear the
        // tail Ok-coercion flag for the duration so a nested expression (an
        // operand, argument, or non-tail statement) can never trip the
        // coercion. The flag is only meaningful on the `check_against` path.
        let prev_tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        // Grow the stack on demand so deeply-nested expressions (e.g. 1000+
        // chained binary operators) don't overflow.
        let result = stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
            self.synthesize_inner(expr, span)
        });
        self.tail_ok_armed = prev_tail_ok_armed;
        self.publish_checked_expression(expr, span, result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression check covers all AST variants"
    )]
    pub(in crate::check) fn synthesize_inner(&mut self, expr: &Expr, span: &Span) -> Ty {
        self.reject_if_wasm_incompatible_expr(expr, span);
        let ty = match expr {
            // Literals
            Expr::Literal(Literal::Float(_)) => Ty::FloatLiteral,
            Expr::Literal(Literal::String(_)) => Ty::String,
            Expr::RegexLiteral(pattern) => {
                // The implicit `use std::text::regex` injected by the CLI is the
                // provider of this type; mark it as used so the unused-import
                // check doesn't fire a false-positive warning.
                self.used_modules.borrow_mut().insert(ImportKey::in_file(
                    self.current_module.clone(),
                    self.current_module_idx,
                    "regex",
                ));
                // Validate the pattern using the same regex engine the runtime
                // uses. An invalid pattern is a compile-time hard error.
                if let Err(err) = regex::Regex::new(pattern) {
                    self.report_error(
                        TypeErrorKind::InvalidRegexLiteral {
                            pattern: pattern.clone(),
                            error: err.to_string(),
                        },
                        span,
                        format!("invalid regex literal `re\"{pattern}\"`: {err}"),
                    );
                }
                self.named_ty_for_key("std.text.regex.Pattern", vec![])
            }
            Expr::ByteStringLiteral(_) | Expr::ByteArrayLiteral(_) => Ty::Bytes,
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    match part {
                        StringPart::Literal(_) => {}
                        StringPart::Expr((expr, expr_span)) => {
                            let part_ty = self.synthesize(expr, expr_span);
                            self.require_display_impl(&part_ty, expr_span);
                        }
                        StringPart::StructuralExpr((expr, expr_span)) => {
                            let part_ty = self.synthesize(expr, expr_span);
                            if let Some(display_ty) = self.display_impl_type(&part_ty) {
                                self.interpolation_display_types.insert(
                                    SpanKey::in_module(expr_span, self.current_module_idx),
                                    display_ty,
                                );
                            } else {
                                self.require_structural_render(&part_ty, expr_span);
                            }
                        }
                    }
                }
                Ty::String
            }
            Expr::Literal(Literal::Bool(_)) => Ty::Bool,
            Expr::Literal(Literal::Char(_)) => Ty::Char,
            Expr::Literal(Literal::Integer { .. }) => Ty::IntLiteral,
            Expr::Literal(Literal::Duration(_)) => Ty::Duration,

            // Builtin `None`: synthesize `Option<fresh>` and fall through to the
            // universal `record_type` tail (line ~505) like every other arm.
            // The fresh tyvar is resolved by the surrounding context (fn return,
            // let-binding, match scrutinee) during unification; the
            // `check_program` boundary resolve (mod.rs:262/364) then writes back
            // the post-substitution concrete `Option<T>` at this span. Recording
            // here — instead of the old early `return`, which bypassed
            // `record_type` and left HIR's unit-ctor fallback to stamp a bare
            // `Named{Option, args:[]}` (→ codegen D10) — converges builtin `None`
            // onto the same record-and-resolve substrate as the user-`TypeDecl`
            // unit-variant path (`check_against`, expressions.rs:2453). A
            // genuinely-unconstrained `None` still fails closed: the recorded
            // `Option<Var>` stays unresolved and `validate_expr_output_contract`
            // (admissibility.rs) surfaces it as an inference error. See W4.042.
            Expr::Ident(name) if name.name.as_str() == "None" => {
                self.report_bare_variant_expr(name.name.as_str(), "Option.None", span);
                Ty::option(Ty::Var(TypeVar::fresh()))
            }
            Expr::Ident(name) => self.synthesize_identifier(name.name.as_str(), span),
            Expr::ContextVariant(context) => {
                if let Some(record) = &context.record {
                    for (_, value) in &record.fields {
                        self.synthesize(&value.0, &value.1);
                    }
                    if let Some(base) = &record.base {
                        self.synthesize(&base.0, &base.1);
                    }
                }
                self.report_error(
                    TypeErrorKind::ContextVariantNoType,
                    span,
                    format!(
                        "E_CONTEXT_VARIANT_NO_TYPE: contextual variant `.{}` requires an expected enum or machine type",
                        context.name
                    ),
                );
                Ty::Error
            }
            Expr::GenericApplySuffix { target, type_args } => match &target.0 {
                Expr::Ident(name) => self.synthesize_identifier_with_type_args(
                    name.name.as_str(),
                    Some(type_args),
                    span,
                ),
                Expr::FieldAccess { object, field } => self.check_field_access_with_type_args(
                    object,
                    field.0.name.as_str(),
                    Some(type_args),
                    span,
                ),
                _ => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "explicit type arguments require a function declaration".to_string(),
                    );
                    Ty::Error
                }
            },
            Expr::RecordInitSuffix {
                target,
                fields,
                base,
            } => {
                let target_ty = self.synthesize(&target.0, &target.1);
                for (_, (value, value_span)) in fields {
                    self.synthesize(value, value_span);
                }
                if let Some(base) = base {
                    self.synthesize(&base.0, &base.1);
                }
                target_ty
            }
            Expr::QualifiedAssoc(path) => self.synthesize_qualified_assoc(path, span),

            // Binary ops
            Expr::Binary { left, op, right } => self.check_binary_op(left, *op, right, span),

            // Unary ops
            Expr::Unary { op, operand } => self.synthesize_unary_op(*op, operand, span),

            // `clone <operand>` — explicit duplication. Resolved exactly like
            // `<operand>.clone()`: method resolution decides cloneability and
            // the result type (checker authority), the operand is read
            // non-consumingly, and the same side tables are recorded at this
            // span so HIR lowering can reuse the `.clone()` lowering path.
            // Types with no clone path fail closed downstream with the existing
            // clone diagnostic.
            Expr::Clone(operand) => self.check_method_call(operand, "clone", &[], span),

            // Call
            Expr::Call {
                function,
                type_args,
                args,
                is_tail_call: _,
            } => {
                let ty = self.check_call(function, type_args.as_deref(), args, span);
                self.finish_named_arguments(args, || Self::callee_label(function), &ty, span);
                ty
            }

            // Method call
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                let ty = self.check_method_call(receiver, method.0.name.as_str(), args, span);
                self.finish_named_arguments(args, || format!("method `{}`", method.0), &ty, span);
                ty
            }

            // Field access
            Expr::FieldAccess { object, field } => {
                self.check_field_access(object, field.0.name.as_str(), span)
            }

            // Block
            Expr::Block(block) => self.check_block(block, None),

            // If expression
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let entry = self.env.ownership_snapshot();
                let then_ty = self.synthesize(&then_block.0, &then_block.1);
                let then_exit = BranchArmExit {
                    ownership: self.env.ownership_snapshot(),
                    diverges: Self::arm_skips_join(&then_ty),
                };
                if let Some(eb) = else_block {
                    self.env.restore_ownership(&entry);
                    let else_ty = self.synthesize(&eb.0, &eb.1);
                    let else_exit = BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&else_ty),
                    };
                    self.join_branch_ownership(&entry, &[then_exit, else_exit]);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    // No `else`: the implicit fall-through arm runs with the
                    // state the condition left behind and never consumes.
                    self.join_fall_through(&entry, then_exit);
                    Ty::Unit
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => self.synthesize_iflet(conditions, body, else_body.as_deref(), span),

            // Match
            Expr::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, scrutinee, arms, span, None)
            }

            // Tuple
            Expr::Tuple(elems) => {
                if elems.is_empty() {
                    Ty::Unit
                } else {
                    let tys: Vec<_> = elems
                        .iter()
                        .map(|(e, s)| {
                            let ty = self.synthesize(e, s);
                            self.record_value_transfer(e, s);
                            ty
                        })
                        .collect();
                    Ty::Tuple(tys)
                }
            }

            // Array
            Expr::Array(elems) => self.synthesize_array_literal(elems, span),
            Expr::ArrayRepeat { value, count } => self.synthesize_array_repeat(value, count, span),

            Expr::MapLiteral { entries } => self.synthesize_map_literal(entries, span),

            // Struct init
            Expr::StructInit {
                path,
                fields,
                type_args,
                base,
            } => self.check_struct_init(
                &path.to_string(), // TRANSITION(P1): deleted by A1 commit 2
                fields,
                type_args.as_deref(),
                base.as_deref(),
                span,
            ),

            // Spawn
            Expr::Spawn {
                target,
                type_args,
                args,
            } => self.check_spawn(target, type_args, args, span),

            // Lambda (synthesize mode — no expected type)
            Expr::Lambda {
                is_move,
                private_captures,
                type_params,
                params,
                return_type,
                body,
                ..
            } => self.check_lambda(
                *is_move,
                private_captures,
                type_params.as_deref(),
                params,
                return_type.as_ref(),
                body,
                None,
                span,
                false,
                false,
            ),

            // Await
            Expr::Await(inner) => {
                // Locate a directly awaited method through a transparent block
                // so suspension permission belongs to the call's exact span.
                let (effective_expr, effective_span) = match &inner.0 {
                    Expr::Block(block)
                        if block.stmts.is_empty()
                            && block
                                .trailing_expr
                                .as_deref()
                                .is_some_and(|(e, _)| matches!(e, Expr::MethodCall { .. })) =>
                    {
                        let trailing = block.trailing_expr.as_deref().unwrap();
                        (&trailing.0, &trailing.1)
                    }
                    _ => (&inner.0, &inner.1),
                };

                self.suspension_operands
                    .insert(SpanKey::in_module(effective_span, self.current_module_idx));
                let inner_ty = self.synthesize(&inner.0, &inner.1);

                // Join one Task layer; `await` joins tasks and nothing else. A
                // `Vec<Task<T>>` joins every task in order and yields `Vec<T>`;
                // the vector and each handle in it are consumed by the join.
                match inner_ty {
                    Ty::Task(output) => {
                        if !self.reject_borrowed_consumption(&inner.0, &inner.1) {
                            self.mark_expr_moved(&inner.0, &inner.1);
                        }
                        *output
                    }
                    ref vector if vec_task_output(vector).is_some() => {
                        let output = vec_task_output(vector).expect("matched a vector of tasks");
                        if !self.reject_borrowed_consumption(&inner.0, &inner.1) {
                            self.mark_expr_moved(&inner.0, &inner.1);
                        }
                        self.make_vec_type(output, span)
                    }
                    other => {
                        self.check_await_operand(effective_expr, effective_span, &other);
                        other
                    }
                }
            }

            // AwaitRestart: `await_restart <supervised-child>` — suspend until the
            // named slot is Live again, then resume with the same stable
            // `ChildRef<ChildType>`. The operand names one slot: a static child
            // accessor (recorded in `supervisor_child_slots`, kind `Static`) or
            // one pool member (`sup.pool[i]`, recorded in `pool_accessor_sites`
            // as `Index`). A whole pool names many slots and has no single
            // restart signal, so it is refused. The result type is the same
            // `ChildRef<ChildType>` — by construction the slot is Live after a
            // completed restart; a permanently-Dead child fails closed at
            // runtime (resumes immediately) rather than hanging, so the bare
            // form never yields an `Option`.
            Expr::AwaitRestart(inner) => {
                // Synthesize the operand first; this records the supervisor child
                // slot and pool accessor side-table entries keyed by the inner
                // expression's span.
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                let inner_key = SpanKey::in_module(&inner.1, self.current_module_idx);
                let member = matches!(
                    self.pool_accessor_sites
                        .get(&inner_key)
                        .map(|accessor| accessor.kind),
                    Some(crate::check::types::PoolAccessorKind::Index)
                );
                if member {
                    // One pool member's own slot: the same `ChildRef<ChildType>`
                    // the indexed accessor produced.
                    inner_ty
                } else {
                    match self.supervisor_child_slots.get(&inner_key).cloned() {
                        Some(slot) if slot.kind == crate::check::types::ChildKind::Static => {
                            // Stable role handle: same `ChildRef<ChildType>` the
                            // accessor produced. Carry the discriminator forward — the
                            // side-table entry already keys MIR lowering on this span.
                            inner_ty
                        }
                        Some(_pool_slot) => {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                "`await_restart` waits on one supervised slot; a pool \
                                 names many, so wait on a member with \
                                 `await_restart sup.pool[i]`"
                                    .to_string(),
                            );
                            Ty::Error
                        }
                        None => {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                "`await_restart` expects a supervised-child accessor \
                                 (`await_restart sup.child` or `await_restart \
                                 sup.pool[i]`); its operand is not a supervisor \
                                 child slot"
                                    .to_string(),
                            );
                            Ty::Error
                        }
                    }
                }
            }

            // PostfixTry: expr? → unwrap Result/Option
            Expr::PostfixTry(inner) => {
                let ty = self.synthesize(&inner.0, &inner.1);
                let ty = self.subst.resolve(&ty);
                // Build an error message if the enclosing function's return type
                // cannot propagate via `?`.  Computed before any mutable borrow.
                //
                // JUSTIFIED: Ty::Var(_) is bypassed because the return type is
                // still being inferred — reporting a context error would be a false
                // positive.  Ty::Error is bypassed for the same reason: it means the
                // return-type annotation failed to resolve (e.g. references an
                // unknown type), so we cannot know whether `?` propagation would be
                // valid.  The annotation-resolution error is already reported
                // separately; adding a second "? cannot be used here" error would be
                // confusing rather than helpful.  Inner-type errors ("`?` requires
                // Result or Option, found X`") are reported unconditionally via the
                // else branch below and are NOT affected by this bypass.
                //
                // Ty::Named where the name is not builtin and not in type_defs or
                // type_aliases is also bypassed: this arises when a return-type
                // annotation references an undefined type (resolution falls through
                // to Ty::normalize_named rather than returning Ty::Error). Emitting
                // the context error in this case is a false positive — we cannot
                // know whether the intended type would have been a Result/Option.
                let current_return_type = self.current_return_type.clone();
                let bad_ctx_msg: Option<String> = current_return_type.as_ref().and_then(|ret| {
                    let r = self.subst.resolve(ret);
                    if (r.as_option().is_some() && ty.as_option().is_some())
                        || (r.as_result().is_some() && ty.as_result().is_some())
                        || matches!(r, Ty::Var(_) | Ty::Error)
                        || matches!(&r, Ty::Named { head, .. }
                                if head.builtin().is_none()
                                    && self.type_def_at(head.registry_key()).is_none()
                                    && !self.type_aliases.contains_key(head.registry_key()))
                    {
                        None
                    } else {
                        Some(format!(
                            "`?` cannot be used in a function returning `{r}` to propagate `{ty}`; \
                             absence requires an Option return and errors require a Result return"
                        ))
                    }
                });
                if let Some(inner_ty) = ty.as_option() {
                    if let Some(msg) = bad_ctx_msg {
                        self.report_error(TypeErrorKind::InvalidOperation, span, msg);
                        Ty::Error
                    } else {
                        inner_ty.clone()
                    }
                } else if let Some((ok, err)) = ty.as_result() {
                    let ok_ty = ok.clone();
                    let err_ty = err.clone();
                    if let Some(msg) = bad_ctx_msg {
                        self.report_error(TypeErrorKind::InvalidOperation, span, msg);
                        Ty::Error
                    } else {
                        if let Some(ret) = current_return_type.as_ref() {
                            let resolved_ret = self.subst.resolve(ret);
                            if let Some((_, ret_err)) = resolved_ret.as_result() {
                                let ret_err = ret_err.clone();
                                let err_ty = self.subst.resolve(&err_ty);
                                if !matches!(ret_err, Ty::Error) && !matches!(err_ty, Ty::Error) {
                                    let snapshot = self.subst.snapshot();
                                    if !self.try_unify_with_owner_identity(&ret_err, &err_ty) {
                                        self.subst.restore(snapshot);
                                        self.report_error(
                                            TypeErrorKind::InvalidOperation,
                                            span,
                                            format!(
                                                "`?` error type mismatch: expected `{}`, found `{}`",
                                                ret_err.user_facing(),
                                                err_ty.user_facing()
                                            ),
                                        );
                                        return Ty::Error;
                                    }
                                }
                            }
                        }
                        ok_ty
                    }
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`?` requires Result or Option, found `{}`",
                            ty.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }

            Expr::Coalesce { left, right } => self.check_local_recovery(left, right, None, span),
            Expr::Handle {
                operand,
                error,
                body,
            } => self.check_local_recovery(operand, body, Some(error), span),

            // Yield
            Expr::Yield(value) => self.synthesize_yield(value.as_deref(), span),

            // `return [expr]` in expression position. A `return` diverges, so
            // the construct itself synthesizes to `Ty::Never` (which unifies
            // with any expected type). The operand is checked against the
            // enclosing function's declared return type via the SAME shared
            // shell as statement-position `Stmt::Return`
            // (LESSONS `one-construct-one-lowering-shell`) — never against this
            // expression's expected type, so a mismatched `return` operand is
            // attributed to the return, not the surrounding expression.
            Expr::Return(value) => {
                self.check_return_operand(value.as_deref(), span);
                Ty::Never
            }
            Expr::ReturnError(value) => {
                let error = self.current_return_type.as_ref().and_then(|ty| {
                    self.subst
                        .resolve(ty)
                        .as_result()
                        .map(|(_, error)| error.clone())
                });
                if let Some(error) = error.filter(|_| self.current_fails) {
                    self.check_against(&value.0, &value.1, &error);
                    self.result_return_coercions.insert(
                        SpanKey::in_module(span, self.current_module_idx),
                        super::ResultReturnKind::Error,
                    );
                } else {
                    self.synthesize(&value.0, &value.1);
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`return error` requires an enclosing function declared with `fails`"
                            .to_string(),
                    );
                }
                self.recheck_return_edge_defers();
                Ty::Never
            }

            // Index
            Expr::Index { object, index } => {
                self.synthesize_index(object, index, span, IndexContext::Read)
            }

            // Range
            Expr::Range {
                start,
                end,
                inclusive: _,
            } => self.synthesize_range(start.as_deref(), end.as_deref()),

            // Cast expression: `expr as Type`
            Expr::Cast {
                expr: inner,
                ty: type_expr,
            } => self.synthesize_cast(inner, type_expr, span),

            // Identity comparison: `lhs is rhs` (slice D-2).
            //
            // Allowed receivers: actors/actor refs and heap-backed
            // `Vec`/`HashMap`/`HashSet`/`bytes`.
            // Rejected with `E_IS_VALUE_TYPE`: scalars, `String`, `type
            // Foo { ... }` record declarations, `record` types, enum
            // declarations (`indirect` included), machines, tuples,
            // ranges, fn/closures.
            //
            // Result is always `bool`. Cross-class mismatches (e.g.
            // `<actor handle> is Vec<int>`) collapse into a single
            // `TypeErrorKind::Mismatch` diagnostic that requires the operands
            // share the same resolved type. Move/consumed-self semantics
            // follow the existing use-after-move rule (plan §D-D4, Q-N3).
            Expr::Is { lhs, rhs } => self.synthesize_is(lhs, rhs, span),

            _ => self.synthesize_concurrency(expr, span),
        };

        self.record_type(span, &ty);
        ty
    }

    pub(in crate::check) fn synthesize_unary_op(
        &mut self,
        op: UnaryOp,
        operand: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        match op {
            UnaryOp::Not => {
                self.check_against(&operand.0, &operand.1, &Ty::Bool);
                Ty::Bool
            }
            UnaryOp::Negate => {
                let ty = self.synthesize(&operand.0, &operand.1);
                let resolved = self.subst.resolve(&ty);
                if !resolved.is_numeric()
                    && !resolved.is_duration()
                    && !matches!(resolved, Ty::Var(_))
                    && resolved != Ty::Error
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot negate type `{}`", resolved.user_facing()),
                    );
                }
                ty
            }
            UnaryOp::BitNot => {
                let ty = self.synthesize(&operand.0, &operand.1);
                let resolved = self.subst.resolve(&ty);
                if !resolved.is_integer()
                    && !matches!(resolved, Ty::Var(_))
                    && resolved != Ty::Error
                {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "bitwise NOT requires integer type, found `{}`",
                            resolved.user_facing()
                        ),
                    );
                }
                ty
            }
            UnaryOp::RawDeref => {
                // Raw-pointer dereference is fail-closed.
                //
                // - Outside `unsafe { ... }` we emit
                //   `UnsafeOperationRequiresBlock` so the user is told to
                //   wrap the operation, matching the diagnostic that
                //   extern fn calls already produce.
                // - Inside `unsafe { ... }` we still reject with
                //   `RawPointerOpNotLowered` because the compiler has no
                //   HIR/MIR/codegen lowering for raw-pointer operations.
                //   Envelope code: `E_M5_RAW_POINTER_OP_NOT_LOWERED`.
                //
                // We still synthesize the operand so a malformed
                // sub-expression still produces a useful diagnostic.
                let _ = self.synthesize(&operand.0, &operand.1);
                if self.in_unsafe {
                    self.report_error(
                        TypeErrorKind::RawPointerOpNotLowered {
                            operation: "raw pointer dereference".to_string(),
                        },
                        span,
                        "raw pointer dereference is not lowered to HIR/MIR/codegen".to_string(),
                    );
                } else {
                    self.report_error(
                        TypeErrorKind::UnsafeOperationRequiresBlock {
                            operation: "raw pointer dereference".to_string(),
                        },
                        span,
                        "raw pointer dereference requires an `unsafe { ... }` block".to_string(),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn synthesize_range(
        &mut self,
        start: Option<&Spanned<Expr>>,
        end: Option<&Spanned<Expr>>,
    ) -> Ty {
        match (start, end) {
            (Some(s), Some(e)) => {
                let start_ty = self.synthesize(&s.0, &s.1);
                let end_ty = self.synthesize(&e.0, &e.1);
                let start_resolved = self.subst.resolve(&start_ty);
                let end_resolved = self.subst.resolve(&end_ty);

                if start_resolved.is_integer() && end_resolved.is_integer() {
                    if let Some(common_ty) =
                        common_integer_type(&start_resolved, &end_resolved, self.pointer_width())
                    {
                        Ty::range(common_ty)
                    } else {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &s.1,
                            format!(
                                "range bounds require compatible integer types; found `{}` and `{}`",
                                start_resolved.user_facing(),
                                end_resolved.user_facing()
                            ),
                        );
                        Ty::Error
                    }
                } else {
                    self.expect_type(&start_ty, &end_ty, &e.1);
                    Ty::range(start_ty)
                }
            }
            (Some(s), None) => Ty::range(self.synthesize(&s.0, &s.1)),
            (None, Some(e)) => Ty::range(self.synthesize(&e.0, &e.1)),
            (None, None) => Ty::range(Ty::I64),
        }
    }

    pub(in crate::check) fn synthesize_cast(
        &mut self,
        inner: &Spanned<Expr>,
        type_expr: &Spanned<TypeExpr>,
        span: &Span,
    ) -> Ty {
        let actual = self.synthesize(&inner.0, &inner.1);
        let (target, hole_vars) = self.resolve_annotation_holes(type_expr);
        let has_target_holes = !hole_vars.is_empty();
        if has_target_holes {
            self.record_deferred_inference_holes(type_expr, "cast target type", hole_vars.clone());
            self.record_deferred_cast_check(span, &actual, &target, hole_vars);
        }
        let actual_resolved = self.subst.resolve(&actual);
        let target_resolved = self.subst.resolve(&target);

        if !has_target_holes && !cast_is_valid(&actual_resolved, &target_resolved) {
            self.report_error(
                TypeErrorKind::Mismatch {
                    expected: target_resolved.user_facing().to_string(),
                    actual: actual_resolved.user_facing().to_string(),
                },
                span,
                format!(
                    "cannot cast `{}` to `{}`",
                    actual_resolved.user_facing(),
                    target_resolved.user_facing()
                ),
            );
        }

        self.record_type(span, &target);
        target
    }

    /// Build a [`ConstEnv`](crate::check::const_eval::ConstEnv) snapshot of the
    /// integer `const` bindings currently in scope, for constexpr count/length
    /// evaluation (e.g. the fixed-array repeat-length check). Float consts are
    /// deliberately excluded — the const-eval sub-engine is integer-only.
    ///
    /// Only bindings with declared-const provenance (`declared_const_bindings`)
    /// are admitted. The current lexical binding must match the declared
    /// constant's binding ID, so a local or parameter cannot inherit a
    /// same-named module constant's value. `const_values` also holds
    /// literal-coercion entries for every unannotated immutable integer literal
    /// (`let n = 4`), whose *value* is not a compile-time constant for length
    /// purposes; admitting those would let a runtime-shaped local silently
    /// satisfy a fixed-array length.
    pub(super) fn const_eval_env(&self) -> crate::check::const_eval::ConstEnv {
        let mut env = crate::check::const_eval::ConstEnv::new();
        for (name, value) in &self.const_values {
            let Some(declared_binding_id) = self.declared_const_bindings.get(name) else {
                continue;
            };
            if self.env.lookup_ref(name).map(|binding| binding.id) != Some(*declared_binding_id) {
                continue;
            }
            if let ConstValue::Integer(v) = value {
                env.insert(name.clone(), *v);
            }
        }
        env
    }

    pub(in crate::check) fn synthesize_array_repeat(
        &mut self,
        value: &Spanned<Expr>,
        count: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let elem_ty = self.synthesize(&value.0, &value.1);
        let count_ty = self.check_against(&count.0, &count.1, &Ty::I64);
        let resolved_count = self.subst.resolve(&count_ty);
        if !resolved_count.is_integer() && !matches!(resolved_count, Ty::Var(_)) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &count.1,
                format!(
                    "array repeat count must be an integer, found `{}`",
                    resolved_count.user_facing()
                ),
            );
        }
        if let Expr::Literal(Literal::Integer { value, .. }) = &count.0 {
            if *value < 0 {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &count.1,
                    "array repeat count cannot be negative".to_string(),
                );
            }
        }
        // An array repeat copies the element into every slot, so it admits
        // exactly the element types the value class gives a copy path — the
        // same answer `xs[i]`, a range slice and cloning iteration get. A
        // trait object and a `Stream` are drop-only in the class table
        // (`CloneKind::None`), so they refuse here without a second rule.
        if let Some(blocker) = self.element_clone_blocker(&elem_ty) {
            if let Some(param) = blocker.unbounded_param() {
                let param = param.to_string();
                self.report_unbounded_param_copy(&param, "[T; N]", span);
                return self.make_vec_type(elem_ty, span);
            }
            let blocker = blocker.concrete_text();
            let resolved_elem = self.subst.resolve(&elem_ty);
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "E_ELEMENT_NO_COPY: `[{elem}; N]` array repeat copies the element into \
                     every slot, but {blocker} has no copy operation; use an explicit loop \
                     that builds each element, or a Copy element type",
                    elem = resolved_elem.user_facing()
                ),
            );
        }
        self.make_vec_type(elem_ty, span)
    }

    /// Type one lazy recovery branch without introducing a callable boundary.
    /// The success path preserves the state after the operand; only the other
    /// path evaluates the fallback or binds the Result error.
    pub(super) fn check_local_recovery(
        &mut self,
        operand: &Spanned<Expr>,
        body: &Spanned<Expr>,
        error: Option<&Spanned<Ident>>,
        span: &Span,
    ) -> Ty {
        let container = self.synthesize(&operand.0, &operand.1);
        let container = self.subst.resolve(&container);
        let scope_recovery =
            error.is_some() && matches!(operand.0, Expr::Scope { .. } | Expr::ScopeDeadline { .. });
        let (payload, error_ty) = if scope_recovery {
            Some((
                container.clone(),
                self.named_ty_for_key("std.builtins.ScopeFailure", Vec::new()),
            ))
        } else if error.is_some() {
            container
                .as_result()
                .map(|(ok, err)| (ok.clone(), err.clone()))
        } else {
            container.as_option().map(|some| (some.clone(), Ty::Unit))
        }
        .unwrap_or_else(|| {
            if !matches!(container, Ty::Error) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    if error.is_some() {
                        format!(
                            "`handle` requires Result, found `{}`",
                            container.user_facing()
                        )
                    } else {
                        format!(
                            "`??` requires Option, found `{}`; handle Result errors explicitly",
                            container.user_facing()
                        )
                    },
                );
            }
            (Ty::Error, Ty::Error)
        });
        let recovery_kind = if scope_recovery {
            super::RecoveryKind::Scope {
                failure_ty: ResolvedTy::from_ty(&error_ty)
                    .expect("ScopeFailure is a concrete source-defined enum"),
            }
        } else if error.is_some() {
            super::RecoveryKind::Result
        } else {
            super::RecoveryKind::Option
        };
        self.recovery_kinds.insert(
            SpanKey::in_module(span, self.current_module_idx),
            recovery_kind,
        );
        let entry = self.env.ownership_snapshot();
        self.env.push_scope();
        if let Some((name, binding_span)) = error {
            self.check_shadowing(name.name.as_str(), binding_span);
            self.env
                .define_with_span(name.to_string(), error_ty, false, binding_span.clone());
        }
        let body_ty = if payload == Ty::Never {
            self.synthesize(&body.0, &body.1)
        } else {
            self.check_expr_with_expected(&body.0, &body.1, &payload)
        };
        let taken = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join(&body_ty),
        };
        self.env.pop_scope();
        self.join_fall_through(&entry, taken);
        let payload = self.subst.resolve(&payload);
        if payload == Ty::Never {
            self.subst.resolve(&body_ty)
        } else {
            payload
        }
    }

    /// `Vec<elem_ty>` without the concrete-element validation `make_vec_type`
    /// performs: used to build an expectation for a spread operand, where the
    /// element type may still be an inference variable.
    pub(super) fn vec_of(elem_ty: Ty) -> Ty {
        Ty::Named {
            head: crate::TypeHead::Builtin(BuiltinType::Vec),
            args: vec![elem_ty],
        }
    }

    pub(in crate::check) fn synthesize_array_literal(
        &mut self,
        elements: &[ArrayElement],
        span: &Span,
    ) -> Ty {
        let mut elem_ty: Option<Ty> = None;
        let mut spread_span: Option<Span> = None;
        for element in elements {
            let (operand, operand_span) = element.expr();
            match element {
                ArrayElement::Value(_) => {
                    match elem_ty.clone() {
                        None => elem_ty = Some(self.synthesize(operand, operand_span)),
                        // Distinct closures and function items only meet in
                        // their erased callable type.
                        Some(current) if self.subst.resolve(&current).contains_callable() => {
                            let next = self.synthesize(operand, operand_span);
                            elem_ty =
                                Some(self.join_callable_values(&current, &next, operand_span));
                        }
                        Some(current) => {
                            self.check_against(operand, operand_span, &current);
                        }
                    }
                    self.record_value_transfer(operand, operand_span);
                }
                ArrayElement::Spread(_) => {
                    // A spread operand is a `Vec` of the literal's element
                    // type, so unifying against `Vec<T>` both infers `T` from
                    // the first spread and refuses a mismatched later one.
                    let current = elem_ty.clone().unwrap_or_else(|| Ty::Var(TypeVar::fresh()));
                    let want = Self::vec_of(current.clone());
                    self.check_against(operand, operand_span, &want);
                    elem_ty = Some(current);
                    spread_span.get_or_insert_with(|| operand_span.clone());
                }
            }
        }
        let elem_ty = elem_ty.unwrap_or_else(|| Ty::Var(TypeVar::fresh()));
        if let Some(spread_span) = spread_span {
            self.refuse_uncopyable_spread_element(&elem_ty, &spread_span);
        }
        self.make_vec_type(elem_ty, span)
    }

    pub(in crate::check) fn synthesize_map_literal(
        &mut self,
        entries: &[(Spanned<Expr>, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        if entries.is_empty() {
            let k = TypeVar::fresh();
            let v = TypeVar::fresh();
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                args: vec![Ty::Var(k), Ty::Var(v)],
            }
        } else {
            let (ref ke, ref ks) = entries[0].0;
            let (ref ve, ref vs) = entries[0].1;
            let first_key_ty = self.synthesize(ke, ks);
            let first_val_ty = self.synthesize(ve, vs);
            for (k, v) in &entries[1..] {
                self.check_against(&k.0, &k.1, &first_key_ty);
                self.check_against(&v.0, &v.1, &first_val_ty);
            }
            self.validate_hashmap_key_value_types(&first_key_ty, &first_val_ty, span);
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                args: vec![first_key_ty, first_val_ty],
            }
        }
    }

    pub(in crate::check) fn synthesize_iflet(
        &mut self,
        conditions: &[ConditionItem],
        body: &Block,
        else_body: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        let entry = self.env.ownership_snapshot();
        self.check_condition(conditions);
        let then_ty = self.check_block(body, None);
        let then_exit = BranchArmExit {
            ownership: self.env.ownership_snapshot(),
            diverges: Self::arm_skips_join(&then_ty),
        };
        self.env.pop_scope();
        if let Some(else_expr) = else_body {
            self.env.restore_ownership(&entry);
            let else_ty = self.synthesize(&else_expr.0, &else_expr.1);
            let else_exit = BranchArmExit {
                ownership: self.env.ownership_snapshot(),
                diverges: Self::arm_skips_join(&else_ty),
            };
            self.join_branch_ownership(&entry, &[then_exit, else_exit]);
            self.unify_branches(&then_ty, &else_ty, span)
        } else {
            self.join_fall_through(&entry, then_exit);
            Ty::Unit
        }
    }

    pub(in crate::check) fn synthesize_yield(
        &mut self,
        value: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        if !self.in_generator {
            self.report_error(
                TypeErrorKind::YieldOutsideGenerator,
                span,
                "`yield` outside of generator function".to_string(),
            );
        }
        if let Some(val_expr) = value {
            if let Some(return_ty) = &self.current_return_type {
                let resolved = self.subst.resolve(return_ty);
                let yield_ty = if let Some((yields, _)) = resolved.as_generator() {
                    yields.clone()
                } else {
                    resolved
                };
                self.check_against(&val_expr.0, &val_expr.1, &yield_ty);
            } else {
                self.synthesize(&val_expr.0, &val_expr.1);
            }
        }
        Ty::Unit
    }

    pub(in crate::check) fn synthesize_identifier(&mut self, name: &str, span: &Span) -> Ty {
        self.synthesize_identifier_with_type_args(name, None, span)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single dispatch over all identifier forms (context readers, module-qualified variants, bindings, fn sigs, constructors, type aliases); splitting would fragment shared error-reporting state"
    )]
    pub(super) fn synthesize_identifier_with_type_args(
        &mut self,
        name: &str,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> Ty {
        if type_args.is_some() && self.env.lookup_ref(name).is_some() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "explicit type arguments require a function declaration, not a value binding"
                    .to_string(),
            );
            return Ty::Error;
        }
        if let Some(reader) = ExecutionContextReader::from_surface_name(name) {
            if self.in_actor_handler_context {
                return reader.ty();
            }
            self.report_error(
                TypeErrorKind::ContextReaderOutsideHandler,
                span,
                format!(
                    "context reader `{}` is only available directly inside an actor handler body; \
                     nested lambdas and ordinary functions have no in-scope execution context",
                    reader.surface_name()
                ),
            );
            return Ty::Error;
        }
        if name.starts_with('@') {
            self.report_error(
                TypeErrorKind::UndefinedVariable,
                span,
                format!(
                    "unknown context reader `{name}`; valid readers are @actor_id, \
                     @supervisor, and @trace_span"
                ),
            );
            return Ty::Error;
        }
        let Ok(canonical_lifecycle_name) =
            self.canonicalize_source_lifecycle_value_path(name, span)
        else {
            return Ty::Error;
        };
        // The lifecycle authority MINTS the canonical identity here; the
        // lexical spelling stays available for the surfaces that must split a
        // `module.Type::Variant` path into its parts. Splitting the minted
        // identity instead would read `std` as a module binding — a rendered
        // identity is never parsed back into one (rc1-F1 stage D).
        let surface_name = name;
        let name = canonical_lifecycle_name.as_deref().unwrap_or(name);
        if self.report_bare_const_scope_error(name, span) {
            return Ty::Error;
        }
        // Module-qualified value constructor reference encoded as a flat
        // `Identifier("module.Type::Variant")` by `parse_dot_postfix` when no
        // call-args or brace-body follow.  Dispatch to the fail-closed
        // module-aware checker before falling through to the generic
        // "undefined variable" path, which would produce a misleading error.
        //
        // Guard: only intercept when:
        //  - the module part isn't a known binding or local type (mirrors
        //    the check_field_access guard at line 3631)
        //  - the combined "module.Type" key is NOT already in type_defs
        //    (registered module-qualified types like "lifecycle.Lifecycle"
        //    are correctly resolved by resolve_identifier_variant via the
        //    type_defs flat-key path — don't short-circuit that path)
        if let Some(dot_pos) = surface_name.find('.') {
            let candidate_module = &surface_name[..dot_pos];
            let rest = &surface_name[dot_pos + 1..];
            if let Some(colon_pos) = rest.find("::") {
                let type_name = &rest[..colon_pos];
                let variant_name = &rest[colon_pos + 2..];
                let is_binding = self.env.lookup_ref(candidate_module).is_some();
                let is_known_type = self.type_def_at(candidate_module);
                let qualified_key = format!("{candidate_module}.{type_name}");
                let qualified_in_type_defs = self.type_def_at(&qualified_key);
                if !is_binding && is_known_type.is_none() && qualified_in_type_defs.is_none() {
                    return self.check_module_qualified_variant_ref(
                        candidate_module,
                        type_name,
                        variant_name,
                        span,
                    );
                }
            }
        }
        if let Some((depth, binding)) = self.env.lookup_with_depth(name) {
            let binding_id = binding.id;
            let is_moved = binding.is_moved;
            let deferred_init = binding.deferred_init();
            let moved_at = binding.moved_at.clone();
            let ty = binding.ty.clone();
            let def_span = binding
                .def_span
                .clone()
                .or_else(|| binding.shadow_span.clone());
            // The outermost place of an assignment target is written, not read:
            // `sock = Socket { .. }` after `sock.detach()` is the re-initialisation
            // that plugs the hole, not a use of the value that left.
            let is_write_target = self.place_write_depth > 0 && self.place_base_depth == 0;
            if !is_write_target {
                self.reject_crash_hook_consumed_state_read(binding_id, span);
            }
            if is_moved && deferred_init && !is_write_target {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "E_ACTOR_FIELD_UNINITIALIZED: state field `{name}` is read before \
                         `init` initializes it; assign it first"
                    ),
                );
            } else if is_moved && !is_write_target {
                let is_linear = matches!(
                    &ty,
                    Ty::Named { head, .. } if self.registry.is_linear(head.registry_key())
                );
                let mut err = TypeError::new(
                    if is_linear {
                        TypeErrorKind::UseAfterConsume
                    } else {
                        TypeErrorKind::UseAfterMove
                    },
                    span.clone(),
                    if is_linear {
                        format!("UseAfterConsume: use of consumed linear value `{name}`")
                    } else {
                        format!("use of moved value `{name}`")
                    },
                );
                if let Some(ref source_module) = self.current_module {
                    err = err.with_source_module(source_module.clone());
                }
                if let Some(moved_span) = moved_at {
                    err = err.with_note(moved_span, "value was consumed here");
                }
                // Substrate handles (Duplex, Sink, Stream, SendHalf, RecvHalf) are
                // affine: each consuming method (`.close()`, `.send_half()`,
                // `.recv_half()`, etc.) moves the handle exactly once. Subsequent
                // uses are rejected here. Name the type so the user knows why.
                if Self::ty_is_substrate_handle(&ty) {
                    err = err.with_suggestion(format!(
                        "`{}` is a substrate handle — consuming methods like `.close()`, \
                         `.send_half()`, and `.recv_half()` move the handle; \
                         use a single consuming call per binding",
                        ty.user_facing()
                    ));
                } else if is_linear {
                    err = err.with_suggestion(
                        "a `#[linear]` binding has exactly one ownership path; invoke its \
                         consuming method only once"
                            .to_string(),
                    );
                } else if self.registry.implements_marker(&ty, MarkerTrait::Clone) {
                    // The value's type has a clone path, so the canonical fix is
                    // to duplicate it before the consuming use and pass the copy.
                    err = err.with_suggestion(format!(
                        "duplicate `{name}` with `clone {name}` before the consuming use \
                         to keep the original usable"
                    ));
                }
                self.errors.push(err);
            }
            // A whole-value use of a partially-moved aggregate would hand a
            // second owner the storage that already moved out. Projection bases
            // are exempt (handled inside the reporter, which is the one
            // authority on that rule) and so are assignment targets, which
            // write rather than read.
            if !is_moved && !is_write_target {
                self.report_place_use_after_move(name, &[], span);
            }
            // A read inside a generator body captures into the generator frame.
            // `in_generator` covers `gen fn`, `receive gen fn` and `gen { }`,
            // and is cleared inside a nested lambda body, whose own capture
            // rule (`finish_closure_captures`) owns that boundary instead.
            if self.in_generator && !is_write_target {
                self.reject_borrowed_generator_capture(name, span);
            }
            // Track captures: variable from scope below the lambda boundary
            if let Some(capture_depth) = self.lambda_capture_depth {
                if depth < capture_depth {
                    self.lambda_captures.push(ty.clone());
                    self.lambda_capture_facts.push(ClosureCaptureFact {
                        binding_id,
                        name: name.to_string(),
                        ty: ty.clone(),
                        acquisition: crate::ClosureCaptureAcquisition::Snapshot,
                        access: crate::ClosureCaptureAccess::Read,
                        consumption: crate::ClosureCaptureConsumption::Retained,
                        is_send: false,
                        is_sync: false,
                        use_span: span.clone(),
                        def_span,
                    });
                }
            }
            ty
        } else if let Some(fn_sig_key) = self.visible_fn_signature_key(name) {
            // Function name used as a value (e.g., variant constructor)
            if let Some(source_identity) = self
                .import_fn_name_aliases
                .get(&(
                    self.current_module.clone(),
                    self.current_module_idx,
                    name.to_string(),
                ))
                .cloned()
            {
                self.reject_wasm_native_only_function_identity(&source_identity, span);
                if let Some((source_owner, _)) = source_identity.rsplit_once('.') {
                    self.mark_module_owner_bindings_used(source_owner);
                }
            }
            self.record_call_edge(&fn_sig_key);
            let sig = self.fn_sigs[&fn_sig_key].clone();
            // A bare enum variant used as a value (`let c = Red;`,
            // `xs.map(Wrap)`) is refused like its call form; nothing here
            // selects the enum, so the fix-it qualifies it.
            // A machine's states are written bare only inside that machine
            // (§3.11.3); elsewhere they follow the same rule (D550).
            if !surface_name.contains("::") {
                if let Some((owner, _, _)) =
                    self.lookup_variant_constructor(name)
                        .filter(|(owner, _, _)| {
                            self.type_def_at(owner)
                                .is_some_and(|td| td.kind == TypeDefKind::Enum)
                                && !self.machine_state_is_bare_here(owner)
                        })
                {
                    let replacement =
                        format!("{}.{name}", super::calls::variant_owner_spelling(&owner));
                    self.report_bare_variant_expr(name, &replacement, span);
                }
            }
            // local-shadows-global: when the fn_sig slot was won by a builtin enum
            // variant, prefer any user-declared enum that has a variant with the
            // same name (e.g. user `enum AppError { NotFound(string); }` shadows
            // the builtin `LookupError::NotFound` unit variant).
            if sig.is_builtin_variant {
                if let Some(user_ty) = self.find_user_variant_shadow_ty(name) {
                    return user_ty;
                }
            }
            if sig.params.is_empty() && self.let_identifier_is_unit_variant(name) {
                sig.return_type
            } else {
                self.instantiate_function_value(&fn_sig_key, type_args, span)
            }
        } else if self.module_binding_in_current_file(surface_name) {
            self.report_error(
                TypeErrorKind::ModuleUsedAsValue,
                span,
                format!("module `{surface_name}` cannot be used as a value"),
            );
            Ty::Error
        } else if self.type_def_at(surface_name).is_some()
            || self.known_types.contains(surface_name)
            || self.type_aliases.contains_key(surface_name)
            || crate::lookup_builtin_type(surface_name).is_some()
            || crate::ty::is_reserved_type_name(surface_name)
        {
            self.report_error(
                TypeErrorKind::TypeUsedAsValue,
                span,
                format!("type `{surface_name}` cannot be used as a value"),
            );
            Ty::Error
        } else {
            self.resolve_identifier_variant(name, span)
        }
    }

    pub(super) fn synthesize_qualified_assoc(
        &mut self,
        path: &hew_parser::ast::QualifiedAssocExpr,
        span: &Span,
    ) -> Ty {
        self.resolve_type_expr(&path.base);
        let Some(member) = path.members.first() else {
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                "qualified associated path requires an item name".to_string(),
            );
            return Ty::Error;
        };
        if path.members.len() != 1 {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                "qualified associated values cannot continue through another path segment"
                    .to_string(),
            );
            return Ty::Error;
        }

        let trait_name = path.trait_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
        let mut candidates = Vec::new();
        if self.trait_defs.contains_key(&trait_name) {
            candidates.push(trait_name.clone());
        } else if !trait_name.contains('.') && !trait_name.contains("::") {
            if let Some(owners) = self.published_bare_trait_owners.get(&(
                self.current_module.clone(),
                self.current_module_idx,
                trait_name.clone(),
            )) {
                candidates.extend(
                    owners
                        .iter()
                        .filter(|owner| self.trait_defs.contains_key(*owner))
                        .cloned(),
                );
            }
        }
        candidates.sort_unstable();
        candidates.dedup();

        if candidates.len() > 1 {
            self.report_error_with_suggestions(
                TypeErrorKind::AssocItemAmbiguous,
                span,
                format!(
                    "associated item `{member}` is ambiguous because trait `{trait_name}` has multiple imported owners"
                ),
                candidates
                    .iter()
                    .map(|candidate| format!("qualify the trait as `{candidate}`"))
                    .collect(),
            );
            return Ty::Error;
        }
        let Some(trait_key) = candidates.first() else {
            self.report_error(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("cannot resolve trait `{trait_name}` for associated item `{member}`"),
            );
            return Ty::Error;
        };
        let info = &self.trait_defs[trait_key];
        if info
            .associated_types
            .iter()
            .any(|associated| associated.name == member.name.as_str())
        {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                format!(
                    "associated item `{trait_key}.{member}` is a type and cannot be used as a value"
                ),
            );
            return Ty::Error;
        }
        if info.methods.iter().any(|method| method.name == *member) {
            self.report_error(
                TypeErrorKind::PathKindMismatch,
                span,
                format!(
                    "associated method `{trait_key}.{member}` requires method-call syntax on a value"
                ),
            );
            return Ty::Error;
        }
        self.report_error(
            TypeErrorKind::PathMemberNotFound,
            span,
            format!("trait `{trait_key}` has no associated item `{member}`"),
        );
        Ty::Error
    }

    #[allow(
        clippy::too_many_lines,
        reason = "index checking covers range slices, Vec runtime indexing, user Index impls, and dyn Index dispatch"
    )]
    pub(in crate::check) fn synthesize_index(
        &mut self,
        object: &Spanned<Expr>,
        index: &Spanned<Expr>,
        span: &Span,
        ctx: IndexContext,
    ) -> Ty {
        let obj_ty = self.synthesize(&object.0, &object.1);

        // C-3 range-slice (`xs[a..b]`, `xs[a..=b]`, `xs[..b]`, `xs[a..]`,
        // `xs[..]`): when the index is a range expression, the result type
        // is `Vec<T>` (a freshly-allocated copy) for `Vec<T>` receivers.
        // Each present endpoint must check against `i64`. Open endpoints
        // contribute no constraint; MIR fills them at lowering.
        // Other receivers (`Array<T, N>`, `Slice<T>`) are not supported by
        // this slice — the checker rejects with a typed-receiver diagnostic
        // that names Vec as the only supported receiver, mirroring C-2's
        // narrow surface.
        if let Expr::Range {
            start,
            end,
            inclusive: _,
        } = &index.0
        {
            if let Some(s) = start.as_deref() {
                self.check_against(&s.0, &s.1, &Ty::I64);
            }
            if let Some(e) = end.as_deref() {
                self.check_against(&e.0, &e.1, &Ty::I64);
            }
            return match &obj_ty {
                Ty::Named {
                    head: crate::TypeHead::Builtin(BuiltinType::Vec),
                    args,
                    ..
                } if !args.is_empty() => {
                    let element = args[0].clone();
                    if self.validate_vec_slice_element_clone_type(&element, span) {
                        obj_ty.clone()
                    } else {
                        Ty::Error
                    }
                }
                // W3 collections-sugar S2: `s[a..b]` over `string` returns a
                // fresh owned `string`. Codepoint-bounds slice, O(n), panic on
                // invalid bounds. Endpoints are i64 (validated above).
                Ty::String => Ty::String,
                // W3 collections-sugar S2: `b[a..b]` over `bytes` returns a
                // refcounted `bytes` slice. Byte-bounds, O(1), panic on
                // invalid bounds. Endpoints are i64 (validated above).
                Ty::Bytes => Ty::Bytes,
                _ => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot range-slice `{}`; range-slice syntax `xs[a..b]` is \
                             supported only for `Vec<T>`, `string`, and `bytes` receivers",
                            obj_ty.user_facing()
                        ),
                    );
                    Ty::Error
                }
            };
        }

        let resolved_obj = self.subst.resolve(&obj_ty);
        if let Some((_, child_ty)) = resolved_obj.as_supervisor_pool() {
            let idx_actual = self.synthesize(&index.0, &index.1);
            let idx_resolved = self.subst.resolve(&idx_actual);
            if Self::is_narrower_signed_int(&idx_resolved) {
                self.numeric_operand_coercions.insert(
                    SpanKey::in_module(&index.1, self.current_module_idx),
                    Ty::I64,
                );
            } else {
                self.check_against(&index.0, &index.1, &Ty::I64);
            }
            if ctx == IndexContext::AssignTarget {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "supervisor pool members cannot be assigned through indexed access".to_string(),
                );
                return Ty::Error;
            }
            self.pool_accessor_sites.insert(
                SpanKey::in_module(span, self.current_module_idx),
                crate::check::types::PoolAccessor {
                    kind: crate::check::types::PoolAccessorKind::Index,
                },
            );
            return Ty::child_ref(child_ty.clone());
        }
        if let Ty::TraitObject { traits } = &resolved_obj {
            for bound in traits {
                if bound.trait_name != "Index" {
                    continue;
                }
                self.check_against(&index.0, &index.1, &Ty::I32);
                if let Some((_, output_ty)) = bound
                    .assoc_bindings
                    .iter()
                    .find(|(name, _)| name == "Output")
                {
                    self.record_dyn_index_method_call(traits, bound, span);
                    return output_ty.clone();
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`[]` over `dyn Index` requires an `Output` associated-type binding"
                        .to_string(),
                );
                return Ty::Error;
            }
        }

        match &resolved_obj {
            // Vec keeps the existing runtime-backed indexing ABI. The std
            // `Index` impl exposes the trait surface, but MIR still owns the
            // bounds-check + hew_vec_get_T lowering and that ABI takes i64.
            //
            // Implicit index-site widening: accept a signed integer narrower
            // than i64 (i8/i16/i32) as a Vec index.  The operand widens to i64
            // at the call site; the element result type is NOT changed (LESSONS
            // `widen-operands-not-result-when-tightening-int-coercion`).
            // Publish the operand widening so HIR inserts an explicit cast
            // before the runtime bounds check.
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::Vec),
                args,
                ..
            } if !args.is_empty() => {
                let idx_actual = self.synthesize(&index.0, &index.1);
                let idx_resolved = self.subst.resolve(&idx_actual);
                if Self::is_narrower_signed_int(&idx_resolved) {
                    self.numeric_operand_coercions.insert(
                        SpanKey::in_module(&index.1, self.current_module_idx),
                        Ty::I64,
                    );
                } else {
                    self.check_against(&index.0, &index.1, &Ty::I64);
                }
                if matches!(ctx, IndexContext::Read) {
                    if !self.validate_vec_index_borrow_surface(&args[0], span) {
                        return Ty::Error;
                    }
                    // D432: an element with no clone is read as a loan of the
                    // slot the vector still owns, never copied out.
                    match self.vec_iteration_element_mode(&args[0], span) {
                        Some(super::types::VecIterationMode::Borrow) => {
                            self.borrowed_element_index_reads
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                        }
                        Some(super::types::VecIterationMode::Clone) => {}
                        None => return Ty::Error,
                    }
                }
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Vector(crate::VecValueOp::Index),
                        crate::RuntimeCallFamily::Vector(crate::VecValueOp::Set),
                    ),
                );
                if matches!(ctx, IndexContext::AssignTarget) {
                    self.record_resolved_vec_call("set", &args[0], span);
                }
                args[0].clone()
            }
            // `m[k]` over `HashMap<K, V>` is the trait-routed `Index<K>`
            // accessor (`<HashMap<K, V> as Index>::Output = V`), mirroring
            // `v[i]` over `Vec<T>`.
            //
            // Read context (`let x = m[k]`): the TRAPPING accessor
            // (`Index::at`) — result type is the BARE value `V`. A missing key
            // aborts with `IndexOutOfBounds` (the map analogue of a `v[i]`
            // out-of-bounds trap), so there is no `Option` round-trip. No
            // resolved `.get` call is recorded here: the MIR `Index` node lowers
            // directly to the `hew_hashmap_get_clone_layout` trap choke
            // (`lower_hashmap_index_trap`). Callers who want the non-aborting
            // outcome use `m.get(k) -> Option<V>` instead.
            //
            // Write context (`m[k] = v`): the assignment-target type is the
            // bare value `V` (so the RHS checks against `V`), and the checker
            // records a `ResolvedCall` to `hew_hashmap_insert_layout` at this
            // span. The key bound is the existing `K: Hash + Eq` admission
            // contract — the same one every HashMap method call enforces.
            Ty::Named {
                head: crate::TypeHead::Builtin(BuiltinType::HashMap),
                args,
                ..
            } if args.len() == 2 => {
                let key_ty = args[0].clone();
                let val_ty = args[1].clone();
                self.check_against(&index.0, &index.1, &key_ty);
                // Enforce `K: Hash + Eq` and reject unsafe key/value element
                // types, exactly as the method-call path does — for both the
                // read (trap) and the write (insert) surfaces.
                if !self.validate_hashmap_owned_element_types(&key_ty, &val_ty, span) {
                    return Ty::Error;
                }
                // The trapping read clones the value out of its slot; the
                // write only moves one in.
                if ctx == IndexContext::Read
                    && !self.validate_collection_value_clone_type(
                        &val_ty,
                        BuiltinType::HashMap,
                        "m[k]",
                        span,
                    )
                {
                    return Ty::Error;
                }
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Map(crate::runtime_call::MapValueOp::Index),
                        crate::RuntimeCallFamily::Map(crate::runtime_call::MapValueOp::Insert),
                    ),
                );
                match ctx {
                    // Trapping bare-`V` read: no `.get` resolved call; MIR's
                    // `Index` node owns the `hew_hashmap_get_clone_layout` trap
                    // lowering.
                    IndexContext::Read => val_ty,
                    // Write target: record the `hew_hashmap_insert_layout` call
                    // at the index span (the same one `m.insert(k, v)` emits).
                    IndexContext::AssignTarget => {
                        self.record_resolved_hashmap_call("insert", &key_ty, &val_ty, span);
                        val_ty
                    }
                }
            }
            // W3 collections-sugar S2: `s[i]` over `string` returns a `char`
            // at codepoint offset, O(n), panic on OOB. Index is i64. The
            // checker is authoritative; MIR will route to `hew_string_index`.
            Ty::String => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                Ty::Char
            }
            // W3 collections-sugar S2: `b[i]` over `bytes` returns a `u8`
            // at byte offset, O(1), panic on OOB. Index is i64. MIR will
            // route to `hew_bytes_index`.
            Ty::Bytes => {
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::BytesIndex,
                        crate::RuntimeCallFamily::BytesSet,
                    ),
                );
                self.check_against(&index.0, &index.1, &Ty::I64);
                Ty::U8
            }
            Ty::Named { head, args } => {
                let name = head.registry_key();
                if self.type_satisfies_trait_bound(&resolved_obj, "Index") {
                    let expected_key = self
                        .lookup_named_method_sig(name, args, "at")
                        .and_then(|sig| sig.params.first().cloned())
                        .unwrap_or(Ty::I32);
                    self.check_against(&index.0, &index.1, &expected_key);
                    let output = self.project_assoc_types(&Ty::AssocType {
                        base: Box::new(resolved_obj.clone()),
                        trait_name: "Index".into(),
                        assoc_name: "Output".into(),
                    });
                    if matches!(output, Ty::AssocType { .. }) {
                        self.report_error(
                            TypeErrorKind::AssocTypeProjectionFailed {
                                type_name: resolved_obj.user_facing().to_string(),
                                trait_name: "Index".to_string(),
                                assoc_name: "Output".to_string(),
                            },
                            span,
                            format!(
                                "could not project associated type `<{} as Index>.Output` \
                                 while checking `[]`; ensure the impl defines \
                                 `type Output = ...`",
                                resolved_obj.user_facing()
                            ),
                        );
                        return Ty::Error;
                    }
                    return output;
                }

                self.check_against(&index.0, &index.1, &Ty::I64);
                // Bracket indexing via a named type's `.get()` method is no longer
                // supported. Use the explicit method call instead.
                if self.lookup_named_method_sig(name, args, "get").is_some() {
                    self.report_error_with_suggestions(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot index into `{}` with `[]`; use `.get(k)` instead",
                            resolved_obj.user_facing()
                        ),
                        vec![format!("use `.get(k)` on `{}`", resolved_obj.user_facing())],
                    );
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot index into `{}`", resolved_obj.user_facing()),
                    );
                }
                Ty::Error
            }
            Ty::Array(elem, _) => {
                self.indexed_place_operations.insert(
                    SpanKey::in_module(span, self.current_module_idx),
                    (
                        crate::RuntimeCallFamily::Array(crate::runtime_call::ArrayValueOp::Index),
                        crate::RuntimeCallFamily::Array(crate::runtime_call::ArrayValueOp::Set),
                    ),
                );
                self.check_against(&index.0, &index.1, &Ty::I64);
                if matches!(ctx, IndexContext::Read) {
                    match self.vec_iteration_element_mode(elem, span) {
                        Some(super::types::VecIterationMode::Borrow) => {
                            self.borrowed_element_index_reads
                                .insert(SpanKey::in_module(span, self.current_module_idx));
                        }
                        Some(super::types::VecIterationMode::Clone) => {}
                        None => return Ty::Error,
                    }
                }
                (**elem).clone()
            }
            Ty::Slice(elem) => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                (**elem).clone()
            }
            other => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                if *other != Ty::Error {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot index into `{}`", other.user_facing()),
                    );
                }
                Ty::Error
            }
        }
    }

    /// Type-check an arithmetic operation where at least one operand is `duration` or `instant`.
    ///
    /// Supported operations:
    /// - `duration +/- duration → duration`
    /// - `duration % duration → duration`
    /// - `duration * int → duration`, `int * duration → duration`
    /// - `duration / int → duration`
    /// - `duration / duration → i64` (ratio)
    /// - `instant + duration → instant` (advance a point in time)
    /// - `duration + instant → instant` (commutative advance)
    pub(in crate::check) fn check_duration_arithmetic(
        &mut self,
        op: BinaryOp,
        left: &Ty,
        right: &Ty,
        span: &Span,
    ) -> Ty {
        match (left, right, op) {
            // duration +/- duration → duration, duration % duration → duration
            (Ty::Duration, Ty::Duration, BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Modulo) => {
                Ty::Duration
            }
            // duration * int → duration, int * duration → duration
            (Ty::Duration, r, BinaryOp::Multiply) if r.is_integer() => Ty::Duration,
            (l, Ty::Duration, BinaryOp::Multiply) if l.is_integer() => Ty::Duration,
            // duration / int → duration
            (Ty::Duration, r, BinaryOp::Divide) if r.is_integer() => Ty::Duration,
            // duration / duration → i64 (ratio)
            (Ty::Duration, Ty::Duration, BinaryOp::Divide) => Ty::I64,
            // instant + duration → instant (advance a point in time by a duration)
            (l, Ty::Duration, BinaryOp::Add) if l.is_instant() => left.clone(),
            // duration + instant → instant (commutative: duration + instant)
            (Ty::Duration, r, BinaryOp::Add) if r.is_instant() => right.clone(),
            // instant - duration → instant (rewind a point in time by a duration)
            (l, Ty::Duration, BinaryOp::Subtract) if l.is_instant() => left.clone(),
            // instant - instant → duration (the elapsed gap between two points;
            // both instants canonicalise to i64 nanos, the difference is a
            // signed nanosecond duration). Not commutative — `duration - instant`
            // is meaningless and stays on the error arm below.
            (l, r, BinaryOp::Subtract) if l.is_instant() && r.is_instant() => Ty::Duration,
            _ => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "cannot apply `{op}` to `{}` and `{}`",
                        left.user_facing(),
                        right.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    /// Check if an expression is typically used for side effects (not for its return value).
    pub(in crate::check) fn record_type(&mut self, span: &Span, ty: &Ty) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .insert(key.clone(), self.current_module.clone());
        self.expr_types.insert(key, ty.clone());
    }

    pub(in crate::check) fn record_integer_literal_type(
        &mut self,
        expr: &Expr,
        span: &Span,
        ty: &Ty,
    ) {
        self.record_type(span, ty);
        if let Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } = expr
        {
            self.record_type(&operand.1, ty);
        }
    }

    /// Type-check `lhs is rhs` (identity comparison, slice D-2).
    ///
    /// See the doc comment on the `Expr::Is` arm in [`Self::synthesize_inner`]
    /// for the allowance set, rejection rules, and cross-class behaviour.
    ///
    /// Always returns `Ty::Bool` (even after reporting errors); the operator
    /// is total at the type level so downstream uses (`if (a is b) { ... }`)
    /// don't double-poison.
    pub(super) fn synthesize_is(
        &mut self,
        lhs: &Spanned<Expr>,
        rhs: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let lhs_ty = self.synthesize(&lhs.0, &lhs.1);
        if let Some(rhs_ty) = self.resolve_is_type_pattern(&rhs.0) {
            return self.synthesize_is_type_pattern(lhs, &lhs_ty, rhs, &rhs_ty, span);
        }
        let rhs_ty = self.synthesize(&rhs.0, &rhs.1);
        let lhs_resolved = self.subst.resolve(&lhs_ty);
        let rhs_resolved = self.subst.resolve(&rhs_ty);

        // Don't double-report when either side is already poisoned by an
        // upstream diagnostic (`Ty::Error`). The operator still produces
        // `bool` so enclosing expressions see a stable type.
        if matches!(lhs_resolved, Ty::Error) || matches!(rhs_resolved, Ty::Error) {
            return Ty::Bool;
        }

        // An operand still under inference cannot be decided here, and it must
        // not be abandoned either: a closure's parameter types are fresh
        // variables while its body is checked and only settle when a call site
        // unifies them, so `let same = |a, b| a is b;` used to escape
        // `is_identity_capable` entirely and die in the codegen front on the
        // span-less `IdentityCompare lhs must be a pointer or integer value`.
        // Record the obligation and re-run the same decision once inference
        // has settled (`report_unresolved_inference_holes`) — #3134.
        if matches!(lhs_resolved, Ty::Var(_)) || matches!(rhs_resolved, Ty::Var(_)) {
            let key = SpanKey::in_module(span, self.current_module_idx);
            let check = DeferredIsCheck {
                span: span.clone(),
                lhs_span: lhs.1.clone(),
                lhs_ty,
                rhs_span: rhs.1.clone(),
                rhs_ty,
                source_module: self.current_diagnostic_source_module(),
            };
            self.deferred_is_checks.insert(key, check);
            return Ty::Bool;
        }

        for (kind, span, message) in
            self.is_value_form_diagnostics(&lhs.1, &lhs_resolved, &rhs.1, &rhs_resolved, span)
        {
            self.report_error(kind, &span, message);
        }

        Ty::Bool
    }
}
