use super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::BuiltinType;

type DangerousRcBinding = (String, String);
type DangerousRcScope = HashMap<String, Option<DangerousRcBinding>>;

impl Checker {
    fn lambda_generic_schema_ty(ty: &Ty, generic_param_names: &HashMap<u32, String>) -> Ty {
        match ty {
            Ty::Var(v) => generic_param_names.get(&v.0).map_or_else(
                || ty.clone(),
                |name| Ty::Named {
                    builtin: None,
                    name: name.clone(),
                    args: vec![],
                },
            ),
            Ty::Named {
                name,
                args,
                builtin,
            } => Ty::Named {
                name: name.clone(),
                builtin: *builtin,
                args: args
                    .iter()
                    .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                    .collect(),
            },
            Ty::Tuple(ts) => Ty::Tuple(
                ts.iter()
                    .map(|elem| Self::lambda_generic_schema_ty(elem, generic_param_names))
                    .collect(),
            ),
            Ty::Array(inner, n) => Ty::Array(
                Box::new(Self::lambda_generic_schema_ty(inner, generic_param_names)),
                *n,
            ),
            Ty::Slice(inner) => Ty::Slice(Box::new(Self::lambda_generic_schema_ty(
                inner,
                generic_param_names,
            ))),
            Ty::Pointer {
                is_mutable,
                pointee,
            } => Ty::Pointer {
                is_mutable: *is_mutable,
                pointee: Box::new(Self::lambda_generic_schema_ty(pointee, generic_param_names)),
            },
            Ty::Function { params, ret } => Ty::Function {
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
            },
            Ty::Closure {
                params,
                ret,
                captures,
            } => Ty::Closure {
                params: params
                    .iter()
                    .map(|param| Self::lambda_generic_schema_ty(param, generic_param_names))
                    .collect(),
                ret: Box::new(Self::lambda_generic_schema_ty(ret, generic_param_names)),
                captures: captures
                    .iter()
                    .map(|capture| Self::lambda_generic_schema_ty(capture, generic_param_names))
                    .collect(),
            },
            Ty::TraitObject { traits } => Ty::TraitObject {
                traits: traits
                    .iter()
                    .map(|bound| crate::ty::TraitObjectBound {
                        trait_name: bound.trait_name.clone(),
                        args: bound
                            .args
                            .iter()
                            .map(|arg| Self::lambda_generic_schema_ty(arg, generic_param_names))
                            .collect(),
                        assoc_bindings: bound
                            .assoc_bindings
                            .iter()
                            .map(|(name, ty)| {
                                (
                                    name.clone(),
                                    Self::lambda_generic_schema_ty(ty, generic_param_names),
                                )
                            })
                            .collect(),
                    })
                    .collect(),
            },
            _ => ty.clone(),
        }
    }

    /// Synthesize: infer the type of an expression (bottom-up).
    pub(super) fn synthesize(&mut self, expr: &Expr, span: &Span) -> Ty {
        // Grow the stack on demand so deeply-nested expressions (e.g. 1000+
        // chained binary operators) don't overflow.
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
            self.synthesize_inner(expr, span)
        })
    }

    pub(super) fn reject_if_wasm_incompatible_expr(&mut self, expr: &Expr, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match expr {
            Expr::Scope { .. } | Expr::Join(_) => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            Expr::ForkChild { .. } => {
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Tasks);
            }
            _ => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression check covers all AST variants"
    )]
    pub(super) fn synthesize_inner(&mut self, expr: &Expr, span: &Span) -> Ty {
        self.reject_if_wasm_incompatible_expr(expr, span);
        let ty = match expr {
            // Literals
            Expr::Literal(Literal::Float(_)) => Ty::FloatLiteral,
            Expr::Literal(Literal::String(_)) => Ty::String,
            Expr::RegexLiteral(pattern) => {
                // The implicit `use std::text::regex` injected by the CLI is the
                // provider of this type; mark it as used so the unused-import
                // check doesn't fire a false-positive warning.
                self.used_modules
                    .borrow_mut()
                    .insert(ImportKey::new(self.current_module.clone(), "regex"));
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
                Ty::Named {
                    builtin: None,
                    name: "regex.Pattern".to_string(),
                    args: vec![],
                }
            }
            Expr::ByteStringLiteral(_) | Expr::ByteArrayLiteral(_) => Ty::Bytes,
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let StringPart::Expr((expr, expr_span)) = part {
                        self.synthesize(expr, expr_span);
                    }
                }
                Ty::String
            }
            Expr::Literal(Literal::Bool(_)) => Ty::Bool,
            Expr::Literal(Literal::Char(_)) => Ty::Char,
            Expr::Literal(Literal::Integer { .. }) => Ty::IntLiteral,
            Expr::Literal(Literal::Duration(_)) => Ty::Duration,

            // Identifier lookup — None bypasses record_type (fresh type var each usage)
            Expr::Identifier(name) if name == "None" => {
                return Ty::option(Ty::Var(TypeVar::fresh()));
            }
            Expr::Identifier(name) => self.synthesize_identifier(name, span),

            // Binary ops
            Expr::Binary { left, op, right } => self.check_binary_op(left, *op, right),

            // Unary ops
            Expr::Unary { op, operand } => self.synthesize_unary_op(*op, operand, span),

            // Call
            Expr::Call {
                function,
                type_args,
                args,
                is_tail_call: _,
            } => self.check_call(function, type_args.as_deref(), args, span),

            // Method call
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => self.check_method_call(receiver, method, args, span),

            // Field access
            Expr::FieldAccess { object, field } => self.check_field_access(object, field, span),

            // Block
            Expr::Block(block) => self.check_block(block, None),

            // If expression
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let then_ty = self.synthesize(&then_block.0, &then_block.1);
                if let Some(eb) = else_block {
                    let else_ty = self.synthesize(&eb.0, &eb.1);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    Ty::Unit
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => self.synthesize_iflet(pattern, expr, body, else_body.as_ref(), span),

            // Match
            Expr::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, arms, span, None)
            }

            // Tuple
            Expr::Tuple(elems) => {
                if elems.is_empty() {
                    Ty::Unit
                } else {
                    let tys: Vec<_> = elems.iter().map(|(e, s)| self.synthesize(e, s)).collect();
                    Ty::Tuple(tys)
                }
            }

            // Array
            Expr::Array(elems) => {
                if elems.is_empty() {
                    let v = TypeVar::fresh();
                    Ty::Array(Box::new(Ty::Var(v)), 0)
                } else {
                    let first_ty = self.synthesize(&elems[0].0, &elems[0].1);
                    for elem in &elems[1..] {
                        self.check_against(&elem.0, &elem.1, &first_ty);
                    }
                    Ty::Array(Box::new(first_ty), elems.len() as u64)
                }
            }
            Expr::ArrayRepeat { value, count } => self.synthesize_array_repeat(value, count, span),

            Expr::MapLiteral { entries } => self.synthesize_map_literal(entries, span),

            // Struct init
            Expr::StructInit {
                name,
                fields,
                type_args,
                base,
            } => self.check_struct_init(name, fields, type_args.as_deref(), base.as_deref(), span),

            // Spawn
            Expr::Spawn { target, args } => {
                if self.in_pure_function {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `spawn` in a pure function".to_string(),
                    );
                }
                self.check_spawn(target, args, span)
            }

            // Lambda (synthesize mode — no expected type)
            Expr::Lambda {
                is_move,
                type_params,
                params,
                return_type,
                body,
            } => self.check_lambda(
                *is_move,
                type_params.as_deref(),
                params,
                return_type.as_ref(),
                body,
                None,
                span,
            ),

            // Await
            Expr::Await(inner) => {
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                // await Task<T> → T (simplified)
                match inner_ty {
                    Ty::Task(inner) => *inner,
                    // `await close(actor)` or bare actor ref → Unit (actor termination).
                    // But NOT for method calls that happen to return an ActorRef —
                    // those should pass through the method's declared return type.
                    _ if inner_ty.as_actor_handle().is_some()
                        && !matches!(inner.0, Expr::MethodCall { .. }) =>
                    {
                        Ty::Unit
                    }
                    _ => inner_ty,
                }
            }

            // PostfixTry: expr? → unwrap Result/Option
            Expr::PostfixTry(inner) => {
                let ty = self.synthesize(&inner.0, &inner.1);
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
                let bad_ctx_msg: Option<String> =
                    self.current_return_type.as_ref().and_then(|ret| {
                        let r = self.subst.resolve(ret);
                        if r.as_option().is_some()
                            || r.as_result().is_some()
                            || matches!(r, Ty::Var(_) | Ty::Error)
                            || matches!(&r, Ty::Named { name, .. }
                                if !Ty::is_named_builtin(name)
                                    && !self.type_defs.contains_key(name)
                                    && !self.type_aliases.contains_key(name))
                        {
                            None
                        } else {
                            Some(format!(
                                "`?` cannot be used in a function returning `{r}`; \
                                 the enclosing function must return `Option` or `Result`"
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
                } else if let Some((ok, _)) = ty.as_result() {
                    if let Some(msg) = bad_ctx_msg {
                        self.report_error(TypeErrorKind::InvalidOperation, span, msg);
                        Ty::Error
                    } else {
                        ok.clone()
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

            // Yield
            Expr::Yield(value) => self.synthesize_yield(value.as_deref(), span),

            // Cooperate
            Expr::Cooperate => Ty::Unit,

            // Actor self-reference handle — returns LocalPid<Self>, not the actor type itself
            Expr::This => {
                if let Some(actor_ty) = &self.current_actor_type {
                    Ty::local_pid(actor_ty.clone())
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`this` can only be used inside an actor".to_string(),
                    );
                    Ty::Error
                }
            }

            // Index
            Expr::Index { object, index } => self.synthesize_index(object, index, span),

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
            // Allowed receivers per plan §D-D2: machines, actors/actor refs,
            // heap-backed `Vec`/`HashMap`/`HashSet`/`bytes`, and user `type`
            // declarations (`TypeDefKind::Struct`/`Enum`, both Rc-backed in
            // the runtime). Rejected with `E_IS_VALUE_TYPE`: scalars, `String`,
            // `record` types, tuples, ranges, fn/closures.
            //
            // Result is always `bool`. Cross-class mismatches (e.g.
            // `ActorRef<T> is Vec<int>`) collapse into a single
            // `TypeErrorKind::Mismatch` diagnostic that requires the operands
            // share the same resolved type. Move/consumed-self semantics
            // follow the existing use-after-move rule (plan §D-D4, Q-N3).
            Expr::Is { lhs, rhs } => self.synthesize_is(lhs, rhs, span),

            _ => self.synthesize_concurrency(expr, span),
        };

        self.record_type(span, &ty);
        ty
    }

    pub(super) fn synthesize_unary_op(
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

    pub(super) fn synthesize_range(
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
                    if let Some(common_ty) = common_integer_type(&start_resolved, &end_resolved) {
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

    pub(super) fn synthesize_cast(
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

    pub(super) fn synthesize_array_repeat(
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
        self.make_vec_type(elem_ty, span)
    }

    pub(super) fn synthesize_map_literal(
        &mut self,
        entries: &[(Spanned<Expr>, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        if entries.is_empty() {
            let k = TypeVar::fresh();
            let v = TypeVar::fresh();
            Ty::Named {
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
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
                builtin: Some(BuiltinType::HashMap),
                name: "HashMap".to_string(),
                args: vec![first_key_ty, first_val_ty],
            }
        }
    }

    pub(super) fn synthesize_iflet(
        &mut self,
        pattern: &Spanned<Pattern>,
        expr: &Spanned<Expr>,
        body: &Block,
        else_body: Option<&Block>,
        span: &Span,
    ) -> Ty {
        let scr_ty = self.synthesize(&expr.0, &expr.1);
        if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
            return Ty::Error;
        }
        self.env.push_scope();
        self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
        let then_ty = self.check_block(body, None);
        self.env.pop_scope();
        if let Some(block) = else_body {
            let else_ty = self.check_block(block, None);
            self.unify_branches(&then_ty, &else_ty, span)
        } else {
            Ty::Unit
        }
    }

    pub(super) fn report_invalid_actor_send(&mut self, ty: &Ty, span: &Span) {
        self.report_error(
            TypeErrorKind::InvalidSend,
            span,
            format!(
                "cannot send `{}` to actor: type is not Send",
                ty.user_facing()
            ),
        );
    }

    pub(super) fn mark_expr_moved_if_non_copy(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        if !self.registry.implements_marker(ty, MarkerTrait::Copy) {
            if let Expr::Identifier(name) = expr {
                self.env.mark_moved(name, span.clone());
            }
        }
    }

    pub(super) fn enforce_actor_boundary_send(
        &mut self,
        expr: &Expr,
        move_span: &Span,
        error_span: &Span,
        ty: &Ty,
    ) {
        let ty = self.subst.resolve(ty);
        if !self.registry.implements_marker(&ty, MarkerTrait::Send) {
            self.report_invalid_actor_send(&ty, error_span);
        }
        // Compute the alias-vs-copy decision before `mark_expr_moved_if_non_copy`
        // marks the binding moved — the move-checker contract is what gives
        // the alias path its no-write-after-send invariant.
        let decision = self.classify_actor_send_aliasing(expr, &ty);
        self.mark_expr_moved_if_non_copy(expr, move_span, &ty);
        self.actor_send_aliasing
            .insert(SpanKey::from(move_span), decision);
    }

    /// Decide whether an actor-send arg should ride the alias (refcount-bumped
    /// envelope) path or the copy (legacy deep-copy mailbox) path.
    ///
    /// The arg is recorded as `Alias` only when *all* hold:
    ///
    /// * The arg is a bare identifier expression. Field accesses, projections,
    ///   and freshly constructed values stay `Copy` because the move-checker
    ///   does not invalidate their parent binding, so reusing the underlying
    ///   storage from an aliased envelope would be unsound.
    /// * The arg's resolved type is **not** `Copy`. `Copy` payloads already
    ///   fit inline at the mailbox boundary and the alias bit would be
    ///   ambiguous (cloning a `Copy` is free).
    /// * The arg's resolved type does **not** implement `Drop` (neither the
    ///   stdlib-registered marker nor a user-declared `impl Drop for T`). A
    ///   destructor means sender-side drop suppression has to be coordinated
    ///   with the receiver, which Phase α does not handle yet.
    ///
    /// Capability transfer (HEW-DIST-SPEC §12) is a third class that must
    /// stay `Copy` and instead flip a `CAPABILITY_TRANSFER` header bit at
    /// codegen time. Until `hew-types` exposes a capability predicate, the
    /// rule conservatively keeps every send `Copy` unless it independently
    /// matches the alias preconditions.
    ///
    /// **Stub note (HEW-DIST-SPEC §12 capability predicate)**:
    /// - WHY: `hew-types` does not yet have a `MarkerTrait::Capability` or
    ///   equivalent registry of capability handle types.
    /// - WHEN obsolete: when capability typing lands (issue tracking
    ///   §12 ratification).
    /// - WHAT real solution: a predicate `is_capability_type(&self, &Ty)` that
    ///   returns true for capability handles; this function would then
    ///   return `Copy` for capability args and codegen would set
    ///   `HEW_MSG_ENVELOPE_CAPABILITY_TRANSFER` on the envelope header.
    fn classify_actor_send_aliasing(&self, expr: &Expr, ty: &Ty) -> ActorSendAliasing {
        if !matches!(expr, Expr::Identifier(_)) {
            return ActorSendAliasing::Copy(ActorSendCopyReason::NotIdentifier);
        }
        if self.registry.implements_marker(ty, MarkerTrait::Copy) {
            return ActorSendAliasing::Copy(ActorSendCopyReason::CopyType);
        }
        if self.registry.implements_marker(ty, MarkerTrait::Drop) {
            return ActorSendAliasing::Copy(ActorSendCopyReason::StdlibDrop);
        }
        // `MarkerTrait::Drop` covers stdlib drop types and `Rc`, but a
        // user-defined `impl Drop for T` records into `trait_impls_set`
        // without flipping the marker. The alias path's sender-side drop
        // suppression is unsafe for user destructors until receiver-side
        // coordination is in place, so any user `impl Drop` stays Copy.
        if let Ty::Named { name, builtin, .. } = ty {
            if self
                .trait_impls_set
                .contains(&(name.clone(), "Drop".to_string()))
            {
                return ActorSendAliasing::Copy(ActorSendCopyReason::UserDrop);
            }
            // Built-in heap-owning collections (`Vec`, `HashMap`, `HashSet`)
            // do not implement the `Drop` marker through the trait registry —
            // their `Drop` derivation depends on element types, and a
            // collection of `Copy` elements (e.g. `HashMap<String, i64>`)
            // therefore reports `implements_marker(MarkerTrait::Drop) == false`.
            // The collection itself still owns heap memory, so the alias
            // path's contract (no sender-side drop) would leak the
            // backing buffer.  Classify them as `Copy(StdlibDrop)` so the
            // legacy `deepCopyOwnedArgs` clone fires and the receiver
            // gets an independent copy.
            if builtin.is_some_and(BuiltinType::is_collection) {
                return ActorSendAliasing::Copy(ActorSendCopyReason::StdlibDrop);
            }
        }
        ActorSendAliasing::Alias
    }

    pub(super) fn synthesize_yield(&mut self, value: Option<&Spanned<Expr>>, span: &Span) -> Ty {
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
                } else if let Some(yields) = resolved.as_async_generator() {
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

    pub(super) fn synthesize_identifier(&mut self, name: &str, span: &Span) -> Ty {
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
        if let Some((depth, binding)) = self.env.lookup_with_depth(name) {
            let binding_id = binding.id;
            let is_moved = binding.is_moved;
            let moved_at = binding.moved_at.clone();
            let ty = binding.ty.clone();
            let def_span = binding.def_span.clone();
            if is_moved {
                let mut err = TypeError::new(
                    TypeErrorKind::UseAfterMove,
                    span.clone(),
                    format!("use of moved value `{name}`"),
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
                }
                self.errors.push(err);
            }
            // Track captures: variable from scope below the lambda boundary
            if let Some(capture_depth) = self.lambda_capture_depth {
                if depth < capture_depth {
                    self.lambda_captures.push(ty.clone());
                    self.lambda_capture_facts.push(ClosureCaptureFact {
                        binding_id,
                        name: name.to_string(),
                        ty: ty.clone(),
                        mode: ClosureCaptureMode::Copy,
                        is_send: false,
                        use_span: span.clone(),
                        def_span,
                    });
                }
            }
            ty
        } else if self.fn_sigs.contains_key(name) {
            // Function name used as a value (e.g., variant constructor)
            if let Some(caller) = &self.current_function {
                self.call_graph
                    .entry(caller.clone())
                    .or_default()
                    .insert(name.to_string());
            }
            let sig = self.fn_sigs[name].clone();
            if sig.params.is_empty() {
                sig.return_type
            } else {
                Ty::Function {
                    params: sig.params,
                    ret: Box::new(sig.return_type),
                }
            }
        } else {
            self.resolve_identifier_variant(name, span)
        }
    }

    /// Look up an identifier as a unit enum variant or qualified variant name.
    #[allow(
        clippy::too_many_lines,
        reason = "multi-branch variant resolution: unqualified, qualified-in-type_defs, and qualified-in-fn_sigs each need distinct handling"
    )]
    pub(super) fn resolve_identifier_variant(&mut self, name: &str, span: &Span) -> Ty {
        let mut found = None;
        for (type_name, td) in &self.type_defs {
            if let Some(variant) = td.variants.get(name) {
                if matches!(variant, VariantDef::Unit) {
                    let ty = Ty::normalize_named(type_name.clone(), vec![]);
                    found = Some(ty);
                    break;
                }
            }
        }
        // Handle qualified variant names (e.g., Light::Off, LightEvent::Toggle)
        if found.is_none() {
            if let Some(pos) = name.rfind("::") {
                let type_prefix = &name[..pos];
                let variant_name = &name[pos + 2..];
                if let Some(td) = self.type_defs.get(type_prefix) {
                    if let Some(variant) = td.variants.get(variant_name) {
                        if matches!(variant, VariantDef::Unit) {
                            // Instantiate type params with fresh inference variables
                            // so that `Option::None` in `let x: Option<i64> = Option::None`
                            // unifies correctly with the annotation.  Bare `Named { "Option",
                            // [] }` fails arity unification against `Named { "Option", [I64] }`.
                            //
                            // Guard: only use fn_sig when its return type names the same enum as
                            // type_prefix.  Two enums sharing a bare variant name (e.g. both
                            // declaring `None`) would collide in fn_sigs because the key is the
                            // bare variant name; without the guard, `A::None` could return
                            // `Named { B, [?] }`.
                            let ty = if let Some(sig) = self.fn_sigs.get(variant_name).cloned() {
                                let sig_names_correct_enum = sig
                                    .return_type
                                    .type_name()
                                    .is_some_and(|n| n == type_prefix);
                                if sig_names_correct_enum {
                                    let mut ret = sig.return_type.clone();
                                    for tp in &sig.type_params {
                                        ret = ret
                                            .substitute_named_param(tp, &Ty::Var(TypeVar::fresh()));
                                    }
                                    ret
                                } else {
                                    // fn_sig belongs to a different enum; bare name is correct.
                                    Ty::normalize_named(type_prefix.to_string(), vec![])
                                }
                            } else {
                                // No fn_sig (monomorphic or machine variant) — bare name is correct.
                                Ty::normalize_named(type_prefix.to_string(), vec![])
                            };
                            found = Some(ty);
                        }
                    }
                }
                // Also check fn_sigs for qualified constructors
                if found.is_none() {
                    if let Some(sig) = self.fn_sigs.get(variant_name) {
                        if sig.params.is_empty() {
                            let ret = &sig.return_type;
                            let matches_type =
                                ret.type_name().is_some_and(|name| name == type_prefix);
                            if matches_type {
                                found = Some(sig.return_type.clone());
                            }
                        }
                    }
                }
            }
        }
        if let Some(ty) = found {
            ty
        } else {
            // Detect recursive closure self-reference: if we are inside a lambda
            // body (capture depth is set) and the name matches the let-binding
            // being defined, emit ClosureRecursive rather than UndefinedVariable.
            // By-value capture cannot capture a value before construction, so
            // recursive closures are forbidden in v0.5.
            if self.lambda_capture_depth.is_some()
                && self
                    .pending_let_closure_name
                    .as_deref()
                    .is_some_and(|pending| pending == name)
            {
                self.report_error(
                    TypeErrorKind::ClosureRecursive {
                        name: name.to_string(),
                    },
                    span,
                    format!(
                        "E_CLOSURE_RECURSIVE: closure cannot refer to its own binding \
                         `{name}` — recursive closures require a fixed-point surface that \
                         is not available in this version; use a named function instead"
                    ),
                );
                return Ty::Error;
            }
            if name == "self" {
                let message = if self.current_actor_type.is_some() {
                    "`self` is not used in Hew actor bodies; access actor state with bare field names like `count` (not `self.count`), or use `this` when you need the actor handle".to_string()
                } else {
                    "`self` is not a valid identifier in Hew; \
                     use a named receiver parameter instead: \
                     `fn method(val: Self)` in traits or `fn method(p: Point)` in impls"
                        .to_string()
                };
                self.report_error(TypeErrorKind::UndefinedVariable, span, message);
            } else {
                let similar = crate::error::find_similar(
                    name,
                    self.env
                        .all_names()
                        .chain(self.fn_sigs.keys().map(String::as_str)),
                );
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    format!("undefined variable `{name}`"),
                    similar,
                );
            }
            Ty::Error
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "index checking covers range slices, Vec runtime indexing, user Index impls, and dyn Index dispatch"
    )]
    pub(super) fn synthesize_index(
        &mut self,
        object: &Spanned<Expr>,
        index: &Spanned<Expr>,
        span: &Span,
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
                    builtin: Some(BuiltinType::Vec),
                    args,
                    ..
                } if !args.is_empty() => obj_ty.clone(),
                _ => {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot range-slice `{}`; range-slice syntax `xs[a..b]` is \
                             supported only for `Vec<T>` receivers",
                            obj_ty.user_facing()
                        ),
                    );
                    Ty::Error
                }
            };
        }

        let resolved_obj = self.subst.resolve(&obj_ty);
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
                    self.record_dyn_index_method_call(&bound.trait_name, span);
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
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                args,
                ..
            } if !args.is_empty() => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                args[0].clone()
            }
            Ty::Named { name, args, .. } => {
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
                                "could not project associated type `<{} as Index>::Output` \
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
            Ty::Array(elem, _) | Ty::Slice(elem) => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                (**elem).clone()
            }
            other => {
                self.check_against(&index.0, &index.1, &Ty::I64);
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!("cannot index into `{}`", other.user_facing()),
                );
                Ty::Error
            }
        }
    }

    fn record_dyn_index_method_call(&mut self, trait_name: &str, span: &Span) {
        let Some(trait_info) = self.trait_defs.get(trait_name) else {
            return;
        };
        let Some(method_idx) = trait_info
            .methods
            .iter()
            .position(|method| method.name == "at")
        else {
            return;
        };
        let slot = 3 + u32::try_from(method_idx).unwrap_or(u32::MAX);
        self.dyn_trait_method_calls.insert(
            SpanKey::from(span),
            crate::check::types::DynMethodCall {
                trait_name: trait_name.to_string(),
                method_name: "at".to_string(),
                slot,
            },
        );
        self.record_method_call_receiver_kind(
            span,
            crate::check::types::MethodCallReceiverKind::TraitObject {
                trait_name: trait_name.to_string(),
            },
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "concurrency variants (scope/select/join/spawn/unsafe/timeout) with purity checks"
    )]
    pub(super) fn synthesize_concurrency(&mut self, expr: &Expr, span: &Span) -> Ty {
        if self.in_pure_function {
            match expr {
                Expr::Scope { .. } => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `scope` in a pure function".to_string(),
                    );
                }
                Expr::ForkChild { .. } => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `fork` in a pure function".to_string(),
                    );
                }
                Expr::Select { .. } => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `select` in a pure function".to_string(),
                    );
                }
                Expr::Join(_) => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `join` in a pure function".to_string(),
                    );
                }
                Expr::SpawnLambdaActor { .. } => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `spawn` in a pure function".to_string(),
                    );
                }
                _ => {}
            }
        }
        match expr {
            Expr::ForkChild { .. } => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`fork name = expr;` is parser-only in this build; type checking lands in a follow-up change"
                        .to_string(),
                );
                Ty::Error
            }
            Expr::SpawnLambdaActor {
                is_move,
                params,
                return_type,
                body,
            } => {
                // Synthesise the body without propagating the return-type annotation as
                // a contextual hint.  This lets us extract the actual body return type
                // and emit targeted diagnostics rather than generic Mismatch errors:
                //   - E_LAMBDA_RETURN_TYPE_MISMATCH: body return type ≠ declared reply type.
                //   - E_LAMBDA_SELF_ESCAPE: body returns a Duplex handle (leaks the actor).
                // Bidirectional hint for the body is intentionally omitted here (slight
                // inference degradation for actor bodies) to keep diagnostics clean.
                // WHEN-OBSOLETE: if a richer bidirectional inference mode is added that
                // can propagate a "return type hint" without actually checking the body
                // against it, restore the hint while keeping targeted diagnostics.
                let lambda_ty = self.check_lambda(*is_move, None, params, None, body, None, span);
                // Check captures for Send (E_DUPLEX_NON_SEND).
                let body_ret = match &lambda_ty {
                    Ty::Function { ret, .. } | Ty::Closure { ret, .. } => {
                        let mut non_send_captures = vec![];
                        if let Ty::Closure { captures, .. } = &lambda_ty {
                            let mut seen = HashSet::new();
                            for capture in captures {
                                if !self.registry.implements_marker(capture, MarkerTrait::Send)
                                    && seen.insert(capture.clone())
                                {
                                    non_send_captures.push(capture.clone());
                                }
                            }
                        }
                        for capture in &non_send_captures {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                span,
                                format!(
                                    "cannot capture `{}` in spawned actor: type is not Send (E_DUPLEX_NON_SEND)",
                                    capture.user_facing()
                                ),
                            );
                        }
                        (**ret).clone()
                    }
                    _ => Ty::Unit,
                };
                // E_LAMBDA_SELF_ESCAPE: the lambda body returns a Duplex handle.
                // A lambda body that produces a Duplex<...> value is leaking an actor
                // handle outside the actor boundary — the handle's lifetime is bound to
                // the let-binding site, not to values the body produces.
                //
                // CONSERVATIVE APPROXIMATION (slice 2): any Duplex-typed body is rejected,
                // including the "factory" pattern (actor body returns a *different* actor's
                // handle). Slice 3 can narrow this to only reject Duplex values that alias
                // a capture from the enclosing let-binding, using MIR-level alias analysis.
                // Until then, returning any Duplex from an actor body is forbidden.
                //
                // WHEN-OBSOLETE: slice 3 adds MIR-level self-ref weak capture that covers
                // the runtime dimension of self-escape; this is the static type-level gate.
                if body_ret.as_duplex().is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "actor lambda body returns a Duplex handle — actor handles cannot \
                         escape the actor boundary via a return value (E_LAMBDA_SELF_ESCAPE); \
                         use a tell-shaped actor (no return type) instead"
                            .to_string(),
                    );
                }
                // Build the message type from the parameter list.
                // Single param → that param's type; multiple params → Tuple.
                // No params → Unit (actor takes no argument).
                let msg_ty = {
                    let param_types: Vec<Ty> = params
                        .iter()
                        .map(|p| {
                            p.ty.as_ref().map_or(Ty::Var(TypeVar::fresh()), |ann| {
                                self.resolve_type_expr(ann)
                            })
                        })
                        .collect();
                    match param_types.len() {
                        0 => Ty::Unit,
                        1 => param_types.into_iter().next().unwrap(),
                        _ => Ty::Tuple(param_types),
                    }
                };
                // The reply type determines tell vs ask:
                //   tell-shaped (`actor |p| { ... }` — no explicit return type, or `-> ()`)
                //     → `Duplex<Msg, ()>` — call-site returns `Result<(), SendError>`
                //   ask-shaped (`actor |p| -> Reply { ... }`)
                //     → `Duplex<Msg, Reply>` — call-site returns `Result<Reply, AskError>`
                let reply_ty = if let Some(ret_ann) = return_type.as_ref() {
                    let resolved = self.resolve_type_expr(ret_ann);
                    if matches!(resolved, Ty::Unit) {
                        Ty::Unit
                    } else {
                        // E_LAMBDA_RETURN_TYPE_MISMATCH: body return type ≠ declared return type
                        // for ask-shaped actors. The generic Mismatch that check_lambda would
                        // normally emit is suppressed because we passed `None` as the return
                        // annotation hint; we emit the targeted diagnostic here instead.
                        let resolved_body = self.subst.resolve(&body_ret);
                        if !matches!(resolved_body, Ty::Error | Ty::Var(_)) {
                            let snapshot = self.subst.snapshot();
                            let mismatch =
                                unify(&mut self.subst, &resolved_body, &resolved).is_err();
                            self.subst.restore(snapshot);
                            if mismatch {
                                self.report_error(
                                    TypeErrorKind::ReturnTypeMismatch,
                                    span,
                                    format!(
                                        "ask-shaped actor body returns `{}` but the declared reply \
                                         type is `{}` (E_LAMBDA_RETURN_TYPE_MISMATCH)",
                                        resolved_body.user_facing(),
                                        resolved.user_facing()
                                    ),
                                );
                            }
                        }
                        // Validate: ask-shaped reply must be Send (crosses actor boundary).
                        if !self
                            .registry
                            .implements_marker(&resolved, MarkerTrait::Send)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                span,
                                format!(
                                    "ask-shaped actor reply type `{}` is not Send (E_DUPLEX_NON_SEND)",
                                    resolved.user_facing()
                                ),
                            );
                        }
                        resolved
                    }
                } else {
                    Ty::Unit
                };
                // Msg type must also be Send (it crosses the actor boundary on call).
                if !matches!(msg_ty, Ty::Unit | Ty::Var(_))
                    && !self.registry.implements_marker(&msg_ty, MarkerTrait::Send)
                {
                    self.report_error(
                        TypeErrorKind::InvalidSend,
                        span,
                        format!(
                            "lambda actor message type `{}` is not Send (E_DUPLEX_NON_SEND)",
                            msg_ty.user_facing()
                        ),
                    );
                }
                Ty::duplex(msg_ty, reply_ty)
            }
            Expr::Scope { body: block } => {
                // Type-check the block body for diagnostics; the scope itself is Unit
                // (it is a lifetime boundary, not a value-producing block).
                self.check_block(block, None);
                Ty::Unit
            }
            Expr::UnsafeBlock(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block(block, None);
                self.in_unsafe = prev;
                ty
            }
            Expr::Select { arms, timeout } => {
                let mut result_ty: Option<Ty> = None;
                for arm in arms {
                    self.env.push_scope();
                    let source_ty = self.synthesize_actor_concurrency_source(
                        &arm.source.0,
                        &arm.source.1,
                        "select arm source",
                    );
                    self.bind_pattern(&arm.binding.0, &source_ty, false, &arm.binding.1);
                    let body_ty = if let Some(expected) = &result_ty {
                        self.check_against(&arm.body.0, &arm.body.1, expected)
                    } else {
                        self.synthesize(&arm.body.0, &arm.body.1)
                    };
                    if result_ty.is_none() {
                        result_ty = Some(body_ty);
                    }
                    self.env.pop_scope();
                }
                if let Some(tc) = timeout {
                    self.check_against(&tc.duration.0, &tc.duration.1, &Ty::Duration);
                    let timeout_ty = self.synthesize(&tc.body.0, &tc.body.1);
                    if let Some(expected) = &result_ty {
                        self.expect_type(expected, &timeout_ty, &tc.body.1);
                    } else {
                        result_ty = Some(timeout_ty);
                    }
                }
                result_ty.unwrap_or(Ty::Unit)
            }
            Expr::Join(exprs) => {
                let types: Vec<Ty> = exprs
                    .iter()
                    .map(|(e, s)| {
                        self.synthesize_actor_concurrency_source(e, s, "join expression element")
                    })
                    .collect();
                if types.len() == 1 {
                    types[0].clone()
                } else {
                    Ty::Tuple(types)
                }
            }
            Expr::Timeout {
                expr: inner,
                duration,
            } => {
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                self.check_against(&duration.0, &duration.1, &Ty::Duration);
                Ty::option(inner_ty)
            }
            Expr::GenBlock { body } => {
                // WHY: `gen { }` expression-position generator blocks are part
                //      of the v0.5 surface (parser support present).
                //      HIR/MIR coroutine lowering is not yet wired; fail closed here.
                // WHEN OBSOLETE: when generator lowering wires
                //      HirExprKind::GenBlock and the coroutine scheduler.
                // REAL SOLUTION: synthesize the yield type from the block,
                //      return Ty::Named { name: "Iterator", args: [yield_ty] }.
                //
                // A98 / Q98: generator blocks inside actor receive handlers are
                // permanently forbidden.  The scheduler holds the actor-state lock
                // for the entire handler invocation; there is no safe point to
                // yield mid-handler.  This is a typed compile error, not a runtime
                // trap.
                if self.in_actor_handler_context {
                    self.report_error(
                        TypeErrorKind::GenBlockInActorReceive,
                        span,
                        "E_GENBLOCK_IN_ACTOR_RECEIVE: `gen { }` blocks are forbidden inside \
                         actor receive handlers — the scheduler holds the actor-state lock for \
                         the entire handler invocation; use a named generator function outside \
                         the handler instead"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                // Typed gen{} checking.
                //
                // Two fresh type-variables seed independent inference:
                //   yield_var — unified by each `yield <expr>` site in the body.
                //   return_var — unified with the body's tail expression type
                //                (and by explicit `return <expr>` statements when
                //                 Stmt::Return extracts the Return component from
                //                 the enclosing Generator type).
                //
                // After the body, EmptyGenerator fires only when the body is
                // genuinely empty of generator-relevant content: yield_var is
                // still unbound AND the Return component is Unit or Never (i.e.
                // no tail expression or explicit `return <value>` provided a
                // useful return type).  `gen { return 1; }` and `gen { 1 }` are
                // both valid generators with inferred Return=i64.
                //
                // The HIR lowerer is still fail-closed on GenBlock; this gates
                // only the type checker so that type errors surface early.
                let yield_var = TypeVar::fresh();
                let return_var = TypeVar::fresh();
                let gen_ty = Ty::generator(Ty::Var(yield_var), Ty::Var(return_var));

                let prev_in_generator = self.in_generator;
                let prev_return_type = self.current_return_type.take();
                self.in_generator = true;
                self.current_return_type = Some(gen_ty.clone());

                let body_ty = self.check_block(body, None);

                self.in_generator = prev_in_generator;
                self.current_return_type = prev_return_type;

                // Unify the tail-expression type with the Return type-variable.
                // Never / Error propagate vacuously (unify is a no-op for Error).
                self.expect_type(&Ty::Var(return_var), &body_ty, span);

                let resolved_yield = self.subst.resolve(&Ty::Var(yield_var));
                let resolved_return = self.subst.resolve(&Ty::Var(return_var));

                // EmptyGenerator: no yield AND no useful return path.
                // A resolved return_var (from a tail expr or `return <expr>`)
                // means the body is doing real work even without a yield site.
                let yield_unresolved = matches!(resolved_yield, Ty::Var(_));
                let return_trivial = matches!(resolved_return, Ty::Var(_) | Ty::Unit | Ty::Never);

                if yield_unresolved && return_trivial {
                    self.report_error(
                        TypeErrorKind::EmptyGenerator,
                        span,
                        "E_EMPTY_GENERATOR: `gen { }` body contains no `yield` expression \
                         and no value-producing tail expression or `return`; \
                         the yield type cannot be inferred — add at least one \
                         `yield <value>` statement"
                            .to_string(),
                    );
                    Ty::Error
                } else {
                    // If yield_var is still unresolved (body has a return but no
                    // yield), the generator never yields — represent that as Never.
                    let final_yield = if yield_unresolved {
                        Ty::Never
                    } else {
                        resolved_yield
                    };
                    Ty::generator(final_yield, resolved_return)
                }
            }
            _ => Ty::Unit,
        }
    }

    pub(super) fn check_expr_with_expected(
        &mut self,
        expr: &Expr,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        match expr {
            Expr::Block(block) => {
                let actual = self.check_block(block, Some(expected));
                if matches!(actual, Ty::Never | Ty::Error) {
                    actual
                } else {
                    let n = self.errors.len();
                    self.expect_type(expected, &actual, span);
                    if self.errors.len() > n {
                        Ty::Error
                    } else {
                        actual
                    }
                }
            }
            _ => self.check_against(expr, span, expected),
        }
    }

    /// Check: verify expression against expected type (top-down).
    #[expect(
        clippy::too_many_lines,
        reason = "literal coercion requires many match arms with range checks"
    )]
    pub(super) fn check_against(&mut self, expr: &Expr, span: &Span, expected: &Ty) -> Ty {
        // Resolve type variables so that Ty::Var(v) unified with e.g. Ty::I32
        // is seen as Ty::I32 by the coercion arms below.
        let resolved = self.subst.resolve(expected);
        let expected = &resolved;
        match (expr, expected) {
            // Lambda with expected function type — propagate param types!
            (
                Expr::Lambda {
                    is_move,
                    type_params,
                    params,
                    return_type,
                    body,
                },
                Ty::Function {
                    params: expected_params,
                    ret,
                },
            ) => {
                let result = self.check_lambda(
                    *is_move,
                    type_params.as_deref(),
                    params,
                    return_type.as_ref(),
                    body,
                    Some((expected_params, ret)),
                    span,
                );
                self.record_type(span, &result);
                result
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
                let then_ty = self.check_expr_with_expected(&then_block.0, &then_block.1, expected);
                let actual = if let Some(else_block) = else_block {
                    let else_ty =
                        self.check_expr_with_expected(&else_block.0, &else_block.1, expected);
                    if matches!(then_ty, Ty::Error) || matches!(else_ty, Ty::Error) {
                        Ty::Error
                    } else if matches!(then_ty, Ty::Never) && matches!(else_ty, Ty::Never) {
                        Ty::Never
                    } else {
                        self.subst.resolve(expected)
                    }
                } else {
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
                let actual = self.check_match_expr(&scr_ty, arms, span, Some(expected));
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

            // Integer literal can coerce to any integer type (with range check)
            (expr, ty) if is_integer_literal(expr) && ty.is_integer() => {
                if !expected.is_numeric_literal() {
                    if let Some(value) = extract_integer_literal_value(expr) {
                        if value < 0 && !integer_type_info(expected).is_some_and(|i| i.signed) {
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
                        if !integer_fits_type(value, expected) {
                            let (lo, hi) = integer_type_range(expected).unwrap_or((0, 0));
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
                self.record_type(span, expected);
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
                for elem in elems {
                    let (expr, sp) = (&elem.0, &elem.1);
                    self.check_against(expr, sp, &elem_ty);
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

            // Array literal coercion to Array<T, N> type
            (Expr::Array(elems), Ty::Array(elem_ty, _)) => {
                for elem in elems {
                    let (expr, sp) = (&elem.0, &elem.1);
                    self.check_against(expr, sp, elem_ty);
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Array repeat coercion to Array<T, N> type
            (Expr::ArrayRepeat { value, count }, Ty::Array(elem_ty, _)) => {
                self.check_against(&value.0, &value.1, elem_ty);
                self.check_against(&count.0, &count.1, &Ty::I64);
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
                                if *value < 0
                                    && !integer_type_info(expected).is_some_and(|i| i.signed)
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
                                if !integer_fits_type(*value, expected) {
                                    let (lo, hi) = integer_type_range(expected).unwrap_or((0, 0));
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
                            // Mark the identifier as used
                            let _ = self.env.lookup(name);
                            self.record_type(span, expected);
                            return expected.clone();
                        }
                        (ConstValue::Integer(_), ty) if ty.is_float() => {
                            let _ = self.env.lookup(name);
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
                            let _ = self.env.lookup(name);
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
                for (elem, expected_ty) in elems.iter().zip(expected_tys.iter()) {
                    self.check_against(&elem.0, &elem.1, expected_ty);
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Struct init coercion: propagate expected type args into field checking
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
                    ..
                },
            ) if name == expected_name => {
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
                        self.report_error(
                            TypeErrorKind::ArityMismatch,
                            span,
                            format!(
                                "struct `{name}` has {} type parameter(s) but {} were supplied",
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
                                let mut field_expected = declared_ty.clone();
                                for (tp, concrete) in &type_arg_map {
                                    field_expected =
                                        field_expected.substitute_named_param(tp, concrete);
                                }
                                let actual = self.check_against(fexpr, fs, &field_expected);

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
                                    format!("no field `{field_name}` on struct `{name}`"),
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
                // Reject mismatched qualified prefix (e.g. OtherEnum::Variant
                // when expected is MyEnum).
                let prefix_ok = !name.contains("::")
                    || name.split("::").next().unwrap_or("") == expected_enum_name.as_str();

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
                                        let mut field_expected = declared_ty.clone();
                                        for (tp, concrete) in &type_arg_map {
                                            field_expected =
                                                field_expected.substitute_named_param(tp, concrete);
                                        }
                                        let actual = self.check_against(fexpr, fs, &field_expected);
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
                    actual
                } else {
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
            (
                Expr::Identifier(name),
                Ty::Named {
                    name: expected_type_name,
                    ..
                },
            ) => {
                let is_unit_variant = self
                    .lookup_type_def(expected_type_name)
                    .and_then(|td| td.variants.get(name.as_str()).cloned())
                    .is_some_and(|v| matches!(v, VariantDef::Unit));
                if is_unit_variant {
                    self.record_type(span, expected);
                    expected.clone()
                } else {
                    // Not a unit variant of this type — synthesize and unify.
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

            // Default: synthesize and unify
            _ => {
                let actual = self.synthesize(expr, span);
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

    #[expect(
        clippy::too_many_lines,
        reason = "builtin method resolution requires many cases"
    )]
    pub(super) fn check_binary_op(
        &mut self,
        left: &Spanned<Expr>,
        op: BinaryOp,
        right: &Spanned<Expr>,
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

        match op {
            // Wrapping arithmetic: integer-only. No string concat, no duration,
            // no float. Both operands must be integer types of the same width.
            BinaryOp::WrappingAdd | BinaryOp::WrappingSub | BinaryOp::WrappingMul => {
                if left_resolved.is_integer() && right_resolved.is_integer() {
                    if let Some(common_ty) = common_integer_type(&left_resolved, &right_resolved) {
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
                if left_resolved.is_duration() || right_resolved.is_duration() {
                    return self.check_duration_arithmetic(
                        op,
                        &left_resolved,
                        &right_resolved,
                        &left.1,
                    );
                }
                if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if let Some(common_ty) = common_numeric_type(&left_resolved, &right_resolved) {
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
                    if let Some(common_ty) = common_integer_type(&left_resolved, &right_resolved) {
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
                if left_resolved.is_numeric() && right_resolved.is_numeric() {
                    if common_numeric_type(&left_resolved, &right_resolved).is_none() {
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
                    self.expect_type(&left_ty, &right_ty, &right.1);
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
                    if let Some(common_ty) = common_integer_type(&left_resolved, &right_resolved) {
                        // When both bounds are integer literals (e.g. `0..8`),
                        // use a fresh type variable so the element type can be
                        // inferred from context (e.g. how the loop variable is
                        // used).  If nothing constrains it, it stays as-is
                        // and defaults to the literal type (i64).
                        if left_is_coercible && right_is_coercible {
                            let var_tv = TypeVar::fresh();
                            // Stash the bound spans + literal values for the
                            // post-inference pass that re-records them with
                            // the concrete resolved element type.
                            self.deferred_range_bounds.push((
                                left.1.clone(),
                                var_tv,
                                extract_integer_literal_value(&left.0),
                            ));
                            self.deferred_range_bounds.push((
                                right.1.clone(),
                                var_tv,
                                extract_integer_literal_value(&right.0),
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

    /// Type-check an arithmetic operation where at least one operand is `duration`.
    pub(super) fn check_duration_arithmetic(
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

    pub(super) fn require_unsafe(&mut self, name: &str, span: &Span) {
        let scoped_unsafe = scoped_module_item_name(self.current_module.as_deref(), name)
            .is_some_and(|qualified| self.unsafe_functions.contains(&qualified));
        if !self.in_unsafe && (scoped_unsafe || self.unsafe_functions.contains(name)) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("calling extern function `{name}` requires `unsafe {{ ... }}`"),
            );
        }
    }

    /// Reject returning an `Rc<T>` parameter as a bare identifier without
    /// `.clone()`.  Under borrow-on-call semantics the callee does not own its
    /// Rc params — returning one aliases the caller's pointer, causing a
    /// double-free when both the caller's local and the callee's return value
    /// are dropped at their respective scope exits.  This is fail-closed: the
    /// error fires even though the double-free is a codegen/runtime bug, because
    /// no correct compilation is possible for this pattern today.
    pub(super) fn warn_rc_param_return(&mut self, fd: &FnDecl) {
        // Collect dangerous params: those with explicit Rc<_> type annotations.
        //
        // NOTE: generic type params (e.g. `x: T`) are NOT flagged here because
        // the danger only materialises when `T` is instantiated with `Rc<U>` at
        // a call site.  Definition-site checking would reject all generic
        // identity patterns (`fn id<T>(x: T) -> T { x }`) which are safe for
        // non-Rc types.  Call-site / monomorphisation-time checking is deferred
        // to a future slice.
        let dangerous_params: DangerousRcScope = fd
            .params
            .iter()
            .filter_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                if matches!(
                    ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Rc),
                        ..
                    }
                ) {
                    return Some((p.name.clone(), Some((p.name.clone(), "Rc".to_string()))));
                }
                None
            })
            .collect();
        if dangerous_params.is_empty() {
            return;
        }
        let mut scopes = vec![dangerous_params];
        self.scan_block_for_rc_param_return(&fd.body, &mut scopes);
    }

    /// If `expr` is a bare identifier matching one of the visible dangerous Rc
    /// bindings (or a block expression whose trailing expression is), emit a
    /// fail-closed error.
    pub(super) fn check_expr_is_rc_param_return(
        &mut self,
        expr: &Expr,
        span: &Span,
        scopes: &[DangerousRcScope],
    ) {
        match expr {
            Expr::Identifier(name) => {
                if let Some((source_param, _tag)) = Self::lookup_dangerous_binding(name, scopes) {
                    self.emit_borrowed_param_return(name, &source_param, span);
                }
            }
            // Descend into block expressions: `{ r }` wraps the identifier
            // in an Expr::Block whose local bindings may also shadow params.
            Expr::Block(blk) => {
                let mut nested_scopes = scopes.to_vec();
                self.scan_block_for_rc_param_return(blk, &mut nested_scopes);
            }
            // Aggregate escapes: enum-variant constructors like Some(r), Ok(r),
            // Err(r) embed an Rc param in a container, transferring the borrowed
            // pointer without a clone.  Only check calls whose callee looks like
            // a constructor (uppercase-initial identifier or Type::Variant path)
            // — regular lowercase function calls are borrows under call-boundary
            // ownership and are safe.
            Expr::Call { function, args, .. } if Self::looks_like_constructor(&function.0) => {
                for arg in args {
                    let (e, s) = arg.expr();
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Tuple literals: (r, 0), (r,) embed the borrowed Rc param.
            Expr::Tuple(elems) => {
                for (e, s) in elems {
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Struct initializers: MyStruct { field: r } embeds the borrowed Rc param.
            Expr::StructInit { fields, .. } => {
                for (_field_name, (e, s)) in fields {
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Promoted tail-position if/if-let/match: each branch can return an Rc
            // param.  Scan each arm's body the same way scan_stmts_for_rc_param_return
            // does for the Stmt::If / Stmt::IfLet / Stmt::Match variants.
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                // then_block is Box<Spanned<Expr>> wrapping Expr::Block
                self.check_expr_is_rc_param_return(&then_block.0, &then_block.1, scopes);
                if let Some(else_expr) = else_block {
                    self.check_expr_is_rc_param_return(&else_expr.0, &else_expr.1, scopes);
                }
            }
            Expr::IfLet {
                pattern,
                body,
                else_body,
                ..
            } => {
                let mut then_scopes = scopes.to_vec();
                Self::shadow_pattern_bindings(&pattern.0, &mut then_scopes);
                self.scan_block_for_rc_param_return(body, &mut then_scopes);
                if let Some(else_blk) = else_body {
                    let mut else_scopes = scopes.to_vec();
                    self.scan_block_for_rc_param_return(else_blk, &mut else_scopes);
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    let mut arm_scopes = scopes.to_vec();
                    Self::shadow_pattern_bindings(&arm.pattern.0, &mut arm_scopes);
                    self.check_expr_is_rc_param_return(&arm.body.0, &arm.body.1, &arm_scopes);
                }
            }
            _ => {}
        }
    }

    /// Returns `true` when the callee expression looks like a value constructor
    /// (enum variant or type-associated constructor like `Rc::new`).
    ///
    /// Heuristic: an uppercase-initial bare identifier (`Some`, `Ok`, `Err`,
    /// user-defined variants) or a `Type::method` / field-access path where
    /// the receiver is uppercase-initial (`Rc::new`, `MyEnum::Variant`).
    pub(super) fn looks_like_constructor(expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name) => name.starts_with(char::is_uppercase),
            Expr::FieldAccess { object, .. } => {
                // Rc::new, MyEnum::Variant, etc.
                matches!(&object.0, Expr::Identifier(name) if name.starts_with(char::is_uppercase))
            }
            _ => false,
        }
    }

    /// Return the nearest visible dangerous Rc binding for `name`.
    fn lookup_dangerous_binding(
        name: &str,
        scopes: &[DangerousRcScope],
    ) -> Option<DangerousRcBinding> {
        for scope in scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return binding.clone();
            }
        }
        None
    }

    fn current_dangerous_scope_mut(scopes: &mut [DangerousRcScope]) -> &mut DangerousRcScope {
        scopes
            .last_mut()
            .expect("borrowed Rc tracking always maintains at least one scope")
    }

    fn define_dangerous_binding(
        scopes: &mut [DangerousRcScope],
        name: String,
        binding: Option<DangerousRcBinding>,
    ) {
        Self::current_dangerous_scope_mut(scopes).insert(name, binding);
    }

    fn update_dangerous_binding(
        scopes: &mut [DangerousRcScope],
        name: &str,
        binding: Option<DangerousRcBinding>,
    ) {
        for scope in scopes.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(name) {
                *existing = binding;
                return;
            }
        }
        Self::define_dangerous_binding(scopes, name.to_string(), binding);
    }

    fn shadow_pattern_bindings(pattern: &Pattern, scopes: &mut [DangerousRcScope]) {
        match pattern {
            Pattern::Identifier(name) => {
                Self::define_dangerous_binding(scopes, name.clone(), None);
            }
            Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
                for (pattern, _) in patterns {
                    Self::shadow_pattern_bindings(pattern, scopes);
                }
            }
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    if let Some((pattern, _)) = &field.pattern {
                        Self::shadow_pattern_bindings(pattern, scopes);
                    } else {
                        Self::define_dangerous_binding(scopes, field.name.clone(), None);
                    }
                }
            }
            Pattern::Or(left, right) => {
                Self::shadow_pattern_bindings(&left.0, scopes);
                Self::shadow_pattern_bindings(&right.0, scopes);
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Regex { .. } => {}
        }
    }

    fn emit_borrowed_param_return(&mut self, name: &str, source_param: &str, span: &Span) {
        let (message, note, suggestion) = if name == source_param {
            (
                format!(
                    "returning Rc parameter `{name}` transfers a borrowed reference \
                     without incrementing the refcount — this will cause a double-free \
                     when both the caller's local and the return value are dropped"
                ),
                "function parameters are borrowed under call-boundary ownership; \
                 the caller retains ownership and drops at scope exit"
                    .to_string(),
                format!(
                    "use `{name}.clone()` to create an owned copy with an incremented refcount"
                ),
            )
        } else {
            (
                format!(
                    "returning local `{name}` which contains borrowed parameter \
                     `{source_param}` — the parameter was stored without cloning, \
                     causing a double-free when both the caller's local and the \
                     return value are dropped"
                ),
                format!(
                    "parameter `{source_param}` is borrowed under call-boundary \
                     ownership; storing it in `{name}` does not transfer ownership"
                ),
                format!("clone the parameter before storing: `{source_param}.clone()`"),
            )
        };
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::BorrowedParamReturn,
            span: span.clone(),
            message,
            notes: vec![(span.clone(), note)],
            suggestions: vec![suggestion],
            source_module: self.current_module.clone(),
        });
    }

    /// Check if `expr` directly names or structurally contains a visible
    /// dangerous Rc binding. Returns the first match or `None`.
    ///
    /// Structural descent mirrors `check_expr_is_rc_param_return`: constructors
    /// (uppercase-initial calls), tuples, struct inits, and blocks are
    /// containers that embed the value.  Regular lowercase function/method
    /// calls are borrows under call-boundary ownership — the return value is
    /// unrelated, so we do NOT recurse into those.
    pub(super) fn dangerous_source_in_expr(
        &mut self,
        expr: &Expr,
        scopes: &[DangerousRcScope],
    ) -> Option<DangerousRcBinding> {
        match expr {
            Expr::Identifier(name) => Self::lookup_dangerous_binding(name, scopes),
            Expr::Call { function, args, .. } if Self::looks_like_constructor(&function.0) => {
                for arg in args {
                    let (e, _) = arg.expr();
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Tuple(elems) => {
                for (e, _) in elems {
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::StructInit { fields, .. } => {
                for (_, (e, _)) in fields {
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Block(blk) => {
                let mut nested_scopes = scopes.to_vec();
                nested_scopes.push(HashMap::new());
                self.scan_stmts_for_rc_param_return(&blk.stmts, &mut nested_scopes);
                blk.trailing_expr
                    .as_deref()
                    .and_then(|(e, _)| self.dangerous_source_in_expr(e, &nested_scopes))
            }
            _ => None,
        }
    }

    pub(super) fn scan_block_for_rc_param_return(
        &mut self,
        block: &Block,
        scopes: &mut Vec<DangerousRcScope>,
    ) {
        scopes.push(HashMap::new());
        self.scan_stmts_for_rc_param_return(&block.stmts, scopes);
        if let Some(trailing) = &block.trailing_expr {
            self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
        }
        scopes.pop();
    }

    /// Recursively scan statements for `return <rc_param_ident>`,
    /// `break <rc_param_ident>`, and nested control-flow bodies.
    #[expect(
        clippy::too_many_lines,
        reason = "borrowed Rc escape scanning covers many statement forms"
    )]
    pub(super) fn scan_stmts_for_rc_param_return(
        &mut self,
        stmts: &[Spanned<Stmt>],
        scopes: &mut Vec<DangerousRcScope>,
    ) {
        for (stmt, _span) in stmts {
            match stmt {
                Stmt::Let { pattern, value, .. } => {
                    let binding = value
                        .as_ref()
                        .and_then(|(expr, _)| self.dangerous_source_in_expr(expr, scopes));
                    match &pattern.0 {
                        Pattern::Identifier(name) => {
                            Self::define_dangerous_binding(scopes, name.clone(), binding);
                        }
                        _ => {
                            Self::shadow_pattern_bindings(&pattern.0, scopes);
                        }
                    }
                }
                Stmt::Var { name, value, .. } => {
                    let binding = value
                        .as_ref()
                        .and_then(|(expr, _)| self.dangerous_source_in_expr(expr, scopes));
                    Self::define_dangerous_binding(scopes, name.clone(), binding);
                }
                Stmt::Assign {
                    target: (Expr::Identifier(name), _),
                    value: (expr, _),
                    ..
                } => {
                    let binding = self.dangerous_source_in_expr(expr, scopes);
                    Self::update_dangerous_binding(scopes, name, binding);
                }
                Stmt::Assign {
                    target: (Expr::FieldAccess { object, .. }, _),
                    value: (expr, _),
                    ..
                } => {
                    if let Expr::Identifier(obj_name) = &object.0 {
                        if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                            Self::update_dangerous_binding(scopes, obj_name, Some(binding));
                        }
                    }
                }
                Stmt::Return(Some((expr, es)))
                | Stmt::Break {
                    value: Some((expr, es)),
                    ..
                } => {
                    self.check_expr_is_rc_param_return(expr, es, scopes);
                }
                Stmt::Expression((
                    Expr::MethodCall {
                        receiver,
                        method,
                        args,
                        ..
                    },
                    _,
                )) => {
                    const STORING_METHODS: &[&str] = &["push", "set", "insert", "extend", "append"];
                    if STORING_METHODS.contains(&method.as_str()) {
                        if let Expr::Identifier(recv_name) = &receiver.0 {
                            for arg in args {
                                let (expr, _) = arg.expr();
                                if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                                    Self::update_dangerous_binding(
                                        scopes,
                                        recv_name,
                                        Some(binding),
                                    );
                                    break;
                                }
                            }
                        }
                    }
                }
                Stmt::Expression((Expr::Block(block), _)) => {
                    self.scan_block_for_rc_param_return(block, scopes);
                }
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.scan_block_for_rc_param_return(then_block, scopes);
                    if let Some(else_blk) = else_block {
                        if let Some(if_stmt) = &else_blk.if_stmt {
                            // else-if: recurse into the nested Stmt::If
                            self.scan_stmts_for_rc_param_return(
                                std::slice::from_ref(if_stmt.as_ref()),
                                scopes,
                            );
                        }
                        if let Some(blk) = &else_blk.block {
                            self.scan_block_for_rc_param_return(blk, scopes);
                        }
                    }
                }
                Stmt::For { pattern, body, .. } => {
                    scopes.push(HashMap::new());
                    Self::shadow_pattern_bindings(&pattern.0, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
                    }
                    scopes.pop();
                }
                Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
                    self.scan_block_for_rc_param_return(body, scopes);
                }
                Stmt::WhileLet { pattern, body, .. } => {
                    scopes.push(HashMap::new());
                    Self::shadow_pattern_bindings(&pattern.0, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
                    }
                    scopes.pop();
                }
                Stmt::IfLet {
                    pattern,
                    body,
                    else_body,
                    ..
                } => {
                    scopes.push(HashMap::new());
                    Self::shadow_pattern_bindings(&pattern.0, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(then_trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(
                            &then_trailing.0,
                            &then_trailing.1,
                            scopes,
                        );
                    }
                    scopes.pop();
                    if let Some(else_blk) = else_body {
                        self.scan_block_for_rc_param_return(else_blk, scopes);
                    }
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        scopes.push(HashMap::new());
                        Self::shadow_pattern_bindings(&arm.pattern.0, scopes);
                        // Match arm body is an Expr — check if it's a bare Rc param
                        self.check_expr_is_rc_param_return(&arm.body.0, &arm.body.1, scopes);
                        scopes.pop();
                    }
                }
                _ => {}
            }
        }
    }

    pub(super) fn reject_owned_handle_field_accessors(&mut self, fd: &FnDecl) {
        let Some((type_name, _)) = self.current_self_type.clone() else {
            return;
        };
        if !self.struct_is_handle_bearing(&type_name) {
            return;
        }
        let Some(receiver_name) = fd
            .params
            .first()
            .filter(|param| self.is_receiver_param(param))
            .map(|param| param.name.clone())
        else {
            return;
        };

        // `bindings` maps let-binding variable names to the (field_name,
        // handle_type_name) they alias, so `return p` after `let p = self.field`
        // fires the same diagnostic as a direct `return self.field`.
        let mut bindings: HashMap<String, (String, String)> = HashMap::new();
        self.scan_block_for_owned_handle_field_return(
            &fd.body,
            &receiver_name,
            &type_name,
            &fd.name,
            &mut bindings,
        );
    }

    fn struct_is_handle_bearing(&mut self, type_name: &str) -> bool {
        self.ensure_handle_bearing_fresh();
        self.handle_bearing_structs.contains(type_name)
            || self
                .registered_type_def_name(type_name)
                .is_some_and(|name| self.handle_bearing_structs.contains(&name))
            || self
                .strip_module_prefix(type_name)
                .is_some_and(|name| self.handle_bearing_structs.contains(name))
    }

    fn scan_block_for_owned_handle_field_return(
        &mut self,
        block: &Block,
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        self.scan_stmts_for_owned_handle_field_return(
            &block.stmts,
            receiver_name,
            type_name,
            method_name,
            bindings,
        );
        if let Some(trailing) = &block.trailing_expr {
            self.check_expr_for_owned_handle_field_return(
                &trailing.0,
                &trailing.1,
                receiver_name,
                type_name,
                method_name,
                bindings,
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "owned-handle bind-then-return scanning covers many statement forms"
    )]
    fn scan_stmts_for_owned_handle_field_return(
        &mut self,
        stmts: &[Spanned<Stmt>],
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        for (stmt, _) in stmts {
            match stmt {
                // Track `let p = receiver.field` when `field` is an owned
                // handle — a subsequent `return p` is the same double-free risk
                // as `return receiver.field` directly.
                Stmt::Let {
                    pattern: (Pattern::Identifier(var_name), _),
                    value: Some((Expr::FieldAccess { object, field }, _)),
                    ..
                } if matches!(&object.0, Expr::Identifier(n) if n == receiver_name) => {
                    if let Some((field_name, handle_name)) =
                        self.owned_handle_field_return_by_name(field, type_name)
                    {
                        bindings.insert(var_name.clone(), (field_name, handle_name));
                    }
                }
                Stmt::Return(Some((expr, span))) => self.check_expr_for_owned_handle_field_return(
                    expr,
                    span,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                ),
                Stmt::Expression((Expr::Block(block), _))
                | Stmt::Loop { body: block, .. }
                | Stmt::While { body: block, .. }
                | Stmt::For { body: block, .. }
                | Stmt::WhileLet { body: block, .. } => self
                    .scan_block_for_owned_handle_field_return(
                        block,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    ),
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.scan_block_for_owned_handle_field_return(
                        then_block,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                    if let Some(else_block) = else_block {
                        if let Some(if_stmt) = &else_block.if_stmt {
                            self.scan_stmts_for_owned_handle_field_return(
                                std::slice::from_ref(if_stmt.as_ref()),
                                receiver_name,
                                type_name,
                                method_name,
                                bindings,
                            );
                        }
                        if let Some(block) = &else_block.block {
                            self.scan_block_for_owned_handle_field_return(
                                block,
                                receiver_name,
                                type_name,
                                method_name,
                                bindings,
                            );
                        }
                    }
                }
                Stmt::IfLet {
                    body, else_body, ..
                } => {
                    self.scan_block_for_owned_handle_field_return(
                        body,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                    if let Some(block) = else_body {
                        self.scan_block_for_owned_handle_field_return(
                            block,
                            receiver_name,
                            type_name,
                            method_name,
                            bindings,
                        );
                    }
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        self.check_expr_for_owned_handle_field_return(
                            &arm.body.0,
                            &arm.body.1,
                            receiver_name,
                            type_name,
                            method_name,
                            bindings,
                        );
                    }
                }
                Stmt::Let { .. }
                | Stmt::Var { .. }
                | Stmt::Assign { .. }
                | Stmt::Break { .. }
                | Stmt::Continue { .. }
                | Stmt::Return(None)
                | Stmt::Expression(_)
                | Stmt::Defer(_) => {}
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "owned-handle return-position scanning covers many expression forms"
    )]
    fn check_expr_for_owned_handle_field_return(
        &mut self,
        expr: &Expr,
        span: &Span,
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        // Direct `return receiver.field` — the original check.
        if let Some((field_name, handle_name)) =
            self.owned_handle_field_return(expr, receiver_name, type_name)
        {
            self.report_owned_handle_field_return(
                span,
                method_name,
                type_name,
                &field_name,
                &handle_name,
                None,
            );
            return;
        }

        // Bind-then-return: `let p = receiver.field; … return p`
        // The alias `p` is still an unprotected return path — same double-free
        // risk as returning the field directly. The check is conservative: if
        // `p` has been observed as an alias of any owned handle field in this
        // method body, we always flag it, even if there are intermediate uses.
        if let Expr::Identifier(var_name) = expr {
            if let Some((field_name, handle_name)) = bindings.get(var_name).cloned() {
                self.report_owned_handle_field_return(
                    span,
                    method_name,
                    type_name,
                    &field_name,
                    &handle_name,
                    Some(var_name),
                );
                return;
            }
        }

        match expr {
            Expr::Block(block) => self.scan_block_for_owned_handle_field_return(
                block,
                receiver_name,
                type_name,
                method_name,
                bindings,
            ),
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                self.check_expr_for_owned_handle_field_return(
                    &then_block.0,
                    &then_block.1,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                );
                if let Some(else_expr) = else_block {
                    self.check_expr_for_owned_handle_field_return(
                        &else_expr.0,
                        &else_expr.1,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::IfLet {
                body, else_body, ..
            } => {
                self.scan_block_for_owned_handle_field_return(
                    body,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                );
                if let Some(block) = else_body {
                    self.scan_block_for_owned_handle_field_return(
                        block,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.check_expr_for_owned_handle_field_return(
                        &arm.body.0,
                        &arm.body.1,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::Binary { .. }
            | Expr::Unary { .. }
            | Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::Tuple(_)
            | Expr::Array(_)
            | Expr::ArrayRepeat { .. }
            | Expr::MapLiteral { .. }
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
            | Expr::Cooperate
            | Expr::This
            | Expr::FieldAccess { .. }
            | Expr::Index { .. }
            | Expr::Cast { .. }
            | Expr::PostfixTry(_)
            | Expr::Range { .. }
            | Expr::Await(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::MachineEmit { .. }
            | Expr::Is { .. }
            | Expr::GenBlock { .. } => {}
        }
    }

    fn report_owned_handle_field_return(
        &mut self,
        span: &Span,
        method_name: &str,
        type_name: &str,
        field_name: &str,
        handle_name: &str,
        via_binding: Option<&str>,
    ) {
        let via_note =
            via_binding.map_or_else(String::new, |b| format!(" (via let-binding `{b}`)"));
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::InvalidOperation,
            span: span.clone(),
            message: format!(
                "method `{method_name}` exposes owned handle field `{field_name}` from \
                 `{type_name}`{via_note} — returning the raw `{handle_name}` aliases the \
                 wrapper's drop path and can double-free the handle"
            ),
            notes: vec![(
                span.clone(),
                "handle-bearing structs are dropped field-by-field; returning the raw handle \
                 bypasses that ownership proof"
                    .to_string(),
            )],
            suggestions: vec![
                "use a dedicated consume/release API instead of returning the raw handle field"
                    .to_string(),
                "prefer a borrow-style accessor once mutable receivers / borrow returns land \
                 (#1295)"
                    .to_string(),
            ],
            source_module: self.current_module.clone(),
        });
    }

    fn owned_handle_field_return(
        &self,
        expr: &Expr,
        receiver_name: &str,
        type_name: &str,
    ) -> Option<(String, String)> {
        let Expr::FieldAccess { object, field } = expr else {
            return None;
        };
        if !matches!(&object.0, Expr::Identifier(name) if name == receiver_name) {
            return None;
        }
        self.owned_handle_field_return_by_name(field, type_name)
    }

    /// Check whether the named field of `type_name` holds an owned handle.
    /// Returns `Some((field_name, handle_type_name))` when it does.
    /// Used by both the direct-return check and the bind-then-return scan.
    fn owned_handle_field_return_by_name(
        &self,
        field: &str,
        type_name: &str,
    ) -> Option<(String, String)> {
        let type_def = self.lookup_type_def(type_name)?;
        let field_ty = type_def.fields.get(field)?;
        let Ty::Named {
            name: field_type_name,
            ..
        } = field_ty
        else {
            return None;
        };
        self.canonical_owned_handle_type_name(field_type_name)
            .map(|handle_name| (field.to_string(), handle_name))
    }

    #[expect(
        clippy::too_many_lines,
        reason = "field access handles many type variants"
    )]
    pub(super) fn check_field_access(
        &mut self,
        object: &Spanned<Expr>,
        field: &str,
        span: &Span,
    ) -> Ty {
        let obj_ty = self.synthesize(&object.0, &object.1);
        let resolved = self.subst.resolve(&obj_ty);
        match &resolved {
            Ty::Named { name, args, .. } => {
                // Named supervisor child access: sup.child_name → LocalPid<ChildType>
                // Accepts local actor handles via as_actor_handle().
                if let Some(Ty::Named { name: sup_name, .. }) = resolved.as_actor_handle() {
                    if let Some(sup_children) = self.supervisor_children.get(sup_name) {
                        // Check static children first, then pool children.
                        let resolved_slot =
                            sup_children
                                .statics
                                .iter()
                                .enumerate()
                                .find_map(|(idx, (cn, ty))| {
                                    (cn == field).then(|| {
                                    (
                                        crate::check::types::ChildSlot {
                                            kind: crate::check::types::ChildKind::Static,
                                            #[expect(
                                                clippy::cast_possible_truncation,
                                                reason = "supervisor child count is always small; \
                                                          u32 is the ABI type"
                                            )]
                                            index: idx as u32,
                                            child_ty: ty.clone(),
                                        },
                                        ty.clone(),
                                    )
                                })
                                });
                        let resolved_slot = resolved_slot.or_else(|| {
                            sup_children
                                .pools
                                .iter()
                                .enumerate()
                                .find_map(|(idx, (cn, ty))| {
                                    (cn == field).then(|| {
                                    (
                                        crate::check::types::ChildSlot {
                                            kind: crate::check::types::ChildKind::Pool,
                                            #[expect(
                                                clippy::cast_possible_truncation,
                                                reason = "supervisor child count is always small; \
                                                          u32 is the ABI type"
                                            )]
                                            index: idx as u32,
                                            child_ty: ty.clone(),
                                        },
                                        ty.clone(),
                                    )
                                })
                                })
                        });
                        if let Some((slot, child_type)) = resolved_slot {
                            self.supervisor_child_slots
                                .insert(SpanKey::from(span), slot);
                            return Ty::local_pid(Ty::Named {
                                builtin: None,
                                name: child_type,
                                args: vec![],
                            });
                        }
                        // The supervisor is known but this child name is not declared.
                        // Emit a clear diagnostic and stop — the fallthrough branch
                        // would silently return Ty::Error because the handle has no type
                        // definition in the checker's type_defs map.
                        let all_names: Vec<&str> = sup_children
                            .statics
                            .iter()
                            .chain(sup_children.pools.iter())
                            .map(|(cn, _)| cn.as_str())
                            .collect();
                        let similar = crate::error::find_similar(field, all_names.iter().copied());
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("supervisor `{sup_name}` has no child named `{field}`"),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(field_ty) = td.fields.get(field) {
                        // Substitute generic type params with concrete args
                        let mut result_ty = field_ty.clone();
                        for (param, arg) in td.type_params.iter().zip(args.iter()) {
                            result_ty = result_ty.substitute_named_param(param, arg);
                        }
                        result_ty
                    } else if let Some((ref mn, ref src_state, ref evt_name)) =
                        self.current_machine_transition
                    {
                        if td.kind == TypeDefKind::Machine && mn == name {
                            if let Some(VariantDef::Struct(variant_fields)) =
                                td.variants.get(src_state).cloned()
                            {
                                if let Some((_, field_ty)) =
                                    variant_fields.iter().find(|(fname, _)| fname == field)
                                {
                                    return field_ty.clone();
                                }
                            }
                        }

                        // Inside a machine transition: resolve `event.field` on the event enum type
                        let event_type_name = format!("{mn}Event");
                        if *name == event_type_name && evt_name != "_" {
                            if let Some(VariantDef::Struct(variant_fields)) =
                                td.variants.get(evt_name).cloned()
                            {
                                if let Some((_, field_ty)) =
                                    variant_fields.iter().find(|(fname, _)| fname == field)
                                {
                                    return field_ty.clone();
                                }
                            }
                        }
                        let similar =
                            crate::error::find_similar(field, td.fields.keys().map(String::as_str));
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("no field `{field}` on type `{name}`"),
                            similar,
                        );
                        Ty::Error
                    } else {
                        let similar =
                            crate::error::find_similar(field, td.fields.keys().map(String::as_str));
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("no field `{field}` on type `{name}`"),
                            similar,
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            Ty::Tuple(elems) => {
                // Tuple field access by index: t.0, t.1
                if let Ok(idx) = field.parse::<usize>() {
                    if idx < elems.len() {
                        elems[idx].clone()
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("tuple index {idx} out of range (len {})", elems.len()),
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            _ => {
                if resolved != Ty::Error {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "cannot access field `{field}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(super) fn check_match_expr(
        &mut self,
        scrutinee_ty: &Ty,
        arms: &[MatchArm],
        span: &Span,
        expected: Option<&Ty>,
    ) -> Ty {
        if arms.is_empty() {
            return Ty::Unit;
        }

        // If the enclosing context supplies a concrete expected type (e.g. the
        // function's declared return type), pre-seed result_ty so every arm body
        // is checked with check_against rather than having the first arm's
        // synthesized type (which defaults literals to i64) propagate to later arms.
        let resolved_expected = expected.map(|ty| self.subst.resolve(ty));
        let mut result_ty: Option<Ty> = match &resolved_expected {
            Some(ty) if !matches!(ty, Ty::Var(_) | Ty::Error) => Some(ty.clone()),
            _ => None,
        };
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            // Check guard if present
            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            let arm_ty = if let Some(expected) = &result_ty {
                self.check_expr_with_expected(&arm.body.0, &arm.body.1, expected)
            } else {
                self.synthesize(&arm.body.0, &arm.body.1)
            };
            // Skip Never/Error when setting the expected type — diverging arms
            // (return, panic, break) shouldn't constrain the match result type.
            if result_ty.is_none() && !matches!(arm_ty, Ty::Never | Ty::Error) {
                result_ty = Some(arm_ty);
            }

            self.env.pop_scope();
        }

        // Exhaustiveness check for enums/Option/Result
        self.check_exhaustiveness(scrutinee_ty, arms, span);

        // If all arms diverge (Never/Error), the match itself diverges
        result_ty.unwrap_or(Ty::Never)
    }

    #[expect(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(super) fn check_lambda(
        &mut self,
        is_move: bool,
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
    ) -> Ty {
        // Save/restore capture tracking state for nested lambdas
        let prev_capture_depth = self.lambda_capture_depth;
        let prev_captures = std::mem::take(&mut self.lambda_captures);
        let prev_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = false;

        // Record the scope depth BEFORE pushing the lambda scope — any variable
        // found below this depth during body checking is a capture.
        let capture_depth = self.env.depth();
        self.lambda_capture_depth = Some(capture_depth);

        // Clear any stale scratch state from a previous call in a non-let or
        // nested context.  We unconditionally reset first so that re-entrant
        // calls (e.g., a generic lambda inside a function argument) cannot
        // bleed their type-var pairs out to an unrelated enclosing Stmt::Let.
        self.last_lambda_generic_sig = None;

        let mut generic_bindings = std::collections::HashMap::new();
        let mut generic_param_names = HashMap::new();
        let mut generic_type_vars = Vec::new();
        if let Some(tps) = type_params {
            for tp in tps {
                let tv = TypeVar::fresh();
                generic_bindings.insert(tp.name.clone(), Ty::Var(tv));
                generic_param_names.insert(tv.0, tp.name.clone());
                generic_type_vars.push(tv);
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        self.env.push_scope();
        let prev_in_generator = self.in_generator;
        self.in_generator = false;

        // Check arity mismatch: lambda parameter count must match expected function type
        if let Some((expected_params, _)) = &expected {
            if params.len() != expected_params.len() {
                self.errors.push(TypeError::new(
                    TypeErrorKind::ArityMismatch,
                    span.clone(),
                    format!(
                        "lambda has {} parameters but expected function type has {}",
                        params.len(),
                        expected_params.len()
                    ),
                ));
            }
        }

        let mut param_tys = Vec::new();
        for (i, p) in params.iter().enumerate() {
            let ty = if let Some(annotation) = &p.ty {
                let (annotated_ty, hole_vars) = self.resolve_annotation_holes(annotation);
                if !hole_vars.is_empty() {
                    if let Some((expected_params, _)) = &expected {
                        if let Some(expected_ty) = expected_params.get(i) {
                            self.expect_type(expected_ty, &annotated_ty, &annotation.1);
                        }
                    }
                    self.record_deferred_inference_holes(
                        annotation,
                        format!("lambda parameter `{}`", p.name),
                        hole_vars,
                    );
                }
                self.subst.resolve(&annotated_ty)
            } else if let Some((expected_params, _)) = &expected {
                expected_params
                    .get(i)
                    .cloned()
                    .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
            } else {
                Ty::Var(TypeVar::fresh())
            };
            self.env.define(p.name.clone(), ty.clone(), false);
            param_tys.push(ty);
        }

        // Save enclosing return type and install the lambda's own return type so
        // that PostfixTry (`?`) context checks see the lambda's return type,
        // not the outer function's.
        let prev_return_type = self.current_return_type.take();

        let ret_ty = if let Some(annotation) = return_type {
            let (expected_ret, hole_vars) = self.resolve_annotation_holes(annotation);
            if !hole_vars.is_empty() {
                if let Some((_, contextual_ret)) = expected {
                    self.expect_type(contextual_ret, &expected_ret, &annotation.1);
                }
                self.record_deferred_inference_holes(annotation, "lambda return type", hole_vars);
            }
            self.current_return_type = Some(expected_ret.clone());
            // Guard: do not pre-seed body with Ty::Error (unresolvable annotation).
            // Synthesize instead so internal body errors are still reported.
            let resolved_ret = self.subst.resolve(&expected_ret);
            if matches!(resolved_ret, Ty::Error) {
                self.synthesize(&body.0, &body.1);
            } else {
                self.check_against(&body.0, &body.1, &expected_ret);
            }
            self.subst.resolve(&expected_ret)
        } else if let Some((_, expected_ret)) = expected {
            self.current_return_type = Some(expected_ret.clone());
            self.check_against(&body.0, &body.1, expected_ret);
            expected_ret.clone()
        } else {
            // Return type is fully inferred: leave current_return_type as None
            // (already taken above) so `?` is not validated against a stale
            // outer return type during synthesis.
            self.synthesize(&body.0, &body.1)
        };

        self.current_return_type = prev_return_type;
        self.in_actor_handler_context = prev_actor_handler_context;
        self.in_generator = prev_in_generator;
        self.env.pop_scope();

        if let Some(tps) = type_params {
            if !tps.is_empty() {
                let type_param_bounds = tps
                    .iter()
                    .filter_map(|tp| {
                        if tp.bounds.is_empty() {
                            None
                        } else {
                            Some((
                                tp.name.clone(),
                                tp.bounds.iter().map(|bound| bound.name.clone()).collect(),
                            ))
                        }
                    })
                    .collect();
                self.last_lambda_generic_sig = Some(GenericLambdaSig {
                    call_sig: FnSig {
                        type_params: tps.iter().map(|tp| tp.name.clone()).collect(),
                        type_param_bounds,
                        param_names: params.iter().map(|param| param.name.clone()).collect(),
                        params: param_tys
                            .iter()
                            .map(|param| {
                                Self::lambda_generic_schema_ty(param, &generic_param_names)
                            })
                            .collect(),
                        return_type: Self::lambda_generic_schema_ty(&ret_ty, &generic_param_names),
                        ..FnSig::default()
                    },
                    type_vars: generic_type_vars,
                });
                self.generic_ctx.pop();
            }
        }

        // Collect binding-accurate captures, preserving first-use order.
        let raw_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        let mut seen_capture_bindings = HashSet::new();
        let mut capture_facts = Vec::new();
        for mut fact in raw_capture_facts {
            if !seen_capture_bindings.insert(fact.binding_id) {
                continue;
            }
            let resolved_ty = self.subst.resolve(&fact.ty).materialize_literal_defaults();
            let is_copy = self
                .registry
                .implements_marker(&resolved_ty, MarkerTrait::Copy);
            fact.is_send = self
                .registry
                .implements_marker(&resolved_ty, MarkerTrait::Send);
            fact.mode = if is_move {
                ClosureCaptureMode::Move
            } else {
                ClosureCaptureMode::Copy
            };
            fact.ty = resolved_ty.clone();
            if !is_move && !is_copy && !matches!(resolved_ty, Ty::Error | Ty::Var(_)) {
                self.report_error(
                    TypeErrorKind::ClosureExplicitMoveRequired {
                        name: fact.name.clone(),
                        ty: resolved_ty.user_facing().to_string(),
                    },
                    &fact.use_span,
                    format!(
                        "closure captures non-Copy binding `{}` by value; use `move |...|` to consume `{}` explicitly",
                        fact.name, fact.name
                    ),
                );
            }
            if is_move && !is_copy {
                self.env.mark_moved(&fact.name, span.clone());
            }
            capture_facts.push(fact);
        }
        self.closure_capture_facts
            .insert(SpanKey::from(span), capture_facts.clone());

        // Keep the public callable type shape unchanged while deriving its
        // capture payload from the binding-accurate ledger.
        let captures: Vec<Ty> = capture_facts.iter().map(|fact| fact.ty.clone()).collect();

        // Restore outer capture tracking state
        self.lambda_capture_depth = prev_capture_depth;
        self.lambda_captures = prev_captures;
        self.lambda_capture_facts = prev_capture_facts;

        if captures.is_empty() {
            Ty::Function {
                params: param_tys,
                ret: Box::new(ret_ty),
            }
        } else {
            Ty::Closure {
                params: param_tys,
                ret: Box::new(ret_ty),
                captures,
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "trait impl checking requires many cases"
    )]
    pub(super) fn check_struct_init(
        &mut self,
        name: &str,
        fields: &[(String, Spanned<Expr>)],
        type_args: Option<&[Spanned<TypeExpr>]>,
        base: Option<&Spanned<Expr>>,
        span: &Span,
    ) -> Ty {
        if let Some(td) = self.lookup_type_def(name) {
            // Track inferred type arguments for generic structs.
            // If the caller supplied explicit type args (e.g. `Wrapper<String> { ... }`),
            // pre-seed the map from them so field checking constrains against the
            // declared types immediately rather than synthesizing unconstrained.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == td.type_params.len() {
                    for (tp, te) in td.type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Foo<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "struct `{name}` has {} type parameter(s) but {} were supplied",
                            td.type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            for (field_name, (expr, es)) in fields {
                if let Some(declared_ty) = td.fields.get(field_name) {
                    // Substitute already-inferred type params into the expected type
                    let mut expected = declared_ty.clone();
                    for (tp, concrete) in &type_arg_map {
                        expected = expected.substitute_named_param(tp, concrete);
                    }

                    // If the expected type is still an unbound type parameter,
                    // synthesize so the field value determines the type (rather
                    // than failing with "expected T, found i64").
                    let is_unbound_param = td.type_params.iter().any(|tp| {
                        !type_arg_map.contains_key(tp)
                            && expected
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                    });
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };

                    // Infer type params: if field type is a bare type param, bind it
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
                        format!("no field `{field_name}` on struct `{name}`"),
                        similar,
                    );
                }
            }
            // Functional-update base: `R { x: 5, ..base }`.
            // The base must evaluate to the same named record/struct type.
            // When base is present, fields not listed explicitly are filled from base,
            // so the missing-field check is skipped.
            if let Some((base_expr, base_span)) = base {
                let base_ty = self.synthesize(base_expr, base_span);
                match &base_ty {
                    Ty::Named {
                        name: base_name, ..
                    } if base_name == name => {}
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            format!(
                                "functional-update base must be of type `{name}`, found `{base_ty}`"
                            ),
                        );
                    }
                }
            } else {
                // No base: all fields must be explicitly provided.
                let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
                for declared in td.fields.keys() {
                    if !provided.contains(declared.as_str()) {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("missing field `{declared}` in initializer of `{name}`"),
                        );
                    }
                }
            }
            // Build type args from inferred bindings
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|tp| {
                    type_arg_map
                        .get(tp)
                        .cloned()
                        .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                })
                .collect();
            // Record the resolved type arguments for downstream monomorphisation
            // (HIR registry, MIR per-instantiation RecordLayout).
            //
            // Emit unconditionally: a record-init's type args may only become
            // fully concrete *after* `check_struct_init` returns (e.g. via an
            // outer annotation `let b: Box<int> = Box { value: 1 }`), so
            // eagerly rejecting at emission time would drop entries that the
            // post-inference boundary resolve in `check_program` would have made
            // concrete.  The fail-closed contract (no `Ty::Var` crosses into HIR)
            // is enforced at the output boundary by
            // `validate_record_init_type_args_output_contract` in `admissibility.rs`.
            self.record_concrete_record_init_type_args(span, &type_args);
            Ty::Named {
                builtin: None,
                name: name.to_string(),
                args: type_args,
            }
        } else if let Some((enum_name, variant_fields, enum_type_params)) =
            self.type_defs.iter().find_map(|(type_name, td)| {
                let short = name.rsplit("::").next().unwrap_or(name);
                // For qualified names (e.g., Keeper::Holding), verify prefix
                if name.contains("::") {
                    let prefix = name.split("::").next().unwrap_or("");
                    if prefix != type_name {
                        return None;
                    }
                }
                match td.variants.get(name).or_else(|| td.variants.get(short)) {
                    Some(VariantDef::Struct(fields)) => {
                        Some((type_name.clone(), fields.clone(), td.type_params.clone()))
                    }
                    _ => None,
                }
            })
        {
            // Infer generic type args from field values, mirroring the plain-struct path.
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();
            // If the caller supplied explicit type args (e.g. `Keeper::Holding<int> { … }`),
            // pre-seed the map so field checking constrains against the declared types
            // rather than synthesizing unconstrained.
            if let Some(explicit_args) = type_args {
                if explicit_args.len() == enum_type_params.len() {
                    for (tp, te) in enum_type_params.iter().zip(explicit_args.iter()) {
                        let resolved = self.resolve_type_expr(te);
                        type_arg_map.insert(tp.clone(), resolved);
                    }
                } else {
                    // Covers both `Variant<>` (zero explicit args) and wrong-count args.
                    self.report_error(
                        TypeErrorKind::ArityMismatch,
                        span,
                        format!(
                            "enum variant `{name}` has {} type parameter(s) but {} were supplied",
                            enum_type_params.len(),
                            explicit_args.len()
                        ),
                    );
                }
            }

            for (field_name, (expr, es)) in fields {
                if let Some((_, declared_ty)) = variant_fields.iter().find(|(n, _)| n == field_name)
                {
                    // Substitute already-inferred type params into the expected type
                    let mut expected = declared_ty.clone();
                    for (tp, concrete) in &type_arg_map {
                        expected = expected.substitute_named_param(tp, concrete);
                    }

                    // If the expected type is still an unbound type parameter, synthesize
                    // so the field value determines the concrete type.
                    let is_unbound_param = enum_type_params.iter().any(|tp| {
                        !type_arg_map.contains_key(tp)
                            && expected
                                == (Ty::Named {
                                    builtin: None,
                                    name: tp.clone(),
                                    args: vec![],
                                })
                    });
                    let actual = if is_unbound_param {
                        self.synthesize(expr, es)
                    } else {
                        self.check_against(expr, es, &expected)
                    };

                    // Bind bare type params from this field's declared type
                    for tp in &enum_type_params {
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
            let provided: HashSet<&str> = fields.iter().map(|(n, _)| n.as_str()).collect();
            for (declared, _) in &variant_fields {
                if !provided.contains(declared.as_str()) {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!("missing field `{declared}` in initializer of `{name}`"),
                    );
                }
            }
            // Build concrete type args from inferred bindings
            let type_args: Vec<Ty> = enum_type_params
                .iter()
                .map(|tp| {
                    type_arg_map
                        .get(tp)
                        .cloned()
                        .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
                })
                .collect();
            // Emit unconditionally; see the struct-init branch above for the
            // boundary-prune rationale and validator location.
            self.record_concrete_record_init_type_args(span, &type_args);
            Ty::Named {
                builtin: None,
                name: enum_name,
                args: type_args,
            }
        } else {
            let similar = crate::error::find_similar(
                name,
                self.type_defs
                    .keys()
                    .map(String::as_str)
                    .chain(self.type_aliases.keys().map(String::as_str))
                    .chain(self.known_types.iter().map(String::as_str)),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedType,
                span,
                format!("undefined type `{name}`"),
                similar,
            );
            Ty::Error
        }
    }

    pub(super) fn check_spawn(
        &mut self,
        target: &Spanned<Expr>,
        args: &[(String, Spanned<Expr>)],
        _span: &Span,
    ) -> Ty {
        let actor_name = match &target.0 {
            Expr::Identifier(name) => Some(name.clone()),
            // Handle module-qualified actor: spawn module.ActorName(args)
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    if self.modules.contains(module) {
                        self.used_modules
                            .borrow_mut()
                            .insert(ImportKey::new(self.current_module.clone(), module.clone()));
                        Some(field.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(name) = actor_name {
            for (_field_name, (arg, as_)) in args {
                let ty_raw = self.synthesize(arg, as_);
                self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
            }
            Ty::local_pid(Ty::Named {
                builtin: None,
                name,
                args: vec![],
            })
        } else {
            Ty::local_pid(Ty::Error)
        }
    }

    /// Check if an expression is typically used for side effects (not for its return value).
    pub(super) fn record_type(&mut self, span: &Span, ty: &Ty) {
        let key = SpanKey::from(span);
        self.expr_type_source_modules
            .insert(key.clone(), self.current_module.clone());
        self.expr_types.insert(key, ty.clone());
    }

    // ── Diagnostic-only stack-allocation hints (HEW-PERF-001) ─────────────
    //
    // Phase A.0 scaffold: walk every `let` / `var` binding in a function body,
    // classify the right-hand side's allocation class, and append a `StackHint`
    // for every non-`Stack` (non-`Indeterminate`) classification. This pass is
    // intentionally noisy — it emits a hint on every observed heap allocation
    // without escape filtering. False-positive suppression lands in subsequent
    // slices (A.1: return-path; A.2: capture/container; A.3: field/send).
    //
    // Conservative bias: when the RHS form is not recognised, classify as
    // `Indeterminate` (no hint emitted). False negatives are safe; false
    // positives are user-trust defects. This rule already applies in A.0
    // because some well-formed RHS expressions lack populated `expr_types`
    // (e.g. inside generic lambda bodies still being inferred).

    /// Walk a function body and emit `StackHint` entries for every binding
    /// whose RHS resolves to a heap allocation class. Called from
    /// `check_function_as` after `warn_rc_param_return`.
    pub(super) fn classify_stack_hints(&mut self, fd: &FnDecl) {
        // Ignore the function's parameter types and return type for hint
        // emission — the walker is binding-scoped, not signature-scoped.
        // Sub-body discipline (`sub-body-scoped-traversal` LESSONS row): a
        // nested function literal is reached via `Stmt::Let` of an
        // `Expr::Lambda`, which is classified as `ClosureEnv` here. The body
        // of that lambda is not re-walked — nested function decls run their
        // own `classify_stack_hints` pass via `check_function_as`.
        self.scan_block_for_stack_hints(&fd.body);
    }

    /// Recursive descent over a block, classifying every binding statement.
    fn scan_block_for_stack_hints(&mut self, block: &Block) {
        for (stmt, span) in &block.stmts {
            self.scan_stmt_for_stack_hints(stmt, span);
        }
        if let Some(trailing) = &block.trailing_expr {
            self.scan_expr_for_stack_hints(&trailing.0);
        }
    }

    fn scan_stmt_for_stack_hints(&mut self, stmt: &Stmt, stmt_span: &Span) {
        match stmt {
            Stmt::Let { pattern, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    let name = match &pattern.0 {
                        Pattern::Identifier(n) => n.clone(),
                        _ => String::new(),
                    };
                    self.maybe_record_stack_hint(stmt_span, &name, class);
                    // Descend into the RHS to classify nested bindings inside
                    // block expressions (`let x = { let y = ...; y }`).
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Var { name, value, .. } => {
                if let Some((expr, expr_span)) = value {
                    let class = self.classify_alloc(expr, expr_span);
                    self.maybe_record_stack_hint(stmt_span, name, class);
                    self.scan_expr_for_stack_hints(expr);
                }
            }
            Stmt::Assign { value, .. } => {
                self.scan_expr_for_stack_hints(&value.0);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(then_block);
                if let Some(eb) = else_block {
                    self.scan_else_block_for_stack_hints(eb);
                }
            }
            Stmt::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                self.scan_expr_for_stack_hints(&expr.0);
                self.scan_block_for_stack_hints(body);
                if let Some(b) = else_body {
                    self.scan_block_for_stack_hints(b);
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::WhileLet { expr, body, .. } => {
                self.scan_expr_for_stack_hints(&expr.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::For { iterable, body, .. } => {
                self.scan_expr_for_stack_hints(&iterable.0);
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Loop { body, .. } => {
                self.scan_block_for_stack_hints(body);
            }
            Stmt::Expression(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            Stmt::Return(opt) => {
                if let Some((e, _)) = opt {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Break { value, .. } => {
                if let Some((e, _)) = value {
                    self.scan_expr_for_stack_hints(e);
                }
            }
            Stmt::Defer(expr) => {
                self.scan_expr_for_stack_hints(&expr.0);
            }
            // Statement forms that cannot host a binding RHS: nothing to do.
            // Listed explicitly so a future Stmt variant addition forces a
            // compile error here (`exhaustive-traversal-and-lowering` LESSONS
            // row — no silent `_ => {}` in semantic positions).
            Stmt::Continue { .. } => {}
        }
    }

    fn scan_else_block_for_stack_hints(&mut self, eb: &hew_parser::ast::ElseBlock) {
        if let Some(b) = &eb.block {
            self.scan_block_for_stack_hints(b);
        }
        if let Some(if_stmt) = &eb.if_stmt {
            let (stmt, span) = if_stmt.as_ref();
            self.scan_stmt_for_stack_hints(stmt, span);
        }
    }

    fn scan_match_arm_body_for_stack_hints(&mut self, arm: &MatchArm) {
        if let Some((g, _)) = &arm.guard {
            self.scan_expr_for_stack_hints(g);
        }
        self.scan_expr_for_stack_hints(&arm.body.0);
    }

    /// Descend into nested expressions to find `let`-bearing block expressions
    /// and inner lambda bodies. Phase A.0 does not classify expression-position
    /// allocations on their own (e.g. `vec.push(Vec::new())` does not emit a
    /// hint for the inner `Vec::new()` because it is unbound). Only `let` /
    /// `var` bindings produce hints in this slice.
    fn scan_expr_for_stack_hints(&mut self, expr: &Expr) {
        match expr {
            Expr::Block(block) => self.scan_block_for_stack_hints(block),
            Expr::If {
                condition,
                then_block,
                else_block,
                ..
            } => {
                self.scan_expr_for_stack_hints(&condition.0);
                self.scan_expr_for_stack_hints(&then_block.0);
                if let Some(eb) = else_block {
                    self.scan_expr_for_stack_hints(&eb.0);
                }
            }
            Expr::IfLet {
                expr,
                body,
                else_body,
                ..
            } => {
                // Mirrors the `Stmt::IfLet` arm in `scan_stmt_for_stack_hints`.
                // `body` and `else_body` are bare `Block` values (not `Spanned<Expr>`),
                // so we call `scan_block_for_stack_hints` directly.
                self.scan_expr_for_stack_hints(&expr.0);
                self.scan_block_for_stack_hints(body);
                if let Some(b) = else_body {
                    self.scan_block_for_stack_hints(b);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.scan_expr_for_stack_hints(&scrutinee.0);
                for arm in arms {
                    self.scan_match_arm_body_for_stack_hints(arm);
                }
            }
            // All remaining expression forms — including `Expr::Lambda`
            // (whose body is *not* re-walked here: nested fn / lambda decls
            // run their own walker pass, and the lambda value itself when
            // assigned is classified at the binding site as `ClosureEnv`,
            // so walking the lambda body would double-emit hints) — cannot
            // host a `let` statement directly. Anything reachable through
            // call args, indices, struct fields, or tuple elements is
            // wrapped in an `Expr::Block` when it contains statements,
            // covered by the `Expr::Block` arm above.
            _ => {}
        }
    }

    /// Classify a binding's RHS expression by looking up its synthesised type
    /// in `expr_types`. Phase A.0 recognises the named heap types
    /// (`Vec`, `String`, `HashMap`, `HashSet`, `Rc`) plus closure literals
    /// (`Expr::Lambda`). Everything else maps to `Stack` (already
    /// stack-shaped) or `Indeterminate` (unknown form, no hint).
    fn classify_alloc(&self, expr: &Expr, span: &Span) -> AllocationClass {
        // Closure literals are env-heap regardless of resolved type
        // (`MLIRGenExpr.cpp:6009`).
        if matches!(expr, Expr::Lambda { .. }) {
            return AllocationClass::ClosureEnv;
        }
        let key = SpanKey::from(span);
        match self.expr_types.get(&key) {
            Some(ty) => Self::classify_ty(&self.subst.resolve(ty)),
            // Type not recorded — happens for some inferred or rewritten
            // expressions. Conservative silence per the bias policy.
            None => AllocationClass::Indeterminate,
        }
    }

    fn classify_ty(ty: &Ty) -> AllocationClass {
        match ty {
            Ty::String => AllocationClass::String,
            Ty::Named { name, .. } => match name.as_str() {
                "Vec" => AllocationClass::Vec,
                "HashMap" => AllocationClass::HashMap,
                "HashSet" => AllocationClass::HashSet,
                "Rc" => AllocationClass::Rc,
                _ => AllocationClass::Stack,
            },
            // Type variables, primitives, tuples, arrays, function types:
            // either already stack-shaped or not yet resolved. A.0 is
            // conservative.
            _ => AllocationClass::Stack,
        }
    }

    fn maybe_record_stack_hint(
        &mut self,
        stmt_span: &Span,
        binding_name: &str,
        class: AllocationClass,
    ) {
        // No hint for stack-shaped or unclassifiable RHSs.
        if matches!(
            class,
            AllocationClass::Stack | AllocationClass::Indeterminate
        ) {
            return;
        }
        self.stack_hints.push(StackHint {
            span_key: SpanKey::from(stmt_span),
            binding_name: binding_name.to_string(),
            alloc_class: class,
        });
    }

    /// Type-check `lhs is rhs` (identity comparison, slice D-2).
    ///
    /// See the doc comment on the `Expr::Is` arm in [`Self::synthesize_inner`]
    /// for the allowance set, rejection rules, and cross-class behaviour.
    ///
    /// Always returns `Ty::Bool` (even after reporting errors); the operator
    /// is total at the type level so downstream uses (`if (a is b) { ... }`)
    /// don't double-poison.
    fn synthesize_is(&mut self, lhs: &Spanned<Expr>, rhs: &Spanned<Expr>, span: &Span) -> Ty {
        let lhs_ty = self.synthesize(&lhs.0, &lhs.1);
        let rhs_ty = self.synthesize(&rhs.0, &rhs.1);
        let lhs_resolved = self.subst.resolve(&lhs_ty);
        let rhs_resolved = self.subst.resolve(&rhs_ty);

        // Don't double-report when either side is already poisoned by an
        // upstream diagnostic (`Ty::Error`) or still under inference
        // (`Ty::Var`). The operator still produces `bool` so enclosing
        // expressions see a stable type.
        if matches!(lhs_resolved, Ty::Error | Ty::Var(_))
            || matches!(rhs_resolved, Ty::Error | Ty::Var(_))
        {
            return Ty::Bool;
        }

        let lhs_ok = self.is_identity_capable(&lhs_resolved);
        let rhs_ok = self.is_identity_capable(&rhs_resolved);

        if !lhs_ok {
            self.report_is_value_type(&lhs.1, &lhs_resolved);
        }
        if !rhs_ok {
            self.report_is_value_type(&rhs.1, &rhs_resolved);
        }

        // Cross-class / cross-instantiation mismatch (e.g. `Vec<int> is Vec<String>`
        // or `ActorRef<Foo> is Vec<int>`) — only reported when both sides are
        // independently identity-capable; otherwise the value-type rejection
        // above carries the diagnostic.
        if lhs_ok && rhs_ok && lhs_resolved != rhs_resolved {
            self.report_error(
                TypeErrorKind::Mismatch {
                    expected: lhs_resolved.user_facing().to_string(),
                    actual: rhs_resolved.user_facing().to_string(),
                },
                span,
                format!(
                    "`is` operands must have the same type; found `{}` and `{}`",
                    lhs_resolved.user_facing(),
                    rhs_resolved.user_facing()
                ),
            );
        }

        Ty::Bool
    }

    /// Report `E_IS_VALUE_TYPE` for a value-type operand of `is`.
    fn report_is_value_type(&mut self, span: &Span, ty: &Ty) {
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`is` requires an identity-bearing operand; `{}` is a value type \
                 (E_IS_VALUE_TYPE) — use `==` for value comparison; `is` checks \
                 identity for heap-backed types only",
                ty.user_facing()
            ),
        );
    }

    /// Classify a resolved type as identity-bearing per plan §D-D2.
    ///
    /// Returns `true` when `is` is valid on values of this type:
    ///
    /// * Machines (`TypeDefKind::Machine`).
    /// * Actors and actor handles: `TypeDefKind::Actor` named types,
    ///   `ActorRef<T>`, and `Actor<T>`.
    /// * Heap-backed collections: `Vec<T>`, `HashMap<K,V>`, `HashSet<T>`.
    /// * `bytes`.
    /// * User `type Foo { ... }` declarations (`TypeDefKind::Struct` and
    ///   `TypeDefKind::Enum`), which are Rc-backed in the runtime.
    ///
    /// Returns `false` for value types: scalars, `String`, `record` types,
    /// tuples, arrays, slices, ranges, durations, functions, closures, and
    /// trait objects. Caller is responsible for handling `Ty::Var` / `Ty::Error`
    /// before invoking this predicate.
    fn is_identity_capable(&self, ty: &Ty) -> bool {
        match ty {
            // Heap-backed builtin handles.
            Ty::Bytes => true,

            // Named types: actor handles, collection builtins, and any user
            // `TypeDef` whose kind carries heap/reference identity.
            Ty::Named { name, builtin, .. } => {
                // Actor handles (`ActorRef<T>` / `Actor<T>`).
                if ty.as_actor_handle().is_some() {
                    return true;
                }
                // Heap-backed builtin collections — kept name-keyed since they
                // have no `TypeDef` entry.
                if builtin.is_some_and(BuiltinType::is_collection) {
                    return true;
                }
                // User type declarations: machines, actors, and user
                // `type Foo { ... }` (Struct/Enum, Rc-backed). `Record` is
                // explicitly rejected as a value type.
                if let Some(td) = self.type_defs.get(name) {
                    return matches!(
                        td.kind,
                        TypeDefKind::Machine
                            | TypeDefKind::Actor
                            | TypeDefKind::Struct
                            | TypeDefKind::Enum
                    );
                }
                false
            }

            // Everything else is a value type for `is` purposes: scalars,
            // `String`, tuples, arrays, slices, function/closure types,
            // pointers, trait objects, durations, unit, never, tasks,
            // type vars (handled by caller), and the error sentinel.
            _ => false,
        }
    }

    /// Return `true` if `ty` is a v0.5 substrate handle type (affine — consumed
    /// by exactly one method call). These are `Duplex<S,R>`, `Sink<T>`,
    /// `Stream<T>`, `SendHalf<S>`, and `RecvHalf<R>`.
    ///
    /// Used by [`synthesize_identifier`](Self::synthesize_identifier) to add a
    /// targeted suggestion when a `UseAfterMove` fires on a substrate binding.
    fn ty_is_substrate_handle(ty: &Ty) -> bool {
        matches!(ty, Ty::Named { builtin: Some(builtin), .. } if builtin.is_substrate_handle())
    }
}
