use super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::eq_eligibility::{ty_is_eq_eligible_with_type_params, EqEligibility};
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
        result
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

    /// Verify that `ty` has a `Display` impl reachable by f-string
    /// interpolation lowering. The Display trait is identified through the
    /// lang-item registry (`LANG_ITEM_DISPLAY` key) so stdlib can rename or
    /// move the trait by relocating its `#[lang_item("display")]` attribute
    /// without code changes here. Mirrors the lookup that
    /// `lower_display_dispatch` performs in HIR: primitives consult
    /// `primitive_trait_impls`, user-named types consult `trait_impls_set`,
    /// and `string` is accepted as a passthrough. Emits a clear
    /// `BoundsNotSatisfied` diagnostic on miss so the negative gate is
    /// raised at check time rather than as an opaque HIR/MIR error.
    pub(super) fn require_display_impl(&mut self, ty: &Ty, span: &Span) {
        let resolved = self.subst.resolve(ty).materialize_literal_defaults();
        if matches!(resolved, Ty::String) {
            return;
        }
        if matches!(resolved, Ty::Var(_) | Ty::Error) {
            return;
        }
        // Resolve the Display trait name through the lang-item registry.
        // No `#[lang_item("display")]` in scope means the program defines no
        // Display trait at all — in which case f-string interpolation can
        // only accept the trivially-string / inference-pending cases handled
        // above. Falling back to the literal name `"Display"` keeps
        // pre-lang-item check-time tests (no stdlib loaded) working with the
        // implicit naming convention.
        let display_trait = self
            .lang_items
            .display_trait()
            .map_or_else(|| "Display".to_string(), str::to_owned);
        if let Some(canonical) = resolved.canonical_lowering_name() {
            if self
                .primitive_trait_impls
                .contains_key(&(canonical.to_string(), display_trait.clone()))
            {
                return;
            }
        }
        if let Ty::Named { name, args, .. } = &resolved {
            if self
                .trait_impls_set
                .contains(&(name.clone(), display_trait.clone()))
            {
                return;
            }
            // A bare type parameter (e.g. `T` in `fn f<T: Display>(x: T)`)
            // carries no registered impl of its own, but the enclosing
            // item's where-clause may declare a `Display` bound that
            // satisfies the obligation abstractly. The concrete `Display`
            // impl is selected per monomorphisation by HIR's static
            // trait-dispatch lowering. Mirrors `type_satisfies_trait_bound`.
            if args.is_empty() && self.type_param_carries_bound(name, &display_trait) {
                return;
            }
        }
        let ty_str = format!("{}", resolved.user_facing());
        self.report_error(
            TypeErrorKind::BoundsNotSatisfied,
            span,
            format!(
                "type `{ty_str}` does not implement `{display_trait}` \
                 (f-string interpolation requires `impl {display_trait} for {ty_str}`)"
            ),
        );
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
                        let part_ty = self.synthesize(expr, expr_span);
                        self.require_display_impl(&part_ty, expr_span);
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
            Expr::Identifier(name) if name == "None" => Ty::option(Ty::Var(TypeVar::fresh())),
            Expr::Identifier(name) => self.synthesize_identifier(name, span),

            // Binary ops
            Expr::Binary { left, op, right } => self.check_binary_op(left, *op, right),

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
            Expr::Array(elems) => self.synthesize_array_literal(elems, span),
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
            Expr::Spawn {
                target,
                type_args,
                args,
            } => self.check_spawn(target, type_args, args, span),

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
                false,
            ),

            // Await
            Expr::Await(inner) => {
                // Signal to check_named_method_fallback that an actor-ask method
                // call under `await` is valid.  Save and restore so nested awaits
                // (e.g. `await { await expr }`) do not clobber the outer flag.
                let prev_inside_await = self.inside_await_expr;
                self.inside_await_expr = true;
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                self.inside_await_expr = prev_inside_await;

                // When the user writes `await { method_call }` the inner expression
                // is an `Expr::Block` wrapping a single trailing method call, not a
                // bare `Expr::MethodCall`.  Unwrap one level of block so the ask-
                // dispatch guard and span-key lookup below can find the right node.
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

                // await Task<T> → T (simplified)
                match inner_ty {
                    Ty::Task(inner) => *inner,
                    // `await close(actor)` or bare actor ref → Unit (actor termination).
                    // But NOT for method calls that happen to return an ActorRef —
                    // those should pass through the method's declared return type.
                    _ if inner_ty.as_actor_handle().is_some()
                        && !matches!(effective_expr, Expr::MethodCall { .. }) =>
                    {
                        Ty::Unit
                    }
                    // Named-actor ask: `await ref.method(args)` (bare or block-wrapped)
                    // where the method is an ask-shaped receive fn (non-unit return).
                    // The checker recorded an `ActorMethodKind::Ask` entry for the
                    // inner method-call span; unify with the lambda/remote paths by
                    // returning `Result<R, AskError>`.
                    _ if matches!(effective_expr, Expr::MethodCall { .. }) => {
                        let dispatch_key =
                            SpanKey::in_module(effective_span, self.current_module_idx);
                        if let Some(ActorMethodKind::Ask(_, reply_ty)) =
                            self.actor_method_dispatch.get(&dispatch_key).cloned()
                        {
                            Ty::result(reply_ty, Ty::ask_error())
                        } else {
                            inner_ty
                        }
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
                let current_return_type = self.current_return_type.clone();
                let bad_ctx_msg: Option<String> = current_return_type.as_ref().and_then(|ret| {
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
                                    if unify(&mut self.subst, &ret_err, &err_ty).is_err() {
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

    fn expect_concrete_integer_operands(
        &mut self,
        common_ty: &Ty,
        left: &Spanned<Expr>,
        left_ty: &Ty,
        right: &Spanned<Expr>,
        right_ty: &Ty,
    ) {
        if integer_type_info(common_ty, self.pointer_width()).is_some() {
            self.expect_type(common_ty, left_ty, &left.1);
            self.expect_type(common_ty, right_ty, &right.1);
            self.record_concrete_integer_operand(common_ty, left, left_ty);
            self.record_concrete_integer_operand(common_ty, right, right_ty);
        }
    }

    fn record_concrete_integer_operand(
        &mut self,
        common_ty: &Ty,
        operand: &Spanned<Expr>,
        operand_ty: &Ty,
    ) {
        let resolved = self.subst.resolve(operand_ty);
        if resolved.is_integer_literal() || matches!(resolved, Ty::Var(_)) {
            self.check_against(&operand.0, &operand.1, common_ty);
        }
    }

    fn concrete_integer_float_mismatch(left: &Ty, right: &Ty, ptr_width: u8) -> bool {
        (integer_type_info(left, ptr_width).is_some()
            && right.is_float()
            && !right.is_float_literal())
            || (integer_type_info(right, ptr_width).is_some()
                && left.is_float()
                && !left.is_float_literal())
    }

    fn expect_inferable_literal_binding(&mut self, name: &str, expected: &Ty, span: &Span) {
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        if binding.def_span.is_none() {
            return;
        }
        let actual = binding.ty.clone();
        self.expect_type(expected, &actual, span);
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
        // Owned (non-Copy) elements are cloned per slot at runtime (N independent
        // owned copies, source dropped once at block exit — matching `vec![x; n]`
        // semantics). Gate on clone admissibility: `string` and `bytes` always
        // admit (push_str / push_bytes copies independently); record/enum/Vec
        // admit when the owned-element thunk path exists. Anything else fails
        // closed with a clear diagnostic rather than a silent double-free.
        if !self.vec_element_has_copy_layout(&elem_ty) {
            let resolved_elem = self.subst.resolve(&elem_ty);
            let clonable = matches!(resolved_elem, Ty::String | Ty::Bytes)
                || self.vec_owned_element_admissible(&elem_ty);
            if !clonable {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`[{elem}; N]` array repeat requires the element type to be Clone, \
                         but `{elem}` has no clone path (not a string, bytes, record, enum, \
                         or Vec/HashMap/HashSet element with a thunk); \
                         use an explicit loop with `.clone()` or a Copy element type",
                        elem = resolved_elem.user_facing()
                    ),
                );
            }
        }
        self.make_vec_type(elem_ty, span)
    }

    pub(super) fn synthesize_array_literal(&mut self, elems: &[Spanned<Expr>], span: &Span) -> Ty {
        let elem_ty = if elems.is_empty() {
            Ty::Var(TypeVar::fresh())
        } else {
            let first_ty = self.synthesize(&elems[0].0, &elems[0].1);
            for elem in &elems[1..] {
                self.check_against(&elem.0, &elem.1, &first_ty);
            }
            first_ty
        };
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
        // Record the pattern resolution so HIR lowering can consume
        // the same `pattern_resolutions` side-table that powers
        // `WhileLet` and `Match` lowering.
        self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
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
        self.actor_send_aliasing.insert(
            SpanKey::in_module(move_span, self.current_module_idx),
            decision,
        );
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
            // codegen deep-copy fires and the receiver
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

    #[allow(
        clippy::too_many_lines,
        reason = "single dispatch over all identifier forms (context readers, module-qualified variants, bindings, fn sigs, constructors, type aliases); splitting would fragment shared error-reporting state"
    )]
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
        if let Some(dot_pos) = name.find('.') {
            let candidate_module = &name[..dot_pos];
            let rest = &name[dot_pos + 1..];
            if let Some(colon_pos) = rest.find("::") {
                let type_name = &rest[..colon_pos];
                let variant_name = &rest[colon_pos + 2..];
                let is_binding = self.env.lookup_ref(candidate_module).is_some();
                let is_known_type = self.type_defs.contains_key(candidate_module);
                let qualified_key = format!("{candidate_module}.{type_name}");
                let qualified_in_type_defs = self.type_defs.contains_key(&qualified_key);
                if !is_binding && !is_known_type && !qualified_in_type_defs {
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
            // Track captures: variable from scope below the lambda boundary
            if let Some(capture_depth) = self.lambda_capture_depth {
                if depth < capture_depth {
                    self.lambda_captures.push(ty.clone());
                    self.lambda_capture_facts.push(ClosureCaptureFact {
                        binding_id,
                        name: name.to_string(),
                        ty: ty.clone(),
                        mode: ClosureCaptureMode::Copy,
                        mode_origin: CaptureModeOrigin::ImplicitCopy,
                        is_send: false,
                        is_sync: false,
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
            // local-shadows-global: when the fn_sig slot was won by a builtin enum
            // variant, prefer any user-declared enum that has a variant with the
            // same name (e.g. user `enum AppError { NotFound(string); }` shadows
            // the builtin `LookupError::NotFound` unit variant).
            if sig.is_builtin_variant {
                if let Some(user_ty) = self.find_user_variant_shadow_ty(name) {
                    return user_ty;
                }
            }
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

    /// Look up whether any user-declared enum type (in `local_type_defs` or
    /// `source_type_defs`) has a variant named `variant_name`.  Returns the
    /// resolved `Ty` for that variant if found (unit variant → the enum type;
    /// tuple variant → a `Ty::Function` constructing that enum), or `None` if
    /// no user type shadows the given bare name.
    ///
    /// Used by `check_identifier` to implement the local-shadows-global rule:
    /// when `fn_sigs[variant_name].is_builtin_variant` is `true`, the builtin
    /// won the bare-name slot but a user-declared variant with the same name
    /// should take priority within this compilation unit.
    fn find_user_variant_shadow_ty(&self, variant_name: &str) -> Option<Ty> {
        // Iterate over local types (user-declared in root or imported source modules).
        // Two-set iteration: local first, then source.  In practice most programs
        // won't have shadowing at all, so this is a short-circuit path.
        for type_name in self
            .local_type_defs
            .iter()
            .chain(self.source_type_defs.iter())
        {
            let Some(td) = self.type_defs.get(type_name.as_str()) else {
                continue;
            };
            if td.kind != TypeDefKind::Enum {
                continue;
            }
            let Some(variant) = td.variants.get(variant_name) else {
                continue;
            };
            // Build fresh inference variables for any type parameters on the
            // user enum (e.g. `enum Outcome<T> { Found(T); NotFound; }`).
            let type_args: Vec<Ty> = td
                .type_params
                .iter()
                .map(|_| Ty::Var(TypeVar::fresh()))
                .collect();
            let return_type = Ty::normalize_named(type_name.clone(), type_args.clone());
            return Some(match variant {
                VariantDef::Tuple(payload_tys) => {
                    // Substitute generic type params with their corresponding
                    // fresh inference variables in each payload type.
                    let params: Vec<Ty> = payload_tys
                        .iter()
                        .map(|ty| {
                            td.type_params.iter().zip(&type_args).fold(
                                ty.clone(),
                                |acc, (tp_name, fresh_var)| {
                                    acc.substitute_named_param(tp_name, fresh_var)
                                },
                            )
                        })
                        .collect();
                    Ty::Function {
                        params,
                        ret: Box::new(return_type),
                    }
                }
                // Unit variants are values (no call needed); struct variants
                // are constructed via `Expr::StructInit`, not
                // `Expr::Identifier` — this path is not reached for them.
                VariantDef::Unit | VariantDef::Struct(_) => return_type,
            });
        }
        None
    }

    /// Look up an identifier as a unit enum variant or qualified variant name.
    #[allow(
        clippy::too_many_lines,
        reason = "multi-branch variant resolution: unqualified, qualified-in-type_defs, and qualified-in-fn_sigs each need distinct handling"
    )]
    pub(super) fn resolve_identifier_variant(&mut self, name: &str, span: &Span) -> Ty {
        // Two-pass scan: user-declared (local/source) types win over builtin/
        // imported types when both declare a unit variant with the same bare name
        // (local-shadows-global rule).  Pass 1 considers only types recorded in
        // `local_type_defs` or `source_type_defs`; pass 2 considers the rest.
        let mut found = None;
        // Pass 1: user-declared types.
        for (type_name, td) in &self.type_defs {
            if !self.local_type_defs.contains(type_name.as_str())
                && !self.source_type_defs.contains(type_name.as_str())
            {
                continue;
            }
            if let Some(variant) = td.variants.get(name) {
                if matches!(variant, VariantDef::Unit) {
                    let ty = Ty::normalize_named(type_name.clone(), vec![]);
                    found = Some(ty);
                    break;
                }
            }
        }
        // Pass 2: builtin/imported types (only when no user type matched).
        if found.is_none() {
            for (type_name, td) in &self.type_defs {
                if self.local_type_defs.contains(type_name.as_str())
                    || self.source_type_defs.contains(type_name.as_str())
                {
                    continue; // already scanned in pass 1
                }
                if let Some(variant) = td.variants.get(name) {
                    if matches!(variant, VariantDef::Unit) {
                        let ty = Ty::normalize_named(type_name.clone(), vec![]);
                        found = Some(ty);
                        break;
                    }
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
                                    .is_some_and(|n| Ty::names_match_qualified(n, type_prefix));
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
                    builtin: Some(BuiltinType::Vec),
                    args,
                    ..
                } if !args.is_empty() => obj_ty.clone(),
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
                    self.record_dyn_index_method_call(bound, span);
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
            // MIR `lower_vec_index` inserts a `NumericCast` for the `xs[i]`
            // path so the bounds-check `IntCmp` sees matching i64 operands.
            Ty::Named {
                builtin: Some(BuiltinType::Vec),
                args,
                ..
            } if !args.is_empty() => {
                let idx_actual = self.synthesize(&index.0, &index.1);
                let idx_resolved = self.subst.resolve(&idx_actual);
                if !Self::is_narrower_signed_int(&idx_resolved) {
                    self.check_against(&index.0, &index.1, &Ty::I64);
                }
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
                builtin: Some(BuiltinType::HashMap),
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
                self.check_against(&index.0, &index.1, &Ty::I64);
                Ty::U8
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

    fn record_dyn_index_method_call(&mut self, bound: &crate::ty::TraitObjectBound, span: &Span) {
        let trait_name = bound.trait_name.as_str();
        let Some(trait_info) = self.trait_defs.get(trait_name).cloned() else {
            return;
        };
        let Some(method_idx) = trait_info
            .methods
            .iter()
            .position(|method| method.name == "at")
        else {
            return;
        };
        // Compute the substituted `at` signature for the originating
        // bound (the bound's assoc bindings carry e.g. `Output = T`).
        // W3.031 Stage 1.6: the typed `FnSig` is self-contained on
        // the call-site side table; no codegen-time re-derivation.
        let Some(mut sig) = self.lookup_trait_method(trait_name, "at") else {
            return;
        };
        self.apply_trait_object_bound_substitutions(&mut sig, bound);
        let slot = 3 + u32::try_from(method_idx).unwrap_or(u32::MAX);
        self.dyn_trait_method_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            crate::check::types::DynMethodCall {
                trait_name: trait_name.to_string(),
                method_name: "at".to_string(),
                slot,
                signature: sig,
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
        reason = "concurrency variants (scope/select/join/spawn/unsafe/timeout)"
    )]
    pub(super) fn synthesize_concurrency(&mut self, expr: &Expr, span: &Span) -> Ty {
        match expr {
            Expr::ForkChild {
                binding,
                expr: child,
            } => {
                // `fork name = call(...)` is a child-task spawn statement and is
                // only meaningful inside a `scope { }` body (TI-2). HIR lowering
                // enforces the same position rule; rejecting here as well gives a
                // check-time diagnostic instead of a lowering error.
                if self.task_scope_depth == 0 {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`fork` is only valid inside a `scope { }` body".to_string(),
                    );
                    return Ty::Error;
                }
                // Parity with HIR's ForkChildNotACall gate: the child must be a
                // call expression — task spawning needs a callee to run, not a
                // value to wrap.
                if !matches!(child.0, Expr::Call { .. }) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &child.1,
                        "`fork name = expr` requires a call expression as the \
                         right-hand side; other expression forms cannot be \
                         spawned as tasks"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                let ret_ty = self.synthesize(&child.0, &child.1);
                // Mark non-Copy call arguments as moved into the fork child.
                //
                // `fork name = f(a, b)` transfers ownership of non-Copy args
                // into the child task env. The general `Expr::Call` arm of
                // `synthesize` does NOT mark args as moved (regular function
                // calls are by-value but the type checker treats them as
                // borrows), so without this pass a `string` arg remains live
                // in the parent scope and a subsequent use goes unreported.
                //
                // We read each arg's type via `lookup_ref` (no read-count
                // increment — `synthesize` already counted the read) and
                // delegate to `mark_expr_moved_if_non_copy`, which gates on
                // `!Copy` and only marks `Expr::Identifier` nodes.
                //
                // WHEN-OBSOLETE: if the general call-arg path gains its own
                // move-tracking, this pass becomes redundant and can be
                // deleted. Until then this is the sole source of
                // UseAfterMove diagnostics for named fork spawn args.
                if let Expr::Call { args, .. } = &child.0 {
                    for arg in args {
                        let (arg_expr, arg_span) = arg.expr();
                        if let Expr::Identifier(arg_name) = arg_expr {
                            if let Some(binding_ty) =
                                self.env.lookup_ref(arg_name).map(|b| b.ty.clone())
                            {
                                let resolved = self.subst.resolve(&binding_ty);
                                self.mark_expr_moved_if_non_copy(arg_expr, arg_span, &resolved);
                            }
                        }
                    }
                }
                if let Some(name) = binding {
                    // Mirror `let`: introduce the binding into the enclosing
                    // block scope so later statements (`await t`) resolve it.
                    // The handle types as Task<T> where T is the callee's
                    // return type; `await` typing unwraps it (await Task<T> → T).
                    self.env.define_with_span(
                        name.clone(),
                        Ty::Task(Box::new(ret_ty)),
                        false,
                        span.clone(),
                    );
                }
                // The fork statement itself produces no value.
                Ty::Unit
            }
            Expr::ForkBlock { body } => {
                // `fork { ... }` is a fire-and-forget anonymous child task,
                // `≈ fork _ = (|| { body })()`. Its body is checked here as a
                // zero-parameter, unit-returning closure context so that body
                // statements, callee arguments, and the tail expression are all
                // type-checked — the same coverage the named `fork name = call()`
                // form already enjoys.
                //
                // Position gate (defence-in-depth): like `fork name = call(...)`,
                // `fork { ... }` is only meaningful inside a `scope { }` body. The
                // parser already rejects `fork { }` outside a scope, so this gate is
                // unreachable via a clean parse; it mirrors the `ForkChild` arm so
                // the checker never silently accepts an out-of-position fork.
                if self.task_scope_depth == 0 {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "`fork { ... }` is only valid inside a `scope { }` body".to_string(),
                    );
                    return Ty::Unit;
                }
                // Shape gate: a fork body that is a single bare non-call tail
                // expression (no stmts, trailing_expr present but not a Call)
                // must be caught here with the actionable fork-shape message.
                // Without this gate the body reaches `check_block` against
                // `Some(&Ty::Unit)`, which emits a generic "type mismatch:
                // expected `()`, found `<T>`" — unhelpful for the user.
                //
                // `fork { 42; }` (with semicolon) becomes a single stmt and
                // reaches the HIR gate's own ForkBlockBodyUnsupported path,
                // so we only need to handle the no-semicolon tail case here.
                if body.stmts.is_empty() {
                    if let Some(tail) = &body.trailing_expr {
                        if !matches!(&tail.0, Expr::Call { .. }) {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                "`fork { }` bodies must be a direct function call, \
                                 e.g. `fork { work() }`; other expression forms are \
                                 not yet supported"
                                    .to_string(),
                            );
                            return Ty::Unit;
                        }
                    }
                }
                // Check the body as an anonymous, unit-returning task context.
                // `task_scope_depth` stays elevated (the body runs inside the
                // enclosing scope, it does not open a fresh scope boundary), so a
                // nested `fork name = call()` inside the body still passes its
                // position gate. Save/restore `current_return_type` exactly as the
                // `Expr::GenBlock` arm does so the enclosing function's return
                // type is not polluted.
                let prev_return_type = self.current_return_type.take();
                self.current_return_type = Some(Ty::Unit);
                self.check_block(body, Some(&Ty::Unit));
                self.current_return_type = prev_return_type;
                // Mirror the `ForkChild` ownership gate: mark non-Copy call
                // arguments as moved into this child task so that parent use
                // after the fork reports `UseAfterMove`.
                //
                // `fork { f(a) }` transfers ownership of non-Copy args to the
                // child task, exactly as `fork name = f(a)` does. The general
                // `Expr::Call` arm does NOT mark args moved, so without this
                // pass a heap `string` arg remains live in the parent scope
                // and a subsequent use goes unreported — contradicting the
                // lowering's "parent emits NO drop for moved-in args" contract.
                //
                // The accepted single-call form appears in two shapes:
                //   a) `fork { f(args) }`  — tail expression (no semicolon)
                //   b) `fork { f(args); }` — single stmt with semicolon
                // For multi-statement bodies the HIR lowering rejects the fork,
                // so we only encounter those shapes during type-checking; we
                // still mark them (each call transfers its non-Copy args) to
                // keep the ownership contract uniform even when HIR will err.
                //
                // WHEN-OBSOLETE: if the general call-arg path gains move-
                // tracking, this pass (and the ForkChild one) become redundant.
                macro_rules! mark_call_args_moved {
                    ($args:expr) => {
                        for arg in $args {
                            let (arg_expr, arg_span) = arg.expr();
                            if let Expr::Identifier(arg_name) = arg_expr {
                                if let Some(binding_ty) =
                                    self.env.lookup_ref(arg_name).map(|b| b.ty.clone())
                                {
                                    let resolved = self.subst.resolve(&binding_ty);
                                    self.mark_expr_moved_if_non_copy(arg_expr, arg_span, &resolved);
                                }
                            }
                        }
                    };
                }
                if let Some(tail) = &body.trailing_expr {
                    if let (Expr::Call { args, .. }, _) = tail.as_ref() {
                        mark_call_args_moved!(args);
                    }
                } else if body.stmts.len() == 1 {
                    if let (Stmt::Expression((Expr::Call { args, .. }, _)), _) = &body.stmts[0] {
                        mark_call_args_moved!(args);
                    }
                }
                // The fork statement itself produces no value.
                Ty::Unit
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
                //   - E_LAMBDA_SELF_ESCAPE: body returns an actor handle (leaks the actor).
                // Bidirectional hint for the body is intentionally omitted here (slight
                // inference degradation for actor bodies) to keep diagnostics clean.
                // WHEN-OBSOLETE: if a richer bidirectional inference mode is added that
                // can propagate a "return type hint" without actually checking the body
                // against it, restore the hint while keeping targeted diagnostics.
                //
                // Pass is_actor_body=true so check_call inside the body can permit
                // recursive self-sends (a Duplex capture called from within its own
                // actor body). Nested fn-closures inside the body pass is_actor_body=false,
                // so they correctly see in_lambda_actor_body=false.
                let lambda_ty =
                    self.check_lambda(*is_move, None, params, None, body, None, span, true);
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
                // E_LAMBDA_SELF_ESCAPE: the lambda body returns an actor handle.
                // A lambda body that produces a `LambdaPid<...>` (lambda-actor handle)
                // or a raw `Duplex<...>` channel is leaking a move-only handle outside
                // the actor boundary — the handle's lifetime is bound to the let-binding
                // site, not to values the body produces.
                //
                // CONSERVATIVE APPROXIMATION (slice 2): any handle-typed body is rejected,
                // including the "factory" pattern (actor body returns a *different* actor's
                // handle). Slice 3 can narrow this to only reject handle values that alias
                // a capture from the enclosing let-binding, using MIR-level alias analysis.
                // Until then, returning any actor handle from an actor body is forbidden.
                //
                // WHEN-OBSOLETE: slice 3 adds MIR-level self-ref weak capture that covers
                // the runtime dimension of self-escape; this is the static type-level gate.
                if body_ret.as_lambda_pid().is_some() || body_ret.as_duplex().is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "actor lambda body returns an actor handle — actor handles cannot \
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
                //     → `LambdaPid<Msg, ()>` — call-site returns `Result<(), SendError>`
                //   ask-shaped (`actor |p| -> Reply { ... }`)
                //     → `LambdaPid<Msg, Reply>` — call-site returns `Result<Reply, AskError>`
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
                Ty::lambda_pid(msg_ty, reply_ty)
            }
            Expr::Scope { body: block } => {
                // Type-check the block body for diagnostics; the scope itself is Unit
                // (it is a lifetime boundary, not a value-producing block).
                // Track the nesting depth so `fork name = call(...)` statements
                // can verify they appear inside a scope body.
                self.task_scope_depth += 1;
                self.check_block(block, None);
                self.task_scope_depth -= 1;
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
                self.check_against(&duration.0, &duration.1, &Ty::Duration);
                let inner_ty = self.synthesize(&inner.0, &inner.1);
                // NEW-6: supported await deadlines resolve to the same Result shape
                // their resume edge binds. Actor asks use `Result<R, AskError>`;
                // raw connection reads use `Result<bytes, IoError>`. Other
                // `| after d` forms are deferred and fail closed in HIR lowering;
                // type them as the inner expression's type so the checker emits no
                // spurious diagnostics here (the precise deferred diagnostic is
                // raised during lowering).
                if let Expr::Await(await_inner) = &inner.0 {
                    let inner_key = SpanKey::in_module(&await_inner.1, self.current_module_idx);
                    if let Some(ActorMethodKind::Ask(_, reply_ty)) =
                        self.actor_method_dispatch.get(&inner_key).cloned()
                    {
                        Ty::result(reply_ty, Ty::ask_error())
                    } else if let Some(&to_string) = self.conn_await_reads.get(&inner_key) {
                        // `await conn.read() | after d` → `Result<bytes, NetError>`
                        // `await conn.read_string() | after d` → `Result<string, NetError>`
                        let ok_ty = if to_string { Ty::String } else { Ty::Bytes };
                        Ty::result(
                            ok_ty,
                            Ty::Named {
                                name: "NetError".to_string(),
                                args: Vec::new(),
                                builtin: None,
                            },
                        )
                    } else if self.listener_await_accepts.contains(&inner_key) {
                        // `await ln.accept() | after d` → `Result<Connection, NetError>`
                        // The Ok type mirrors the plain await's inner type (Connection).
                        Ty::result(
                            inner_ty,
                            Ty::Named {
                                name: "NetError".to_string(),
                                args: Vec::new(),
                                builtin: None,
                            },
                        )
                    } else if matches!(
                        self.method_call_rewrites.get(&inner_key),
                        Some(MethodCallRewrite::RewriteToFunction { c_symbol, .. })
                            if c_symbol == "hew_channel_recv_layout"
                    ) {
                        // `await rx.recv() | after d` → `Result<Option<T>, TimeoutError>`
                        // `inner_ty` is `Option<T>` from the plain `await rx.recv()` path.
                        Ty::result(inner_ty, Ty::timeout_error())
                    } else if matches!(
                        self.method_call_rewrites.get(&inner_key),
                        Some(MethodCallRewrite::RewriteToFunction { c_symbol, .. })
                            if c_symbol == "hew_stream_next_layout"
                    ) {
                        // `await stream.recv() | after d` → `Result<Option<T>, TimeoutError>`
                        // `inner_ty` is `Option<T>` from the plain `await stream.recv()` path.
                        Ty::result(inner_ty, Ty::timeout_error())
                    } else {
                        inner_ty
                    }
                } else {
                    inner_ty
                }
            }
            Expr::GenBlock { body } => {
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
        // Capture whether THIS expression is a function-return tail (armed by
        // `check_fn_decl` and threaded through `check_block`), then disarm so
        // the recursive operand/field/condition checks below never inherit it —
        // only a genuine tail expression may Ok-coerce. The `Expr::If` and
        // `Expr::Match` arms below re-arm explicitly for their branch bodies
        // (which are themselves tail-flowing), and the default arm consults
        // `tail_ok_armed` to perform the actual coercion.
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
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
                    false,
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
                // Both branch bodies of a tail `if` flow to the function return,
                // so they inherit this expression's armed state; the condition
                // (checked above against `Bool`) does not.
                self.tail_ok_armed = tail_ok_armed;
                let then_ty = self.check_expr_with_expected(&then_block.0, &then_block.1, expected);
                let actual = if let Some(else_block) = else_block {
                    self.tail_ok_armed = tail_ok_armed;
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
                // A tail `match`'s arm bodies flow to the function return, so
                // re-arm before checking them; the scrutinee (synthesized above)
                // does not. `check_match_expr` threads the flag to each arm body.
                self.tail_ok_armed = tail_ok_armed;
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
                let actual = self.check_binary_op(left, *op, right);
                let actual_resolved = self.subst.resolve(&actual);
                if actual_resolved.is_integer_literal() {
                    self.check_against(&left.0, &left.1, expected);
                    self.check_against(&right.0, &right.1, expected);
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
                for (elem, expected_ty) in elems.iter().zip(expected_tys.iter()) {
                    self.check_against(&elem.0, &elem.1, expected_ty);
                }
                self.record_type(span, expected);
                expected.clone()
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
                && expected_name
                    .rsplit_once('.')
                    .is_some_and(|(_, short)| short == name.as_str())
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
                && expected_name
                    .rsplit_once('.')
                    .is_some_and(|(_, short)| short == name.as_str())
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
                                let field_expected =
                                    declared_ty.substitute_named_params_parallel(&type_arg_map);
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
                    || Ty::names_match_qualified(
                        name.split("::").next().unwrap_or(""),
                        expected_enum_name,
                    );

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
            (
                Expr::Identifier(name),
                Ty::Named {
                    name: expected_type_name,
                    args: expected_args,
                    ..
                },
            ) => {
                let is_unit_variant = self
                    .lookup_type_def(expected_type_name)
                    .and_then(|td| td.variants.get(name.as_str()).cloned())
                    .is_some_and(|v| matches!(v, VariantDef::Unit));
                if is_unit_variant {
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

            // Context-determined generic cross-module function as a value.
            //
            // When `module.fn_name` resolves to a generic function and the
            // expected type is a concrete `Ty::Function`, infer the type
            // args by unifying the freshened parameter/return types with the
            // expected shape.  Concretely-resolved type args are recorded into
            // `call_type_args` at this span so the HIR lowerer can compute the
            // mangled monomorphisation symbol and register the instantiation.
            //
            // Guard: fires ONLY when
            //   (a) the object is a bare module identifier,
            //   (b) the field names a GENERIC function in `fn_sigs`, and
            //   (c) the expected type is a `Ty::Function`.
            // Non-generic cross-module fns are handled by the synthesize path
            // (which returns the monomorphic function type directly without
            // needing this arm).  Ambiguous fn values (no expected context) fall
            // through to `synthesize` → `check_field_access` → diagnostic.
            (Expr::FieldAccess { object, field }, Ty::Function { .. })
                if if let Expr::Identifier(module_name) = &object.0 {
                    // Must be a known module (not a local binding or user type).
                    let receiver_is_binding = self.env.lookup_ref(module_name).is_some();
                    let receiver_is_known_type = self.type_defs.contains_key(module_name);
                    !receiver_is_binding
                        && !receiver_is_known_type
                        && self.modules.contains(module_name)
                        && !field.contains("::")
                        && {
                            let qk = format!("{module_name}.{field}");
                            self.fn_sigs
                                .get(&qk)
                                .is_some_and(|s| !s.type_params.is_empty())
                        }
                } else {
                    false
                } =>
            {
                // Re-extract the module name and qualified key now that the
                // guard has confirmed the shape.
                let Expr::Identifier(module_name) = &object.0 else {
                    unreachable!("guard confirmed Identifier shape")
                };
                let qualified_key = format!("{module_name}.{field}");
                // `unwrap` is safe: the guard confirmed the entry exists.
                let sig = self.fn_sigs.get(&qualified_key).unwrap().clone();

                // Freshen the sig (allocates fresh inference vars for each
                // type parameter) and unify against the expected function type.
                let (freshened_params, freshened_ret, resolved_type_args) =
                    self.instantiate_fn_sig_for_call(&sig, None, span);

                let fresh_fn_ty = Ty::Function {
                    params: freshened_params.clone(),
                    ret: Box::new(freshened_ret.clone()),
                };

                // Trial-unify: if the expected type is inconsistent (e.g. wrong
                // arity or incompatible concrete types) surface a type mismatch
                // diagnostic and return Error — never a garbage fn value.
                let n = self.errors.len();
                self.expect_type(expected, &fresh_fn_ty, span);
                if self.errors.len() > n {
                    return Ty::Error;
                }

                // Resolve after unification — type args should now be concrete.
                let concrete_args: Vec<Ty> = resolved_type_args
                    .iter()
                    .map(|ty| self.subst.resolve(ty))
                    .collect();

                if concrete_args.iter().any(Ty::has_inference_var) {
                    // Still ambiguous after unification — the expected type did
                    // not fully determine the type parameters (e.g. a partially-
                    // polymorphic context). Fail closed with a diagnostic that
                    // asks for an explicit annotation.
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "`{qualified_key}` is a generic function; the type context \
                             did not fully determine its type parameters — add an \
                             explicit type annotation (e.g. `let f: fn({params}) -> {ret} \
                             = {qualified_key}`)",
                            params = sig
                                .params
                                .iter()
                                .map(|t| format!("{t}"))
                                .collect::<Vec<_>>()
                                .join(", "),
                            ret = sig.return_type,
                        ),
                    );
                    return Ty::Error;
                }

                // Record the concrete type args so the HIR lowerer can look
                // them up by this expression's span and compute the mangled
                // monomorphisation symbol.
                self.record_concrete_call_type_args(span, &concrete_args);

                // Mark the module as used.
                self.used_modules.borrow_mut().insert(ImportKey::new(
                    self.current_module.clone(),
                    module_name.clone(),
                ));

                // Resolve the resulting function type and record it.
                let result_ty = self.subst.resolve(&fresh_fn_ty);
                self.record_type(span, &result_ty);
                result_ty
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
    fn try_tail_ok_coercion(&mut self, expected: &Ty, actual: &Ty, span: &Span) -> Option<Ty> {
        let (ok_ty, err_ty) = expected.as_result()?;
        let ok_ty = ok_ty.clone();
        let err_ty = err_ty.clone();

        // Probe 1 — does the tail already produce the FULL `Result<Ok, Err>`?
        // If so this is `fn f() -> Result<..> { g() }` where `g()` returns the
        // Result directly: no coercion, fall back to the normal path (which
        // re-unifies). Roll the probe back so it commits nothing.
        let snapshot = self.subst.snapshot();
        let full_result = Ty::result(ok_ty.clone(), err_ty.clone());
        let unifies_full = unify(&mut self.subst, &full_result, actual).is_ok();
        self.subst.restore(snapshot);
        if unifies_full {
            return None;
        }

        // Probe 2 — does the tail produce the `Ok` payload? If so, Ok-wrap it.
        // Commit this unification (it is the path we take) so the tail
        // expression's recorded type and any inference variables settle against
        // the `Ok` payload.
        let snapshot = self.subst.snapshot();
        if unify(&mut self.subst, &ok_ty, actual).is_ok() {
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
        if matches!(left_resolved, Ty::Error) || matches!(right_resolved, Ty::Error) {
            return Ty::Error;
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
                if left_resolved.is_duration() || right_resolved.is_duration() {
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
                            ));
                            self.deferred_range_bounds.push((
                                right.1.clone(),
                                var_tv,
                                extract_integer_literal_value(&right.0),
                                right_inner_span,
                                self.current_module_idx,
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

    fn reject_unbounded_generic_ordering(
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

    fn same_current_type_param_name(&self, left: &Ty, right: &Ty) -> Option<String> {
        let left_name = self.current_type_param_name(left)?;
        let right_name = self.current_type_param_name(right)?;
        (left_name == right_name).then_some(left_name)
    }

    fn current_type_param_name(&self, ty: &Ty) -> Option<String> {
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

    fn current_type_param_names(&self) -> HashSet<String> {
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

    /// Fail-closed gate for aggregate comparisons outside the structural-equality subset.
    ///
    /// Eligible records and payload enums may use `==`/`!=`; ordering remains
    /// rejected. The gate deliberately uses the structural equality eligibility
    /// substrate, not the broader `MarkerTrait::Eq`, because semantic Eq can
    /// admit handles that have no structural compare path. Generic type
    /// parameters are admitted only inside a generic frame: MIR/codegen must
    /// resolve the concrete monomorphized leaves and fail closed for any leaf
    /// that still lacks an equality path.
    #[expect(
        clippy::too_many_lines,
        reason = "aggregate comparison admission keeps record, enum, and diagnostic routing in one checker gate"
    )]
    fn reject_record_comparison(
        &mut self,
        op: BinaryOp,
        left_resolved: &Ty,
        right_resolved: &Ty,
        left_span: &Span,
        right_span: &Span,
    ) {
        enum UnsupportedComparison {
            Record {
                type_name: String,
                reason: Option<EqEligibility>,
            },
            PayloadEnum {
                type_name: String,
                reason: EqEligibility,
            },
            EnumOrdering(String),
        }

        let current_type_params = self.current_type_param_names();
        let unsupported = [left_resolved, right_resolved].into_iter().find_map(|ty| {
            let Ty::Named { name, builtin, .. } = ty else {
                return None;
            };
            let type_name = ty.user_facing().to_string();
            if matches!(builtin, Some(BuiltinType::Option | BuiltinType::Result)) {
                if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
                    let eligibility = ty_is_eq_eligible_with_type_params(
                        ty,
                        &self.type_defs,
                        &current_type_params,
                    );
                    return (eligibility != EqEligibility::Eligible).then_some(
                        UnsupportedComparison::PayloadEnum {
                            type_name,
                            reason: eligibility,
                        },
                    );
                }
                return Some(UnsupportedComparison::EnumOrdering(type_name));
            }
            let type_def = self.type_defs.get(name)?;
            match type_def.kind {
                TypeDefKind::Struct | TypeDefKind::Record => {
                    if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
                        let eligibility = ty_is_eq_eligible_with_type_params(
                            ty,
                            &self.type_defs,
                            &current_type_params,
                        );
                        return (eligibility != EqEligibility::Eligible).then_some(
                            UnsupportedComparison::Record {
                                type_name,
                                reason: Some(eligibility),
                            },
                        );
                    }
                    Some(UnsupportedComparison::Record {
                        type_name,
                        reason: None,
                    })
                }
                TypeDefKind::Enum => {
                    let has_payload_variant = type_def
                        .variants
                        .values()
                        .any(|variant| !matches!(variant, VariantDef::Unit));
                    if has_payload_variant {
                        if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
                            let eligibility = ty_is_eq_eligible_with_type_params(
                                ty,
                                &self.type_defs,
                                &current_type_params,
                            );
                            (eligibility != EqEligibility::Eligible).then_some(
                                UnsupportedComparison::PayloadEnum {
                                    type_name,
                                    reason: eligibility,
                                },
                            )
                        } else {
                            Some(UnsupportedComparison::EnumOrdering(type_name))
                        }
                    } else if matches!(op, BinaryOp::Equal | BinaryOp::NotEqual) {
                        None
                    } else {
                        Some(UnsupportedComparison::EnumOrdering(type_name))
                    }
                }
                TypeDefKind::Actor | TypeDefKind::Machine => None,
            }
        });
        let Some(unsupported) = unsupported else {
            return;
        };
        // Span the whole comparison, not just one operand.
        let span = Span {
            start: left_span.start,
            end: right_span.end,
        };
        let (message, suggestion) = match unsupported {
            UnsupportedComparison::Record { type_name, reason } => {
                if let Some(reason) = reason {
                    (
                        format!(
                            "`{op}` on record type `{type_name}` is not supported because {}",
                            Self::structural_eq_ineligibility_reason(reason)
                        ),
                        "compare individual eligible fields explicitly, or match/destructure and \
                         handle managed fields with their supported equality operations"
                            .to_string(),
                    )
                } else {
                    (
                        format!("`{op}` is not supported for record type `{type_name}`"),
                        format!("compare an individual field instead (e.g. `a.x {op} b.x`)"),
                    )
                }
            }
            UnsupportedComparison::PayloadEnum { type_name, reason } => {
                (
                    format!(
                        "`{op}` on enum `{type_name}` with payload variants is not supported because {}",
                        Self::structural_eq_ineligibility_reason(reason)
                    ),
                    "match on the enum and compare eligible payload fields in the relevant arms"
                        .to_string(),
                )
            }
            UnsupportedComparison::EnumOrdering(type_name) => (
                format!("`{op}` is not supported for enum `{type_name}`"),
                "match on the enum and compare an explicit value in each arm".to_string(),
            ),
        };
        self.report_error_with_suggestions(
            TypeErrorKind::InvalidOperation,
            &span,
            message,
            vec![suggestion],
        );
    }

    fn structural_eq_ineligibility_reason(reason: EqEligibility) -> String {
        match reason {
            EqEligibility::Eligible => {
                "structural equality eligibility was unexpectedly unresolved".to_string()
            }
            EqEligibility::IneligibleFloat(float_ty) => format!(
                "a field or payload contains floating-point data `{}`",
                float_ty.user_facing()
            ),
            EqEligibility::IneligibleManaged(managed_ty) => format!(
                "a field or payload contains layout-managed/non-Copy data `{}`",
                managed_ty.user_facing()
            ),
            EqEligibility::IneligibleOwned(owned_ty) => format!(
                "a field or payload contains owned or heap-backed data `{}`",
                owned_ty.user_facing()
            ),
            EqEligibility::IneligibleUnknown => {
                "aggregate equality eligibility is unknown".to_string()
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
                // `&T` immutable borrow (F5): a borrow parameter's owner lives in
                // the caller's scope. Returning the borrow — directly or stored
                // in a returned local/aggregate — lets the reference outlive its
                // owner (the owner is dropped at the caller's scope exit while the
                // returned borrow dangles). Reuse the same return-position escape
                // analysis as the Rc case, tagged `borrow` for a borrow-specific
                // diagnostic.
                if matches!(ty, Ty::Borrow { .. }) {
                    return Some((p.name.clone(), Some((p.name.clone(), "borrow".to_string()))));
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
                if let Some((source_param, tag)) = Self::lookup_dangerous_binding(name, scopes) {
                    self.emit_borrowed_param_return(name, &source_param, &tag, span);
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
            Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields } => {
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

    fn emit_borrowed_param_return(
        &mut self,
        name: &str,
        source_param: &str,
        tag: &str,
        span: &Span,
    ) {
        let is_borrow = tag == "borrow";
        let (message, note, suggestion) = if name == source_param {
            if is_borrow {
                (
                    format!(
                        "returning borrow parameter `{name}` lets the reference outlive \
                         its owner — the `&T` borrows a value owned by the caller, which \
                         is dropped at the caller's scope exit while the returned \
                         reference would still point at it"
                    ),
                    "an immutable borrow `&T` is non-owning; its owner lives in the \
                     caller's scope and the borrow cannot escape that scope via return"
                        .to_string(),
                    format!(
                        "return an owned value instead — `{name}.clone()` produces an \
                         owned copy the caller can keep"
                    ),
                )
            } else {
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
            }
        } else if is_borrow {
            (
                format!(
                    "returning local `{name}` which contains borrow parameter \
                     `{source_param}` — the `&T` reference would outlive its owner, \
                     which the caller drops at scope exit"
                ),
                format!(
                    "borrow parameter `{source_param}` is non-owning; storing it in \
                     `{name}` does not extend the borrowed value's lifetime"
                ),
                format!("return an owned value: clone before storing — `{source_param}.clone()`"),
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
            | Expr::Clone(_)
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
            | Expr::Return(_)
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
        if matches!(&object.0, Expr::This) && self.current_actor_type.is_some() {
            if self.current_actor_fields.iter().any(|f| f.name == field) {
                self.report_error_with_suggestions(
                    TypeErrorKind::UndefinedField,
                    span,
                    format!(
                        "`this` is the actor handle, not actor state; access actor field \
                         `{field}` as a bare name (`{field}`), not `this.{field}`"
                    ),
                    vec![field.to_string()],
                );
            } else {
                self.report_error(
                    TypeErrorKind::UndefinedField,
                    span,
                    format!(
                        "`this` is the actor handle, not actor state; actor body has no \
                         field `{field}`"
                    ),
                );
            }
            return Ty::Error;
        }

        if let Expr::Identifier(name) = &object.0 {
            if name == "self" {
                if let Some(ty) = self.check_machine_transition_self_field_access(field, span) {
                    return ty;
                }
            }
        }

        // Pre-dispatch: module-qualified value-constructor reference, e.g.
        // `m.Type::Variant` (unit or tuple-naked).  This must run BEFORE
        // `synthesize(object)` because `module` is not bound in `self.env`
        // — without the early dispatch the synthesize call would emit the
        // leaky "undefined variable `module`" diagnostic.
        //
        // Mirrors the `module_fn_exports` guard pattern at
        // `check_method_call` (methods.rs).  Gated on:
        //   - object is a bare `Expr::Identifier`
        //   - `field` contains `::` (the type-variant separator)
        //   - the identifier is neither a value binding nor a known type
        // The neither-binding-nor-type guard preserves all existing
        // field-on-value access semantics — only shapes that could only be a
        // module-qualified reference take the new path.  Nested-module paths
        // (`a.b.Type::Variant`) are out of scope for v0.5.
        if let Expr::Identifier(name) = &object.0 {
            if let Some(pos) = field.find("::") {
                let receiver_is_binding = self.env.lookup_ref(name).is_some();
                let receiver_is_known_type = self.type_defs.contains_key(name);
                if !receiver_is_binding && !receiver_is_known_type {
                    let type_name = &field[..pos];
                    let variant_name = &field[pos + 2..];
                    return self.check_module_qualified_variant_ref(
                        name,
                        type_name,
                        variant_name,
                        span,
                    );
                }
            }
        }

        // Pre-dispatch: module-qualified constant reference, e.g. `module.CONST_NAME`.
        // Must run BEFORE `synthesize(object)` for the same reason as the variant
        // arm above — the module short-name is not in env as a value binding.
        //
        // Gated on:
        //   - object is a bare `Expr::Identifier`
        //   - field does NOT contain `::` (plain const name, not a variant)
        //   - receiver is not a value binding or known type
        //   - `"{name}.{field}"` IS registered in env (i.e. exported from the module)
        // The env lookup is the authoritative guard: `register_user_module` inserts
        // `module_short.CONST_NAME` into env for every `pub const` in the imported
        // module, so any key found there is a valid exported constant.
        if let Expr::Identifier(name) = &object.0 {
            if !field.contains("::") {
                let receiver_is_binding = self.env.lookup_ref(name).is_some();
                let receiver_is_known_type = self.type_defs.contains_key(name);
                if !receiver_is_binding && !receiver_is_known_type {
                    let qualified_key = format!("{name}.{field}");
                    if let Some(binding) = self.env.lookup_ref(&qualified_key) {
                        let ty = binding.ty.clone();
                        if self.modules.contains(name) {
                            self.used_modules
                                .borrow_mut()
                                .insert(ImportKey::new(self.current_module.clone(), name.clone()));
                        }
                        return ty;
                    }
                    // If the receiver looks like a module (known to self.modules) but
                    // the const is not exported, emit a targeted diagnostic rather than
                    // falling through to the generic "undefined variable `module`" error.
                    if self.modules.contains(name) {
                        // The module DOES export a function under this name:
                        // a non-generic cross-module function in value
                        // position resolves to its function type (the HIR
                        // lowerer emits a fn-value BindingRef for it). The
                        // generic case reached here without a context-determined
                        // expected type (the `check_against` arm handles the
                        // annotated case); emit a diagnostic asking for a type
                        // annotation or suggesting a direct call / lambda wrap.
                        if let Some(sig) = self.fn_sigs.get(&qualified_key) {
                            if sig.type_params.is_empty() {
                                let ty = Ty::Function {
                                    params: sig.params.clone(),
                                    ret: Box::new(sig.return_type.clone()),
                                };
                                self.used_modules.borrow_mut().insert(ImportKey::new(
                                    self.current_module.clone(),
                                    name.clone(),
                                ));
                                // Apply the same wasm native-only guard used for call
                                // expressions (#2135): a value-position reference to a
                                // native-only stdlib function is itself a
                                // `PlatformLimitation` rejection on wasm32, mirroring
                                // the call-form guard in methods.rs.
                                // NATIVE_ONLY_WASM_MODULE_REJECTIONS is the single source
                                // of truth; both guards iterate the same slice.
                                if self.wasm_target && !self.user_modules.contains(name.as_str()) {
                                    for &(module, feature) in
                                        Self::NATIVE_ONLY_WASM_MODULE_REJECTIONS
                                    {
                                        if name.as_str() == module {
                                            self.reject_wasm_feature(span, feature);
                                        }
                                    }
                                    // crypto.random_bytes depends on a native-only secure
                                    // entropy source absent from the wasm32 link set.
                                    if name.as_str() == "crypto" && field == "random_bytes" {
                                        self.reject_wasm_feature(
                                            span,
                                            WasmUnsupportedFeature::CryptoRandom,
                                        );
                                    }
                                }
                                return ty;
                            }
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "`{qualified_key}` is a generic function; to use it as a \
                                     value, add a type annotation that fully determines its \
                                     type parameters (e.g. `let f: fn({params}) -> {ret} = \
                                     {qualified_key}`), call it directly, or wrap it in a \
                                     lambda (`|x| {qualified_key}(x)`)",
                                    params = sig
                                        .params
                                        .iter()
                                        .map(|t| format!("{t}"))
                                        .collect::<Vec<_>>()
                                        .join(", "),
                                    ret = sig.return_type,
                                ),
                            );
                            return Ty::Error;
                        }
                        let similar = crate::error::find_similar(
                            field,
                            self.env
                                .all_names()
                                .filter_map(|k| k.strip_prefix(&format!("{name}.")))
                                .filter(|k| !k.contains('.')),
                        );
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("module `{name}` has no exported constant `{field}`"),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
            }
        }

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
                                            child_name: cn.clone(),
                                            supervisor: sup_name.clone(),
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
                                            child_name: cn.clone(),
                                            supervisor: sup_name.clone(),
                                        },
                                        ty.clone(),
                                    )
                                })
                                })
                        });
                        if let Some((slot, child_type)) = resolved_slot {
                            self.supervisor_child_slots
                                .insert(SpanKey::in_module(span, self.current_module_idx), slot);
                            // Path-4 defense-in-depth bound enforcement on
                            // supervisor-child PID synthesis. The
                            // `SupervisorChildren` table stores child types
                            // as bare `String` names (see
                            // `check::types::SupervisorChildren`); the
                            // synthesised PID payload is `Ty::Named { args:
                            // vec![] }`, so the helper short-circuits today
                            // on empty args. Wiring the call here means any
                            // future change that admits type-parameterised
                            // children (e.g. `child h: Holder<File>`)
                            // inherits enforcement at the canonical seam
                            // rather than re-litigating bound checking at a
                            // sibling site.
                            self.enforce_type_def_instantiation_bounds(&child_type, &[], span);
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
                        // Substitute generic type params with concrete args in
                        // parallel so a swap instantiation like `Pair<B, A>` does
                        // not alias: sequential A→B then B→A would produce A again.
                        let subst_map: HashMap<String, Ty> = td
                            .type_params
                            .iter()
                            .zip(args.iter())
                            .map(|(p, a)| (p.clone(), a.clone()))
                            .collect();
                        field_ty.substitute_named_params_parallel(&subst_map)
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
                    } else if let Some((ref mn, ref state_name)) =
                        self.current_machine_lifecycle.clone()
                    {
                        // Inside a state entry/exit lifecycle block: resolve
                        // `state.<field>` for payload states.  `event` is NOT
                        // bound (that binding belongs to transition scope only),
                        // so event-enum field access is correctly unreachable here.
                        if td.kind == TypeDefKind::Machine && *mn == *name {
                            if let Some(VariantDef::Struct(variant_fields)) =
                                td.variants.get(state_name.as_str()).cloned()
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

    fn check_machine_transition_self_field_access(
        &mut self,
        field: &str,
        span: &Span,
    ) -> Option<Ty> {
        let (machine_name, source_state, _) = self.current_machine_transition.clone()?;
        if source_state == "_" {
            self.report_error(
                TypeErrorKind::UndefinedField,
                span,
                format!(
                    "cannot read `self.{field}` in wildcard transition for machine \
                     `{machine_name}` because `_` has no single source-state payload"
                ),
            );
            return Some(Ty::Error);
        }

        let variant_fields = match self.lookup_type_def(&machine_name) {
            Some(td) if td.kind == TypeDefKind::Machine => td.variants.get(&source_state).cloned(),
            _ => None,
        };
        let Some(VariantDef::Struct(variant_fields)) = variant_fields else {
            self.report_error(
                TypeErrorKind::UndefinedField,
                span,
                format!(
                    "state `{source_state}` of machine `{machine_name}` has no payload field \
                     `{field}`"
                ),
            );
            return Some(Ty::Error);
        };

        if let Some((_, field_ty)) = variant_fields
            .iter()
            .find(|(field_name, _)| field_name == field)
        {
            return Some(field_ty.clone());
        }

        let similar =
            crate::error::find_similar(field, variant_fields.iter().map(|(name, _)| name.as_str()));
        self.report_error_with_suggestions(
            TypeErrorKind::UndefinedField,
            span,
            format!("state `{source_state}` of machine `{machine_name}` has no field `{field}`"),
            similar,
        );
        Some(Ty::Error)
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
        // When this `match` is itself a function-return tail, every arm body
        // flows to the return and may Ok-coerce. Capture the armed state once;
        // the per-arm guard check and pattern binding are not tail positions, so
        // re-arm immediately before each arm body.
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            // Check guard if present
            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            self.tail_ok_armed = tail_ok_armed;
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
        // Leave the flag disarmed: the arm loop set it per-arm, and the
        // exhaustiveness check below is not a tail position.
        self.tail_ok_armed = false;

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
        is_actor_body: bool,
    ) -> Ty {
        // Save/restore capture tracking state for nested lambdas
        let prev_capture_depth = self.lambda_capture_depth;
        let prev_captures = std::mem::take(&mut self.lambda_captures);
        let prev_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = false;
        // A closure is NOT the crash hook itself, even if it is syntactically
        // nested inside one.  Clear in_crash_hook so a `return CrashAction::X;`
        // inside the nested closure does not inherit the hook's flag and fire a
        // false-positive CrashActionReturnNotYetWired diagnostic.
        let prev_in_crash_hook = self.in_crash_hook;
        self.in_crash_hook = false;
        // A lambda body does not inherit the lexical task scope it is written
        // inside: the closure may run after the scope has joined, so `fork`
        // statements inside it have no spawn context.
        let prev_task_scope_depth = self.task_scope_depth;
        self.task_scope_depth = 0;
        let prev_in_lambda_actor_body = self.in_lambda_actor_body;
        // Set is_actor_body for the duration of this lambda's body; nested fn-closures
        // are called with is_actor_body=false, so they get false regardless of the outer flag.
        self.in_lambda_actor_body = is_actor_body;

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
                // Unify the annotated type against the expected param type regardless
                // of whether the annotation contains holes.  The holes path (deferred
                // inference) is orthogonal: a fully-concrete annotation (`|x: i64|`)
                // must still be rejected when the expected param type is `bool`.
                if let Some((expected_params, _)) = &expected {
                    if let Some(expected_ty) = expected_params.get(i) {
                        self.expect_type(expected_ty, &annotated_ty, &annotation.1);
                    }
                }
                if !hole_vars.is_empty() {
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
            // Unify the annotated return type against the contextual expected return
            // type regardless of holes — same rationale as annotated param types above.
            if let Some((_, contextual_ret)) = expected {
                self.expect_type(contextual_ret, &expected_ret, &annotation.1);
            }
            if !hole_vars.is_empty() {
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
        self.in_crash_hook = prev_in_crash_hook;
        self.task_scope_depth = prev_task_scope_depth;
        self.in_lambda_actor_body = prev_in_lambda_actor_body;
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
        // Scan the lambda body once syntactically to determine which
        // capture names are mutated (`BorrowMut` over `Borrow`) and
        // whether the body contains a suspend point
        // (`NonSyncMutCaptureCrossesSuspend` gate).
        let body_facts = super::closure_inference::scan_lambda_body(body);
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
            fact.is_sync = self.registry.is_sync(&resolved_ty);
            let mutates = body_facts.mutated_names.contains(&fact.name);
            // Capture-mode inference rule:
            //   `move`     → Move  / ExplicitMove
            //   Copy type  → Copy  / ImplicitCopy
            //   mut use    → BorrowMut / InferredBorrowMut
            //   read-only  → Borrow    / InferredBorrow
            let (selected_mode, selected_origin) = if is_move {
                (ClosureCaptureMode::Move, CaptureModeOrigin::ExplicitMove)
            } else if is_copy {
                (ClosureCaptureMode::Copy, CaptureModeOrigin::ImplicitCopy)
            } else if mutates {
                (
                    ClosureCaptureMode::BorrowMut,
                    CaptureModeOrigin::InferredBorrowMut,
                )
            } else {
                (
                    ClosureCaptureMode::Borrow,
                    CaptureModeOrigin::InferredBorrow,
                )
            };
            fact.mode = selected_mode;
            fact.mode_origin = selected_origin;
            fact.ty = resolved_ty.clone();
            // Substrate gain: inferred `Borrow` / `BorrowMut`
            // captures are ACCEPTED. Previously, non-Copy non-`move`
            // captures emitted `ClosureExplicitMoveRequired`; the
            // checker now records the inferred mode and lets the
            // lowerer materialise the reference. The legacy diagnostic
            // is dead for this site (still declared for the source
            // span on `move` keyword misuse, if a future caller needs
            // it).
            // An inferred `BorrowMut` capture of a non-`Sync` binding
            // crossing a suspend point is rejected until a future
            // auto-lock pass subscribes to this kind and rewrites the
            // closure. Diagnostic kind name is the public seam; do not
            // rename.
            if matches!(fact.mode, ClosureCaptureMode::BorrowMut)
                && !fact.is_sync
                && body_facts.has_suspend
                && !matches!(resolved_ty, Ty::Error | Ty::Var(_))
            {
                let suspend_label = if body_facts.suspend_kind.is_empty() {
                    "await".to_string()
                } else {
                    body_facts.suspend_kind.clone()
                };
                let origin_label = match fact.mode_origin {
                    CaptureModeOrigin::InferredBorrowMut => "inferred from a mutating use",
                    CaptureModeOrigin::InferredBorrow => "inferred read-only borrow",
                    CaptureModeOrigin::ExplicitMove => "explicit `move`",
                    CaptureModeOrigin::ImplicitCopy => "implicit copy",
                };
                self.report_error(
                    TypeErrorKind::NonSyncMutCaptureCrossesSuspend {
                        capture_name: fact.name.clone(),
                        suspend_kind: suspend_label.clone(),
                    },
                    &fact.use_span,
                    format!(
                        "non-Sync capture `{}` is mutated across a `{}` point \
                         (mode = BorrowMut, {}); this is unsound until \
                         automatic locking lands",
                        fact.name, suspend_label, origin_label
                    ),
                );
            }
            if is_move && !is_copy {
                self.env.mark_moved(&fact.name, span.clone());
            }
            capture_facts.push(fact);
        }
        self.closure_capture_facts.insert(
            SpanKey::in_module(span, self.current_module_idx),
            capture_facts.clone(),
        );

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

    /// Resolve a module-qualified value-constructor reference of the form
    /// `module.Type::Variant` to its result type.  Emits a fail-closed
    /// diagnostic for each of the four error shapes (unknown module alias,
    /// no exported type, no such variant, struct-variant without braces)
    /// — never falls through to the leaky "undefined variable" /
    /// "undefined type" surface.  Called only from the
    /// `check_field_access` pre-dispatch arm.
    pub(super) fn check_module_qualified_variant_ref(
        &mut self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
        span: &Span,
    ) -> Ty {
        if !self.modules.contains(module_short) {
            let similar =
                crate::error::find_similar(module_short, self.modules.iter().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedVariable,
                span,
                format!("unknown module alias `{module_short}`"),
                similar,
            );
            return Ty::Error;
        }
        self.used_modules.borrow_mut().insert(ImportKey::new(
            self.current_module.clone(),
            module_short.to_string(),
        ));
        let Some(td) = self.resolve_module_type(module_short, type_name) else {
            let similar = self
                .module_type_exports
                .get(module_short)
                .map(|set| crate::error::find_similar(type_name, set.iter().map(String::as_str)))
                .unwrap_or_default();
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedType,
                span,
                format!("module `{module_short}` has no exported type `{type_name}`"),
                similar,
            );
            return Ty::Error;
        };
        let Some((_td_again, variant)) =
            self.resolve_module_variant(module_short, type_name, variant_name)
        else {
            let similar =
                crate::error::find_similar(variant_name, td.variants.keys().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedField,
                span,
                format!("type `{module_short}.{type_name}` has no variant `{variant_name}`"),
                similar,
            );
            return Ty::Error;
        };
        let qualified_type = format!("{module_short}.{type_name}");
        match variant {
            VariantDef::Unit => {
                // Instantiate type params with fresh inference vars so generic
                // enums (e.g. `Option<T>::None`) unify against later annotations.
                // Mirrors the unit-variant path in `resolve_identifier_variant`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                Ty::normalize_named(qualified_type, args)
            }
            VariantDef::Tuple(params) => {
                // Tuple-variant naked reference (no call): treat as a function
                // value, matching the bare-identifier function-value path at
                // expressions.rs (resolve_identifier).  The call form
                // `m.Type::V(args)` is handled by `check_method_call` via
                // `lookup_variant_constructor`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                let ctor_subst_map: HashMap<String, Ty> = td
                    .type_params
                    .iter()
                    .zip(args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                let subst_params: Vec<Ty> = params
                    .iter()
                    .map(|p| p.substitute_named_params_parallel(&ctor_subst_map))
                    .collect();
                let ret = Ty::normalize_named(qualified_type, args);
                Ty::Function {
                    params: subst_params,
                    ret: Box::new(ret),
                }
            }
            VariantDef::Struct(_) => {
                // Struct variants require the braced initialiser; parse never
                // reaches this arm in the StructInit shape (that goes through
                // `check_struct_init`).  A naked `m.E::V` for a struct variant
                // is a user error — emit a hint rather than silently typing it.
                self.report_error(
                    TypeErrorKind::UndefinedField,
                    span,
                    format!(
                        "variant `{module_short}.{type_name}::{variant_name}` is a struct \
                         variant; use `{module_short}.{type_name}::{variant_name} {{ ... }}` \
                         to construct it"
                    ),
                );
                Ty::Error
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
        // Module-qualified diagnostic pre-pass: when `name` has the shape
        // `module.Type::Variant` and `module` is a known module alias, route
        // the failure modes (no exported type / no such variant) through the
        // same fail-closed diagnostics used by `check_field_access`'s
        // module-qualified pre-dispatch.  Without this pre-pass the
        // enum-variant fallback in the main body falls through to
        // "undefined type `module.Type::Variant`" which leaks the
        // qualified-name layout into the diagnostic and gives the user no
        // actionable signal.  Success cases (both type and variant exist) fall
        // through to the existing struct/enum-variant init logic.
        if let Some(dot) = name.find('.') {
            let module_short = &name[..dot];
            if self.modules.contains(module_short) {
                let after_dot = &name[dot + 1..];
                if let Some(colon) = after_dot.find("::") {
                    let type_name = &after_dot[..colon];
                    let variant_name = &after_dot[colon + 2..];
                    self.used_modules.borrow_mut().insert(ImportKey::new(
                        self.current_module.clone(),
                        module_short.to_string(),
                    ));
                    let Some(td) = self.resolve_module_type(module_short, type_name) else {
                        let similar = self
                            .module_type_exports
                            .get(module_short)
                            .map(|set| {
                                crate::error::find_similar(
                                    type_name,
                                    set.iter().map(String::as_str),
                                )
                            })
                            .unwrap_or_default();
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedType,
                            span,
                            format!("module `{module_short}` has no exported type `{type_name}`"),
                            similar,
                        );
                        return Ty::Error;
                    };
                    if !td.variants.contains_key(variant_name) {
                        let similar = crate::error::find_similar(
                            variant_name,
                            td.variants.keys().map(String::as_str),
                        );
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!(
                                "type `{module_short}.{type_name}` has no variant `{variant_name}`"
                            ),
                            similar,
                        );
                        return Ty::Error;
                    }
                    // Both type and variant exist — fall through.
                }
            }
        }
        // Fail closed under qualified-by-default before binding a bare record
        // constructor: a bare name published by more than one module is
        // ambiguous, and one exported but published by none is not in scope.
        // Without this gate the construction falls through to `lookup_type_def`
        // and silently binds a last-write-wins bare def, then trips a confusing
        // downstream MIR field-order failure. The `::` enum-variant and
        // explicitly module-qualified spellings already routed above are left
        // untouched (they carry a `.` or `::` and never match a bare name).
        if !name.contains('.')
            && !name.contains("::")
            && self.report_bare_type_scope_error(name, span)
        {
            return Ty::Error;
        }
        // A bare construction (`Gadget { … }`) of a type published by exactly
        // one imported module binds to that owner's QUALIFIED identity, so the
        // constructed value carries `owner.Gadget` rather than the bare
        // last-write-wins key. This keeps two modules' same-bare-name records
        // from colliding in the downstream MIR record-layout / field-order
        // registry (the same identity discipline `samename_type_layout` proves
        // for explicitly qualified constructions).
        let qualified_owned = self.published_bare_type_qualified(name);
        if let Some(qualified) = qualified_owned.as_deref() {
            if let Some((module, _)) = qualified.split_once('.') {
                self.used_modules.borrow_mut().insert(ImportKey::new(
                    self.current_module.clone(),
                    module.to_string(),
                ));
            }
        }
        let name = qualified_owned.as_deref().unwrap_or(name);
        // Fail closed on opaque handle direct construction — but ONLY for
        // cross-module constructions. The module that DECLARES an `#[opaque]`
        // type is the producer: its impl blocks contain the legitimate FFI
        // constructors (`extern "C"` stubs returning the handle) and must be
        // allowed to write `Handle { }` as the return value stub. Only OTHER
        // modules (importers / users) see it as opaque and must use the
        // declared constructor functions.
        //
        // `local_type_defs` is seeded (in `mod.rs`) with every type NAME
        // declared in the current module before body-checking begins. A bare
        // name that is present there means "this module declared it", so the
        // construction is in-module / producer-side and is ALLOWED.
        //
        // `name` at this point may be qualified (`module.Handle`) after
        // `published_bare_type_qualified` resolves a bare import reference.
        // `user_opaque_type_names` stores only unqualified names (from `td.name`),
        // so we must also check the unqualified component of the qualified name.
        let unqualified = name.split_once('.').map_or(name, |(_, unqual)| unqual);
        let is_declaring_module = self.local_type_defs.contains(unqualified);
        let is_opaque_handle = !is_declaring_module
            && (self.user_opaque_type_names.contains(name)
                || self.user_opaque_type_names.contains(unqualified)
                || self.canonical_owned_handle_type_name(name).is_some());
        if is_opaque_handle {
            self.report_error(
                TypeErrorKind::OpaqueDirectConstruct {
                    type_name: name.to_string(),
                },
                span,
                format!(
                    "cannot construct opaque type `{name}` directly; \
                     opaque handles are produced by their stdlib constructors \
                     [E_OPAQUE_CONSTRUCT]"
                ),
            );
            return Ty::Error;
        }
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
                    // Substitute already-inferred type params into the expected type.
                    // Use parallel substitution so a swap map {"A": B, "B": A} does not
                    // alias both params: each Named leaf is replaced in one structural pass.
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

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
            // Declaration-bound enforcement on the plain struct-init path.
            // The helper short-circuits cleanly for bound-free names and
            // enforces the TypeDef-owned bound map for every generic nominal
            // whose arguments were inferred from the fields above.
            self.enforce_type_def_instantiation_bounds(name, &type_args, span);
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
                    let expected = declared_ty.substitute_named_params_parallel(&type_arg_map);

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
            // Enforce trait bounds declared on the enum's generic type
            // parameters via the canonical nominal helper. This keeps
            // struct-variant brace init on the same TypeDef-bound authority as
            // annotations, tuple variants, and plain struct/record init.
            self.enforce_type_def_instantiation_bounds(&enum_name, &type_args, span);
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

    /// Check each constructor argument of a `spawn` expression, pushing the
    /// actor field's declared type down as the expected type so that generic
    /// constructors like `HashMap::new()` and `Vec::new()` can resolve their
    /// type parameters.
    ///
    /// Without this, `spawn Cache(store: HashMap::new())` synthesises the arg
    /// with unbound type variables (`HashMap<?T, ?U>`).  The Send check then
    /// fires on those unbound vars with the misleading message:
    ///   "cannot send `HashMap<?T22, ?T23>` to actor: type is not Send"
    ///
    /// Mirrors `check_struct_init`'s field push-down.  Two exceptions fall back
    /// to `synthesize`:
    ///
    /// 1. Unknown field name — an error will be reported separately.
    /// 2. Bare-actor-name field (e.g. `let target: Printer`): the spawn arg is
    ///    `LocalPid<Printer>`, not `Printer`, so checking against the bare name
    ///    produces a spurious type mismatch.
    fn check_spawn_constructor_args(&mut self, actor_name: &str, args: &[(String, Spanned<Expr>)]) {
        let actor_fields: Option<HashMap<String, Ty>> =
            self.lookup_type_def(actor_name).map(|td| td.fields);
        for (field_name, (arg, as_)) in args {
            let declared = actor_fields.as_ref().and_then(|f| f.get(field_name));
            let ty_raw = match declared {
                Some(declared_ty) => {
                    let is_bare_actor = if let Ty::Named {
                        name: field_type_name,
                        ..
                    } = declared_ty
                    {
                        self.type_defs
                            .get(field_type_name)
                            .is_some_and(|td| td.kind == TypeDefKind::Actor)
                    } else {
                        false
                    };
                    if is_bare_actor {
                        self.synthesize(arg, as_)
                    } else {
                        self.check_against(arg, as_, declared_ty)
                    }
                }
                None => self.synthesize(arg, as_),
            };
            self.enforce_actor_boundary_send(arg, as_, as_, &ty_raw);
        }
    }

    /// Resolve a `spawn` target expression to the registered actor identity.
    ///
    /// `Ok(Some(identity))` carries the identity key (bare for root/flat
    /// actors, dotted `{module}.{name}` for module actors); `Ok(None)` is an
    /// unsupported target shape; `Err(())` means a diagnostic was already
    /// emitted and the spawn must type to bare `Ty::Error`.
    fn resolve_spawn_target(
        &mut self,
        target: &Spanned<Expr>,
        span: &Span,
    ) -> Result<Option<String>, ()> {
        Ok(match &target.0 {
            // Bare spawn target: resolve local-first to the registered
            // actor identity (the current module's own actor, then a
            // root/flat actor, then a named-import binding, then a unique
            // module export). A bare name exported by 2+ modules with no
            // local actor is a typed error naming the candidates — never
            // silent first-wins.
            Expr::Identifier(name) => match self.resolve_bare_actor_identity(name) {
                super::types::BareActorResolution::Resolved(identity) => Some(identity),
                super::types::BareActorResolution::Ambiguous(candidate_modules) => {
                    self.report_ambiguous_actor_reference(name, &candidate_modules, span);
                    return Err(());
                }
                // Unknown actor: keep the bare name so the pre-existing
                // unknown-actor diagnostics downstream fire unchanged.
                super::types::BareActorResolution::Unknown => Some(name.clone()),
            },
            // Handle module-qualified actor: spawn module.ActorName(args)
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    if self.modules.contains(module) {
                        // Verify the qualifier resolves to an ACTOR that is a
                        // public export of `module` before stripping it to the
                        // bare name. `module_type_exports` membership alone is
                        // insufficient: that set also holds public NON-actor
                        // types, so it is true even when `secret.Account` is a
                        // `pub type`/struct/enum (and a private actor is absent
                        // from it entirely). Resolve the qualified definition and
                        // require `TypeDefKind::Actor`; otherwise
                        // `spawn secret.Account()` would lower to bare `Account`
                        // and silently route to a same-named root/pub actor -- a
                        // capability-boundary hole. `resolve_module_type` already
                        // gates on `pub` export + the module-qualified `type_defs`
                        // entry (which is copied from the module's own decl, so it
                        // is not clobbered by a same-named root/other-module type).
                        // Fail closed before HIR/MIR rather than misroute.
                        let is_actor_export = self
                            .resolve_module_type(module, field)
                            .is_some_and(|td| td.kind == TypeDefKind::Actor);
                        if !is_actor_export {
                            let similar = self
                                .module_type_exports
                                .get(module)
                                .map(|set| {
                                    crate::error::find_similar(
                                        field,
                                        set.iter().map(String::as_str),
                                    )
                                })
                                .unwrap_or_default();
                            self.report_error_with_suggestions(
                                TypeErrorKind::UndefinedType,
                                span,
                                format!("module `{module}` has no exported actor `{field}`"),
                                similar,
                            );
                            // The caller types the spawn as bare `Ty::Error`
                            // (not `LocalPid<Error>`) so a subsequent
                            // `await handle.method()` is suppressed (method
                            // calls on a `Ty::Error` receiver short-circuit),
                            // keeping a single clear diagnostic.
                            return Err(());
                        }
                        self.used_modules
                            .borrow_mut()
                            .insert(ImportKey::new(self.current_module.clone(), module.clone()));
                        // Keep the module qualifier: the dotted
                        // `{module}.{field}` key IS the actor's identity in
                        // `type_defs`/`fn_sigs`, and the spawn result type
                        // (`LocalPid<bank.Account>`) is what every ask site
                        // and the MIR layout lookup key on. Stripping it to
                        // the bare name made two same-named module actors
                        // indistinguishable below the checker.
                        Some(format!("{module}.{field}"))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        })
    }

    pub(super) fn check_spawn(
        &mut self,
        target: &Spanned<Expr>,
        type_args: &[Spanned<TypeExpr>],
        args: &[(String, Spanned<Expr>)],
        span: &Span,
    ) -> Ty {
        let Ok(actor_name) = self.resolve_spawn_target(target, span) else {
            return Ty::Error;
        };

        if let Some(name) = actor_name {
            self.check_spawn_constructor_args(&name, args);

            // Resolve the explicit type-argument list.  When type_args is
            // non-empty, substitute them into the PID type and enforce any
            // declared bounds.  When empty, check whether the actor is
            // generic (has declared type params) and emit a diagnostic if so.
            let resolved_type_args: Vec<Ty> = type_args
                .iter()
                .map(|te| self.resolve_type_expr(te))
                .collect();

            // Look up the arity of the actor's declared type params (0 for
            // non-generic actors).
            let declared_arity = self
                .type_defs
                .get(&name)
                .map_or(0, |td| td.type_params.len());

            if declared_arity > 0 && resolved_type_args.is_empty() {
                // Generic actor spawned without type arguments.
                self.report_error(
                    TypeErrorKind::MissingActorTypeArgs {
                        actor_name: name.clone(),
                        expected_arity: declared_arity,
                    },
                    span,
                    format!(
                        "actor `{name}` has {declared_arity} type parameter(s); \
                         supply explicit type arguments: `spawn {name}<T>(...)`"
                    ),
                );
                return Ty::local_pid(Ty::Error);
            }

            if declared_arity > 0
                && !resolved_type_args.is_empty()
                && resolved_type_args.len() != declared_arity
            {
                self.report_error(
                    TypeErrorKind::ActorTypeArgArityMismatch {
                        actor_name: name.clone(),
                        expected: declared_arity,
                        got: resolved_type_args.len(),
                    },
                    span,
                    format!(
                        "actor `{name}` has {declared_arity} type parameter(s) but \
                         {} type argument(s) were supplied",
                        resolved_type_args.len()
                    ),
                );
                return Ty::local_pid(Ty::Error);
            }

            // Enforce declared bounds on the resolved type arguments.
            if !resolved_type_args.is_empty() {
                self.enforce_actor_instantiation_bounds(&name, &resolved_type_args, span);
            }

            // Record the actor-mono entry: (actor_name, resolved_type_args).
            // This populates the per-spawn instantiation table consulted by
            // the actor-mono discovery pass (blocked on MachineMonoPass infra).
            // Today's entry is keyed by mangled name; the runtime
            // mailbox ABI is unchanged (opaque pointer, no `hew_actor_spawn`
            // change).
            if !resolved_type_args.is_empty() {
                self.record_actor_mono_entry(&name, resolved_type_args.clone(), span);
            }

            Ty::local_pid(Ty::Named {
                builtin: None,
                name,
                args: resolved_type_args,
            })
        } else {
            Ty::local_pid(Ty::Error)
        }
    }

    /// Report the typed ambiguity error for a bare actor reference that is
    /// exported by two or more modules with no local actor to win the
    /// local-first resolution. Names every candidate and suggests the
    /// qualified spawn spelling — never silent first-wins.
    fn report_ambiguous_actor_reference(
        &mut self,
        name: &str,
        candidate_modules: &[String],
        span: &Span,
    ) {
        let candidates_list = candidate_modules
            .iter()
            .map(|m| format!("`{m}.{name}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let qualified_examples = candidate_modules
            .iter()
            .map(|m| format!("`spawn {m}.{name}(...)`"))
            .collect::<Vec<_>>()
            .join(" or ");
        self.report_error_with_suggestions(
            TypeErrorKind::AmbiguousActorReference {
                actor_name: name.to_string(),
                candidate_modules: candidate_modules.to_vec(),
            },
            span,
            format!(
                "actor `{name}` is ambiguous: it is exported by multiple \
                 modules ({candidates_list}) and no local actor `{name}` \
                 exists to take precedence"
            ),
            vec![format!(
                "qualify the spawn target with its module: {qualified_examples}"
            )],
        );
    }

    /// Record a generic actor spawn instantiation.
    ///
    /// Inserts `(actor_name, type_args)` into the `actor_spawn_type_args`
    /// table keyed by the spawn expression span. The mangled symbol is
    /// derived at output time via
    /// `mangle_instantiation(SymbolClass::Actor, actor_name, type_args, &[])`.
    /// This table is consumed by the actor-mono discovery pass.
    pub(super) fn record_actor_mono_entry(
        &mut self,
        actor_name: &str,
        type_args: Vec<Ty>,
        span: &Span,
    ) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.actor_spawn_type_args
            .entry(key)
            .or_insert_with(|| (actor_name.to_string(), type_args));
    }

    /// Check if an expression is typically used for side effects (not for its return value).
    pub(super) fn record_type(&mut self, span: &Span, ty: &Ty) {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .insert(key.clone(), self.current_module.clone());
        self.expr_types.insert(key, ty.clone());
    }

    pub(super) fn record_integer_literal_type(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        self.record_type(span, ty);
        if let Expr::Unary {
            op: UnaryOp::Negate,
            operand,
        } = expr
        {
            self.record_type(&operand.1, ty);
        }
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
        // Closure literals are env-heap regardless of resolved type.
        if matches!(expr, Expr::Lambda { .. }) {
            return AllocationClass::ClosureEnv;
        }
        let key = SpanKey::in_module(span, self.current_module_idx);
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
            span_key: SpanKey::in_module(stmt_span, self.current_module_idx),
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
        if let Some(rhs_ty) = self.resolve_is_type_pattern(&rhs.0) {
            return self.synthesize_is_type_pattern(lhs, &lhs_ty, rhs, &rhs_ty, span);
        }
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

    fn resolve_is_type_pattern(&self, rhs: &Expr) -> Option<Ty> {
        let Expr::Identifier(name) = rhs else {
            return None;
        };
        Ty::from_name(name).or_else(|| {
            self.lookup_type_def(name)
                .map(|type_def| Ty::normalize_named(type_def.name, vec![]))
        })
    }

    fn synthesize_is_type_pattern(
        &mut self,
        lhs: &Spanned<Expr>,
        lhs_ty: &Ty,
        rhs: &Spanned<Expr>,
        rhs_ty: &Ty,
        span: &Span,
    ) -> Ty {
        let lhs_resolved = self.subst.resolve(lhs_ty);
        let rhs_resolved = self.subst.resolve(rhs_ty);

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

        if lhs_ok && rhs_ok && lhs_resolved != rhs_resolved {
            self.report_error(
                TypeErrorKind::Mismatch {
                    expected: lhs_resolved.user_facing().to_string(),
                    actual: rhs_resolved.user_facing().to_string(),
                },
                span,
                format!(
                    "`is` type pattern must match the operand type; found `{}` and `{}`",
                    lhs_resolved.user_facing(),
                    rhs_resolved.user_facing()
                ),
            );
        } else if lhs_ok && rhs_ok {
            if !matches!(lhs.0, Expr::Identifier(_)) {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    &lhs.1,
                    "`is` type patterns currently require an identifier operand".to_string(),
                );
                return Ty::Bool;
            }
            let rhs_key = SpanKey::in_module(&rhs.1, self.current_module_idx);
            self.is_type_patterns.insert(rhs_key, rhs_resolved.clone());
            self.record_type(&rhs.1, &rhs_resolved);
            // Static-tautology warning: the LHS type already equals the RHS
            // type pattern, so the comparison lowers to `Bool(true)` (see
            // `hew-hir/src/lower.rs` Expr::Is branch) and any `else` branch
            // gated on the negation is silently dead. Surface this as a
            // `RedundantIs` warning so the user is told before they wonder
            // why their else-branch never runs.
            self.warnings.push(crate::error::TypeError {
                severity: crate::error::Severity::Warning,
                kind: TypeErrorKind::RedundantIs,
                span: span.clone(),
                message: format!(
                    "`is {0}` is always true here: the operand already has type `{0}`",
                    rhs_resolved.user_facing()
                ),
                notes: vec![],
                suggestions: vec![
                    "remove the `is` check, or compare against a different type".to_string()
                ],
                source_module: None,
            });
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
