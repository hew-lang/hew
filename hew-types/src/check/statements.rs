#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;
use crate::BuiltinType;

impl Checker {
    fn iterator_trait_item_ty(&mut self, iter_ty: &Ty, span: &Span) -> Option<Ty> {
        let resolved = self.subst.resolve(iter_ty);
        if let Ty::TraitObject { traits } = &resolved {
            for bound in traits {
                if bound.trait_name != "Iterator"
                    && !self.trait_extends(&bound.trait_name, "Iterator")
                {
                    continue;
                }
                if let Some((_, item_ty)) =
                    bound.assoc_bindings.iter().find(|(name, _)| name == "Item")
                {
                    return Some(item_ty.clone());
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "`for` over `dyn Iterator` requires an `Item` associated-type binding"
                        .to_string(),
                );
                return Some(Ty::Error);
            }
        }

        if let Ty::Named {
            name,
            args,
            builtin: Some(BuiltinType::Generator | BuiltinType::AsyncGenerator),
        } = &resolved
        {
            return args.first().cloned().or_else(|| {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!("`for` over a {name} requires a resolved yield type"),
                );
                Some(Ty::Error)
            });
        }

        if self.type_satisfies_trait_bound(&resolved, "IntoIterator") {
            let item_projection = Ty::AssocType {
                base: Box::new(resolved),
                trait_name: "IntoIterator".into(),
                assoc_name: "Item".into(),
            };
            return Some(self.project_assoc_types(&item_projection));
        }

        if !self.type_satisfies_trait_bound(&resolved, "Iterator") {
            return None;
        }

        let item_projection = Ty::AssocType {
            base: Box::new(resolved),
            trait_name: "Iterator".into(),
            assoc_name: "Item".into(),
        };
        Some(self.project_assoc_types(&item_projection))
    }

    fn assignment_root_binding_name(expr: &Expr) -> Option<&str> {
        match expr {
            Expr::Identifier(name) => Some(name.as_str()),
            Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => {
                Self::assignment_root_binding_name(&object.0)
            }
            _ => None,
        }
    }

    fn for_await_actor_method_name(&mut self, iterable: &Expr) -> Option<String> {
        let Expr::MethodCall {
            receiver, method, ..
        } = iterable
        else {
            return None;
        };
        let receiver_ty = {
            let ty = self.synthesize(&receiver.0, &receiver.1);
            self.subst.resolve(&ty)
        };
        let actor_ty = match receiver_ty.as_actor_handle() {
            Some(actor_ty) => self.subst.resolve(actor_ty),
            None => return None,
        };
        let Ty::Named { name, .. } = actor_ty else {
            return None;
        };
        let actor_name = self
            .type_defs
            .get(&name)
            .filter(|def| def.kind == TypeDefKind::Actor)
            .map_or(name, |def| def.name.clone());
        Some(format!("{actor_name}::{method}"))
    }

    pub(super) fn check_block(&mut self, block: &Block, expected: Option<&Ty>) -> Ty {
        self.env.push_scope();
        // Snapshot const_values so let-bound literal entries added in this
        // scope are cleaned up when the scope exits.
        let const_values_snapshot: HashMap<String, ConstValue> = self.const_values.clone();
        let num_stmts = block.stmts.len();
        let mut terminated = false;
        // Tail Ok-coercion is armed by the enclosing `check_fn_decl` only when
        // this block sits in function-return tail position. Every non-tail
        // statement in this block (and any sub-block reached for a non-tail
        // statement) must NOT coerce, so clear the flag for the statement loop
        // and restore it only for the block's own tail computation (the
        // `is_last` If/Match/Return arm and the trailing expression).
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        for (i, (stmt, span)) in block.stmts.iter().enumerate() {
            // If a previous statement was terminal, warn about this unreachable code
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: span.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                    source_module: self.current_module.clone(),
                });
                // Still type-check the remaining statements for error coverage,
                // but only emit the unreachable warning once per block.
                for (s, sp) in &block.stmts[i..] {
                    self.check_stmt(s, sp);
                }
                self.const_values = const_values_snapshot;
                self.emit_scope_warnings();
                return Ty::Never;
            }

            let is_last = i + 1 == num_stmts && block.trailing_expr.is_none();
            if is_last {
                // The final statement is the block's tail: re-arm so a tail
                // If/Match (whose arm bodies flow to the function return) can
                // Ok-coerce. `synthesize` (the `Stmt::Expression` arm) clears
                // the flag itself, and a trailing-`;` expression statement is
                // not a value-producing tail, so no coercion fires there.
                self.tail_ok_armed = tail_ok_armed;
                let ty = match stmt {
                    Stmt::If { .. }
                    | Stmt::IfLet { .. }
                    | Stmt::Match { .. }
                    | Stmt::Return(_)
                    | Stmt::Break { .. }
                    | Stmt::Continue { .. } => self.check_stmt_as_expr(stmt, span, expected),
                    Stmt::Expression((expr, es)) => {
                        let expr_ty = self.synthesize(expr, es);
                        if matches!(expr_ty, Ty::Never) {
                            Ty::Never
                        } else {
                            Ty::Unit
                        }
                    }
                    _ => {
                        self.check_stmt(stmt, span);
                        Ty::Unit
                    }
                };
                self.const_values = const_values_snapshot;
                self.emit_scope_warnings();
                return ty;
            }
            // For If/Match, use check_stmt_as_expr to get the result type
            // so we can detect when all branches terminate.
            if matches!(
                stmt,
                Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. }
            ) {
                let ty = self.check_stmt_as_expr(stmt, span, None);
                if matches!(ty, Ty::Never) {
                    terminated = true;
                }
            } else {
                self.check_stmt(stmt, span);
            }

            // Check if this statement terminates control flow
            if matches!(
                stmt,
                Stmt::Return(_) | Stmt::Break { .. } | Stmt::Continue { .. }
            ) {
                terminated = true;
            }
        }
        // If a trailing expression follows a terminal statement, it's unreachable
        let ty = if let Some(expr) = &block.trailing_expr {
            if terminated {
                self.warnings.push(TypeError {
                    severity: crate::error::Severity::Warning,
                    kind: TypeErrorKind::UnreachableCode,
                    span: expr.1.clone(),
                    message: "unreachable code".to_string(),
                    notes: vec![],
                    suggestions: vec![
                        "remove this code or restructure the control flow".to_string()
                    ],
                    source_module: self.current_module.clone(),
                });
            }
            // When the block has a known expected type, use check_against so that
            // numeric literals coerce to the target width before any fallback
            // materialization. For all other expressions check_against falls
            // through to synthesize + expect_type, producing the same result as
            // before.
            //
            // Re-arm the tail Ok-coercion: a trailing expression is this block's
            // value-producing tail, so it inherits the enclosing function's
            // armed state. `check_against` is the type-directed propagation site
            // that performs the actual Ok-wrap.
            self.tail_ok_armed = tail_ok_armed;
            if let Some(exp) = expected {
                self.check_against(&expr.0, &expr.1, exp)
            } else {
                self.synthesize(&expr.0, &expr.1)
            }
        } else {
            Ty::Unit
        };
        self.const_values = const_values_snapshot;
        self.emit_scope_warnings();
        ty
    }

    /// Emit `CrashActionReturnNotYetWired` for a `return CrashAction::…;` inside
    /// an `#[on(crash)]` hook body.  Called from both `Stmt::Return` arms
    /// (`check_stmt` and `check_stmt_as_expr`) after the type of the returned
    /// expression is confirmed to be `CrashAction`.
    ///
    /// WHEN obsolete: when v0.6 wires the full `CrashAction` return path through
    /// HIR lowering, remove this helper together with the `in_crash_hook` field
    /// and the `body_is_crash_action` check in `check_crash_hook` in `items.rs`.
    fn emit_crash_action_return_error(&mut self, span: &Span, fn_name: &str) {
        self.errors.push(TypeError::new(
            TypeErrorKind::CrashActionReturnNotYetWired,
            span.clone(),
            format!(
                "`#[on(crash)]` hook `{fn_name}` uses `return CrashAction::…`; \
                 `CrashAction` enum-variant construction is not yet wired through \
                 the compiler — use `panic(...)` (a diverging expression) as the \
                 hook body for now",
            ),
        ));
    }

    /// Type-check the operand of a `return` against the enclosing function's
    /// declared return type.
    ///
    /// This is the single shared shell for ALL `return` positions: the two
    /// statement-position `Stmt::Return` arms (`check_stmt` and
    /// `check_stmt_as_expr`) and the expression-position `Expr::Return`
    /// (`synthesize`). Routing every position through one helper keeps the
    /// generator-Return extraction, the `Ty::Error` guard, the unit-vs-declared
    /// mismatch diagnostic, and the `#[on(crash)]` fail-closed gate identical
    /// across positions (LESSONS `one-construct-one-lowering-shell`).
    ///
    /// The return *type* of the construct itself is always `Ty::Never` (a
    /// `return` diverges); callers assign that directly.
    pub(super) fn check_return_operand(&mut self, value: Option<&Spanned<Expr>>, span: &Span) {
        let Some(expected) = self.current_return_type.clone() else {
            return;
        };
        // Inside a gen{} body, `current_return_type` is shaped as
        // `Generator<Y, R>`. A `return <expr>` targets the Return component R,
        // not the full Generator type, so `return 1` inside gen{} unifies
        // against i64 rather than Generator<Y, i64>.
        let effective_expected = if self.in_generator {
            let resolved = self.subst.resolve(&expected);
            match resolved.as_generator() {
                Some((_, ret)) => ret.clone(),
                None => expected,
            }
        } else {
            expected
        };
        // Guard: do not check against Ty::Error — it would silently suppress
        // mismatch diagnostics in the returned expression. Synthesize the value
        // instead so its own errors are still caught.
        if matches!(self.subst.resolve(&effective_expected), Ty::Error) {
            if let Some((val, vs)) = value {
                self.synthesize(val, vs);
            }
        } else {
            match value {
                Some((val, vs)) => {
                    self.check_against(val, vs, &effective_expected);
                }
                None if effective_expected != Ty::Unit => {
                    self.errors.push(TypeError::return_type_mismatch(
                        span.clone(),
                        &effective_expected,
                        &Ty::Unit,
                    ));
                }
                _ => {}
            }
        }
        // Fail-closed: a `return <CrashAction>` inside a `#[on(crash)]` hook hits
        // the same unimplemented lowering path as the tail-expression form.
        // Reject it here so the user never reaches codegen.
        if self.in_crash_hook
            && matches!(
                self.subst.resolve(&effective_expected),
                Ty::Named { name, .. } if name == "CrashAction"
            )
        {
            let fn_name = self
                .current_function
                .clone()
                .unwrap_or_else(|| "<unknown>".to_string());
            self.emit_crash_action_return_error(span, &fn_name);
        }
    }

    /// Check a statement that may serve as a block's trailing expression.
    /// Returns the "expression type" of the statement.
    pub(super) fn check_stmt_as_expr(
        &mut self,
        stmt: &Stmt,
        span: &Span,
        expected: Option<&Ty>,
    ) -> Ty {
        match stmt {
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                let then_ty = self.check_block(then_block, expected);
                if let Some(eb) = else_block {
                    if let Some(ref if_stmt) = eb.if_stmt {
                        let else_ty = self.check_stmt_as_expr(&if_stmt.0, &if_stmt.1, expected);
                        self.unify_branches(&then_ty, &else_ty, &if_stmt.1)
                    } else if let Some(block) = &eb.block {
                        let else_ty = self.check_block(block, expected);
                        self.unify_branches(&then_ty, &else_ty, span)
                    } else {
                        Ty::Unit
                    }
                } else {
                    Ty::Unit
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
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
                let then_ty = self.check_block(body, expected);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    let else_ty = self.check_block(block, expected);
                    self.unify_branches(&then_ty, &else_ty, span)
                } else {
                    Ty::Unit
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_expr(&scr_ty, arms, span, expected)
            }
            Stmt::Expression((expr, es)) => self.synthesize(expr, es),
            Stmt::Return(value) => {
                self.check_return_operand(value.as_ref(), span);
                Ty::Never
            }
            Stmt::Break { .. } | Stmt::Continue { .. } => {
                self.check_stmt(stmt, span);
                Ty::Never
            }
            _ => {
                self.check_stmt(stmt, span);
                Ty::Unit
            }
        }
    }

    fn infer_integer_literal_binding_type(
        &mut self,
        value: Option<&Spanned<Expr>>,
        val_ty: Ty,
    ) -> Ty {
        let Some((expr, span)) = value else {
            return val_ty;
        };
        if is_integer_literal(expr) && val_ty.is_integer_literal() {
            let inferred = Ty::Var(TypeVar::fresh());
            self.expect_type(&inferred, &val_ty, span);
            self.record_integer_literal_type(expr, span, &inferred);
            inferred
        } else {
            val_ty
        }
    }

    /// Would a bare identifier `name` at a USE-site resolve to a known *unit*
    /// enum variant / nullary constructor?
    ///
    /// This decides, on the let-side, whether a bare pattern identifier is a
    /// refutable variant pattern (route to the refutability gate) or a fresh
    /// binding. It must agree with how `synthesize`/`synthesize_identifier`
    /// (expressions.rs) resolve the SAME bare name at a use-site: a
    /// disagreement produces a half-built binding, where the let-side binds a
    /// local that the use-side then shadows with the builtin/global variant,
    /// leaving an unused-variable warning plus a `cannot infer type` at the use.
    ///
    /// Resolution is therefore by the GLOBAL/builtin namespace and is
    /// independent of the value type, mirroring the use-side exactly:
    ///   * `None` is special-cased at the use-site (expressions.rs, the
    ///     `name == "None"` arm of `synthesize`) as `Option::None`
    ///     *unconditionally*, ahead of any binding lookup — so a bare `None`
    ///     is always a variant pattern here. A value type that is not `Option`
    ///     then surfaces as a clean single type mismatch (`None` is
    ///     `Option<_>`, the value is not), never a stray binding.
    ///   * any user enum's unit variant is found by the use-side's
    ///     `resolve_identifier_variant`, which scans every `type_defs` entry
    ///     for a `VariantDef::Unit` of that name — so the let-side scans the
    ///     same table the same way.
    ///
    /// Qualified paths (`E::A`) are handled by the caller and never reach here.
    /// Mirrors Rust: a name that names a unit variant/const is always a
    /// pattern, and a mismatched value type is a plain type error — not a
    /// binding.
    fn bare_identifier_resolves_to_unit_variant(&self, name: &str) -> bool {
        // Builtin `None`: the use-side resolves a bare `None` to `Option::None`
        // unconditionally and ahead of any binding, so the let-side treats it
        // as a variant pattern regardless of the value type. (`Some`, `Ok`,
        // `Err` carry payloads — never bare unit variants.)
        if name == "None" {
            return true;
        }
        // User enums: scan every type definition for a unit variant of this
        // name, exactly as the use-side's `resolve_identifier_variant` does.
        // A bare name that is a unit variant of *any* known enum is a pattern.
        self.type_defs
            .values()
            .any(|td| matches!(td.variants.get(name), Some(VariantDef::Unit)))
    }

    /// Does an identifier in `let`/binding position name a unit variant — a
    /// refutable tag-test that binds NOTHING — rather than introduce a fresh
    /// binder? This is the single authority for the let-side binder-vs-tag-test
    /// decision: `check_stmt`'s `let` arm uses it to skip `bind_pattern` for a
    /// unit-variant identifier (a `let None = … else { … }` / `let red = color
    /// else { … }` tag-test), and the borrowed-Rc escape scanner consults the
    /// SAME predicate so it never invents a dangerous-scope shadow for a name
    /// that did not actually bind. Detection is by resolution (a `::`-qualified
    /// path or a bare name resolving to a known unit variant), never by casing.
    pub(super) fn let_identifier_is_unit_variant(&self, name: &str) -> bool {
        name.contains("::") || self.bare_identifier_resolves_to_unit_variant(name)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "statement checking covers many Stmt variants"
    )]
    pub(super) fn check_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                let binding_context = match &pattern.0 {
                    Pattern::Identifier(name) => format!("local binding `{name}`"),
                    _ => "local binding".to_string(),
                };
                let deferred_hole_mark = self.deferred_inference_holes.len();
                let deferred_cast_mark = self.deferred_cast_checks.len();
                // Forward-bind for actor-lambda RHS: when `let name = actor |params| { body }`,
                // the body may reference `name` for recursive self-dispatch (architecture §5.9
                // ratification 2). Pre-bind the name with the Duplex type computed from
                // param/return annotations BEFORE synthesising the body so that the recursive
                // call resolves during body synthesis. The real `define_with_span` call below
                // overwrites this synthetic binding with the user-visible one.
                //
                // WHY: statement order synthesises the RHS before inserting the let-binding
                //   into scope; the actor body would see `fib` as undefined.
                // WHEN-OBSOLETE: if a general "let-rec" deferred-binding pass is added.
                // WHAT (real solution): a proper letrec/fix-point binder in the type checker.
                if ty.is_none() {
                    if let (
                        Pattern::Identifier(bind_name),
                        Some((
                            Expr::SpawnLambdaActor {
                                params,
                                return_type,
                                ..
                            },
                            _,
                        )),
                    ) = (&pattern.0, value)
                    {
                        let param_types: Vec<Ty> = params
                            .iter()
                            .map(|p| {
                                p.ty.as_ref().map_or_else(
                                    || Ty::Var(TypeVar::fresh()),
                                    |ann| self.resolve_type_expr(ann),
                                )
                            })
                            .collect();
                        let msg_ty = match param_types.len() {
                            0 => Ty::Unit,
                            1 => param_types.into_iter().next().unwrap(),
                            _ => Ty::Tuple(param_types),
                        };
                        let reply_ty = return_type
                            .as_ref()
                            .map_or(Ty::Unit, |ret| self.resolve_type_expr(ret));
                        let handle_ty = Ty::lambda_pid(msg_ty, reply_ty);
                        // Synthetic binding (no source span) — pre-populated for body lookup.
                        // Marked as already-used (read_count=1 in `define`) to avoid a
                        // spurious unused-variable warning at this site.
                        self.env.define(bind_name.clone(), handle_ty, false);
                    }
                }
                // Set pending_let_closure_name so synthesize_identifier can
                // detect recursive self-reference inside a closure body and emit
                // ClosureRecursive instead of UndefinedVariable.
                let prev_pending = self.pending_let_closure_name.take();
                if let (Pattern::Identifier(name), Some((Expr::Lambda { .. }, _))) =
                    (&pattern.0, &value)
                {
                    self.pending_let_closure_name = Some(name.clone());
                }
                let val_ty = if let Some((val, vs)) = value {
                    if let Some(annotation) = ty {
                        let expected =
                            self.resolve_annotation_with_holes(annotation, binding_context.clone());
                        self.check_against(val, vs, &expected)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some(annotation) = ty {
                    self.resolve_annotation_with_holes(annotation, binding_context.clone())
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                self.pending_let_closure_name = prev_pending;
                let val_ty = if ty.is_none() {
                    self.infer_integer_literal_binding_type(value.as_ref(), val_ty)
                } else {
                    val_ty
                };
                // Consume the scratch field unconditionally so stale state
                // never accumulates across statements.  Only register the
                // generic call signature in lambda_poly_sig_map when the binding value is
                // *directly* a generic lambda expression — indirect nesting
                // (a generic lambda buried inside a call argument, etc.) must
                // not be treated as a let-bound generic lambda.
                let generic_sig = self.last_lambda_generic_sig.take();
                let value_is_direct_generic_lambda = value.as_ref().is_some_and(|(val, _)| {
                    matches!(
                        val,
                        Expr::Lambda {
                            type_params: Some(tps),
                            ..
                        } if !tps.is_empty()
                    )
                });
                if ty.is_none() && !value_is_direct_generic_lambda {
                    let more_specific_hole_vars: Vec<_> = self.deferred_inference_holes
                        [deferred_hole_mark..]
                        .iter()
                        .flat_map(|hole| hole.hole_vars.iter().copied())
                        .chain(
                            self.deferred_cast_checks[deferred_cast_mark..]
                                .iter()
                                .flat_map(|check| check.target_hole_vars.iter().copied()),
                        )
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .collect();
                    self.record_deferred_monomorphic_site(
                        &pattern.1,
                        &binding_context,
                        &val_ty,
                        more_specific_hole_vars,
                    );
                }
                // A constructor-like identifier (`None`, `E::A`) is a refutable
                // UNIT-VARIANT pattern, not a plain binding — route it to the
                // refutability gate below so a let-else records its resolution
                // and a plain `let` is rejected.
                //
                // Detection is by RESOLUTION, never by case, and must AGREE
                // with the use-side: a `::`-qualified path is unambiguously a
                // variant, and a bare identifier is a unit-variant pattern when
                // the use-side would resolve it to a known unit variant in the
                // global/builtin namespace (`None`, any user enum's unit
                // variant) — independent of the value type. Any other bare
                // identifier — even an uppercase one like `INF` or `Foo` — is a
                // fresh binding. This mirrors Rust pattern resolution: a name
                // that names a unit variant/const is always a pattern (a
                // mismatched value type is then a clean type error); otherwise
                // it binds.
                let identifier_is_unit_variant = match &pattern.0 {
                    Pattern::Identifier(name) => self.let_identifier_is_unit_variant(name),
                    _ => false,
                };
                // For simple identifier patterns, track the definition span.
                // A unit-variant identifier is NOT a binding — it falls through
                // to the refutability gate below.
                let plain_identifier = match &pattern.0 {
                    Pattern::Identifier(name) if !identifier_is_unit_variant => Some(name),
                    _ => None,
                };
                if let Some(name) = plain_identifier {
                    if val_ty == Ty::Unit && value.is_some() {
                        self.warnings.push(TypeError {
                            severity: crate::error::Severity::Warning,
                            kind: TypeErrorKind::StyleSuggestion,
                            span: span.clone(),
                            message: format!("binding `{name}` has unit type and carries no value"),
                            notes: vec![],
                            suggestions: vec!["prefix with underscore: `_name`".to_string()],
                            source_module: self.current_module.clone(),
                        });
                    }
                    self.check_shadowing(name, &pattern.1);
                    self.env.define_with_span(
                        name.clone(),
                        val_ty.clone(),
                        false,
                        pattern.1.clone(),
                    );
                    // Register generic lambda binding for call-site inference.
                    // Both guards must hold: the scratch field was populated
                    // AND the let value is itself (not just contains) a generic
                    // lambda expression.
                    if value_is_direct_generic_lambda {
                        if let Some(sig) = generic_sig {
                            self.lambda_poly_sig_map.insert(
                                SpanKey::in_module(&pattern.1, self.current_module_idx),
                                sig,
                            );
                        }
                    }
                    // Track let-bound numeric literals for later coercion at use
                    // sites. Only unannotated immutable bindings preserve the
                    // literal kind/value; explicit annotations and mutable vars
                    // materialize immediately.
                    if ty.is_none() {
                        if let Some((val, _)) = value {
                            if is_integer_literal(val) {
                                if let Some(v) = extract_integer_literal_value(val) {
                                    self.const_values
                                        .insert(name.clone(), ConstValue::Integer(v));
                                }
                            } else if val_ty.is_float_literal() {
                                if let Some(v) = extract_float_literal_value(val) {
                                    self.const_values.insert(name.clone(), ConstValue::Float(v));
                                }
                            }
                        }
                    }
                } else {
                    // Refutability gate for `let` (not in `bind_pattern`, which
                    // `match` arms share).  A `let` binding has no failure arm,
                    // so only irrefutable patterns are admitted.  Poison-guard:
                    // skip the gate when the value type is unresolved — the root
                    // error has already fired and cascades would obscure it.
                    let resolved_val_ty = self.subst.resolve(&val_ty);
                    let maybe_refutable_kind = match &pattern.0 {
                        // Irrefutable product types — admitted without a gate error.
                        Pattern::Struct { name: pat_name, .. } => {
                            let type_name = resolved_val_ty.type_name();
                            match type_name {
                                Some(tn) => {
                                    let td = self.lookup_type_def(tn);
                                    match td {
                                        Some(td)
                                            if matches!(
                                                td.kind,
                                                TypeDefKind::Record | TypeDefKind::Struct
                                            ) =>
                                        {
                                            None
                                        } // irrefutable product type
                                        Some(_) => Some("enum variant"),
                                        None => {
                                            // Unknown type — checker already reported; allow
                                            // bind_pattern to run for error recovery.
                                            let _ = pat_name;
                                            None
                                        }
                                    }
                                }
                                None => None, // Ty::Var/Ty::Error — skip gate
                            }
                        }
                        // Enum-variant constructor (e.g. `Some(x)`) — always refutable.
                        Pattern::Constructor { .. } => Some("enum variant"),
                        // Unit variant written as a bare/qualified identifier
                        // (e.g. `None`, `E::A`) — refutable; binds nothing.
                        Pattern::Identifier(_) if identifier_is_unit_variant => {
                            Some("enum variant")
                        }
                        // Literal patterns are always refutable.
                        Pattern::Literal(_) => Some("literal"),
                        // Or-patterns are always refutable.
                        Pattern::Or(_, _) => Some("or-pattern"),
                        // All other patterns (Tuple, plain Identifier, Wildcard,
                        // Regex, …) — handled above or not refutable here.
                        _ => None,
                    };
                    match (maybe_refutable_kind, else_block) {
                        // Refutable pattern WITH an `else` clause: this is a
                        // let-else. The refutable pattern is admitted because
                        // the else clause supplies the failure path. The else
                        // block must diverge — it runs when the pattern fails
                        // and there is no value to bind, so control must not
                        // fall through to the binding. Check the else block
                        // BEFORE binding the pattern so the Ok-path binders are
                        // not visible inside it (they are bound only on the
                        // success path).
                        (Some(_), Some(else_blk)) => {
                            // Record the success-path pattern resolution so HIR
                            // lowering can consume the same `pattern_resolutions`
                            // side-table that powers `if let` / `match` /
                            // `while let`. Without this the let-else lowering
                            // finds no resolution and fails closed.
                            self.record_arm_resolution(&pattern.0, &pattern.1, &val_ty);
                            let else_ty = self.check_block(else_blk, None);
                            if !matches!(else_ty, Ty::Never)
                                && !matches!(resolved_val_ty, Ty::Var(_) | Ty::Error)
                            {
                                // Span the diagnostic on the else block tail.
                                // The trailing-expr / last-stmt span brackets
                                // it; fall back to the pattern span for an
                                // empty block (which is itself non-diverging).
                                let else_span = else_blk
                                    .trailing_expr
                                    .as_ref()
                                    .map(|e| e.1.clone())
                                    .or_else(|| else_blk.stmts.last().map(|(_, sp)| sp.clone()))
                                    .unwrap_or_else(|| pattern.1.clone());
                                self.report_error(
                                    TypeErrorKind::LetElseDoesNotDiverge,
                                    &else_span,
                                    "the `else` block of a `let … else` must \
                                     diverge (e.g. `return`, `break`, `continue`, or a \
                                     `!`-typed call); it must not fall through to the \
                                     binding"
                                        .to_string(),
                                );
                            }
                        }
                        // Refutable pattern with NO `else` clause: rejected. A
                        // plain `let` has no failure arm, so only irrefutable
                        // patterns are admitted. Suggest the let-else `else`
                        // clause (now that it exists) or `if let`/`match`.
                        (Some(kind_label), None) => {
                            // Only emit when the value type is actually resolved
                            // (not Ty::Var / Ty::Error) so a prior root error is
                            // not buried under a cascade.
                            if !matches!(resolved_val_ty, Ty::Var(_) | Ty::Error) {
                                self.report_error(
                                    TypeErrorKind::RefutableLetPattern {
                                        kind_label: kind_label.to_string(),
                                    },
                                    &pattern.1,
                                    format!(
                                        "refutable {kind_label} pattern is not allowed in a \
                                         plain `let`; add an `else {{ … }}` clause (it \
                                         must diverge), or use `if let`/`match`"
                                    ),
                                );
                            }
                        }
                        // Irrefutable pattern with an `else` clause: the else can
                        // never run, so divergence is not required. Type-check it
                        // for error coverage only.
                        (None, Some(else_blk)) => {
                            let _ = self.check_block(else_blk, None);
                        }
                        (None, None) => {}
                    }
                    // Call bind_pattern for error-recovery so payload binders
                    // exist and subsequent uses don't cascade into
                    // UnresolvedSymbol. Binders are defined into the CURRENT
                    // (enclosing) scope, so a let-else `let Ok(n) = … else { … };`
                    // makes `n` visible in the rest of the enclosing block.
                    //
                    // A unit-variant identifier (`None`, `E::A`) binds nothing —
                    // it is a refutable tag-test. Skip bind_pattern so it does
                    // not introduce a phantom binding (which would otherwise warn
                    // "unused variable `None`" and shadow the variant constructor).
                    if !identifier_is_unit_variant {
                        self.bind_pattern(&pattern.0, &val_ty, false, &pattern.1);
                    }
                }
            }
            Stmt::Var { name, ty, value } => {
                let binding_context = format!("local binding `{name}`");
                let deferred_hole_mark = self.deferred_inference_holes.len();
                let deferred_cast_mark = self.deferred_cast_checks.len();
                let val_ty = if let Some((val, vs)) = value {
                    if let Some(annotation) = ty {
                        let expected =
                            self.resolve_annotation_with_holes(annotation, binding_context.clone());
                        self.check_against(val, vs, &expected)
                    } else {
                        self.synthesize(val, vs)
                    }
                } else if let Some(annotation) = ty {
                    self.resolve_annotation_with_holes(annotation, binding_context.clone())
                } else {
                    let v = TypeVar::fresh();
                    Ty::Var(v)
                };
                let generic_sig = self.last_lambda_generic_sig.take();
                let val_ty = if ty.is_none() {
                    self.infer_integer_literal_binding_type(value.as_ref(), val_ty)
                        .materialize_literal_defaults()
                } else {
                    val_ty
                };
                if let Some((_, vs)) = value {
                    self.record_type(vs, &val_ty);
                }
                let value_is_direct_generic_lambda = value.as_ref().is_some_and(|(val, _)| {
                    matches!(
                        val,
                        Expr::Lambda {
                            type_params: Some(tps),
                            ..
                        } if !tps.is_empty()
                    )
                });
                if ty.is_none() && !value_is_direct_generic_lambda {
                    let more_specific_hole_vars: Vec<_> = self.deferred_inference_holes
                        [deferred_hole_mark..]
                        .iter()
                        .flat_map(|hole| hole.hole_vars.iter().copied())
                        .chain(
                            self.deferred_cast_checks[deferred_cast_mark..]
                                .iter()
                                .flat_map(|check| check.target_hole_vars.iter().copied()),
                        )
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .collect();
                    self.record_deferred_monomorphic_site(
                        span,
                        &binding_context,
                        &val_ty,
                        more_specific_hole_vars,
                    );
                }
                self.check_shadowing(name, span);
                self.env
                    .define_with_span(name.clone(), val_ty, true, span.clone());
                if value_is_direct_generic_lambda {
                    if let Some(sig) = generic_sig {
                        self.lambda_poly_sig_map
                            .insert(SpanKey::in_module(span, self.current_module_idx), sig);
                    }
                }
            }
            Stmt::Assign { target, op, value } => {
                // Classify the assignment target for the side-table before synthesising
                // so that the entry is always emitted whenever the target is syntactically
                // valid, regardless of whether subsequent type-checking finds errors.
                let assign_target_kind: Option<AssignTargetKind> = match &target.0 {
                    Expr::Identifier(name) => {
                        if self.current_actor_fields.iter().any(|f| &f.name == name) {
                            Some(AssignTargetKind::ActorField)
                        } else if self.env.lookup_ref(name).is_some() {
                            Some(AssignTargetKind::LocalVar)
                        } else {
                            None
                        }
                    }
                    Expr::FieldAccess { .. } => Some(AssignTargetKind::FieldAccess),
                    Expr::Index { .. } => Some(AssignTargetKind::Index),
                    _ => {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            "invalid assignment target; expected identifier, field access, or \
                             index"
                                .to_string(),
                        );
                        None
                    }
                };
                if let Some(kind) = assign_target_kind {
                    self.assign_target_kinds
                        .insert(SpanKey::in_module(&target.1, self.current_module_idx), kind);
                }

                // Record fields follow the same write rule as other aggregate
                // fields: immutable roots (`let r`, parameters) reject at the
                // root mutability check below; mutable roots (`var r`) may be
                // updated in place.  Keep the record-specific diagnostic for
                // roots that are known immutable so users see the value-type
                // rule, not just a generic binding error.
                if let Expr::FieldAccess { object, field } = &target.0 {
                    let obj_ty = self.synthesize(&object.0, &object.1);
                    let resolved = self.subst.resolve(&obj_ty);
                    if let Ty::Named { name, .. } = &resolved {
                        let root_is_mutable = Self::assignment_root_binding_name(&target.0)
                            .is_some_and(|root| {
                                self.current_actor_fields.iter().any(|f| f.name == root)
                                    || self
                                        .env
                                        .lookup_ref(root)
                                        .is_some_and(|binding| binding.is_mutable)
                            });
                        if !root_is_mutable
                            && self
                                .lookup_type_def(name)
                                .is_some_and(|td| td.kind == TypeDefKind::Record)
                        {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                span,
                                format!(
                                    "cannot assign to field `{field}` of record `{name}` through \
                                    an immutable binding; declare the binding mutable or use \
                                    functional update syntax `{name} {{ {field}: <value>, ..old }}` \
                                    instead"
                                ),
                            );
                        }
                    }
                }

                // Index targets (`obj[k] = rhs`) synthesise in assignment
                // context so the target type is the element/value type the RHS
                // must match — for `HashMap<K, V>` this is the bare `V` (a read
                // would instead yield `Option<V>`), and the checker records the
                // `hew_hashmap_insert_layout` runtime call at the index span.
                let target_ty = match &target.0 {
                    Expr::Index { object, index } => {
                        let ty = self.synthesize_index(
                            object,
                            index,
                            &target.1,
                            IndexContext::AssignTarget,
                        );
                        // `synthesize_index` is called directly here (not via the
                        // `synthesize` dispatch tail), so stamp `expr_types` for
                        // the target span ourselves — downstream HIR/MIR read the
                        // checker-authoritative type at this site.
                        self.record_type(&target.1, &ty);
                        ty
                    }
                    _ => self.synthesize(&target.0, &target.1),
                };
                // Record the type-shape metadata for every accepted target
                // immediately after synthesising the target type so the codegen
                // compound-assignment paths can read signedness without
                // falling back to the unreliable `resolvedTypeOf` path.
                if self
                    .assign_target_kinds
                    .contains_key(&SpanKey::in_module(&target.1, self.current_module_idx))
                {
                    let shape = AssignTargetShape {
                        is_unsigned: target_ty.is_unsigned(),
                    };
                    self.assign_target_shapes.insert(
                        SpanKey::in_module(&target.1, self.current_module_idx),
                        shape,
                    );
                }
                let root_binding_name = match &target.0 {
                    Expr::Identifier(_) | Expr::FieldAccess { .. } | Expr::Index { .. } => {
                        Self::assignment_root_binding_name(&target.0)
                    }
                    _ => None,
                };
                if let Some(name) = root_binding_name {
                    if let Some(binding) = self.env.lookup_ref(name) {
                        if !binding.is_mutable {
                            // Actor state fields get a field-specific
                            // diagnostic pointing at the declaration site;
                            // plain locals keep the variable-shaped error.
                            // In `init { }` fields are bound writable, so
                            // this arm only fires in handler/method/hook
                            // bodies.
                            if let Some(field) =
                                self.current_actor_fields.iter().find(|f| f.name == *name)
                            {
                                self.errors.push(TypeError::immutable_field_assignment(
                                    span.clone(),
                                    name,
                                    field.decl_span.clone(),
                                ));
                            } else {
                                self.errors
                                    .push(TypeError::mutability_error(span.clone(), name));
                            }
                        }
                    }
                    // Plain assignment (=) is a write-only, not a read.
                    // Compound assignment (+=, etc.) is both read and write.
                    // Must unmark BEFORE mark_written so the guard check works.
                    if op.is_none() {
                        self.env.unmark_used(name);
                    }
                    self.env.mark_written(name);
                }
                self.check_against(&value.0, &value.1, &target_ty);
            }
            Stmt::Expression((expr, es)) => {
                if let Expr::MethodCall {
                    receiver,
                    method,
                    args: _,
                } = expr
                {
                    if method == "set" {
                        if let Some(name) = Self::assignment_root_binding_name(&receiver.0) {
                            self.env.mark_written(name);
                        }
                    }
                }
                self.synthesize(expr, es);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                self.check_block(then_block, None);
                if let Some(eb) = else_block {
                    if let Some(ref if_stmt) = eb.if_stmt {
                        self.check_stmt(&if_stmt.0, &if_stmt.1);
                    } else if let Some(block) = &eb.block {
                        self.check_block(block, None);
                    }
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return;
                }
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                // Record the pattern resolution so HIR lowering can consume
                // the same `pattern_resolutions` side-table that powers
                // `WhileLet` and `Match` lowering — without this entry HIR
                // cannot resolve the constructor's `(type_name, variant_name)`
                // identity or payload-binding field indices for `if-let`.
                self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
                self.check_block(body, None);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    self.check_block(block, None);
                }
            }
            Stmt::Return(value) => {
                // Fail-closed crash-hook gate inventory (all positions covered by
                // the shared `check_return_operand` shell):
                //   (1) non-final `return CrashAction`  → THIS site
                //   (2) final/tail `return CrashAction` → check_stmt_as_expr site
                //   (3) tail-expr CrashAction (no return keyword) → items.rs body_is_crash_action
                //   (4) if/match expr whose arms all yield CrashAction → flows into (3)
                //   (5) let-bound CrashAction, then returned → flows into (1) or (2)
                self.check_return_operand(value.as_ref(), span);
            }
            Stmt::Loop { label, body } => {
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.check_block(body, None);
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
            }
            Stmt::For {
                label,
                pattern,
                iterable,
                body,
                is_await,
            } => {
                let iter_ty = self.synthesize(&iterable.0, &iterable.1);
                // Infer element type from iterable, and enforce `for await` restrictions.
                let elem_ty = match &iter_ty {
                    Ty::Array(inner, _) | Ty::Slice(inner) => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over an Array or Slice; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        (**inner).clone()
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Range),
                        args,
                        ..
                    } if args.len() == 1 => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a Range; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        args[0].clone()
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Stream),
                        args,
                        ..
                    } => {
                        let inner_opt = args.first().cloned();
                        if *is_await {
                            if args.is_empty() {
                                self.report_error(
                                    TypeErrorKind::InvalidOperation,
                                    &iterable.1,
                                    "`for await` over a stream requires a resolved element type"
                                        .to_string(),
                                );
                                Ty::Error
                            } else if let Some(method_name) =
                                self.for_await_actor_method_name(&iterable.0)
                            {
                                // SAFETY: args is non-empty (checked above)
                                let inner = inner_opt.unwrap();
                                if self.receive_generator_methods.contains(&method_name) {
                                    let resolved_inner = self.subst.resolve(&inner);
                                    if resolved_inner.has_inference_var() {
                                        self.report_error(
                                            TypeErrorKind::InvalidOperation,
                                            &iterable.1,
                                            "`for await` over a generator receive fn requires a resolved element type"
                                                .to_string(),
                                        );
                                        Ty::Error
                                    } else {
                                        resolved_inner
                                    }
                                } else {
                                    self.report_error(
                                        TypeErrorKind::InvalidOperation,
                                        &iterable.1,
                                        format!(
                                            "`for await` over actor method `{method_name}` requires a `receive gen fn`"
                                        ),
                                    );
                                    Ty::Error
                                }
                            } else {
                                match self.validate_stream_sink_element_type(
                                    args,
                                    BuiltinNamedType::Stream.canonical_name(),
                                    "next",
                                    &iterable.1,
                                ) {
                                    Some(validated_inner) => {
                                        // Stream runtime is native-only in v0.5. Method-call
                                        // `.recv()` already rejects on wasm; `for await` must
                                        // mirror that checker gate before HIR desugars it.
                                        // WASM-TODO(#1451): port stream suspend substrate.
                                        self.reject_wasm_feature(
                                            &iterable.1,
                                            WasmUnsupportedFeature::Streams,
                                        );
                                        let resolved = self.subst.resolve(&validated_inner);
                                        if !matches!(resolved, Ty::Var(_))
                                            && !self.queue_elem_admissible(&resolved)
                                        {
                                            let reason =
                                                self.queue_elem_rejection_reason(&resolved);
                                            self.report_error(
                                                TypeErrorKind::InvalidOperation,
                                                &iterable.1,
                                                format!(
                                                    "`Stream<{}>` is not supported in \
                                                     `for await`: {reason}",
                                                    validated_inner.user_facing()
                                                ),
                                            );
                                            Ty::Error
                                        } else {
                                            validated_inner
                                        }
                                    }
                                    None => Ty::Error,
                                }
                            }
                        } else if let Some(inner) = inner_opt {
                            inner
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for` over a Stream requires a resolved element type".to_string(),
                            );
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Vec),
                        args,
                        ..
                    } => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a Vec; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        if let Some(elem) = args.first().cloned() {
                            elem
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for` over a Vec requires a resolved element type".to_string(),
                            );
                            Ty::Error
                        }
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::HashMap),
                        args,
                        ..
                    } if args.len() >= 2 => {
                        if *is_await {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "`for await` is not valid over a HashMap; \
                                 use a plain `for` loop"
                                    .to_string(),
                            );
                        }
                        Ty::Tuple(vec![args[0].clone(), args[1].clone()])
                    }
                    Ty::Named {
                        builtin: Some(BuiltinType::Generator | BuiltinType::AsyncGenerator),
                        args,
                        ..
                    } if !args.is_empty() => args[0].clone(),
                    Ty::Named {
                        builtin: Some(BuiltinType::Receiver),
                        args,
                        ..
                    } if !args.is_empty() => {
                        let inner = args[0].clone();
                        if *is_await {
                            self.check_receiver_element_type_for_await(&inner, &iterable.1);
                        }
                        inner
                    }
                    // Propagate already-errored or divergent iterable expressions
                    // without adding a redundant "type is not iterable" diagnostic.
                    Ty::Error => Ty::Error,
                    Ty::Never => Ty::Never,
                    _ => {
                        if let Some(item_ty) = self.iterator_trait_item_ty(&iter_ty, &iterable.1) {
                            item_ty
                        } else {
                            self.report_error(
                                TypeErrorKind::InvalidOperation,
                                &iterable.1,
                                "type is not iterable".to_string(),
                            );
                            Ty::Error
                        }
                    }
                };
                self.env.push_scope();
                self.in_for_binding = true;
                self.bind_pattern(&pattern.0, &elem_ty, true, &pattern.1);
                self.in_for_binding = false;
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.check_block(body, None);
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
                self.env.pop_scope();
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                // Detect `while true` — suggest `loop` instead
                if matches!(&condition.0, Expr::Literal(Literal::Bool(true))) {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::StyleSuggestion,
                        span: span.clone(),
                        message: "`while true` can be simplified".to_string(),
                        notes: vec![],
                        suggestions: vec![
                            "use `loop { ... }` instead of `while true { ... }`".to_string()
                        ],
                        source_module: self.current_module.clone(),
                    });
                }
                self.check_against(&condition.0, &condition.1, &Ty::Bool);
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.check_block(body, None);
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
            }
            Stmt::WhileLet {
                label,
                pattern,
                expr,
                body,
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return;
                }
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                // Record the pattern resolution so HIR lowering can consume
                // the same `pattern_resolutions` side-table that powers
                // `Match` lowering — without this entry HIR cannot resolve
                // the constructor's `(type_name, variant_name)` identity or
                // payload-binding field indices for `while-let`.
                self.record_arm_resolution(&pattern.0, &pattern.1, &scr_ty);
                if let Some(lbl) = label {
                    self.loop_labels.push(lbl.clone());
                }
                self.loop_depth += 1;
                self.check_block(body, None);
                self.loop_depth -= 1;
                if label.is_some() {
                    self.loop_labels.pop();
                }
                self.env.pop_scope();
            }
            Stmt::Break { label, value } => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        "break used outside of a loop",
                    ));
                } else if let Some(lbl) = label {
                    if !self.loop_labels.contains(lbl) {
                        self.errors.push(TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            span.clone(),
                            format!("unknown loop label `@{lbl}`"),
                        ));
                    }
                }
                if let Some((val_expr, val_span)) = value {
                    self.synthesize(val_expr, val_span);
                }
            }
            Stmt::Continue { label } => {
                if self.loop_depth == 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        "continue used outside of a loop",
                    ));
                } else if let Some(lbl) = label {
                    if !self.loop_labels.contains(lbl) {
                        self.errors.push(TypeError::new(
                            TypeErrorKind::InvalidOperation,
                            span.clone(),
                            format!("unknown loop label `@{lbl}`"),
                        ));
                    }
                }
            }
            Stmt::Match { scrutinee, arms } => {
                let scr_ty = self.synthesize(&scrutinee.0, &scrutinee.1);
                self.check_match_stmt(&scr_ty, arms, span);
            }
            Stmt::Defer(expr) => {
                self.synthesize(&expr.0, &expr.1);
            }
        }
    }

    pub(super) fn check_match_stmt(&mut self, scrutinee_ty: &Ty, arms: &[MatchArm], span: &Span) {
        for arm in arms {
            self.env.push_scope();
            self.bind_pattern(&arm.pattern.0, scrutinee_ty, false, &arm.pattern.1);
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            self.synthesize(&arm.body.0, &arm.body.1);
            self.env.pop_scope();
        }

        self.check_exhaustiveness(scrutinee_ty, arms, span);
    }
}
