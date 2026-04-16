#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::builtin_names::BuiltinNamedType;

impl Checker {
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
                if let Some(expected) = self.current_return_type.clone() {
                    // Guard: do not check against Ty::Error — it would silently
                    // suppress mismatch diagnostics in the returned expression.
                    // Synthesize the value instead so its own errors are still caught.
                    if matches!(self.subst.resolve(&expected), Ty::Error) {
                        if let Some((val, vs)) = value {
                            self.synthesize(val, vs);
                        }
                    } else {
                        match value {
                            Some((val, vs)) => {
                                self.check_against(val, vs, &expected);
                            }
                            None if expected != Ty::Unit => {
                                self.errors.push(TypeError::return_type_mismatch(
                                    span.clone(),
                                    &expected,
                                    &Ty::Unit,
                                ));
                            }
                            _ => {}
                        }
                    }
                }
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

    #[expect(
        clippy::too_many_lines,
        reason = "statement checking covers many Stmt variants"
    )]
    pub(super) fn check_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                let binding_context = match &pattern.0 {
                    Pattern::Identifier(name) => format!("local binding `{name}`"),
                    _ => "local binding".to_string(),
                };
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
                // For simple identifier patterns, track the definition span
                if let Pattern::Identifier(name) = &pattern.0 {
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
                            self.lambda_poly_sig_map
                                .insert(SpanKey::from(&pattern.1), sig);
                        }
                    }
                    // Track let-bound numeric literals for later coercion at use
                    // sites. Only unannotated immutable bindings preserve the
                    // literal kind/value; explicit annotations and mutable vars
                    // materialize immediately.
                    if ty.is_none() {
                        if let Some((val, _)) = value {
                            if val_ty.is_integer_literal() {
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
                    self.bind_pattern(&pattern.0, &val_ty, false, &pattern.1);
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
                let val_ty = if ty.is_none() {
                    val_ty.materialize_literal_defaults()
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
            }
            Stmt::Assign { target, op, value } => {
                // Purity check: pure functions cannot assign to actor fields
                if self.in_pure_function {
                    // Check bare field name assignment (actor fields in scope)
                    if let Expr::Identifier(name) = &target.0 {
                        if self.current_actor_fields.contains(name) {
                            self.report_error(
                                TypeErrorKind::PurityViolation,
                                span,
                                format!("cannot assign to actor field `{name}` in a pure function"),
                            );
                        }
                    }
                }
                // Classify the assignment target for the side-table before synthesising
                // so that the entry is always emitted whenever the target is syntactically
                // valid, regardless of whether subsequent type-checking finds errors.
                let assign_target_kind: Option<AssignTargetKind> = match &target.0 {
                    Expr::Identifier(name) => {
                        if self.current_actor_fields.contains(name) {
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
                        .insert(SpanKey::from(&target.1), kind);
                }
                let target_ty = self.synthesize(&target.0, &target.1);
                // Record the type-shape metadata for every accepted target
                // immediately after synthesising the target type so the MLIR
                // compound-assignment paths can read signedness without
                // falling back to the unreliable `resolvedTypeOf` path.
                if self
                    .assign_target_kinds
                    .contains_key(&SpanKey::from(&target.1))
                {
                    let shape = AssignTargetShape {
                        is_unsigned: target_ty.is_unsigned(),
                    };
                    self.assign_target_shapes
                        .insert(SpanKey::from(&target.1), shape);
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
                            self.errors
                                .push(TypeError::mutability_error(span.clone(), name));
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
                self.check_block(body, None);
                self.env.pop_scope();
                if let Some(block) = else_body {
                    self.check_block(block, None);
                }
            }
            Stmt::Return(value) => {
                if let Some(expected) = self.current_return_type.clone() {
                    // Guard: do not check against Ty::Error — same as in
                    // check_stmt_as_expr; synthesize instead to preserve body errors.
                    if matches!(self.subst.resolve(&expected), Ty::Error) {
                        if let Some((val, vs)) = value {
                            self.synthesize(val, vs);
                        }
                    } else {
                        match value {
                            Some((val, vs)) => {
                                self.check_against(val, vs, &expected);
                            }
                            None if expected != Ty::Unit => {
                                self.errors.push(TypeError::return_type_mismatch(
                                    span.clone(),
                                    &expected,
                                    &Ty::Unit,
                                ));
                            }
                            _ => {}
                        }
                    }
                }
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
                    Ty::Named { name, args } if name == "Range" && args.len() == 1 => {
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
                    Ty::Named { name, args }
                        if builtin_named_type(name) == Some(BuiltinNamedType::Stream) =>
                    {
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
                                    Some(validated_inner) => validated_inner,
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
                    Ty::Named { name, args } if name == "Vec" => {
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
                    Ty::Named { name, args } if name == "HashMap" && args.len() >= 2 => {
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
                    Ty::Named { name, args }
                        if (name == "Generator" && !args.is_empty())
                            || (name == "AsyncGenerator" && args.len() == 1) =>
                    {
                        args[0].clone()
                    }
                    Ty::Named { name, args }
                        if builtin_named_type(name) == Some(BuiltinNamedType::Receiver)
                            && !args.is_empty() =>
                    {
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
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &iterable.1,
                            "type is not iterable".to_string(),
                        );
                        Ty::Error
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
                pattern,
                expr,
                body,
                ..
            } => {
                let scr_ty = self.synthesize(&expr.0, &expr.1);
                if self.reject_unsupported_iflet_pattern(&pattern.0, &pattern.1) {
                    return;
                }
                self.env.push_scope();
                self.bind_pattern(&pattern.0, &scr_ty, false, &pattern.1);
                self.loop_depth += 1;
                self.check_block(body, None);
                self.loop_depth -= 1;
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

            if let Some((guard, gs)) = &arm.guard {
                self.check_against(guard, gs, &Ty::Bool);
            }

            self.synthesize(&arm.body.0, &arm.body.1);
            self.env.pop_scope();
        }

        self.check_exhaustiveness(scrutinee_ty, arms, span);
    }
}
