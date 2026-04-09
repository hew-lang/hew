use super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    /// Synthesize: infer the type of an expression (bottom-up).
    pub(super) fn synthesize(&mut self, expr: &Expr, span: &Span) -> Ty {
        // Grow the stack on demand so deeply-nested expressions (e.g. 1000+
        // chained binary operators) don't overflow.
        stacker::maybe_grow(32 * 1024, 2 * 1024 * 1024, || {
            self.synthesize_inner(expr, span)
        })
    }

    pub(super) fn maybe_warn_wasm_expr(&mut self, expr: &Expr, span: &Span) {
        if !self.wasm_target {
            return;
        }
        match expr {
            Expr::Scope { .. } | Expr::Join(_) => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::StructuredConcurrency);
            }
            Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) => {
                self.warn_wasm_limitation(span, WasmUnsupportedFeature::Tasks);
            }
            Expr::Select { .. } => {
                let supports_single_arm_timeout = matches!(
                    expr,
                    Expr::Select { arms, timeout: Some(timeout) }
                        if arms.len() == 1
                            && matches!(timeout.duration.0, Expr::Literal(Literal::Duration(_)))
                );
                if !supports_single_arm_timeout {
                    self.warn_wasm_limitation(span, WasmUnsupportedFeature::Select);
                }
            }
            _ => {}
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression check covers all AST variants"
    )]
    pub(super) fn synthesize_inner(&mut self, expr: &Expr, span: &Span) -> Ty {
        self.maybe_warn_wasm_expr(expr, span);
        let ty = match expr {
            // Literals
            Expr::Literal(Literal::Float(_)) => Ty::FloatLiteral,
            Expr::Literal(Literal::String(_)) => Ty::String,
            Expr::RegexLiteral(_) => {
                // The implicit `use std::text::regex` injected by the CLI is the
                // provider of this type; mark it as used so the unused-import
                // check doesn't fire a false-positive warning.
                self.used_modules
                    .borrow_mut()
                    .insert(ImportKey::new(self.current_module.clone(), "regex"));
                Ty::Named {
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
                let tys: Vec<_> = elems.iter().map(|(e, s)| self.synthesize(e, s)).collect();
                Ty::Tuple(tys)
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
            Expr::StructInit { name, fields } => self.check_struct_init(name, fields, span),

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
                is_move: _,
                type_params,
                params,
                return_type,
                body,
            } => self.check_lambda(
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
                    Ty::Named { name, args } if name == "Task" && !args.is_empty() => {
                        args[0].clone()
                    }
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
                let bad_ctx_msg: Option<String> =
                    self.current_return_type.as_ref().and_then(|ret| {
                        let r = self.subst.resolve(ret);
                        if r.as_option().is_some()
                            || r.as_result().is_some()
                            || matches!(r, Ty::Var(_) | Ty::Error)
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

            // Send: target <- message
            Expr::Send { target, message } => self.synthesize_send(target, message, span),

            // Yield
            Expr::Yield(value) => self.synthesize_yield(value.as_deref(), span),

            // Cooperate
            Expr::Cooperate => Ty::Unit,

            // Actor self-reference handle — returns ActorRef<Self>, not the actor type itself
            Expr::This => {
                if let Some(actor_ty) = &self.current_actor_type {
                    Ty::actor_ref(actor_ty.clone())
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

    pub(super) fn synthesize_send(
        &mut self,
        target: &Spanned<Expr>,
        message: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let _target_ty = self.synthesize(&target.0, &target.1);
        let msg_ty_raw = self.synthesize(&message.0, &message.1);
        self.enforce_actor_boundary_send(&message.0, &message.1, span, &msg_ty_raw);
        Ty::Unit
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
        self.mark_expr_moved_if_non_copy(expr, move_span, &ty);
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
        if let Some((depth, binding)) = self.env.lookup_with_depth(name) {
            let is_moved = binding.is_moved;
            let ty = binding.ty.clone();
            if is_moved {
                self.report_error(
                    TypeErrorKind::UseAfterMove,
                    span,
                    format!("use of moved value `{name}`"),
                );
            }
            // Track captures: variable from scope below the lambda boundary
            if let Some(capture_depth) = self.lambda_capture_depth {
                if depth < capture_depth {
                    self.lambda_captures.push(ty.clone());
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
                            let ty = Ty::normalize_named(type_prefix.to_string(), vec![]);
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

    pub(super) fn synthesize_index(
        &mut self,
        object: &Spanned<Expr>,
        index: &Spanned<Expr>,
        span: &Span,
    ) -> Ty {
        let obj_ty = self.synthesize(&object.0, &object.1);
        self.check_against(&index.0, &index.1, &Ty::I64);
        match &obj_ty {
            Ty::Array(elem, _) | Ty::Slice(elem) => (**elem).clone(),
            Ty::Named { name, args } if name == "Vec" && !args.is_empty() => args[0].clone(),
            // Custom type indexing: desugar obj[key] → obj.get(key)
            Ty::Named { name, args } => {
                if let Some(sig) = self.lookup_named_method_sig(name, args, "get") {
                    if let Some(param_ty) = sig.params.first() {
                        self.check_against(&index.0, &index.1, param_ty);
                    }
                    sig.return_type
                } else if self.lookup_type_def(name).is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!(
                            "cannot index into `{}`: type has no `get` method",
                            obj_ty.user_facing()
                        ),
                    );
                    Ty::Error
                } else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("cannot index into `{}`", obj_ty.user_facing()),
                    );
                    Ty::Error
                }
            }
            _ => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!("cannot index into `{}`", obj_ty.user_facing()),
                );
                Ty::Error
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "concurrency variants (scope/select/join/spawn/unsafe/timeout) with purity checks"
    )]
    pub(super) fn synthesize_concurrency(&mut self, expr: &Expr, span: &Span) -> Ty {
        if self.in_pure_function {
            match expr {
                Expr::Scope { .. } | Expr::ScopeLaunch(_) | Expr::ScopeSpawn(_) => {
                    self.report_error(
                        TypeErrorKind::PurityViolation,
                        span,
                        "cannot use `scope` in a pure function".to_string(),
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
            Expr::SpawnLambdaActor {
                params,
                return_type,
                body,
                ..
            } => {
                let lambda_ty =
                    self.check_lambda(None, params, return_type.as_ref(), body, None, span);
                if let Ty::Closure { captures, .. } = lambda_ty {
                    let mut non_send_captures = HashSet::new();
                    for capture in captures {
                        if !self.registry.implements_marker(&capture, MarkerTrait::Send)
                            && non_send_captures.insert(capture.clone())
                        {
                            self.report_error(
                                TypeErrorKind::InvalidSend,
                                span,
                                format!(
                                    "cannot capture `{}` in spawned actor: type is not Send",
                                    capture.user_facing()
                                ),
                            );
                        }
                    }
                }
                let param_ty = params
                    .first()
                    .and_then(|p| p.ty.as_ref())
                    .map_or(Ty::Var(TypeVar::fresh()), |annotation| {
                        self.resolve_type_expr(annotation)
                    });
                Ty::Named {
                    name: "Actor".to_string(),
                    args: vec![param_ty],
                }
            }
            Expr::Scope { body: block, .. } => self.check_block(block, None),
            Expr::Unsafe(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block(block, None);
                self.in_unsafe = prev;
                ty
            }
            Expr::ScopeLaunch(block) | Expr::ScopeSpawn(block) => {
                let body_ty = self.check_block(block, None);
                Ty::Named {
                    name: "Task".to_string(),
                    args: vec![body_ty],
                }
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
                    type_params,
                    params,
                    return_type,
                    body,
                    ..
                },
                Ty::Function {
                    params: expected_params,
                    ret,
                },
            ) => {
                let result = self.check_lambda(
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
                Ty::Named { name, args },
            ) if name == "Range"
                && args.len() == 1
                && !matches!(&args[0], Ty::Error | Ty::Var(_)) =>
            {
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
            (Expr::Array(elems), Ty::Named { name, args }) if name == "Vec" => {
                let elem_ty = args.first().cloned().unwrap_or(Ty::Var(TypeVar::fresh()));
                for elem in elems {
                    let (expr, sp) = (&elem.0, &elem.1);
                    self.check_against(expr, sp, &elem_ty);
                }
                self.record_type(span, expected);
                expected.clone()
            }

            // Map literal can coerce to HashMap<K,V> when expected
            (Expr::MapLiteral { entries }, Ty::Named { name, args }) if name == "HashMap" => {
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
            (Expr::Block(block), Ty::Named { name, .. })
                if name == "HashMap" && block.stmts.is_empty() && block.trailing_expr.is_none() =>
            {
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
                Expr::StructInit { name, fields },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                },
            ) if name == expected_name => {
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
                                        self.substitute_named_param(&field_expected, tp, concrete);
                                }
                                let actual = self.check_against(fexpr, fs, &field_expected);

                                // Still infer any remaining unbound type params
                                for tp in &td.type_params {
                                    if !type_arg_map.contains_key(tp)
                                        && *declared_ty
                                            == (Ty::Named {
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
            BinaryOp::Send => {
                // target <- message
                Ty::Unit
            }
            BinaryOp::RegexMatch | BinaryOp::RegexNotMatch => {
                if !matches!(left_ty, Ty::String) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &left.1,
                        format!(
                            "left side of regex match must be string, found `{}`",
                            left_ty.user_facing()
                        ),
                    );
                }
                if !matches!(&right_ty, Ty::Named { name, .. } if name == "regex.Pattern") {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &right.1,
                        format!(
                            "right side of regex match must be regex, found `{}`",
                            right_ty.user_facing()
                        ),
                    );
                }
                Ty::Bool
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
        let dangerous_params: Vec<(&str, &str)> = fd
            .params
            .iter()
            .filter_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                if matches!(ty, Ty::Named { ref name, .. } if name == "Rc") {
                    return Some((p.name.as_str(), "Rc"));
                }
                None
            })
            .collect();
        if dangerous_params.is_empty() {
            return;
        }

        let param_names: Vec<&str> = dangerous_params.iter().map(|(n, _)| *n).collect();
        let param_tags: HashMap<&str, &str> = dangerous_params.iter().copied().collect();

        // Collect locals tainted by storing a dangerous param via method
        // calls (v.push(r)), direct aliasing (let v = r), or aggregate
        // wrapping (let v = Some(r)).  Taint propagates forward so that
        // `let a = r; v.push(a);` taints both `a` and `v`.
        let mut tainted: HashMap<String, (String, String)> = HashMap::new();
        Self::collect_tainted_locals(&fd.body.stmts, &param_names, &param_tags, &mut tainted);

        // Build extended name/tag sets including tainted locals so the
        // return-position checker also flags `return v` when v is tainted.
        let tainted_tag_storage: Vec<(String, String)> = tainted
            .iter()
            .map(|(local, (source, tag))| (local.clone(), format!("tainted:{source}:{tag}")))
            .collect();
        let mut all_names: Vec<&str> = param_names.clone();
        let mut all_tags: HashMap<&str, &str> = param_tags.clone();
        for (name, tag) in &tainted_tag_storage {
            all_names.push(name.as_str());
            all_tags.insert(name.as_str(), tag.as_str());
        }

        // Check trailing expression (implicit return).
        if let Some(trailing) = &fd.body.trailing_expr {
            self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, &all_names, &all_tags);
        }

        // Scan statements for explicit `return <ident>` / `break <ident>`.
        self.scan_stmts_for_rc_param_return(&fd.body.stmts, &all_names, &all_tags);
    }

    /// If `expr` is a bare identifier matching one of `rc_params` (or a block
    /// expression whose trailing expression is), emit a fail-closed error.
    ///
    /// `param_tags` maps param name → tag string (`"Rc"` for explicit Rc params,
    /// or `"tainted:<source>:<tag>"` for locals tainted by storing an Rc param).
    pub(super) fn check_expr_is_rc_param_return(
        &mut self,
        expr: &Expr,
        span: &Span,
        rc_params: &[&str],
        param_tags: &HashMap<&str, &str>,
    ) {
        match expr {
            Expr::Identifier(name) if rc_params.contains(&name.as_str()) => {
                let tag = param_tags.get(name.as_str()).copied().unwrap_or("Rc");
                let (message, note, suggestion) = if let Some(rest) = tag.strip_prefix("tainted:") {
                    // Tainted local: tag = "tainted:<source_param>:Rc"
                    let (source_param, _source_tag) = rest.split_once(':').unwrap_or((rest, "Rc"));
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
            // Descend into block expressions: `{ r }` wraps the identifier
            // in an Expr::Block whose trailing_expr carries the real value.
            Expr::Block(blk) => {
                if let Some(trailing) = &blk.trailing_expr {
                    self.check_expr_is_rc_param_return(
                        &trailing.0,
                        &trailing.1,
                        rc_params,
                        param_tags,
                    );
                }
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
                    self.check_expr_is_rc_param_return(e, s, rc_params, param_tags);
                }
            }
            // Tuple literals: (r, 0), (r,) embed the borrowed Rc param.
            Expr::Tuple(elems) => {
                for (e, s) in elems {
                    self.check_expr_is_rc_param_return(e, s, rc_params, param_tags);
                }
            }
            // Struct initializers: MyStruct { field: r } embeds the borrowed Rc param.
            Expr::StructInit { fields, .. } => {
                for (_field_name, (e, s)) in fields {
                    self.check_expr_is_rc_param_return(e, s, rc_params, param_tags);
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

    /// Check if `expr` directly names or structurally contains an identifier
    /// in `dangerous`.  Returns the first match or `None`.
    ///
    /// Structural descent mirrors `check_expr_is_rc_param_return`: constructors
    /// (uppercase-initial calls), tuples, struct inits, and blocks are
    /// containers that embed the value.  Regular lowercase function/method
    /// calls are borrows under call-boundary ownership — the return value is
    /// unrelated, so we do NOT recurse into those.
    pub(super) fn expr_mentions_dangerous_param(
        expr: &Expr,
        dangerous: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::Identifier(name) if dangerous.contains(name) => Some(name.clone()),
            Expr::Call { function, args, .. } if Self::looks_like_constructor(&function.0) => {
                for arg in args {
                    let (e, _) = arg.expr();
                    if let Some(hit) = Self::expr_mentions_dangerous_param(e, dangerous) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Tuple(elems) => {
                for (e, _) in elems {
                    if let Some(hit) = Self::expr_mentions_dangerous_param(e, dangerous) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::StructInit { fields, .. } => {
                for (_, (e, _)) in fields {
                    if let Some(hit) = Self::expr_mentions_dangerous_param(e, dangerous) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Block(blk) => blk
                .trailing_expr
                .as_deref()
                .and_then(|(e, _)| Self::expr_mentions_dangerous_param(e, dangerous)),
            _ => None,
        }
    }

    /// Forward-scan statements to find local variables tainted by storing a
    /// dangerous Rc parameter.
    ///
    /// A local is "tainted" when:
    /// 1. Direct alias: `let v = r;`
    /// 2. Aggregate wrap: `let v = Some(r);`
    /// 3. Method-call store: `v.push(r);`
    ///
    /// Taint propagates forward: `let a = r; v.push(a);` taints both `a`
    /// and `v` because `a` enters the dangerous set before the `push` is
    /// processed.  Control-flow bodies are scanned unconditionally
    /// (fail-closed: if a dangerous param is stored inside a branch, the
    /// local is tainted regardless of the branch condition).
    #[allow(
        clippy::too_many_lines,
        reason = "forward taint tracks many statement kinds"
    )]
    pub(super) fn collect_tainted_locals(
        stmts: &[Spanned<Stmt>],
        rc_params: &[&str],
        param_tags: &HashMap<&str, &str>,
        tainted: &mut HashMap<String, (String, String)>,
    ) {
        for (stmt, _span) in stmts {
            // Rebuild the dangerous set each iteration to include newly
            // tainted locals.  Uses owned Strings to avoid borrow conflicts
            // when inserting into `tainted` below.
            let dangerous: HashSet<String> = rc_params
                .iter()
                .map(|s| (*s).to_string())
                .chain(tainted.keys().cloned())
                .collect();

            match stmt {
                Stmt::Let {
                    pattern: (Pattern::Identifier(name), _),
                    value: Some((expr, _)),
                    ..
                }
                | Stmt::Var {
                    name,
                    value: Some((expr, _)),
                    ..
                } => {
                    if let Some(source) = Self::expr_mentions_dangerous_param(expr, &dangerous) {
                        let resolved = Self::resolve_taint_source(&source, param_tags, tainted);
                        tainted.insert(name.clone(), resolved);
                    }
                }
                Stmt::Assign {
                    target: (Expr::Identifier(target_name), _),
                    value: (expr, _),
                    ..
                } => {
                    if let Some(source) = Self::expr_mentions_dangerous_param(expr, &dangerous) {
                        let resolved = Self::resolve_taint_source(&source, param_tags, tainted);
                        tainted.insert(target_name.clone(), resolved);
                    }
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
                    // Only taint the receiver for methods that actually store
                    // the argument.  Read-only methods (contains, index, len,
                    // etc.) borrow the arg and return independently.
                    const STORING_METHODS: &[&str] = &["push", "set", "insert", "extend", "append"];
                    if STORING_METHODS.contains(&method.as_str()) {
                        if let Expr::Identifier(recv_name) = &receiver.0 {
                            for arg in args {
                                let (e, _) = arg.expr();
                                if let Some(source) =
                                    Self::expr_mentions_dangerous_param(e, &dangerous)
                                {
                                    let resolved =
                                        Self::resolve_taint_source(&source, param_tags, tainted);
                                    tainted.insert(recv_name.clone(), resolved);
                                    break;
                                }
                            }
                        }
                    }
                }
                // Field-assignment escape: `s.field = r;` stores a dangerous
                // param into a struct, tainting the struct variable.
                Stmt::Assign {
                    target: (Expr::FieldAccess { object, .. }, _),
                    value: (expr, _),
                    ..
                } => {
                    if let Expr::Identifier(obj_name) = &object.0 {
                        if let Some(source) = Self::expr_mentions_dangerous_param(expr, &dangerous)
                        {
                            let resolved = Self::resolve_taint_source(&source, param_tags, tainted);
                            tainted.insert(obj_name.clone(), resolved);
                        }
                    }
                }
                // Recurse into control flow — fail-closed: if a dangerous
                // param is stored inside a branch, the local is tainted
                // unconditionally.
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    Self::collect_tainted_locals(&then_block.stmts, rc_params, param_tags, tainted);
                    if let Some(else_blk) = else_block {
                        if let Some(if_stmt) = &else_blk.if_stmt {
                            Self::collect_tainted_locals(
                                std::slice::from_ref(if_stmt.as_ref()),
                                rc_params,
                                param_tags,
                                tainted,
                            );
                        }
                        if let Some(blk) = &else_blk.block {
                            Self::collect_tainted_locals(
                                &blk.stmts, rc_params, param_tags, tainted,
                            );
                        }
                    }
                }
                Stmt::IfLet {
                    body, else_body, ..
                } => {
                    Self::collect_tainted_locals(&body.stmts, rc_params, param_tags, tainted);
                    if let Some(else_blk) = else_body {
                        Self::collect_tainted_locals(
                            &else_blk.stmts,
                            rc_params,
                            param_tags,
                            tainted,
                        );
                    }
                }
                Stmt::For { body, .. }
                | Stmt::While { body, .. }
                | Stmt::WhileLet { body, .. }
                | Stmt::Loop { body, .. } => {
                    Self::collect_tainted_locals(&body.stmts, rc_params, param_tags, tainted);
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        if let Expr::Block(blk) = &arm.body.0 {
                            Self::collect_tainted_locals(
                                &blk.stmts, rc_params, param_tags, tainted,
                            );
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Trace a taint source back to the original parameter.  If `source` is
    /// itself a tainted local, follow the chain to the root param.
    pub(super) fn resolve_taint_source(
        source: &str,
        param_tags: &HashMap<&str, &str>,
        tainted: &HashMap<String, (String, String)>,
    ) -> (String, String) {
        if let Some(tag) = param_tags.get(source) {
            (source.to_string(), (*tag).to_string())
        } else if let Some((orig, tag)) = tainted.get(source) {
            (orig.clone(), tag.clone())
        } else {
            (source.to_string(), "Rc".to_string())
        }
    }

    /// Recursively scan statements for `return <rc_param_ident>`,
    /// `break <rc_param_ident>`, and nested control-flow bodies.
    pub(super) fn scan_stmts_for_rc_param_return(
        &mut self,
        stmts: &[Spanned<Stmt>],
        rc_params: &[&str],
        param_tags: &HashMap<&str, &str>,
    ) {
        for (stmt, _span) in stmts {
            match stmt {
                Stmt::Return(Some((expr, es)))
                | Stmt::Break {
                    value: Some((expr, es)),
                    ..
                } => {
                    self.check_expr_is_rc_param_return(expr, es, rc_params, param_tags);
                }
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.scan_stmts_for_rc_param_return(&then_block.stmts, rc_params, param_tags);
                    if let Some(then_trailing) = &then_block.trailing_expr {
                        self.check_expr_is_rc_param_return(
                            &then_trailing.0,
                            &then_trailing.1,
                            rc_params,
                            param_tags,
                        );
                    }
                    if let Some(else_blk) = else_block {
                        if let Some(if_stmt) = &else_blk.if_stmt {
                            // else-if: recurse into the nested Stmt::If
                            self.scan_stmts_for_rc_param_return(
                                std::slice::from_ref(if_stmt.as_ref()),
                                rc_params,
                                param_tags,
                            );
                        }
                        if let Some(blk) = &else_blk.block {
                            self.scan_stmts_for_rc_param_return(&blk.stmts, rc_params, param_tags);
                            if let Some(trailing) = &blk.trailing_expr {
                                self.check_expr_is_rc_param_return(
                                    &trailing.0,
                                    &trailing.1,
                                    rc_params,
                                    param_tags,
                                );
                            }
                        }
                    }
                }
                Stmt::For { body, .. }
                | Stmt::While { body, .. }
                | Stmt::WhileLet { body, .. }
                | Stmt::Loop { body, .. } => {
                    self.scan_stmts_for_rc_param_return(&body.stmts, rc_params, param_tags);
                }
                Stmt::IfLet {
                    body, else_body, ..
                } => {
                    self.scan_stmts_for_rc_param_return(&body.stmts, rc_params, param_tags);
                    if let Some(else_blk) = else_body {
                        self.scan_stmts_for_rc_param_return(&else_blk.stmts, rc_params, param_tags);
                    }
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        // Match arm body is an Expr — check if it's a bare Rc param
                        self.check_expr_is_rc_param_return(
                            &arm.body.0,
                            &arm.body.1,
                            rc_params,
                            param_tags,
                        );
                        // If the body is a Block, recurse into its statements
                        if let Expr::Block(blk) = &arm.body.0 {
                            self.scan_stmts_for_rc_param_return(&blk.stmts, rc_params, param_tags);
                            if let Some(trailing) = &blk.trailing_expr {
                                self.check_expr_is_rc_param_return(
                                    &trailing.0,
                                    &trailing.1,
                                    rc_params,
                                    param_tags,
                                );
                            }
                        }
                    }
                }
                _ => {}
            }
        }
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
            Ty::Named { name, args } => {
                // Named supervisor child access: sup.child_name → ActorRef<ChildType>
                if let Some(Ty::Named { name: sup_name, .. }) = resolved.as_actor_ref() {
                    if let Some(children) = self.supervisor_children.get(sup_name) {
                        if let Some((_child_name, child_type)) =
                            children.iter().find(|(cn, _)| cn == field)
                        {
                            return Ty::actor_ref(Ty::Named {
                                name: child_type.clone(),
                                args: vec![],
                            });
                        }
                    }
                }
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(field_ty) = td.fields.get(field) {
                        // Substitute generic type params with concrete args
                        let mut result_ty = field_ty.clone();
                        for (param, arg) in td.type_params.iter().zip(args.iter()) {
                            result_ty = self.substitute_named_param(&result_ty, param, arg);
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
        clippy::too_many_lines,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(super) fn check_lambda(
        &mut self,
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

        // Record the scope depth BEFORE pushing the lambda scope — any variable
        // found below this depth during body checking is a capture.
        let capture_depth = self.env.depth();
        self.lambda_capture_depth = Some(capture_depth);

        // Clear any stale scratch state from a previous call in a non-let or
        // nested context.  We unconditionally reset first so that re-entrant
        // calls (e.g., a generic lambda inside a function argument) cannot
        // bleed their type-var pairs out to an unrelated enclosing Stmt::Let.
        self.last_lambda_generic_vars = None;

        let mut generic_bindings = std::collections::HashMap::new();
        let mut generic_var_pairs: Vec<(String, TypeVar)> = Vec::new();
        if let Some(tps) = type_params {
            for tp in tps {
                let tv = TypeVar::fresh();
                generic_bindings.insert(tp.name.clone(), Ty::Var(tv));
                generic_var_pairs.push((tp.name.clone(), tv));
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
            // Signal to the immediately-enclosing Stmt::Let that this lambda
            // is generic.  The field is cleared at entry above, so it is only
            // non-None when check_lambda is the *direct* synthesized value.
            self.last_lambda_generic_vars = Some(generic_var_pairs);
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
            self.check_against(&body.0, &body.1, &expected_ret);
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
        self.in_generator = prev_in_generator;
        self.env.pop_scope();

        if let Some(tps) = type_params {
            if !tps.is_empty() {
                self.generic_ctx.pop();
            }
        }

        // Collect captures and resolve their types
        let captures: Vec<Ty> = std::mem::take(&mut self.lambda_captures)
            .into_iter()
            .map(|c| self.subst.resolve(&c))
            .collect();

        // Restore outer capture tracking state
        self.lambda_capture_depth = prev_capture_depth;
        self.lambda_captures = prev_captures;

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
        span: &Span,
    ) -> Ty {
        if let Some(td) = self.lookup_type_def(name) {
            // Track inferred type arguments for generic structs
            let mut type_arg_map: HashMap<String, Ty> = HashMap::new();

            for (field_name, (expr, es)) in fields {
                if let Some(declared_ty) = td.fields.get(field_name) {
                    // Substitute already-inferred type params into the expected type
                    let mut expected = declared_ty.clone();
                    for (tp, concrete) in &type_arg_map {
                        expected = self.substitute_named_param(&expected, tp, concrete);
                    }

                    // If the expected type is still an unbound type parameter,
                    // synthesize so the field value determines the type (rather
                    // than failing with "expected T, found i64").
                    let is_unbound_param = td.type_params.iter().any(|tp| {
                        !type_arg_map.contains_key(tp)
                            && expected
                                == (Ty::Named {
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
            Ty::Named {
                name: name.to_string(),
                args: type_args,
            }
        } else if let Some((enum_name, variant_fields)) =
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
                    Some(VariantDef::Struct(fields)) => Some((type_name.clone(), fields.clone())),
                    _ => None,
                }
            })
        {
            for (field_name, (expr, es)) in fields {
                if let Some((_, declared_ty)) =
                    variant_fields.iter().find(|(name, _)| name == field_name)
                {
                    self.check_against(expr, es, declared_ty);
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
            Ty::Named {
                name: enum_name,
                args: vec![],
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
            Ty::actor_ref(Ty::Named { name, args: vec![] })
        } else {
            Ty::actor_ref(Ty::Error)
        }
    }

    /// Check if an expression is typically used for side effects (not for its return value).
    pub(super) fn record_type(&mut self, span: &Span, ty: &Ty) {
        let key = SpanKey::from(span);
        self.expr_type_source_modules
            .insert(key.clone(), self.current_module.clone());
        self.expr_types.insert(key, ty.clone());
    }
}
