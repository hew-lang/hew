//! `var self` receiver return wrapping.

use super::*;

impl LowerCtx {
    /// Reduce a symbol-prefix impl self-type name to the bare AST receiver
    /// type name — may be the mangled form for concrete specialised impls
    /// (e.g. `"Wrapper$$i64"`), and must compare as `"Wrapper"` so the
    /// param-type annotation `Wrapper<i64>` still matches (#2270).
    ///
    /// The SINGLE derivation feeding [`Self::is_var_self_method_for_type`]:
    /// both the fn-registry ABI registration (dual-return tuple) and the
    /// body-lowering return wrap key off this same name reduction so the
    /// registered callee return type and the emitted body agree.
    pub(super) fn bare_impl_self_type_name(self_type_name: &str) -> &str {
        let bare = self_type_name
            .split_once("$$")
            .map_or(self_type_name, |(bare, _)| bare);
        hew_types::short_name(bare)
    }

    /// THE var-self receiver predicate. A method takes a `var self` receiver
    /// iff its first parameter is mutable and typed `Self` — or, inside an
    /// impl block, typed as the impl's own target type (the checker's
    /// `is_receiver_param` accepts both spellings, so the ABI decision here
    /// must too). Every site that decides the dual-return `(ret, Self)` ABI
    /// carrier — registration and body lowering — goes through this one
    /// function; a second predicate is how the call site and the callee
    /// disagree on the return struct (the exact fail-closed ABI mismatch a
    /// `fn next(var p: Pair<T>)` where-clause impl used to hit).
    pub(super) fn is_var_self_method_for_type(
        method: &FnDecl,
        self_type_name: Option<&str>,
    ) -> bool {
        method.params.first().is_some_and(|param| {
            param.is_mutable
                && match self_type_name {
                    Some(name) => Self::is_receiver_param_for_type(param, name),
                    None => Self::is_receiver_param(param),
                }
        })
    }

    pub(super) fn is_receiver_param_for_type(param: &Param, self_type_name: &str) -> bool {
        match &param.ty.0 {
            TypeExpr::Named { path, .. } => {
                let name = path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
                name == "Self" || name == self_type_name
            }
            _ => false,
        }
    }

    pub(super) fn is_receiver_param(param: &Param) -> bool {
        matches!(&param.ty.0, TypeExpr::Named { path, .. } if path.as_single().is_some_and(|name| name.name.as_str() == "Self"))
    }

    pub(super) fn var_self_dual_return_ty(
        result_ty: ResolvedTy,
        receiver_ty: ResolvedTy,
    ) -> ResolvedTy {
        ResolvedTy::Tuple(vec![result_ty, receiver_ty])
    }

    pub(super) fn make_var_self_return_expr(
        &mut self,
        result_expr: HirExpr,
        receiver: &HirBinding,
        abi_return_ty: &ResolvedTy,
        span: Span,
    ) -> HirExpr {
        let self_ref = HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: receiver.ty.clone(),
            intent: IntentKind::Consume,
            kind: HirExprKind::BindingRef {
                name: receiver.name.clone(),
                resolved: ResolvedRef::Binding(receiver.id),
            },
            span: receiver.span.clone(),
        };
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: abi_return_ty.clone(),
            intent: IntentKind::Read,
            kind: HirExprKind::TupleLiteral {
                elements: vec![result_expr, self_ref],
            },
            span,
        }
    }

    pub(super) fn make_unit_expr(&mut self, span: Span) -> HirExpr {
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span,
        }
    }

    /// Build the `match` that `if let` and `while let` desugar to: the success
    /// arms from `lower_pattern_arms` (one per or-pattern leaf), then a
    /// wildcard arm for the fallthrough. Everything downstream sees one
    /// ordered match instead of a second pattern form.
    pub(super) fn pattern_conditional_match(
        &mut self,
        scrutinee: HirExpr,
        mut arms: Vec<HirMatchArm>,
        fallthrough: HirExpr,
        result_ty: &ResolvedTy,
        span: &Span,
    ) -> HirExpr {
        arms.push(HirMatchArm {
            scope: None,
            predicate: HirMatchArmPredicate::Wildcard,
            bindings: Vec::new(),
            payload_predicates: Vec::new(),
            payload_variant_predicates: Vec::new(),
            guard: None,
            body: fallthrough,
            span: span.clone(),
        });
        HirExpr {
            node: self.ids.node(),
            site: self.ids.site(),
            ty: result_ty.clone(),
            intent: IntentKind::Read,
            kind: HirExprKind::Match {
                scrutinee: Box::new(scrutinee),
                arms,
            },
            span: span.clone(),
        }
    }

    pub(super) fn wrap_var_self_function_returns(
        &mut self,
        block: &mut HirBlock,
        receiver: &HirBinding,
        abi_return_ty: &ResolvedTy,
    ) {
        self.wrap_var_self_explicit_returns_in_block(block, receiver, abi_return_ty);
        let tail = block
            .tail
            .take()
            .map_or_else(|| self.make_unit_expr(block.span.clone()), |tail| *tail);
        let span = tail.span.clone();
        block.tail = Some(Box::new(self.make_var_self_return_expr(
            tail,
            receiver,
            abi_return_ty,
            span,
        )));
        block.ty = abi_return_ty.clone();
    }

    pub(super) fn wrap_var_self_explicit_returns_in_block(
        &mut self,
        block: &mut HirBlock,
        receiver: &HirBinding,
        abi_return_ty: &ResolvedTy,
    ) {
        for stmt in &mut block.statements {
            self.wrap_var_self_stmt_returns(stmt, receiver, abi_return_ty);
        }
        if let Some(tail) = &mut block.tail {
            self.wrap_var_self_explicit_expr_returns(tail, receiver, abi_return_ty);
        }
    }

    pub(super) fn wrap_var_self_stmt_returns(
        &mut self,
        stmt: &mut HirStmt,
        receiver: &HirBinding,
        abi_return_ty: &ResolvedTy,
    ) {
        match &mut stmt.kind {
            HirStmtKind::Return(Some(expr)) => {
                self.wrap_var_self_explicit_expr_returns(expr, receiver, abi_return_ty);
                let span = expr.span.clone();
                let old = std::mem::replace(expr, self.make_unit_expr(span.clone()));
                *expr = self.make_var_self_return_expr(old, receiver, abi_return_ty, span);
            }
            HirStmtKind::Return(None) => {
                let span = stmt.span.clone();
                let unit = self.make_unit_expr(span.clone());
                stmt.kind = HirStmtKind::Return(Some(self.make_var_self_return_expr(
                    unit,
                    receiver,
                    abi_return_ty,
                    span,
                )));
            }
            HirStmtKind::Let(_, Some(expr)) | HirStmtKind::Expr(expr) => {
                self.wrap_var_self_explicit_expr_returns(expr, receiver, abi_return_ty);
            }
            HirStmtKind::Destructure { value, .. } => {
                self.wrap_var_self_explicit_expr_returns(value, receiver, abi_return_ty);
            }
            HirStmtKind::Assign { target, value, .. } => {
                self.wrap_var_self_explicit_expr_returns(target, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(value, receiver, abi_return_ty);
            }
            HirStmtKind::Defer { body, .. } => {
                self.wrap_var_self_explicit_expr_returns(body, receiver, abi_return_ty);
            }
            HirStmtKind::Let(_, None) => {}
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "visitor mirrors HIR shape while avoiding nested function-like bodies"
    )]
    pub(super) fn wrap_var_self_explicit_expr_returns(
        &mut self,
        expr: &mut HirExpr,
        receiver: &HirBinding,
        abi_return_ty: &ResolvedTy,
    ) {
        match &mut expr.kind {
            HirExprKind::RcIntrinsic {
                receiver: rc_receiver,
                value,
                ..
            } => {
                for operand in rc_receiver.iter_mut().chain(value.iter_mut()) {
                    self.wrap_var_self_explicit_expr_returns(operand, receiver, abi_return_ty);
                }
            }
            HirExprKind::Block(block)
            | HirExprKind::Scope { body: block }
            | HirExprKind::Race { body: block }
            | HirExprKind::ForkBlock { body: block, .. } => {
                self.wrap_var_self_explicit_returns_in_block(block, receiver, abi_return_ty);
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => {
                self.wrap_var_self_explicit_expr_returns(condition, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(then_expr, receiver, abi_return_ty);
                if let Some(else_expr) = else_expr {
                    self.wrap_var_self_explicit_expr_returns(else_expr, receiver, abi_return_ty);
                }
            }
            HirExprKind::While {
                condition, body, ..
            } => {
                self.wrap_var_self_explicit_expr_returns(condition, receiver, abi_return_ty);
                self.wrap_var_self_explicit_returns_in_block(body, receiver, abi_return_ty);
            }
            HirExprKind::ForRange {
                start,
                end,
                step,
                body,
                ..
            } => {
                self.wrap_var_self_explicit_expr_returns(start, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(end, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(step, receiver, abi_return_ty);
                self.wrap_var_self_explicit_returns_in_block(body, receiver, abi_return_ty);
            }
            HirExprKind::Loop { body, .. } => {
                self.wrap_var_self_explicit_returns_in_block(body, receiver, abi_return_ty);
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.wrap_var_self_explicit_expr_returns(scrutinee, receiver, abi_return_ty);
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        self.wrap_var_self_explicit_expr_returns(guard, receiver, abi_return_ty);
                    }
                    self.wrap_var_self_explicit_expr_returns(
                        &mut arm.body,
                        receiver,
                        abi_return_ty,
                    );
                }
            }
            HirExprKind::Call { callee, args, .. } => {
                self.wrap_var_self_explicit_expr_returns(callee, receiver, abi_return_ty);
                for arg in args {
                    self.wrap_var_self_explicit_expr_returns(arg, receiver, abi_return_ty);
                }
            }
            HirExprKind::ActorDelivery {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::ActorMessage {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::ActorAsk {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::ActorGenStream {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::CallDynMethod {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::ResolvedImplCall {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::CallTraitMethodStatic {
                receiver: target,
                args,
                ..
            }
            | HirExprKind::VarSelfMethodCall {
                receiver: target,
                args,
                ..
            } => {
                self.wrap_var_self_explicit_expr_returns(target, receiver, abi_return_ty);
                for arg in args {
                    self.wrap_var_self_explicit_expr_returns(arg, receiver, abi_return_ty);
                }
            }
            HirExprKind::RemoteActorAsk {
                receiver: target,
                msg,
                timeout_ms,
                ..
            } => {
                self.wrap_var_self_explicit_expr_returns(target, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(msg, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(timeout_ms, receiver, abi_return_ty);
            }
            HirExprKind::RemoteActorSend {
                receiver: target,
                msg,
            } => {
                self.wrap_var_self_explicit_expr_returns(target, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(msg, receiver, abi_return_ty);
            }
            HirExprKind::Binary { left, right, .. }
            | HirExprKind::IdentityCompare { left, right } => {
                self.wrap_var_self_explicit_expr_returns(left, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(right, receiver, abi_return_ty);
            }
            HirExprKind::Unary { operand, .. } | HirExprKind::WireCodec { operand, .. } => {
                self.wrap_var_self_explicit_expr_returns(operand, receiver, abi_return_ty);
            }
            HirExprKind::ArrayRepeat { value }
            | HirExprKind::NumericCast { value, .. }
            | HirExprKind::SaturatingWidthCast { value, .. }
            | HirExprKind::TryWidthCast { value, .. }
            | HirExprKind::CoerceToDynTrait { value, .. } => {
                self.wrap_var_self_explicit_expr_returns(value, receiver, abi_return_ty);
            }
            HirExprKind::TupleLiteral { elements } | HirExprKind::ArrayLiteral { elements } => {
                for elem in elements {
                    self.wrap_var_self_explicit_expr_returns(elem, receiver, abi_return_ty);
                }
            }
            HirExprKind::StructInit { fields, base, .. } => {
                for (_, field_expr) in fields {
                    self.wrap_var_self_explicit_expr_returns(field_expr, receiver, abi_return_ty);
                }
                if let Some(base) = base {
                    self.wrap_var_self_explicit_expr_returns(base, receiver, abi_return_ty);
                }
            }
            HirExprKind::FieldAccess { object, .. }
            | HirExprKind::TupleIndex { tuple: object, .. }
            | HirExprKind::CancellationTokenIsCancelled { receiver: object }
            | HirExprKind::GeneratorNext {
                receiver: object, ..
            }
            | HirExprKind::RecordCloneCall { src: object, .. }
            | HirExprKind::SubsumedValue { source: object, .. }
            | HirExprKind::ConnAwaitRead { conn: object, .. }
            | HirExprKind::AwaitRestart { child: object }
            | HirExprKind::AwaitTask {
                operand: object, ..
            } => {
                self.wrap_var_self_explicit_expr_returns(object, receiver, abi_return_ty);
            }
            HirExprKind::ListenerAwaitAccept { listener, .. } => {
                self.wrap_var_self_explicit_expr_returns(listener, receiver, abi_return_ty);
            }
            HirExprKind::StreamRecvAwait { stream, .. } => {
                self.wrap_var_self_explicit_expr_returns(stream, receiver, abi_return_ty);
            }
            HirExprKind::Index { container, index }
            | HirExprKind::BorrowedIndex { container, index } => {
                self.wrap_var_self_explicit_expr_returns(container, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(index, receiver, abi_return_ty);
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                ..
            } => {
                self.wrap_var_self_explicit_expr_returns(container, receiver, abi_return_ty);
                if let Some(start) = start {
                    self.wrap_var_self_explicit_expr_returns(start, receiver, abi_return_ty);
                }
                if let Some(end) = end {
                    self.wrap_var_self_explicit_expr_returns(end, receiver, abi_return_ty);
                }
            }
            HirExprKind::MachineVariantCtor {
                payload: Some(fields),
                ..
            } => {
                for (_, field_expr) in fields {
                    self.wrap_var_self_explicit_expr_returns(field_expr, receiver, abi_return_ty);
                }
            }
            HirExprKind::ScopeRecovery { scope, handler, .. } => {
                self.wrap_var_self_explicit_expr_returns(scope, receiver, abi_return_ty);
                self.wrap_var_self_explicit_expr_returns(handler, receiver, abi_return_ty);
            }
            HirExprKind::ScopeDeadline { duration, body } => {
                self.wrap_var_self_explicit_expr_returns(duration, receiver, abi_return_ty);
                self.wrap_var_self_explicit_returns_in_block(body, receiver, abi_return_ty);
            }
            HirExprKind::Select(select) => {
                for arm in &mut select.arms {
                    match &mut arm.kind {
                        HirSelectArmKind::StreamNext { stream } => {
                            self.wrap_var_self_explicit_expr_returns(
                                stream,
                                receiver,
                                abi_return_ty,
                            );
                        }
                        HirSelectArmKind::ActorAsk { call } => {
                            self.wrap_var_self_explicit_expr_returns(call, receiver, abi_return_ty);
                        }
                        HirSelectArmKind::TaskAwait { task } => {
                            self.wrap_var_self_explicit_expr_returns(task, receiver, abi_return_ty);
                        }
                        HirSelectArmKind::AfterTimer { duration } => {
                            self.wrap_var_self_explicit_expr_returns(
                                duration,
                                receiver,
                                abi_return_ty,
                            );
                        }
                    }
                    self.wrap_var_self_explicit_expr_returns(
                        &mut arm.body,
                        receiver,
                        abi_return_ty,
                    );
                }
            }
            HirExprKind::Yield { value, .. }
            | HirExprKind::Break { value, .. }
            | HirExprKind::Return { value } => {
                if let Some(value) = value {
                    self.wrap_var_self_explicit_expr_returns(value, receiver, abi_return_ty);
                }
            }
            HirExprKind::Spawn { args, .. } => {
                for (_, arg) in args {
                    self.wrap_var_self_explicit_expr_returns(arg, receiver, abi_return_ty);
                }
            }
            HirExprKind::SpawnLambdaActor { .. }
            | HirExprKind::Closure { .. }
            | HirExprKind::GenBlock { .. }
            | HirExprKind::Literal(_)
            | HirExprKind::RegexLiteralRef { .. }
            | HirExprKind::BindingRef { .. }
            | HirExprKind::ContextReader { .. }
            | HirExprKind::Continue { .. }
            | HirExprKind::ActorSelf
            | HirExprKind::MachineVariantCtor { payload: None, .. }
            | HirExprKind::Unsupported(_) => {}
        }
    }
}
