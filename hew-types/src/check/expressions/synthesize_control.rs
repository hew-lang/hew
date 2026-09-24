//! Checker methods grouped by responsibility: synthesize control.
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
use hew_parser::ast::{Ident, Path};
use std::collections::VecDeque;

impl Checker {
    /// `await` joins a task and nothing else. A plain call suspends the caller
    /// on its own, so `await` adds nothing there; every other operand is not a
    /// task and is refused with the move that replaces it.
    pub(super) fn check_await_operand(&mut self, expr: &Expr, span: &Span, ty: &Ty) {
        if matches!(ty, Ty::Error) {
            return;
        }
        // Every call waits on its own, an actor call included (U383): `await`
        // adds nothing there. `fork` is how a call runs concurrently, and
        // `await` then joins that task.
        if matches!(expr, Expr::Call { .. } | Expr::MethodCall { .. }) {
            self.errors.push(TypeError {
                severity: crate::error::Severity::Error,
                kind: TypeErrorKind::InvalidOperation,
                span: span.clone(),
                message: "`await` on a plain call adds nothing: the call suspends on its own"
                    .to_string(),
                notes: vec![],
                suggestions: vec![
                    "remove `await`, or fork the call to run it concurrently".to_string()
                ],
                source_module: self.current_module.clone(),
            });
        } else {
            let mut suggestions = vec!["remove `await`, or fork a call to get a task".to_string()];
            if matches!(
                ty,
                Ty::Named {
                    builtin: Some(BuiltinType::Vec),
                    ..
                }
            ) {
                suggestions.push(
                    "`await` over a vector joins a vector of task handles, so fill it with \
                     forked calls"
                        .to_string(),
                );
            }
            if ty.as_local_actor_ref().is_some() {
                suggestions
                    .push("`closed(actor)` waits for an actor to finish terminating".to_string());
            }
            self.report_error_with_suggestions(
                TypeErrorKind::InvalidOperation,
                span,
                format!("`await` joins a task; `{}` is not one", ty.user_facing()),
                suggestions,
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "concurrency variants (scope/select/join/spawn/unsafe/timeout)"
    )]
    pub(in crate::check) fn synthesize_concurrency(&mut self, expr: &Expr, span: &Span) -> Ty {
        match expr {
            Expr::ForkChild { expr: child } => {
                let children: Vec<&Spanned<Expr>> = match &child.0 {
                    Expr::Array(elements) => elements.iter().map(ArrayElement::expr).collect(),
                    Expr::Tuple(children) => children.iter().collect(),
                    _ => vec![child.as_ref()],
                };
                for branch in &children {
                    if !matches!(branch.0, Expr::Call { .. } | Expr::MethodCall { .. }) {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            &branch.1,
                            "fork expects a call or a batch of calls; use fork { ... } for a body"
                                .to_string(),
                        );
                    }
                    self.suspension_operands
                        .insert(SpanKey::in_module(&branch.1, self.current_module_idx));
                }
                let ret_ty = self.synthesize(&child.0, &child.1);
                for branch in &children {
                    self.record_fork_call_inputs(branch);
                }
                Ty::Task(Box::new(ret_ty))
            }
            Expr::ForkBlock { body } => {
                // Share capture identity and child return inference with closures.
                let synthetic_body = (Expr::Block(body.clone()), span.clone());
                let lambda_ty = self.check_lambda(
                    true,
                    &[],
                    None,
                    &[],
                    None,
                    &synthetic_body,
                    None,
                    span,
                    false,
                    true,
                );

                self.check_fork_transfer(expr, span, &lambda_ty);

                // Ordinary parameters are borrowed at Hew call boundaries.
                // Value snapshots acquire an independent child owner; an affine
                // borrowed parameter or explicit view cannot escape that way.
                let capture_key = SpanKey::in_module(span, self.current_module_idx);
                if let Some(captures) = self.closure_capture_facts.get(&capture_key).cloned() {
                    for capture in captures {
                        let capture_is_copy = self.ty_is_non_owning(&capture.ty);
                        let borrowed_parameter = !capture_is_copy
                            && capture.acquisition == crate::ClosureCaptureAcquisition::Move
                            && self.env.lookup_ref(&capture.name).is_some_and(|binding| {
                                binding.id == capture.binding_id && binding.is_param()
                            });
                        let borrowed_view = matches!(capture.ty, Ty::Borrow { .. });
                        if borrowed_parameter || borrowed_view {
                            self.errors.push(TypeError::new(
                                TypeErrorKind::ForkBorrowCapture {
                                    binding: capture.name.clone(),
                                },
                                capture.use_span,
                                format!(
                                    "fork body cannot borrow parent binding `{}` across the child boundary",
                                    capture.name
                                ),
                            ));
                        }
                    }
                }
                match lambda_ty {
                    Ty::Function { ret, .. } | Ty::Closure { ret, .. } => Ty::Task(ret),
                    _ => Ty::Error,
                }
            }
            Expr::SpawnLambdaActor {
                is_move,
                params,
                return_type,
                body,
            } => {
                // A lambda actor is an actor declaration without a source
                // name. Mint its identity here, keyed by the exact span of
                // the `actor` expression, so HIR can synthesize the actor
                // declaration and its single receive handler against a
                // resolver-owned `DefId` like every named actor.
                self.declare_lambda_actor(span);
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
                let lambda_ty = self.check_lambda(
                    *is_move,
                    &[],
                    None,
                    params,
                    None,
                    body,
                    None,
                    span,
                    true,
                    false,
                );
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
                // A lambda body that produces an `actor(...) -> ...` handle (lambda-actor
                // handle) or a raw `Duplex<...>` channel is leaking a move-only handle outside
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
                if body_ret.as_actor_fn().is_some() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "actor lambda body returns an actor handle — actor handles cannot \
                         escape the actor boundary via a return value (E_LAMBDA_SELF_ESCAPE); \
                         use an actor with no return type instead"
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
                // The reply type determines send vs ask:
                //   send-shaped (`actor |p| { ... }` — no explicit return type, or `-> ()`)
                //     → `actor(Msg) -> ()` — call-site returns `Result<(), SendError>`
                //   ask-shaped (`actor |p| -> Reply { ... }`)
                //     → `actor(Msg) -> Reply` — call-site returns `Result<Reply, AskError>`
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
                                !self.try_unify_with_owner_identity(&resolved_body, &resolved);
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
                Ty::actor_fn(msg_ty, reply_ty)
            }
            Expr::Scope { body: block } => {
                self.task_scope_depth += 1;
                let ty = self.check_block(block, None);
                self.task_scope_depth -= 1;
                ty
            }
            Expr::ScopeDeadline { duration, body } => {
                self.check_against(&duration.0, &duration.1, &Ty::Duration);
                self.task_scope_depth += 1;
                let ty = self.check_block(body, None);
                self.task_scope_depth -= 1;
                ty
            }
            Expr::UnsafeBlock(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block(block, None);
                self.in_unsafe = prev;
                ty
            }
            Expr::Select { arms, timeout } => {
                // WASM-TODO(suspending-select): compile the readiness waitset for wasm32.
                self.reject_wasm_feature(span, WasmUnsupportedFeature::Select);
                if arms.is_empty() && timeout.is_none() {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        "a `select` needs at least one arm: a source arm \
                         (`name from source => body`), or an `after` timer arm"
                            .to_string(),
                    );
                    return Ty::Error;
                }
                let mut result_ty: Option<Ty> = None;
                let prepared_depth = self.prepared_select_tasks.len();
                // Only the BODIES of a select are alternatives. Every arm's
                // source is prepared before dispatch chooses a winner — all the
                // asks are issued, all the receivers polled — so the sources run
                // on one execution, in order, and handing the same affine value
                // to two of them is a real double transfer. They thread
                // sequentially; the same goes for the timeout duration, which
                // arms the deadline before any arm fires.
                let mut source_tys = Vec::with_capacity(arms.len());
                let mut sources = Vec::with_capacity(arms.len());
                for arm in arms {
                    self.env.push_scope();
                    let (ty, source) = self.synthesize_select_source(&arm.source.0, &arm.source.1);
                    if matches!(source, Some(super::CheckedSelectSource::TaskAwait { .. })) {
                        if let Some((root, path)) = self.expr_place(&arm.source.0) {
                            if let Some(binding) = self.env.lookup_ref(&root) {
                                self.prepared_select_tasks
                                    .push(super::types::PreparedSelectTask {
                                        binding: binding.id,
                                        path,
                                        span: arm.source.1.clone(),
                                    });
                            }
                        }
                    }
                    source_tys.push(ty);
                    sources.push(source);
                    self.env.pop_scope();
                }
                if let Some(checked) = sources.iter().cloned().collect::<Option<Vec<_>>>() {
                    self.select_sources
                        .insert(SpanKey::in_module(span, self.current_module_idx), checked);
                }
                if let Some(tc) = timeout {
                    self.check_against(&tc.duration.0, &tc.duration.1, &Ty::Duration);
                }
                self.prepared_select_tasks.truncate(prepared_depth);

                // Dispatch happens here: from this state exactly one body runs.
                let entry = self.env.ownership_snapshot();
                let mut arm_exits = Vec::with_capacity(arms.len() + 1);
                for ((arm, source_ty), source) in arms.iter().zip(&source_tys).zip(&sources) {
                    self.env.push_scope();
                    self.env.restore_ownership(&entry);
                    if matches!(source, Some(super::CheckedSelectSource::TaskAwait { .. }))
                        && !self.reject_borrowed_consumption(&arm.source.0, &arm.source.1)
                    {
                        self.mark_expr_moved(&arm.source.0, &arm.source.1);
                    }
                    self.bind_pattern(&arm.binding.0, source_ty, false, &arm.binding.1);
                    let body_ty = if let Some(expected) = &result_ty {
                        self.check_against(&arm.body.0, &arm.body.1, expected)
                    } else {
                        self.synthesize(&arm.body.0, &arm.body.1)
                    };
                    arm_exits.push(BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&body_ty),
                    });
                    if result_ty.is_none() {
                        result_ty = Some(body_ty);
                    }
                    self.env.pop_scope();
                }
                if let Some(tc) = timeout {
                    self.env.restore_ownership(&entry);
                    let timeout_ty = self.synthesize(&tc.body.0, &tc.body.1);
                    arm_exits.push(BranchArmExit {
                        ownership: self.env.ownership_snapshot(),
                        diverges: Self::arm_skips_join(&timeout_ty),
                    });
                    if let Some(expected) = &result_ty {
                        self.expect_type(expected, &timeout_ty, &tc.body.1);
                    } else {
                        result_ty = Some(timeout_ty);
                    }
                }
                self.join_branch_ownership(&entry, &arm_exits);
                result_ty.unwrap_or(Ty::Unit)
            }
            Expr::Race(branches) => self.synthesize_race(branches, span),
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
                        "`gen { }` blocks are forbidden inside \
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
                let previous_defer = self.deferred_body.take();
                let prev_fails = std::mem::replace(&mut self.current_fails, false);
                self.in_generator = true;
                self.current_return_type = Some(gen_ty.clone());

                let effect_body = super::effects::EffectBody::GeneratorBlock(SpanKey::in_module(
                    span,
                    self.current_module_idx,
                ));
                self.effect_graph
                    .bodies
                    .entry(effect_body.clone())
                    .or_default();
                let previous_effect_body = self.effect_graph.current_body.replace(effect_body);
                let body_ty = self.check_block(body, None);
                self.effect_graph.current_body = previous_effect_body;

                self.in_generator = prev_in_generator;
                self.current_return_type = prev_return_type;
                self.deferred_body = previous_defer;
                self.current_fails = prev_fails;

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
                        "`gen { }` body contains no `yield` expression \
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

    pub(in crate::check) fn check_expr_with_expected(
        &mut self,
        expr: &Expr,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        match expr {
            Expr::Block(block) => self.check_block_expr_with_expected(expr, block, span, expected),
            // An `unsafe` block is a block: its tail flows to the surrounding
            // expectation, so `.Ok(x)` resolves inside one.
            Expr::UnsafeBlock(block) => {
                let prev = self.in_unsafe;
                self.in_unsafe = true;
                let ty = self.check_block_expr_with_expected(expr, block, span, expected);
                self.in_unsafe = prev;
                ty
            }
            _ => self.check_against(expr, span, expected),
        }
    }

    pub(super) fn check_block_expr_with_expected(
        &mut self,
        expr: &Expr,
        block: &Block,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        let actual = self.check_block(block, Some(expected));
        let result = if matches!(actual, Ty::Never | Ty::Error) {
            actual.clone()
        } else {
            let n = self.errors.len();
            self.expect_type(expected, &actual, span);
            if self.errors.len() > n {
                Ty::Error
            } else {
                actual.clone()
            }
        };
        // A block's value IS its trailing expression's value. When that
        // tail fails to meet the expectation, `check_against` reports
        // the mismatch on the tail's own span, PUBLISHES the tail's
        // recovered type, and returns the error placeholder to poison
        // the caller. Publish the same recovered type for the block so
        // the two agree: the produced-value graph treats the tail as
        // the block's identity dependency and rejects a disagreement
        // ("identity dependency changes type from T to Error"), and
        // consumers that read published types -- hover -- surface the
        // placeholder as an unknown type. The placeholder is still what
        // this call returns, so callers keep their poisoned result.
        let published = if matches!(result, Ty::Error) {
            block
                .trailing_expr
                .as_ref()
                .and_then(|tail| {
                    self.expr_types
                        .get(&SpanKey::in_module(&tail.1, self.current_module_idx))
                        .cloned()
                })
                .unwrap_or_else(|| result.clone())
        } else {
            result.clone()
        };
        self.publish_checked_expression(expr, span, published);
        result
    }

    /// Check: verify expression against expected type (top-down).
    pub(in crate::check) fn check_against(
        &mut self,
        expr: &Expr,
        span: &Span,
        expected: &Ty,
    ) -> Ty {
        let result = self.check_against_inner(expr, span, expected);
        self.publish_checked_expression(expr, span, result)
    }

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
                let Some(variant) =
                    self.context_variant_definition(&owner, context.name.name.as_str())
                else {
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
                        path: Path::single(Ident::new(&qualified_name), span.clone()), // TRANSITION(P1): deleted by A1 commit 2
                        fields: record.fields.clone(),
                        type_args: None,
                        base: record.base.clone(),
                    }
                } else {
                    Expr::Ident(Ident::new(&qualified_name))
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
            (Expr::Ident(name), ty) if ty.is_numeric() => {
                if let Some(cv) = self.const_values.get(name.name.as_str()).cloned() {
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
                            self.expect_inferable_literal_binding(
                                name.name.as_str(),
                                expected,
                                span,
                            );
                            let _ = self.synthesize_identifier(name.name.as_str(), span);
                            self.record_type(span, expected);
                            return expected.clone();
                        }
                        (ConstValue::Integer(_), ty) if ty.is_float() => {
                            self.expect_inferable_literal_binding(
                                name.name.as_str(),
                                expected,
                                span,
                            );
                            let _ = self.synthesize_identifier(name.name.as_str(), span);
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
                            self.expect_inferable_literal_binding(
                                name.name.as_str(),
                                expected,
                                span,
                            );
                            let _ = self.synthesize_identifier(name.name.as_str(), span);
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
                    path: named_path,
                    fields,
                    type_args,
                    base,
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    ..
                },
            ) if named_path.to_string() != *expected_name // TRANSITION(P1): deleted by A1 commit 2
                && expected_args.is_empty()
                && !named_path.to_string().contains('.')
                && !named_path.to_string().contains("::")
                && expected_name.contains('.')
                && crate::short_name(expected_name) == named_path.to_string()
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
                    path: named_path,
                    fields,
                    type_args,
                    base,
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    ..
                },
            ) if named_path.to_string() != *expected_name // TRANSITION(P1): deleted by A1 commit 2
                && !expected_args.is_empty()
                && !named_path.to_string().contains('.')
                && !named_path.to_string().contains("::")
                && expected_name.contains('.')
                && crate::short_name(expected_name) == named_path.to_string()
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
                    path: Path::single(Ident::new(expected_name), span.clone()), // TRANSITION(P1): deleted by A1 commit 2
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
                    path: named_path,
                    fields,
                    type_args,
                    ..
                },
                Ty::Named {
                    name: expected_name,
                    args: expected_args,
                    builtin: expected_builtin,
                },
            ) if named_path.to_string() == *expected_name // TRANSITION(P1): deleted by A1 commit 2
                && !expected_builtin.is_some_and(crate::BuiltinType::is_substrate_handle) =>
            {
                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
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
                            if let Some(declared_ty) = td.fields.get(field_name.name.as_str()) {
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
                                    field_name.name.as_str(),
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
                            fields.iter().map(|(n, _)| n.name.as_str()).collect();
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
                    path: named_path,
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
                let name = &named_path.to_string(); // TRANSITION(P1): deleted by A1 commit 2
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
                                    if let Some((_, declared_ty)) = variant_fields
                                        .iter()
                                        .find(|(n, _)| n == field_name.name.as_str())
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
                                            field_name.name.as_str(),
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
                                    fields.iter().map(|(n, _)| n.name.as_str()).collect();
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
                        receiver,
                        method.0.name.as_str(),
                        args,
                        expected,
                        span,
                    )
                    .unwrap_or_else(|| self.synthesize(expr, span));
                self.finish_named_arguments(
                    args,
                    || format!("method `{}`", method.0),
                    &actual,
                    span,
                );
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
                Expr::Ident(name),
                Ty::Named {
                    builtin: Some(crate::BuiltinType::Option),
                    ..
                },
            ) if name.name.as_str() == "None" => {
                self.report_bare_variant_expr(name.name.as_str(), ".None", span);
                self.record_type(span, expected);
                expected.clone()
            }
            (
                Expr::Ident(name),
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
                    .name
                    .as_str()
                    .rsplit_once("::")
                    .filter(|(prefix, _)| !prefix.contains('.'))
                    .filter(|_| self.variant_surface_owner_matches(name.name.as_str(), expected))
                    .map(|(_, variant)| variant.to_string());
                let expected_type_def = self.lookup_type_def(expected_type_name);
                let is_unit_variant = expected_type_def
                    .as_ref()
                    .and_then(|td| {
                        td.variants
                            .get(variant_after_owner.as_deref().unwrap_or(name.name.as_str()))
                            .cloned()
                    })
                    .is_some_and(|v| matches!(v, VariantDef::Unit))
                    && (variant_after_owner.is_some() || !name.name.as_str().contains("::"));
                // A `machine`'s states are not enum variants in expression
                // position (HEW-SPEC-2026 §3.11.3, "State names are not
                // variants"): the target name after `=>` in a body-less
                // transition (`on E: Src => Tgt;`) desugars to a bare
                // `Expr::Ident(Tgt)` checked against the machine's own
                // type, and resolving it here must not suggest the enum
                // `.Variant` fix-it — that fix-it is for real enum bare
                // variants (#3264).
                let bare_state_here = self.machine_state_is_bare_here(expected_type_name);
                if is_unit_variant {
                    if !name.name.as_str().contains("::") && !bare_state_here {
                        self.report_bare_variant_expr(
                            name.name.as_str(),
                            &format!(".{name}"),
                            span,
                        );
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

    /// Stable rendering of a substitution, for the visited-set key.
    pub(super) fn render_substitution(substitution: &HashMap<String, Ty>) -> String {
        let mut pairs: Vec<String> = substitution
            .iter()
            .map(|(param, ty)| format!("{param}={}", ty.user_facing()))
            .collect();
        pairs.sort();
        pairs.join(", ")
    }

    pub(in crate::check) fn selected_eq_available(service: &mut TypeFactService, ty: &Ty) -> bool {
        ResolvedTy::from_ty(ty).ok().is_some_and(|resolved| {
            service
                .capability_plan(&resolved, crate::ValueCapability::Eq)
                .is_ok_and(|selection| selection.is_some())
        })
    }

    /// Publish a checked expression type without overwriting a more precise
    /// source type recorded during contextual checking.
    pub(super) fn publish_checked_expression(
        &mut self,
        expr: &Expr,
        span: &Span,
        result: Ty,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
        self.expr_type_source_modules
            .entry(key.clone())
            .or_insert_with(|| self.current_module.clone());
        self.expr_types.entry(key).or_insert_with(|| result.clone());
        self.record_expression_effect(expr, span);
        self.check_receiver_whole_at_expr(expr, span, &result);
        result
    }
}
