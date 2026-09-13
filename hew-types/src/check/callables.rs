//! Binding-resolved callable construction and invocation contracts.

use super::{
    Checker, ClosureCaptureFact, Expr, HashSet, MarkerTrait, ResolvedTy, Span, Spanned, Ty,
    TypeError, TypeErrorKind,
};
use crate::env::{TypeBindingId, TypeEnv};
use crate::{
    CallableCallMode, CallableCapabilities, ClosureCaptureAccess, ClosureCaptureAcquisition,
    ClosureCaptureConsumption,
};

impl Checker {
    /// Instantiate a declaration used as a value through the same signature,
    /// bound and identity authorities as a direct call. Inference can settle
    /// from the destination type or a later invocation of the local binding.
    pub(super) fn instantiate_function_value(
        &mut self,
        signature_key: &str,
        type_args: Option<&[Spanned<hew_parser::ast::TypeExpr>]>,
        span: &Span,
    ) -> Ty {
        let sig = self.fn_sigs[signature_key].clone();
        if sig
            .param_ownership
            .contains(&crate::env::ParameterOwnership::Consume)
            || sig.consumes_receiver
        {
            self.report_error_with_suggestions(TypeErrorKind::InvalidOperation, span,
                "cannot use a consuming function declaration as a callable value: callable types do not preserve parameter ownership modes".to_string(),
                vec!["call the declaration directly to preserve its consume contract".to_string()]);
            return Ty::Error;
        }
        let (params, ret, arguments) = self.instantiate_fn_sig_for_call(&sig, type_args, span);
        let assoc_bindings = self
            .fn_type_param_assoc_bindings
            .get(signature_key)
            .cloned()
            .unwrap_or_default();
        self.enforce_type_param_bounds_with_assoc(&sig, &assoc_bindings, &arguments, span);
        self.record_concrete_call_type_args(span, &arguments);
        let target = self.call_target_for_signature(signature_key);
        self.record_direct_call_target(span, target.clone());
        // A declaration with a checked body keeps its identity so calls through
        // the value inherit the body's effect; every other target has a static
        // contract and is already a plain function type.
        match target {
            super::CallTarget::User(id) | super::CallTarget::ImplMethod(id) => Ty::Closure {
                capabilities: CallableCapabilities::FUNCTION_ITEM,
                params,
                ret: Box::new(ret),
                captures: Vec::new(),
                identity: super::effects::EffectBody::Declaration(id),
            },
            target => Ty::Function {
                capabilities: CallableCapabilities {
                    suspends: self.effect_graph.builtin_suspensions.contains(&target)
                        || matches!(&target, super::CallTarget::Runtime(family)
                            if family.is_async_suspending().is_some()),
                    ..CallableCapabilities::FUNCTION_ITEM
                },
                params,
                ret: Box::new(ret),
            },
        }
    }

    /// Whether a value of this type has exactly one owner, so reading it out
    /// of a place is a transfer rather than a copy. The single class-fact
    /// authority SIR's own take/copy decision reads.
    pub(super) fn place_read_transfers_ownership(&self, ty: &Ty) -> bool {
        let Ok(resolved) = ResolvedTy::from_ty(&ty.materialize_literal_defaults()) else {
            return false;
        };
        let declarations = self.class_declarations();
        let context = crate::value_class::ClassContext::new(&declarations);
        matches!(
            crate::value_class::classify_ty(&resolved, &context),
            Ok((_, crate::type_facts::CloneKind::None))
        )
    }

    /// Record a value transfer using the shared clone classification.
    /// SIR authors the actual transfer; the checker uses the same class facts
    /// to determine capture capabilities and reject later source uses.
    pub(super) fn record_value_transfer(&mut self, expr: &Expr, span: &Span) {
        let key = super::SpanKey::in_module(span, self.current_module_idx);
        let Some(ty) = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty)) else {
            return;
        };
        if self.place_read_transfers_ownership(&ty) && !self.reject_borrowed_consumption(expr, span)
        {
            self.mark_expr_moved(expr, span);
        }
    }

    /// Index reads lend affine elements; aliases preserve the same loan.
    pub(super) fn collection_borrow_origin(&self, expr: &Expr, span: &Span) -> Option<Span> {
        let key = super::SpanKey::in_module(span, self.current_module_idx);
        let ty = self.expr_types.get(&key).map(|ty| self.subst.resolve(ty))?;
        if !self.place_read_transfers_ownership(&ty) {
            return None;
        }
        if self.borrowed_element_index_reads.contains(&key) {
            return Some(span.clone());
        }
        let (root, _) = self.expr_place(expr)?;
        self.env.lookup_ref(&root)?.collection_borrow.clone()
    }

    /// A pattern binder takes one field out of the place it destructures.
    /// Same class facts and same refusals as the projected `let` that spells
    /// the transfer out (`let t = booking.ticket`), so the two agree on what
    /// is still owned after the pattern binds.
    pub(super) fn record_pattern_place_transfer(
        &mut self,
        root: &str,
        path: crate::env::PlacePath,
        ty: &Ty,
        span: &Span,
    ) {
        if !self.place_read_transfers_ownership(ty)
            || self.reject_borrowed_place_consumption(root, &path, span)
            || self.reject_partial_place_consumption(root, &path, span)
        {
            return;
        }
        self.env.mark_place_moved(root, path, span.clone());
    }

    pub(super) fn infer_lambda_result(&mut self, body: &Spanned<Expr>) -> Ty {
        let inferred = Ty::Var(crate::ty::TypeVar::fresh());
        self.current_return_type = Some(inferred.clone());
        self.inferred_lambda_returns = Some(Vec::new());
        let tail = self.synthesize(&body.0, &body.1);
        let mut returns = self
            .inferred_lambda_returns
            .take()
            .expect("inferred lambda return context");
        returns.push(tail);
        let mut result = Ty::Never;
        for ty in returns {
            let ty = self.subst.resolve(&ty);
            if matches!(ty, Ty::Never | Ty::Error) {
                continue;
            }
            if result == Ty::Never {
                result = ty;
            } else if (result == Ty::Unit) != (ty == Ty::Unit) {
                self.expect_type(&result, &ty, &body.1);
            } else {
                result = self.unify_branches(&result, &ty, &body.1);
            }
        }
        self.expect_type(&inferred, &result, &body.1);
        if result == Ty::Never {
            result
        } else {
            self.subst.resolve(&inferred)
        }
    }

    pub(super) fn resolve_private_captures(
        &mut self,
        captures: &[Spanned<String>],
    ) -> HashSet<TypeBindingId> {
        let mut bindings = HashSet::new();
        for (name, span) in captures {
            if let Some(binding) = self.env.lookup_ref(name) {
                if !bindings.insert(binding.id) {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        span,
                        format!("duplicate private capture `{name}`"),
                    );
                }
            } else {
                self.report_error(
                    TypeErrorKind::UndefinedVariable,
                    span,
                    format!("private capture `{name}` must name an existing binding"),
                );
            }
        }
        bindings
    }

    pub(super) fn finish_closure_captures(
        &mut self,
        facts: Vec<ClosureCaptureFact>,
        private: &HashSet<TypeBindingId>,
        body: &TypeEnv,
        is_move: bool,
        span: &Span,
        is_fork_body: bool,
    ) -> Vec<ClosureCaptureFact> {
        let mut seen = HashSet::new();
        let mut captures = Vec::new();
        for mut fact in facts {
            if !seen.insert(fact.binding_id) {
                continue;
            }
            fact.ty = self.subst.resolve(&fact.ty).materialize_literal_defaults();
            let fork_snapshot = is_fork_body
                && !matches!(fact.ty, Ty::Borrow { .. })
                && self.parameter_has_independent_clone(&fact.ty);
            fact.acquisition = if is_move && !fork_snapshot {
                ClosureCaptureAcquisition::Move
            } else {
                ClosureCaptureAcquisition::Snapshot
            };
            fact.access = if private.contains(&fact.binding_id) {
                ClosureCaptureAccess::Var
            } else {
                ClosureCaptureAccess::Read
            };
            fact.consumption = if body.binding_by_id(fact.binding_id).is_some_and(|binding| {
                binding.capture_consumption == ClosureCaptureConsumption::Consumed
            }) {
                ClosureCaptureConsumption::Consumed
            } else {
                ClosureCaptureConsumption::Retained
            };
            fact.is_send = self.registry.implements_marker(&fact.ty, MarkerTrait::Send);
            fact.is_sync = self.registry.is_sync(&fact.ty);
            let is_copy = self.registry.implements_marker(&fact.ty, MarkerTrait::Copy);
            if is_move && !fork_snapshot {
                if !is_copy
                    && !self.reject_borrowed_consumption(&Expr::Identifier(fact.name.clone()), span)
                {
                    self.env.mark_moved(&fact.name, span.clone());
                }
            } else if !self.capture_is_cloneable(&fact.ty)
                && !matches!(fact.ty, Ty::Error | Ty::Var(_))
            {
                self.report_error(
                    TypeErrorKind::ClosureExplicitMoveRequired {
                        name: fact.name.clone(), ty: fact.ty.user_facing().to_string(),
                    }, &fact.use_span,
                    format!("capture `{}` has no independent snapshot operation; use `move` to transfer it into the closure", fact.name),
                );
            }
            captures.push(fact);
        }
        captures
    }

    pub(super) fn closure_capabilities(
        &self,
        captures: &[ClosureCaptureFact],
    ) -> CallableCapabilities {
        let call = if captures
            .iter()
            .any(|fact| fact.consumption == ClosureCaptureConsumption::Consumed)
        {
            CallableCallMode::Once
        } else if captures
            .iter()
            .any(|fact| fact.access == ClosureCaptureAccess::Var)
        {
            CallableCallMode::Var
        } else {
            CallableCallMode::Read
        };
        CallableCapabilities {
            call,
            clone: captures
                .iter()
                .all(|fact| self.capture_is_cloneable(&fact.ty)),
            suspends: false,
        }
    }
    pub(super) fn parameter_has_independent_clone(&self, ty: &Ty) -> bool {
        self.parameter_clone_kind(ty)
            .is_some_and(|clone| clone != crate::type_facts::CloneKind::None)
    }

    pub(super) fn parameter_clone_kind(&self, ty: &Ty) -> Option<crate::type_facts::CloneKind> {
        let resolved =
            ResolvedTy::from_ty(&self.subst.resolve(ty).materialize_literal_defaults()).ok()?;
        let declarations = self.class_declarations();
        let context = crate::value_class::ClassContext::new(&declarations);
        crate::value_class::classify_ty(&resolved, &context)
            .ok()
            .map(|(_, clone)| clone)
    }

    /// Private writes require either a replaced place or an independent entry
    /// copy. An unresolved generic type does not prove absence of Clone.
    pub(super) fn reject_borrowed_parameter_mutation(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) {
        if self.reject_prepared_task_access(root, path, span, TypeErrorKind::OwnMutateBorrowed) {
            return;
        }
        if self.is_current_closure_capture(root)
            || !self.env.place_borrows_parameter(root, path)
            || !self.env.lookup_ref(root).is_some_and(|binding| {
                self.parameter_clone_kind(&binding.ty) == Some(crate::type_facts::CloneKind::None)
            })
        {
            return;
        }
        self.report_error_with_suggestions(
            TypeErrorKind::OwnMutateBorrowed,
            span,
            format!("cannot mutate through borrowed parameter `{root}`: its type has no independent clone"),
            vec![format!("add `consume` to parameter `{root}` to transfer ownership")],
        );
    }

    fn capture_is_cloneable(&self, ty: &Ty) -> bool {
        let Ok(resolved) = ResolvedTy::from_ty(ty) else {
            return false;
        };
        let declarations = self.class_declarations();
        let context = crate::value_class::ClassContext::new(&declarations);
        matches!(
            crate::value_class::ValueClass::of_ty(&resolved, &context),
            Ok(crate::value_class::ValueClass::BitCopy
                | crate::value_class::ValueClass::CowValue
                | crate::value_class::ValueClass::PersistentShare
                | crate::value_class::ValueClass::View)
        ) && (self.registry.implements_marker(ty, MarkerTrait::Copy)
            || self.registry.implements_marker(ty, MarkerTrait::Clone))
    }

    pub(super) fn private_capture_mutation_error(
        &self,
        name: &str,
        span: &Span,
    ) -> Option<TypeError> {
        let depth = self.lambda_capture_depth?;
        let (binding_depth, binding) = self.env.lookup_ref_with_depth(name)?;
        if binding_depth >= depth || binding.is_mutable || self.in_lambda_actor_body {
            return None;
        }
        Some(TypeError::new(TypeErrorKind::MutabilityError, span.clone(),
            format!("capture `{name}` is an immutable snapshot; add `capture(var {name})` before the lambda to mutate its private field")))
    }

    pub(super) fn check_callable_receiver(&mut self, ty: &Ty, callee: &Spanned<Expr>) {
        let (Ty::Function { capabilities, .. } | Ty::Closure { capabilities, .. }) =
            self.normalize_for_use(ty)
        else {
            return;
        };
        let place = self.expr_place(&callee.0);
        if let Some((root, path)) = &place {
            if self
                .env
                .lookup_ref(root)
                .is_some_and(|binding| binding.is_moved)
            {
                self.report_error(
                    TypeErrorKind::UseAfterMove,
                    &callee.1,
                    format!("cannot invoke consumed callable `{root}`"),
                );
            } else {
                self.report_place_use_after_move(root, path, &callee.1);
            }
        }
        match capabilities.call {
            CallableCallMode::Read => {}
            CallableCallMode::Var => {
                let writable = place.as_ref().is_some_and(|(root, _)| {
                    self.env
                        .lookup_ref(root)
                        .is_some_and(|binding| binding.is_mutable)
                });
                if writable {
                    if !capabilities.clone {
                        if let Some((root, path)) = &place {
                            if !self.is_current_closure_capture(root)
                                && self.env.place_borrows_parameter(root, path)
                            {
                                self.report_error_with_suggestions(TypeErrorKind::OwnMutateBorrowed, &callee.1,
                                format!("cannot invoke this mutable callable through borrowed parameter `{root}`: its type does not guarantee an independent clone"),
                                vec![format!("add `consume` to parameter `{root}` to transfer ownership"),
                                    "require `fn[var, clone]` so the callee can mutate an independent copy".to_string()]);
                                return;
                            }
                        }
                    }
                    if let Some((root, _)) = place {
                        self.env.discount_mutation_receiver_read(&root);
                        self.env.mark_written(&root);
                    }
                } else {
                    let error = place.as_ref().and_then(|(root, _)| self.private_capture_mutation_error(root, &callee.1))
                        .unwrap_or_else(|| TypeError::new(TypeErrorKind::MutabilityError, callee.1.clone(),
                            "this callable requires a mutable place; bind it with `var` before calling"));
                    self.errors.push(error);
                }
            }
            CallableCallMode::Once => self.record_callable_consumption(&callee.0, &callee.1),
        }
    }

    pub(super) fn record_declared_argument_consumption(
        &mut self,
        sig: &super::FnSig,
        index: usize,
        expr: &Expr,
        span: &Span,
    ) {
        if sig.param_ownership.get(index) != Some(&crate::env::ParameterOwnership::Consume) {
            return;
        }
        if !self.reject_borrowed_consumption(expr, span) {
            self.mark_expr_moved(expr, span);
        }
    }

    fn is_current_closure_capture(&self, root: &str) -> bool {
        self.lambda_capture_depth.is_some_and(|capture_depth| {
            self.env
                .lookup_ref_with_depth(root)
                .is_some_and(|(depth, _)| depth < capture_depth)
        })
    }

    pub(super) fn reject_borrowed_consumption(&mut self, expr: &Expr, span: &Span) -> bool {
        if matches!(expr, Expr::Index { .. }) && self.collection_borrow_origin(expr, span).is_some()
        {
            self.report_error(
                TypeErrorKind::OwnConsumeBorrowed,
                span,
                "E_OWN_CONSUME_BORROWED: cannot consume an indexed collection borrow".to_string(),
            );
            return true;
        }
        let Some((root, path)) = self.expr_place(expr) else {
            return false;
        };
        self.reject_borrowed_place_consumption(&root, &path, span)
    }

    /// The same refusal keyed by the place itself, for consuming uses that
    /// have no expression to name them - a pattern binder taking one field
    /// out of the place it destructures.
    pub(super) fn reject_borrowed_place_consumption(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
    ) -> bool {
        if self.reject_prepared_task_access(root, path, span, TypeErrorKind::OwnConsumeBorrowed) {
            return true;
        }
        if let Some(origin) = self
            .env
            .lookup_ref(root)
            .and_then(|binding| binding.collection_borrow.clone())
        {
            self.report_error_with_note(
                TypeErrorKind::OwnConsumeBorrowed,
                span,
                format!(
                    "E_OWN_CONSUME_BORROWED: cannot consume borrowed collection element `{root}`"
                ),
                &origin,
                "the collection retains this value's owner".to_string(),
            );
            return true;
        }
        // A closure environment capture follows its existing acquisition contract.
        if self.is_current_closure_capture(root) || !self.env.place_borrows_parameter(root, path) {
            return false;
        }
        let suggestion = if self.lambda_capture_depth.is_some() {
            format!("lambda parameters cannot declare consume; use a named function with a `consume {root}: ...` parameter")
        } else {
            format!("declare the parameter `consume {root}: ...` before consuming it")
        };
        self.report_error_with_suggestions(TypeErrorKind::OwnConsumeBorrowed, span,
            format!("E_OWN_CONSUME_BORROWED: cannot consume a value through borrowed parameter `{root}`"),
            vec![suggestion]);
        true
    }

    pub(super) fn reject_prepared_task_access(
        &mut self,
        root: &str,
        path: &[String],
        span: &Span,
        kind: TypeErrorKind,
    ) -> bool {
        let Some(binding) = self.env.lookup_ref(root) else {
            return false;
        };
        let origin = self
            .prepared_select_tasks
            .iter()
            .find(|loan| {
                loan.binding == binding.id
                    && (path.starts_with(&loan.path) || loan.path.starts_with(path))
            })
            .map(|loan| loan.span.clone());
        let Some(origin) = origin else {
            return false;
        };
        self.report_error_with_note(
            kind,
            span,
            format!(
                "cannot change prepared task `{}` while other selection sources are evaluated",
                Self::render_place(root, path)
            ),
            &origin,
            "selection borrows this task until its winning branch is chosen".into(),
        );
        true
    }

    fn record_callable_consumption(&mut self, expr: &Expr, span: &Span) {
        if self.reject_borrowed_consumption(expr, span) {
            return;
        }
        self.mark_expr_moved(expr, span);
    }

    pub(super) fn callable_erasure_loses_obligation(&self, expected: &Ty, actual: &Ty) -> bool {
        match (expected, actual) {
            (Ty::Function { .. }, Ty::Closure { .. }) => {
                let Ok(actual) = ResolvedTy::from_ty(actual) else {
                    return false;
                };
                let declarations = self.class_declarations();
                let context = crate::value_class::ClassContext::new(&declarations);
                matches!(
                    crate::value_class::ValueClass::of_ty(&actual, &context),
                    Ok(crate::value_class::ValueClass::Linear)
                )
            }
            (Ty::Named { args: expected, .. }, Ty::Named { args: actual, .. })
            | (Ty::Tuple(expected), Ty::Tuple(actual)) => expected
                .iter()
                .zip(actual)
                .any(|(expected, actual)| self.callable_erasure_loses_obligation(expected, actual)),
            (Ty::Array(expected, _), Ty::Array(actual, _)) => {
                self.callable_erasure_loses_obligation(expected, actual)
            }
            _ => false,
        }
    }

    pub(super) fn reject_callable_erasure(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: &Span,
    ) -> bool {
        if !self.callable_erasure_loses_obligation(expected, actual) {
            return false;
        }
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            "callable erasure cannot discard a captured linear ownership obligation".to_string(),
        );
        true
    }

    pub(super) fn check_method_callable_place(
        &mut self,
        receiver: &Spanned<Expr>,
        method: &str,
        span: &Span,
    ) {
        let key = super::SpanKey::in_module(span, self.current_module_idx);
        if let Some(super::MethodCallRewrite::RecordFnFieldCall { field_ty }) =
            self.method_call_rewrites.get(&key).cloned()
        {
            let callee = (
                Expr::FieldAccess {
                    object: Box::new(receiver.clone()),
                    field: method.to_string(),
                },
                span.clone(),
            );
            self.check_callable_receiver(&field_ty.to_ty(), &callee);
        } else if let Some((root, path)) = self.expr_place(&receiver.0) {
            if self
                .env
                .place_move_conflict(&root, &path)
                .is_some_and(|(kind, _, _)| kind == crate::env::PlaceConflict::WholeOfPartial)
            {
                self.report_place_use_after_move(&root, &path, &receiver.1);
            }
        }
    }

    fn join_callable_type_list(&mut self, left: &[Ty], right: &[Ty]) -> Option<Vec<Ty>> {
        if left.len() != right.len() {
            return None;
        }
        left.iter()
            .zip(right)
            .map(|(left, right)| self.join_callable_types(left, right))
            .collect()
    }

    /// The one type two callable-bearing values share, erasing distinct
    /// closures to their written callable type; anything else must coerce.
    pub(super) fn join_callable_values(&mut self, left: &Ty, right: &Ty, span: &Span) -> Ty {
        let snapshot = self.subst.snapshot();
        if let Some(joined) = self.join_callable_types(left, right) {
            self.expect_type(&joined, left, span);
            self.expect_type(&joined, right, span);
            return joined;
        }
        self.subst.restore(snapshot);
        self.expect_type(left, right, span);
        self.subst.resolve(left)
    }

    pub(super) fn join_callable_types(&mut self, left: &Ty, right: &Ty) -> Option<Ty> {
        if left == right {
            return Some(left.clone());
        }
        match (left, right) {
            (
                Ty::Function {
                    capabilities: lc,
                    params: lp,
                    ret: lr,
                }
                | Ty::Closure {
                    capabilities: lc,
                    params: lp,
                    ret: lr,
                    ..
                },
                Ty::Function {
                    capabilities: rc,
                    params: rp,
                    ret: rr,
                }
                | Ty::Closure {
                    capabilities: rc,
                    params: rp,
                    ret: rr,
                    ..
                },
            ) => {
                if lp.len() != rp.len() {
                    return None;
                }
                let mut trial = self.subst.clone();
                for (left, right) in lp.iter().zip(rp).chain(std::iter::once((&**lr, &**rr))) {
                    self.try_unify_invariant_with_owner_identity(&mut trial, left, right)
                        .then_some(())?;
                }
                let capabilities = CallableCapabilities {
                    call: lc.call.max(rc.call),
                    clone: lc.clone && rc.clone,
                    suspends: lc.suspends || rc.suspends,
                };
                let params = lp.iter().map(|ty| trial.resolve(ty)).collect();
                let ret = Box::new(trial.resolve(lr));
                let joined = match (left, right) {
                    (
                        Ty::Closure { identity, .. },
                        Ty::Closure {
                            identity: right_identity,
                            ..
                        },
                    ) if identity == right_identity => trial.resolve(left),
                    _ => Ty::Function {
                        capabilities,
                        params,
                        ret,
                    },
                };
                if self.callable_erasure_loses_obligation(&joined, left)
                    || self.callable_erasure_loses_obligation(&joined, right)
                {
                    return None;
                }
                self.subst = trial;
                Some(joined)
            }
            (Ty::Tuple(left), Ty::Tuple(right)) => {
                self.join_callable_type_list(left, right).map(Ty::Tuple)
            }
            (
                Ty::Named {
                    name: left_name,
                    args: left,
                    builtin: left_builtin,
                },
                Ty::Named {
                    name: right_name,
                    args: right,
                    builtin: right_builtin,
                },
            ) if left_name == right_name
                && left_builtin == right_builtin
                && left.len() == right.len() =>
            {
                Some(Ty::Named {
                    name: left_name.clone(),
                    builtin: *left_builtin,
                    args: self.join_callable_type_list(left, right)?,
                })
            }
            (Ty::Array(left, left_len), Ty::Array(right, right_len)) if left_len == right_len => {
                self.join_callable_types(left, right)
                    .map(|ty| Ty::Array(Box::new(ty), *left_len))
            }
            _ => None,
        }
    }
}
