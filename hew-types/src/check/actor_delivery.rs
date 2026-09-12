//! Message construction and submission share a checked protocol and policy.

use super::{
    scoped_module_item_name, ActorMethodKind, CallArg, CallTarget, Checker, Expr, Span, SpanKey,
    Spanned, Ty, TypeErrorKind,
};
use crate::actor_delivery::{self as delivery, ActorDeliveryCall, SendPolicy};

impl Checker {
    /// `mailbox(..)` and `policy(..)` are the two delivery views: one submits,
    /// one completes. Both are compiler builtins unless the program declares
    /// its own binding of that name.
    pub(super) fn actor_delivery_view_builtin(&self, expr: &Expr) -> Option<&'static str> {
        let Expr::Identifier(name) = expr else {
            return None;
        };
        let view = ["mailbox", "policy"]
            .into_iter()
            .find(|view| *view == name.as_str())?;
        (self.env.lookup_ref(name).is_none()
            && !self.fn_def_spans.contains_key(name)
            && !scoped_module_item_name(self.canonical_fn_owner(), name)
                .is_some_and(|owner| self.fn_def_spans.contains_key(&owner))
            && matches!(self.builtin_call_targets.get(name), Some(CallTarget::Builtin { endpoint }) if endpoint == view))
        .then_some(view)
    }

    pub(super) fn check_actor_delivery_view(
        &mut self,
        view: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let completes = view == "policy";
        let [CallArg::Positional(target), CallArg::Named { name, value }] = args else {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                format!("a delivery view requires `{view}(actor, on_full: .Reject|.Wait|.DropNewest|.ReplaceLatest)`"));
            return Ty::Error;
        };
        if name != "on_full" {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "the delivery view option is named `on_full`".to_string(),
            );
            return Ty::Error;
        }
        let target_ty = self.synthesize(&target.0, &target.1);
        let target_ty = self.subst.resolve(&target_ty);
        let target_ty = delivery::sender_parts(&target_ty)
            .or_else(|| delivery::policy_view_parts(&target_ty))
            .map_or(&target_ty, |(target, _)| target)
            .clone();
        // A lambda actor's handle carries its protocol rather than an actor
        // nominal, but it addresses an actor the same way, so it takes the
        // same two views.
        if !target_ty.addresses_local_actor() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &target.1,
                "a delivery view requires a local actor reference".to_string(),
            );
            return Ty::Error;
        }
        let on_full_ty = delivery::nominal(delivery::ON_FULL_TYPE, Vec::new());
        self.check_against(&value.0, &value.1, &on_full_ty);
        let policy = match &value.0 {
            Expr::ContextVariant(variant) if variant.record.is_none() => {
                match variant.name.as_str() {
                    "Reject" => Some(SendPolicy::Reject),
                    "Wait" => Some(SendPolicy::Wait),
                    "DropNewest" => Some(SendPolicy::DropNewest),
                    "ReplaceLatest" => Some(SendPolicy::ReplaceLatest),
                    _ => None,
                }
            }
            _ => None,
        };
        let Some(policy) = policy else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &value.1,
                "a delivery view's `on_full` must be a constant `OnFull` variant".to_string(),
            );
            return Ty::Error;
        };
        // A completion call waits for its own handler, so it cannot discard or
        // displace its own request and still have an outcome to report.
        if completes && matches!(policy, SendPolicy::DropNewest | SendPolicy::ReplaceLatest) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &value.1,
                "a completion view admits `.Wait` and `.Reject` only; a call cannot wait for a \
                 request it discards. Use `.Reject` to refuse a full mailbox, or `mailbox(..)` \
                 for a submission that may be discarded"
                    .to_string(),
            );
            return Ty::Error;
        }
        if policy == SendPolicy::ReplaceLatest {
            let permits_replacement = matches!(target_ty.as_local_actor_ref(), Some(Ty::Named { name, .. })
                if matches!(self.actor_overflow_policies.get(name), Some(hew_parser::ast::OverflowPolicy::Coalesce { .. })));
            if !permits_replacement {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "ReplaceLatest requires the actor protocol to opt into coalescing".to_string(),
                );
                return Ty::Error;
            }
        }
        self.actor_delivery_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ActorDeliveryCall::Policy { policy },
        );
        self.record_submission_suspension(span, false);
        if completes {
            delivery::policy_view_type(target_ty, policy)
        } else {
            delivery::sender_type(target_ty, policy)
        }
    }

    pub(super) fn check_actor_delivery_method(
        &mut self,
        receiver: &Spanned<Expr>,
        ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        if let Some((target, _)) =
            delivery::sender_parts(ty).or_else(|| delivery::policy_view_parts(ty))
        {
            // A lambda actor answers to `view(msg)` and nothing else: it
            // declares no named handler a method call could select.
            let Some(actor) = target.as_local_actor_ref().cloned() else {
                for arg in args {
                    let (expr, sp) = arg.expr();
                    self.synthesize(expr, sp);
                }
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "a lambda actor has no method `{method}`; call the view \
                         itself: `view(message)`"
                    ),
                );
                return Some(Ty::Error);
            };
            return Some(self.check_named_method_fallback(
                &actor,
                method,
                args,
                span,
                &actor.user_facing().to_string(),
            ));
        }
        let message_ty = match ty {
            Ty::Named { name, args, .. }
                if args.len() == 1
                    && (name == delivery::FAILURE_TYPE
                        || self.published_bare_type_qualified(name).as_deref()
                            == Some(delivery::FAILURE_TYPE)) =>
            {
                &args[0]
            }
            _ => ty,
        };
        let (target, payload, old_policy) = delivery::message_parts(message_ty)?;
        if let Some((method_id, params, success, failure)) = delivery::request_parts(payload) {
            return Some(self.check_request_recovery(
                receiver, message_ty, target, old_policy, method_id, params, success, failure,
                method, args, span,
            ));
        }
        if message_ty != ty {
            return None;
        }
        // A returned message has exactly two moves left: resubmit it as it
        // stands, or readdress it to a compatible actor and resubmit. Both
        // yield the same delivery outcome an ordinary call does.
        if method == "retry" && args.is_empty() {
            self.mark_expr_moved(&receiver.0, &receiver.1);
            self.actor_delivery_calls.insert(
                SpanKey::in_module(span, self.current_module_idx),
                ActorDeliveryCall::Submit { policy: old_policy },
            );
            self.record_submission_suspension(span, old_policy.may_suspend());
            return Some(delivery::result_type(ty.clone()));
        }
        if method != "to" || args.len() != 1 || args[0].name().is_some() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "a returned message supports `.retry()` and `.to(actor)`".to_string(),
            );
            return Some(Ty::Error);
        }
        let destination = args[0].expr();
        let dest_ty = self.synthesize(&destination.0, &destination.1);
        let dest_ty = self.subst.resolve(&dest_ty);
        let view = delivery::sender_parts(&dest_ty);
        let (new_target, policy) = view.unwrap_or((&dest_ty, old_policy));
        self.expect_type(target, new_target, &destination.1);
        self.mark_expr_moved(&receiver.0, &receiver.1);
        self.actor_delivery_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ActorDeliveryCall::Readdress {
                policy,
                target_is_view: view.is_some(),
            },
        );
        self.record_submission_suspension(span, policy.may_suspend());
        Some(delivery::result_type(delivery::message_type(
            target.clone(),
            payload.clone(),
            policy,
        )))
    }

    /// Pair each call argument with its receive parameter position, so every
    /// downstream stage consumes declaration order rather than call order.
    fn receive_argument_order(
        &mut self,
        method_id: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Vec<usize>> {
        let signature = self.fn_sigs.get(method_id)?;
        let mut argument_order = vec![None; signature.params.len()];
        for (index, arg) in args.iter().enumerate() {
            let position = match arg.name() {
                Some(name) => signature
                    .param_names
                    .iter()
                    .position(|parameter| parameter == name),
                None => Some(index),
            };
            let slot = position.and_then(|index| argument_order.get_mut(index))?;
            if slot.replace(index).is_some() {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "message arguments must supply each receive parameter exactly once".to_string(),
                );
                return None;
            }
        }
        let order = argument_order.into_iter().collect::<Option<Vec<_>>>();
        if order.is_none() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "message arguments must supply every receive parameter".to_string(),
            );
        }
        order
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one receive boundary records argument order, admission and completion"
    )]
    pub(super) fn finish_actor_receive_call(
        &mut self,
        receiver: &Spanned<Expr>,
        args: &[CallArg],
        span: &Span,
        result: Ty,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
        // A lambda actor's dispatch is complete when it is recorded: it names
        // no `receive fn` this pass could resolve arguments against.
        if matches!(self.actor_method_dispatch.get(&key),
            Some(ActorMethodKind::Ask { method_id, .. } | ActorMethodKind::Message { method_id, .. })
                if method_id == crate::actor_protocol::LAMBDA_ACTOR_METHOD_ID)
        {
            return result;
        }
        let (method_id, reply_ty) = match self.actor_method_dispatch.get(&key).cloned() {
            Some(ActorMethodKind::Message { method_id, .. }) => (method_id, None),
            Some(ActorMethodKind::Ask {
                method_id,
                reply_ty,
                ..
            }) => (method_id, Some(reply_ty)),
            _ => return result,
        };
        let Some(receiver_ty) = self
            .expr_types
            .get(&SpanKey::in_module(&receiver.1, self.current_module_idx))
        else {
            return Ty::Error;
        };
        let receiver_ty = self.subst.resolve(receiver_ty);
        let submitting_view = delivery::sender_parts(&receiver_ty);
        let (target, policy) = submitting_view.unwrap_or((&receiver_ty, SendPolicy::Reject));
        let Some(argument_order) = self.receive_argument_order(&method_id, args, span) else {
            return Ty::Error;
        };
        let through_view = submitting_view.is_some();
        // Private actor methods share the caller's state seat. Deferred closure
        // and generator bodies have their own effect identity and do not inherit it.
        let owns_actor_turn = self.in_actor_handler_context
            || (self.current_actor_type.is_some()
                && matches!(
                    self.effect_graph.current_body,
                    Some(super::effects::EffectBody::Declaration(_))
                ));
        if !through_view
            && owns_actor_turn
            && matches!(&receiver.0, Expr::Identifier(name) if name == "self")
            && !self.suspension_operands.contains(&key)
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "a completion call to self waits for the actor turn that is already running; \
                 use mailbox(self, on_full: .Reject) for a one-way submission"
                    .into(),
            );
        }
        // A `policy(..)` view completes like a bare handle; only its admission
        // behaviour differs, so it selects the call's own policy.
        let completion_policy = delivery::policy_view_parts(&receiver_ty)
            .map_or(SendPolicy::Wait, |(_, policy)| policy);
        // The envelope stores the declared protocol after argument coercion,
        // not a closure's concrete environment or a literal's narrower type.
        let protocol_target =
            delivery::policy_view_parts(target).map_or(target, |(target, _)| target);
        let Some((payload, _)) = self.request_signature(&method_id, protocol_target) else {
            return Ty::Error;
        };
        // A `fails` handler whose success is unit owes the caller no value, so
        // a mailbox view may submit it one way. Its declared failure then has
        // no caller to answer and becomes the actor's own fault; the fault
        // text comes from the error's rendering, so record the site for the
        // checker's later renderability proof.
        let fails_one_way = matches!(&reply_ty, Some(ty)
            if self.receive_fails_methods.contains(&method_id)
                && matches!(ty.as_result(), Some((success, _)) if matches!(self.subst.resolve(success), Ty::Unit)));
        if through_view && fails_one_way {
            self.view_submitted_fails_methods
                .insert(method_id.clone(), span.clone());
        }
        if let Some(reply_ty) = reply_ty {
            if through_view {
                if !fails_one_way {
                    self.reject_replying_handler_through_view(&method_id, span);
                    return Ty::Error;
                }
            } else {
                let completion = self.completion_request_type(
                    &method_id,
                    &reply_ty,
                    &receiver_ty,
                    completion_policy,
                    &payload,
                );
                self.record_completion_call_edge(&method_id, span);
                self.actor_method_dispatch.insert(
                    key,
                    ActorMethodKind::Ask {
                        method_id,
                        reply_ty,
                        policy: completion_policy,
                        argument_order,
                    },
                );
                return completion;
            }
        }
        if !through_view {
            // The call on an actor handle is a completion call: it waits for
            // the handler to finish and yields its unit reply, exactly as a
            // value-returning handler yields its own.
            let completion = self.completion_request_type(
                &method_id,
                &Ty::Unit,
                &receiver_ty,
                completion_policy,
                &payload,
            );
            self.record_completion_call_edge(&method_id, span);
            self.actor_method_dispatch.insert(
                key,
                ActorMethodKind::Ask {
                    method_id,
                    reply_ty: Ty::Unit,
                    policy: completion_policy,
                    argument_order,
                },
            );
            self.record_submission_suspension(span, true);
            return completion;
        }
        self.actor_method_dispatch.insert(
            key,
            ActorMethodKind::Message {
                method_id,
                policy,
                argument_order,
            },
        );
        // The call is the send: a `receive fn` without a reply submits at its
        // call site, under the policy its receiver view carries.
        self.record_submission_suspension(span, policy.may_suspend());
        delivery::result_type(delivery::message_type(
            target.clone(),
            Ty::Tuple(payload),
            policy,
        ))
    }

    /// Record one handler-to-handler completion call. A completion call waits
    /// for the callee's handler to finish, so a cycle among these edges is a
    /// deadlock: every actor in the ring is blocked on the next.
    fn record_completion_call_edge(&mut self, callee: &str, span: &Span) {
        if !self.in_receive_fn {
            return;
        }
        let Some(enclosing) = self.current_function.clone() else {
            return;
        };
        if !self.actor_receive_methods.contains(&enclosing) {
            return;
        }
        self.completion_call_edges
            .push((enclosing, callee.to_string(), span.clone()));
    }

    /// Report every completion-call cycle a handle resolves statically. This
    /// sees only calls whose target actor the checker knows at the call site;
    /// a ring formed through a handle passed at runtime still deadlocks and is
    /// left to the runtime's own wait-cycle detection.
    pub(super) fn report_completion_call_cycles(&mut self) {
        use std::collections::{BTreeMap, BTreeSet};
        let mut edges: BTreeMap<&str, Vec<(&str, &Span)>> = BTreeMap::new();
        for (caller, callee, span) in &self.completion_call_edges {
            edges
                .entry(caller.as_str())
                .or_default()
                .push((callee.as_str(), span));
        }
        let mut reported: BTreeSet<Vec<String>> = BTreeSet::new();
        let mut findings = Vec::new();
        for start in edges.keys().copied() {
            let mut path: Vec<(&str, &Span)> = vec![(start, edges[start][0].1)];
            walk(start, &edges, &mut path, &mut findings, &mut reported);
            path.pop();
        }
        for (ring, span) in findings {
            let path = ring
                .iter()
                .map(|name| name.replace("::", "."))
                .collect::<Vec<_>>()
                .join(" -> ");
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &span,
                format!(
                    "completion calls form a cycle: {path}. Every handler in the ring waits for \
                     the next, so none can finish; make one leg a `mailbox(..)` submission or \
                     `fork` it"
                ),
            );
        }
    }

    /// A mailbox view submits and nothing more, so a handler that owes the
    /// caller a value cannot be called through one.
    fn reject_replying_handler_through_view(&mut self, method_id: &str, span: &Span) {
        let handler = method_id
            .rsplit_once("::")
            .map_or("this handler", |(_, name)| name);
        self.report_error(
            TypeErrorKind::InvalidOperation,
            span,
            format!(
                "`{handler}` returns a value, so it cannot be called through a mailbox view, \
                 which only submits; call it on the actor handle to wait for the reply, or \
                 `fork target.{handler}(..)` to run it concurrently"
            ),
        );
    }

    /// The result of a completion call: the handler's success value, or an
    /// `ActorError` carrying the handler's declared failure. A handler without
    /// a `fails` clause can never produce `Failed`, so its error parameter is
    /// the uninhabited `Never`.
    fn completion_call_type(&mut self, method_id: &str, reply_ty: &Ty) -> Ty {
        let (success, failure) = match reply_ty.as_result() {
            Some((success, failure)) if self.receive_fails_methods.contains(method_id) => {
                (success.clone(), failure.clone())
            }
            _ => (reply_ty.clone(), Ty::never_type()),
        };
        Ty::result(success, Ty::actor_error(failure))
    }

    pub(super) fn completion_request_type(
        &mut self,
        method_id: &str,
        reply: &Ty,
        receiver: &Ty,
        policy: SendPolicy,
        params: &[Ty],
    ) -> Ty {
        let completion = self.completion_call_type(method_id, reply);
        if policy != SendPolicy::Reject {
            return completion;
        }
        self.register_request_protocol(method_id);
        let (success, error) = completion.as_result().unwrap();
        let Ty::Named { args, .. } = error else {
            unreachable!()
        };
        let failure = args[0].clone();
        let target = delivery::policy_view_parts(receiver).map_or(receiver, |(target, _)| target);
        let params = if method_id == crate::actor_protocol::LAMBDA_ACTOR_METHOD_ID {
            params.to_vec()
        } else {
            let Some((params, _)) = self.request_signature(method_id, target) else {
                return Ty::Error;
            };
            params
        };
        let request = delivery::message_type(
            target.clone(),
            delivery::request_type(
                method_id,
                Ty::Tuple(params),
                success.clone(),
                failure.clone(),
            ),
            policy,
        );
        Ty::result(
            success.clone(),
            Ty::actor_error_with_request(failure, request),
        )
    }

    /// Specialize the declaration's protocol, never the argument expression's
    /// pre-coercion type, before sealing the runtime wrapper.
    fn request_signature(&self, method: &str, target: &Ty) -> Option<(Vec<Ty>, Ty)> {
        let signature = self.fn_sigs.get(method)?;
        let Ty::Named { name, args, .. } = target.as_local_actor_ref()? else {
            return None;
        };
        let declaration = self.type_defs.get(name)?;
        let substitutions = declaration
            .type_params
            .iter()
            .cloned()
            .zip(args.iter().cloned())
            .collect::<std::collections::HashMap<_, _>>();
        let resolve = |ty: &Ty| {
            self.subst
                .resolve(&ty.substitute_named_params_parallel(&substitutions))
        };
        Some((
            signature.params.iter().map(resolve).collect(),
            resolve(&signature.return_type),
        ))
    }

    /// A protocol witness has no runtime fields. Its identity is the checked
    /// receive declaration; its parameters preserve the concrete signature.
    /// Register it here so later specialization consumes checker facts even
    /// when the request has crossed a binding or generic function boundary.
    fn register_request_protocol(&mut self, method_id: &str) {
        self.type_defs
            .entry(method_id.to_string())
            .or_insert_with(|| super::TypeDef {
                kind: super::TypeDefKind::Struct,
                name: method_id.to_string(),
                type_params: vec!["Params".into(), "Reply".into(), "Failure".into()],
                bounds: std::collections::HashMap::new(),
                fields: std::collections::HashMap::new(),
                field_order: Vec::new(),
                variants: std::collections::HashMap::new(),
                methods: std::collections::HashMap::new(),
                doc_comment: None,
                is_indirect: false,
            });
    }

    #[allow(
        clippy::too_many_arguments,
        reason = "the sealed protocol supplies the complete recovery contract"
    )]
    fn check_request_recovery(
        &mut self,
        receiver: &Spanned<Expr>,
        message: &Ty,
        target: &Ty,
        policy: SendPolicy,
        method_id: &str,
        params: &Ty,
        success: &Ty,
        failure: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Ty {
        let redirect = match (method, args) {
            ("retry", []) => false,
            ("to", [arg]) if arg.name().is_none() => true,
            _ => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "a returned request supports `.retry()` and `.to(actor)`".into(),
                );
                return Ty::Error;
            }
        };
        let mut destination = target.clone();
        let mut destination_method = method_id.to_string();
        if redirect {
            let argument = args[0].expr();
            let ty = self.synthesize(&argument.0, &argument.1);
            destination = self.subst.resolve(&ty);
            if method_id == crate::actor_protocol::LAMBDA_ACTOR_METHOD_ID {
                self.expect_type(target, &destination, &argument.1);
            } else {
                let Some(Ty::Named { name, .. }) = destination.as_local_actor_ref() else {
                    self.report_error(
                        TypeErrorKind::InvalidOperation,
                        &argument.1,
                        "request redirection requires a compatible actor handle".into(),
                    );
                    return Ty::Error;
                };
                let handler = method_id.rsplit("::").next().unwrap_or(method_id);
                destination_method = crate::actor_protocol::qualified_handler_name(name, handler);
                let signature = self.request_signature(&destination_method, &destination);
                let compatible = signature.is_some_and(|(parameters, reply)| {
                    Ty::Tuple(parameters) == *params
                        && self.completion_call_type(&destination_method, &reply)
                            == Ty::result(success.clone(), Ty::actor_error(failure.clone()))
                });
                if !compatible {
                    self.report_error(TypeErrorKind::InvalidOperation, &argument.1,
                    format!("request for `{method_id}` cannot be redirected to `{destination_method}`: the handler name, parameters and reply must agree"));
                    return Ty::Error;
                }
            }
        }
        self.mark_expr_moved(&receiver.0, &receiver.1);
        self.register_request_protocol(&destination_method);
        self.actor_delivery_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ActorDeliveryCall::Resume {
                policy,
                method_id: destination_method.clone(),
                redirect,
            },
        );
        self.record_submission_suspension(span, true);
        let request = if redirect {
            delivery::message_type(
                destination,
                delivery::request_type(
                    &destination_method,
                    params.clone(),
                    success.clone(),
                    failure.clone(),
                ),
                policy,
            )
        } else {
            message.clone()
        };
        Ty::result(
            success.clone(),
            Ty::actor_error_with_request(failure.clone(), request),
        )
    }

    pub(super) fn reject_sealed_delivery_access(&mut self, ty: &Ty, span: &Span) -> bool {
        if matches!(ty, Ty::Named { name, builtin: None, .. } if matches!(name.as_str(), delivery::MESSAGE_TYPE | delivery::SENDER_TYPE | delivery::POLICY_VIEW_TYPE | delivery::REQUEST_TYPE))
        {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                "actor message and view fields are sealed; use receive calls, `mailbox`, `policy`, `.retry` and `.to`".to_string());
            true
        } else {
            false
        }
    }
}

/// Depth-first walk of the completion-call graph, reporting the first time it
/// re-enters a handler already on the current path.
fn walk<'a>(
    node: &'a str,
    edges: &std::collections::BTreeMap<&'a str, Vec<(&'a str, &'a Span)>>,
    path: &mut Vec<(&'a str, &'a Span)>,
    findings: &mut Vec<(Vec<String>, Span)>,
    reported: &mut std::collections::BTreeSet<Vec<String>>,
) {
    for (callee, span) in edges.get(node).into_iter().flatten() {
        if let Some(entry) = path.iter().position(|(name, _)| name == callee) {
            let mut ring: Vec<String> = path[entry..]
                .iter()
                .map(|(name, _)| (*name).to_string())
                .collect();
            ring.push((*callee).to_string());
            let mut key = ring.clone();
            key.sort();
            key.dedup();
            if reported.insert(key) {
                findings.push((ring, (*span).clone()));
            }
            continue;
        }
        path.push((callee, span));
        walk(callee, edges, path, findings, reported);
        path.pop();
    }
}
