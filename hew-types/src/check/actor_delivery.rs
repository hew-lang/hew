//! Message construction and submission share a checked protocol and policy.

use super::{
    scoped_module_item_name, ActorMethodKind, CallArg, CallTarget, Checker, Expr, Span, SpanKey,
    Spanned, Ty, TypeErrorKind,
};
use crate::actor_delivery::{self as delivery, ActorDeliveryCall, SendPolicy};

impl Checker {
    pub(super) fn is_actor_mailbox_builtin(&self, expr: &Expr) -> bool {
        let Expr::Identifier(name) = expr else {
            return false;
        };
        name == "mailbox"
            && self.env.lookup_ref(name).is_none()
            && !self.fn_def_spans.contains_key(name)
            && !scoped_module_item_name(self.canonical_fn_owner(), name)
                .is_some_and(|owner| self.fn_def_spans.contains_key(&owner))
            && matches!(self.builtin_call_targets.get(name), Some(CallTarget::Builtin { endpoint }) if endpoint == "mailbox")
    }

    pub(super) fn check_actor_mailbox(&mut self, args: &[CallArg], span: &Span) -> Ty {
        let [CallArg::Positional(target), CallArg::Named { name, value }] = args else {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                "a mailbox view requires `mailbox(actor, on_full: .Reject|.Wait|.DropNewest|.ReplaceLatest)`".to_string());
            return Ty::Error;
        };
        if name != "on_full" {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "the mailbox view option is named `on_full`".to_string(),
            );
            return Ty::Error;
        }
        let target_ty = self.synthesize(&target.0, &target.1);
        let target_ty = self.subst.resolve(&target_ty);
        let target_ty = delivery::sender_parts(&target_ty)
            .map_or(&target_ty, |(target, _)| target)
            .clone();
        let Some(actor_ty) = target_ty.as_local_actor_ref() else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &target.1,
                "a mailbox view requires a local actor reference".to_string(),
            );
            return Ty::Error;
        };
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
                "a mailbox view's `on_full` must be a constant `OnFull` variant".to_string(),
            );
            return Ty::Error;
        };
        if policy == SendPolicy::ReplaceLatest {
            let permits_replacement = matches!(actor_ty, Ty::Named { name, .. }
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
        delivery::sender_type(target_ty, policy)
    }

    pub(super) fn check_actor_delivery_method(
        &mut self,
        receiver: &Spanned<Expr>,
        ty: &Ty,
        method: &str,
        args: &[CallArg],
        span: &Span,
    ) -> Option<Ty> {
        if let Some((target, _)) = delivery::sender_parts(ty) {
            let actor = target
                .as_local_actor_ref()
                .expect("checked sender protocol")
                .clone();
            return Some(self.check_named_method_fallback(
                &actor,
                method,
                args,
                span,
                &actor.user_facing().to_string(),
            ));
        }
        let (target, payload, old_policy) = delivery::message_parts(ty)?;
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

    pub(super) fn finish_actor_receive_call(
        &mut self,
        receiver: &Spanned<Expr>,
        args: &[CallArg],
        span: &Span,
        result: Ty,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
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
        let (target, policy) =
            delivery::sender_parts(&receiver_ty).unwrap_or((&receiver_ty, SendPolicy::Reject));
        let Some(argument_order) = self.receive_argument_order(&method_id, args, span) else {
            return Ty::Error;
        };
        let through_view = delivery::sender_parts(&receiver_ty).is_some();
        let payload = argument_order
            .iter()
            .map(|index| {
                self.expr_types
                    .get(&SpanKey::in_module(
                        &args[*index].expr().1,
                        self.current_module_idx,
                    ))
                    .map_or(Ty::Error, |ty| self.subst.resolve(ty))
            })
            .collect::<Vec<_>>();
        if let Some(reply_ty) = reply_ty {
            if through_view {
                let handler = method_id
                    .rsplit_once("::")
                    .map_or("this handler", |(_, name)| name);
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "`{handler}` returns a value, so it cannot be called through a mailbox \
                         view, which only submits; call it on the actor handle to wait for the \
                         reply, or `fork target.{handler}(..)` to run it concurrently"
                    ),
                );
                return Ty::Error;
            }
            let completion = self.completion_call_type(
                &method_id,
                &reply_ty,
                target,
                Ty::Tuple(payload),
                SendPolicy::Wait,
            );
            self.actor_method_dispatch.insert(
                key,
                ActorMethodKind::Ask {
                    method_id,
                    reply_ty,
                    argument_order,
                },
            );
            return completion;
        }
        if !through_view {
            // The call on an actor handle is a completion call: it waits for
            // the handler to finish and yields its unit reply, exactly as a
            // value-returning handler yields its own.
            let completion = self.completion_call_type(
                &method_id,
                &Ty::Unit,
                target,
                Ty::Tuple(payload),
                SendPolicy::Wait,
            );
            self.actor_method_dispatch.insert(
                key,
                ActorMethodKind::Ask {
                    method_id,
                    reply_ty: Ty::Unit,
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

    /// The result of a completion call: the handler's success value, or an
    /// `ActorError` carrying the handler's declared failure and the call's own
    /// sealed message. A handler without a `fails` clause can never produce
    /// `Failed`, so its error parameter is the uninhabited `Never`.
    fn completion_call_type(
        &mut self,
        method_id: &str,
        reply_ty: &Ty,
        target: &Ty,
        payload: Ty,
        policy: SendPolicy,
    ) -> Ty {
        let (success, failure) = match reply_ty.as_result() {
            Some((success, failure)) if self.receive_fails_methods.contains(method_id) => {
                (success.clone(), failure.clone())
            }
            _ => (reply_ty.clone(), Ty::never_type()),
        };
        Ty::result(
            success,
            Ty::actor_error(
                failure,
                delivery::message_type(target.clone(), payload, policy),
            ),
        )
    }

    pub(super) fn reject_sealed_delivery_access(&mut self, ty: &Ty, span: &Span) -> bool {
        if matches!(ty, Ty::Named { name, builtin: None, .. } if matches!(name.as_str(), delivery::MESSAGE_TYPE | delivery::SENDER_TYPE))
        {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                "actor message and sender fields are sealed; use receive calls, `policy`, `.retry` and `.to`".to_string());
            true
        } else {
            false
        }
    }
}
