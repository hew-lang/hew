//! Message construction and submission share a checked protocol and policy.

use super::{
    scoped_module_item_name, ActorMethodKind, CallArg, CallTarget, Checker, Expr, Span, SpanKey,
    Spanned, Ty, TypeErrorKind,
};
use crate::actor_delivery::{self as delivery, ActorDeliveryCall, SendPolicy};

impl Checker {
    pub(super) fn is_actor_policy_builtin(&self, expr: &Expr) -> bool {
        let Expr::Identifier(name) = expr else {
            return false;
        };
        name == "policy"
            && self.env.lookup_ref(name).is_none()
            && !self.fn_def_spans.contains_key(name)
            && !scoped_module_item_name(self.canonical_fn_owner(), name)
                .is_some_and(|owner| self.fn_def_spans.contains_key(&owner))
            && matches!(self.builtin_call_targets.get(name), Some(CallTarget::Builtin { endpoint }) if endpoint == "policy")
    }

    pub(super) fn check_actor_policy(&mut self, args: &[CallArg], span: &Span) -> Ty {
        let [CallArg::Positional(target), CallArg::Named { name, value }] = args else {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                "sender policy requires `policy(actor, on_full: .Reject|.Wait|.DropNewest|.ReplaceLatest)`".to_string());
            return Ty::Error;
        };
        if name != "on_full" {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "sender policy option is named `on_full`".to_string(),
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
                "sender policy requires a local actor reference".to_string(),
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
                "sender policy must be a constant `OnFull` variant".to_string(),
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

    pub(super) fn check_actor_submission(&mut self, message: &Spanned<Expr>, span: &Span) -> Ty {
        let ty = self.synthesize(&message.0, &message.1);
        let ty = self.subst.resolve(&ty);
        let Some((_, _, policy)) = delivery::message_parts(&ty) else {
            self.report_error(
                TypeErrorKind::InvalidSend,
                span,
                "`send` requires an owned actor message description".to_string(),
            );
            return Ty::Error;
        };
        self.mark_expr_moved(&message.0, &message.1);
        self.actor_delivery_calls.insert(
            SpanKey::in_module(span, self.current_module_idx),
            ActorDeliveryCall::Submit { policy },
        );
        self.record_submission_suspension(span, policy.may_suspend());
        delivery::result_type(ty)
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
        if method != "to" || args.len() != 1 || args[0].name().is_some() {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "an owned message supports `.to(actor)` to change its destination".to_string(),
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
        self.record_submission_suspension(span, false);
        Some(delivery::message_type(
            target.clone(),
            payload.clone(),
            policy,
        ))
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
        let Some(signature) = self.fn_sigs.get(&method_id) else {
            return Ty::Error;
        };
        let mut argument_order = vec![None; signature.params.len()];
        for (index, arg) in args.iter().enumerate() {
            let position = match arg.name() {
                Some(name) => signature
                    .param_names
                    .iter()
                    .position(|parameter| parameter == name),
                None => Some(index),
            };
            let Some(slot) = position.and_then(|index| argument_order.get_mut(index)) else {
                return Ty::Error;
            };
            if slot.replace(index).is_some() {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    "message arguments must supply each receive parameter exactly once".to_string(),
                );
                return Ty::Error;
            }
        }
        let Some(argument_order) = argument_order.into_iter().collect::<Option<Vec<_>>>() else {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "message arguments must supply every receive parameter".to_string(),
            );
            return Ty::Error;
        };
        if let Some(reply_ty) = reply_ty {
            self.actor_method_dispatch.insert(
                key,
                ActorMethodKind::Ask {
                    method_id,
                    reply_ty,
                    argument_order,
                },
            );
            return result;
        }
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
            .collect();
        self.actor_method_dispatch.insert(
            key,
            ActorMethodKind::Message {
                method_id,
                policy,
                argument_order,
            },
        );
        self.record_submission_suspension(span, false);
        delivery::message_type(target.clone(), Ty::Tuple(payload), policy)
    }

    pub(super) fn reject_sealed_delivery_access(&mut self, ty: &Ty, span: &Span) -> bool {
        if matches!(ty, Ty::Named { name, builtin: None, .. } if matches!(name.as_str(), delivery::MESSAGE_TYPE | delivery::SENDER_TYPE))
        {
            self.report_error(TypeErrorKind::InvalidOperation, span,
                "actor message and sender fields are sealed; use receive calls, `policy`, `.to`, and `send`".to_string());
            true
        } else {
            false
        }
    }
}
