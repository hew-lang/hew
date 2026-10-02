//! The receive member a `RemotePid` addresses and whether its payloads cross
//! the wire. A remote payload is data whose every record and enum is
//! `#[wire]` (D524).

use super::Checker;
use crate::actor_protocol::ActorProtocolDescriptor;
use crate::{ResolvedTy, Ty};

const ACTOR_MSG: &str = "std.builtins.ActorMsg";

impl Checker {
    /// Mark each actor's remote member: the receive fn its `ActorMsg` impl
    /// names, when every payload it carries has a wire schema.
    pub(super) fn select_actor_codecs(&mut self) {
        let mut protocols = std::mem::take(&mut self.actor_protocol_descriptors);
        for (actor, protocol) in &mut protocols {
            let Some((msg, reply)) = self.actor_msg_types(actor) else {
                continue;
            };
            let Some(index) = remote_member(protocol, &msg, &reply) else {
                continue;
            };
            let handler = &mut protocol.handlers[index];
            handler.remote_codec = handler
                .param_tys
                .iter()
                .chain((handler.return_ty != ResolvedTy::Unit).then_some(&handler.return_ty))
                .all(|ty| self.remote_payload_error(ty).is_none());
        }
        self.actor_protocol_descriptors = protocols;
    }

    /// Project `A.Msg` and `A.Reply` through the actor's `ActorMsg` impl.
    fn actor_msg_types(&self, actor: &str) -> Option<(ResolvedTy, ResolvedTy)> {
        let base = Ty::actor_handle(self.nominal_head_for_key(actor)?, Vec::new());
        let project = |assoc: &str| {
            let ty = self.project_assoc_types(&Ty::AssocType {
                base: Box::new(base.clone()),
                trait_name: ACTOR_MSG.into(),
                assoc_name: assoc.into(),
            });
            (!matches!(ty, Ty::AssocType { .. }))
                .then(|| ResolvedTy::from_ty(&ty).ok())
                .flatten()
        };
        Some((project("Msg")?, project("Reply")?))
    }

    /// Check a `RemotePid<A>` send or ask against A's remote member. Every
    /// payload must be scalar or declare a `#[wire]` schema with field tags;
    /// a send carries only the message.
    pub(super) fn check_remote_actor_payloads(
        &mut self,
        actor: &Ty,
        ask: bool,
        span: &hew_parser::ast::Span,
    ) -> bool {
        let Ty::Named { head, .. } = self.subst.resolve(actor) else {
            return true;
        };
        let name = head.registry_key();
        let canonical = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.to_string());
        let Some((msg, reply)) = self.actor_msg_types(&canonical) else {
            return true;
        };
        let mut ok = true;
        for ty in std::iter::once(&msg).chain(ask.then_some(&reply)) {
            if *ty == ResolvedTy::Unit {
                continue;
            }
            let Some(error) = self.remote_payload_error(ty) else {
                continue;
            };
            let owner = format!(
                "a value sent to remote actor `{canonical}` (`{}`)",
                ty.user_facing()
            );
            let message = Self::not_data_message(&owner, &error);
            if error.reason == crate::data_shape::NotDataReason::Untagged {
                self.report_error_with_suggestions(
                    super::TypeErrorKind::BoundsNotSatisfied,
                    span,
                    message,
                    vec![format!(
                        "declare `{}` with `#[wire]` and tag each member, e.g. `seq: i64 @1`",
                        error.ty.user_facing()
                    )],
                );
            } else {
                self.report_error(super::TypeErrorKind::BoundsNotSatisfied, span, message);
            }
            ok = false;
        }
        let addressed = self
            .actor_protocol_descriptors
            .get(&canonical)
            .is_some_and(|protocol| remote_member(protocol, &msg, &reply).is_some());
        if ok && !addressed {
            self.report_error(
                super::TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "remote actor `{canonical}` has no receive fn taking `{}` and returning \
                     `{}`, the message and reply its `ActorMsg` impl names",
                    msg.user_facing(),
                    reply.user_facing()
                ),
            );
            ok = false;
        }
        ok
    }
}

/// The single-parameter receive fn taking `msg` and returning `reply`.
fn remote_member(
    protocol: &ActorProtocolDescriptor,
    msg: &ResolvedTy,
    reply: &ResolvedTy,
) -> Option<usize> {
    let mut members = protocol.handlers.iter().enumerate().filter(|(_, handler)| {
        handler.param_tys.as_slice() == std::slice::from_ref(msg) && handler.return_ty == *reply
    });
    let (index, _) = members.next()?;
    members.next().is_none().then_some(index)
}
