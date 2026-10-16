//! The receive member a `RemotePid` addresses and whether its payloads cross
//! the wire. A remote payload is data whose every record and enum is
//! `#[wire]` (D524).

use super::Checker;
use crate::actor_protocol::ActorProtocolDescriptor;
use crate::{ResolvedTy, Ty};

const ACTOR_MSG: &str = "std.builtins.ActorMsg";

impl Checker {
    pub(super) fn select_service_codecs(&mut self, program: &hew_parser::ast::Program) {
        fn collect(ty: &Ty, actors: &mut Vec<Ty>) {
            if let Ty::Named { head, args } = ty {
                if [crate::KnownDecl::Service, crate::KnownDecl::ServiceExport]
                    .into_iter()
                    .any(|known| head.nominal() == Some(known.nominal()))
                {
                    actors.extend(args.iter().cloned());
                }
            }
            ty.map_children(&mut |child| {
                collect(child, actors);
                child.clone()
            });
        }
        let mut instances = Vec::new();
        for (site, ty) in &self.expr_types {
            let mut actors = Vec::new();
            collect(&self.subst.resolve(ty), &mut actors);
            instances.extend(actors.into_iter().map(|actor| (actor, site.clone())));
        }
        let indices = program
            .module_graph
            .as_ref()
            .map(hew_parser::module::ModuleGraph::file_span_indices);
        let mut seen = std::collections::HashSet::new();
        for (instance, site) in instances {
            if !seen.insert(instance.clone()) {
                continue;
            }
            if instance.has_inference_var() || instance.has_type_parameters() {
                continue;
            }
            self.select_service_instance(&instance, &site, indices.as_ref());
        }
    }

    fn select_service_instance(
        &mut self,
        instance: &Ty,
        site: &crate::SpanKey,
        indices: Option<&hew_parser::module::FileSpanIndices>,
    ) {
        let Ty::Named {
            head: crate::TypeHead::Actor(head),
            args,
        } = instance
        else {
            if !instance.has_inference_var() && !instance.has_type_parameters() {
                let mut error = crate::TypeError::new(
                    super::TypeErrorKind::InvalidOperation,
                    site.start..site.end,
                    "a supplied service names a concrete actor interface",
                );
                error.source_module = indices
                    .and_then(|indices| indices.module_name(site.module_idx))
                    .map(str::to_owned);
                self.errors.push(error);
            }
            return;
        };
        let Some(definition) = self.head_type_def(crate::TypeHead::Actor(*head)) else {
            return;
        };
        let substitutions = definition
            .type_params
            .iter()
            .copied()
            .zip(args.iter().cloned())
            .collect();
        let actor_key = self.defs.path(head.id.declaration()).to_owned();
        let Some(protocol) = self.actor_protocol_descriptors.get(&actor_key) else {
            return;
        };
        let mut valid = !protocol.handlers.is_empty();
        for handler in &protocol.handlers {
            for ty in handler
                .param_tys
                .iter()
                .chain(std::iter::once(&handler.return_ty))
            {
                let ty = self
                    .normalize_for_use(&ty.to_ty().substitute_type_params_parallel(&substitutions));
                let Ok(ty) = ResolvedTy::from_ty(&ty) else {
                    valid = false;
                    continue;
                };
                if let Some(reason) = self.remote_payload_error(&ty) {
                    valid = false;
                    let message = Self::not_data_message(
                        &format!("service handler `{}`", handler.wire_identity),
                        &reason,
                    );
                    let mut error = crate::TypeError::new(
                        super::TypeErrorKind::BoundsNotSatisfied,
                        site.start..site.end,
                        message,
                    );
                    error.source_module = indices
                        .and_then(|indices| indices.module_name(site.module_idx))
                        .map(str::to_owned);
                    self.errors.push(error);
                }
            }
        }
        if !valid {
            return;
        }
        let Ok(args) = args
            .iter()
            .map(ResolvedTy::from_ty)
            .collect::<Result<Vec<_>, _>>()
        else {
            return;
        };
        let protocol = self
            .actor_protocol_descriptors
            .get_mut(&actor_key)
            .expect("selected actor protocol");
        for handler in &mut protocol.handlers {
            let receiver = crate::actor_protocol::ActorRemoteReceiver {
                interface: crate::actor_protocol::RemoteActorInterface::Service,
                parameters: Vec::new(),
                args: args.clone(),
            };
            if !handler.remote_receivers.contains(&receiver) {
                handler.remote_receivers.push(receiver);
            }
        }
    }

    /// Select each receive member through its checked `ActorMsg` impl receiver.
    /// Concrete instances reuse ordinary impl matching and wire admission.
    pub(super) fn select_actor_codecs(&mut self) {
        let Some(trait_id) = self.assoc_carrier_trait(ACTOR_MSG) else {
            return;
        };
        let mut protocols = std::mem::take(&mut self.actor_protocol_descriptors);
        for (actor, protocol) in &mut protocols {
            let Some(nominal) = self.nominal_head_for_key(actor) else {
                continue;
            };
            let Some(definition) = self.head_type_def(crate::TypeHead::Actor(nominal)) else {
                continue;
            };
            let Some(key) = Self::impl_self_key(&Ty::actor_handle(nominal, Vec::new())) else {
                continue;
            };
            let Some(bindings) = self.impl_assoc_type_bindings.get(&(
                key,
                trait_id,
                hew_parser::ast::Symbol::intern("Msg"),
            )) else {
                continue;
            };
            for binding in bindings {
                let receiver = self.normalize_for_use(&binding.receiver);
                let Ty::Named { args, .. } = &receiver else {
                    continue;
                };
                let Some((msg, reply)) = self.actor_msg_types(&receiver) else {
                    continue;
                };
                let substitutions = definition
                    .type_params
                    .iter()
                    .copied()
                    .zip(args.iter().cloned())
                    .collect();
                let Some(index) = remote_member(protocol, &msg, &reply, |ty| {
                    self.normalize_for_use(
                        &ty.to_ty().substitute_type_params_parallel(&substitutions),
                    )
                }) else {
                    continue;
                };
                let Ok(args) = args
                    .iter()
                    .map(ResolvedTy::from_ty)
                    .collect::<Result<Vec<_>, _>>()
                else {
                    continue;
                };
                protocol.handlers[index].remote_receivers.push(
                    crate::actor_protocol::ActorRemoteReceiver {
                        interface: crate::actor_protocol::RemoteActorInterface::Message,
                        parameters: binding.parameters.clone(),
                        args,
                    },
                );
            }
        }
        self.actor_protocol_descriptors = protocols;
    }

    /// Project `A.Msg` and `A.Reply` through the actor's `ActorMsg` impl.
    fn actor_msg_types(&self, base: &Ty) -> Option<(ResolvedTy, ResolvedTy)> {
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
        let actor = self.subst.resolve(actor);
        let Ty::Named { head, args } = &actor else {
            return true;
        };
        let name = head.registry_key();
        let canonical = self
            .canonical_nominal_name(name)
            .unwrap_or_else(|| name.to_string());
        let Some((msg, reply)) = self.actor_msg_types(&actor) else {
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
        let substitutions = self
            .head_type_def(*head)
            .map(|definition| {
                definition
                    .type_params
                    .iter()
                    .copied()
                    .zip(args.iter().cloned())
                    .collect()
            })
            .unwrap_or_default();
        let addressed = self
            .actor_protocol_descriptors
            .get(&canonical)
            .is_some_and(|protocol| {
                remote_member(protocol, &msg, &reply, |ty| {
                    self.normalize_for_use(
                        &ty.to_ty().substitute_type_params_parallel(&substitutions),
                    )
                })
                .is_some()
            });
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
    specialize: impl Fn(&ResolvedTy) -> Ty,
) -> Option<usize> {
    let mut members = protocol.handlers.iter().enumerate().filter(|(_, handler)| {
        matches!(handler.param_tys.as_slice(), [param] if specialize(param) == msg.to_ty())
            && specialize(&handler.return_ty) == reply.to_ty()
    });
    let (index, _) = members.next()?;
    members.next().is_none().then_some(index)
}
