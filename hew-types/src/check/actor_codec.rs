//! The receive member a `RemotePid` addresses and whether its payloads cross
//! the wire. Only explicit `#[wire]` schemas are portable (D524).

use super::Checker;
use crate::actor_protocol::ActorProtocolDescriptor;
use crate::{BuiltinType, ResolvedTy, Ty};

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
                .all(|ty| self.is_wire_portable(ty));
        }
        self.actor_protocol_descriptors = protocols;
    }

    /// Project `A.Msg` and `A.Reply` through the actor's `ActorMsg` impl.
    fn actor_msg_types(&self, actor: &str) -> Option<(ResolvedTy, ResolvedTy)> {
        let base = Ty::actor_handle(actor, Vec::new());
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
        let Ty::Named { name, .. } = self.subst.resolve(actor) else {
            return true;
        };
        let canonical = self.canonical_nominal_name(&name).unwrap_or(name);
        let Some((msg, reply)) = self.actor_msg_types(&canonical) else {
            return true;
        };
        let mut ok = true;
        for ty in std::iter::once(&msg).chain(ask.then_some(&reply)) {
            if *ty == ResolvedTy::Unit || self.is_wire_portable(ty) {
                continue;
            }
            self.report_error_with_suggestions(
                super::TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "remote actor `{canonical}` cannot carry `{}`: a value sent to a \
                     remote actor needs an explicit wire schema",
                    ty.user_facing()
                ),
                vec![format!(
                    "declare `{}` with `#[wire]` and tag each field, e.g. `seq: i64 @1`",
                    ty.user_facing()
                )],
            );
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
                    "remote actor `{canonical}` has no receive fn taking its `ActorMsg.Msg` \
                     and returning its `ActorMsg.Reply`"
                ),
            );
            ok = false;
        }
        ok
    }

    /// Mirrors the SIR wire schema walk: scalars, collections of portable
    /// values, and declarations with a checked `#[wire]` layout.
    pub(super) fn is_wire_portable(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::String
            | ResolvedTy::Bytes => true,
            ResolvedTy::Named {
                builtin: Some(builtin),
                args,
                ..
            } => match (builtin, args.as_slice()) {
                (BuiltinType::Vec | BuiltinType::HashSet, [element]) => {
                    self.is_wire_portable(element)
                }
                (BuiltinType::HashMap, [key, value]) => {
                    self.is_wire_portable(key) && self.is_wire_portable(value)
                }
                (BuiltinType::Option, [value]) => {
                    !value.is_builtin(BuiltinType::Option) && self.is_wire_portable(value)
                }
                _ => false,
            },
            ResolvedTy::Named {
                name,
                builtin: None,
                args,
                is_opaque: false,
            } => args.is_empty() && self.wire_layouts.contains_key(name),
            _ => false,
        }
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
