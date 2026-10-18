//! The receive members an imported actor handle addresses and whether their payloads cross
//! the wire. A remote payload is data whose every record and enum is
//! `#[wire]` (D524).

use super::Checker;
use crate::{ResolvedTy, Ty};

impl Checker {
    pub(super) fn select_wire_actor_codecs(&mut self, program: &hew_parser::ast::Program) {
        fn collect(ty: &Ty, actors: &mut Vec<Ty>) {
            if matches!(
                ty,
                Ty::Named {
                    head: crate::TypeHead::Actor(_),
                    ..
                }
            ) {
                actors.push(ty.clone());
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
            self.select_wire_actor_instance(&instance, &site, indices.as_ref());
        }
    }

    fn select_wire_actor_instance(
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
        let mut valid = protocol.handlers.iter().any(|handler| handler.wire);
        for handler in protocol.handlers.iter().filter(|handler| handler.wire) {
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
                        &format!("wire handler `{}`", handler.wire_identity),
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
        for handler in protocol.handlers.iter_mut().filter(|handler| handler.wire) {
            let receiver = crate::actor_protocol::ActorRemoteReceiver {
                parameters: Vec::new(),
                args: args.clone(),
            };
            if !handler.remote_receivers.contains(&receiver) {
                handler.remote_receivers.push(receiver);
            }
        }
    }
}
