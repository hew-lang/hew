//! Resolve the checker-selected remote member's payloads to wire schemas.

use super::InstanceService;
use hew_types::ResolvedTy;

impl InstanceService<'_> {
    /// Demand every actor with a remote member so its codecs register at
    /// startup even when this program never addresses it by handle.
    pub(super) fn request_actor_codec_roots(&mut self) -> Result<(), String> {
        let roots = self
            .module
            .items
            .iter()
            .filter_map(|item| {
                let hew_hir::HirItem::Actor(actor) = item else {
                    return None;
                };
                if !actor.type_params.is_empty() || actor.lambda_handle_ty.is_some() {
                    return None;
                }
                actor
                    .protocol_descriptor
                    .as_ref()
                    .filter(|protocol| protocol.handlers.iter().any(|handler| handler.remote_codec))
                    .map(|_| {
                        ResolvedTy::named_builtin(
                            actor.declaration.full_path(),
                            hew_types::BuiltinType::ActorHandle,
                            Vec::new(),
                        )
                    })
            })
            .collect::<Vec<_>>();
        for ty in roots {
            self.require_actor(&ty)?;
        }
        Ok(())
    }

    pub(super) fn resolve_actor_codec(
        &mut self,
        params: &[ResolvedTy],
        reply: &ResolvedTy,
    ) -> Result<crate::actor::SemActorCodec, String> {
        Ok(crate::actor::SemActorCodec {
            params: params
                .iter()
                .map(|ty| self.wire_plan(ty, &mut Vec::new()))
                .collect::<Result<_, _>>()?,
            reply: (*reply != ResolvedTy::Unit)
                .then(|| self.wire_plan(reply, &mut Vec::new()))
                .transpose()?,
        })
    }
}
