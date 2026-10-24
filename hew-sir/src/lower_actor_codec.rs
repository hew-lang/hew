//! Resolve the checker-selected remote member's payloads to wire schemas.

use crate::SirLoweringError;

use super::InstanceService;
use hew_types::ResolvedTy;

impl InstanceService<'_> {
    /// Demand every actor with a remote member so its codecs register at
    /// startup even when this program never addresses it by handle.
    pub(super) fn request_actor_codec_roots(&mut self) -> Result<(), SirLoweringError> {
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
                    .handlers
                    .iter()
                    .any(|handler| {
                        handler
                            .remote_receivers
                            .iter()
                            .any(|receiver| receiver.matches(&[]))
                            && self.actor_codec_eligible(&handler.param_tys, &handler.return_ty)
                    })
                    .then(|| ResolvedTy::Named {
                        head: hew_types::TypeHead::Actor(hew_types::NominalHead::new(
                            hew_types::NominalId::of_declaration(actor.declaration),
                            self.module.defs.path(actor.declaration),
                        )),
                        args: Vec::new(),
                        is_opaque: false,
                    })
            })
            .collect::<Vec<_>>();
        for ty in roots {
            self.require_actor(&ty)?;
        }
        Ok(())
    }

    pub(super) fn actor_codec_eligible(&self, params: &[ResolvedTy], reply: &ResolvedTy) -> bool {
        params.iter().chain(std::iter::once(reply)).all(|ty| {
            self.checked_facts
                .data_error(ty, true, &|_, _| false)
                .is_none()
        })
    }

    pub(super) fn actor_wire_identity(
        &self,
        declaration: &str,
        arguments: &[ResolvedTy],
    ) -> Result<String, SirLoweringError> {
        // Only rendering changes: nominal identities stay intact, while root
        // source paths and import aliases never become portable type names.
        fn portable(defs: &hew_types::DefTable, ty: &hew_types::Ty) -> hew_types::Ty {
            let mut ty = ty.map_children_pub(&|child| portable(defs, child));
            if let hew_types::Ty::Named {
                head: hew_types::TypeHead::Nominal(nominal) | hew_types::TypeHead::Actor(nominal),
                ..
            } = &mut ty
            {
                *nominal = hew_types::NominalHead::new(
                    nominal.id,
                    &defs.dotted_declaration_path(nominal.id.declaration()),
                );
            }
            ty
        }
        if arguments.is_empty() {
            return Ok(declaration.to_string());
        }
        let arguments = arguments
            .iter()
            .map(|ty| {
                ResolvedTy::from_ty(&portable(&self.module.defs, &ty.to_ty()))
                    .map(|ty| ty.canonical_string())
                    .map_err(|error| SirLoweringError::from(error.to_string()))
            })
            .collect::<Result<Vec<_>, SirLoweringError>>()?;
        Ok(format!("{declaration}<{}>", arguments.join(",")))
    }

    pub(super) fn resolve_actor_codec(
        &mut self,
        params: &[ResolvedTy],
        reply: &ResolvedTy,
    ) -> Result<crate::actor::SemActorCodec, SirLoweringError> {
        Ok(crate::actor::SemActorCodec {
            params: params
                .iter()
                .map(|ty| self.wire_plans(ty))
                .collect::<Result<_, _>>()?,
            reply: (*reply != ResolvedTy::Unit)
                .then(|| self.wire_plans(reply))
                .transpose()?,
        })
    }
}
