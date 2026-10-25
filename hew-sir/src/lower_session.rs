//! Interactive bindings retain their owning places across entry boundaries.

use super::{
    BindingTarget, BlockArg, BodySource, Builder, OwnKind, PlaceId, PlaceOrigin, SirLoweringError,
    ValueId,
};

impl Builder<'_, '_> {
    pub(super) fn bind_session_places(
        &mut self,
        source: &BodySource,
    ) -> Result<(), SirLoweringError> {
        let BodySource::Session(entry) = source else {
            return Ok(());
        };
        let abi = &self.callable.signature.params[0];
        self.params.insert(
            0,
            BlockArg {
                value: ValueId(0),
                ty: abi.ty.clone(),
                own: OwnKind::None,
            },
        );
        for retained in &entry.bindings {
            let ty = self.ty(&retained.binding.ty);
            self.service.require_type_facts(&ty)?;
            let place = PlaceId(
                u32::try_from(self.places.len())
                    .map_err(|_| SirLoweringError::from("session place count exceeds u32"))?,
            );
            self.places.push(crate::PlaceDecl {
                id: place,
                ty,
                origin: PlaceOrigin::Session {
                    context: ValueId(0),
                    slot: retained.slot,
                    initialized: retained.initialized,
                },
            });
            self.session_places.insert(retained.binding.id, place);
            if retained.initialized {
                self.bind_source_target(&retained.binding, BindingTarget::Place(place))?;
            }
        }
        Ok(())
    }
}
