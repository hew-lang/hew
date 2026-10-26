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
                    contents: retained.contents.clone(),
                },
            });
            self.expand_session_partition(place)?;
            self.session_places.insert(retained.binding.id, place);
            if retained.retained {
                self.bind_source_target(&retained.binding, BindingTarget::Place(place))?;
            }
        }
        Ok(())
    }
    /// A persistent type has one partition independent of which fields this
    /// submission happens to select. Ownership-marked records remain whole.
    fn expand_session_partition(&mut self, root: PlaceId) -> Result<(), SirLoweringError> {
        let mut pending = vec![root];
        while let Some(place) = pending.pop() {
            let ty = self.places[place.0 as usize].ty.clone();
            if matches!(
                ty,
                hew_types::ResolvedTy::Named {
                    is_opaque: true,
                    ..
                }
            ) || !super::is_concrete_aggregate_type(
                self.service.module,
                &self.service.checked_facts,
                &ty,
            ) || self.service.checked_facts.declaration_marker(&ty)?
                != hew_types::DeclarationMarker::None
            {
                continue;
            }
            let shape = self.service.require_aggregate_shape(&ty)?;
            let fields = crate::aggregate_field_recipes(
                shape,
                &ty,
                &self.service.aggregate_shapes,
                self.service.checked_facts.rows(),
            )?;
            for (field, _) in fields.iter().enumerate() {
                let field = u32::try_from(field)
                    .map_err(|_| SirLoweringError::from("session field count exceeds u32"))?;
                let child = super::projection::declare_path(
                    &mut self.places,
                    crate::PlaceBase::Place(place),
                    &ty,
                    &[(shape, field)],
                    &self.service.aggregate_shapes,
                    self.service.checked_facts.rows(),
                )?;
                pending.push(child);
            }
        }
        Ok(())
    }
}
